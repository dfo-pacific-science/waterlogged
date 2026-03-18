#' QA/QC a single water temperature time series
#'
#' Applies automatic QA/QC rules to a water temperature time series for one
#' station: flags possible ice and dry conditions, corrects small negative
#' temperatures to 0, and removes clearly impossible values (< -1 C). Returns
#' the input data (filtered to \code{select_station}) with adjusted columns and
#' boolean flag/edit columns.
#'
#' @param input_data Data frame of water-level or dissolved-oxygen logger data
#'   containing at least \code{timestamp}, \code{site_station_code}, and
#'   \code{watertemp_C}. An \code{airtemp_C} column is optional: when present,
#'   air-water comparison dry checks are also applied.
#' @param select_station Character, station code to process
#'   (matches \code{site_station_code}).
#' @param temp_ice_threshold_C Water temperature (C) below which ice conditions
#'   are flagged. Default \code{0.3}.
#' @param temp_high_dry_C Water temperature (C) above which a dry sensor is
#'   suspected. Default \code{19}.
#' @param temp_air_water_diff_C Absolute difference (C) between water and air
#'   temperature below which a dry sensor is suspected (when \code{airtemp_C}
#'   is present). Default \code{2}.
#' @param log_root Root folder where QA/QC logs are written by
#'   [qaqc_log_maybe_append()]. Pass \code{NULL} (default) to skip file writing.
#' @param user Character username written into the QA/QC log. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]}.
#'
#' @details
#' The following rules are applied:
#' \describe{
#'   \item{\code{OUTLIER_WT}}{Water temperature < -1 C: set to
#'     \code{NA} in \code{watertemp_C_adj}.}
#'   \item{\code{OUTLIER_WT}}{Water temperature in [-1, 0) C: corrected
#'     to 0 in \code{watertemp_C_adj}.}
#'   \item{\code{FLAG_ICE}}{Water temperature < \code{temp_ice_threshold_C}:
#'     flagged for review; data not changed.}
#'   \item{\code{FLAG_DRY}}{Possible dry sensor: water temperature >
#'     \code{temp_high_dry_C}; or, when \code{airtemp_C} is present, air-water
#'     temperature within \code{temp_air_water_diff_C} degrees C, or air warmer
#'     than water.}
#' }
#'
#' @return A data frame containing only rows for \code{select_station}, sorted
#'   by \code{timestamp}, with \code{watertemp_C_adj}, \code{FLAG_ICE},
#'   \code{FLAG_DRY}, \code{OUTLIER_WT}, and
#'   \code{OUTLIER_WT} columns added.
#'   \code{attr(result, "qaqc_log")} holds a tibble of QA/QC log rows.
#'
#' @export
qc_watertemp <- function(input_data,
                           select_station,
                           temp_ice_threshold_C  = 0.3,
                           temp_high_dry_C       = 19,
                           temp_air_water_diff_C = 2,
                           log_root = NULL,
                           user     = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 1. Filter & basic checks ---------------------------------------------
  required_cols <- c("timestamp", "site_station_code", "watertemp_C")
  missing_cols  <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "qc_watertemp(): input_data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  df <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(df) == 0) {
    stop("qc_watertemp(): no rows found for station '", select_station, "'.")
  }

  if (!inherits(df$timestamp, "POSIXct")) {
    df$timestamp <- lubridate::ymd_hms(df$timestamp, tz = "UTC")
  }

  has_airtemp <- "airtemp_C" %in% names(df)

  # Create adjusted column if missing
  if (!"watertemp_C_adj" %in% names(df)) df$watertemp_C_adj <- df$watertemp_C

  # Create flag/edit cols if missing
  new_flags <- c("FLAG_ICE", "FLAG_DRY", "OUTLIER_WT")
  for (f in new_flags) if (!f %in% names(df)) df[[f]] <- FALSE

  # ---- 2. Detection rules ---------------------------------------------------
  neg_lt_minus1 <- !is.na(df$watertemp_C) & df$watertemp_C < -1
  neg_between   <- !is.na(df$watertemp_C) & df$watertemp_C >= -1 & df$watertemp_C < 0
  flag_ice_vec  <- !is.na(df$watertemp_C) & df$watertemp_C < temp_ice_threshold_C
  wt_high_dry   <- !is.na(df$watertemp_C) & df$watertemp_C > temp_high_dry_C

  if (has_airtemp) {
    wt_air_close  <- !is.na(df$watertemp_C) & !is.na(df$airtemp_C) &
      abs(df$watertemp_C - df$airtemp_C) < temp_air_water_diff_C
    wt_air_warmer <- !is.na(df$watertemp_C) & !is.na(df$airtemp_C) &
      (df$airtemp_C - df$watertemp_C) > 0
    flag_wt_dry_vec <- wt_high_dry | wt_air_close | wt_air_warmer
  } else {
    flag_wt_dry_vec <- wt_high_dry
  }

  # ---- 3. Apply adjustments -------------------------------------------------
  df <- df |>
    dplyr::mutate(
      watertemp_C_adj = dplyr::case_when(
        neg_lt_minus1 ~ NA_real_,
        neg_between   ~ 0,
        TRUE          ~ watertemp_C_adj
      ),
      FLAG_ICE            = flag_ice_vec,
      OUTLIER_WT = neg_lt_minus1,
      OUTLIER_WT  = neg_between,
      FLAG_DRY         = flag_wt_dry_vec
    )

  # ---- 4. Log entries -------------------------------------------------------
  metric   <- "WT"
  fun_name <- "qc_watertemp"
  run_time <- Sys.time()

  rules <- list(
    list(
      flag        = neg_lt_minus1 | neg_between,
      field       = "watertemp_C_adj",
      code        = "NEGATIVE_WT",
      action_note = "Negative water temperature detected; removed (< -1\u00b0C) or corrected to 0\u00b0C (-1 to 0\u00b0C)."
    ),
    list(
      flag        = flag_ice_vec,
      field       = "watertemp_C_adj",
      code        = "FLAG_ICE",
      action_note = paste0("Possible ice conditions: water temperature < ",
                           temp_ice_threshold_C, "\u00b0C; flagged for review.")
    ),
    list(
      flag        = flag_wt_dry_vec,
      field       = "watertemp_C_adj",
      code        = "FLAG_DRY",
      action_note = paste0("Possible dry sensor: water temperature > ",
                           temp_high_dry_C, "\u00b0C",
                           if (has_airtemp) paste0(", or air-water temperature within ",
                                                   temp_air_water_diff_C, "\u00b0C / air warmer than water")
                           else "",
                           "; flagged for review.")
    )
  )

  logs_list <- lapply(rules, function(r) {
    make_qaqc_log_row(
      timestamps  = df$timestamp[r$flag],
      station     = select_station,
      metric      = metric,
      field       = r$field,
      action      = "AUTO_QAQC",
      code        = r$code,
      action_note = r$action_note,
      manual_note = "Automatic water temperature QA/QC; no manual note provided.",
      fun_name    = fun_name,
      user        = user,
      run_time    = run_time
    )
  })

  log_this <- dplyr::bind_rows(logs_list)

  if (nrow(log_this) == 0) {
    log_this <- dplyr::tibble(
      station        = select_station,
      metric         = metric,
      field          = "watertemp_C_adj",
      action         = "AUTO_QAQC",
      code           = NA_character_,
      action_note    = "Water temperature QA/QC ran; no flags applied.",
      manual_note    = "Automatic water temperature QA/QC; no manual note provided.",
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }

  qaqc_log_maybe_append(log_root, select_station, metric, log_this)

  message("Water temperature QAQC completed for: ", select_station)

  attr(df, "qaqc_log") <- log_this
  return(df)
}
