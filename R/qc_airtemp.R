#' QA/QC a single air temperature time series
#'
#' Applies automatic range-check QA/QC to an air temperature logger time series
#' for one station. Out-of-range values are set to \code{NA} in the adjusted
#' column and flagged. Returns the input data (filtered to \code{select_station})
#' with adjusted columns and a boolean edit flag.
#'
#' @param input_data Data frame of barometric or air-temperature logger data
#'   containing at least \code{timestamp}, \code{site_station_code}, and
#'   \code{airtemp_C}.
#' @param select_station Character, station code to process
#'   (matches \code{site_station_code}).
#' @param temp_low_limit Lower bound (C) for plausible air temperature.
#'   Values below this are set to \code{NA} in \code{airtemp_C_adj} and flagged.
#' @param temp_high_limit Upper bound (C) for plausible air temperature.
#'   Values above this are set to \code{NA} in \code{airtemp_C_adj} and flagged.
#' @param log_root Character scalar or \code{NULL}. Root directory for QAQC
#'   log files. When non-\code{NULL}, a log CSV is written via
#'   \code{\link{qaqc_log_maybe_append()}}. When \code{NULL} (default, Shiny
#'   mode), no file is written.
#' @param user Character username written into the QA/QC log. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]}.
#'
#' @details
#' If \code{airtemp_C_adj} does not exist it is created as a copy of
#' \code{airtemp_C}. \code{OUTLIER_AT} is created if missing and set to
#' \code{TRUE} where the rule applies.
#'
#' @return A data frame containing only rows for \code{select_station}, sorted
#'   by \code{timestamp}, with \code{airtemp_C_adj} and \code{OUTLIER_AT}
#'   columns added. \code{attr(result, "qaqc_log")} holds a tibble of QA/QC log
#'   rows.
#'
#' @export
qc_airtemp <- function(input_data,
                         select_station,
                         temp_low_limit  = -40,
                         temp_high_limit =  40,
                         log_root = NULL,
                         user     = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 1. Filter & basic checks ---------------------------------------------
  df <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(df) == 0) stop("qc_airtemp(): no rows for station ", select_station)

  required <- c("timestamp", "site_station_code", "airtemp_C")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    stop("qc_airtemp(): input_data is missing required columns: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  # Create adjusted column if missing
  if (!"airtemp_C_adj" %in% names(df)) df$airtemp_C_adj <- df$airtemp_C

  # Create edit flag if missing
  if (!"OUTLIER_AT" %in% names(df)) df$OUTLIER_AT <- FALSE

  # ---- 2. Detection rule ----------------------------------------------------
  bad_temp <- !is.na(df$airtemp_C) &
    (df$airtemp_C < temp_low_limit | df$airtemp_C > temp_high_limit)

  # ---- 3. Apply adjustments -------------------------------------------------
  df <- df |>
    dplyr::mutate(
      airtemp_C_adj  = dplyr::if_else(bad_temp, NA_real_, airtemp_C_adj),
      OUTLIER_AT = bad_temp
    )

  # ---- 4. Log entries -------------------------------------------------------
  log_rows_list <- list()
  if (any(bad_temp)) {
    log_row <- make_qaqc_log_row(
      timestamps  = df$timestamp[bad_temp],
      station     = select_station,
      metric      = "AT",
      field       = "airtemp_C_adj",
      action      = "TEMP_OUT_RANGE",
      code        = "TEMP_OUT_RANGE",
      action_note = paste0("Air temperature outside [", temp_low_limit, ", ",
                           temp_high_limit, "] C; set to NA"),
      manual_note = "AUTOMATIC QC",
      fun_name    = "qc_airtemp",
      user        = user
    )
    if (!is.null(log_row)) log_rows_list[[1L]] <- log_row
  }

  log_this <- dplyr::bind_rows(log_rows_list)

  if (nrow(log_this) == 0) {
    log_this <- dplyr::tibble(
      station        = select_station,
      metric         = "AT",
      field          = "airtemp_C_adj",
      action         = "AUTO_QAQC",
      code           = NA_character_,
      action_note    = "Air temperature QA/QC ran; no out-of-range values found.",
      manual_note    = "AUTOMATIC QC",
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = "qc_airtemp",
      run_at         = Sys.time(),
      user           = user
    )
  }

  qaqc_log_maybe_append(log_root, select_station, "AT", log_this)

  message("Air temperature QAQC completed for: ", select_station)

  attr(df, "qaqc_log") <- log_this
  return(df)
}
