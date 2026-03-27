#' Automatic QAQC for dissolved oxygen logger data
#'
#' Applies a suite of automated quality-control rules to dissolved oxygen
#' time series for a single station. Raw values are never modified; instead,
#' adjusted copies (\code{do_mgl_adj}, \code{do_percsat_adj},
#' \code{watertemp_C_do_adj}) are created (or updated if they already exist)
#' and Boolean flag columns are added for review.
#'
#' The following rules are applied in order:
#' \describe{
#'   \item{\code{ERROR_DO}}{Logger error code (\code{-888.88}) in DO or
#'     water temperature -- DO and percent sat set to \code{NA}.}
#'   \item{\code{OUTLIER_DO}}{DO between 0 and \code{-1} mg/L corrected to
#'     0; DO less than \code{-1} mg/L or greater than 30 mg/L removed (\code{NA}).}
#'   \item{\code{OUTLIER_WT}}{Water temperature below \code{-1}
#'     degrees C removed (\code{NA}); between \code{-1} and 0
#'     degrees C corrected to 0.}
#'   \item{\code{FLAG_RANGE}}{Sliding 12-point window where DO mg/L is greater than the calibration range of 20 mg/L.}
#'   \item{\code{FLAG_ICE}}{Water temperature < 0.3 degrees C flagged
#'     for possible ice conditions.}
#'   \item{\code{FLAG_DRY}}{Sliding 12-point window where percent sat
#'     >= 100 and air--water temperature difference <= 2
#'     degrees C, indicating a potentially dry sensor.}
#' }
#'
#' @param input_data A data frame containing dissolved oxygen observations.
#'   Must include the columns: \code{site_station_code}, \code{timestamp}
#'   (POSIXct or parseable by \code{lubridate::ymd_hms()}), \code{do_mgl},
#'   \code{do_percsat}, \code{watertemp_C}, and \code{airtemp_C}. Typically
#'   the output of \code{\link{convert_do_mgl_percsat}} or
#'   \code{\link{add_nearest_baro}}.
#' @param select_station Character scalar. The \code{site_station_code} to
#'   process. Only rows matching this station are retained.
#' @param do_high_limit_mgl Upper bound (mg/L) for plausible dissolved oxygen.
#'   Values above this are set to \code{NA}. Default \code{30}.
#' @param do_low_limit_mgl Lower bound (mg/L) for dissolved oxygen. Values
#'   between \code{-1} and \code{0} are corrected to \code{0}; values below
#'   \code{-1} are set to \code{NA}. Default \code{0}.
#' @param do_high_flag_mgl Upper bound (mg/L) for flag dry dissolved oxygen.
#'   Values above this are flagged as \code{FLAG_DRY}. Default \code{20}.
#' @param do_low_flag_mgl Lower bound (mg/L) for flag range dissolved oxygen; values below
#'   \code{do_low_flag_mgl} and \code(0) are flagged as \code{FLAG_RANGE}. Default \code{0}.
#' @param temp_ice_threshold_C Water temperature (C) below which ice conditions
#'   are flagged. Default \code{0.3}.
#' @param temp_air_water_diff_C Absolute difference (C) between water and air
#'   temperature below which a dry sensor is suspected (when \code{airtemp_C}
#'   is present). Default \code{2}.
#' @param temp_low_limit Lower bound (C) for plausible water temperature. Default \code{-1}.
#'   Values below this are set to \code{NA} in \code{watertemp_C_do_adj} and flagged as \code{TRUE} in \code{OUTLIER_WT}.
#' @param temp_high_limit Upper bound (C) for plausible water temperature.
#'   Values above this are set to \code{NA} in \code{watertemp_C_do_adj} and flagged as \code{TRUE} in \code{OUTLIER_WT}
#' @param log_root Character scalar or \code{NULL}. Root directory for QAQC
#'   log files. When non-\code{NULL}, a log CSV is written via
#'   \code{\link{qaqc_log_maybe_append}()}. When \code{NULL} (default, Shiny
#'   mode), no file is written.
#' @param user Character scalar. User name recorded in the QAQC log.
#'   Defaults to \code{NULL}, which resolves to
#'   \code{Sys.info()[["user"]]} (the local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#'
#' @return The filtered data frame for \code{select_station}, sorted by
#'   \code{timestamp}, with the following additions:
#'   \itemize{
#'     \item \strong{Adjusted columns:} \code{do_mgl_adj},
#'       \code{do_percsat_adj}, \code{watertemp_C_do_adj}.
#'     \item \strong{Flag columns:} \code{ERROR_DO}, \code{OUTLIER_DO},
#'        \code{OUTLIER_WT}, \code{FLAG_RANGE}, \code{FLAG_ICE},
#'       \code{FLAG_DRY}.
#'     \item \strong{Attribute:} \code{attr(result, "qaqc_log")} -- a tibble
#'       of QAQC log rows summarising every rule that fired.
#'   }
#'
#' @seealso \code{\link{qc_waterlevel}} (analogous QAQC for water level),
#'   \code{\link{qc_conductivity}} (analogous for conductivity),
#'   \code{\link{adjust_logger_NA}} (downstream manual adjustment),
#'   \code{\link{convert_do_mgl_percsat}} (upstream DO conversion)
#'
#' @export

qc_dissox <- function(input_data,
                        select_station,
                        do_high_limit_mgl = 30,
                        do_low_limit_mgl = 0,
                        do_high_flag_mgl = 20,
                        do_low_flag_mgl = 0,
                        temp_high_limit = 50,
                        temp_low_limit = -1,
                        temp_ice_threshold_C = 0.3,
                        temp_air_water_diff_C = 2,
                        log_root = NULL,
                        user = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 0. Required columns ----
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "do_mgl",
    "do_percsat",
    "watertemp_C",
    "airtemp_C"
  )

  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "qc_dissox(): input_data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 1. Filter to station & sort ----
  output_data <- input_data |>
    dplyr::filter(.data$site_station_code == !!select_station) |>
    dplyr::arrange(.data$timestamp)

  if (nrow(output_data) == 0) {
    warning("qc_dissox(): no rows for station ", select_station)
    return(output_data)
  }

  # Ensure timestamp is POSIXct
  if (!inherits(output_data$timestamp, "POSIXct")) {
    output_data$timestamp <- lubridate::ymd_hms(output_data$timestamp, tz = "UTC")
  }

  # ---- 2. Initialise internal temp fields + *_adj columns --------------------
  # Internal temp names (consistent with plotting / other funcs)
  if (!"watertemp_C_do" %in% names(output_data)) {
    output_data$watertemp_C_do <- output_data$watertemp_C
  }
  if (!"airtemp_C_baro" %in% names(output_data)) {
    output_data$airtemp_C_baro <- output_data$airtemp_C
  }

  # Only initialise *_adj if they don't already exist
  if (!"do_mgl_adj" %in% names(output_data)) {
    output_data$do_mgl_adj <- output_data$do_mgl
  }
  if (!"do_percsat_adj" %in% names(output_data)) {
    output_data$do_percsat_adj <- output_data$do_percsat
  }
  if (!"watertemp_C_do_adj" %in% names(output_data)) {
    output_data$watertemp_C_do_adj <- output_data$watertemp_C_do
  }

  # ---- 3. Derived helpers & flags (vectorised) -------------------------------
  # All conditions are based on the *current* adj values so we don't
  # overwrite manual corrections.
  output_data <- output_data |>
    dplyr::mutate(
      # temp difference for dryness test (using current temp adj)
      air_watertemp_diff = airtemp_C_baro - watertemp_C_do_adj,

      # DO flags (using adj as working column)
      ERROR_DO         = (do_mgl == -888.88) | (watertemp_C_do == -888.88),
      do_neg_between   = !is.na(do_mgl_adj) & do_mgl_adj < 0  & do_mgl_adj >= do_low_limit_mgl,
      do_neg_lt_minus1 = !is.na(do_mgl_adj) & do_mgl_adj <= do_low_limit_mgl,
      do_high          = !is.na(do_mgl_adj) & do_mgl_adj >= do_high_limit_mgl,
      do_low_flag = !is.na(do_mgl_adj) & do_mgl_adj < do_low_flag_mgl,
      do_high_flag          = !is.na(do_mgl_adj) & do_mgl_adj >= do_high_flag_mgl,

      # temperature flags (using adj as working col)
      FLAG_ICE       = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj < temp_ice_threshold_C,
      temp_neg_between    = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj < 0 & watertemp_C_do_adj >= temp_low_limit,
      temp_neg_leq_minus1 = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj <= temp_low_limit,
      wt_high_limit   = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj >= temp_high_limit

    )

  # ---- 4. Apply DO / temp adjustments (vectorised) ---------------------------
  output_data <- output_data |>
    dplyr::mutate(
      # DO adjustments, applied to adj column only
      do_mgl_adj = dplyr::case_when(
        ERROR_DO | do_neg_lt_minus1 | do_high ~ NA_real_,
        do_neg_between                         ~ 0,
        TRUE                                   ~ do_mgl_adj
      ),
      do_percsat_adj = dplyr::case_when(
        ERROR_DO | do_neg_lt_minus1 | do_high ~ NA_real_,
        do_neg_between                         ~ 0,
        TRUE                                   ~ do_percsat_adj
      ),
      # DO temp adjustments
      watertemp_C_do_adj = dplyr::case_when(
        temp_neg_leq_minus1 ~ NA_real_,
        temp_neg_between    ~ 0,
        wt_high_limit       ~ NA_real_,
        TRUE                ~ watertemp_C_do_adj
      ),

       # combine outlier flags

      OUTLIER_DO = do_high | do_neg_between | do_neg_lt_minus1,
      OUTLIER_WT =  temp_neg_between | temp_neg_leq_minus1 | wt_high_limit,
      FLAG_DRY = do_high_flag,
      FLAG_RANGE = do_low_flag
       ) |>
    # drop only internal helpers we don't want in the output
    dplyr::select(
      -do_neg_between, -do_neg_lt_minus1, -do_high, -do_high_flag, - do_low_flag,
      -temp_neg_leq_minus1, -temp_neg_between, - wt_high_limit
    )


  # ---- 5. 12-point dry flag (FLAG_DRY) using adj DO -----------------------
  n <- nrow(output_data)
  output_data$FLAG_DRY <- FALSE

  if (n >= 12) {
    for (j in 1:(n - 11)) {
      window_diff <- output_data$air_watertemp_diff[j:(j + 11)]
      window_do   <- output_data$do_percsat_adj[j:(j + 11)]

      if (any(!is.na(window_diff)) && any(!is.na(window_do))) {
        if (max(abs(window_diff), na.rm = TRUE) <= temp_air_water_diff_C &&
            min(window_do, na.rm = TRUE) >= 100) {
          output_data$FLAG_DRY[j:(j + 11)] <- TRUE
        }
      }
    }
  }

 # drop only internal helpers we don't want in the output
  output_data <- output_data |>
  dplyr::select(
    -air_watertemp_diff
  )

  # ---- 6. Build QAQC log rows (WL-style rules list) -------------------------
  metric      <- "DO"
  fun_name    <- "qc_dissox"
  action      <- "AUTO_QAQC"
  manual_note <- "Automatic dissolved oxygen QA/QC; no manual note provided."
  run_time    <- Sys.time()

  rules <- list(
    # DO logger error
    list(
      flag        = output_data$ERROR_DO %in% TRUE,
      field       = "do_mgl_adj",
      code        = "ERROR_DO",
      action_note = "Logger error code (-888.88) in DO or DO temperature; DO and %sat removed."
    ),
    # DO out of range
    list(
      flag        = output_data$OUTLIER_DO %in% TRUE,
      field       = "do_mgl_adj",
      code        = "OUTLIER_DO",
      action_note = paste0("DO greater than ", do_high_limit_mgl, " mg/L removed (DO and %sat set to NA). DO < ", do_low_limit_mgl, " removed and DO values between ", do_low_limit_mgl, " and 0 corrected to 0.")
    ),
    # temperature-driven rules
    list(
      flag        = output_data$OUTLIER_WT %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "OUTLIER_WT",
      action_note = paste0("WT greater than ", temp_high_limit, " removed. WT < ", temp_low_limit, " removed and WT values between ", temp_low_limit, " and 0 corrected to 0.")
    ),
    list(
      flag        = output_data$FLAG_ICE %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "FLAG_ICE",
      action_note = paste0("Possible ice conditions: DO water temp < ", temp_ice_threshold_C," \u00b0C; flagged for review.")
    ),
    # dry periods (12-point rule) - DO-specific
    list(
      flag        = output_data$FLAG_DRY %in% TRUE,
      field       = "do_mgl_adj",
      code        = "FLAG_DRY",
      action_note = "Possible dry sensor: DO %sat >= 100 and air-water temps within 2 \u00b0C for >= 12 points."
    ),
    list(
      flag        = output_data$FLAG_RANGE %in% TRUE,
      field       = "do_mgl_adj",
      code        = "FLAG_RANGE",
      action_note = paste0("Possible sensor error: DO < ", do_low_flag_mgl," ; flagged for review" )
    )
  )

  logs_list <- lapply(rules, function(r) {
    ts_sel <- output_data$timestamp[r$flag]
    if (length(ts_sel) == 0) return(NULL)

    make_qaqc_log_row(
      timestamps  = ts_sel,
      station     = select_station,
      metric      = metric,
      field       = r$field,
      action      = action,
      code        = r$code,
      action_note = r$action_note,
      manual_note = manual_note,
      fun_name    = fun_name,
      user        = user,
      run_time    = run_time
    )
  })

  log_this <- logs_list[!vapply(logs_list, is.null, logical(1))]
  log_this <- if (length(log_this) > 0) {
    dplyr::bind_rows(log_this)
  } else {
    dplyr::tibble(
      station        = select_station,
      metric         = metric,
      field          = "do_mgl_adj",
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic dissolved oxygen QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }

  # ---- 7. Append to on-disk log (conditionally) ------------------------------
  qaqc_log_maybe_append(log_root, select_station, metric, log_this)

  attr(output_data, "qaqc_log") <- log_this

  return(output_data)
}
