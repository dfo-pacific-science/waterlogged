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
#'   \item{\code{DO_ERROR}}{Logger error code (\code{-888.88}) in DO or
#'     water temperature -- DO and percent sat set to \code{NA}.}
#'   \item{\code{NEGATIVE_DO}}{DO between 0 and \code{-1} mg/L corrected to
#'     0; DO less than \code{-1} mg/L removed (\code{NA}).}
#'   \item{\code{OUTLIER_DO}}{DO greater than 21 mg/L removed (\code{NA}).}
#'   \item{\code{NEGATIVE_TEMP}}{Water temperature between 0 and \code{-1}
#'     degrees C corrected to 0.}
#'   \item{\code{LOGGER_ICE}}{Water temperature <= \code{-1}
#'     degrees C removed (\code{NA}).}
#'   \item{\code{FLAG_ICE}}{Water temperature < 0.3 degrees C flagged
#'     for possible ice conditions.}
#'   \item{\code{FLAG_DO_DRY}}{Sliding 12-point window where percent sat
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
#' @param log_root Character scalar or \code{NULL}. Root directory for QAQC
#'   log files. When non-\code{NULL}, a log CSV is written via
#'   \code{\link{qaqc_log_maybe_append}}. When \code{NULL} (default, Shiny
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
#'     \item \strong{Flag columns:} \code{do_error}, \code{do_neg_between},
#'       \code{do_neg_lt_minus1}, \code{do_high}, \code{temp_flag_ice},
#'       \code{temp_neg_between}, \code{temp_neg_leq_minus1},
#'       \code{flag_do_dry}.
#'     \item \strong{Attribute:} \code{attr(result, "qaqc_log")} -- a tibble
#'       of QAQC log rows summarising every rule that fired.
#'   }
#'
#' @seealso \code{\link{waterlevel_qaqc}} (analogous QAQC for water level),
#'   \code{\link{conductivity_qaqc}} (analogous for conductivity),
#'   \code{\link{adjust_logger_NA}} (downstream manual adjustment),
#'   \code{\link{convert_do_mgl_percsat}} (upstream DO conversion)
#'
#' @export

dissox_qaqc <- function(input_data,
                        select_station,
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
      "dissox_qaqc(): input_data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 1. Filter to station & sort ----
  output_data <- input_data |>
    dplyr::filter(.data$site_station_code == !!select_station) |>
    dplyr::arrange(.data$timestamp)

  if (nrow(output_data) == 0) {
    warning("dissox_qaqc(): no rows for station ", select_station)
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
      do_error         = (do_mgl == -888.88) | (watertemp_C_do == -888.88),
      do_neg_between   = !is.na(do_mgl_adj) & do_mgl_adj < 0  & do_mgl_adj >= -1,
      do_neg_lt_minus1 = !is.na(do_mgl_adj) & do_mgl_adj < -1,
      do_high          = !is.na(do_mgl_adj) & do_mgl_adj > 21,

      # temperature flags (using adj as working col)
      temp_flag_ice       = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj < 0.3,
      temp_neg_between    = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj < 0 & watertemp_C_do_adj > -1,
      temp_neg_leq_minus1 = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj <= -1
    )

  # ---- 4. Apply DO / temp adjustments (vectorised) ---------------------------
  output_data <- output_data |>
    dplyr::mutate(
      # DO adjustments, applied to adj column only
      do_mgl_adj = dplyr::case_when(
        do_error | do_neg_lt_minus1 | do_high ~ NA_real_,
        do_neg_between                         ~ 0,
        TRUE                                   ~ do_mgl_adj
      ),
      do_percsat_adj = dplyr::case_when(
        do_error | do_neg_lt_minus1 | do_high ~ NA_real_,
        do_neg_between                         ~ 0,
        TRUE                                   ~ do_percsat_adj
      ),
      # DO temp adjustments
      watertemp_C_do_adj = dplyr::case_when(
        temp_neg_leq_minus1 ~ NA_real_,
        temp_neg_between    ~ 0,
        TRUE                ~ watertemp_C_do_adj
      )
    )

  # ---- 5. 12-point dry flag (FLAG_DO_DRY) using adj DO -----------------------
  n <- nrow(output_data)
  output_data$flag_do_dry <- FALSE

  if (n >= 12) {
    for (j in 1:(n - 11)) {
      window_diff <- output_data$air_watertemp_diff[j:(j + 11)]
      window_do   <- output_data$do_percsat_adj[j:(j + 11)]

      if (any(!is.na(window_diff)) && any(!is.na(window_do))) {
        if (max(abs(window_diff), na.rm = TRUE) <= 2 &&
            min(window_do, na.rm = TRUE) >= 100) {
          output_data$flag_do_dry[j:(j + 11)] <- TRUE
        }
      }
    }
  }

  # ---- 6. Build QAQC log rows (WL-style rules list) -------------------------
  metric      <- "DO"
  fun_name    <- "dissox_qaqc"
  action      <- "AUTO_QAQC"
  manual_note <- "Automatic dissolved oxygen QA/QC; no manual note provided."
  run_time    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  rules <- list(
    # DO logger error
    list(
      flag        = output_data$do_error %in% TRUE,
      field       = "do_mgl_adj",
      code        = "DO_ERROR",
      action_note = "Logger error code (-888.88) in DO or DO temperature; DO and %sat removed."
    ),
    # negative DO
    list(
      flag        = output_data$do_neg_between %in% TRUE,
      field       = "do_mgl_adj",
      code        = "NEGATIVE_DO",
      action_note = "DO between 0 and -1 mg/L corrected to 0 (and %sat to 0)."
    ),
    list(
      flag        = output_data$do_neg_lt_minus1 %in% TRUE,
      field       = "do_mgl_adj",
      code        = "NEGATIVE_DO",
      action_note = "DO less than -1 mg/L removed (DO and %sat set to NA)."
    ),
    # very high DO
    list(
      flag        = output_data$do_high %in% TRUE,
      field       = "do_mgl_adj",
      code        = "OUTLIER_DO",
      action_note = "DO greater than 21 mg/L removed (DO and %sat set to NA)."
    ),
    # temperature-driven rules
    list(
      flag        = output_data$temp_neg_between %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "NEGATIVE_TEMP",
      action_note = "DO water temp between 0 and -1 \u00b0C corrected to 0 \u00b0C."
    ),
    list(
      flag        = output_data$temp_neg_leq_minus1 %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "LOGGER_ICE",
      action_note = "DO water temp <= -1 \u00b0C removed (set to NA)."
    ),
    list(
      flag        = output_data$temp_flag_ice %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "FLAG_ICE",
      action_note = "Possible ice conditions: DO water temp < 0.3 \u00b0C; flagged for review."
    ),
    # dry periods (12-point rule) - DO-specific
    list(
      flag        = output_data$flag_do_dry %in% TRUE,
      field       = "do_mgl_adj",
      code        = "FLAG_DO_DRY",
      action_note = "Possible dry sensor: DO %sat >= 100 and air-water temps within 2 \u00b0C for >= 12 points."
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
