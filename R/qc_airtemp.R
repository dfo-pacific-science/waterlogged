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
#' @param temp_low_flag_C Lower bound (C) for acceptable air temperature.
#'   Values below this are flagged as \code{TRUE} in \code{FLAG_RANGE}.
#' @param temp_high_flag_C Upper bound (C) for acceptable air temperature.
#'   Values above this are flagged as \code{TRUE} in \code{FLAG_RANGE}.
#'@param temp_low_limit_C Lower bound (C) for plausible air temperature.
#'   Values below this are set to \code{NA} in \code{airtemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_AT}.
#' @param temp_high_limit_C Upper bound (C) for plausible air temperature.
#'   Values above this are set to \code{NA} in \code{airtemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_AT}
#' @param log_root Character scalar or \code{NULL}. Root directory for QAQC
#'   log files. When non-\code{NULL}, a log CSV is written via
#'   \code{\link{qaqc_log_maybe_append}()}. When \code{NULL} (default, Shiny
#'   mode), no file is written.
#' @param user Character username written into the QA/QC log. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]}.
#'
#' @details
#' If \code{airtemp_C_adj} does not exist it is created as a copy of
#' \code{airtemp_C}.\code{OUTLIER_AT} and \code{FLAG_RANGE} is created if missing and set to
#' \code{TRUE} where the rule applies.
#'
#' @return A data frame containing only rows for \code{select_station}, sorted
#'   by \code{timestamp}, with \code{airtemp_C_adj}, \code{OUTLIER_AT} and \code{FLAG_RANGE}
#'   columns added. \code{attr(result, "qaqc_log")} holds a tibble of QA/QC log
#'   rows.
#'
#' @export
qc_airtemp <- function(input_data,
                         select_station,
                         temp_low_flag_C  = -40,
                         temp_high_flag_C =  50,
                         temp_low_limit_C  = -Inf,
                         temp_high_limit_C = Inf,
                         log_root = NULL,
                         user     = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 1. Filter & basic checks ---------------------------------------------
  output_data <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(output_data) == 0) stop("qc_airtemp(): no rows for station ", select_station)

  required <- c("timestamp", "site_station_code", "airtemp_C")
  miss <- setdiff(required, names(output_data))
  if (length(miss) > 0) {
    stop("qc_airtemp(): input_data is missing required columns: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  # ensure flags and limits in range
  if (temp_low_limit_C > 0) {
    stop(
      "qc_airtemp(): temp_low_limit_C must be <= 0. "
    )
  }

  if (temp_low_flag_C < temp_low_limit_C) {
    stop(
      paste("qc_airtemp(): flagging value for low temperature, (temp_low_flag_C:", temp_low_flag_C,
            ") must be greater than temperature outlier limit (temp_low_limit_C:", temp_low_limit_C, "). ")
    )
  }

  # Create adjusted column if missing
  if (!"airtemp_C_adj" %in% names(output_data)) output_data$airtemp_C_adj <- output_data$airtemp_C

  # Create edit flag if missing
  if (!"FLAG_RANGE" %in% names(output_data)) output_data$FLAG_RANGE <- FALSE
  if (!"OUTLIER_AT" %in% names(output_data)) output_data$OUTLIER_AT <- FALSE

  # ---- 2. Detection rule ----------------------------------------------------
 flag_temp <- !is.na(output_data$airtemp_C) &
    (output_data$airtemp_C <= temp_low_flag_C | output_data$airtemp_C >= temp_high_flag_C)

  bad_temp <- !is.na(output_data$airtemp_C) &
    (output_data$airtemp_C < temp_low_limit_C | output_data$airtemp_C > temp_high_limit_C)

  # ---- 3. Apply adjustments -------------------------------------------------
  output_data <- output_data |>
    dplyr::mutate(
      airtemp_C_adj  = dplyr::if_else(bad_temp, NA_real_, airtemp_C_adj),
      OUTLIER_AT = bad_temp,
      FLAG_RANGE = flag_temp
    )

  # ---- 4. Log entries -------------------------------------------------------

  metric      <- "AT"
  fun_name    <- "qc_airtemp"
  action      <- "AUTO_QAQC"
  manual_note <- "AUTOMATIC QC"
  run_time    <- Sys.time()

  rules <- list(
    # AT out of range
    list(
      flag        = output_data$OUTLIER_AT %in% TRUE,
      field       = "airtemp_C_adj",
      action      = "TEMP_OUT_RANGE",
      code        = "TEMP_OUT_RANGE",
      action_note = paste0("Air temperature outside [", temp_low_limit_C, ", ",
                           temp_high_limit_C, "] C; set to NA")
    ),
    # AT flagged
    list(
      flag        = output_data$FLAG_RANGE %in% TRUE,
      field       = "airtemp_C_adj",
      action      = "TEMP_FLAG_RANGE",
      code        = "TEMP_FLAG_RANGE",
      action_note = paste0("Air temperature outside [", temp_low_flag_C, ", ",
                           temp_high_flag_C, "] C; flagged for review")
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

  attr(output_data, "qaqc_log") <- log_this
  return(output_data)
}
