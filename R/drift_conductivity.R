# R/quality/wq_drift_correction.R

#' Water-quality drift correction (COND / DO / pH) \[DRAFT\]
#'
#' \strong{DRAFT — not fully tested. Do not use in production workflows
#' without independent verification of results.}
#'
#' Apply forward linear drift correction to a water-quality time series
#' using discrete reference measurements collected during field visits.
#' The correction ramps linearly from zero at the start of each segment
#' to the full correction at the visit endpoint, then restarts at the
#' next segment. No correction is applied after the last visit.
#'
#' Two correction methods are available:
#' \itemize{
#'   \item \strong{ratio} (multiplicative): each value is scaled by a
#'     linearly interpolated ratio between 1.0 (segment start) and the
#'     reference/logger ratio (segment end).
#'   \item \strong{offset} (additive): each value is shifted by a
#'     linearly interpolated offset between 0 (segment start) and the
#'     reference minus logger difference (segment end).
#' }
#'
#' Two event types control how segment boundaries are determined:
#' \itemize{
#'   \item \strong{check}: the end-target is the nearest logger sample
#'     to the visit timestamp; the next segment restarts at that same
#'     sample.
#'   \item \strong{calibration}: the end-target is the last logger
#'     sample strictly before \code{calibration_datetime}; the next
#'     segment restarts at the first sample at or after
#'     \code{calibration_datetime} (post-calibration values are
#'     trusted).
#' }
#'
#' @param input_data Data frame of QA/QC'd logger data. Must contain
#'   \code{site_station_code}, \code{timestamp} (POSIXct or parseable),
#'   and the adjusted value column for the chosen metric
#'   (\code{conduct_uScm_adj}, \code{do_percsat_adj}, or
#'   \code{ph_adj}).
#' @param ref_data_path Character path to a CSV file containing
#'   reference measurements. Set to \code{NULL} when providing
#'   \code{ref_data} directly (e.g., in a Shiny app). One of
#'   \code{ref_data_path} or \code{ref_data} must be provided.
#' @param select_station Character. The \code{site_station_code} to
#'   correct. Only rows matching this station are modified; all other
#'   rows are returned unchanged.
#' @param metric Character identifying the water-quality metric.
#'   Accepted values (case-insensitive): \code{"conductivity"},
#'   \code{"cond"}, \code{"dissolvedoxygen"}, \code{"do"}, \code{"ph"}.
#' @param method One of \code{"ratio"} (multiplicative, default) or
#'   \code{"offset"} (additive).
#' @param max_ref_gap_h Numeric. Maximum allowable time gap (in hours)
#'   between a reference measurement timestamp and the nearest logger
#'   sample. Reference events exceeding this gap are skipped with a
#'   warning. Default \code{1}.
#' @param log_root Optional path to the QA/QC log folder. If provided,
#'   a log entry is written to disk. If \code{NULL}, log rows are
#'   still returned as an attribute on the result.
#' @param user Character. Username recorded in QA/QC logs. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]} (the
#'   local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param ref_data Data frame of reference measurements, as an
#'   alternative to \code{ref_data_path}. Must contain the columns
#'   described in Details.
#'
#' @return The input data frame with drift-corrected values written
#'   back to the adjusted column (\code{*_adj}), plus an
#'   \code{edit_*_drift} logical flag column (\code{TRUE} for
#'   corrected rows). For dissolved oxygen, \code{do_mgl_adj} is also
#'   corrected using the same ratio as \code{do_percsat_adj} (ratio
#'   is dimensionless and applies consistently across both units).
#'   A \code{"qaqc_log"} attribute is attached containing the log
#'   rows as a tibble (or \code{NULL} if no rows were corrected).
#'
#' @details
#' \strong{Reference data format}
#'
#' The reference data (CSV or data frame) must contain these columns:
#' \describe{
#'   \item{metric}{Character. The water-quality metric (e.g.,
#'     \code{"conductivity"}, \code{"do"}, \code{"ph"}).}
#'   \item{site_station_code}{Character. Station identifier matching
#'     the logger data.}
#'   \item{timestamp}{Character or POSIXct. When the reference
#'     measurement was taken.}
#'   \item{watertemp_C}{Numeric. Water temperature at the time of
#'     measurement.}
#'   \item{value}{Numeric. The reference measurement value.}
#'   \item{unit}{Character. Unit of \code{value} (e.g.,
#'     \code{"uS/cm"}, \code{"\%sat"}, \code{"pH"}).}
#'   \item{event_type}{Character. One of \code{"check"},
#'     \code{"calibration"}, or \code{"ignore"}.}
#'   \item{calibration_datetime}{Character or POSIXct. Required for
#'     \code{"calibration"} events; the moment the sensor was
#'     recalibrated. \code{NA} or empty for other event types.}
#' }
#'
#' \strong{Timezone handling}
#'
#' Reference timestamps are parsed into the logger's timezone, which
#' is determined from the POSIXct \code{tzone} attribute or an
#' optional \code{timezone} column in \code{input_data}. Falls back
#' to \code{"UTC"} if neither is available.
#'
#' @seealso \code{\link{wl_multipoint_correction}} (water level drift
#'   correction)
#'
#' @export
wq_drift_correction <- function(input_data,
                                ref_data_path = NULL,
                                select_station,
                                metric,
                                method        = c("ratio", "offset"),
                                max_ref_gap_h = 1,
                                log_root      = NULL,
                                user          = NULL,
                                ref_data      = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  method <- match.arg(method)
  
  # ---------- metric -> columns ----------
  metric_norm <- normalize_string(metric)
  if (metric_norm %in% c("cond","conductivity")) {
    metric_norm    <- "conductivity"
    value_col      <- "conduct_uScm_adj"
    secondary_col  <- NULL
    edit_flag_col  <- "edit_cond_drift"
    accepted_units <- c("uscm","\u00b5s/cm","\u03bcs/cm","uS/cm")
    metric_log     <- "COND"
  } else if (metric_norm %in% c("do","dissolvedoxygen")) {
    metric_norm    <- "dissolvedoxygen"
    value_col      <- "do_percsat_adj"
    secondary_col  <- "do_mgl_adj"   # corrected with same ratio; keeps %sat & mg/L consistent
    edit_flag_col  <- "edit_do_drift"
    accepted_units <- c("%","percent","percent saturation","%sat","percsat")
    metric_log     <- "DO"
  } else if (metric_norm == "ph") {
    metric_norm    <- "ph"
    value_col      <- "ph_adj"
    secondary_col  <- NULL
    edit_flag_col  <- "edit_ph_drift"
    accepted_units <- c("ph","pH")
    metric_log     <- "PH"
  } else {
    stop("wq_drift_correction(): unsupported metric '", metric, "'.")
  }
  
  # ---------- logger data ----------
  req_logger_cols <- c("site_station_code","timestamp", value_col)
  missing_logger  <- setdiff(req_logger_cols, names(input_data))
  if (length(missing_logger)) {
    stop("wq_drift_correction(): input_data missing: ", paste(missing_logger, collapse=", "))
  }
  
  logger_data <- input_data |>
    dplyr::filter(.data$site_station_code == select_station) |>
    dplyr::arrange(.data$timestamp)
  if (!nrow(logger_data)) stop("wq_drift_correction(): no rows for '", select_station, "'.")
  
  if (!inherits(logger_data$timestamp, "POSIXct")) {
    logger_data$timestamp <- lubridate::ymd_hms(logger_data$timestamp, tz = "UTC")
    if (any(is.na(logger_data$timestamp))) stop("wq_drift_correction(): bad logger timestamps.")
  }
  
  # Determine logger TZ (from POSIXct or an optional 'timezone' column)
  tz_from_posix <- attr(logger_data$timestamp, "tzone")
  tz_from_col <- NULL
  if ("timezone" %in% names(logger_data)) {
    tz_vals <- stats::na.omit(unique(as.character(logger_data$timezone)))
    if (length(tz_vals)) tz_from_col <- tz_vals[1]
  }
  tz_logger <- normalize_tz(tz_from_posix) %||% normalize_tz(tz_from_col) %||% "UTC"
  
  log_ts   <- logger_data$timestamp
  log_vals <- logger_data[[value_col]]
  has_sec      <- !is.null(secondary_col) && secondary_col %in% names(logger_data)
  log_vals_sec <- if (has_sec) logger_data[[secondary_col]] else NULL
  new_vals_sec <- log_vals_sec
  time_range <- range(log_ts, na.rm = TRUE)
  
  # ---------- reference data (data frame OR file path) ----------
  if (!is.null(ref_data) && is.data.frame(ref_data)) {
    # Use in-memory data frame directly (Shiny mode)
    ref_data_source <- "in-memory"
  } else if (!is.null(ref_data_path)) {
    if (!is.character(ref_data_path) || length(ref_data_path) != 1L) {
      stop("wq_drift_correction(): `ref_data_path` must be a single character path.")
    }
    if (!file.exists(ref_data_path)) stop("wq_drift_correction(): reference file not found: ", ref_data_path)
    ref_data <- utils::read.csv(ref_data_path, stringsAsFactors = FALSE, check.names = FALSE)
    ref_data_source <- basename(ref_data_path)
  } else {
    stop("wq_drift_correction(): must provide either 'ref_data_path' or 'ref_data' as a data frame.")
  }
  
  req_ref_cols <- c("metric","site_station_code","timestamp","watertemp_C","value","unit",
                    "event_type","calibration_datetime")
  missing_ref  <- setdiff(req_ref_cols, names(ref_data))
  if (length(missing_ref)) {
    stop("wq_drift_correction(): reference data missing: ", paste(missing_ref, collapse=", "))
  }
  
  ref_data$._row_id <- seq_len(nrow(ref_data))
  
  ref_data <- ref_data |>
    dplyr::mutate(
      metric_norm_file = dplyr::case_when(
        normalize_string(.data$metric) %in% c("cond","conductivity")   ~ "conductivity",
        normalize_string(.data$metric) %in% c("do","dissolvedoxygen") ~ "dissolvedoxygen",
        normalize_string(.data$metric) %in% c("ph")                   ~ "ph",
        TRUE ~ NA_character_
      ),
      site_station_code = as.character(.data$site_station_code)
    ) |>
    dplyr::filter(.data$metric_norm_file == metric_norm,
                  .data$site_station_code == select_station)
  
  if (!nrow(ref_data)) stop("No refs for '", select_station, "' & metric '", metric, "'.")
  
  # Parse ref times IN LOGGER TZ
  parse_orders <- c("ymd HMS","ymd HM","mdy HMS","mdy HM","dmy HMS","dmy HM")
  ref_data$timestamp <- lubridate::parse_date_time(ref_data$timestamp, orders = parse_orders, tz = tz_logger)
  ref_data$calibration_datetime[
    ref_data$calibration_datetime == "" | trimws(ref_data$calibration_datetime) == ""
  ] <- NA
  ref_data$calibration_datetime <- lubridate::parse_date_time(
    ref_data$calibration_datetime, orders = parse_orders, tz = tz_logger
  )
  if (any(is.na(ref_data$timestamp))) {
    bad <- ref_data$._row_id[is.na(ref_data$timestamp)]
    stop("Bad reference `timestamp` rows: ", paste(bad, collapse=", "))
  }
  
  ref_data$event_type <- tolower(trimws(ref_data$event_type))
  bad_event <- which(!ref_data$event_type %in% c("calibration","check","ignore") | is.na(ref_data$event_type))
  if (length(bad_event)) {
    stop("Invalid `event_type` rows: ", paste(ref_data$._row_id[bad_event], collapse=", "),
         " (allowed: calibration, check, ignore).")
  }
  cal_missing <- which(ref_data$event_type == "calibration" & is.na(ref_data$calibration_datetime))
  if (length(cal_missing)) {
    stop("Calibration rows require `calibration_datetime` on rows: ",
         paste(ref_data$._row_id[cal_missing], collapse=", "))
  }
  noncal_with_cal <- which(ref_data$event_type != "calibration" & !is.na(ref_data$calibration_datetime))
  if (length(noncal_with_cal)) {
    warning("`calibration_datetime` provided on non-cal rows: ",
            paste(ref_data$._row_id[noncal_with_cal], collapse=", "), "; ignored.")
    ref_data$calibration_datetime[noncal_with_cal] <- NA
  }
  
  ref_data$unit_clean <- tolower(trimws(ref_data$unit))
  unit_ok <- ref_data$unit_clean %in% tolower(accepted_units)
  if (!all(unit_ok)) {
    bad <- ref_data$._row_id[!unit_ok]
    stop("Unexpected `unit` for metric '", metric, "' on rows: ", paste(bad, collapse=", "),
         ". Accepted: ", paste(accepted_units, collapse=", "), ".")
  }
  
  ref_data$value <- suppressWarnings(as.numeric(ref_data$value))
  if (anyNA(ref_data$value)) {
    stop("Non-numeric `value` on rows: ", paste(ref_data$._row_id[is.na(ref_data$value)], collapse=", "))
  }
  
  # keep refs if either time intersects window (+/-2h)
  win_start <- time_range[1] - 2*3600
  win_end   <- time_range[2] + 2*3600
  in_window <- function(t) !is.na(t) & t >= win_start & t <= win_end
  ref_data <- ref_data |>
    dplyr::filter(in_window(.data$timestamp) | in_window(.data$calibration_datetime))
  
  if (!nrow(ref_data)) stop("No reference measurements within logger window for '", select_station, "'.")
  
  # ---------- build events ----------
  max_gap_sec <- max_ref_gap_h * 3600
  nearest_idx <- function(target_time) {
    diffs <- abs(as.numeric(log_ts - target_time, units = "secs"))
    if (!length(diffs)) NA_integer_ else which.min(diffs)
  }
  last_before_idx <- function(target_time) {
    idx <- which(log_ts < target_time)
    if (!length(idx)) NA_integer_ else utils::tail(idx, 1L)
  }
  first_at_or_after_idx <- function(target_time) {
    idx <- which(log_ts >= target_time)
    if (!length(idx)) NA_integer_ else idx[1L]
  }
  
  events_list <- list()
  skipped <- list()
  
  for (i in seq_len(nrow(ref_data))) {
    r  <- ref_data[i, ]
    et <- r$event_type
    if (et == "ignore") next
    
    if (et == "check") {
      j_end <- nearest_idx(r$timestamp)
      if (is.na(j_end)) next
      gap <- abs(as.numeric(log_ts[j_end] - r$timestamp, units = "secs"))
      if (!is.finite(gap) || gap > max_gap_sec) {
        skipped[[length(skipped)+1L]] <- list(row=r$._row_id, reason="gap>max", gap_sec=gap,
                                              ref_time=r$timestamp, used_time=log_ts[j_end])
        next
      }
      logger_end <- log_vals[j_end]
      if (is.na(logger_end)) {
        skipped[[length(skipped)+1L]] <- list(row=r$._row_id, reason="NA logger at end")
        next
      }
      
      ratio_end  <- if (logger_end == 0) NA_real_ else r$value / logger_end
      offset_end <- r$value - logger_end
      
      events_list[[length(events_list)+1L]] <- data.frame(
        event_end_time    = log_ts[j_end],
        event_type        = "check",
        ratio_end         = ratio_end,
        offset_end        = offset_end,
        anchor_start_time = log_ts[j_end],
        stringsAsFactors  = FALSE
      )
      
    } else if (et == "calibration") {
      cal_time <- r$calibration_datetime
      j_pre  <- last_before_idx(cal_time)
      j_post <- first_at_or_after_idx(cal_time)
      if (is.na(j_pre) || is.na(j_post)) {
        skipped[[length(skipped)+1L]] <- list(row=r$._row_id, reason="no pre/post sample near cal")
        next
      }
      
      gap <- as.numeric(cal_time - log_ts[j_pre], units = "secs")
      if (!is.finite(gap) || gap > max_gap_sec) {
        skipped[[length(skipped)+1L]] <- list(row=r$._row_id, reason="gap>max (cal-pre)", gap_sec=gap,
                                              ref_time=cal_time, used_time=log_ts[j_pre])
        next
      }
      
      logger_end <- log_vals[j_pre]
      if (is.na(logger_end)) {
        skipped[[length(skipped)+1L]] <- list(row=r$._row_id, reason="NA logger pre-cal")
        next
      }
      
      ratio_end  <- if (logger_end == 0) NA_real_ else r$value / logger_end
      offset_end <- r$value - logger_end
      
      events_list[[length(events_list)+1L]] <- data.frame(
        event_end_time    = log_ts[j_pre],
        event_type        = "calibration",
        ratio_end         = ratio_end,
        offset_end        = offset_end,
        anchor_start_time = log_ts[j_post],
        stringsAsFactors  = FALSE
      )
    }
  }
  
  if (!length(events_list)) {
    if (length(skipped)) {
      msgs <- vapply(skipped, function(s) {
        if (!is.null(s$gap_sec)) {
          paste0("row ", s$row, " skipped (", s$reason, "): gap=",
                 round(s$gap_sec/3600,2), " h; ref=", as.character(s$ref_time), " ", tz_logger,
                 " | nearest logger=", as.character(s$used_time), " ", tz_logger)
        } else paste0("row ", s$row, " skipped (", s$reason, ")")
      }, "")
      warning("No usable events built. Diagnostics:\n", paste("  - ", msgs, collapse="\n"))
    }
    message("wq_drift_correction(): no usable events; returning input unchanged.")
    attr(input_data, "qaqc_log") <- NULL
    return(input_data)
  }
  
  events <- dplyr::bind_rows(events_list) |>
    dplyr::arrange(.data$event_end_time) |>
    dplyr::distinct(.data$event_end_time, .keep_all = TRUE)
  
  # ---------- build forward segments ----------
  segments <- list()
  segments[[length(segments)+1L]] <- list(
    start_time = log_ts[1],
    end_time   = events$event_end_time[1],
    ratio_end  = events$ratio_end[1],
    offset_end = events$offset_end[1]
  )
  if (nrow(events) >= 2) {
    for (k in 1:(nrow(events)-1)) {
      segments[[length(segments)+1L]] <- list(
        start_time = events$anchor_start_time[k],
        end_time   = events$event_end_time[k+1],
        ratio_end  = events$ratio_end[k+1],
        offset_end = events$offset_end[k+1]
      )
    }
  }
  
  # ---------- apply linear drift ----------
  new_vals <- log_vals
  edited   <- rep(FALSE, length(log_vals))
  
  apply_full_target_at <- function(t_end, ratio_end, offset_end) {
    idx <- which(log_ts == t_end & !is.na(log_vals))
    if (!length(idx)) return()
    if (method == "ratio") {
      if (!is.na(ratio_end) && is.finite(ratio_end)) {
        new_vals[idx] <<- log_vals[idx] * ratio_end
        if (has_sec) {
          sec_idx <- idx[!is.na(log_vals_sec[idx])]
          if (length(sec_idx)) new_vals_sec[sec_idx] <<- log_vals_sec[sec_idx] * ratio_end
        }
      }
    } else {
      if (!is.na(offset_end) && is.finite(offset_end)) {
        new_vals[idx] <<- log_vals[idx] + offset_end
        if (has_sec && !is.na(ratio_end) && is.finite(ratio_end)) {
          sec_idx <- idx[!is.na(log_vals_sec[idx])]
          if (length(sec_idx)) new_vals_sec[sec_idx] <<- log_vals_sec[sec_idx] * ratio_end
        }
      }
    }
    edited[idx] <<- TRUE
  }
  
  for (seg in segments) {
    st <- seg$start_time; et <- seg$end_time
    if (is.na(st) || is.na(et)) next
    if (et <= st) { apply_full_target_at(et, seg$ratio_end, seg$offset_end); next }
    
    idx <- which(log_ts > st & log_ts <= et & !is.na(log_vals))
    if (!length(idx)) next
    
    dur <- as.numeric(et - st, units = "secs")
    if (dur <= 0) { apply_full_target_at(et, seg$ratio_end, seg$offset_end); next }
    
    w <- as.numeric(log_ts[idx] - st, units = "secs") / dur
    w[w < 0] <- 0; w[w > 1] <- 1
    
    if (method == "ratio") {
      r_end <- seg$ratio_end
      if (!is.na(r_end) && is.finite(r_end)) {
        r_interp <- 1 + w * (r_end - 1)
        new_vals[idx] <- log_vals[idx] * r_interp
        if (has_sec) {
          sec_idx <- idx[!is.na(log_vals_sec[idx])]
          if (length(sec_idx)) new_vals_sec[sec_idx] <- log_vals_sec[sec_idx] * r_interp[match(sec_idx, idx)]
        }
        edited[idx] <- TRUE
      }
    } else {
      off_end <- seg$offset_end
      r_end   <- seg$ratio_end
      if (!is.na(off_end) && is.finite(off_end)) {
        new_vals[idx] <- log_vals[idx] + w * off_end
        if (has_sec && !is.na(r_end) && is.finite(r_end)) {
          r_interp <- 1 + w * (r_end - 1)
          sec_idx <- idx[!is.na(log_vals_sec[idx])]
          if (length(sec_idx)) new_vals_sec[sec_idx] <- log_vals_sec[sec_idx] * r_interp[match(sec_idx, idx)]
        }
        edited[idx] <- TRUE
      }
    }
  }
  
  # ---------- write back & QA/QC ----------
  logger_data[[value_col]] <- new_vals
  if (has_sec) logger_data[[secondary_col]] <- new_vals_sec
  if (!edit_flag_col %in% names(logger_data)) logger_data[[edit_flag_col]] <- FALSE
  logger_data[[edit_flag_col]] <- logger_data[[edit_flag_col]] | edited
  
  new_cols <- setdiff(names(logger_data), names(input_data))
  if (length(new_cols)) for (nm in new_cols) input_data[[nm]] <- FALSE
  
  input_data <- dplyr::rows_update(input_data, logger_data, by = c("site_station_code","timestamp"))
  edited_ts <- logger_data$timestamp[edited]

  idx_station <- input_data$site_station_code == select_station
  idx_edit    <- idx_station & input_data$timestamp %in% edited_ts
  drift_label <- paste0("DRIFT_", toupper(method))

  if (!any(idx_edit)) {
    message("wq_drift_correction(): no values changed for '", select_station,
            "'. Consider increasing max_ref_gap_h or checking timezone parsing (logger tz=",
            tz_logger, ").")
  }

  log_row <- NULL
  if (any(idx_edit)) {
    ts_edit <- input_data$timestamp[idx_edit]
    log_row <- make_qaqc_log_row(
      timestamps  = ts_edit,
      station     = select_station,
      metric      = metric_log,
      field       = value_col,
      action      = "DRIFT_CORR",
      code        = drift_label,
      action_note = "Forward linear drift; restart at each visit; cal uses pre-cal.",
      manual_note = "AUTOMATIC DRIFT CORRECTION",
      fun_name    = "wq_drift_correction",
      user        = user
    )
    qaqc_log_maybe_append(log_root, select_station, metric_log, log_row)
  }

  attr(input_data, "qaqc_log") <- log_row
  input_data
}