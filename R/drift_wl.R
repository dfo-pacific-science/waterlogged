#' Apply multi-point linear drift correction to water level
#'
#' This function applies a piecewise-linear drift correction to
#' `waterlevel_m_adj` for a single station. You provide a set of
#' time-stamped drift control points , each with a desired drift value in metres.
#' The function linearly interpolates between successive control points and
#' adds that correction to `waterlevel_m_adj`. Data before the first
#' control point and after the last control point are left unchanged.
#'
#' @param input_data Data frame containing at least:
#'   `site_station_code`, `timestamp`, `waterlevel_m` or `waterlevel_m_adj`.
#' @param select_station Character. Station code to correct.
#' @param log_root Character. Root folder for QA/QC logs.
#' @param drift_points A data frame or tibble with columns:
#'   \describe{
#'     \item{timestamp}{Drift control timestamps (POSIXct or character).
#'       Character strings are parsed in the logger's timezone (detected
#'       from the POSIXct \code{tzone} attribute of \code{input_data$timestamp}
#'       or an optional \code{timezone} column). Falls back to \code{"UTC"}
#'       if neither source is available. Pass a POSIXct with the correct
#'       \code{tzone} to override this detection.}
#'     \item{drift_m}{Numeric drift (m) at each timestamp. Positive means
#'     the logger is reading too low (we will add this amount to bring
#'     the series up).}
#'   }
#'   Must contain at least two rows.
#' @param user Character. Username for the QA/QC log. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]} (the
#'   local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param manual_note Optional character note to include in the log.
#'
#' @return A modified data frame with updated `waterlevel_m_adj` and a new
#'   logical column `edit_wl_drift` (`TRUE` for every row where a non-zero
#'   correction was applied) for `select_station`. All other stations/rows
#'   are unchanged.
#'
#' @details
#' For each interval \eqn{[t_i, t_{i+1}]} between control points, a linear
#' correction is constructed:
#'
#' \deqn{
#'   c(t) = d_i + (d_{i+1} - d_i) * \frac{t - t_i}{t_{i+1} - t_i}
#' }
#'
#' where \eqn{d_i} and \eqn{d_{i+1}} are the drift values (in metres) at
#' times \eqn{t_i} and \eqn{t_{i+1}}. The adjusted water level is then:
#'
#' \deqn{
#'   waterlevel\_m\_adj = waterlevel\_m\_adj + c(t)
#' }
#'
#' No correction is applied outside the range of the earliest and latest
#' control timestamps.
#'
#' A single QA/QC log row is written with code `MULTI_WL_DRIFT` summarising
#' the time window and drift values.
#'
#' @export
wl_multipoint_correction <- function(input_data,
                                      select_station,
                                      log_root = NULL,
                                      drift_points,
                                      user        = NULL,
                                      manual_note = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  
  # ---- basic checks on input_data -------------------------------------------
  required_cols <- c("site_station_code", "timestamp")
  missing_cols  <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "wl_multipoint_correction(): input_data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # ---- detect logger timezone -------------------------------------------------
  # Used when parsing character drift timestamps, matching wq_drift_correction()
  tz_from_posix <- attr(input_data$timestamp, "tzone")
  tz_from_col   <- NULL
  if ("timezone" %in% names(input_data)) {
    tz_vals <- stats::na.omit(unique(as.character(input_data$timezone)))
    if (length(tz_vals)) tz_from_col <- tz_vals[1]
  }
  tz_logger <- normalize_tz(tz_from_posix) %||% normalize_tz(tz_from_col) %||% "UTC"

  # ---- check drift_points ----------------------------------------------------
  if (is.null(drift_points) || nrow(drift_points) < 2) {
    stop("wl_multipoint_correction(): drift_points must have at least 2 rows.")
  }
  
  if (!all(c("timestamp", "drift_m") %in% names(drift_points))) {
    stop("wl_multipoint_correction(): drift_points must have columns 'timestamp' and 'drift_m'.")
  }
  
  # Coerce drift timestamps to POSIXct in the logger's timezone
  if (!inherits(drift_points$timestamp, "POSIXct")) {
    drift_points$timestamp <- lubridate::ymd_hms(drift_points$timestamp, tz = tz_logger)
  }
  
  # Sort drift points by time
  drift_points <- drift_points[order(drift_points$timestamp), ]
  
  # Remove any duplicated timestamps
  dup_ts <- duplicated(drift_points$timestamp)
  if (any(dup_ts)) {
    warning("wl_multipoint_correction(): duplicate timestamps in drift_points removed.")
    drift_points <- drift_points[!dup_ts, ]
  }
  
  if (nrow(drift_points) < 2) {
    stop("wl_multipoint_correction(): need at least 2 unique drift timestamps.")
  }

  # Check for NA drift values
  na_drift <- which(is.na(drift_points$drift_m))
  if (length(na_drift) > 0) {
    stop(
      "wl_multipoint_correction(): drift_points$drift_m has NA at row(s): ",
      paste(na_drift, collapse = ", "),
      ". All drift values must be numeric."
    )
  }

  # ---- subset to station & ensure waterlevel_m_adj --------------------------
  output_data <- input_data
  
  idx_station <- which(output_data$site_station_code == select_station)
  if (length(idx_station) == 0) {
    stop("wl_multipoint_correction(): no rows found for station '", select_station, "'.")
  }
  
  station_df <- output_data[idx_station, ]
  
  # Ensure timestamp is POSIXct
  if (!inherits(station_df$timestamp, "POSIXct")) {
    station_df$timestamp <- lubridate::ymd_hms(station_df$timestamp, tz = "UTC")
  }
  
  # Make sure we have an adjusted column to work on
  if (!"waterlevel_m_adj" %in% names(station_df)) {
    if (!"waterlevel_m" %in% names(station_df)) {
      stop("wl_multipoint_correction(): need 'waterlevel_m' or 'waterlevel_m_adj' in station data.")
    }
    # Initialise on the full dataset first so the stitch-back column counts match
    output_data$waterlevel_m_adj <- output_data$waterlevel_m
    station_df$waterlevel_m_adj  <- station_df$waterlevel_m
  }
  
  # ---- build correction vector (piecewise-linear) ---------------------------
  # Initialise correction = 0 everywhere
  correction <- rep(0, nrow(station_df))
  
  # Only apply correction between min and max drift timestamps
  t_min <- min(drift_points$timestamp, na.rm = TRUE)
  t_max <- max(drift_points$timestamp, na.rm = TRUE)
  
  # If station has no data in that window, just return unchanged
  has_overlap <- any(station_df$timestamp >= t_min & station_df$timestamp <= t_max)
  if (!has_overlap) {
    warning("wl_multipoint_correction(): station data has no overlap with drift_points window; returning unchanged data.")
    output_data[idx_station, ] <- station_df
    return(output_data)
  }
  
  # For each segment [t_i, t_{i+1}], interpolate linearly
  for (i in seq_len(nrow(drift_points) - 1L)) {
    t_start <- drift_points$timestamp[i]
    t_end   <- drift_points$timestamp[i + 1L]
    d_start <- drift_points$drift_m[i]
    d_end   <- drift_points$drift_m[i + 1L]
    
    # indices of rows in this segment
    seg_idx <- which(station_df$timestamp >= t_start &
                       station_df$timestamp <= t_end)
    if (length(seg_idx) == 0) next
    
    t_seg <- station_df$timestamp[seg_idx]
    # normalised time from 0 to 1
    frac <- as.numeric(t_seg - t_start, units = "secs") /
      as.numeric(t_end - t_start, units = "secs")
    
    # linear interpolation: d(t) = d_start + (d_end - d_start) * frac
    correction[seg_idx] <- d_start + (d_end - d_start) * frac
  }
  
  # NOTE: outside [t_min, t_max], correction remains 0 by design (no carry-forward)
  
  # ---- apply correction to waterlevel_m_adj ---------------------------------
  station_df$waterlevel_m_adj <- station_df$waterlevel_m_adj + correction

  # ---- edit flag ------------------------------------------------------------
  # Initialise on the full dataset first so the stitch-back assignment works
  if (!"edit_wl_drift" %in% names(output_data)) output_data$edit_wl_drift <- FALSE
  station_df$edit_wl_drift <- correction != 0

  # ---- write QA/QC log row --------------------------------------------------
  metric   <- "WL"
  fun_name <- "wl_multipoint_correction"
  action   <- "DRIFT_CORRECTION"
  code     <- "MULTI_WL_DRIFT"
  run_time <- Sys.time()
  
  # build a short summary of control points for the log
  drift_summary <- paste0(
    "Multi-point linear drift correction: ",
    "from ", format(t_min, "%Y-%m-%d %H:%M:%S"), " (", drift_points$drift_m[1], " m) ",
    "to ", format(t_max, "%Y-%m-%d %H:%M:%S"), " (", utils::tail(drift_points$drift_m, 1), " m). ",
    "Intermediate points: ",
    paste(
      paste0("[", format(drift_points$timestamp, "%Y-%m-%d %H:%M:%S"),
             " -> ", drift_points$drift_m, " m]"),
      collapse = "; "
    )
  )
  
  if (!is.null(manual_note) && nzchar(manual_note)) {
    action_note <- paste(drift_summary, "User note:", manual_note)
  } else {
    action_note <- drift_summary
  }
  
  log_row <- make_qaqc_log_row(
    timestamps  = c(t_min, t_max),
    station     = select_station,
    metric      = metric,
    field       = "waterlevel_m_adj",
    action      = action,
    code        = code,
    action_note = action_note,
    manual_note = manual_note %||% "Multi-point linear drift correction.",
    fun_name    = fun_name,
    user        = user,
    run_time    = run_time
  )

  qaqc_log_maybe_append(log_root, select_station, metric, log_row)

  message("WL drift correction applied for station ", select_station)

  # ---- stitch back into full dataset ----------------------------------------
  output_data[idx_station, ] <- station_df
  attr(output_data, "qaqc_log") <- log_row
  output_data
}
