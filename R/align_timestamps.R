#' Align logger timestamps to a regular interval
#'
#' This function standardises the `timestamp` column onto a regular time
#' grid (e.g., 5, 10, 15, 30 minutes, 1 hour) using rounding, flooring,
#' or ceiling. The original logger times are preserved in `timestamp_raw`
#' (created once if missing).
#'
#' Importantly, this function **does not change the sampling frequency**:
#' it does not drop or average rows. If your logger is at 5-minute
#' resolution and you specify `interval = "30 min"`, all 5-minute rows
#' are kept; their timestamps are simply moved to the nearest 30-minute
#' grid line. This may result in multiple rows sharing the same
#' `timestamp`.
#'
#' For true resampling/aggregation (e.g., converting 5-minute data to
#' 30-minute means), use a separate aggregation function.
#'
#' If `log_root`, `metric`, and `select_station` are provided, a QA/QC log
#' entry is written using the package logging helpers.
#'
#' @param input_data A data frame containing at least a `timestamp` column.
#'   If `select_station` is used, a `site_station_code` column is also
#'   required.
#' @param interval Character string giving the target time interval, passed
#'   to \code{lubridate::round_date()}, e.g. `"5 min"`, `"10 min"`,
#'   `"15 min"`, `"30 min"`, `"1 hour"`.
#' @param align How to align timestamps to the grid. One of `"round"`,
#'   `"floor"`, `"ceiling"`. See \code{lubridate::round_date()}.
#' @param select_station Optional station code. If supplied and
#'   `site_station_code` exists, the function will operate only on that
#'   station's rows. The full input data frame (all stations) is always
#'   returned.
#' @param metric Optional character code for the metric (e.g. `"WL"`,
#'   `"DO"`, `"COND"`, `"BARO"`), used only for QA/QC logging.
#' @param log_root Optional path to the root QA/QC log folder. If provided
#'   together with `metric` and `select_station`, a log entry is appended
#'   for this operation.
#' @param user Username recorded in QA/QC logs. Defaults to \code{NULL},
#'   which resolves to \code{Sys.info()[["user"]]} (the local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#'
#' @return A data frame with:
#'   \itemize{
#'     \item \code{timestamp_raw}: original logger timestamps (created if
#'       missing).
#'     \item \code{timestamp}: aligned timestamps on the specified grid.
#'   }
#'
#'   All other columns are returned unchanged.
#'
#' @examples
#' \dontrun{
#' aligned_wl <- align_timestamps(
#'   input_data     = wl_bound,
#'   interval       = "15 min",
#'   align          = "round",
#'   select_station = "ALBR_ST_30",
#'   metric         = "WL",
#'   log_root       = "data/processed"
#' )
#' }
#'
#' @export
align_timestamps <- function(input_data,
                                interval       = "15 min",
                                align          = c("round", "floor", "ceiling"),
                                select_station = NULL,
                                metric         = NULL,
                                log_root       = NULL,
                                user           = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  align <- match.arg(align)

  if (!"timestamp" %in% names(input_data)) {
    stop("align_timestamps(): input_data must contain a 'timestamp' column.")
  }

  # Optional station filter ----------------------------------------------------
  output_data <- input_data

  if (!is.null(select_station)) {
    if (!"site_station_code" %in% names(input_data)) {
      stop(
        "align_timestamps(): `select_station` provided but ",
        "'site_station_code' column is missing."
      )
    }

    idx_station <- which(input_data$site_station_code == select_station)

    if (length(idx_station) == 0) {
      warning("align_timestamps(): no rows for station ", select_station)
      return(output_data)
    }

    df <- input_data[idx_station, , drop = FALSE]
  } else {
    df <- input_data
  }

  # Ensure POSIXct -------------------------------------------------------------
  if (!inherits(df$timestamp, "POSIXct")) {
    df$timestamp <- lubridate::ymd_hms(df$timestamp, tz = "UTC")
  }

  # Sort by time to make interval check sensible
  df <- df[order(df$timestamp), , drop = FALSE]

  # Preserve original times in timestamp_raw -----------------------------------
  if (!"timestamp_raw" %in% names(df)) {
    df$timestamp_raw <- df$timestamp
  }

  # Warn if requested interval is finer than native logger interval -----------
  if (nrow(df) > 1) {
    # Use raw timestamps if present to reflect logger behaviour
    ts_vec <- df$timestamp_raw
    orig_interval_sec <- stats::median(diff(as.numeric(ts_vec)), na.rm = TRUE)

    target_interval_sec <- as.numeric(lubridate::duration(interval))

    if (!is.na(orig_interval_sec) && !is.na(target_interval_sec) &&
        orig_interval_sec > target_interval_sec) {
      warning(
        "align_timestamps(): The requested interval (", interval,
        ") is finer than the original logger interval (~",
        round(orig_interval_sec / 60, 2), " minutes). ",
        "Timestamps will be alinged to the nearest ", interval,
        " grid, but no new rows will be created and no interpolation will occur.\n"
      )
    }
  }

  # Compute aligned timestamps -------------------------------------------------
  snap_fun <- switch(
    align,
    round   = lubridate::round_date,
    floor   = lubridate::floor_date,
    ceiling = lubridate::ceiling_date
  )

  snapped_ts <- snap_fun(df$timestamp, unit = interval)

  # Warn if snapping created duplicate timestamps
  n_dups <- sum(duplicated(snapped_ts))
  if (n_dups > 0) {
    warning(
      "align_timestamps(): alignment created ", n_dups,
      " duplicate timestamp(s). Consider aggregating before aligning."
    )
  }

  # Identify changed rows (for logging)
  changed_idx <- which(df$timestamp_raw != snapped_ts)

  # Apply aligned timestamps
  df$timestamp <- snapped_ts

  # QA/QC logging --------------------------------------------------------------
  do_logging <- !is.null(log_root) && !is.null(metric) && !is.null(select_station)

  if (do_logging) {
    run_time    <- Sys.time()
    fun_name    <- "align_timestamps"
    manual_note <- "Timestamps aligned to a regular interval."
    action      <- "ALIGN_TIME"
    field       <- "timestamp"

    if (length(changed_idx) > 0) {
      ts_changed <- df$timestamp_raw[changed_idx]

      log_rows <- make_qaqc_log_row(
        timestamps  = ts_changed,
        station     = select_station,
        metric      = metric,
        field       = field,
        action      = action,
        code        = "ALIGN_TIME",
        action_note = paste0(
          "Timestamps aligned to ", interval,
          " using ", align, " alignment."
        ),
        manual_note = manual_note,
        fun_name    = fun_name,
        user        = user,
        run_time    = run_time
      )
    } else {
      log_rows <- make_qaqc_log_row(
        timestamps  = range(df$timestamp_raw),
        station     = select_station,
        metric      = metric,
        field       = field,
        action      = action,
        code        = "ALIGN_TIME",
        action_note = paste0(
          "align_timestamps() ran; timestamps already aligned to ",
          interval, " (no changes applied)."
        ),
        manual_note = manual_note,
        fun_name    = fun_name,
        user        = user,
        run_time    = run_time
      )
    }

    qaqc_log_maybe_append(log_root, select_station, metric, log_rows)
  }

  # Stitch back into full dataset ----------------------------------------------
  if (!is.null(select_station)) {
    output_data[idx_station, ] <- df
  } else {
    output_data <- df
  }

  return(output_data)
}
