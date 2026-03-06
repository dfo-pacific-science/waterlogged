#' QA/QC log helpers
#'
#' Internal helpers for writing station+metric QA/QC logs to disk.
#' Logs are appended (or created if missing) in `file.path(log_root, "logs")`.
#' Duplicate log entries are prevented when re-running QA/QC functions.
#'
#' @name qaqc_log_helpers
#' @keywords internal
NULL

#' Build path to a station+metric QA/QC log file
#'
#' Logs live in `file.path(log_root, "logs")` and are named:
#' `<station>_<metric>_qaqc_log.csv`.
#'
#' @param log_root Character. Root directory where QA/QC logs are stored.
#' @param station Character. Station code (e.g., "ALBR_ST_30").
#' @param metric Character. Metric label used in the log filename (e.g., "WL", "DO", "COND").
#'
#' @return Character scalar path to the log file.
#' @keywords internal
qaqc_log_path <- function(log_root, station, metric) {
  log_dir  <- file.path(log_root, "logs")
  log_file <- paste0(station, "_", metric, "_qaqc_log.csv")
  file.path(log_dir, log_file)
}

#' Create QA/QC log rows from flagged timestamps
#'
#' Splits `timestamps` into contiguous runs and returns one log row per
#' run.
#'
#' A "gap" is any inter-timestamp interval greater than
#' `gap_factor * median(diff(timestamps))`.
#' When all timestamps are already contiguous (or when the vector has
#' fewer than 3 elements), the function returns a single row — preserving
#' identical behaviour for manual-adjustment callers that pass a
#' continuous range.
#'
#' @param timestamps POSIXct vector of affected timestamps.
#' @param station Character. Station code.
#' @param metric Character. Metric label (e.g., "WL", "DO", "COND").
#' @param field Character. Field/column name that was affected (e.g., "wl_elev_m").
#' @param action Character. High-level action label (e.g., "DATUM_SET", "DRIFT_CORR").
#' @param code Character. QA/QC code (e.g., "WL_DATUM_APPLIED").
#' @param action_note Character. Machine-readable-ish note describing what happened.
#' @param manual_note Character. User/analyst note, if applicable.
#' @param fun_name Character. Function name generating the entry.
#' @param user Character. Analyst identifier/name.
#' @param run_time POSIXct. Run time stamp. Defaults to `Sys.time()`.
#' @param gap_factor Numeric. Multiplier applied to the median time step
#'   to define the gap threshold. Defaults to `1.5`.
#'
#' @return A tibble with one row per contiguous run, or NULL if
#'   `timestamps` is empty / all NA.
#' @keywords internal
make_qaqc_log_row <- function(timestamps,
                              station,
                              metric,
                              field,
                              action,
                              code,
                              action_note,
                              manual_note,
                              fun_name,
                              user,
                              run_time = Sys.time(),
                              gap_factor = 1.5) {
  if (length(timestamps) == 0 || all(is.na(timestamps))) {
    return(NULL)
  }

  ts <- sort(timestamps[!is.na(timestamps)])

  # --- Identify contiguous runs ------------------------------------------------
  # For a single timestamp or a pair, everything is one run.
  if (length(ts) <= 2) {
    run_id <- rep(1L, length(ts))
  } else {
    diffs    <- as.numeric(diff(ts), units = "secs")
    med_step <- stats::median(diffs, na.rm = TRUE)
    # Guard against zero median (all-identical timestamps)
    threshold <- if (med_step > 0) gap_factor * med_step else 1
    # Increment run id wherever the gap exceeds the threshold
    run_id <- c(1L, cumsum(diffs > threshold) + 1L)
  }

  # --- Build one log row per run -----------------------------------------------
  runs <- split(ts, run_id)

  rows <- lapply(runs, function(run_ts) {
    r_start <- min(run_ts)
    r_end   <- max(run_ts)
    tibble::tibble(
      station        = station,
      metric         = metric,
      field          = field,
      action         = action,
      code           = code,
      action_note    = action_note,
      manual_note    = manual_note,
      ts_start       = r_start,
      ts_end         = r_end,
      duration_hours = as.numeric(difftime(r_end, r_start, units = "hours")),
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  })

  dplyr::bind_rows(rows)
}

#' Conditionally append QA/QC log rows (Shiny-compatible wrapper)
#'
#' When `log_root` is non-NULL, writes log rows to disk via
#' [qaqc_log_path()] + [qaqc_log_append()]. When `log_root` is NULL
#' (typical in Shiny), disk I/O is skipped entirely.
#'
#' In both cases the log rows tibble is returned invisibly so callers
#' can attach it as an attribute on the output data frame.
#'
#' @param log_root Character or NULL. Root directory for QA/QC logs.
#' @param station Character. Station code.
#' @param metric Character. Metric label.
#' @param log_rows Data frame/tibble of log rows.
#'
#' @return Invisibly returns `log_rows` (or an empty tibble if input was
#'   NULL / zero-row).
#' @keywords internal
qaqc_log_maybe_append <- function(log_root, station, metric, log_rows) {
  if (is.null(log_rows) || nrow(log_rows) == 0) {
    return(invisible(tibble::tibble()))
  }
  if (!is.null(log_root)) {
    log_path <- qaqc_log_path(log_root, station, metric)
    qaqc_log_append(log_path = log_path, log_rows = log_rows)
    message("QA/QC log updated: ", log_path)
  }
  invisible(log_rows)
}

#' Append QA/QC log rows to disk with deduplication
#'
#' Appends `log_rows` to the log at `log_path`, creating the file if needed.
#' Deduplicates entries so re-running the same QA/QC function does not
#' write repeated identical log rows.
#'
#' Deduplication key:
#' `station, metric, field, action, code, ts_start, ts_end, fun_name`.
#'
#' Time columns (`ts_start`, `ts_end`, `run_at`) are normalised to UTC before
#' being written as character strings, ensuring consistent deduplication
#' regardless of the timezone attribute on the incoming POSIXct values.
#'
#' @param log_path Character. Output path (use [qaqc_log_path()]).
#' @param log_rows Data frame/tibble of log rows (from [make_qaqc_log_row()]
#'   or [dplyr::bind_rows()]).
#'
#' @return Invisibly returns `log_path`.
#' @keywords internal
qaqc_log_append <- function(log_path, log_rows) {

  if (is.null(log_rows) || nrow(log_rows) == 0) {
    return(invisible(log_path))
  }

  # Validate required columns before touching the data
  required <- c("station", "metric", "field", "action", "code",
                "ts_start", "ts_end", "run_at", "fun_name")
  missing_cols <- setdiff(required, names(log_rows))
  if (length(missing_cols) > 0) {
    stop(
      "qaqc_log_append(): log_rows is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  # Coerce time fields to UTC character strings for stable CSV I/O and
  # consistent deduplication across sessions with different timezone attributes.
  log_rows <- log_rows |>
    dplyr::mutate(
      ts_start = format(ts_start, tz = "UTC"),
      ts_end   = format(ts_end,   tz = "UTC"),
      run_at   = format(run_at,   tz = "UTC")
    )

  if (file.exists(log_path)) {
    existing <- utils::read.csv(log_path, stringsAsFactors = FALSE)

    # Warn if on-disk schema differs from new rows (e.g. after a package update)
    if (!setequal(names(existing), names(log_rows))) {
      warning(
        "qaqc_log_append(): existing log has a different column set from new rows. ",
        "Missing from existing: ",
        paste(setdiff(names(log_rows), names(existing)), collapse = ", "), ". ",
        "Extra in existing: ",
        paste(setdiff(names(existing), names(log_rows)), collapse = ", "), "."
      )
    }

    log_out <- dplyr::bind_rows(existing, log_rows) |>
      dplyr::distinct(
        station,
        metric,
        field,
        action,
        code,
        ts_start,
        ts_end,
        fun_name,
        .keep_all = TRUE
      )
  } else {
    log_out <- log_rows
  }

  utils::write.csv(log_out, log_path, row.names = FALSE)

  invisible(log_path)
}
