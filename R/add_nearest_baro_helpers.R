
# =============================================================================
# add_nearest_baro_helpers.R
# Internal helper functions for add_nearest_baro()
# =============================================================================


# ── resolve_metrics_param ─────────────────────────────────────────────────────

#' Resolve metric argument for barometric assignment
#'
#' Normalizes and validates the \code{metric} argument used in
#' \code{add_nearest_baro()}, ensuring it is one of \code{"DO"},
#' \code{"waterlevel"}, \code{"conductivity"}, \code{"both"}, or \code{"all"}.
#' Returns a list describing the normalized input and the set of metric keys
#' that require barometric data.
#'
#' @param metrics A single character string specifying which metrics require
#'   barometric data. Accepted values: \code{"DO"}, \code{"waterlevel"},
#'   \code{"conductivity"}, \code{"both"} (waterlevel + DO), or \code{"all"}
#'   (waterlevel + DO + conductivity).
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{metrics_raw}: normalized version of \code{metrics}
#'       (lowercase, no spaces).
#'     \item \code{metrics_need_baro}: character vector of metric keys that
#'       should receive barometric data.
#'   }
#'
#' @keywords internal
resolve_metrics_param <- function(metrics) {
  if (length(metrics) != 1L) {
    stop("`metrics` must be a single value ('DO', 'waterlevel', 'conductivity', 'both', or 'all'), ",
         "not a vector of length ", length(metrics), ".",
         call. = FALSE)
  }

  metrics_raw <- normalize_string(metrics)

  valid_inputs <- c("do", "waterlevel", "conductivity", "both", "all")
  if (!metrics_raw %in% valid_inputs) {
    stop("`metrics` must be one of: 'DO', 'waterlevel', 'conductivity', 'both', or 'all'. You supplied: ",
         metrics, call. = FALSE)
  }

  wl_keys   <- c("waterlevel")
  do_keys   <- c("do", "dissolvedoxygen")
  cond_keys <- c("conductivity", "cond")

  metrics_need_baro <- switch(
    metrics_raw,
    "waterlevel"    = wl_keys,
    "do"            = do_keys,
    "conductivity"  = cond_keys,
    "both"          = c(wl_keys, do_keys),
    "all"           = c(wl_keys, do_keys, cond_keys)
  )

  list(
    metrics_raw       = metrics_raw,
    metrics_need_baro = metrics_need_baro
  )
}


# ── calibrate_baro_series ─────────────────────────────────────────────────────

#' Calibrate and stitch multiple barometric series
#'
#' Adjusts one or more barometric pressure time series to the scale of a
#' reference series using simple linear regression, and returns adjusted
#' columns along with regression diagnostics. Typically used internally by
#' \code{add_nearest_baro()} to place multiple barometric stations onto a
#' common reference scale.
#'
#' @param wide_press A wide data frame containing a \code{timestamp} column
#'   and one column per barometric station specified in \code{baro_cols}.
#' @param baro_cols Character vector of column names in \code{wide_press}
#'   representing barometric series.
#' @param ref_col Name of the reference barometric column in \code{wide_press}
#'   to which all other series will be aligned.
#' @param min_overlap Integer; minimum number of overlapping points required
#'   between the reference and an alternate series before regression-based
#'   adjustment is considered reliable. A warning is issued if overlap is
#'   fewer than this.
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{wide_press}: the input data frame with additional
#'       \code{"<baro_col>_adj"} columns containing adjusted series, including
#'       an adjusted copy of the reference column.
#'     \item \code{reg_summary}: a data frame summarizing the regression used
#'       for each alternate series (columns include \code{alt_col},
#'       \code{ref_col}, \code{n_overlap}, \code{intercept}, \code{slope},
#'       and \code{r2}).
#'   }
#'
#' @keywords internal
calibrate_baro_series <- function(wide_press,
                                  baro_cols = NULL,
                                  ref_col   = NULL,
                                  min_overlap = 20L) {

  alt_cols <- setdiff(baro_cols, ref_col)

  # Container for regression diagnostics
  reg_summary <- vector("list", length(alt_cols))
  names(reg_summary) <- alt_cols

  for (alt in alt_cols) {

    # ---- Identify overlapping rows (both ref and alt non-NA) ----
    overlap      <- !is.na(wide_press[[ref_col]]) & !is.na(wide_press[[alt]])
    numb_overlap <- sum(overlap)

    if (numb_overlap == 0L) {
      warning("No overlap between ", ref_col, " and ", alt,
              "; ", alt, "_adj is unadjusted copy of ", alt, ".")

      wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]]

      reg_summary[[alt]] <- list(
        alt_col   = alt,
        ref_col   = ref_col,
        n_overlap = 0L,
        intercept = NA_real_,
        slope     = NA_real_,
        r2        = NA_real_
      )
      next
    }

    if (numb_overlap < min_overlap) {
      warning("Only ", numb_overlap, " overlapping points between ",
              ref_col, " and ", alt,
              "; regression-based adjustment may be unstable.")
    }

    df_ovlp <- data.frame(
      ref = wide_press[[ref_col]][overlap],
      alt = wide_press[[alt]][overlap]
    )

    fit   <- stats::lm(ref ~ alt, data = df_ovlp)
    coefs <- stats::coef(fit)
    intercept <- unname(coefs[1])
    slope     <- unname(coefs[2])

    # ---- Guard against degenerate regression (e.g. zero-variance alt) ----
    if (is.na(slope)) {
      warning("Regression failed for ", alt, " vs ", ref_col,
              " (likely zero variance); ", alt, "_adj is unadjusted copy.")

      wide_press[[paste0(alt, "_adj")]] <- wide_press[[alt]]

      reg_summary[[alt]] <- list(
        alt_col   = alt,
        ref_col   = ref_col,
        n_overlap = numb_overlap,
        intercept = NA_real_,
        slope     = NA_real_,
        r2        = NA_real_
      )
      next
    }

    r2 <- summary(fit)$r.squared

    # Adjust alt onto ref scale: adj = intercept + slope * alt
    adj_col <- paste0(alt, "_adj")
    wide_press[[adj_col]] <- intercept + slope * wide_press[[alt]]

    reg_summary[[alt]] <- list(
      alt_col   = alt,
      ref_col   = ref_col,
      n_overlap = numb_overlap,
      intercept = intercept,
      slope     = slope,
      r2        = r2
    )
  }

  # Reference gets its own _adj copy (identity — no adjustment)
  wide_press[[paste0(ref_col, "_adj")]] <- wide_press[[ref_col]]

  # Turn reg_summary into a data frame
  reg_summary_df <- purrr::map_dfr(reg_summary, ~ as.data.frame(.x))

  list(
    wide_press  = wide_press,
    reg_summary = reg_summary_df
  )
}


# ── read_baro_station ────────────────────────────────────────────────────────

#' Read and standardize barometric data for a single station
#'
#' Retrieves barometric pressure and air temperature data for one station,
#' either from an in-memory data frame or from CSV files on disk. Timestamps
#' are coerced to POSIXct (UTC). Prefers \code{_adj} columns when available.
#'
#' @param baro_stn Character scalar. The \code{site_station_code} of the
#'   barometric station to read.
#' @param baro_data A data frame of barometric observations, or \code{NULL}.
#'   When non-\code{NULL}, the function filters to rows matching
#'   \code{baro_stn}.
#' @param baro_data_path Character scalar. Directory to search for BARO CSV
#'   files (recursive). Used only when \code{baro_data} is \code{NULL}.
#'
#' @return A data frame with columns \code{timestamp} (POSIXct, UTC),
#'   \code{airpress_kPa} (numeric), and \code{airtemp_C} (numeric, or
#'   \code{NA_real_} if unavailable). Returns \code{NULL} if the station
#'   cannot be read (no files, no data, or missing pressure column).
#'
#' @keywords internal
read_baro_station <- function(baro_stn, baro_data = NULL, baro_data_path = NULL) {

  # ---- Source the data: in-memory df or CSV files ----
  if (!is.null(baro_data) && is.data.frame(baro_data)) {
    file <- baro_data |>
      dplyr::filter(.data$site_station_code == baro_stn)
    if (!nrow(file)) return(NULL)
  } else {
    baro_pattern <- paste0("^", escape_regex(baro_stn), "_BARO_.*\\.csv$")
    file_paths <- list.files(
      baro_data_path,
      pattern    = baro_pattern,
      full.names = TRUE,
      recursive  = TRUE
    )
    if (!length(file_paths)) return(NULL)
    file <- dplyr::bind_rows(
      lapply(file_paths, utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
    )
  }

  # ---- Identify timestamp column ----
  if (!"timestamp" %in% names(file)) {
    ts_cand <- intersect(
      c("timestamp", "Timestamp", "Date Time", "datetime", "date_time"),
      names(file)
    )
    if (!length(ts_cand)) return(NULL)
    file <- dplyr::rename(file, timestamp = !!rlang::sym(ts_cand[1]))
  }

  # ---- Coerce timestamp to POSIXct ----
  if (!inherits(file$timestamp, "POSIXct")) {
    file$timestamp <- lubridate::ymd_hms(file$timestamp, tz = "UTC")
  }

  # ---- Select preferred columns (_adj over raw) ----
  press_col <- if ("airpress_kPa_adj" %in% names(file)) "airpress_kPa_adj" else "airpress_kPa"
  temp_col  <- if ("airtemp_C_adj"    %in% names(file)) "airtemp_C_adj"    else "airtemp_C"

  if (!press_col %in% names(file)) return(NULL)

  result <- dplyr::tibble(
    timestamp    = file$timestamp,
    airpress_kPa = file[[press_col]]
  )

  result$airtemp_C <- if (temp_col %in% names(file)) file[[temp_col]] else NA_real_

  result
}


# ── detect_timestamp_offset ──────────────────────────────────────────────────

#' Check for constant timestamp offset between site and baro data
#'
#' After joining site data to barometric data by timestamp, checks whether
#' missing values in the joined pressure column are caused by a constant
#' offset between the two timestamp grids. Issues a warning with the detected
#' offset (suggesting \code{resample_timestamps()}) or a general mismatch
#' percentage.
#'
#' @param joined_airpress Numeric vector. The \code{airpress_kPa} column from
#'   the join result (used to detect NA rate).
#' @param site_timestamps POSIXct vector. Timestamps from the site data.
#' @param baro_timestamps POSIXct vector. Timestamps from the baro composite.
#' @param site_code Character scalar. Site identifier for warning messages.
#'
#' @return Called for side effects (warnings). Returns \code{invisible(NULL)}.
#'
#' @keywords internal
detect_timestamp_offset <- function(joined_airpress,
                                    site_timestamps,
                                    baro_timestamps,
                                    site_code) {

  pct <- pct_missing(joined_airpress)
  if (is.na(pct) || pct == 0) return(invisible(NULL))

  # ---- Find nearest baro timestamp to start of site data ----
  start_ts <- min(site_timestamps)
  after    <- baro_timestamps[baro_timestamps > start_ts]
  before   <- baro_timestamps[baro_timestamps < start_ts]

  has_after  <- length(after)  > 0
  has_before <- length(before) > 0

  # Guard: no baro timestamps on either side (shouldn't happen, but safe)
  if (!has_after && !has_before) {
    warning(sprintf(
      "add_nearest_baro(): Baro data didnt match timestamps for %.1f%% of timestamps for site %s.",
      pct, site_code
    ), call. = FALSE)
    return(invisible(NULL))
  }

  # ---- Compute candidate offsets, guarding against empty subsets ----
  offset_after  <- if (has_after)  min(after)  - start_ts else as.difftime(Inf, units = "secs")
  offset_before <- if (has_before) max(before) - start_ts else as.difftime(Inf, units = "secs")

  # Pick the smaller absolute offset
  offset <- if (abs(as.numeric(offset_before, units = "secs")) <
                abs(as.numeric(offset_after,  units = "secs"))) {
    offset_before
  } else {
    offset_after
  }

  # ---- Test: shift baro by -offset and check alignment with site ----
  shifted   <- baro_timestamps - offset
  n_matched <- sum(site_timestamps %in% shifted)

  if (n_matched == length(site_timestamps)) {
    # All site timestamps matched after shifting: constant offset detected
    offset_mins <- round(as.numeric(offset, units = "mins"), 2)
    warning(
      "add_nearest_baro(): Baro data is offset by ", offset_mins,
      " mins and doesnt match timestamps for site ", site_code,
      ". Try running resample_timestamps() before adding nearest baro.",
      call. = FALSE
    )
  } else {
    # Partial/no match: general mismatch
    shifted_pct <- round((1 - n_matched / length(site_timestamps)) * 100, 1)
    warning(sprintf(
      "add_nearest_baro(): Baro data didnt match timestamps for %.1f%% of timestamps for site %s.",
      shifted_pct, site_code
    ), call. = FALSE)
  }

  invisible(NULL)
}


# ── pct_missing ──────────────────────────────────────────────────────────────

#' Compute percentage of NA values in a vector
#'
#' @param x A vector.
#' @return Numeric scalar: percentage of \code{NA} values, rounded to 1
#'   decimal place. Returns \code{NA_real_} when \code{x} is \code{NULL} or
#'   zero-length.
#'
#' @keywords internal
pct_missing <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  round(mean(is.na(x)) * 100, 1)
}


# ── metric_norm_to_code ──────────────────────────────────────────────────────

#' Map normalized metric name to short file-naming code
#'
#' Used when writing v0.2 CSV filenames (e.g. \code{"waterlevel"} -> \code{"WL"}).
#'
#' @param metric_norm_value Character scalar. Normalized metric name.
#' @return Character scalar: short metric code for file naming.
#'
#' @keywords internal
metric_norm_to_code <- function(metric_norm_value) {
  code_map <- list(
    "waterlevel"      = "WL",
    "wl"              = "WL",
    "do"              = "DO",
    "dissolvedoxygen" = "DO",
    "conductivity"    = "COND",
    "cond"            = "COND"
  )
  if (metric_norm_value %in% names(code_map)) {
    code_map[[metric_norm_value]]
  } else {
    toupper(metric_norm_value)
  }
}
