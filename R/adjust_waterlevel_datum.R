#' Set an arbitrary datum for water-level time series
#'
#' Creates a consistent arbitrary elevation system for a station by
#' anchoring the first valid surveying visit's primary benchmark (PBM) at
#' a user-specified elevation (default 100.0 m) and translating all other
#' benchmarks using within-visit survey
#' differences.  If later visits use a different PBM, its arbitrary
#' elevation is inferred from the existing benchmark network (it is
#' \emph{not} forced to 100.0 m).
#'
#' A single constant datum offset is computed from the first valid visit's
#' mean water-surface elevation and applied to the station's logger stage
#' series, producing the column \code{wl_elev_m}.
#'
#' @param input_data A data frame of logger observations (one or more
#'   stations). Must contain \code{site_station_code}, \code{timestamp}
#'   (POSIXct or parseable), and either \code{waterlevel_m_adj} (preferred)
#'   or \code{waterlevel_m}.
#' @param bm_survey_path Character scalar or \code{NULL}. Path to a
#'   benchmark survey CSV file. Required when \code{bm_survey} is
#'   \code{NULL}.
#' @param select_station Character scalar. The \code{site_station_code} to
#'   process.
#' @param log_root Character scalar or \code{NULL}. Root directory for QAQC
#'   log files. When non-\code{NULL}, a log entry is written via
#'   \code{\link{qaqc_log_maybe_append}}. When \code{NULL} (default, Shiny
#'   mode), no file is written.
#' @param user Character scalar. Analyst name recorded in the QAQC log.
#'   Defaults to \code{NULL}, which resolves to
#'   \code{Sys.info()[["user"]]} (the local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param bm_survey A data frame of benchmark survey data (alternative to
#'   \code{bm_survey_path} for Shiny mode). Exactly one of
#'   \code{bm_survey_path} or \code{bm_survey} must be supplied.
#' @param max_anchor_gap_h Numeric scalar. Maximum allowable gap (in hours)
#'   between the anchor visit datetime and the nearest logger sample.
#'   Default \code{1}. Increase for loggers with coarse time steps (e.g.
#'   2-hour intervals).
#' @param bm_tolerance_m Numeric scalar. Benchmark inconsistency threshold
#'   (in metres). A warning is issued if the inferred absolute elevation for
#'   a benchmark differs by more than this amount across visits. Default
#'   \code{0.02}.
#' @param datum_elev_m Numeric scalar. The arbitrary elevation (in metres)
#'   assigned to the first valid visit's primary benchmark. Default
#'   \code{100}. Adjust if you want a different datum origin (e.g. a known
#'   geodetic elevation).
#'
#' @return The full \code{input_data} data frame with two columns
#'   added/updated for \code{select_station}:
#'   \itemize{
#'     \item \code{wl_elev_m} -- water level on the arbitrary datum (m).
#'     \item \code{flag_wl_datum_applied} -- logical flag (\code{TRUE} for
#'       station rows where the datum was applied).
#'   }
#'   \code{attr(result, "qaqc_log")} contains a tibble with the
#'   \code{WL_DATUM_APPLIED} log entry.
#'
#' @details
#' \strong{Benchmark survey data format:}
#'
#' The benchmark survey CSV (or data frame) must contain at minimum:
#' \itemize{
#'   \item \code{site_station_code}
#'   \item \code{survey_date} (e.g. \code{"2025-04-19"} or
#'     \code{"4/19/2025"})
#'   \item \code{survey_time} (e.g. \code{"16:26"} or \code{"16:26:00"})
#'   \item \code{primary_bm} -- must exactly match a \code{bm*} or
#'     \code{TBM*} column name (e.g. \code{"bm1"}, \code{"tbm1"})
#'   \item \code{grade} -- one of \code{"Excellent"}, \code{"Good"},
#'     \code{"OK"}, \code{"Poor"}
#'   \item One or more benchmark columns named \code{bm*} or \code{tbm*}
#'   \item One or more water-surface columns named \code{water_elev_<n>}
#'     (e.g. \code{water_elev_1}, \code{water_elev_2})
#' }
#'
#' All survey values are assumed to be in the same per-visit coordinate
#' system.  The absolute arbitrary elevation for any value is:
#' \code{pbm_abs + (value_raw - pbm_raw)}.
#'
#' \strong{Grade handling:}
#' No filtering by grade is performed, but a warning is issued if the
#' first valid survey visit (the datum anchor) is graded \code{"Poor"}.
#'
#' \strong{Benchmark network:}
#' The function iterates surveys in chronological order. If a visit's PBM
#' is not yet in the network, it is inferred from any co-observed
#' benchmark that \emph{is} known. An error is raised if a visit cannot be
#' connected to the existing network. Benchmark inconsistencies exceeding
#' \code{bm_tolerance_m} across visits trigger a warning.
#'
#' \strong{Timezone handling:}
#' Survey date/time strings are parsed in the logger's timezone (detected
#' from the POSIXct \code{tzone} attribute of \code{input_data$timestamp}
#' or an optional \code{timezone} column). Falls back to \code{"UTC"} if
#' neither is available. Pass logger timestamps as POSIXct with the correct
#' \code{tzone} to ensure accurate matching.
#'
#' @seealso \code{\link{qc_waterlevel}} (upstream automatic QAQC),
#'   \code{\link{convert_waterlevel_kPa_m}} (upstream kPa-to-m conversion),
#'   \code{\link{wl_multipoint_correction}} (water level drift correction)
#'
#' @export
adjust_waterlevel_datum <- function(input_data,
                         bm_survey_path = NULL,
                         select_station,
                         log_root = NULL,
                         user = NULL,
                         bm_survey = NULL,
                         max_anchor_gap_h = 1,
                         bm_tolerance_m   = 0.02,
                         datum_elev_m     = 100) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 0. Basic checks -------------------------------------------------------
  req_cols <- c("site_station_code", "timestamp")
  missing_logger <- setdiff(req_cols, names(input_data))
  if (length(missing_logger) > 0) {
    stop(
      "adjust_waterlevel_datum(): input_data is missing required column(s): ",
      paste(missing_logger, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- detect logger timezone -------------------------------------------------
  # Visit date/time strings are parsed in this TZ to match logger timestamps
  tz_from_posix <- attr(input_data$timestamp, "tzone")
  tz_from_col   <- NULL
  if ("timezone" %in% names(input_data)) {
    tz_vals <- stats::na.omit(unique(as.character(input_data$timezone)))
    if (length(tz_vals)) tz_from_col <- tz_vals[1]
  }
  tz_logger <- normalize_tz(tz_from_posix) %||% normalize_tz(tz_from_col) %||% "UTC"

  # pick stage column (no col-name params)
  stage_col <- NULL
  if ("waterlevel_m_adj" %in% names(input_data)) {
    stage_col <- "waterlevel_m_adj"
  } else if ("waterlevel_m" %in% names(input_data)) {
    stage_col <- "waterlevel_m"
  } else {
    stop(
      "adjust_waterlevel_datum(): input_data must contain `waterlevel_m_adj` or `waterlevel_m`.",
      call. = FALSE
    )
  }

  # subset station
  logger_data <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(logger_data) == 0) {
    stop(
      "adjust_waterlevel_datum(): no rows found for station '", select_station, "'.",
      call. = FALSE
    )
  }

  # timestamp parse if needed
  if (!inherits(logger_data$timestamp, "POSIXct")) {
    logger_data$timestamp <- lubridate::ymd_hms(logger_data$timestamp, tz = "UTC")
    if (any(is.na(logger_data$timestamp))) {
      stop("adjust_waterlevel_datum(): could not parse logger timestamps to POSIXct.", call. = FALSE)
    }
  }

  # ---- 1. Read benchmark surveys (data frame OR file path) --------------------------
  if (!is.null(bm_survey) && is.data.frame(bm_survey)) {
    lv <- bm_survey
  } else if (!is.null(bm_survey_path)) {
    if (!is.character(bm_survey_path) || length(bm_survey_path) != 1L) {
      stop("adjust_waterlevel_datum(): `bm_survey_path` must be a single file path.", call. = FALSE)
    }
    if (!file.exists(bm_survey_path)) {
      stop("adjust_waterlevel_datum(): benchmark survey file not found: ", bm_survey_path, call. = FALSE)
    }
    lv <- utils::read.csv(bm_survey_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("adjust_waterlevel_datum(): must provide either 'bm_survey_path' or 'bm_survey' as a data frame.", call. = FALSE)
  }

  req_lv <- c("site_station_code", "survey_date", "survey_time", "primary_bm", "grade")
  missing_lv <- setdiff(req_lv, names(lv))
  if (length(missing_lv) > 0) {
    stop(
      "adjust_waterlevel_datum(): benchmark survey file is missing required column(s): ",
      paste(missing_lv, collapse = ", "),
      call. = FALSE
    )
  }

  # restrict to station
  lv <- lv |>
    dplyr::filter(site_station_code == select_station)

  if (nrow(lv) == 0) {
    stop("adjust_waterlevel_datum(): no benchmark survey rows for station '", select_station, "'.", call. = FALSE)
  }

  # benchmark + water elevation columns
  bm_cols <- grep("^(bm|tbm)", names(lv), value = TRUE)
  we_cols <- grep("^water_elev_", names(lv), value = TRUE)

  if (length(bm_cols) == 0) {
    stop(
      "adjust_waterlevel_datum(): no benchmark columns found. ",
      "Expected columns starting with 'bm' or 'tbm' (e.g. bm1, tbm1).",
      call. = FALSE
    )
  }
  if (length(we_cols) == 0) {
    stop(
      "adjust_waterlevel_datum(): no water elevation columns found (expected `water_elev_<n>`).",
      call. = FALSE
    )
  }

  # build visit datetime from separate date + time (Excel-friendly)
  # parsed in the logger's detected timezone so timestamps align correctly
  lv$visit_dt <- lubridate::parse_date_time(
    paste(lv$survey_date, lv$survey_time),
    orders = c(
      "ymd HMS", "ymd HM",
      "mdy HMS", "mdy HM",
      "dmy HMS", "dmy HM"
    ),
    tz = tz_logger
  )

  if (any(is.na(lv$visit_dt))) {
    bad <- which(is.na(lv$visit_dt))
    stop(
      "adjust_waterlevel_datum(): could not parse survey_date/survey_time on row(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  # normalize grade text
  lv$grade <- trimws(lv$grade)
  grade_ok <- lv$grade %in% c("Excellent", "Good", "OK", "Poor")
  if (!all(grade_ok)) {
    bad <- which(!grade_ok)
    stop(
      "adjust_waterlevel_datum(): invalid grade on row(s): ",
      paste(bad, collapse = ", "),
      ". Allowed: Excellent, Good, OK, Poor.",
      call. = FALSE
    )
  }

  # primary_bm must match an existing BM/TBM column exactly
  lv$primary_bm <- trimws(lv$primary_bm)
  missing_pbm <- which(!lv$primary_bm %in% bm_cols)
  if (length(missing_pbm) > 0) {
    stop(
      "adjust_waterlevel_datum(): `primary_bm` does not match any bm/tbm column on row(s): ",
      paste(missing_pbm, collapse = ", "),
      ". `primary_bm` must exactly match one of: ",
      paste(bm_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # coerce bm/tbm and water_elev columns to numeric
  for (nm in c(bm_cols, we_cols)) {
    lv[[nm]] <- suppressWarnings(as.numeric(lv[[nm]]))
  }

  lv <- lv |> dplyr::arrange(visit_dt)

  # ---- 2. Choose first valid visit (anchor) ---------------------------------
  # valid = has primary BM value AND at least one water_elev value
  is_valid <- rep(TRUE, nrow(lv))
  for (i in seq_len(nrow(lv))) {
    pbm_col_i <- lv$primary_bm[i]
    pbm_raw_i <- lv[[pbm_col_i]][i]

    we_vals <- unlist(lv[i, we_cols, drop = FALSE])
    we_mean <- mean(we_vals, na.rm = TRUE)

    if (is.na(pbm_raw_i) || is.na(we_mean)) {
      is_valid[i] <- FALSE
    }
  }

  if (!any(is_valid)) {
    stop(
      "adjust_waterlevel_datum(): no valid visits. Need primary_bm value and at least one non-NA water_elev_*.",
      call. = FALSE
    )
  }

  first_idx <- which(is_valid)[1]

  if (lv$grade[first_idx] == "Poor") {
    warning(
      "adjust_waterlevel_datum(): First visit used to define datum is graded 'Poor'. ",
      "All subsequent elevations will be relative to this anchor.",
      call. = FALSE
    )
  }

  # ---- 3. Build benchmark network across visits ------------------------------
  # bm_abs_map: named numeric vector stored as list for easy lookup
  bm_abs_map <- list()


  anchor_pbm_id  <- lv$primary_bm[first_idx]   # e.g., "BM1"
  anchor_pbm_raw <- lv[[anchor_pbm_id]][first_idx]

  bm_abs_map[[anchor_pbm_id]] <- datum_elev_m

  infer_pbm_abs <- function(pbm_id, pbm_raw, visit_row) {
    # If PBM abs isn't known, infer it from any BM/TBM in this visit that IS known.
    for (bm_id in bm_cols) {
      known_abs <- bm_abs_map[[bm_id]]
      bm_raw    <- visit_row[[bm_id]]
      if (!is.null(known_abs) && !is.na(bm_raw)) {
        # bm_abs = pbm_abs + (bm_raw - pbm_raw)  => pbm_abs = bm_abs - (bm_raw - pbm_raw)
        return(known_abs - (bm_raw - pbm_raw))
      }
    }
    NA_real_
  }

  # iterate visits in time order and populate bm_abs_map
  for (i in seq_len(nrow(lv))) {
    visit_row <- lv[i, , drop = FALSE]

    pbm_id_i  <- visit_row$primary_bm
    pbm_raw_i <- visit_row[[pbm_id_i]]

    if (is.na(pbm_raw_i)) next

    pbm_abs_i <- bm_abs_map[[pbm_id_i]]
    if (is.null(pbm_abs_i)) {
      pbm_abs_i <- infer_pbm_abs(pbm_id_i, pbm_raw_i, visit_row)
      if (is.na(pbm_abs_i)) {
        before_anchor <- visit_row$visit_dt < lv$visit_dt[first_idx]
        stop(
          "adjust_waterlevel_datum(): cannot connect visit at ", as.character(visit_row$visit_dt),
          " (", tz_logger, ") to the existing benchmark network.",
          if (isTRUE(before_anchor))
            " This visit precedes the datum anchor -- it must share at least one BM/TBM with the anchor visit."
          else
            "",
          " primary_bm '", pbm_id_i, "' is not yet known and no co-observed BM/TBM is known.",
          call. = FALSE
        )
      }
      bm_abs_map[[pbm_id_i]] <- pbm_abs_i
    }

    # update all BM/TBM abs values seen on this visit
    for (bm_id in bm_cols) {
      bm_raw <- visit_row[[bm_id]]
      if (is.na(bm_raw)) next

      bm_abs_new <- pbm_abs_i + (bm_raw - pbm_raw_i)

      if (!is.null(bm_abs_map[[bm_id]])) {
        if (abs(bm_abs_map[[bm_id]] - bm_abs_new) > bm_tolerance_m) {
          warning(
            "adjust_waterlevel_datum(): benchmark '", bm_id,
            "' has inconsistent inferred elevation across visits (>",
            bm_tolerance_m, " m). ",
            "Keeping first value. Visit time: ", as.character(visit_row$visit_dt),
            " (", tz_logger, ").",
            call. = FALSE
          )
        }
      } else {
        bm_abs_map[[bm_id]] <- bm_abs_new
      }
    }
  }

  # ---- 4. Compute anchor visit water elevation (absolute) --------------------
  anchor_row    <- lv[first_idx, , drop = FALSE]
  anchor_we_raw <- mean(unlist(anchor_row[we_cols]), na.rm = TRUE)

  anchor_pbm_abs <- bm_abs_map[[anchor_pbm_id]]

  # absolute arbitrary water elevation at anchor visit
  anchor_water_abs <- anchor_pbm_abs + (anchor_we_raw - anchor_pbm_raw)

  # ---- 5. Compute constant datum offset using nearest logger sample ----------
  anchor_dt <- anchor_row$visit_dt

  dt_sec <- abs(as.numeric(logger_data$timestamp - anchor_dt, units = "secs"))
  j <- which.min(dt_sec)

  max_anchor_gap_sec <- max_anchor_gap_h * 3600
  if (is.na(dt_sec[j]) || dt_sec[j] > max_anchor_gap_sec) {
    stop(
      "adjust_waterlevel_datum(): anchor visit is too far from the nearest logger sample. ",
      "Anchor visit: ", as.character(anchor_dt), " (", tz_logger, "). ",
      "Nearest logger sample: ", as.character(logger_data$timestamp[j]),
      " (gap = ", round(dt_sec[j] / 3600, 2), " h; max allowed = ",
      max_anchor_gap_h, " h). ",
      "Increase `max_anchor_gap_h` if your logger interval is coarser than ",
      max_anchor_gap_h, " h.",
      call. = FALSE
    )
  }

  stage_at_anchor <- logger_data[[stage_col]][j]
  if (is.na(stage_at_anchor)) {
    stop("adjust_waterlevel_datum(): nearest logger stage is NA at anchor visit; cannot set datum.", call. = FALSE)
  }

  datum_offset_m <- anchor_water_abs - stage_at_anchor

  # ---- 6. Apply datum to station time series --------------------------------
  logger_data$wl_elev_m <- logger_data[[stage_col]] + datum_offset_m

  logger_data$flag_wl_datum_applied <- TRUE

  # ---- ensure columns exist in full input_data with correct types ------------
  # wl_elev_m must be numeric
  if (!"wl_elev_m" %in% names(input_data) || !is.numeric(input_data$wl_elev_m)) {
    input_data$wl_elev_m <- NA_real_
  }

  # flag must be logical
  if (!"flag_wl_datum_applied" %in% names(input_data) || !is.logical(input_data$flag_wl_datum_applied)) {
    input_data$flag_wl_datum_applied <- FALSE
  }

  # bring station rows back into the full dataset
  input_data <- dplyr::rows_update(
    input_data,
    logger_data,
    by = c("site_station_code", "timestamp")
  )

  # ---- 7. QA/QC log ----------------------------------------------------------
  ts_for_log <- c(min(logger_data$timestamp, na.rm = TRUE),
                  max(logger_data$timestamp, na.rm = TRUE))

  log_row <- make_qaqc_log_row(
    timestamps  = ts_for_log,
    station     = select_station,
    metric      = "WL",
    field       = "wl_elev_m",
    action      = "DATUM_SET",
    code        = "WL_DATUM_APPLIED",
    action_note = paste0(
      "Applied arbitrary datum using first valid survey visit (",
      as.character(anchor_dt), "). Primary BM '", anchor_pbm_id,
      "' anchored at ", datum_elev_m, " m. Datum offset = ", round(datum_offset_m, 3),
      " m (wl_elev_m = ", stage_col, " + offset)."
    ),
    manual_note = "AUTOMATIC DATUM SET",
    fun_name    = "adjust_waterlevel_datum",
    user        = user
  )

  qaqc_log_maybe_append(log_root, select_station, "WL", log_row)

  attr(input_data, "qaqc_log") <- log_row
  input_data
}
