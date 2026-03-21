
#' Set logger values to NA over a time window and log the change
#'
#' Convenience wrapper to set adjusted logger values to \code{NA} over a
#' specified time window (or, optionally, for all data below a dry threshold)
#' for a given station and metric. Typical use cases are removing periods
#' affected by ice, dry conditions, or logger disturbances after visual QA/QC.
#'
#'
#' @param input_data Data frame containing logger data for one or more stations,
#'   including a \code{timestamp} column, \code{site_station_code}, and the
#'   relevant adjusted fields (e.g. \code{waterlevel_m_adj},
#'   \code{do_mgl_adj}, etc.).
#' @param select_station Station code to adjust (value of
#'   \code{site_station_code}).
#' @param metric Character string specifying which metric to adjust. Accepted
#'   values include \code{"waterlevel"}, \code{"dissolvedoxygen"},
#'   \code{"watertemp"}, \code{"barometric"}, \code{"airtemp"},
#'   \code{"conductivity"} and common variants (e.g. \code{"water level"},
#'   \code{"DO"}, \code{"BARO"}, \code{"AT"}).
#' @param reason_to_adjust Character; reason for the adjustment. One of
#'   \code{"ice"}, \code{"dry"}, or \code{"disturbance"}. This controls which
#'   \code{edit_*} flag column is set and how the NA adjustment is applied.
#'   Note: barometric and air temperature metrics only accept
#'   \code{"disturbance"}.
#' @param timestamp_start,timestamp_end Character date-time values defining
#'   the adjustment window, in \code{"YYYY-MM-DD HH:MM:SS"} format, parsed
#'   with \code{lubridate::ymd_hms()}.
#' @param keep_temp Logical; if \code{TRUE}, associated temperature adjustments
#'   are skipped for \code{"ice"} and \code{"disturbance"} events. Ignored for
#'   \code{"dry"} (temperature always removed) and for metrics where no
#'   temperature column is associated (\code{"watertemp"}, \code{"airtemp"}).
#' @param apply_to_all_data Logical; if \code{FALSE} (default), only values
#'   within \code{timestamp_start}–\code{timestamp_end} are set to NA. If
#'   \code{TRUE} and \code{reason_to_adjust = "dry"}, a water level threshold
#'   is derived from the window and applied to the whole record. Requires
#'   a water level column (\code{waterlevel_m_adj} or \code{waterlevel_m}).
#'   Ignored for non-dry reasons and for metrics without water level support
#'   (\code{"watertemp"}, \code{"barometric"}, \code{"airtemp"}).
#' @param manual_note Character; required free-text explanation describing why
#'   this adjustment is being made. Stored in the QA/QC log.
#' @param log_root Root directory where QA/QC logs are stored (e.g.
#'   \code{"data/testing/processed"}). The actual log path is derived from
#'   this root, station, and metric.
#' @param user Character; user name recorded in the log entry. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]} (the
#'   local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#'
#' @return A data frame for the selected station with updated adjusted fields
#'   and edit flags. Two internal helper columns (\code{in_window} and
#'   \code{edit_na}) are removed before returning.
#'
#' @details
#' The function is metric-aware: it only modifies the adjusted fields relevant
#' to the requested \code{metric} and sets a matching \code{edit_*} flag
#' (e.g. \code{edit_remove_ice}, \code{edit_remove_dry},
#' \code{edit_remove_disturbance}). If no rows fall within the time window
#' (or dry threshold) for the given station, a warning is issued and no log
#' entry is written.
#'
#' @seealso \code{\link{qc_waterlevel}}, \code{\link{qc_dissox}},
#'   \code{\link{make_qaqc_log_row}},
#'   \code{\link{qaqc_log_append}}
#'
#' @export
adjust_logger_NA <- function(
    input_data,
    select_station,
    metric,              # "waterlevel", "dissolvedoxygen", "watertemp", "barometric", "conductivity"
    reason_to_adjust,    # "ice", "dry", "disturbance"
    timestamp_start,     # "YYYY-MM-DD HH:MM:SS"
    timestamp_end,       # "YYYY-MM-DD HH:MM:SS"
    keep_temp         = FALSE,
    apply_to_all_data = FALSE,
    manual_note,         # required free-text explanation
    log_root = NULL,     # root folder where /logs lives (NULL for Shiny)
    user = NULL
) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ===========================================================================
  # Metric configuration
  # ===========================================================================
  #
  # Each metric defines:
  #   primary_fields  — adjusted columns to always NA
  #   temp_field      — associated temperature column (NULL if metric IS temp)
  #   log_field       — column name recorded in the QAQC log
  #   allowed_reasons — which reasons are valid for this metric
  #   dry_wl_col      — "self" (use own primary), "waterlevel" (need external

  #                     WL column), or NULL (no global dry support)

  metric_configs <- list(
    WL = list(
      primary_fields  = "waterlevel_m_adj",
      temp_field      = "watertemp_C_adj",
      log_field       = "waterlevel_m_adj",
      allowed_reasons = c("ice", "dry", "disturbance"),
      dry_wl_col      = "self"
    ),
    DO = list(
      primary_fields  = c("do_mgl_adj", "do_percsat_adj"),
      temp_field      = "watertemp_C_do_adj",
      log_field       = "do_mgl_adj",
      allowed_reasons = c("ice", "dry", "disturbance"),
      dry_wl_col      = "waterlevel"
    ),
    WT = list(
      primary_fields  = "watertemp_C_adj",
      temp_field      = NULL,
      log_field       = "watertemp_C_adj",
      allowed_reasons = c("ice", "dry", "disturbance"),
      dry_wl_col      = NULL
    ),
    BARO = list(
      primary_fields  = "airpress_kPa_adj",
      temp_field      = "airtemp_C_adj",
      log_field       = "airpress_kPa_adj",
      allowed_reasons = c("disturbance"),
      dry_wl_col      = NULL
    ),
    AT = list(
      primary_fields  = "airtemp_C_adj",
      temp_field      = NULL,
      log_field       = "airtemp_C_adj",
      allowed_reasons = c("disturbance"),
      dry_wl_col      = NULL
    ),
    COND = list(
      primary_fields  = "conduct_uScm_adj",
      temp_field      = "watertemp_C_adj",
      log_field       = "conduct_uScm_adj",
      allowed_reasons = c("ice", "dry", "disturbance"),
      dry_wl_col      = "waterlevel"
    )
  )

  # Local helper: set columns to NA where condition is TRUE
  apply_na_to_cols <- function(df, cols, condition) {
    for (col in cols) {
      if (col %in% names(df)) {
        df[[col]] <- dplyr::if_else(condition, NA_real_, df[[col]])
      }
    }
    df$edit_na <- dplyr::if_else(condition, TRUE, df$edit_na)
    df
  }

  # ===========================================================================
  # Validate inputs
  # ===========================================================================

  # ---- Reason ----
  allowed_reasons <- c("ice", "dry", "disturbance")
  if (length(reason_to_adjust) != 1L) {
    stop(
      "reason_to_adjust must be a single value, one of: ",
      paste(allowed_reasons, collapse = ", "),
      call. = FALSE
    )
  }
  reason_to_adjust <- tolower(reason_to_adjust)
  if (!reason_to_adjust %in% allowed_reasons) {
    stop(
      "reason_to_adjust must be one of: ",
      paste(allowed_reasons, collapse = ", "),
      ". You provided: ", reason_to_adjust,
      call. = FALSE
    )
  }

  # ---- Manual note ----
  if (missing(manual_note) || trimws(manual_note) == "") {
    stop("adjust_logger_NA(): 'manual_note' must be a non-empty string.", call. = FALSE)
  }

  # ---- Metric normalization (lookup table, not case_when) ----
  metric_aliases <- c(
    "waterlevel" = "WL", "water level" = "WL", "WL" = "WL", "wl" = "WL",
    "dissolvedoxygen" = "DO", "dissolved_oxygen" = "DO", "DO" = "DO", "do" = "DO",
    "watertemp" = "WT", "watertemp_TidbiT" = "WT", "WT" = "WT",
    "temperature" = "WT", "wt" = "WT",
    "barometric" = "BARO", "baro" = "BARO", "BARO" = "BARO",
    "conductivity" = "COND", "cond" = "COND", "EC" = "COND", "COND" = "COND",
    "airtemp" = "AT", "air temperature" = "AT", "AT" = "AT",
    "air_temp" = "AT", "at" = "AT"
  )
  metric_norm <- unname(metric_aliases[metric])
  if (is.na(metric_norm)) {
    stop(
      "adjust_logger_NA(): Unrecognized metric '", metric,
      "'. Expected one of waterlevel, dissolvedoxygen, watertemp, barometric, airtemp, conductivity.",
      call. = FALSE
    )
  }

  cfg <- metric_configs[[metric_norm]]

  # ---- Reason restriction for BARO / AT ----
  if (!reason_to_adjust %in% cfg$allowed_reasons) {
    stop(
      "adjust_logger_NA(): reason '", reason_to_adjust,
      "' is not applicable to ", metric_norm, " loggers. Use 'disturbance'.",
      call. = FALSE
    )
  }

  # ---- Edit flag column ----
#  edit_flag_map <- c(
#    ice         = "edit_remove_ice",
#    dry         = "edit_remove_dry",
#    disturbance = "edit_remove_disturbance"
#  )
#  edit_flag_col <- unname(edit_flag_map[reason_to_adjust])

  # ===========================================================================
  # Subset to station and validate structure
  # ===========================================================================

  if (!all(c("timestamp", "site_station_code") %in% names(input_data))) {
    stop("adjust_logger_NA(): data must contain 'timestamp' and 'site_station_code'.", call. = FALSE)
  }

  df <- input_data |>
    dplyr::filter(.data$site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(df) == 0) {
    stop("adjust_logger_NA(): no rows found for station '", select_station, "'.", call. = FALSE)
  }

  # Check that all primary fields exist
  missing_fields <- setdiff(cfg$primary_fields, names(df))
  if (length(missing_fields)) {
    stop(
      "adjust_logger_NA(): ", metric_norm, " metric expects '",
      paste(missing_fields, collapse = "' and '"), "'.",
      call. = FALSE
    )
  }

  # Ensure temp column exists (initialize as NA if missing)
  if (!is.null(cfg$temp_field) && !cfg$temp_field %in% names(df)) {
    df[[cfg$temp_field]] <- NA_real_
  }

  # ===========================================================================
  # Parse timestamps
  # ===========================================================================

  ts_start <- lubridate::ymd_hms(timestamp_start, tz = "UTC")
  ts_end   <- lubridate::ymd_hms(timestamp_end,   tz = "UTC")

  if (is.na(ts_start) || is.na(ts_end)) {
    stop(
      "Could not parse timestamp_start or timestamp_end with ymd_hms(). ",
      "Use 'YYYY-MM-DD HH:MM:SS' format if passing character values.",
      call. = FALSE
    )
  }
  if (ts_end <= ts_start) {
    stop("timestamp_end must be after timestamp_start.", call. = FALSE)
  }

  # ===========================================================================
  # Internal flags
  # ===========================================================================

  df$in_window <- df$timestamp >= ts_start & df$timestamp <= ts_end
  df$edit_na   <- FALSE

  # Ensure edit_* flag column exists and is logical
#  if (!edit_flag_col %in% names(df)) {
#    df[[edit_flag_col]] <- FALSE
#  } else {
#    if (!is.logical(df[[edit_flag_col]])) {
#      df[[edit_flag_col]] <- as.logical(df[[edit_flag_col]])
#    }
#    df[[edit_flag_col]][is.na(df[[edit_flag_col]])] <- FALSE
#  }

  # ===========================================================================
  # Determine which columns to set to NA
  # ===========================================================================

  cols_to_na <- cfg$primary_fields

  # Include temp field unless keep_temp is TRUE and reason is not dry.
  # For dry: temperature is always removed regardless of keep_temp.
  if (!is.null(cfg$temp_field)) {
    if (reason_to_adjust == "dry" || !keep_temp) {
      cols_to_na <- c(cols_to_na, cfg$temp_field)
    }
  }

  # For COND: also NA spc_uScm_adj (specific conductance) if it exists,
  # since it was derived from conduct_uScm_adj and would be stale.
  if (metric_norm == "COND" && "spc_uScm_adj" %in% names(df)) {
    cols_to_na <- c(cols_to_na, "spc_uScm_adj")
  }

  # ===========================================================================
  # Apply NA adjustment
  # ===========================================================================

  if (reason_to_adjust == "dry" && apply_to_all_data && !is.null(cfg$dry_wl_col)) {

    # ---- Global dry threshold mode ------------------------------------------
    # Derive a water level threshold from the user-specified window, then NA
    # all rows (globally) where water level is at or below that threshold.

    # Determine which water level column to use for the threshold
    if (cfg$dry_wl_col == "self") {
      # WL metric: use own primary adjusted field (guaranteed to exist)
      wl_col <- cfg$primary_fields[1]
    } else {
      # DO / COND: prefer waterlevel_m_adj, fall back to waterlevel_m
      wl_col <- if ("waterlevel_m_adj" %in% names(df)) {
        "waterlevel_m_adj"
      } else if ("waterlevel_m" %in% names(df)) {
        "waterlevel_m"
      } else {
        NULL
      }
    }

    if (is.null(wl_col)) {
      warning(
        "Waterlevel column ('waterlevel_m_adj' or 'waterlevel_m') not found. ",
        "Cannot apply ", metric_norm, " dry adjustment to all data."
      )
      # No modification — "no rows were modified" warning will fire at the end
    } else {
      window_vals <- df[[wl_col]][df$in_window & !is.na(df[[wl_col]])]

      if (!length(window_vals) || is.infinite(max(window_vals))) {
        warning("No non-NA ", wl_col, " in dry window; no ",
                metric_norm, " dry adjustment applied.")
      } else {
        dry_threshold <- max(window_vals)
        is_dry <- !is.na(df[[wl_col]]) & df[[wl_col]] <= dry_threshold
        df <- apply_na_to_cols(df, cols_to_na, is_dry)
      }
    }

  } else {

    # ---- Window-based NA mode -----------------------------------------------
    # Simply NA all target columns within [timestamp_start, timestamp_end].
    df <- apply_na_to_cols(df, cols_to_na, df$in_window)

  }

  # ===========================================================================
  # Update the edit_* flag column
  # ===========================================================================

#  if (any(df$edit_na, na.rm = TRUE)) {
#    df[[edit_flag_col]] <- df[[edit_flag_col]] | df$edit_na
#  }

  # ===========================================================================
  # Build & append QA/QC log entry
  # ===========================================================================

  ts_affected <- df$timestamp[df$edit_na]

  if (length(ts_affected) > 0) {

    fun_name <- "adjust_logger_NA"
    run_time <- Sys.time()

    scope_txt <- if (apply_to_all_data && reason_to_adjust == "dry") {
      "window-based threshold applied to full record"
    } else {
      "fixed time window"
    }

    action_note <- paste0(
      "Set ", cfg$log_field, " to NA for reason '", reason_to_adjust,
      "' using ", scope_txt, " from ", timestamp_start,
      " to ", timestamp_end, "."
    )

    log_row <- make_qaqc_log_row(
      timestamps  = ts_affected,
      station     = select_station,
      metric      = metric_norm,
      field       = cfg$log_field,
      action      = "SET_NA",
      code        = paste0("LOGGER_", toupper(reason_to_adjust)),
      action_note = action_note,
      manual_note = manual_note,
      fun_name    = fun_name,
      user        = user,
      run_time    = run_time
    )

    if (!is.null(log_row)) {
      qaqc_log_maybe_append(log_root, select_station, metric_norm, log_row)
      if (is.null(log_root)) {
        message("adjust_logger_NA(): log_root is NULL; QA/QC log was not written to disk.")
      }
    }
  } else {
    log_row <- NULL
    warning("adjust_logger_NA(): no rows were modified; no log entry written.")
  }

  # ===========================================================================
  # Clean up internal columns and return
  # ===========================================================================

  result <- df |>
#    dplyr::select(-in_window, -edit_na)
  dplyr::select(-in_window)
  attr(result, "qaqc_log") <- log_row
  result
}
