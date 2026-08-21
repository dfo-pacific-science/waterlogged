#' Automatic QAQC for water level logger data
#'
#' Applies a suite of automated quality-control rules to water level and
#' water temperature time series for a single station. Raw values are never
#' modified; instead, adjusted copies (\code{waterlevel_m_adj},
#' \code{watertemp_C_adj}) are created and Boolean flag columns are added
#' for review.
#'
#' The following rules are applied:
#' \describe{
#'   \item{\code{NEGATIVE_WT}}{Water temperature below \code{-1}
#'     degrees C removed (\code{NA}); between \code{-1} and 0
#'     degrees C corrected to 0.}
#'   \item{\code{FLAG_ICE}}{Water temperature < 0.3 degrees C flagged
#'     for possible ice conditions.}
#'   \item{\code{FLAG_SPIKE}}{Consecutive water level step change
#'     > 0.1 m flagged as potential logger disturbance.}
#'   \item{\code{FLAG_DRY}}{Possible dry sensor: water temperature
#'     > 19 degrees C, water pressure <= air pressure.}
#' }
#'
#' @param input_data A data frame of water level observations. Must contain
#'   the columns: \code{site_station_code}, \code{timestamp} (POSIXct or
#'   parseable by \code{lubridate::ymd_hms()}), \code{waterlevel_m},
#'   \code{watertemp_C}, \code{waterpress_kPa}, \code{airpress_kPa}, and
#'   \code{airtemp_C}. Typically the output of
#'   \code{\link{convert_waterlevel_kPa_m}} or
#'   \code{\link{add_nearest_baro}}.
#' @param select_station Character scalar. The \code{site_station_code} to
#'   process. Only rows matching this station are retained.
#' @param temp_ice_flag_C Upper bound of water temperature (C) for
#'   flagging ice conditions. Temperatures between \code{temp_low_limit_C}
#'   and \code{temp_ice_flag_C} are flagged. Must be positive and greater
#'   than \code{temp_low_limit_C}. Values are flagged for review as \code{TRUE} in \code{FLAG_ICE}. Default is 0.3 C.
#' @param temp_dry_flag_C Water temperature (C) above which a dry sensor is
#'   suspected. Values are flagged for review as \code{TRUE} in \code{FLAG_DRY}.
#'   Default is 19.
#' @param wl_spike_flag_m Absolute step-change value (m) between
#'   consecutive water level readings above which a spike is flagged.
#'   Values are flagged for review as \code{TRUE} in \code{FLAG_SPIKE}.
#'   Default is 0.1 (m).
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
#'     \item \strong{Adjusted columns:} \code{waterlevel_m_adj},
#'       \code{watertemp_C_adj}.
#'     \item \strong{Flag columns:} \code{FLAG_SPIKE},
#'       \code{OUTLIER_WT}, \code{FLAG_ICE},
#'       \code{FLAG_DRY}.
#'     \item \strong{Attribute:} \code{attr(result, "qaqc_log")} -- a tibble
#'       of QAQC log rows summarising every rule that fired.
#'   }
#'
#' @seealso \code{\link{qc_dissox}} (analogous QAQC for dissolved oxygen),
#'   \code{\link{qc_conductivity}} (analogous for conductivity),
#'   \code{\link{adjust_waterlevel_spike}} (downstream spike removal),
#'   \code{\link{adjust_logger_NA}} (downstream manual adjustment),
#'   \code{\link{convert_waterlevel_kPa_m}} (upstream WL conversion)
#'
#' @export
qc_waterlevel <- function(input_data,
                            select_station,
                            temp_ice_flag_C  = 0.3,
                            temp_dry_flag_C       = 19,
                            wl_spike_flag_m = 0.1,
                            log_root = NULL,
                            user = NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # 1. Basic checks ------------------------------------------------------------
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "waterlevel_m",
    "watertemp_C",
    "waterpress_kPa",
    "airpress_kPa",
    "airtemp_C"
  )

  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "qc_waterlevel(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # 2. Subset to station + init adjusted cols ----------------------------------
  output_data <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(output_data) == 0) {
    stop("qc_waterlevel(): no rows found for station '", select_station, "'.")
  }

  # Ensure timestamp is POSIXct (just in case)
  if (!inherits(output_data$timestamp, "POSIXct")) {
    output_data$timestamp <- lubridate::ymd_hms(output_data$timestamp, tz = "UTC")
  }

  # Create _adj columns if missing — preserves upstream corrections (e.g. wl_set_datum)
  if (!"waterlevel_m_adj" %in% names(output_data))
    output_data$waterlevel_m_adj <- output_data$waterlevel_m
  if (!"watertemp_C_adj" %in% names(output_data))
    output_data$watertemp_C_adj  <- output_data$watertemp_C

  # Pre-compute median timestep for spike gap filtering (scalar, not a column)
  med_step_secs <- as.numeric(
    stats::median(diff(output_data$timestamp), na.rm = TRUE),
    units = "secs"
  )

  # 3. Apply QA/QC rules (WL + WT) --------------------------------------------
  output_data <- output_data |>
    dplyr::mutate(
      # --- spike detection (lag-based: flags the first disturbed row) ---
      wl_diff_abs = c(NA_real_, abs(diff(waterlevel_m))),
      wl_dt_secs  = c(NA_real_, as.numeric(diff(timestamp), units = "secs")),

      FLAG_SPIKE = !is.na(wl_diff_abs) &
        !is.na(wl_dt_secs) &
        wl_dt_secs <= 2 * med_step_secs &
        wl_diff_abs > wl_spike_flag_m,

      # --- water temperature helpers (from old qc_watertemp) ---
      wt_high_dry   = !is.na(watertemp_C) & watertemp_C > temp_dry_flag_C,
      FLAG_ICE      = !is.na(watertemp_C) & watertemp_C < temp_ice_flag_C,
      neg_lt_minus1 = !is.na(watertemp_C) & watertemp_C < -1,
      neg_between   = !is.na(watertemp_C) & watertemp_C < 0 & watertemp_C >= -1,

      # combine temperature-based ice flags

      OUTLIER_WT = neg_lt_minus1 | neg_between,

      # old WL-based dry rule, updated to 19 deg C to match qc_watertemp doc
      flag_dry_wl = !is.na(waterlevel_m) & !is.na(watertemp_C) &
        !is.na(waterpress_kPa) & !is.na(airpress_kPa) &
        (watertemp_C > temp_dry_flag_C | waterpress_kPa <= airpress_kPa),

      # combine temperature-based dry signals
      flag_dry_temp = wt_high_dry,

      # final dry flag (WL + WT signals) - make it WL-specific
      FLAG_DRY = flag_dry_wl | flag_dry_temp,

      # adjust temperature based on negatives
      watertemp_C_adj = dplyr::case_when(
        neg_lt_minus1 ~ NA_real_,  # remove clearly impossible temps
        neg_between   ~ 0,         # small negatives corrected to 0 deg C
        TRUE          ~ watertemp_C_adj
      )
    ) |>
    # drop only internal helpers we don't want in the output
    dplyr::select(
      -wl_diff_abs, -wl_dt_secs,
      -wt_high_dry,
      -neg_lt_minus1, -neg_between,
      -flag_dry_wl, -flag_dry_temp
    )

  # 4. Build QA/QC log rows from flags ----------------------------------------
  metric      <- "WL"
  action      <- "AUTO_QAQC"
  fun_name    <- "qc_waterlevel"
  manual_note <- "Automatic water level & water temperature QA/QC; no manual note provided."
  run_time    <- Sys.time()

  rules <- list(
    # temperature-related rules (watertemp_C_adj field)
    list(
      flag        = output_data$OUTLIER_WT,
      field       = "watertemp_C_adj",
      code        = "OUTLIER_WT",
      action_note = "Negative water temperature detected; removed (< -1 C) or corrected to 0C (-1 to 0 C)."
    ),
    list(
      flag        = output_data$FLAG_ICE %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "FLAG_ICE",
      action_note = paste0("Possible ice conditions: water temperature < ",temp_ice_flag_C," C; flagged for review.")
    ),
    # WL-related rules (waterlevel_m_adj field)
    list(
      flag        = output_data$FLAG_SPIKE %in% TRUE,
      field       = "waterlevel_m_adj",
      code        = "FLAG_SPIKE",
      action_note = paste0("Large step change in water level (> ", wl_spike_flag_m,
                           " m); flagged as potential logger disturbance.")
    ),
    list(
      flag        = output_data$FLAG_DRY %in% TRUE,
      field       = "waterlevel_m_adj",
      code        = "FLAG_DRY",
      action_note = paste0("Possible dry sensor (water temperature > ", temp_dry_flag_C," C or low pressure); flagged.")
    )
  )

  logs_list <- lapply(rules, function(r) {
    make_qaqc_log_row(
      timestamps  = output_data$timestamp[r$flag],
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

  log_this <- dplyr::bind_rows(logs_list)

  # 5. If nothing flagged, still log that QAQC ran -----------------------------
  if (nrow(log_this) == 0) {
    log_this <- dplyr::tibble(
      station        = select_station,
      metric         = metric,
      field          = "waterlevel_m_adj",
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic water level & water temperature QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = as.POSIXct(NA_real_, tz = "UTC"),
      ts_end         = as.POSIXct(NA_real_, tz = "UTC"),
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }

  # 6. Append to on-disk log (conditionally) ------------------------------------
  qaqc_log_maybe_append(log_root, select_station, metric, log_this)

  attr(output_data, "qaqc_log") <- log_this
  return(output_data)
}
