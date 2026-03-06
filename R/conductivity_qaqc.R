#' Automatic QAQC for conductivity logger data
#'
#' Applies a suite of automated quality-control rules to conductivity and
#' water temperature time series for a single station. Raw values are never
#' modified; instead, adjusted copies (\code{conduct_uScm_adj},
#' \code{watertemp_C_adj}) are created (or updated if they already exist)
#' and Boolean flag columns are added for review.
#'
#' The following rules are applied:
#' \describe{
#'   \item{\code{FLAG_CO_SPIKE}}{Consecutive conductivity change exceeding
#'     \code{disturbance_threshold_uScm}; flagged for review (no automatic
#'     removal).}
#'   \item{\code{FLAG_ICE}}{Water temperature < 0.1 degrees C; flagged
#'     for possible ice conditions.}
#'   \item{\code{NEGATIVE_WT}}{Water temperature between \code{-1} and 0
#'     degrees C corrected to 0.}
#'   \item{\code{TEMP_RANGE}}{Water temperature outside
#'     \code{temp_low_limit_C} to \code{temp_high_limit_C}; set to
#'     \code{NA}.}
#'   \item{\code{FLAG_CO_DRY}}{Conductivity below
#'     \code{dry_threshold_uScm}, optionally supported by air--water
#'     temperature difference <=
#'     \code{air_water_diff_threshold_C}; flagged as probable dry sensor.}
#' }
#'
#' @param input_data A data frame of conductivity logger observations.
#'   Must contain: \code{site_station_code}, \code{timestamp} (POSIXct or
#'   parseable by \code{lubridate::ymd_hms()}), \code{conduct_uScm}, and
#'   \code{watertemp_C}. Optionally \code{airtemp_C} (used for the dry
#'   inference rule). Typically the output of
#'   \code{\link{conductivity_qaqc_all}} or \code{\link{bind_hobo_files}}.
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
#' @param temp_low_limit_C Numeric. Lower bound for acceptable water
#'   temperature. Values below this are set to \code{NA}. Default
#'   \code{-2}.
#' @param temp_high_limit_C Numeric. Upper bound for acceptable water
#'   temperature. Values above this are set to \code{NA}. Default
#'   \code{40}.
#' @param disturbance_threshold_uScm Numeric. Absolute conductivity change
#'   between consecutive samples that triggers the spike flag. Default
#'   \code{200}.
#' @param dry_threshold_uScm Numeric. Conductivity below this value is
#'   considered a probable dry sensor. Default \code{10}.
#' @param air_water_diff_threshold_C Numeric. Maximum absolute air--water
#'   temperature difference to support the dry-sensor inference (requires
#'   \code{airtemp_C} column). Default \code{2}.
#'
#' @return The filtered data frame for \code{select_station}, sorted by
#'   \code{timestamp}, with the following additions:
#'   \itemize{
#'     \item \strong{Adjusted columns:} \code{conduct_uScm_adj},
#'       \code{watertemp_C_adj}.
#'     \item \strong{Flag columns:} \code{flag_co_spike},
#'       \code{flag_co_temp_ice}, \code{flag_co_temp_low},
#'       \code{flag_co_temp_high}, \code{flag_co_temp_neg_near},
#'       \code{flag_co_dry}.
#'     \item \strong{Attribute:} \code{attr(result, "qaqc_log")} -- a tibble
#'       of QAQC log rows summarising every rule that fired.
#'   }
#'
#' @seealso \code{\link{conductivity_qaqc_all}} (wrapper that runs this
#'   function across all conductivity stations),
#'   \code{\link{waterlevel_qaqc}} (analogous QAQC for water level),
#'   \code{\link{dissox_qaqc}} (analogous for dissolved oxygen),
#'   \code{\link{wq_drift_correction}} (downstream drift correction)
#'
#' @export
conductivity_qaqc <- function(
    input_data,
    select_station,
    log_root = NULL,
    user                       = NULL,
    temp_low_limit_C           = -2,
    temp_high_limit_C          = 40,
    disturbance_threshold_uScm = 200,
    dry_threshold_uScm         = 10,
    air_water_diff_threshold_C = 2
) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  # ---------------------------------------------------------------------------
  # Hard-coded small-temp rules:
  #   * Water temperature < 0.1 deg C is considered possible ICE.
  #   * Slightly negative water temperatures between -1 and 0 deg C are set to 0 deg C.
  # ---------------------------------------------------------------------------
  ice_threshold_C        <- 0.1
  near_zero_temp_window  <- c(-1, 0)
  
  # ---- 1. Filter & basic checks ---------------------------------------------
  df <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)
  
  if (nrow(df) == 0) {
    stop("conductivity_qaqc(): no rows for station ", select_station)
  }
  
  required <- c("timestamp", "conduct_uScm", "watertemp_C")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "conductivity_qaqc(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Ensure timestamp is POSIXct
  if (!inherits(df$timestamp, "POSIXct")) {
    df$timestamp <- lubridate::ymd_hms(df$timestamp, tz = "UTC")
  }
  
  # ---- 2. Create adjusted columns if missing --------------------------------
  if (!"conduct_uScm_adj" %in% names(df)) {
    df$conduct_uScm_adj <- df$conduct_uScm
  }
  if (!"watertemp_C_adj" %in% names(df)) {
    df$watertemp_C_adj <- df$watertemp_C
  }
  
  # ---- 3. Detection helpers (use adj as working values) ---------------------
  # Temperature conditions
  temp_low   <- df$watertemp_C_adj < temp_low_limit_C
  temp_high  <- df$watertemp_C_adj > temp_high_limit_C
  temp_ice   <- df$watertemp_C_adj < ice_threshold_C
  
  near_zero  <- df$watertemp_C_adj >= near_zero_temp_window[1] &
    df$watertemp_C_adj <  near_zero_temp_window[2]
  
  # Conductivity spike (use adj)
  diff_cond  <- c(NA_real_, diff(df$conduct_uScm_adj))
  spike      <- abs(diff_cond) > disturbance_threshold_uScm
  
  # Very low conductivity
  very_low_cond <- df$conduct_uScm_adj < dry_threshold_uScm
  
  # Air-water support for dry inference (optional)
  if ("airtemp_C" %in% names(df)) {
    air_water_close <- abs(df$airtemp_C - df$watertemp_C_adj) <= air_water_diff_threshold_C
  } else {
    air_water_close <- rep(FALSE, nrow(df))
  }
  
  dry_air_based <- very_low_cond & air_water_close
  
  # Combined dry flag (COND-specific)
  flag_co_dry <- very_low_cond | dry_air_based
  
  # ---- 4. Apply *temperature* adjustments; cond remains as-is ---------------
  df <- df |>
    dplyr::mutate(
      watertemp_C_adj = dplyr::case_when(
        near_zero ~ 0,            # small negatives -> 0
        temp_low  ~ NA_real_,     # out-of-range low
        temp_high ~ NA_real_,     # out-of-range high
        TRUE      ~ watertemp_C_adj
      ),
      
      # Keep key flags for plotting / diagnostics
      flag_co_spike         = spike,
      flag_co_temp_ice      = temp_ice,
      flag_co_temp_low      = temp_low,
      flag_co_temp_high     = temp_high,
      flag_co_temp_neg_near = near_zero,
      flag_co_dry           = flag_co_dry
    ) |>
    # Drop purely helper columns we don't need to expose
    dplyr::select(
      -dplyr::any_of(c(
        "air_water_close",
        "diff_cond"
      ))
    )
  
  # ---- 5. Build QA/QC log rows (COND-style rules list) ----------------------
  metric      <- "COND"
  fun_name    <- "conductivity_qaqc"
  action      <- "AUTO_QAQC"
  manual_note <- "Automatic conductivity QA/QC; no manual note provided."
  run_time    <- Sys.time()
  
  rules <- list(
    # Spikes (no automatic removal, just flag & log)
    list(
      flag        = df$flag_co_spike %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "FLAG_CO_SPIKE",
      action_note = paste0("Spike > ", disturbance_threshold_uScm, " \u00b5S/cm between samples; flagged for review.")
    ),
    # Ice-like temperatures
    list(
      flag        = df$flag_co_temp_ice %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "FLAG_ICE",
      action_note = "Water temperature < 0.1 \u00b0C (likely ice); flagged for review."
    ),
    # Slight negatives corrected to 0
    list(
      flag        = df$flag_co_temp_neg_near %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "NEGATIVE_WT",
      action_note = "Water temperature between -1 and 0 \u00b0C corrected to 0 \u00b0C."
    ),
    # Out-of-range temps (low or high)
    list(
      flag        = (df$flag_co_temp_low | df$flag_co_temp_high) %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "TEMP_RANGE",
      action_note = paste0(
        "Water temperature outside acceptable range (",
        temp_low_limit_C, " to ", temp_high_limit_C, " \u00b0C); set to NA."
      )
    ),
    # Dry conditions - COND-specific
    list(
      flag        = df$flag_co_dry %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "FLAG_CO_DRY",
      action_note = paste0(
        "Possible dry sensor: conductivity < ", dry_threshold_uScm,
        " \u00b5S/cm (and, where available, air-water temperatures behave like air exposure)."
      )
    )
  )
  
  logs_list <- lapply(rules, function(r) {
    ts_sel <- df$timestamp[r$flag]
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
      field          = "conduct_uScm_adj",
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic conductivity QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }
  
  # ---- 6. Append to on-disk log (conditionally) ------------------------------
  qaqc_log_maybe_append(log_root, select_station, metric, log_this)

  message("Conductivity QAQC completed for: ", select_station)
  attr(df, "qaqc_log") <- log_this
  df
}
