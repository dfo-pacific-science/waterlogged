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
#'   \item{\code{OUTLIER_CO}}{Conductivity outside
#'     \code{cond_low_limit_uScm} to \code{cond_high_limit_uScm}; corresponding
#'     values in \code{conduct_uScm_adj} and \code{spc_uScm_adj} are set to
#'     \code{NA}.}
#'   \item{\code{OUTLIER_WT}}{Water temperature outside
#'     \code{temp_low_limit_C} to \code{temp_high_limit_C}; corresponding
#'     values in \code{watertemp_C_adj} and \code{spc_uScm_adj} are set to
#'     \code{NA}.}
#'   \item{\code{FLAG_SPIKE}}{Consecutive conductivity change exceeding
#'     \code{spike_flag_uScm}; flagged for review (no automatic
#'     removal).}
#'   \item{\code{FLAG_ICE}}{Water temperature >= \code{temp_low_limit_C} and
#'     < \code{temp_ice_flag_C} degrees C; flagged for possible ice conditions.}
#'   \item{\code{FLAG_DRY}}{Sliding 12-point window where conductivity <=
#'     \code{dry_flag_uScm}. If air temperature data is available, the
#'     air--water temperature difference must also be <=
#'     \code{temp_air_water_diff_C}. Flagged as probable dry sensor.}
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
#'   \code{\link{qaqc_log_maybe_append}()}. When \code{NULL} (default, Shiny
#'   mode), no file is written.
#' @param user Character scalar. User name recorded in the QAQC log.
#'   Defaults to \code{NULL}, which resolves to
#'   \code{Sys.info()[["user"]]} (the local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param temp_low_limit_C Lower bound (C) for plausible water temperatures.
#'   Values below this are set to \code{NA} in \code{watertemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_WT}.
#'   Default is -1 C.
#' @param temp_high_limit_C Upper bound (C) for plausible water temperature.
#'   Values above this are set to \code{NA} in \code{watertemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_WT}.
#'   Default is 50 C.
#' @param cond_high_limit_uScm Numeric. Upper bound for acceptable conductivity.
#'   Values above this are set to \code{NA} in \code{conduct_uScm_adj} and flagged as \code{TRUE} in \code{OUTLIER_CO}.
#'   Adjust this value depending on logger specifications. Default is 2500 uScm.
#' @param cond_low_limit_uScm Numeric. Lower bound for acceptable conductivity.
#'   Values below this are set to \code{NA} in \code{conduct_uScm_adj} and flagged as \code{TRUE} in \code{OUTLIER_CO}.
#'   Adjust this value depending on logger specifications. Default is 0 uScm.
#' @param temp_ice_flag_C Upper bound of water temperature (\code{°C}) for
#'   flagging ice conditions. Temperatures between \code{temp_low_limit_C}
#'   and \code{temp_ice_flag_C} are flagged. Must be positive and greater
#'   than \code{temp_low_limit_C}. Values are flagged for review as \code{TRUE} in \code{FLAG_ICE}. Default is 0.3 C.
#' @param spike_flag_uScm Numeric. Absolute conductivity change
#'   between consecutive samples that triggers the spike flag.
#'   Values are flagged for review as \code{TRUE} in \code{FLAG_SPIKE}.
#'   Default is 200 uScm.
#' @param dry_flag_uScm Numeric. Conductivity below this value is
#'   considered a probable dry sensor. Default \code{10}.
#' @param temp_air_water_diff_C Numeric. Maximum absolute air-water
#'   temperature difference to support the dry-sensor inference (requires
#'   \code{airtemp_C} column). Default \code{2}.
#'
#' @return The filtered data frame for \code{select_station}, sorted by
#'   \code{timestamp}, with the following additions:
#'   \itemize{
#'     \item \strong{Adjusted columns:} \code{conduct_uScm_adj},
#'       \code{watertemp_C_adj}.
#'     \item \strong{Flag columns:} \code{FLAG_SPIKE},
#'       \code{FLAG_ICE}, \code{OUTLIER_WT}, \code{OUTLIER_CO},
#'       \code{FLAG_DRY}.
#'     \item \strong{Attribute:} \code{attr(result, "qaqc_log")} -- a tibble
#'       of QAQC log rows summarising every rule that fired.
#'   }
#'
#' @seealso \code{\link{conductivity_qaqc_all}} (wrapper that runs this
#'   function across all conductivity stations),
#'   \code{\link{qc_waterlevel}} (analogous QAQC for water level),
#'   \code{\link{qc_dissox}} (analogous for dissolved oxygen),
#'   \code{\link{wq_drift_correction}} (downstream drift correction)
#'
#' @export
qc_conductivity <- function(
    input_data,
    select_station,
    log_root                   = NULL,
    user                       = NULL,
    cond_low_limit_uScm        = 0,
    cond_high_limit_uScm       = 2500,
    temp_low_limit_C           = -1,
    temp_high_limit_C          = 50,
    temp_ice_flag_C            = 0.3,
    spike_flag_uScm            = 200,
    dry_flag_uScm              = 10,
    temp_air_water_diff_C      = 2
) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 0. Validate inputs ---------------------------------------------------
  if (temp_low_limit_C > 0) {
    stop(
      "qc_conductivity(): temp_low_limit_C must be <= 0. "
    )
  }
  if (temp_ice_flag_C <= 0) {
    stop(
      "qc_conductivity(): temp_ice_flag_C must be > 0. "
    )
  }
  if (temp_ice_flag_C <= temp_low_limit_C) {
    stop(
      "qc_conductivity(): temp_ice_flag_C (", temp_ice_flag_C,
      ") must be > temp_low_limit_C (", temp_low_limit_C, ") to form a valid window."
    )
  }


  # ---- 1. Filter & basic checks ---------------------------------------------
  df <- input_data |>
    dplyr::filter(site_station_code == select_station) |>
    dplyr::arrange(timestamp)

  if (nrow(df) == 0) {
    stop("qc_conductivity(): no rows for station ", select_station)
  }

  required <- c("timestamp", "conduct_uScm", "watertemp_C")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "qc_conductivity(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Ensure timestamp is POSIXct
  if (!inherits(df$timestamp, "POSIXct")) {
    df$timestamp <- parse_dt(df$timestamp, "timestamp")
  }

  # ---- 2. Create adjusted columns if missing --------------------------------
  if (!"conduct_uScm_adj" %in% names(df)) {
    df$conduct_uScm_adj <- df$conduct_uScm
  }
  if (!"watertemp_C_adj" %in% names(df)) {
    df$watertemp_C_adj <- df$watertemp_C
  }

  # ---- 3. Detection helpers (use adj as working values) ---------------------
  # Conductivity range
  cond_range <- df$conduct_uScm_adj < cond_low_limit_uScm | df$conduct_uScm_adj > cond_high_limit_uScm

  # Temperature conditions
  temp_range <- df$watertemp_C_adj < temp_low_limit_C | df$watertemp_C_adj > temp_high_limit_C
  temp_ice   <- df$watertemp_C_adj >= temp_low_limit_C & df$watertemp_C_adj < temp_ice_flag_C

  # Conductivity spike (use adj)
  diff_cond  <- c(NA_real_, diff(df$conduct_uScm_adj))
  spike      <- abs(diff_cond) > spike_flag_uScm

  # Air-water support for dry inference (optional)
  if ("airtemp_C_adj" %in% names(df)) {
    df$air_watertemp_diff <- abs(df$airtemp_C_adj - df$watertemp_C_adj)
  } else if ("airtemp_C" %in% names(df)) {
    df$air_watertemp_diff <- abs(df$airtemp_C - df$watertemp_C_adj)
  } else {
    df$air_watertemp_diff <- NA_real_
  }


  # ---- 4. Apply *temperature* adjustments; cond remains as-is ---------------

  # If watertemp_C_adj is set to NA, then apply the same rule to spc_uScm_adj,
  # since it is derived from watertemp_C
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("watertemp_C_adj", "spc_uScm_adj")),
        ~ dplyr::case_when(
          temp_range ~ NA_real_,
          .default = .x
        )
      ),
      dplyr::across(
        dplyr::any_of(c("conduct_uScm_adj", "spc_uScm_adj")),
        ~ dplyr::case_when(
          cond_range ~ NA_real_,
          .default = .x
        )
      ),

      # Keep key flags for plotting / diagnostics
      FLAG_SPIKE            = spike,
      FLAG_ICE              = temp_ice,
      OUTLIER_WT            = temp_range,
      OUTLIER_CO            = cond_range
    )

  # ---- 5. 12-point dry flag (FLAG_DRY) using adj CO -----------------------
  n <- nrow(df)
  df$FLAG_DRY <- FALSE

  # Check if the dataset has air temp
  has_air_temp <- any(!is.na(df$air_watertemp_diff))

  if (n >= 12) {
    if (has_air_temp) {
      # RULE 1: Air temp is present. Use both conductivity and air temp thresholds.
      for (j in 1:(n - 11)) {
        window_co   <- df$conduct_uScm_adj[j:(j + 11)]
        window_diff <- df$air_watertemp_diff[j:(j + 11)]

        if (any(!is.na(window_co)) && any(!is.na(window_diff))) {
          if (min(window_co, na.rm = TRUE) <= dry_flag_uScm &&
              max(abs(window_diff), na.rm = TRUE) <= temp_air_water_diff_C) {
            df$FLAG_DRY[j:(j + 11)] <- TRUE
          }
        }
      }
    } else {
      # RULE 2: Air temp is absent. Use only the conductivity threshold.
      for (j in 1:(n - 11)) {
        window_co <- df$conduct_uScm_adj[j:(j + 11)]

        if (any(!is.na(window_co))) {
          if (min(window_co, na.rm = TRUE) <= dry_flag_uScm) {
            df$FLAG_DRY[j:(j + 11)] <- TRUE
          }
        }
      }
    }
  }

 df <- df |>
    # Drop purely helper columns we don't need to expose
    dplyr::select(
      -dplyr::any_of(c(
        "air_watertemp_diff",
        "diff_cond", "metric_norm"
      ))
    )



  # ---- 6. Build QA/QC log rows (COND-style rules list) ----------------------
  metric      <- "COND"
  fun_name    <- "qc_conductivity"
  action      <- "AUTO_QAQC"
  manual_note <- "Automatic conductivity QA/QC; no manual note provided."
  run_time    <- Sys.time()

  rules <- list(
    # Spikes (no automatic removal, just flag & log)
    list(
      flag        = df$FLAG_SPIKE %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "FLAG_SPIKE",
      action_note = paste0("Spike > ", spike_flag_uScm, " uS/cm between samples; flagged for review.")
    ),
    # Ice-like temperatures
    list(
      flag        = df$FLAG_ICE %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "FLAG_ICE",
      action_note = paste0("Water temperature >= ", temp_low_limit_C, " and < ", temp_ice_flag_C, " C (likely ice); flagged for review.")
    ),
    # Out-of-range temps (low or high)
    list(
      flag        = (df$OUTLIER_WT) %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "OUTLIER_WT",
      action_note = paste0(
        "Water temperature outside acceptable range (",
        temp_low_limit_C, " to ", temp_high_limit_C, " C): removed."
      )
    ),
    # Out-of-range conductivity (low or high)
    list(
      flag        = (df$OUTLIER_CO) %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "OUTLIER_CO",
      action_note = paste0(
        "Conductivity outside acceptable range (",
        cond_low_limit_uScm, " to ", cond_high_limit_uScm, " uS/cm): removed."
      )
    ),
    # Dry conditions - COND-specific
    list(
      flag        = df$FLAG_DRY %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "FLAG_DRY",
      action_note = if (has_air_temp) {
        paste0(
          "Possible dry sensor: conductivity < ", dry_flag_uScm,
          " uS/cm and air-water temperature difference <= ", temp_air_water_diff_C,
          " C for >= 12 consecutive points."
        )
      } else {
        paste0(
          "Possible dry sensor: conductivity < ", dry_flag_uScm,
          " uS/cm for >= 12 consecutive points (no air temperature data available)."
        )
      }
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

  # ---- 7. Append to on-disk log (conditionally) ------------------------------
  qaqc_log_maybe_append(log_root, select_station, metric, log_this)

  message("Conductivity QAQC completed for: ", select_station)
  attr(df, "qaqc_log") <- log_this
  df
}
