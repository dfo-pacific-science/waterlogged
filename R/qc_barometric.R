#' QA/QC a single barometric pressure time series
#'
#' Applies automatic QA/QC rules to a barometric logger time series for one
#' station: removes out-of-range pressures and temperatures, flags spikes
#' and flatlines, and writes QA/QC log entries. Returns the input data with
#' adjusted columns (`airpress_kPa_adj`, `airtemp_C_adj`) and boolean edit
#' flags.
#'
#' @param input_data Data frame of barometric logger data containing at least
#'   `timestamp`, `site_station_code`, `airpress_kPa` and `airtemp_C`, possibly
#'   for multiple stations.
#' @param select_station Character, station code to process
#'   (matches `site_station_code`).
#' @param log_root Root folder where QA/QC logs are written by
#'   [qaqc_log_append()], typically the same root used elsewhere in the
#'   workflow (e.g. `"data/testing/processed"`).
#' @param user Character username written into the QA/QC log. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]} (the
#'   local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param temp_low_limit Lower bound (C) for plausible barometric temperatures.
#'   Values outside `[temp_low_limit, temp_high_limit]` are set to `NA` in
#'   `airtemp_C_adj` and flagged.
#' @param temp_high_limit Upper bound (C) for plausible barometric temperatures.
#' @param pressure_low_kpa Lower bound (kPa) for plausible barometric pressure.
#'   Values below this are set to `NA` in `airpress_kPa_adj` and flagged. Default is 51 kPa.
#' @param pressure_high_kpa Upper bound (kPa) for plausible barometric pressure.
#'   Values above this are set to `NA` in `airpress_kPa_adj` and flagged. Default is 112 kPa.
#' @param spike_threshold_kpa Threshold (kPa) for flagging sudden pressure
#'   spikes between consecutive timesteps (absolute difference in kPa). Default is 1.5 kPa.
#' @param flatline_n Integer; number of consecutive identical values required
#'   to flag a potential flatline in the pressure series. Default is 6.
#'
#' @details
#' If `airpress_kPa_adj` or `airtemp_C_adj` do not exist, they are created as
#' copies of the raw `airpress_kPa` / `airtemp_C` columns and then modified
#' by the QA/QC rules. Edit flags (`OUTLIER_AP`,
#' `OUTLIER_AT`, `FLAG_DISTURBANCE`, `FLAG_FLATLINE`) are created if missing
#' and set to `TRUE` where each rule applies.
#'
#' For each rule that triggers, a corresponding QA/QC log entry is appended via
#' [make_qaqc_log_row()] and [qaqc_log_append()], using metric `"BARO"` and
#' field `"airpress_kPa_adj"`.
#'
#' @return A data frame containing only rows for `select_station`, sorted by
#'   `timestamp`, with updated `*_adj` columns and edit flags. The input is not
#'   modified in-place.
#'
#' @seealso [barometric_qaqc_all()], [add_nearest_baro()]
#'
#' @export
qc_barometric <- function(input_data,
                            select_station,
                            log_root = NULL,
                            user = NULL,
                            temp_low_limit  = -40,
                            temp_high_limit = 50,
                            pressure_low_kpa  = 51,
                            pressure_high_kpa = 112,
                            spike_threshold_kpa = 1.5,   # sudden >1.5 kPa jump
                            flatline_n = 6               # 6 consecutive identical values
) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  # ---- 1. Filter & basic checks ---------------------------------------------
  df <- input_data |>
    dplyr::filter(.data$site_station_code == select_station) |>
    dplyr::arrange(.data$timestamp)

  if (nrow(df) == 0) {
    stop("No rows for station ", select_station, call. = FALSE)
  }

  required <- c("timestamp", "airpress_kPa", "airtemp_C")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  # Create adjusted columns if missing
  if (!"airpress_kPa_adj" %in% names(df)) df$airpress_kPa_adj <- df$airpress_kPa
  if (!"airtemp_C_adj"   %in% names(df)) df$airtemp_C_adj   <- df$airtemp_C

  # Create edit flags if missing
  flags <- c("OUTLIER_AP","FLAG_FLATLINE",
             "FLAG_DISTURBANCE","OUTLIER_AT")
  for (f in flags) if (!f %in% names(df)) df[[f]] <- FALSE


  # ---- 2. Detection Rules ---------------------------------------------------
  press <- df$airpress_kPa
  temp  <- df$airtemp_C

  # Temperature outside realistic range (NA-safe: NA → FALSE)
  bad_temp <- !is.na(temp) & (temp < temp_low_limit | temp > temp_high_limit)

  # Pressure outside expected terrestrial range (NA-safe)
  bad_press_low  <- !is.na(press) & press < pressure_low_kpa
  bad_press_high <- !is.na(press) & press > pressure_high_kpa

  # Spikes: sudden jump > threshold (first element → FALSE, NA gaps → FALSE)
  diff_press <- c(NA_real_, abs(diff(press)))
  is_spike   <- !is.na(diff_press) & diff_press > spike_threshold_kpa

  # Flatline detection (n identical values in a row)
  # Replace NAs with a sentinel so rle() doesn't treat NA runs as flatlines
  press_for_rle <- press
  press_for_rle[is.na(press_for_rle)] <- Inf
  rle_vals    <- rle(press_for_rle)
  is_flatline <- rep(rle_vals$lengths >= flatline_n, rle_vals$lengths)
  # Don't flag actual NA runs

  is_flatline[is.na(press)] <- FALSE


  # ---- 3. Apply Adjustments -------------------------------------------------
  df <- df |>
    dplyr::mutate(
      # Remove obviously wrong values
      airpress_kPa_adj = dplyr::if_else(
        bad_press_low | bad_press_high, NA_real_, .data$airpress_kPa_adj
        # bad_press, NA_real_, .data$airpress_kPa_adj
      ),
      airtemp_C_adj = dplyr::if_else(
        bad_temp, NA_real_, .data$airtemp_C_adj
      ),

      # Preserve any existing TRUE flags from prior calls
      OUTLIER_AP = dplyr::if_else(bad_press_high, TRUE, .data$OUTLIER_AP),
      OUTLIER_AP  = dplyr::if_else(bad_press_low,  TRUE, .data$OUTLIER_AP),
      OUTLIER_AT = dplyr::if_else(bad_temp,        TRUE, .data$OUTLIER_AT),
      FLAG_DISTURBANCE      = dplyr::if_else(is_spike,        TRUE, .data$FLAG_DISTURBANCE),
      FLAG_FLATLINE   = dplyr::if_else(is_flatline,     TRUE, .data$FLAG_FLATLINE)
    )


  # ---- 4. Log Entries -------------------------------------------------------
  metric <- "BARO"
  field  <- "airpress_kPa_adj"

  events <- list(
    list(name="PRESSURE_HIGH", idx=which(bad_press_high),
         note=paste0("Pressure above ", pressure_high_kpa, " kPa removed")),
    list(name="PRESSURE_LOW", idx=which(bad_press_low),
         note=paste0("Pressure below ", pressure_low_kpa, " kPa removed")),
    list(name="TEMP_OUT_RANGE", idx=which(bad_temp),
         note=paste0("Temperature outside [", temp_low_limit,", ", temp_high_limit, "] C")),
    list(name="SPIKE", idx=which(is_spike),
         note=paste0("Spike > ", spike_threshold_kpa, " kPa detected")),
    list(name="FLATLINE", idx=which(is_flatline),
         note=paste0("Flatline >= ", flatline_n, " identical readings"))
  )

  log_rows_list <- list()
  for (ev in events) {
    if (length(ev$idx) > 0) {
      ts_vals <- df$timestamp[ev$idx]

      log_row <- make_qaqc_log_row(
        timestamps = ts_vals,
        station    = select_station,
        metric     = metric,
        field      = field,
        action     = ev$name,
        code       = ev$name,
        action_note= ev$note,
        manual_note= "AUTOMATIC QC",
        fun_name   = "qc_barometric",
        user       = user
      )

      if (!is.null(log_row)) {
        log_rows_list[[length(log_rows_list) + 1L]] <- log_row
      }
    }
  }

  log_this <- dplyr::bind_rows(log_rows_list)
  qaqc_log_maybe_append(log_root, select_station, metric, log_this)

  message("Barometric QAQC completed for: ", select_station)

  message("") # add line break in console

  attr(df, "qaqc_log") <- log_this
  return(df)
}
