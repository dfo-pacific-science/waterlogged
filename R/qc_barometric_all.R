#' Run barometric QC for all baro stations in metadata
#'
#' Reads all barometric logger files for stations listed in the metadata
#' (where `metric == "barometric"`), applies `qc_barometric()` to each
#' station, writes yearly QC CSVs (v0.1), and returns a combined data
#' frame of all QC'd barometric data.
#'
#' Files are expected to follow the naming pattern
#' `"<site_station_code>_BARO_YYYYMMDD_YYYYMMDD_v*.csv"` and live somewhere
#' under `baro_data_path`.
#'
#' @param baro_data_path Root directory containing processed barometric CSV
#'   files. All subdirectories are searched recursively for files matching
#'   `"<station>_BARO_*.csv"`, where `<station>` comes from the metadata.
#'   For example: `"data/testing/processed"`.
#' @param metadata_path Path to the metadata CSV used by QC_metadata(),
#'   e.g. `"data/testing/raw/testing_metadata.csv"`. Must include
#'   `site_station_code`, `metric`, and barometric station definitions
#'   (`metric == "barometric"`).
#' @param path_to_output_folder Root directory where yearly QC barometric
#'   files will be written. Year- and `processed`-subfolders are created as
#'   needed, e.g. `"data/testing/processed"`.
#' @param log_root Root folder where QC logs are written by
#'   [qc_barometric()] and related functions, typically the same as
#'   `path_to_output_folder`, e.g. `"data/testing/processed"`.
#' @param temp_low_limit_C Lower bound (C) for plausible air temperatures.
#'   Values below this are set to \code{NA} in \code{airtemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_AT}.
#'   Default is -40 C.
#' @param temp_high_limit_C Upper bound (C) for plausible air temperature.
#'   Values above this are set to \code{NA} in \code{airtemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_AT}.
#'   Default is 50 C.
#' @param pressure_low_limit_kpa Lower bound (kPa) for plausible barometric pressure.
#'   Values below this are set to \code{NA} in \code{airpress_kPa_adj} and flagged as \code{TRUE} in \code{OUTLIER_AP}.
#'   Default is 85 kPa.
#' @param pressure_high_limit_kpa Upper bound (kPa) for plausible barometric pressure.
#'   Values above this are set to \code{NA} in \code{airpress_kPa_adj} and flagged as \code{TRUE} in \code{OUTLIER_AP}.
#'   Default is 105 kPa.
#' @param spike_flag_kpa Pressure value (kPa) for flagging sudden pressure
#'   spikes between consecutive timesteps (absolute difference in kPa).
#'   Values are flagged for review as \code{TRUE} in \code{FLAG_SPIKE}.
#'   Default is 1.5 kPa.
#' @param flatline_n Integer; number of consecutive identical values required
#'   to flag a potential flatline in the pressure series.
#'   Values are flagged for review as \code{TRUE} in \code{FLAG_FLATLINE}.
#'   Default is 6.
#' @param user Character username written into the QC log. Defaults to
#'   \code{NULL}, which resolves to \code{Sys.info()[["user"]]} (the
#'   local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param metadata Optional data frame of station metadata (as returned by
#'   \code{\link{QAQC_metadata}}). When supplied, \code{metadata_path} is
#'   ignored and this data frame is used directly.
#' @param baro_data Optional data frame of pre-loaded barometric logger
#'   data. When supplied, \code{baro_data_path} is ignored and stations
#'   are filtered from this data frame instead of reading CSV files.
#'
#'
#' @return A single long-format data frame created by binding the QC'd
#'    barometric data for all stations. If no stations are processed
#'   successfully, an empty tibble is returned and a warning is issued.
#'
#' @seealso [qc_barometric()], [add_nearest_baro()]
#'
#' @export
qc_barometric_all <- function(
    baro_data_path = NULL,
    metadata_path = NULL,
    path_to_output_folder = NULL,
    log_root = NULL,
    temp_low_limit_C     = -40,
    temp_high_limit_C    = 50,
    pressure_low_limit_kpa   = 85,
    pressure_high_limit_kpa  = 105,
    spike_flag_kpa = 1.5,
    flatline_n          = 6,
    user = NULL,
    metadata = NULL,
    baro_data = NULL
) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  metadata <- QAQC_metadata(metadata_path = metadata_path, metadata = metadata)

  baro_sites <- metadata |>
    dplyr::filter(.data$metric == "barometric") |>
    dplyr::pull(.data$site_station_code) |>
    unique()

  if (!length(baro_sites)) {
    stop("qc_barometric_all(): No barometric stations found in metadata ",
         "(metric == 'barometric').", call. = FALSE)
  }

  all_baro_list <- list()

  for (stn in baro_sites) {
    message("\n--- Barometric QC for station: ", stn, " ---")

    if (!is.null(baro_data) && is.data.frame(baro_data)) {
      raw_baro <- baro_data |>
        dplyr::filter(.data$site_station_code == stn)
      if (!nrow(raw_baro)) {
        warning("  No baro data for station ", stn, " in baro_data. Skipping.")
        next
      }
    } else {
      pattern <- paste0("^", escape_regex(stn), "_BARO_.*\\.csv$")
      file_paths <- list.files(
        baro_data_path,
        pattern    = pattern,
        full.names = TRUE,
        recursive  = TRUE
      )

      if (!length(file_paths)) {
        warning("  No BARO files found for station ", stn,
                " under ", baro_data_path, ". Skipping.")
        next
      }

      raw_baro <- dplyr::bind_rows(
        lapply(file_paths, utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
      )
    }

    # ensure timestamp
    if (!"timestamp" %in% names(raw_baro)) {
      ts_cand <- intersect(
        c("timestamp", "Timestamp", "Date Time", "datetime", "date_time"),
        names(raw_baro)
      )
      if (!length(ts_cand)) {
        warning("  No timestamp column found for station ", stn, ". Skipping.")
        next
      }
      raw_baro <- dplyr::rename(raw_baro, timestamp = !!rlang::sym(ts_cand[1]))
    }
    if (!inherits(raw_baro$timestamp, "POSIXct")) {
      raw_baro$timestamp <- lubridate::ymd_hms(raw_baro$timestamp, tz = "UTC")
    }

    # ensure site_station_code
    if (!"site_station_code" %in% names(raw_baro)) {
      raw_baro$site_station_code <- stn
    }

    # run single-station QC (this keeps all original cols + *_adj + baro_qaqc_*)
    checked <- qc_barometric(
      input_data         = raw_baro,
      select_station     = stn,
      log_root           = log_root,
      user               = user,
      temp_low_limit_C     = temp_low_limit_C,
      temp_high_limit_C    = temp_high_limit_C,
      pressure_low_limit_kpa   = pressure_low_limit_kpa,
      pressure_high_limit_kpa  = pressure_high_limit_kpa,
      spike_flag_kpa = spike_flag_kpa,
      flatline_n          = flatline_n
    )

    # simple printed summary (not returned)
    n_tot        <- nrow(checked)
    n_spike      <- sum(checked$edit_spike,      na.rm = TRUE)
    n_flat       <- sum(checked$edit_flatline,    na.rm = TRUE)
    n_press_high <- sum(checked$edit_press_high,  na.rm = TRUE)
    n_press_low  <- sum(checked$edit_press_low,   na.rm = TRUE)
    n_temp       <- sum(checked$edit_temp_range,  na.rm = TRUE)
    n_any        <- sum(checked$edit_spike | checked$edit_flatline |
                        checked$edit_press_high | checked$edit_press_low |
                        checked$edit_temp_range, na.rm = TRUE)

    message("  Total rows:      ", n_tot)
    message("  Any flags:       ", n_any)
    message("    SPIKE:         ", n_spike)
    message("    FLATLINE:      ", n_flat)
    message("    PRESSURE_HIGH: ", n_press_high)
    message("    PRESSURE_LOW:  ", n_press_low)
    message("    TEMP_RANGE:    ", n_temp)

    # store for combined return
    all_baro_list[[stn]] <- checked

    # ---- write yearly v0.1 BARO files (adjusted, guarded) ----
    if (!is.null(path_to_output_folder)) {
      checked_year <- checked |>
        dplyr::mutate(year = lubridate::year(.data$timestamp))

      years_i <- sort(unique(checked_year$year))

      for (yy in years_i) {
        sub <- checked_year |>
          dplyr::filter(.data$year == yy) |>
          dplyr::arrange(.data$timestamp)

        if (!nrow(sub)) next

        start_date <- min(lubridate::as_date(sub$timestamp))
        end_date   <- max(lubridate::as_date(sub$timestamp))

        start_str <- gsub("\\D", "", as.character(start_date))
        end_str   <- gsub("\\D", "", as.character(end_date))

        year_dir <- file.path(path_to_output_folder, yy)
        proc_dir <- file.path(year_dir, "processed")
        if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE)
        if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

        sub_write <- sub |>
          dplyr::select(-year)
        sub_write$timestamp <- format(sub_write$timestamp, tz = "UTC", usetz = FALSE)

        out_name <- sprintf("%s_BARO_%s_%s_v0.1.csv", stn, start_str, end_str)
        out_path <- file.path(proc_dir, out_name)

        utils::write.csv(sub_write, out_path, row.names = FALSE)
        message("  QC BARO written: ", basename(out_path))
      }
    }
  }

  # ---- RETURN: combined QC’d BARO data (bind_logger_files-style long DF) ----
  if (!length(all_baro_list)) {
    warning("qc_barometric_all(): No stations processed successfully; returning empty tibble.")
    return(tibble::tibble())
  }

  combined <- dplyr::bind_rows(all_baro_list)
  rownames(combined) <- NULL
  combined
}
