#' Run conductivity QC for all conductivity stations in metadata
#'
#' Reads conductivity logger data for every station listed in the metadata
#' (where \code{metric == "conductivity"}), applies
#' \code{\link{qc_conductivity}} to each station, optionally writes
#' per-year v0.1 CSVs, and returns a combined data frame of all QC'd
#' conductivity data.
#'
#' When reading from disk, files are expected to match the pattern
#' \code{"<site_station_code>_<file_suffix>_*.csv"} under
#' \code{cond_data_path} (searched recursively).
#'
#' @param cond_data_path Character scalar or \code{NULL}. Root directory
#'   containing processed conductivity CSV files (searched recursively).
#'   Required when \code{cond_data} is \code{NULL}.
#' @param metadata_path Character scalar or \code{NULL}. Path to the
#'   metadata CSV. Required when \code{metadata} is \code{NULL}. Must
#'   include \code{site_station_code} and \code{metric} columns with
#'   entries where \code{metric == "conductivity"}.
#' @param path_to_output_folder Character scalar or \code{NULL}. Root
#'   directory where per-year v0.1 COND CSVs will be written. When
#'   \code{NULL} (default, Shiny mode), no files are written.
#' @param log_root Character scalar or \code{NULL}. Root directory for
#'   QC log files, passed through to \code{\link{qc_conductivity}}.
#'   When \code{NULL} (default), no log files are written.
#' @param temp_low_limit_C Lower bound (C) for plausible water temperatures.
#'   Values below this are set to \code{NA} in \code{watertemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_WT}.
#'   Default is -2 C.
#' @param temp_high_limit_C Upper bound (C) for plausible water temperature.
#'   Values above this are set to \code{NA} in \code{watertemp_C_adj} and flagged as \code{TRUE} in \code{OUTLIER_WT}.
#'   Default is 50 C.
#' @param cond_high_limit_uScm Numeric. Upper bound for acceptable conductivity.
#'   Values above this are set to \code{NA} and n \code{conduct_uScm_adj} and flagged as \code{TRUE} in \code{OUTLIER_CO}.
#'   Adjust this value depending on logger specifications. Default is 2500 uScm.
#' @param cond_low_limit_uScm Numeric. Lower bound for acceptable conductivity.
#'   Values below this are set to \code{NA} and n \code{conduct_uScm_adj} and flagged as \code{TRUE} in \code{OUTLIER_CO}.
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
#' @param temp_air_water_diff_C Numeric. Maximum absolute air--water
#'   temperature difference to support the dry-sensor inference (requires
#'   \code{airtemp_C} column). Default \code{2}.
#' @param file_suffix Character scalar. Suffix used when building the
#'   filename search pattern (e.g. \code{"COND"} matches
#'   \code{STATION_COND_*.csv}). Default \code{"COND"}.
#' @param user Character scalar. User name recorded in QC logs.
#'   Defaults to \code{NULL}, which resolves to
#'   \code{Sys.info()[["user"]]} (the local OS user).
#'   In Shiny apps, pass the logged-in user explicitly
#'   (e.g. \code{session$user}).
#' @param metadata A data frame of station metadata (alternative to
#'   \code{metadata_path} for Shiny mode). Exactly one of
#'   \code{metadata_path} or \code{metadata} must be supplied.
#' @param cond_data A data frame of conductivity observations (alternative
#'   to \code{cond_data_path} for Shiny mode). Must contain
#'   \code{site_station_code}, \code{timestamp}, \code{conduct_uScm}, and
#'   \code{watertemp_C}. Exactly one of \code{cond_data_path} or
#'   \code{cond_data} must be supplied.
#'
#' @return A single long-format data frame created by binding the QC'd
#'   conductivity data for all stations. If no stations are processed
#'   successfully, an empty tibble is returned with a warning.
#'
#' @seealso \code{\link{qc_conductivity}} (per-station QC called
#'   internally), \code{\link{barometric_qaqc_all}} (analogous batch
#'   wrapper for barometric data),
#'   \code{\link{bind_logger_files}} (upstream data ingestion)
#'
#' @export
qc_conductivity_all <- function(
    cond_data_path          = NULL,
    metadata_path           = NULL,
    path_to_output_folder   = NULL,
    log_root                = NULL,
    cond_low_limit_uScm     = 0,
    cond_high_limit_uScm    = 2500,
    temp_low_limit_C        = -2,
    temp_high_limit_C       = 50,
    spike_flag_uScm         = 200,
    dry_flag_uScm           = 10,
    temp_ice_flag_C            = 0.3,
    temp_air_water_diff_C   = 2,
    file_suffix             = "COND",
    user                    = NULL,
    metadata                = NULL,
    cond_data               = NULL
) {
  if (is.null(user)) user <- Sys.info()[["user"]]

  metadata <- QAQC_metadata(metadata_path = metadata_path, metadata = metadata)

  cond_sites <- metadata |>
    dplyr::filter(metric == "conductivity") |>
    dplyr::pull(site_station_code) |>
    unique()

  if (!length(cond_sites)) {
    stop("qc_conductivity_all(): No conductivity stations found in metadata (metric == 'conductivity').")
  }

  all_cond_list <- list()

  for (stn in cond_sites) {
    message("\n--- Conductivity QC for station: ", stn, " ---")

    if (!is.null(cond_data) && is.data.frame(cond_data)) {
      raw_cond <- cond_data |>
        dplyr::filter(site_station_code == stn)
      if (!nrow(raw_cond)) {
        warning("  No cond data for station ", stn, " in cond_data. Skipping.")
        next
      }
    } else {
      pattern <- paste0("^", escape_regex(stn), "_", file_suffix, "_.*\\.csv$")
      file_paths <- list.files(
        cond_data_path,
        pattern    = pattern,
        full.names = TRUE,
        recursive  = TRUE
      )

      if (!length(file_paths)) {
        warning("  No ", file_suffix, " files found for station ", stn,
                " under ", cond_data_path, ". Skipping.")
        next
      }

      raw_cond <- dplyr::bind_rows(
        lapply(
          file_paths,
          utils::read.csv,
          stringsAsFactors = FALSE,
          check.names      = FALSE
        )
      )
    }

    # Ensure timestamp column exists & is POSIXct
    if (!"timestamp" %in% names(raw_cond)) {
      ts_cand <- intersect(
        c("timestamp", "Timestamp", "Date Time", "datetime", "date_time"),
        names(raw_cond)
      )
      if (!length(ts_cand)) {
        warning("  No timestamp column found for station ", stn, ". Skipping.")
        next
      }
      raw_cond <- dplyr::rename(raw_cond, timestamp = !!rlang::sym(ts_cand[1]))
    }

    if (!inherits(raw_cond$timestamp, "POSIXct")) {
      raw_cond$timestamp <- parse_dt(raw_cond$timestamp, "timestamp")
    }

    # Ensure site_station_code column
    if (!"site_station_code" %in% names(raw_cond)) {
      raw_cond$site_station_code <- stn
    }

    # Ensure core cond columns are present with expected names
    req_cols <- c("conduct_uScm", "watertemp_C")
    missing_req <- setdiff(req_cols, names(raw_cond))
    if (length(missing_req) > 0) {
      warning("  Station ", stn,
              " is missing required column(s) for conductivity: ",
              paste(missing_req, collapse = ", "),
              ". Skipping.")
      next
    }

    # Run single-station QC
    checked <- qc_conductivity(
      input_data               = raw_cond,
      select_station           = stn,
      log_root                 = log_root,
      user                     = user,
      temp_low_limit_C         = temp_low_limit_C,
      temp_high_limit_C        = temp_high_limit_C,
      cond_low_limit_uScm      = cond_low_limit_uScm,
      cond_high_limit_uScm     = cond_high_limit_uScm,
      spike_flag_uScm          = spike_flag_uScm,
      dry_flag_uScm            = dry_flag_uScm,
      temp_ice_flag_C = temp_ice_flag_C,
      temp_air_water_diff_C    = temp_air_water_diff_C
    )

    # Simple printed summary
    n_tot  <- nrow(checked)
    n_flag <- sum(!is.na(checked$cond_qaqc_code))
    n_spike <- sum(checked$cond_qaqc_code == "SPIKE", na.rm = TRUE)
    n_dry   <- sum(grepl("^DRY", checked$cond_qaqc_code), na.rm = TRUE)
    n_temp  <- sum(
      checked$cond_qaqc_code %in% c("TEMP_LOW", "TEMP_HIGH", "NEGATIVE_WT", "ICE"),
      na.rm = TRUE
    )

    message("  Total rows:       ", n_tot)
    message("  Any flags:        ", n_flag)
    message("    SPIKE:          ", n_spike)
    message("    DRY (all):      ", n_dry)
    message("    TEMP / ICE etc: ", n_temp)

    # Store for combined return
    all_cond_list[[stn]] <- checked

    # ---- Write yearly v0.1 COND files (guarded for Shiny) --------------------
    if (!is.null(path_to_output_folder)) {
      checked_year <- checked |>
        dplyr::mutate(year = lubridate::year(timestamp))

      years_i <- sort(unique(checked_year$year))

      for (yy in years_i) {
        sub <- checked_year |>
          dplyr::filter(year == yy) |>
          dplyr::arrange(timestamp)

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

        out_name <- sprintf("%s_%s_%s_%s_v0.1.csv",
                            stn, file_suffix, start_str, end_str)
        out_path <- file.path(proc_dir, out_name)

        utils::write.csv(sub_write, out_path, row.names = FALSE)
        message("  QC ", file_suffix, " written: ", basename(out_path))
      }
    }
  }

  # ---- Combined return ------------------------------------------------------
  if (!length(all_cond_list)) {
    warning("qc_conductivity_all(): No stations processed successfully; returning empty tibble.")
    return(tibble::tibble())
  }

  combined <- dplyr::bind_rows(all_cond_list)
  rownames(combined) <- NULL
  combined
}
