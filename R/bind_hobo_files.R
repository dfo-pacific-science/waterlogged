#' Bind HOBO logger CSVs and attach site metadata
#'
#' Reads all HOBO CSV files in a folder (or from explicit file paths), links
#' each logger serial number to deployment metadata, trims records to
#' deployment/removal windows, standardises core column names by metric
#' (barometric, waterlevel, DO, TidbiT temperature, conductivity), optionally
#' writes per-station/year v0.1 CSVs, and returns a compiled long data frame
#' plus a quick QC plot.
#'
#' @param path_to_raw_folder Character scalar. Path to the folder containing
#'   raw HOBO CSV files (one or more loggers, all of the same metric). For
#'   example \code{"data/raw/level"} or \code{"data/testing/raw/COND"}.
#'   Required when \code{file_paths} is \code{NULL}.
#' @param path_to_output_folder Character scalar or \code{NULL}. Root folder
#'   where processed v0.1 CSVs will be written. Files are created under
#'   \code{<path_to_output_folder>/<year>/processed/}. When \code{NULL}
#'   (default, Shiny mode), no files are written.
#' @param metadata_path Character scalar. Path to a metadata CSV that has at
#'   least logger serial numbers and deployment/removal information (e.g.
#'   \code{sn}, \code{metric}, \code{site_station_code},
#'   \code{timestamp_deploy}, \code{timestamp_remove}, \code{model}).
#'   Required when \code{metadata} is \code{NULL}.
#' @param timestamp_timezone Character scalar. Time zone used when filling in
#'   open-ended deployments (where \code{timestamp_remove} is \code{NA}).
#'   Defaults to \code{"UTC"}.
#' @param file_paths Character vector of full file paths to HOBO CSV files
#'   (alternative to \code{path_to_raw_folder} for Shiny mode). Exactly one
#'   of \code{path_to_raw_folder} or \code{file_paths} must be supplied.
#' @param metadata A data frame of station metadata (alternative to
#'   \code{metadata_path} for Shiny mode). Must contain the same columns
#'   expected by \code{\link{QAQC_metadata}}. Exactly one of
#'   \code{metadata_path} or \code{metadata} must be supplied.
#'
#' @details
#' The function:
#'
#' \itemize{
#'   \item Reads all \code{*.csv} files in \code{path_to_raw_folder} (or uses
#'     \code{file_paths} directly) via \code{extract_alldata_from_file()} and
#'     binds them into a single data frame.
#'   \item Uses \code{\link{QAQC_metadata}} to clean/standardise the metadata.
#'   \item Assigns \code{site_station_code} to each record based on \code{sn}
#'     and the deployment/removal timestamps.
#'   \item Enforces a single \code{metric} per folder (e.g., all barometric,
#'     all waterlevel, all conductivity).
#'   \item Standardises core column names depending on logger/metric, e.g.
#'     \code{airpress_kPa}, \code{waterpress_kPa}, \code{watertemp_C},
#'     \code{airtemp_C}, \code{do_mgl}, \code{conduct_uScm}, and
#'     \code{conduct_source}.
#'   \item For conductivity (U24) loggers, unifies low/full range columns
#'     into a single \code{conduct_uScm} column and records which range was
#'     used in \code{conduct_source}.
#'   \item When \code{path_to_output_folder} is non-\code{NULL}, writes one
#'     v0.1 CSV per station per year to
#'     \code{<path_to_output_folder>/<year>/processed/} with filenames of the
#'     form \code{<site>_<LOGGER>_<startdate>_<enddate>_v0.1.csv}.
#' }
#'
#' @return A list of length 2:
#' \describe{
#'   \item{\code{[[1]]}}{A data frame of compiled logger data
#'     (\code{sites_compiled}), with \code{site_station_code}, \code{sn},
#'     \code{timestamp}, \code{logger_type}, \code{metric}, and
#'     metric-specific value columns.}
#'   \item{\code{[[2]]}}{A \code{ggplot2} object showing the primary value
#'     column vs time, faceted by \code{site_station_code}, for quick visual
#'     QA/QC.}
#' }
#'
#' @seealso \code{\link{add_nearest_baro}} (downstream, attaches baro data
#'   to produce v0.2), \code{\link{barometric_qaqc_all}} (QAQC baro data
#'   produced by this function),
#'   \code{\link{QAQC_metadata}} (metadata validation)
#'
#' @export
bind_hobo_files <- function(path_to_raw_folder = NULL,
                            path_to_output_folder = NULL,
                            metadata_path = NULL, timestamp_timezone = "UTC",
                            file_paths = NULL, metadata = NULL) {

  # Dual input: file_paths OR path_to_raw_folder (mutually exclusive)
  if (is.null(path_to_raw_folder) && is.null(file_paths)) {
    stop("Must provide either 'path_to_raw_folder' or 'file_paths'.")
  }
  if (!is.null(path_to_raw_folder) && !is.null(file_paths)) {
    stop("Provide either 'path_to_raw_folder' or 'file_paths', not both.")
  }

  if (!is.null(path_to_raw_folder) && !dir.exists(path_to_raw_folder)) {
    stop("Folder not found: ", path_to_raw_folder)
  }
  if (!is.null(path_to_output_folder) && !dir.exists(path_to_output_folder)) {
    stop("Folder not found: ", path_to_output_folder)
  }

  # QAQC and format metadata file (dual input)
  metadata <- QAQC_metadata(metadata_path = metadata_path, metadata = metadata, tz = timestamp_timezone)

  if (!is.null(file_paths)) {
    path_to_files <- file_paths
  } else {
    path_to_files <- list.files(
      path    = path_to_raw_folder,
      pattern = "\\.csv$",
      full.names = TRUE
    )
  }

  #check if there is csv files in
  if(length(path_to_files) == 0) {
    stop("No CSV files found in input folder. Please check the path to the folder containing raw HOBO CSV files and the folder itself.")
  }

  # extract data from hobo file(s)
  hobo_data_raw <- purrr::map(path_to_files, extract_alldata_from_file) |> dplyr::bind_rows()

  # Error messages for file binding — suppress for conductivity (Low/Full Range

  # is expected and handled downstream)
  is_conductivity_folder <- any(grepl("conduct|Range",
                                      hobo_data_raw$data1_type,
                                      ignore.case = TRUE))

  if (length(unique(hobo_data_raw$data1_type)) > 1 && !is_conductivity_folder) {
    warning(
      "Folder contains different types of HOBO loggers. ",
      "Check that all files in input folder are of the same logger type ",
      "(refer to 'file_summary')."
    )
  }

  # link sn to site_station_code in metadata
  metadat_link <- metadata |> dplyr::filter(sn %in% unique(hobo_data_raw$sn))

  # Defer the single-metric check until after timestamps are parsed (needed
  # for resolve_metric_by_overlap). Store for now.
  metadat_link_needs_resolution <- length(unique(metadat_link$metric)) > 1

  # format timestamp using the timezone string extracted from the HOBO file header
  tz_str <- normalize_tz(hobo_data_raw$timezone[1]) %||% "UTC"

  # detect if csv is in hh:mm:ss or hh:mm (common if excel file was opened prior to import)
  if(lengths(gregexpr(":", hobo_data_raw$timestamp[1])) == 2) {
    hobo_data_raw$timestamp <- lubridate::mdy_hms(hobo_data_raw$timestamp, tz = "UTC")
  }

  if(lengths(gregexpr(":", hobo_data_raw$timestamp[1])) == 1) {
    hobo_data_raw$timestamp <- lubridate::mdy_hm(hobo_data_raw$timestamp, tz = "UTC")
  }

  if(any(is.na(hobo_data_raw$timestamp))) {
    missing_ts <- hobo_data_raw |>
      dplyr::filter(is.na(timestamp))
    missing_ts_sn <- unique(missing_ts$sn)

    warning(paste("NAs produced in timestamp for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted as mm/dd/yy HH:MM:SS AM/PM (%m/%d/%y %I:%M:%S %p). Correct and try again."))
  }


  if(any(is.na(metadata$timestamp_deploy))) {
    missing_ts <- metadata |>
      dplyr::filter(is.na(timestamp_deploy))

    missing_ts_sn <- unique(missing_ts$sn)

    warning(paste("NAs produced in timestamp_deploy for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted the same as the input data and is noted correctly in timestamp_format. Correct and try again."))
  }

  # --- Resolve multi-metric SNs using timestamp overlap -----------------------
  # Now that timestamps are parsed, we can compare deployment windows against
  # actual raw data timestamps. A single SN can appear in metadata with
  # different metrics across deployment periods (e.g. U20 used as barometric
  # one season, waterlevel the next).
  if (metadat_link_needs_resolution) {
    metadat_link <- resolve_metric_by_overlap(metadat_link, hobo_data_raw,
                                              timestamp_timezone)
  }

  measurement_type <- unique(metadat_link$metric)

  # Ensure a single metric type after timestamp-based filtering
  if (length(measurement_type) != 1) {
    stop("Multiple metric types found for these loggers: ",
         paste(measurement_type, collapse = ", "),
         "\nMake sure your input folder only contains one metric type ",
         "(e.g., all barometric or all waterlevel).\n",
         "If a serial number was redeployed for a different metric, check ",
         "that the raw files and metadata deployment timestamps are consistent.")
  }

  # Safely check units for channel 1 and (if present) channel 2
  units1_mixed <- length(unique(hobo_data_raw$data1_unit)) > 1

  if(length(measurement_type) == 1 && measurement_type == "airtemp") {
    units2_mixed = FALSE
  } else {
    units2_mixed <- "data2_unit" %in% names(hobo_data_raw) &&
      length(unique(hobo_data_raw$data2_unit)) > 1
  }

  if (units1_mixed || units2_mixed) {
    if (is_conductivity_folder) {
      warning(
        "Conductivity files have different HOBO channel layouts (e.g., Low Range vs Full Range). ",
        "All channels are in \u00b5S/cm. bind_hobo_files() will merge them into a single ",
        "`conduct_uScm` column and record the source in `conduct_source`."
      )
    } else {
      warning("Some files are in different units.")
    }
  }

  # make dummy columns and datasets to fill in
  hobo_data_raw$site_station_code <- NA
  data_i <- data.frame()
  sites_compiled <- data.frame()

  uniq_sn_list <- unique(hobo_data_raw$sn)

  for(serial_num in seq_along(uniq_sn_list)){
    logger_i <- uniq_sn_list[serial_num]
    data_i <- hobo_data_raw |> dplyr::filter(sn ==logger_i)

    # get site codes for each sn
    metadat_i <- metadat_link |> dplyr::filter(sn==logger_i)

    # if no metadat for that logger produce error message
    if(length(metadat_i$sn)==0){
      stop(paste(c("Metadata missing for logger serial number:", logger_i)))
    }

    for(metadata_sn_j in seq_along(metadat_i$sn)){
      site_j <- metadat_i$site_station_code[metadata_sn_j]
      deploy_j <- metadat_i$timestamp_deploy[metadata_sn_j]
      remove_j<- metadat_i$timestamp_remove[metadata_sn_j]

      # if logger is still logging it wont have a remove date so make it today
      if(is.na(remove_j)){remove_j <- lubridate::now(tzone=timestamp_timezone)}

      # assign location based on matching timestamps in metadata
      data_i$site_station_code[data_i$timestamp >= deploy_j & data_i$timestamp <= remove_j] <- site_j

    } # end of j loop

    # trim data to within install/removal at each site
    data_i <- data_i |> tidyr::drop_na(site_station_code)
    sites_compiled <- rbind(sites_compiled, data_i)

  } # end of i loop

  ## CLEAN UP ####

  # remove duplicate rows in case multiple downloads per logger results in overlapping dates
  sites_compiled <- sites_compiled |>
    dplyr::distinct(site_station_code, sn, timestamp, .keep_all = TRUE)

  # order by site and timestamp
  sites_compiled<- sites_compiled[order(sites_compiled$site_station_code, sites_compiled$timestamp), ]



  #####
  # NEED TO ADD CODE FOR TIDBIT QAQC STILL, ALSO WHAT HAPPENS IF USER HAS MORE THAN 1 LOGGER TYPE?
  # U20s and TidbiTs can be used for different metrics (U20 can be barometricpressure or waterlevel,
  # TidbiT can be air or water temperature, Pendant can be temp/light/event)
  # perhaps separate by model and metric?
  #####

  logger_type <- unique(metadat_link$model)
  if (length(logger_type) != 1) {
    warning("Multiple logger models found for these files: ",
            paste(logger_type, collapse = ", "),
            ". Using first.")
  }

  logger_type <- logger_type[[1]]


  # mutate a column for logger type and metric
  sites_compiled <- sites_compiled |>
    dplyr::mutate(logger_type = logger_type, metric = measurement_type)


  # setting this sa default. That way if logger_header is not assigned in anything below we can create an error.
  logger_header <- NA_character_


  # DO LOGGER -----------------------------------------------------------
  if (colnames(sites_compiled)[3] == "DO conc mg/L") {
    colnames(sites_compiled)[3] <- "do_mgl"
    logger_header <- "DO"

    # Temp handling
    if (ncol(sites_compiled) >= 4) {
      if (colnames(sites_compiled)[4] == "Temp \u00b0C") {
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
      if (colnames(sites_compiled)[4] == "Temp \u00b0F") {
        sites_compiled[[4]] <- convert_F_to_C(sites_compiled[[4]])
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
    }
  }

  # BAROMETRIC LOGGER ---------------------------------------------------
  if (colnames(sites_compiled)[3] == "Abs Pres kPa" && measurement_type == "barometric") {
    # note: suggest modifying conditionals to use identical() instead of == to ensure length 1
      # even if this doesn't cause problems currently
    colnames(sites_compiled)[3] <- "airpress_kPa"
    sites_compiled <- sites_compiled[stats::complete.cases(sites_compiled$airpress_kPa),]
    logger_header <- "BARO"

    if (ncol(sites_compiled) >= 4) {
      if (colnames(sites_compiled)[4] == "Temp \u00b0C") {
        colnames(sites_compiled)[4] <- "airtemp_C"
      }
      if (colnames(sites_compiled)[4] == "Temp \u00b0F") {
        sites_compiled[[4]] <- convert_F_to_C(sites_compiled[[4]])
        colnames(sites_compiled)[4] <- "airtemp_C"
      }
    }
  }

  # WATER LEVEL LOGGER --------------------------------------------------
  if (colnames(sites_compiled)[3] == "Abs Pres kPa" && measurement_type == "waterlevel") {
    colnames(sites_compiled)[3] <- "waterpress_kPa"
    logger_header <- "WL"

    if (ncol(sites_compiled) >= 4) {
      if (colnames(sites_compiled)[4] == "Temp \u00b0C") {
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
      if (colnames(sites_compiled)[4] == "Temp \u00b0F") {
        sites_compiled[[4]] <- convert_F_to_C(sites_compiled[[4]])
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
    }
  }

  # TIDBIT WATER TEMP LOGGER -------------------------------------------
  if (colnames(sites_compiled)[3] == "Temp \u00b0C" && measurement_type == "watertemp") {
    logger_header <- "WT"
    colnames(sites_compiled)[3] <- "watertemp_C"
  }

  if (colnames(sites_compiled)[3] == "Temp \u00b0F" && measurement_type == "watertemp") {
    logger_header <- "WT"
    sites_compiled[[3]] <- convert_F_to_C(sites_compiled[[3]])
    colnames(sites_compiled)[3] <- "watertemp_C"
  }

  # TIDBIT AIR TEMP LOGGER ---------------------------------------------
  if (colnames(sites_compiled)[3] == "Temp \u00b0C" && measurement_type == "airtemp") {
    logger_header <- "AT"
    colnames(sites_compiled)[3] <- "airtemp_C"
  }

  if (colnames(sites_compiled)[3] == "Temp \u00b0F" && measurement_type == "airtemp") {
    logger_header <- "AT"
    sites_compiled[[3]] <- convert_F_to_C(sites_compiled[[3]])
    colnames(sites_compiled)[3] <- "airtemp_C"
  }

  # CONDUCTIVITY LOGGER U24 ---------------------------------------------
  # Only run this block if the folder metric is conductivity

  if (measurement_type == "conductivity") {

    nm <- names(sites_compiled)

    # Much looser patterns: just look for "Low Range" / "Full Range" and "S/cm"
    low_col  <- nm[grepl("Low Range",  nm, ignore.case = TRUE) &
                     grepl("S/cm",       nm, ignore.case = TRUE)]
    full_col <- nm[grepl("Full Range", nm, ignore.case = TRUE) &
                     grepl("S/cm",       nm, ignore.case = TRUE)]

    # Temp column: "Temp deg C" or "Temp, deg C", any variant with C
    temp_col <- nm[grepl("^Temp", nm, ignore.case = TRUE) &
                     grepl("C",     nm, ignore.case = TRUE)]

    if (length(low_col)  > 1L) stop("Multiple 'Low Range' columns found.")
    if (length(full_col) > 1L) stop("Multiple 'Full Range' columns found.")
    if (length(temp_col) > 1L) stop("Multiple temperature (\u00b0C) columns found.")

    low_col  <- if (length(low_col))  low_col[[1]]  else NA_character_
    full_col <- if (length(full_col)) full_col[[1]] else NA_character_
    temp_col <- if (length(temp_col)) temp_col[[1]] else NA_character_

    # Extract numeric vectors safely
    n <- nrow(sites_compiled)

    low_vals  <- if (!is.na(low_col))  suppressWarnings(as.numeric(sites_compiled[[low_col]]))  else rep(NA_real_, n)
    full_vals <- if (!is.na(full_col)) suppressWarnings(as.numeric(sites_compiled[[full_col]])) else rep(NA_real_, n)
    temp_vals <- if (!is.na(temp_col)) suppressWarnings(as.numeric(sites_compiled[[temp_col]])) else rep(NA_real_, n)

    # HOBO U24 low-range ~0-1000 uS/cm
    low_max <- 1000

    sites_compiled <- sites_compiled |>
      dplyr::mutate(
        conduct_uScm = dplyr::case_when(
          !is.na(low_vals)  & low_vals <= low_max ~ low_vals,   # prefer low range when valid & in range
          !is.na(full_vals)                      ~ full_vals,   # else full range
          !is.na(low_vals)                       ~ low_vals,    # fallback to low
          TRUE                                   ~ NA_real_
        ),
        conduct_source = dplyr::case_when(
          !is.na(low_vals)  & low_vals <= low_max ~ "low",
          !is.na(full_vals)                      ~ "full",
          !is.na(low_vals)                       ~ "low",
          TRUE                                   ~ NA_character_
        ),
        watertemp_C = temp_vals
      )

    logger_header <- "COND"

    # Standardise metadata so "data1" = cond, "data2" = temp for ALL cond files
    sites_compiled$data1_type <- "conductivity"
    sites_compiled$data1_unit <- "\u00b5S/cm"
    sites_compiled$data2_type <- "Temp"
    sites_compiled$data2_unit <- "\u00b0C"

    # Drop the raw Low Range / Full Range / Temp columns now that unified
    # conduct_uScm, conduct_source, and watertemp_C columns exist
    cols_to_drop <- c(low_col, full_col, temp_col)
    cols_to_drop <- cols_to_drop[!is.na(cols_to_drop)]
    if (length(cols_to_drop) > 0) {
      sites_compiled <- sites_compiled |>
        dplyr::select(-dplyr::all_of(cols_to_drop))
    }
  }


  # If logger_header is not successfully reassigned above
  if (is.na(logger_header)) {
    stop("Could not determine logger_header from column names and measurement_type.")
  }

  # Remove HOBO event columns we don't need
  sites_compiled <- sites_compiled |>
    dplyr::select(
      -dplyr::matches("Coupler Detached|Coupler Attached|Stopped|End Of File")
    )

  #### EXPORT CSV BY LOGGER TYPE LOCATION AND YEAR
  # some stations may have multiple logger files, one from a download in spring and another for a download in fall
  # this loop will split data by type, location and year for ease of access and processing

  if (!is.null(path_to_output_folder)) {
    for(site_i in unique(sites_compiled$site_station_code)) {

      site_dat <- sites_compiled |>
        dplyr::filter(site_station_code == site_i)

      # split by year
      years_i <- unique(lubridate::year(site_dat$timestamp))

      for (j in seq_along(years_i)) {
        year_j <- years_i[j]

        site_year_j <- site_dat |>
          dplyr::filter(lubridate::year(timestamp) == year_j)

        # skip if no data for this year
        if (!nrow(site_year_j)) next

        # name by timestamp range
        start_j <- gsub("\\D", "", as.character(min(lubridate::date(site_year_j$timestamp))))
        end_j   <- gsub("\\D", "", as.character(max(lubridate::date(site_year_j$timestamp))))

        # create paths for output folders
        year_dir <- file.path(path_to_output_folder, year_j)
        proc_dir <- file.path(year_dir, "processed")

        # Create them recursively (no warnings if they already exist)
        dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
        dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

        # correct missing midnight timestamps & put into character so no formatting issues arise when in Excel
        site_year_j$timestamp <- format(site_year_j$timestamp, "%Y-%m-%d %H:%M:%S")

        # build output path using file.path for consistency
        out_file <- sprintf("%s_%s_%s_%s_v0.1.csv", site_i, logger_header, start_j, end_j)
        out_path <- file.path(proc_dir, out_file)

        utils::write.csv(site_year_j, out_path, row.names = FALSE)

        message(sprintf("Data (v0.1) from %s added to %s/processed", site_i, year_j))


      } #end of year level loop

    } # end of site level loop
  } # end of file-writing guard

  message("List returned: [1] trimmed and compiled dataset of csvs in raw folder \n [2] summary plots of data by site_station_code and timestamp")

  # Make sure the measurement columns are numeric (coerce by name, not position)
  meas_cols_to_coerce <- intersect(
    c("do_mgl", "watertemp_C", "airtemp_C", "airpress_kPa", "waterpress_kPa", "conduct_uScm"),
    names(sites_compiled)
  )
  if (length(meas_cols_to_coerce) > 0) {
    sites_compiled <- sites_compiled |>
      dplyr::mutate(dplyr::across(dplyr::all_of(meas_cols_to_coerce), as.numeric))
  }

  time_col  <- "timestamp"
  value_col <- switch(
    logger_header,
    "COND" = "conduct_uScm",
    names(sites_compiled)[3]
  )

  multi_plots <- ggplot2::ggplot(
    data = sites_compiled,
    ggplot2::aes(x = .data[[time_col]], y = .data[[value_col]])
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ site_station_code, scales = "free") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::scale_x_datetime(date_labels = "%y-%m") +
    ggplot2::labs(
      x = "Timestamp (YY-MM)",
      y = value_col
    )

  return(list(
    sites_compiled,
    multi_plots
  ))

}

