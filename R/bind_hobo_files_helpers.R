#' QC for logger deployment metadata
#'
#' Reads (from file or in-memory data frame) and validates a table of
#' logger deployment metadata used throughout the \pkg{waterlogged}
#' pipeline.  Called internally by \code{\link{bind_logger_files}},
#' \code{\link{add_nearest_baro}}, \code{\link{qc_barometric_all}},
#' and \code{\link{qc_conductivity_all}}.
#'
#' @param metadata_path Character scalar.
#'   Path to a metadata CSV file.  Required when \code{metadata} is
#'   \code{NULL}.
#' @param metadata A data frame of deployment metadata (alternative to
#'   \code{metadata_path} for Shiny mode).  Exactly one of
#'   \code{metadata_path} or \code{metadata} must be supplied.
#' @param tz Character scalar. Timezone used when parsing
#'   \code{timestamp_deploy} and \code{timestamp_remove}. Default
#'   \code{"UTC"}.
#'
#' @return A cleaned data frame with:
#'   \itemize{
#'     \item \code{timestamp_deploy} and \code{timestamp_remove} parsed
#'       to POSIXct (UTC).
#'     \item \code{latitude} and \code{longitude} coerced to numeric
#'       decimal degrees.
#'     \item \code{sn} coerced to character (preserves leading zeros).
#'     \item \code{metric} standardised to lowercase with spaces removed.
#'   }
#'
#' @details
#' The following validation steps are applied in order:
#' \enumerate{
#'   \item Empty rows (all \code{NA} or blank) are dropped.
#'   \item Required columns are checked: \code{site_station_code},
#'     \code{timestamp_deploy}, \code{timestamp_remove}, \code{model},
#'     \code{sn}, \code{metric}, \code{latitude},
#'     \code{longitude}.
#'   \item \code{timestamp_deploy} and \code{timestamp_remove} are
#'     parsed using \code{lubridate::mdy_hm()} first, then fall back to
#'     flexible multi-format parsing.  \code{NA} values in
#'     \code{timestamp_remove} are expected for loggers still deployed.
#'   \item Chronology is checked: deployment must precede removal.
#'   \item \code{sn} (serial number) is coerced to character.
#'   \item Latitude and longitude are validated: DMS / cardinal-letter
#'     formats trigger a warning, values are coerced to numeric, and
#'     range checks (\eqn{[-90, 90]} and \eqn{[-180, 180]}) are applied.
#'   \item \code{metric} is standardised to lowercase with spaces removed
#'     and validated against the allowed set: \code{"waterlevel"},
#'     \code{"barometric"}, \code{"conductivity"},
#'     \code{"dissolvedoxygen"}, \code{"airtemp"}, \code{"watertemp"},
#'     \code{"light"}.
#' }
#'
#' @seealso \code{\link{QAQC_reference_data}} (analogous validation for
#'   manual stage/depth reference data),
#'   \code{\link{bind_logger_files}} (primary consumer of the validated
#'   metadata)
#'
#' @export
QAQC_metadata <- function(metadata_path = NULL, metadata = NULL, tz = "UTC") {
  tryCatch({
    # 1) Dual input: accept data frame OR file path
    if (!is.null(metadata) && is.data.frame(metadata)) {
      # Use in-memory data frame directly (Shiny mode)
    } else if (!is.null(metadata_path)) {
      if (!file.exists(metadata_path)) {
        stop("File not found: ", metadata_path)
      }
      # 2) Read CSV (quietly, no factors)
      metadata <- utils::read.csv(metadata_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      stop("Must provide either 'metadata_path' or 'metadata' as a data frame.")
    }

    # Remove completely empty rows (all NA or all blank strings)
      # note: this will fail for datetime cols (not currently an issue).
      # Suggest changing blanks to NA first if the input method changes.
    metadata <- metadata[rowSums(is.na(metadata) | metadata == "") != ncol(metadata), ]

    # 3) Required columns present?
    required_cols <- c("site_station_code", "timestamp_deploy", "timestamp_remove",
                       "model", "sn", "metric", "latitude", "longitude")

    missing_cols  <- setdiff(required_cols, names(metadata))
    if (length(missing_cols)) {
      stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
    }

    # 4) Parse timestamps (expect mdy_hm; auto-UTC)
    # Following is deprecated; refer to parse_dt in utils.R
    # parse_dt <- function(x, col_name = "timestamp") {
    #   if (inherits(x, "POSIXct")) return(x)
    #   x_chr     <- as.character(x)
    #   all_blank <- is.na(x_chr) | trimws(x_chr) %in% c("", "NA")
    #   # All values are blank/NA — valid for still-deployed loggers (timestamp_remove)
    #   if (all(all_blank)) {
    #     return(as.POSIXct(rep(NA_character_, length(x_chr)), tz = tz))
    #   }
    #   p1 <- try(lubridate::mdy_hm(x_chr, tz = tz, quiet = TRUE), silent = TRUE)
    #   if (!inherits(p1, "try-error") && !all(is.na(p1[!all_blank]))) return(p1)
    #   p2 <- suppressWarnings(
    #     lubridate::parse_date_time(
    #       x_chr,
    #       orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "ymd HM", "ymd HMS"),
    #       tz = tz
    #     )
    #   )
    #   if (all(is.na(p2[!all_blank]))) {
    #     stop("Failed to parse '", col_name, "' column. Expected format like '4/30/2025 13:45' (", tz, ").")
    #   }
    #   p2
    # }
    metadata$timestamp_deploy <- parse_dt(metadata$timestamp_deploy, "timestamp_deploy")
    metadata$timestamp_remove <- parse_dt(metadata$timestamp_remove, "timestamp_remove")

    # 4) Warn if any timestamps are NA after parsing

    ####
    # NA values in timestamp_remove are expected if the logger is still logging
    # update to produce an error if the is a value and the timestamp does not parse but ignore if the row was read in as "NA"
    ####

    na_deploy <- which(is.na(metadata$timestamp_deploy))
    na_remove <- which(is.na(metadata$timestamp_remove))
    if (length(na_deploy)) warning("Missing/invalid 'timestamp_deploy' on row(s): ", paste(na_deploy, collapse = ", "))
    if (length(na_remove)) message("Note: 'timestamp_remove' is NA on row(s): ", paste(na_remove, collapse = ", "), " (expected for still-deployed loggers).")

    # 5) check deploy > remove
    if (any(metadata$timestamp_deploy > metadata$timestamp_remove, na.rm = TRUE)) {
      bad_rows <- which(metadata$timestamp_deploy > metadata$timestamp_remove)
      stop("Deploy time is after remove time on row(s): ", paste(bad_rows, collapse = ", "))
    }

    # 6) station number as character
    if ("sn" %in% names(metadata)) {
      metadata$sn <- as.character(metadata$sn)  # preserve leading zeros if any
    }

    # 7) Latitude / Longitude QAQC (decimal degrees expected)
    has_lat <- "latitude"  %in% names(metadata)
    has_lon <- "longitude" %in% names(metadata)
    if (!has_lat || !has_lon) {
      warning("Latitude/longitude column(s) missing in metadata (expected 'latitude' and 'longitude').")
    } else {
      lat_chr <- trimws(as.character(metadata$latitude))
      lon_chr <- trimws(as.character(metadata$longitude))

      # Looks like DMS or contains N/S/E/W or degree symbols?
      looks_non_decimal <- function(x) grepl("[NSEW\u00b0'\"\u00B0]", x, ignore.case = TRUE)
      bad_fmt_rows <- which((!is.na(lat_chr) & looks_non_decimal(lat_chr)) |
                              (!is.na(lon_chr) & looks_non_decimal(lon_chr)))
      if (length(bad_fmt_rows)) {
        warning("Lat/long values appear non-decimal (DMS or N/S/E/W) on row(s): ",
                paste(bad_fmt_rows, collapse = ", "),
                ". Expected decimal degrees (e.g., 49.1234, -123.4567).")
      }

      # Coerce to numeric (non-numeric -> NA)
      metadata$latitude  <- suppressWarnings(as.numeric(lat_chr))
      metadata$longitude <- suppressWarnings(as.numeric(lon_chr))

      # Missingness warnings
      miss_lat <- which(is.na(metadata$latitude))
      miss_lon <- which(is.na(metadata$longitude))
      if (length(miss_lat)) warning("Missing latitude (decimal degrees) on row(s): ", paste(miss_lat, collapse = ", "))
      if (length(miss_lon)) warning("Missing longitude (decimal degrees) on row(s): ", paste(miss_lon, collapse = ", "))

      # Only-one-missing per row
      one_missing <- which(xor(is.na(metadata$latitude), is.na(metadata$longitude)))
      if (length(one_missing)) {
        warning("Only one of latitude/longitude supplied on row(s): ", paste(one_missing, collapse = ", "))
      }

      # Range checks
      bad_lat <- which(!is.na(metadata$latitude)  & (metadata$latitude  < -90  | metadata$latitude  > 90))
      bad_lon <- which(!is.na(metadata$longitude) & (metadata$longitude < -180 | metadata$longitude > 180))
      if (length(bad_lat)) warning("Latitude out of range [-90, 90] on row(s): ", paste(bad_lat, collapse = ", "))
      if (length(bad_lon)) warning("Longitude out of range [-180, 180] on row(s): ", paste(bad_lon, collapse = ", "))
    }

    # 8) Metric / measurement_type QAQC (folded-in qc_measurement_type)
    valid_metrics = c("waterlevel", "barometric", "conductivity", "dissolvedoxygen", "airtemp", "watertemp", "light")

    metric_raw <- metadata$metric
    metric_std <- gsub(" ", "", tolower(as.character(metric_raw)))

    bad_metric_rows <- which(is.na(metric_std) | !metric_std %in% valid_metrics)


    if (length(bad_metric_rows) > 0) {
      stop(
        "QC check failed: invalid 'metric' value(s) on row(s): ",
        paste(bad_metric_rows, collapse = ", "),
        "\nValid options are: ",
        paste(valid_metrics, collapse = ", ")
      )
    }

    # Overwrite with standardized metric names (lowercase, no spaces)
    metadata$metric <- metric_std

    metadata

  }, error = function(e) {
    stop("Error reading/parsing metadata: ", e$message)
  })
}


#' Parse a single raw HOBO CSV export
#'
#' Internal helper called by \code{\link{bind_logger_files}} (via
#' \code{purrr::map_df()}) to read one HOBO-exported CSV file, extract
#' the logger serial number and channel metadata from row 2, build
#' standardised column names, coerce measurements to numeric, and attach
#' file-level metadata.
#'
#' @param file Character scalar.  Full path to a single HOBO CSV file.
#'   The file is expected to have the standard Onset HOBOware export
#'   layout: row 1 is a title/plot-title row, row 2 contains the column
#'   headers (date-time format, serial number, and channel descriptors of
#'   the form \code{"<Type>, <Unit> (LGR S/N: ...)"}).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{\code{sn}}{Logger serial number (8-digit character).}
#'     \item{\code{timestamp}}{Raw timestamp string (not yet parsed to
#'       POSIXct; parsing happens downstream in
#'       \code{\link{bind_logger_files}}).}
#'     \item{\emph{<measurement channels>}}{One numeric column per data
#'       channel, named \code{"<Type> <Unit>"} (e.g.
#'       \code{"Temp \u00b0C"}, \code{"Abs Pres kPa"}).}
#'     \item{\code{file_name}}{Base name of the source CSV.}
#'     \item{\code{timezone}}{Timezone string extracted from the header
#'       row.}
#'     \item{\code{data1_type}, \code{data1_unit}}{Type and unit of the
#'       first data channel.}
#'     \item{\code{data2_type}, \code{dat2_unit}}{Type and unit of the
#'       second data channel.}
#'   }
#'   Rows where all measurement columns are \code{NA} are dropped.
#'
#' @seealso \code{\link{bind_logger_files}} (sole caller)
#'
#' @keywords internal
extract_alldata_from_file <- function(file) {
  # Read raw HOBO file as-is (no name mangling)
  data <- utils::read.csv(
    file,
    header           = FALSE,
    sep              = ",",
    dec              = ".",
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  file_name <- sub(".*/", "", file, perl = TRUE)

  # ---- Parse timezone + logger info from row 2 -------------------------------
  tz_raw <- as.character(data[2, 2])
  tz     <- sub(".*(Date Time.*), ", "", tz_raw)

  logger_info <- as.character(data[2, 3])
  logger_sn <- regmatches(logger_info, regexpr("\\d{8}", logger_info))
  if (length(logger_sn) == 0) {
    warning("Could not extract 8-digit serial number from header of: ", basename(file), call. = FALSE)
    logger_sn <- NA_character_
  }

  # helper: parse "<Type>, <unit> (LGR ...)" -> type + unit
  parse_type_unit <- function(info) {
    info <- as.character(info)
    if (is.na(info) || info == "") {
      return(list(type = NA_character_, unit = NA_character_))
    }
    type <- sub("\\,.*", "", info)              # before first comma
    unit <- sub("\\s*\\(.*", "", info)          # before "("
    unit <- sub(".*, ", "", unit)               # after last ", "
    list(type = trimws(type), unit = trimws(unit))
  }

  header_row <- as.character(data[2, ])
  n_cols     <- ncol(data)

  # ---- Build metadata for all data columns (>=3 and non-empty) ---------------
  meta_tbl <- tibble::tibble(
    col_index  = seq_len(n_cols),
    raw_header = header_row
  ) |>
    dplyr::filter(
      col_index >= 3,
      !is.na(raw_header),
      raw_header != ""
    ) |>
    dplyr::mutate(
      parsed = purrr::map(raw_header, parse_type_unit),
      type   = purrr::map_chr(parsed, "type"),
      unit   = purrr::map_chr(parsed, "unit")
    )

  # First two channels kept for backward compatibility
  if (nrow(meta_tbl) >= 1) {
    dat1_type <- meta_tbl$type[1]
    dat1_unit <- meta_tbl$unit[1]
  } else {
    dat1_type <- dat1_unit <- NA_character_
  }
  if (nrow(meta_tbl) >= 2) {
    dat2_type <- meta_tbl$type[2]
    dat2_unit <- meta_tbl$unit[2]
  } else {
    dat2_type <- dat2_unit <- NA_character_
  }

  # ---- Trim to data rows + relevant columns ----------------------------------
  # Use serial number as first column
  data$V1 <- logger_sn

  # Remove the two header rows, keep SN + timestamp + all data cols we identified
  data_clean <- data[-c(1, 2), c(1, 2, meta_tbl$col_index), drop = FALSE]

  # Build measurement column names
  meas_names <- purrr::map2_chr(
    meta_tbl$type,
    meta_tbl$unit,
    function(type, unit) {
      if (is.na(type) && is.na(unit)) {
        NA_character_
      } else {
        paste(type, unit)
      }
    }
  )

  # Fallback to the raw header text when parse failed
  meas_names <- ifelse(
    is.na(meas_names) | meas_names == "",
    meta_tbl$raw_header,
    meas_names
  )

  new_names <- c("sn", "timestamp", meas_names)
  colnames(data_clean) <- new_names

  data_clean <- tibble::as_tibble(data_clean)

  # ---- Coerce measurement cols to numeric and drop all-NA rows ---------------
  meas_cols <- setdiff(names(data_clean), c("sn", "timestamp"))

  if (length(meas_cols) > 0) {
    # numeric coercion (suppress warnings from non-numeric junk)
    data_clean <- data_clean |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(meas_cols),
          ~ suppressWarnings(as.numeric(.x))
        )
      )

    # drop rows where ALL measurement columns are NA
    keep_row <- apply(
      dplyr::select(data_clean, dplyr::all_of(meas_cols)),
      1,
      function(z) any(!is.na(z))
    )
    data_clean <- data_clean[keep_row, , drop = FALSE]
  }

  # ---- Attach metadata columns -----------------------------------------------
  data_clean <- data_clean |>
    dplyr::mutate(
      file_name  = file_name,
      timezone   = tz,
      data1_type = dat1_type,
      data1_unit = dat1_unit,
      data2_type = dat2_type,
      data2_unit = dat2_unit
    )

  return(data_clean)
}


#' Filter metadata to rows whose deployment windows overlap raw data timestamps
#'
#' When a serial number appears in metadata with multiple metric types (e.g.
#' a U20 deployed as barometric one season and waterlevel the next), this
#' helper keeps only the metadata rows whose deployment/removal window
#' overlaps with the actual timestamps present in the raw HOBO data for
#' that serial number.
#'
#' @param metadat_link Data frame of metadata rows matching the raw-file SNs.
#' @param hobo_data_raw Data frame of parsed raw HOBO data (must have
#'   \code{sn} and \code{timestamp} columns; timestamps must be POSIXct).
#' @param timestamp_timezone Character timezone for filling open-ended
#'   deployments (where \code{timestamp_remove} is \code{NA}).
#'
#' @return A filtered version of \code{metadat_link} containing only rows
#'   that overlap with the raw data.
#' @keywords internal
#' @noRd
resolve_metric_by_overlap <- function(metadat_link, hobo_data_raw,
                                      timestamp_timezone = "UTC") {

  # Compute min/max timestamp per SN in the raw data
  sn_ranges <- hobo_data_raw |>
    dplyr::group_by(sn) |>
    dplyr::summarise(
      raw_min = min(timestamp, na.rm = TRUE),
      raw_max = max(timestamp, na.rm = TRUE),
      .groups = "drop"
    )

  # For each metadata row, check if the deployment window overlaps the raw data
  keep_rows <- logical(nrow(metadat_link))

  for (i in seq_len(nrow(metadat_link))) {
    sn_i     <- metadat_link$sn[i]
    deploy_i <- metadat_link$timestamp_deploy[i]
    remove_i <- metadat_link$timestamp_remove[i]

    # Fill open-ended deployments with current time
    if (is.na(remove_i)) {
      remove_i <- lubridate::now(tzone = timestamp_timezone)
    }

    # Find the raw data range for this SN
    sn_range <- sn_ranges[sn_ranges$sn == sn_i, , drop = FALSE]
    if (nrow(sn_range) == 0) {
      keep_rows[i] <- FALSE
      next
    }

    # Check for any overlap: deployment window intersects raw data range
    # Overlap exists when: deploy <= raw_max AND remove >= raw_min
    keep_rows[i] <- !is.na(deploy_i) &&
      deploy_i <= sn_range$raw_max[1] &&
      remove_i >= sn_range$raw_min[1]
  }

  filtered <- metadat_link[keep_rows, , drop = FALSE]

  if (nrow(filtered) == 0) {
    stop(
      "bind_logger_files(): No metadata deployment windows overlap with the ",
      "raw data timestamps. Check that the metadata file matches the raw ",
      "HOBO files being processed."
    )
  }

  # Report what was filtered out
  n_removed <- sum(!keep_rows)
  if (n_removed > 0) {
    removed <- metadat_link[!keep_rows, , drop = FALSE]
    message(
      "Note: ", n_removed, " metadata row(s) filtered out (deployment window ",
      "does not overlap raw data timestamps): ",
      paste(
        sprintf("SN %s as %s (%s to %s)",
                removed$sn, removed$metric,
                removed$timestamp_deploy,
                ifelse(is.na(removed$timestamp_remove), "ongoing",
                       as.character(removed$timestamp_remove))),
        collapse = "; "
      )
    )
  }

  filtered
}
