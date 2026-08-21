#' Retrieve logger data
#'
#' High-level helper to load logger data from disk, optionally filter by station
#'
#' This function works with the original wide-format processed files
#' (e.g. columns like \code{waterlevel_m_U20_adj}, \code{do_mgl_U26_adj}).
#'
#' @param path_to_data Path to the folder containing the CSV files. The function
#'   will search this folder and its subfolders recursively.
#' @param data_processing One of \code{"v0.1"}, \code{"v0.2"},
#'   \code{"v0.3"}, \code{"v1.0"}, \code{"clean"}. The value is used to select
#'   files by version suffix. \code{"clean"} is treated as \code{"v1.0"}.
#' @param metric Optional  character string indicating which logger file type to
#'   search for. If \code{NULL} (default), all logger type files loaded.
#'   Accepted values (case and spaces ignored) are mapped to:
#'   \itemize{
#'     \item \code{"waterlevel"}: water level + water temperature
#'     \item \code{"dissolvedoxygen"}: DO (mg/L, %sat) + water temperature
#'     \item \code{"barometric"}: barometric pressure + air temperature
#'     \item \code{"conductivity"}: conductivity + water temperature
#'     \item \code{"airtemp"}: air temperature (e.g., TidbiT)
#'     \item \code{"watertemp"}: water temperature (e.g., TidbiT)
#'   }
#' @param select_station Either \code{"all"} (default) or one or more
#'   \code{site_station_code} values to filter on.
#' @param year Either \code{"all"} (default) or 4 digit numeric value
#' designating year of data to filter data files (format = YYYY).
#' @param input_data A data frame of logger observations (alternative to
#'   \code{path_to_data} for Shiny mode). When supplied, file discovery is
#'   skipped and this data frame is used directly. Exactly one of
#'   \code{path_to_data} or \code{input_data} must be provided.
#'
#' @return
#'   \itemize{
#'     \item A data frame of combined logger data.
#'       }
#'
#' @seealso \code{\link{write_clean_data}} (produces the v1.0 files this
#'   function reads), \code{\link{bind_logger_files}} (produces v0.1),
#'   \code{\link{plot_qc_timeseries}} (interactive QC plotting)
#'
#' @export

get_logger_data <- function(path_to_data = NULL,
                            data_processing = "v1.0",
                            metric          = NULL,
                            year = "all",
                            select_station  = "all",
                            input_data      = NULL) {

  dp_info        <- resolve_data_processing_pattern(data_processing)
  data_processing <- dp_info$data_processing

  # if either metric, year or station are provided
  if(!is.null(metric) | select_station!="all" | year != "all"){
    station_info  <- normalise_station(select_station)
    station_norm <- station_info$station_norm

    metric_info <- normalise_metric(metric)
    metric_norm <- metric_info$metric_norm

    year_norm <- normalise_year(year)

    # look for files containing site or metric first, and the suffix of the data processing stage
    pattern         <- paste0("(^", station_info$pattern, metric_info$pattern, year_norm, "|^", metric_info$pattern, station_info$pattern, year_norm, ").*", dp_info$pattern)

  }else{
    # otherwise just look for the matching data processing suffix
    pattern <- dp_info$pattern
    }


  # ---- stop messages  -------------------------------

  # Dual input: in-memory data frame OR file path (mutually exclusive)
  if (!is.null(input_data) && !is.null(path_to_data)) {
    stop(
      "Provide either 'path_to_data' or 'input_data', not both.",
      call. = FALSE
    )
  }

  if (!is.null(input_data) && is.data.frame(input_data)) {
    req_cols <- c("timestamp", "site_station_code")
    miss <- setdiff(req_cols, names(input_data))
    if (length(miss) > 0) {
      stop(
        "get_logger_data(): input_data is missing required column(s): ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
    }
    dat <- input_data

    if (!inherits(dat$timestamp, "POSIXct")) {
      # parse timestamp column (utils.R)
      dat$timestamp <- parse_dt(dat$timestamp)

      if (any(is.na(dat$timestamp))) {
        warning(
          "read_logger_file(): some timestamps failed to parse in '",
          input_data, "'. ",
          "Check that the timestamp column uses a YYYY-MM-DD HH:MM:SS format.",
          call. = FALSE
        )
      }
    }
  } else {
    if (is.null(path_to_data)) {
      stop("Must provide either 'path_to_data' or 'input_data' as a data frame.", call. = FALSE)
    }

    # find files recursively
    file_paths <- list.files(
      path       = path_to_data,
      pattern    = pattern,
      full.names = TRUE,
      recursive  = TRUE
    )


    if (length(file_paths) == 0L) {
      stop(
        "No files found in `path_to_data` (recursively) matching pattern '", pattern, "'. ",
        "Check the folder path, data_processing choice, and file naming.",
        call. = FALSE
      )
    }

    # read and combine
    # TODO allow for multi metric, extra columns, join by site_station_code and timestamp
    # allows for easy merge of different logger types by site

    dat <- purrr::map(file_paths, read_logger_file) |> dplyr::bind_rows()
  }

   # optional station filter
    if (!identical(station_norm, "all")) {
      dat <- dat |> dplyr::filter(site_station_code %in% station_norm)
    }

    # optional year filter
    if (!is.null(year_norm)) {
      dat <- dat |> dplyr::filter(lubridate::year(timestamp) %in% year_norm)
    }

    return(dat)

  if(is.null(input_data))
  message(paste("Returning all data from", station_norm, "station(s) found in", path_to_data))

  if(is.null(path_to_data))
    message(paste("Returning all data from", station_norm, "station(s) found in", input_data))

}
