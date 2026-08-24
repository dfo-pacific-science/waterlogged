# ---- Helpers ---------------------------------------------------------------

#' Normalise a user-supplied temporal scale string
#'
#' Internal helper called by \code{\link{get_logger_data}} and \code{\link{summarise_clean_data}} to validate
#' and standardise the \code{temporal_scale} argument.
#'
#' @param temporal_scale Character scalar.  Recognised values
#'   (case-insensitive, whitespace-stripped):
#'   \code{"none"}, \code{"hourly"} / \code{"hour"},
#'   \code{"daily"} / \code{"day"},
#'   \code{"weekly"} / \code{"week"},
#'   \code{"monthly"} / \code{"month"},
#'   \code{"yearly"} / \code{"year"} / \code{"annual"}.
#'
#' @return A single canonical temporal-scale string.
#'
#' @seealso \code{\link{get_logger_data}} \code{\link{summarise_clean_data}}
#'
#' @keywords internal
normalise_temporal_scale <- function(temporal_scale) {
  if (is.null(temporal_scale) || length(temporal_scale) != 1L) {
    stop("`temporal_scale` must be a single, non-NULL value.", call. = FALSE)
  }

  ts <- tolower(trimws(temporal_scale))
  ts <- gsub("\\s+", "", ts)

  if (ts %in% c("hourly", "hour", "hr"))             return("hourly")
  if (ts %in% c("daily", "day"))                     return("daily")
  if (ts %in% c("weekly", "week", "wk"))             return("weekly")
  if (ts %in% c("monthly", "month", "mo"))           return("monthly")
  if (ts %in% c("yearly", "year", "yr", "annual"))   return("yearly")

  stop(
    "Unknown `temporal_scale`. Expected one of: ",
    "'hourly', 'daily', 'weekly', 'monthly', 'yearly'",
    " (case and spaces ignored). You gave: '", temporal_scale, "'.",
    call. = FALSE
  )
}


#' Read a single logger CSV file
#'
#' Internal helper called by \code{\link{get_logger_data}} via
#' \code{purrr::map_df()} to read one processed CSV file, parse
#' the timestamp column, and coerce known factor columns.
#'
#' @param file_path Character scalar.  Path to a CSV file.
#'
#' @return A data frame with \code{timestamp} parsed to POSIXct (UTC)
#'   and factor columns coerced to character.
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
read_logger_file <- function(file_path) {
  dat <- utils::read.csv(
    file_path,
    header           = TRUE,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  if (!"timestamp" %in% names(dat)) {
    ts_cand <- intersect(
      c("Timestamp", "Date Time", "datetime", "date_time"),
      names(dat)
    )
    if (!length(ts_cand)) {
      warning("read_logger_file(): No timestamp column in ", basename(file_path), ". Skipping.")
      return(dat)
    }
    names(dat)[names(dat) == ts_cand[1]] <- "timestamp"
  }

  if (!inherits(dat$timestamp, "POSIXct")) {
      # parse timestamp column (utils.R)
      dat$timestamp <- parse_dt(dat$timestamp)

    if (any(is.na(dat$timestamp))) {
      warning(
        "read_logger_file(): some timestamps failed to parse in '",
        basename(file_path), "'. ",
        "Check that the timestamp column uses a YYYY-MM-DD HH:MM:SS format.",
        call. = FALSE
      )
    }
  }

  # Coerce known factor/code columns to character
  for (col in intersect(factor_cols, names(dat))) {
    dat[[col]] <- as.character(dat[[col]])
  }

  dat
}


#' Normalise a user-supplied metric string to a canonical name
#'
#' Internal helper called by \code{\link{get_logger_data}} to
#' standardise the \code{metric} argument.  Accepts common aliases
#' (case-insensitive, whitespace-stripped) and returns one of the six
#' canonical metric names used throughout the package.
#'
#' @param metric Character scalar.  The user-supplied metric string.
#'   Recognised aliases:
#'   \describe{
#'     \item{\code{"waterlevel"}}{Also \code{"stage"}.}
#'     \item{\code{"dissolvedoxygen"}}{Also \code{"do"},
#'       \code{"oxygen"}.}
#'     \item{\code{"barometric"}}{Also \code{"baro"},
#'       \code{"airpressure"}.}
#'     \item{\code{"watertemp"}}{Also \code{"watertemperature"}.}
#'     \item{\code{"airtemp"}}{Also \code{"airtemperature"}.}
#'   }
#'
#' @return A single canonical metric string.
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
normalise_metric <- function(metric) {
  if (length(metric) > 1L) {
    stop("`metric` must be a single value.", call. = FALSE)
  }
  if (is.null(metric)) {
    return(list(metric_norm =NULL, pattern = ".*_"))
    warning("No metric supplied by default. If specific metric is required, ensure that
    folder mapped by 'file_path' only contains one type of logger, or 'metric'
    is supplied, else logger files of multiple metrics will be returned.")
  }else{

    m <- tolower(trimws(metric))
    m <- gsub("\\s+", "", m)

    if (m %in% c("waterlevel", "stage", "wl", "depth"))               return(list(metric_norm ="waterlevel", pattern = "WL_"))
    if (m %in% c("do", "dissolvedoxygen", "oxygen", "dissox"))   return(list(metric_norm ="dissolvedoxygen", pattern = "DO_"))
    if (m %in% c("baro", "barometric", "airpressure", "ap", "airpress")) return(list(metric_norm ="barometric", pattern = "BARO_"))
    if (m %in% c("watertemp", "watertemperature", "wt"))     return(list(metric_norm ="watertemp", pattern = "WT_"))
    if (m %in% c("airtemp", "airtemperature", "at"))         return(list(metric_norm ="airtemp",pattern = "AT_"))
    if (m %in% c("conductivity", "cond", "spc", "co"))       return(list(metric_norm ="conductivity", pattern = "CO_"))

    stop(
      "Unknown `metric`. Expected one of: ",
      "'waterlevel', 'dissolvedoxygen', 'barometric', 'conductivity', 'watertemp', 'airtemp'",
      " (case and spaces ignored). You gave: '", metric, "'.",
      call. = FALSE
    )
  }

}

#' Normalise a user-supplied station identifier string to file matching pattern
#'
#' Internal helper called by \code{\link{get_logger_data}} to
#' standardise the \code{select_station} argument.  Accepts character string
#' (case-sensitive, whitespace-stripped) and returns regex pattern for file
#' name
#'
#' @param select_station Either \code{"all"} (default) or one or more
#'   \code{site_station_code} values to filter on. Must be exact match to
#'   pattern in file name, case-sensitive.
#'
#' @return A normalized character string and regex pattern.
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
normalise_station <- function(select_station) {
  if (length(select_station) > 1L) {
    stop("`select_station` must be a non-NULL, single value.", call. = FALSE)
  }
  # remove whitespace
  s <- trimws(select_station)
  s <- gsub("\\s+", "", s)

  if (s == "all") {

    return(list(station_norm ="all", pattern = ".*_"))
    warning("`select_station = 'all'` will return a data file that may contain
    multiple stations if present in folder supplied in file_path. To only return
    data from one station ID, specify the station ID in the 'select_station'
            argument.")
  } else{return(list(station_norm = s, pattern = paste0(s,"_")))}

}

#' Normalise a user-supplied year as character string to file matching pattern
#'
#' Internal helper called by \code{\link{get_logger_data}} to
#' standardise the \code{year} argument.  Accepts 4 digit number and returns
#' regex pattern for file name
#'
#' @param year Either \code{"all"} (default) or 4 digit character string
#' designating year of data to filter data files (format = YYYY).
#'
#' @return A normalized numeric year and regex pattern.
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
normalise_year <- function(year) {
  if (length(year) > 1L) {
    stop("`year` must be a non-NULL, single value.", call. = FALSE)
  }
  # remove whitespace
  y <- trimws(year)
  y <- gsub("\\s+", "", y)

  if (y == "all") {
    return(NULL)
    warning("`year = 'all'` will return a data file that may contain
    multiple years if present in folder supplied in file_path. To only return
    data from one year, specify the year in the 'year' argument.")
  } else{return(y)}

}

#' Resolve a data-processing version string to a file-matching pattern
#'
#' Internal helper called by \code{\link{get_logger_data}} to normalise
#' the \code{data_processing} argument and derive the regex pattern used
#' to find versioned CSV files on disk.
#'
#' @param data_processing Character scalar.  User-supplied version label.
#'   Recognized values (case-insensitive, whitespace-stripped):
#'   \describe{
#'     \item{\code{"v0.1"}}{Also \code{"v01"}, \code{"0.1"}.  Matches
#'       \code{*v0.1.csv}.}
#'     \item{\code{"v0.2"}}{Also \code{"v02"}, \code{"0.2"}.  Matches
#'       \code{*v0.2.csv}.}
#'     \item{\code{"v0.3"}}{Also \code{"v03"}, \code{"0.3"}.  Matches
#'       \code{*v0.3.csv}.}
#'     \item{\code{"v1.0"}}{Also \code{"v10"}, \code{"1.0"},
#'       \code{"1"}, \code{"final"}, \code{"processed"},
#'       \code{"clean"}.  Matches \code{*v1.0.csv}.}
#'   }
#'   A fuzzy fallback (partial regex) is attempted for unrecognised
#'   inputs before erroring.
#'
#' @return A two-element named list:
#'   \describe{
#'     \item{\code{data_processing}}{Canonical version string (e.g.
#'       \code{"v0.1"}).}
#'     \item{\code{pattern}}{Regex suitable for
#'       \code{list.files(pattern = ...)}.}
#'   }
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
resolve_data_processing_pattern <- function(data_processing) {

  if (is.null(data_processing) || length(data_processing) != 1L) {
    stop("`data_processing` must be a non-NULL, single value.", call. = FALSE)
  }

  dp <- tolower(trimws(data_processing))
  # remove whitespace
  dp <- gsub("\\s+", "", dp)

  # explicit mappings
  if(dp %in% c("v0.1", "v01", "0.1")) {
    dp <- "v0.1"
  } else if (dp %in% c("v0.2", "v02", "0.2")) {
    dp <- "v0.2"
  } else if (dp %in% c("v0.3", "v03", "0.3")) {
    dp <- "v0.3"
  } else if (dp %in% c("v1.0", "v10", "1.0", "v1", "1", "final", "processed", "clean")) {
    dp <- "v1.0"
  } else {
    # partial / fuzzy mappings as backup
    if (grepl("0\\.1$", dp))    dp <- "v0.1" #ends with 0.1
    else if (grepl("0\\.2$", dp))    dp <- "v0.2" #ends with 0.2
    else if (grepl("0\\.3$", dp))    dp <- "v0.3" #ends with 0.3
    else if (grepl("^v1$", dp))       dp <- "v1.0"
    # starts with v, any digit, ., ends with any digit
    else if (grepl("^v\\d*\\.\\d*$", dp))     dp <- dp
     else {
      stop(
        "`data_processing` expecting one of:\n",
        "  - 'v0.1': data bound and trimmed from bind_logger_files function \n",
        "  - 'v0.2': data joined with barometric data from add_nearest_baro function \n",
        "  - 'v0.3': data converted to new metric from convert_.... functions \n",
        "  - 'v1.#' / 'clean' data exported from write_clean_data function \n\n",
        "  - 'v#.#' / user specified version suffix \n\n",
        "You gave: '", data_processing, "'.",
        call. = FALSE
      )
    }
  }

  pattern <-
    switch(
    dp,
    "v0.1" = "v0.1\\.csv$",
    "v0.2" = "v0.2\\.csv$",
    "v0.3" = "v0.3\\.csv$",
    "v1.0" = "v1.0\\.csv$",
    paste0(dp,"\\.csv$") # user specified version suffix
  )

  list(data_processing = dp, pattern = pattern)
}

factor_cols <- c(
  "sn"
)

# get metric header from columns -------------------------------------
metric_config <- list(
  dissolvedoxygen = list(
    header   = "DO",
    column_name = c(
      "do_mgl",
      "do_mgl_adj"
    )
  ),
  barometric = list(
    header   = "BARO",
    column_name = c(
      "airpress_kPa",
      "airpress_kPa_adj"
    )
  ),
  waterlevel = list(
    header   = "WL",
    column_name = c(
      "waterlevel_m",
      "waterlevel_m_adj"
    )
  ),
  watertemp = list(
    header   = "WT",
    column_name = c(
      "watertemp_C",
      "watertemp_C_adj"
    )
  ),
  airtemp = list(
    header   = "AT",
    column_name = c(
      "airtemp_C",
      "airtemp_C_adj"
    )
  ),
  conductivity = list(
    header   = "CO",
    column_name = c(
      "conduct_uScm",
      "conduct_uScm_adj"
    )
  )
)


make_ribbon_plot <- function(data, mean_col, q5_col, q95_col,
                             y_lab, title, colour = "#233d4d") {
  req_cols <- c(mean_col, q5_col, q95_col)
  miss <- setdiff(req_cols, names(data))
  if (length(miss) > 0) {
    warning(
      "plot_clean_timeseries(): input_data is missing required column(s): ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }else{
    ggplot2::ggplot(data, ggplot2::aes(x = time_group)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[[q5_col]],
          ymax = .data[[q95_col]]
        ),
        alpha = 0.25,
        fill  = colour
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[mean_col]]),
        colour = colour
      ) +
      ggplot2::facet_wrap(~ site_station_code, scales = "free_y") +
      ggplot2::labs(title = title, x = "Date", y = y_lab) +
      ggplot2::theme_classic()
  }

}



make_clean_plot <- function(data, metric_col,
                             y_lab, colour = "#233d4d") {

  req_cols <- c(metric_col)
  miss <- setdiff(req_cols, names(data))
  if (length(miss) > 0) {
    warning(
      "plot_clean_timeseries(): input_data is missing required column(s): ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }else{
    ggplot2::ggplot(data, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[metric_col]]),
        colour = colour
      ) +
      ggplot2::facet_wrap(~ site_station_code, scales = "free_y") +
      ggplot2::labs( x = "Date", y = y_lab) +
      ggplot2::theme_classic()
  }

}
