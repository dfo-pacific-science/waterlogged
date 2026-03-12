# ---- Helpers ---------------------------------------------------------------

#' Normalise a user-supplied temporal scale string
#'
#' Internal helper called by \code{\link{get_logger_data}} to validate
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
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
normalise_temporal_scale <- function(temporal_scale) {
  if (is.null(temporal_scale) || length(temporal_scale) != 1L) {
    stop("`temporal_scale` must be a single, non-NULL value.", call. = FALSE)
  }

  ts <- tolower(trimws(temporal_scale))
  ts <- gsub("\\s+", "", ts)

  if (ts %in% c("none", "raw", "original"))          return("none")
  if (ts %in% c("hourly", "hour", "hr"))             return("hourly")
  if (ts %in% c("daily", "day"))                     return("daily")
  if (ts %in% c("weekly", "week", "wk"))             return("weekly")
  if (ts %in% c("monthly", "month", "mo"))           return("monthly")
  if (ts %in% c("yearly", "year", "yr", "annual"))   return("yearly")

  stop(
    "Unknown `temporal_scale`. Expected one of: ",
    "'none', 'hourly', 'daily', 'weekly', 'monthly', 'yearly'",
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
    dat$timestamp <- suppressWarnings(
      lubridate::ymd_hms(dat$timestamp, tz = "UTC")
    )
    if (all(is.na(dat$timestamp))) {
      warning(
        "read_logger_file(): all timestamps failed to parse in '",
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
#' @param allow_all Logical.  When \code{TRUE}, the special value
#'   \code{"all"} is accepted and returned as-is.  Default \code{FALSE}.
#'
#' @return A single canonical metric string.
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
normalise_metric <- function(metric, allow_all = FALSE) {
  if (is.null(metric) || length(metric) != 1L) {
    stop("`metric` must be a single, non-NULL value.", call. = FALSE)
  }
  
  m <- tolower(trimws(metric))
  m <- gsub("\\s+", "", m)
  
  if (m == "all") {
    if (allow_all) return("all")
    stop("`metric = 'all'` is only allowed when explicitly permitted.", call. = FALSE)
  }
  
  if (m %in% c("waterlevel", "stage"))               return("waterlevel")
  if (m %in% c("do", "dissolvedoxygen", "oxygen"))   return("dissolvedoxygen")
  if (m %in% c("baro", "barometric", "airpressure")) return("barometric")
  if (m %in% c("watertemp", "watertemperature"))     return("watertemp")
  if (m %in% c("airtemp", "airtemperature"))         return("airtemp")
  if (m %in% c("conductivity", "cond", "spc"))       return("conductivity")

  stop(
    "Unknown `metric`. Expected one of: ",
    "'waterlevel', 'dissolvedoxygen', 'barometric', 'conductivity', 'watertemp', 'airtemp'",
    " (case and spaces ignored). You gave: '", metric, "'.",
    call. = FALSE
  )
}


#' Resolve a data-processing version string to a file-matching pattern
#'
#' Internal helper called by \code{\link{get_logger_data}} to normalise
#' the \code{data_processing} argument and derive the regex pattern used
#' to find versioned CSV files on disk.
#'
#' @param data_processing Character scalar.  User-supplied version label.
#'   Recognised values (case-insensitive, whitespace-stripped):
#'   \describe{
#'     \item{\code{"raw"}}{Matches any \code{.csv} file.}
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
    stop("`data_processing` must be a single value.", call. = FALSE)
  }
  
  dp <- tolower(trimws(data_processing))
  dp <- gsub("\\s+", "", dp)
  
  # explicit mappings
  if (dp %in% c("raw")) {
    dp <- "raw"
  } else if (dp %in% c("v0.1", "v01", "0.1")) {
    dp <- "v0.1"
  } else if (dp %in% c("v0.2", "v02", "0.2")) {
    dp <- "v0.2"
  } else if (dp %in% c("v0.3", "v03", "0.3")) {
    dp <- "v0.3"
  } else if (dp %in% c("v1.0", "v10", "1.0", "1", "final", "processed", "clean")) {
    dp <- "v1.0"
  } else {
    # partial / fuzzy mappings as backup
    if (grepl("^raw", dp))           dp <- "raw"
    else if (grepl("0\\.1$", dp))    dp <- "v0.1"
    else if (grepl("0\\.2$", dp))    dp <- "v0.2"
    else if (grepl("0\\.3$", dp))    dp <- "v0.3"
    else if (grepl("^v1", dp))       dp <- "v1.0"
    else {
      stop(
        "`data_processing` must be one of:\n",
        "  - 'raw'\n",
        "  - 'v0.1'\n",
        "  - 'v0.2'\n",
        "  - 'v0.3'\n",
        "  - 'v1.0' / 'clean' (final version)\n\n",
        "You gave: '", data_processing, "'.",
        call. = FALSE
      )
    }
  }
  
  pattern <- switch(
    dp,
    "raw"  = "\\.csv$",      # any csv
    "v0.1" = "v0.1\\.csv$",
    "v0.2" = "v0.2\\.csv$",
    "v0.3" = "v0.3\\.csv$",
    "v1.0" = "v1.0\\.csv$",
    stop("Unexpected `data_processing` value after QC: ", dp, call. = FALSE)
  )
  
  list(data_processing = dp, pattern = pattern)
}



#' Temporally aggregate clean logger data by metric
#'
#' Internal helper called by \code{\link{get_logger_data}} to compute
#' grouped (mean, min, max) summaries of QAQC'd \code{_adj} columns at
#' the requested temporal resolution.  Each supported metric defines its
#' own set of required columns and summary statistics.
#'
#' @param dat Data frame of clean logger observations.
#'   Must contain \code{site_station_code}, \code{timestamp} (POSIXct),
#'   and the metric-specific \code{_adj} columns (see \strong{Details}).
#' @param metric_norm Character scalar.  One of the canonical metric
#'   names returned by \code{normalise_metric()}: \code{"dissolvedoxygen"},
#'   \code{"waterlevel"}, \code{"barometric"}, \code{"conductivity"},
#'   \code{"watertemp"}, or \code{"airtemp"}.
#' @param temporal_scale Character scalar.  One of \code{"hourly"},
#'   \code{"daily"}, \code{"weekly"}, \code{"monthly"}, or
#'   \code{"yearly"}.  Already validated upstream by
#'   \code{\link{get_logger_data}}.
#'
#' @return A tibble grouped by \code{site_station_code} and
#'   \code{time_group}, with mean/min/max columns for each metric
#'   variable (rounded to 2 decimal places; \code{Inf}/\code{NaN}
#'   replaced with \code{NA}).
#'
#' @details
#' Required \code{_adj} columns by metric:
#' \describe{
#'   \item{\code{"dissolvedoxygen"}}{\code{do_mgl_adj},
#'     \code{do_percsat_adj}, \code{watertemp_C_adj}.}
#'   \item{\code{"waterlevel"}}{\code{waterlevel_m_adj},
#'     \code{watertemp_C_adj}.}
#'   \item{\code{"barometric"}}{\code{airpress_kPa_adj},
#'     \code{airtemp_C_adj}.}
#'   \item{\code{"watertemp"}}{\code{watertemp_C_adj}.}
#'   \item{\code{"airtemp"}}{\code{airtemp_C_adj}.}
#' }
#'
#' @seealso \code{\link{get_logger_data}} (sole caller),
#'   \code{\link{write_clean_data}} (produces the v1.0 files typically
#'   summarised here)
#'
#' @keywords internal
summarise_clean_data <- function(dat, metric_norm, temporal_scale) {
  # temporal_scale already validated upstream
  
  # --- create time_group ---------------------------------------------
  dat <- dat |>
    dplyr::mutate(
      time_group = dplyr::case_when(
        temporal_scale == "hourly"  ~ lubridate::floor_date(timestamp, "hour"),
        temporal_scale == "daily"   ~ lubridate::floor_date(timestamp, "day"),
        temporal_scale == "weekly"  ~ lubridate::floor_date(timestamp, "week", week_start = 1),
        temporal_scale == "monthly" ~ lubridate::floor_date(timestamp, "month"),
        temporal_scale == "yearly"  ~ lubridate::floor_date(timestamp, "year")
      )
    )
  
  if (all(is.na(dat$time_group))) {
    stop("Failed to create `time_group` for temporal_scale = ", temporal_scale, call. = FALSE)
  }
  
  group_vars <- c("site_station_code", "time_group")
  
  # --- metric-specific summaries -------------------------------------
  if (metric_norm == "dissolvedoxygen") {

    required <- c("do_mgl_adj", "do_percsat_adj", "watertemp_C_do_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'dissolvedoxygen', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 DO data)?",
        call. = FALSE
      )
    }
    
    out <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_do_mgl      = mean(do_mgl_adj,      na.rm = TRUE),
        mean_do_percsat  = mean(do_percsat_adj,  na.rm = TRUE),
        mean_watertemp_C = mean(watertemp_C_do_adj, na.rm = TRUE),
        min_do_mgl       = min(do_mgl_adj,       na.rm = TRUE),
        min_do_percsat   = min(do_percsat_adj,   na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_do_adj,  na.rm = TRUE),
        max_do_mgl       = max(do_mgl_adj,       na.rm = TRUE),
        max_do_percsat   = max(do_percsat_adj,   na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_do_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else if (metric_norm == "waterlevel") {
    
    required <- c("waterlevel_m_adj", "watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'waterlevel', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 WL data)?",
        call. = FALSE
      )
    }
    
    out <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_waterlevel_m = mean(waterlevel_m_adj, na.rm = TRUE),
        mean_watertemp_C  = mean(watertemp_C_adj,  na.rm = TRUE),
        min_waterlevel_m  = min(waterlevel_m_adj,  na.rm = TRUE),
        min_watertemp_C   = min(watertemp_C_adj,   na.rm = TRUE),
        max_waterlevel_m  = max(waterlevel_m_adj,  na.rm = TRUE),
        max_watertemp_C   = max(watertemp_C_adj,   na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "barometric") {
    
    required <- c("airpress_kPa_adj", "airtemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'barometric', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 BARO data)?",
        call. = FALSE
      )
    }
    
    out <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_airpress_kPa = mean(airpress_kPa_adj, na.rm = TRUE),
        mean_airtemp_C    = mean(airtemp_C_adj,    na.rm = TRUE),
        min_airpress_kPa  = min(airpress_kPa_adj,  na.rm = TRUE),
        min_airtemp_C     = min(airtemp_C_adj,     na.rm = TRUE),
        max_airpress_kPa  = max(airpress_kPa_adj,  na.rm = TRUE),
        max_airtemp_C     = max(airtemp_C_adj,     na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "airtemp") {
    
    required <- c("airtemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'airtemp', expected column: airtemp_C_adj. Missing: ",
        paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 AT data)?",
        call. = FALSE
      )
    }
    
    out <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_airtemp_C = mean(airtemp_C_adj, na.rm = TRUE),
        min_airtemp_C  = min(airtemp_C_adj,  na.rm = TRUE),
        max_airtemp_C  = max(airtemp_C_adj,  na.rm = TRUE),
        .groups        = "drop"
      )
    
  } else if (metric_norm == "watertemp") {
    
    required <- c("watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'watertemp', expected column: watertemp_C_adj. Missing: ",
        paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 WT data)?",
        call. = FALSE
      )
    }
    
    out <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_watertemp_C = mean(watertemp_C_adj, na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_adj,  na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else if (metric_norm == "conductivity") {

    required <- c("conduct_uScm_adj", "watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'conductivity', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 COND data)?",
        call. = FALSE
      )
    }

    out <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_conduct_uScm = mean(conduct_uScm_adj, na.rm = TRUE),
        mean_watertemp_C  = mean(watertemp_C_adj,  na.rm = TRUE),
        min_conduct_uScm  = min(conduct_uScm_adj,  na.rm = TRUE),
        min_watertemp_C   = min(watertemp_C_adj,   na.rm = TRUE),
        max_conduct_uScm  = max(conduct_uScm_adj,  na.rm = TRUE),
        max_watertemp_C   = max(watertemp_C_adj,   na.rm = TRUE),
        .groups           = "drop"
      )

  } else {
    stop("Metric '", metric_norm, "' not implemented in summarise_clean_data().", call. = FALSE)
  }
  
  # round numeric, clean Inf/NaN ------------------------------------------------
  out <- out |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))
  
  out[] <- lapply(out, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x) | is.nan(x)] <- NA_real_
    }
    x
  })
  
  out
}


factor_cols <- c(
  "sn", "baro_qaqc_code"
)


#' Build summary plots for get_logger_data
#'
#' Internal helper called by \code{\link{get_logger_data}} to create
#' ggplot time series panels for the summarised data.  Each metric
#' defines its own set of panels (up to three).
#'
#' @param dat_sum Data frame produced by \code{summarise_clean_data()}.
#'   Must contain \code{site_station_code}, \code{time_group}, and the
#'   metric-specific mean/min/max columns.
#' @param metric_norm Character scalar.  Canonical metric name.
#' @param temporal_scale Character scalar.  The temporal resolution
#'   label used in the plot title.
#'
#' @return A named list with elements \code{plot1}, \code{plot2},
#'   \code{plot3} (some may be \code{NULL}).
#'
#' @seealso \code{\link{get_logger_data}} (sole caller)
#'
#' @keywords internal
build_plots <- function(dat_sum, metric_norm, temporal_scale) {

  make_ribbon_plot <- function(data, mean_col, min_col, max_col,
                               y_lab, title, colour = "#233d4d") {
    ggplot2::ggplot(data, ggplot2::aes(x = time_group)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = .data[[min_col]],
          ymax = .data[[max_col]]
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

  label <- paste0("(", temporal_scale, ")")

  if (metric_norm == "waterlevel") {
    p1 <- make_ribbon_plot(dat_sum, "mean_waterlevel_m", "min_waterlevel_m",
                           "max_waterlevel_m", "Water Level (m)",
                           paste("Water Level", label), "#233d4d")
    p2 <- make_ribbon_plot(dat_sum, "mean_watertemp_C", "min_watertemp_C",
                           "max_watertemp_C", "Water Temp (\u00b0C)",
                           paste("Water Temperature", label), "#619b8a")
    return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

  } else if (metric_norm == "dissolvedoxygen") {
    p1 <- make_ribbon_plot(dat_sum, "mean_do_mgl", "min_do_mgl",
                           "max_do_mgl", "DO (mg/L)",
                           paste("Dissolved Oxygen", label), "#a1c181")
    p2 <- make_ribbon_plot(dat_sum, "mean_do_percsat", "min_do_percsat",
                           "max_do_percsat", "DO (% sat)",
                           paste("DO % Saturation", label), "#a1c181")
    p3 <- make_ribbon_plot(dat_sum, "mean_watertemp_C", "min_watertemp_C",
                           "max_watertemp_C", "Water Temp (\u00b0C)",
                           paste("Water Temperature", label), "#619b8a")
    return(list(plot1 = p1, plot2 = p2, plot3 = p3))

  } else if (metric_norm == "barometric") {
    p1 <- make_ribbon_plot(dat_sum, "mean_airpress_kPa", "min_airpress_kPa",
                           "max_airpress_kPa", "Air Pressure (kPa)",
                           paste("Barometric Pressure", label), "#915e3d")
    p2 <- make_ribbon_plot(dat_sum, "mean_airtemp_C", "min_airtemp_C",
                           "max_airtemp_C", "Air Temp (\u00b0C)",
                           paste("Air Temperature", label), "#FDB840")
    return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

  } else if (metric_norm == "conductivity") {
    p1 <- make_ribbon_plot(dat_sum, "mean_conduct_uScm", "min_conduct_uScm",
                           "max_conduct_uScm", "Conductivity (\u00b5S/cm)",
                           paste("Conductivity", label), "#000000")
    p2 <- make_ribbon_plot(dat_sum, "mean_watertemp_C", "min_watertemp_C",
                           "max_watertemp_C", "Water Temp (\u00b0C)",
                           paste("Water Temperature", label), "#619b8a")
    return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

  } else if (metric_norm == "airtemp") {
    p1 <- make_ribbon_plot(dat_sum, "mean_airtemp_C", "min_airtemp_C",
                           "max_airtemp_C", "Air Temp (\u00b0C)",
                           paste("Air Temperature", label), "#FDB840")
    return(list(plot1 = p1, plot2 = NULL, plot3 = NULL))

  } else if (metric_norm == "watertemp") {
    p1 <- make_ribbon_plot(dat_sum, "mean_watertemp_C", "min_watertemp_C",
                           "max_watertemp_C", "Water Temp (\u00b0C)",
                           paste("Water Temperature", label), "#619b8a")
    return(list(plot1 = p1, plot2 = NULL, plot3 = NULL))

  } else {
    return(list(plot1 = NULL, plot2 = NULL, plot3 = NULL))
  }
}

