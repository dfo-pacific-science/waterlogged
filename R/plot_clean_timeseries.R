#' Build summary plots for clean logger data
#'
#' ggplot time series panels for the summarised data.  Each metric
#' defines its own set of panels (up to three).
#'
#' @param select_station Either \code{"all"} (default) or one or more
#'   \code{site_station_code} values to filter on. If  \code{"all"}, data will be
#'   summarized for each station in  \code{"input_data"}
#' @param temporal_scale Temporal aggregation. Case and spaces ignored and
#'   normalised to one of:  \code{"hourly"}, \code{"daily"},
#'   \code{"weekly"}, \code{"monthly"}, \code{"yearly"}. "Hourly" aggregates to
#'   average-by-hour (using \code{floor_date}), will produce error if provided
#'   with hourly input data. Week start date is Monday by default.
#' @param input_data A data frame of clean logger observations, produced by
#' \code{\link{write_clean_data}}, or summarized clean data produced by
#' \code{\link{summarize_clean_data}}.
#' @param path_to_output_folder Character scalar. Optional root directory for
#'   csv file output. If \code{NULL} (default), no files are written
#'   (Shiny mode).
#'
#' @return A named list with elements \code{plot1}, \code{plot2},
#'   \code{plot3} (some may be \code{NULL}).
#'
#' @seealso \code{\link{summarize_clean_data}}
#'
#' @keywords internal
plot_clean_timeseries <- function(input_data,
                        select_station  = "all",
                        temporal_scale  =  "daily",
                        path_to_output_folder = NULL) {


# prepare arguments -------------------------------------------------------

  station_norm <- normalise_station(select_station)$station_norm
  temporal_norm <- normalise_temporal_scale(temporal_scale)

  if (!is.null(path_to_output_folder) && !endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

# check input data meets requirements -------------------------------------

  if (!is.null(input_data) && is.data.frame(input_data)) {
    # if time group column exists, then input data is summary from summarize_clean_data
    if("time_group" %in% names(input_data)){
      dat <- input_data
    # otherwise, it is timeseries data, ensure correct formatting
    } else {

      req_cols <- c("timestamp", "site_station_code")
      miss <- setdiff(req_cols, names(input_data))
      if (length(miss) > 0) {
        stop(
          "plot_clean_timeseries(): input_data is missing required column(s): ",
          paste(miss, collapse = ", "),
          call. = FALSE
        )
      }

      dat <- input_data

      # parse timestamp column (utils.R)
      if (!inherits(dat$timestamp, "POSIXct")) {
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
    }

  } else {
    if (is.null(input_data)) {
      stop("Must provide'input_data' as a data frame.", call. = FALSE)
    }
  }


# extract metric from column headers --------------------------------------

  # if any dat columns are found in the sublists, get the associated header for those matches
  # any, if at least one value is true
  if("time_group" %in% names(dat)){
    #summarized column headers
    summdat_names <- gsub("_mean$|_q5$|_q95$","", names(dat))
    matched_val <- unlist(lapply(Filter(function(x) any(summdat_names %in% x$column_name), metric_config), `[[`, "header"))

  }else{
    #timeseries columns headers
    matched_val <- unlist(lapply(Filter(function(x) any(names(dat) %in% x$column_name), metric_config), `[[`, "header"))
  }
  # if multiple headers match, only take the non-temperature headers
  if(length(matched_val)>1){
    metric_header <- matched_val[matched_val != "AT" & matched_val != "WT"]
  }else{metric_header <- matched_val}

  if (is.null(matched_val)) {
    stop("plot_clean_timeseries(): Unsupported metric to plot.",
         "\nExpected metrics are: \"dissolvedoxygen\", \"barometric\", \"waterlevel\", \"watertemp\", \"airtemp\", \"conductivity\".",
         "\nExpected column headers:  \"do_mgl_adj\", \"airpress_kPa_adj\", \"waterlevel_m_adj\", \"watertemp_C_adj\", \"airtemp_C_adj\", \"conduct_uScm_adj\".")
  }


# plot summary

  if("time_group" %in% names(dat)){
    label <- paste0("(", temporal_scale, ")")

    if (metric_header == "WL") {

      p1 <- make_ribbon_plot(dat, "waterlevel_m_adj_mean", "waterlevel_m_adj_q5",
                             "waterlevel_m_adj_q95", "Water Level (m)",
                             paste("Water Level", label), "#233d4d")


      p2 <- make_ribbon_plot(dat, "watertemp_C_adj_mean", "watertemp_C_adj_q5",
                             "watertemp_C_adj_q95", "Water Temperature (\u00b0C)",
                             paste("Water Temperature", label), "#619b8a")
      return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

    } else if (metric_header == "DO") {

      p1 <- make_ribbon_plot(dat, "do_mgl_adj_mean", "do_mgl_adj_q5",
                             "do_mgl_adj_q95", "DO (mg/L)",
                             paste("Dissolved Oxygen", label), "#a1c181")
      p2 <- make_ribbon_plot(dat, "watertemp_C_adj_mean", "watertemp_C_adj_q5",
                             "watertemp_C_adj_q95", "Water Temperature (\u00b0C)",
                             paste("Water Temperature", label), "#619b8a")
      if("do_percsat_adj_mean" %in% names(dat)){
        p3 <- make_ribbon_plot(dat, "do_percsat_adj_mean", "do_percsat_adj_q5",
                               "do_percsat_adj_q95", "DO (% air sat.)",
                               paste("DO (% air sat.)", label), "#a1c181")
        return(list(plot1 = p1, plot2 = p2, plot3 = p3))
      }else{
        return(list(plot1 = p1, plot2 = p2, plot3 = NULL))
      }

    } else if (metric_header == "BARO") {

      p1 <- make_ribbon_plot(dat, "airpress_kPa_adj_mean", "airpress_kPa_adj_q5",
                             "airpress_kPa_adj_q95", "Air Pressure (kPa)",
                             paste("Barometric Pressure", label), "#915e3d")
      p2 <- make_ribbon_plot(dat, "airtemp_C_adj_mean", "airtemp_C_adj_q5",
                             "airtemp_C_adj_q95", "Air Temperature (\u00b0C)",
                             paste("Air Temperature", label), "#FDB840")
      return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

    } else if (metric_header=="CO") {

      p1 <- make_ribbon_plot(dat, "conduct_uScm_mean", "conduct_uScm_q5",
                             "conduct_uScm_q95", "Conductivity (\u00b5S/cm)",
                             paste("Conductivity", label), "#000000")
      p2 <- make_ribbon_plot(dat, "watertemp_C_adj_mean", "watertemp_C_adj_q5",
                             "watertemp_C_adj_q95", "Water Temperature (\u00b0C)",
                             paste("Water Temperature", label), "#619b8a")
      if("spc_uScm_adj_mean" %in% names(dat)){
      p3 <- make_ribbon_plot(dat, "spc_uScm_adj_mean", "spc_uScm_adj_q5",
                             "spc_uScm_adj_q95", "Specific Conductance (\u00b5S/cm)",
                             paste("Specific Conductance", label), "#4682B4")
      return(list(plot1 = p1, plot2 = p2, plot3 = p3))

      }else {
        return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

      }

    } else if (metric_header=="AT") {

      p1 <- make_ribbon_plot(dat, "airtemp_C_adj_mean", "airtemp_C_adj_q5",
                             "airtemp_C_adj_q95", "Air Temperature (\u00b0C)",
                             paste("Air Temperature", label), "#FDB840")
      return(list(plot1 = p1, plot2 = NULL, plot3 = NULL))

    } else if (metric_header=="WT") {

      p1 <- make_ribbon_plot(dat, "watertemp_C_adj_mean", "watertemp_C_adj_q5",
                             "watertemp_C_adj_q95", "Water Temperature (\u00b0C)",
                             paste("Water Temperature", label), "#619b8a")
      return(list(plot1 = p1, plot2 = NULL, plot3 = NULL))

    } else {
      return(list(plot1 = NULL, plot2 = NULL, plot3 = NULL))
    }
  }

  #  if no time_group, column, make clean timeseries plot -------------------
  if("timestamp" %in% names(dat)){

  if (metric_header == "WL") {

    p1 <- make_clean_plot(dat, "waterlevel_m_adj", "Water Level (m)", "#233d4d")
    p2 <- make_clean_plot(dat, "watertemp_C_adj", "Water Temperature (\u00b0C)",
                           "#619b8a")
    return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

  } else if (metric_header == "DO") {

    p1 <- make_clean_plot(dat, "do_mgl_adj","DO (mg/L)",
                           "#a1c181")
    p2 <- make_clean_plot(dat, "watertemp_C_adj", "Water Temperature (\u00b0C)",
                          "#619b8a")
    if("do_percsat_adj_mean" %in% names(dat)){
      p3 <- make_clean_plot(dat, "do_percsat_adj", "DO (% air sat.)",
                              "#a1c181")
      return(list(plot1 = p1, plot2 = p2, plot3 = p3))
    }else{
      return(list(plot1 = p1, plot2 = p2, plot3 = NULL))
    }

  } else if (metric_header == "BARO") {

    p1 <- make_clean_plot(dat, "airpress_kPa_adj", "Air Pressure (kPa)",
                           "#915e3d")
    p2 <- make_clean_plot(dat, "airtemp_C_adj", "Air Temperature (\u00b0C)",
                          "#FDB840")
    return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

  } else if (metric_header=="CO") {

    p1 <- make_clean_plot(dat, "conduct_uScm",  "Conductivity (\u00b5S/cm)",
                          "#000000")
    p2 <- make_clean_plot(dat, "watertemp_C_adj", "Water Temperature (\u00b0C)",
                          "#619b8a")
    if("spc_uScm_adj_mean" %in% names(dat)){
      p3 <- make_clean_plot(dat, "spc_uScm_adj", "Specific Conductance (\u00b5S/cm)",
                            "#4682B4")
      return(list(plot1 = p1, plot2 = p2, plot3 = p3))

    }else {
      return(list(plot1 = p1, plot2 = p2, plot3 = NULL))

    }

  } else if (metric_header=="AT") {

    p1 <- make_clean_plot(dat, "airtemp_C_adj", "Air Temperature (\u00b0C)","#FDB840")
    return(list(plot1 = p1, plot2 = NULL, plot3 = NULL))

  } else if (metric_header=="WT") {

    req_cols <- c("watertemp_C_adj")

    p1 <- make_clean_plot(dat, "watertemp_C_adj", "Water Temperature (\u00b0C)",
                         "#619b8a")
    return(list(plot1 = p1, plot2 = NULL, plot3 = NULL))

  } else {
    return(list(plot1 = NULL, plot2 = NULL, plot3 = NULL))
    }
  }
}

