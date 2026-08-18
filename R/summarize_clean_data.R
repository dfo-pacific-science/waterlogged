#' Summarise logger data
#'
#' This function works with the original wide-format processed files
#' (e.g. columns like \code{waterlevel_m_U20_adj}, \code{do_mgl_U26_adj}).
#' Summarisation is only performed for clean data (v1.0), and only for the
#' measurement variables contained in input_data with suffix "_adj".
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
#' \code{\link{write_clean_data}}.
#' @param path_to_output_folder Character scalar. Optional root directory for
#'   csv file output. If \code{NULL} (default), no files are written
#'   (Shiny mode).

#'
#' @return
#'   \itemize{
#'     \item \code{data}: summarised data frame
#'   }
#'
#' @seealso \code{\link{write_clean_data}} (produces the v1.0 files this
#'   function reads), \code{\link{bind_hobo_files}} (produces v0.1),
#'   \code{\link{get_logger_data_helpers}} (helper files)
#'
#' @export

summarize_clean_data <- function(input_data,
                            select_station  = "all",
                            temporal_scale  =  "daily",
                            path_to_output_folder = NULL
                            ) {


# prepare arguments -------------------------------------------------------


  temporal_norm <- normalise_temporal_scale(temporal_scale)
  station_norm <- normalise_station(select_station)$station_norm

  if (!is.null(path_to_output_folder) && !endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }


# check input data meets requirements -------------------------------------

  if (!is.null(input_data) && is.data.frame(input_data)) {
    req_cols <- c("timestamp", "site_station_code")
    miss <- setdiff(req_cols, names(input_data))
    if (length(miss) > 0) {
      stop(
        "summarize_clean_data(): input_data is missing required column(s): ",
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
  } else {
    if (is.null(input_data)) {
      stop("Must provide'input_data' as a data frame.", call. = FALSE)
    }
}

  # optional station filter
  if (station_norm!="all" & !is.null(station_norm)) {
    dat <- dat |> dplyr::filter(site_station_code %in% station_norm)
  }

  # grab station code if data is only from one station and user did not specify select_station
  if(station_norm=="all" & length(unique(dat$site_station_code))==1){
    station_norm<-unique(dat$site_station_code)
  }


  # if any dat columns are found in the sublists, get the associated header for those matches
  # any, if at least one value is true
  matched_val <- unlist(lapply(Filter(function(x) any(names(dat) %in% x$column_name), metric_config), `[[`, "header"))
  # if multiple headers match, only take the non-temperature headers
  if(length(matched_val>1)){
    metric_header <- matched_val[matched_val != "AT" & matched_val != "WT"]
  }else{metric_header <- matched_val}


  if (is.null(matched_val)) {
    stop("summarize_clean_data(): Unsupported metric to summarize: ", metric,
         "\nExpected metrics are: \"dissolvedoxygen\", \"barometric\", \"waterlevel\", \"watertemp\", \"airtemp\", \"conductivity\".",
         "\nExpected column headers:  \"do_mgl_adj\", \"airpress_kPa_adj\", \"waterlevel_m_adj\", \"watertemp_C_adj\", \"airtemp_C_adj\", \"conduct_uScm_adj\".")
  }


  # --- create time_group ---------------------------------------------
  dat <- dat |>
    dplyr::mutate(
      time_group = dplyr::case_when(
        temporal_norm == "hourly"  ~ lubridate::floor_date(timestamp, "hour"),
        temporal_norm == "daily"   ~ lubridate::floor_date(timestamp, "day"),
        temporal_norm == "weekly"  ~ lubridate::floor_date(timestamp, "week", week_start = 1),
        temporal_norm == "monthly" ~ lubridate::floor_date(timestamp, "month"),
        temporal_norm == "yearly"  ~ lubridate::floor_date(timestamp, "year")
      )
    )

  if (all(is.na(dat$time_group))) {
    stop("Failed to create `time_group` for temporal_scale = ", temporal_scale, call. = FALSE)
  }

  if (all(dat$time_group==dat$timestamp)) {
    stop("Timestamp interval is the same as 'temporal_scale':", temporal_scale,
         ". \n'temporal_scale' must be greater than timestamp interval. ", call. = FALSE)
  }

  group_vars <- c("site_station_code", "time_group")

  out <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(dplyr::across(dplyr::ends_with("_adj"),
                                   list(
                                     mean=~mean(.x, na.rm=TRUE),
                                     q5=~quantile(.x, na.rm=TRUE, probs = 0.05),
                                     q95=~quantile(.x, na.rm=TRUE, probs = 0.95),
                                     min=~min(.x, na.rm=TRUE),
                                     max=~max(.x, na.rm=TRUE),
                                     n=~sum(!is.na(.x)))),
                     .groups = "drop")


  # round numeric, clean Inf/NaN ------------------------------------------------
  out <- out |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))

  out[] <- lapply(out, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x) | is.nan(x)] <- NA_real_
    }
    x
  })

  return(out)

# write CSV (guarded for Shiny / in-memory mode) --------------------------

  if (!is.null(path_to_output_folder)) {

    if(station_norm=="all") {
      site_i <- "multistation"
    } else {
        site_i<- station_norm
        }

    # date range for filename
    start_date <- gsub("\\D", "", as.character(min(lubridate::as_date(dat$timestamp))))
    end_date   <- gsub("\\D", "", as.character(max(lubridate::as_date(dat$timestamp))))


    out_name <- sprintf("%s_%s_%s_%s_%s.csv", site_i, metric_header, temporal_norm, start_date, end_date)
    out_path <- paste0(path_to_output_folder, out_name)

   utils::write.csv(out, out_path, row.names = FALSE)
   message(sprintf("%s summary for %s %s written to %s", temporal_norm, site_i, metric_header, out_path))
  }

}
