#' Retrieve data
#'
#' Retrieve processed data by selecting desired variable, waterbody type, station, clean or raw, and temporal scale
#' @param path_to_data path to location of desired files encased in quotations
#' @param select_station unique site_station_code
#' @param logger_type either dissolvedoxygen_U26, waterlevel_U20, or baro_U20
#' @param data_processing either raw or clean, clean data has been processed via QAQC protocols, raw data should not be summarized ie. set temporal_scale to hourly
#' @param temporal_scale either hourly, daily or monthly, daily and monthly return mean values of selected variable
#' @return dataframe
#' @export

get_logger_data <- function(path_to_data, logger_type, data_processing = "clean", select_station = "all", temporal_scale = "hourly") {

  # input errors
  if(logger_type != "baro_U20" & logger_type != "waterlevel_U20"& logger_type != "dissolvedoxygen_U26" & logger_type != "airtemp_TidbiT" & logger_type != "watertemp_TidbiT") {
    print("Uh oh! logger_type not recognized. Must be either dissolvedoxygen_U26, waterlevel_U20, baro_U20, airtemp_TidbiT or watertemp_TidbiT.")
  }

  # raw data
  if(data_processing=="raw") {
    file_names <- list.files(path = path_to_data, pattern = "\\.csv$") # extract file names ending in csv
  }
  # the processed folder contains output of several stages of data processed.
  # For processed data that is the output of bind_hobo_files
  # extract ONLY file names ending in "v0.1.csv"
  if(data_processing=="v0.1") {
    file_names <- list.files(path = path_to_data, pattern = "v0.1.csv$")
  }
  # For processed data that is the output of add_nearest_baro
  # extract ONLY file names ending in "v0.2.csv"
  if(data_processing=="v0.2") {
    file_names <- list.files(path = path_to_data, pattern = "v0.2.csv$")
  }
  # For processed data that is the output of convert_waterlevel_m
  # extract ONLY file names ending in "v0.3.csv"
  if(data_processing=="v0.3") {
    file_names <- list.files(path = path_to_data, pattern = "v0.3.csv$")
  }

  # For processed data that is the output of write_clean_data
  # extract ONLY file names ending in "v1.0.csv"
  if(data_processing=="clean") {
    file_names <- list.files(path = path_to_data, pattern = "v1.0.csv$")
  }

  path_to_files <- paste0(path_to_data, file_names) # makes string of full file path for all csv files

  extract_data_from_file <- function(file) {
    data <- read.csv(file, header = TRUE,
                     stringsAsFactors = FALSE)
    # format time stamp
    data$timestamp <-as.POSIXct(data$timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

    # format factor variables
    try(data$sn <- as.factor(data$sn), silent = TRUE)
    try(data$baro_code <- as.factor(data$baro_code), silent = TRUE)

    data$site_station_code <- as.factor(data$site_station_code)
    try(data$qaqc_code <- as.factor(data$qaqc_code), silent = TRUE)
    try(data$wl_qaqc_code <- as.factor(data$wl_qaqc_code), silent = TRUE)
    try(data$do_qaqc_code <- as.factor(data$do_qaqc_code), silent = TRUE)
    try(data$wt_qaqc_code <- as.factor(data$wt_qaqc_code), silent = TRUE)
    try(data$at_qaqc_code <- as.factor(data$at_qaqc_code), silent = TRUE)
    try(data$baro_qaqc_code <- as.factor(data$baro_qaqc_code), silent = TRUE)

    return(data)
  } # end of extract data files loop

  dat1 <- map_df(path_to_files, extract_data_from_file)

  # optional select station
  if(select_station != "all") {
    dat1 <- dat1 %>% filter(site_station_code %in% select_station)
  }

  # optional select scale ONLY FOR CLEAN DATA

  if(data_processing=="clean"&temporal_scale!="hourly") {
    # make new grouping_var column
    dat1$grouping_var <- NA
    #group by selected scale
    if(temporal_scale == "daily") {
      dat1$grouping_var <- date(dat1$timestamp)
    }
    if(temporal_scale == "weekly") {
      dat1$grouping_var <- week(dat1$timestamp)
    }
    if(temporal_scale == "monthly") {
      dat1$grouping_var <- month(dat1$timestamp)
    }
    if(temporal_scale == "yearly") {
      dat1$grouping_var <- year(dat1$timestamp)
    }
    # columns differ by logger type
    if(logger_type=="dissolvedoxygen_U26"){
      dat1 <- dat1 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_do_mgl = mean(do_mgl_U26_adj, na.rm = TRUE),
                         mean_do_percsat = mean(do_percsat_U26_adj, na.rm = TRUE),
                         mean_watertemp_C = mean(watertemp_C_U26_adj, na.rm = TRUE),
                         min_do_mgl = min(do_mgl_U26_adj, na.rm = TRUE),
                         min_do_percsat = min(do_percsat_U26_adj, na.rm = TRUE),
                         min_watertemp_C = min(watertemp_C_U26_adj, na.rm = TRUE),
                         max_do_mgl = max(do_mgl_U26_adj, na.rm = TRUE),
                         max_do_percsat = max(do_percsat_U26_adj, na.rm = TRUE),
                         max_watertemp_C = max(watertemp_C_U26_adj, na.rm = TRUE))
    }
    if(logger_type=="waterlevel_U20"){
      dat1 <- dat1 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_waterlevel_m = mean(waterlevel_m_U20_adj, na.rm = TRUE),
                         mean_watertemp_C = mean(watertemp_C_U20_adj, na.rm = TRUE),
                         max_waterlevel_m = max(waterlevel_m_U20_adj, na.rm = TRUE),
                         max_watertemp_C = max(watertemp_C_U20_adj, na.rm = TRUE),
                         min_waterlevel_m = min(waterlevel_m_U20_adj, na.rm = TRUE),
                         min_watertemp_C = min(watertemp_C_U20_adj, na.rm = TRUE))
    }
    if(logger_type=="baro_U20"){
      dat1 <- dat1 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_airpress_kPa = mean(airpress_kPa_U20, na.rm = TRUE),
                         mean_airtemp_C = mean(airtemp_C_U20_adj, na.rm = TRUE),
                         min_airpress_kPa = min(airpress_kPa_U20, na.rm = TRUE),
                         min_airtemp_C = min(airtemp_C_U20_adj, na.rm = TRUE),
                         max_airpress_kPa = max(airpress_kPa_U20, na.rm = TRUE),
                         max_airtemp_C = max(airtemp_C_U20_adj, na.rm = TRUE))
    }
    if(logger_type=="airtemp_TidbiT"){
      dat1 <- dat1 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_airtemp_C = mean(airtemp_C_TidbiT_adj, na.rm = TRUE),
                         min_airtemp_C = min(airtemp_C_TidbiT_adj, na.rm = TRUE),
                         max_airtemp_C = max(airtemp_C_TidbiT_adj, na.rm = TRUE))
    }
    if(logger_type=="watertemp_TidbiT"){
      dat1 <- dat1 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_watertemp_C = mean(watertemp_C_TidbiT_adj, na.rm = TRUE),
                         min_watertemp_C = min(watertemp_C_TidbiT_adj, na.rm = TRUE),
                         max_watertemp_C = max(watertemp_C_TidbiT_adj, na.rm = TRUE))
    }

    # rename columns
    if(temporal_scale == "daily") {
      colnames(dat1)[colnames(dat1) == 'grouping_var'] <- 'date'

    }
    if(temporal_scale == "weekly") {
      colnames(dat1)[colnames(dat1) == 'grouping_var'] <- 'week'
    }
    if(temporal_scale == "monthly") {
      colnames(dat1)[colnames(dat1) == 'grouping_var'] <- 'month'
    }
    if(temporal_scale == "yearly") {
      colnames(dat1)[colnames(dat1) == 'grouping_var'] <- 'year'
    }

    dat1 <- dat1 %>% mutate(across(where(is.numeric), round, 2))

  } # end of select_temporal_scale loop

  # return requested dataset
  return(dat1)

} # end of get_logger_data loop
