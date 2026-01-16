#' Save clean data
#'
#' Save clean water level data as .csv for a single site_station_code
#' @param input_data Dataframe with waterlevel in metres for a single site_station_code
#' @param path_to_output_folder The desired folder for output of bound dissolved oxygen and barometric pressure per station
#' @param logger_type HOBO logger type,either waterlevel_U20, baro_U20, dissolvedoxygen_U26, conductivity_U24, watertemp_TidbiT, or airtemp_TidbiT
#' @param var_do_percsat Name of column for dissolved oxygen in percent air saturation in input data
#' @param var_do_mgl Name of column for dissolved oxygen in mg per litre in input data
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_conduct_uScm Name of column for conductivity (in microSiemens per centimeter) in input data
#' @param var_spc_uScm Name of column for specific conductance (in microSiemens per centimeter) in input data
#' @param var_airpress_kPa Name of column for air pressure (in kPa) in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @return Clean water level data for station as csv
#' @export
#' @import dplyr
#' @import tidyr
#' @import lubridate


write_clean_data <- function(input_data, path_to_output_folder, logger_type, var_do_mgl = "do_mgl_U26", var_do_percsat = "do_percsat_U26",
                             var_waterlevel_m = "waterlevel_m_U20", var_watertemp_C = "watertemp_C_U20", var_conduct_uScm = "conduct_uScm_U24",
                             var_spc_uScm = "spc_uScm_U24", var_airpress_kPa = "airpress_kPa_U20", var_airtemp_C = "airtemp_C_U20") {

  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  # ensure the dataset is for a single station only
  if (length(unique(input_data$site_station_code)) != 1) {
    stop("Multiple site_station_code values detected in the data. Ensure the dataset is for one site only.")
  } else {
    site_station_code <- unique(input_data$site_station_code)
  }

  if(logger_type == "dissolvedoxygen_U26"){
    logger_header <- "DO"

    # correct default header for water temp to match with DO logger
    if(var_watertemp_C=="watertemp_C_U20"){
      var_watertemp_C <- "watertemp_C_U26"
    }

    output_data <- input_data %>% select(c(site_station_code,
                                          timestamp,
                                          all_of(var_do_mgl),
                                          all_of(paste0(var_do_mgl, "_adj")),
                                          all_of(var_do_percsat),
                                          all_of(paste0(var_do_percsat, "_adj")),
                                          all_of(var_watertemp_C),
                                          all_of(paste0(var_watertemp_C, "_adj")),
                                          do_qaqc_code,
                                          do_qaqc_adj,
                                          do_qaqc_note))


  }
  if(logger_type == "baro_U20"){
    logger_header <- "BARO"

    output_data <- input_data %>% select(c(site_station_code,
                                          timestamp,
                                          all_of(var_airpress_kPa),
                                          all_of(paste0(var_airpress_kPa, "_adj")),
                                          all_of(var_airtemp_C),
                                          all_of(paste0(var_airtemp_C, "_adj")),
                                          baro_qaqc_code,
                                          baro_qaqc_adj,
                                          baro_qaqc_note))


  }
  if(logger_type == "waterlevel_U20"){
    logger_header <- "WL"

    output_data <- input_data %>% select(c(site_station_code,
                                          timestamp,
                                          all_of(var_waterlevel_m),
                                          all_of(paste0(var_waterlevel_m, "_adj")),
                                          all_of(var_watertemp_C),
                                          all_of(paste0(var_watertemp_C, "_adj")),
                                          waterlevel_reference,
                                          wl_qaqc_code,
                                          wl_qaqc_adj,
                                          wl_qaqc_note))
  }

  if(logger_type == "conductivity_U24"){
    logger_header <- "CO"

    # correct default header for water temp to match with conductivity logger
    if(var_watertemp_C=="watertemp_C_U20"){
      var_watertemp_C <- "watertemp_C_U24"
    }

    base_cols <- c("site_station_code", "timestamp")
    qaqc_cols <- c("co_qaqc_code", "co_qaqc_adj", "co_qaqc_note")

    value_cols <- c(intersect(var_spc_uScm, names(input_data)),
                    intersect(var_conduct_uScm, names(input_data)),                     # raw conductivity
                    intersect(paste0(var_conduct_uScm, "_adj"), names(input_data)),     # adjusted conductivity
                    intersect(paste0(var_conduct_uScm, "_adj_dc"), names(input_data)),  # drift-corrected conductivity
                    intersect(var_watertemp_C, names(input_data)),                      # raw water temp
                    intersect(paste0(var_watertemp_C, "_adj"), names(input_data)),      # adjusted water temp
                    intersect(paste0(var_watertemp_C, "_adj_dc"), names(input_data))    # drift-corrected water temp
                    )

    cols_to_select <- c(base_cols, value_cols, qaqc_cols)

    output_data <- input_data %>% select(all_of(cols_to_select))
  }


  if(logger_type == "watertemp_TidbiT"){
    logger_header <- "WT"

    if(var_watertemp_C=="watertemp_C_U20"){
      var_watertemp_C <- "watertemp_C_TidbiT"
    }

    output_data <- input_data %>% select(c(site_station_code,
                                          timestamp,
                                          all_of(var_watertemp_C),
                                          all_of(paste0(var_watertemp_C, "_adj")),
                                          wt_qaqc_code,
                                          wt_qaqc_adj,
                                          wt_qaqc_note))

  }

  if(logger_type == "airtemp_TidbiT"){
    logger_header <- "AT"

    if(var_airtemp_C=="airtemp_C_U20"){
      var_airtemp_C <- "airtemp_C_TidbiT"
    }

    output_data <- input_data %>% select(c(site_station_code,
                                          timestamp,
                                          all_of(var_airtemp_C),
                                          all_of(paste0(var_airtemp_C, "_adj")),
                                          at_qaqc_code,
                                          at_qaqc_adj,
                                          at_qaqc_note))

  }

  #### EXPORT CSV BY LOGGER TYPE LOCATION AND YEAR
  # some stations may have multiple logger files, one from a download in spring and another for a download in fall
  # this loop will split data by type, location and year for ease of access and processing


  for(i in 1:length(unique(output_data$site_station_code))) {

    site_i <- unique(output_data$site_station_code)[i]
    site_dat <- output_data %>% filter(site_station_code==site_i)

    # split by year
    years_i <- unique(lubridate::year(site_dat$timestamp))

    for(j in 1:length(years_i)){
      year_j <- years_i[j]
      site_year_j <- site_dat %>% filter(lubridate::year(timestamp)==year_j)

      #name by timestamp range
      start_j <- as.character(min(lubridate::date(site_year_j$timestamp)))
      start_j <- gsub("\\D", "", start_j)
      end_j <- as.character(max(lubridate::date(site_year_j$timestamp)))
      end_j <- gsub("\\D", "", end_j)

      # search for year folder, if does not exist, write new one
      # if(!dir.exists(paste0(path_to_output_folder, year_j))){dir.create(paste0(path_to_output_folder, year_j))}
      if(!dir.exists(paste0(path_to_output_folder, year_j))){dir.create(paste0(path_to_output_folder, year_j))}
      if(!dir.exists(paste0(path_to_output_folder, year_j,"/clean/"))){dir.create(paste0(path_to_output_folder, year_j,"/clean/"))}

      # correct missing midnight timestamps
      site_year_j$timestamp <- as.character(format(site_year_j$timestamp))

      # remove data flags

      # site_year_j$wl_qaqc_code <- gsub("FLAG_ICE", NA, site_year_j$wl_qaqc_code)
      # site_year_j$wl_qaqc_code <- gsub("FLAG_DISTURBANCE", NA, site_year_j$wl_qaqc_code)
      # site_year_j$wl_qaqc_code <- gsub("FLAG_DRY", NA, site_year_j$wl_qaqc_code)


      # write csv for each year and site_station_code
      write.csv(site_year_j, paste0(path_to_output_folder, year_j,"/clean/", site_i,"_",logger_header ,"_", start_j,"_", end_j, "_v1.0", ".csv"), row.names = FALSE)
      print(paste("Clean data (v1.0) from",site_i,"added to", year_j, "folder"))

    } #end of year level loop

  } # end of site level loop

} # end of function
