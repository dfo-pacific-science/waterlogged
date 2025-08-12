#' Bind raw dissolved oxygen data
#'
#' Merge dissolved oxygen data from multiple loggers by site_station_code and trim for install/removal dates
#' @param path_to_raw_folder The folder containing raw dissolved oxygen in mg/L from HOBO as csv files
#' @param path_to_output_folder The desired folder for output of bound dissolved oxygen and barometric pressure per station
#' @param logger_type Recognised logger_types are: dissolvedoxygen_U26, waterlevel_U20, baro_U20, watertemp_TidbiT, airtemp_TidbiT
#' @param  metadata File containing serial number, install date, install time and status for each site_station_code
#' @return Station specific datasets with dissolved oxygen in percent saturation, as well as a global dataset of all stations

bind_hobo_files <- function(path_to_raw_folder, path_to_output_folder, logger_type, metadata) {

  ## GET DATA FROM HOBO CSV FILES####
  file_names <- list.files(path = path_to_raw_folder, pattern = "\\.csv$") # extract file names ending in csv
  path_to_files <- paste0(path_to_raw_folder, file_names) # makes string of full file path for all csv files

  extract_data_from_file <- function(file) {
    data <- read.csv(file, header = FALSE, sep = ",", dec = ".",
                     stringsAsFactors = FALSE)
    logger_info <- as.character(data[2,3])
    dat1_type <- sub("\\,.*", "", logger_info) # extract everything before comma
    dat1_unit <- sub("\\s*\\(.*", "", logger_info) # extract everything before the left bracket
    dat1_unit <- sub(".*, ", "", dat1_unit) # extract everything after the comma
    logger_sn <- regmatches(logger_info, regexpr("\\d{8}", logger_info)) # extract 8 digit serial number

    dat2_info <- as.character(data[2,4])
    dat2_type <- sub("\\,.*", "", dat2_info) # extract everything before comma
    dat2_unit <- sub("\\s*\\(.*", "", dat2_info) # extract everything before the left bracket
    dat2_unit <- sub(".*, ", "", dat2_unit) # extract everything after the comma

    # use serial number for data link
    # add as first column
    data$V1 <- logger_sn
    data <- data[-c(1:2),] # drop header rows
    data <- data[,c(1:4)] # drop empty columns

    # drop empty rows
    data$V3 <- as.numeric(data$V3)
    data <- data[complete.cases(data), ]

    #write new headers
    colnames(data) <- c("sn", "timestamp", paste(dat1_type,dat1_unit), paste(dat2_type, dat2_unit))

    return(data)
  }

  dat1 <- map_df(path_to_files, extract_data_from_file)

  ## SUMMARIZE DATA FILES ####
  extract_metadata_from_file <- function(file) {
    data <- read.csv(file, header = FALSE, sep = ",", dec = ".",
                     stringsAsFactors = FALSE)
    file_name <- sub(".*/", "", file, perl = T) # get file name

    tz <- as.character(data[2,2]) # get timezone
    tz <- sub(".*(Date Time.*), ", "", tz)
    logger_info <- as.character(data[2,3])
    dat1_type <- sub("\\,.*", "", logger_info) # extract everything before comma
    dat1_unit <- sub("\\s*\\(.*", "", logger_info) # extract everything before the left bracket
    dat1_unit <- sub(".*, ", "", dat1_unit) # extract everything after the comma
    logger_sn <- regmatches(logger_info, regexpr("\\d{8}", logger_info)) # extract 8 digit serial number

    dat2_info <- as.character(data[2,4])
    dat2_type <- sub("\\,.*", "", dat2_info) # extract everything before comma
    dat2_unit <- sub("\\s*\\(.*", "", dat2_info) # extract everything before the left bracket
    dat2_unit <- sub(".*, ", "", dat2_unit) # extract everything after the comma

    file_summary <- data.frame(t(c(file_name, logger_sn, tz, dat1_type, dat1_unit, dat2_type, dat2_unit)))
    colnames(file_summary) <- c("file_name", "sn", "timezone", "data1_type", "data1_unit", "data2_type", "data2_unit")

    return(file_summary)

  }

  file_summary <- map_df(path_to_files, extract_metadata_from_file)

  # Error messages for file binding
  if(length(unique(file_summary$data1_type))>1){
    print("WARNING: folder contains different types of HOBO loggers.  Check that all files in input folder are of the same logger type (refer to 'file_summary').")
  }
  if(length(unique(file_summary$data1_unit))>1|length(unique(file_summary$data2_unit))>1){
    print("WARNING: some files are in different units (refer to 'file_summary').")
  }
  if(length(unique(file_summary$timezone))>1){
    print("WARNING: some files are in different timezones (refer to 'file_summary').")
  }

  ## TIMESTAMPS ####
  # format timestamp
  # NOTE: all times are in PDT (setting timezone to UTC tricks R to ignore daylight savings time)
  dat1$timestamp <-as.POSIXct(dat1$timestamp, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")

  if(any(is.na(dat1$timestamp))) {
    missing_ts <- dat1 %>% filter(is.na(timestamp))
    missing_ts_sn <- unique(missing_ts$sn)

    print(paste("WARNING: NAs produced in timestamp for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted as mm/dd/yy HH:MM:SS AM/PM (%m/%d/%y %I:%M:%S %p). Correct and try again."))
}
  # LABEL AND TRIM ####
  ## Loop to link site_station_code to logger data by serial number and trim data by install and retrieval dates in metadata

  # link sn to site_station_code in metadata
  metadat_link <- metadata %>% filter(sn %in% unique(dat1$sn))

  # make dummy columns and datasets to fill in
  dat1$site_station_code <- NA
  data_i <- data.frame()
  sites_compiled <- data.frame()

  for(i in 1:length(unique(dat1$sn))){
    logger_i <- unique(dat1$sn)[i]
    data_i <- dat1 %>% filter(sn ==logger_i)

    # get site codes for each sn
    metadat_i <- metadat_link %>% filter(sn==logger_i)

    # if no metadat for that logger produce error message
    if(length(metadat_i$sn)==0){
      print(paste(c("ERROR: Metadata missing for logger serial number:", logger_i)))
    }
    for(j in 1:length(metadat_i$sn)){
      site_j <- metadat_i$site_station_code[j]
      deploy_j <- metadat_i$timestamp_deploy[j]
      remove_j<- metadat_i$timestamp_remove[j]

      # if logger is still logging it wont have a remove date so make it today
      if(is.na(remove_j)){remove_j <- now(tzone="UTC")}

      # assign location based on matching timestamps in metadata
      for(k in 1:length(data_i$timestamp)){
        if(data_i$timestamp[k]>=deploy_j&data_i$timestamp[k]<=remove_j){
          data_i$site_station_code[k]<-site_j
        }
      } # end of k loop
    } # end of j loop

    # trim data to within install/removal at each site
    data_i <- data_i %>% drop_na(site_station_code)
    sites_compiled <- rbind(sites_compiled, data_i)

  } # end of i loop

## CLEAN UP ####

  # remove duplicate rows in case multiple downloads per logger results in overlapping dates
  sites_compiled <- sites_compiled %>%
    distinct(site_station_code, sn, timestamp, .keep_all = TRUE)

  # order by site and timestamp
  sites_compiled<- sites_compiled[order(sites_compiled$site_station_code, sites_compiled$timestamp), ]

  # change column names to remove spaces and symbols for data processing
  # if columns are in different units, then just keep original headers
  if(colnames(sites_compiled)[3]=="DO conc mg/L"){
    colnames(sites_compiled)[3]<- "do_mgl_U26"
    logger_header <- "DO"

    if(colnames(sites_compiled)[4]=="Temp °C"){
      colnames(sites_compiled)[4]<- "watertemp_C_U26"
    }
  }
  if(colnames(sites_compiled)[3]=="Abs Pres kPa" & logger_type == "baro_U20"){
    colnames(sites_compiled)[3]<- "airpress_kPa_U20"
    sites_compiled <- sites_compiled[complete.cases(sites_compiled$airpress_kPa_U20),]

    logger_header <- "BARO"
    if(colnames(sites_compiled)[4]=="Temp °C"){
      colnames(sites_compiled)[4]<- "airtemp_C_U20"
    }
  }
  if(colnames(sites_compiled)[3]=="Abs Pres kPa" & logger_type == "waterlevel_U20"){
    colnames(sites_compiled)[3]<- "waterpress_kPa_U20"
    logger_header <- "WL"

    if(colnames(sites_compiled)[4]=="Temp °C"){
      colnames(sites_compiled)[4]<- "watertemp_C_U20"
    }
  }
  if(colnames(sites_compiled)[3]=="Temp °C" & logger_type == "watertemp_TidbiT"){
    logger_header <- "WT"
    colnames(sites_compiled)[3]<- "watertemp_C_TidbiT"

  }
  if(colnames(sites_compiled)[3]=="Temp °C" & logger_type == "airtemp_TidbiT"){
    logger_header <- "AT"
    colnames(sites_compiled)[3]<- "airtemp_C_TidbiT"

  }


  #### EXPORT CSV BY LOGGER TYPE LOCATION AND YEAR
  # some stations may have multiple logger files, one from a download in spring and another for a download in fall
  # this loop will split data by type, location and year for ease of access and processing



  for(i in 1:length(unique(sites_compiled$site_station_code))) {

    site_i <- unique(sites_compiled$site_station_code)[i]
    site_dat <- sites_compiled %>% filter(site_station_code==site_i)

    # split by year
    years_i <- unique(year(site_dat$timestamp))

    for(j in 1:length(years_i)){
      year_j <- years_i[j]
      site_year_j <- site_dat %>% filter(year(timestamp)==year_j)

      #name by timestamp range
      start_j <- as.character(min(date(site_year_j$timestamp)))
      start_j <- gsub("\\D", "", start_j)
      end_j <- as.character(max(date(site_year_j$timestamp)))
      end_j <- gsub("\\D", "", end_j)

      # search for year folder, if does not exist, write new one
      # if(!dir.exists(paste0(path_to_output_folder, year_j))){dir.create(paste0(path_to_output_folder, year_j))}
        if(!dir.exists(paste0(path_to_output_folder, year_j,"/processed/"))){dir.create(paste0(path_to_output_folder, year_j,"/processed/"))}


      # write csv for each year and site_station_code

      # write_csv(site_year_j, paste0(path_to_output_folder, year_j,"/", site_i,"_",logger_header,"_", start_j,"_", end_j, "_v0.1", ".csv"))
      write_csv(site_year_j, paste0(path_to_output_folder, year_j,"/processed/", site_i,"_",logger_header,"_", start_j,"_", end_j, "_v0.1", ".csv"))

       print(paste("Data (v0.1) from",site_i,"added to", year_j, "folder"))

      } #end of year level loop

    } # end of site level loop

  print("List returned: [1] trimmed and compiled dataset of csvs in raw folder [2] file summary of csvs in raw folder [3] summary plots of data by site_station_code and timestamp")

  ## Graph Raw Data ####
  sites_compiled[,3] <- as.numeric(sites_compiled[,3])
  sites_compiled[,4] <- as.numeric(sites_compiled[,4])

 multi_plots <- ggplot(data=sites_compiled, aes(x = sites_compiled[,2], y = sites_compiled[,3])) +
                          geom_line(linewidth = 1)+
                          facet_wrap(~site_station_code, scales = "free")+
                          theme_classic()+
                          theme(axis.text.x = element_text(angle = 90))+
                          scale_x_datetime(date_labels = "%y-%m")


  return(list(sites_compiled, file_summary, multi_plots))
  } # end of function

