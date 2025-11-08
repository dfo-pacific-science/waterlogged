#' Barometric pressure compensation for dissolved oxygen
#'
#' Add nearest barometric logger to existing dataset of dissolved oxygen and/or water level data, based on metadata file
#' @param input_data The dataset containing raw dissolved oxygen in mg/L or waterlevel in kPa
#' @param input_logger_type Recognised logger_types are: dissolvedoxygen_U26, waterlevel_U20
#' @param path_to_output_folder The desired folder for output of bound dissolved oxygen and barometric pressure per station
#' @param metadata File containing site_station_code, latitude, and longitude
#' @param baro_data File containing barometric pressure in kPa for barometric stations as site_code that match with those in the metadata_file under nearest_baro, secondary_baro, tertiary_baro
#' @param var_airtemp_C The variable for air temperature in degrees Celsius from barometric logger
#' @param var_airpress_kPa The variable for barometric pressure in kPa
#' @param  timestamp_format Formatting of timestamp column of input data, following POSTIX convention. Default is output of HOBO files to excel "%m/%d/%Y %H:%M"
#' @param  timestamp_timezone Timezone of timestamp column of input data, following POSTIX convention. Default is UTC to force ignore daylight savings time.
#' @return dataset with barometric data and adjustments added as new columns, as well as a saved csv
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2

add_nearest_baro <- function(input_data, input_logger_type, path_to_output_folder, baro_data, timestamp_format = "%Y-%m-%d %H:%M:%S", timestamp_timezone = "UTC", var_airpress_kPa = "airpress_kPa_U20", var_airtemp_C = "airtemp_C_U20", metadata){

  ## SETUP ####
  # format timestamp
  # NOTE: all times are in PDT (setting timezone to UTC tricks R to ignore daylight savings time)
  input_data$timestamp <-as.POSIXct(input_data$timestamp, format = timestamp_format, tz = timestamp_timezone)
  baro_data$timestamp <-as.POSIXct(baro_data$timestamp, format = timestamp_format, tz = timestamp_timezone)

  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  # user input baro data names to local names
  names(baro_data)[names(baro_data) == var_airpress_kPa ] <- "airpress_kPa"
  names(baro_data)[names(baro_data) == var_airtemp_C ] <- "airtemp_C"
  baro_data$site_station_code <- as.character(baro_data$site_station_code)


  ## Bind Data ####
  ## get baro list
  baro_link <- metadata %>% drop_na(site_station_code, latitude, longitude)

   output_data <- input_data
  # make empty columns in input data to fill with baro data
  output_data$baro_code <- NA
  output_data$airpress_kPa <- NA
  output_data$airtemp_C <- NA
  output_data$site_station_code <- as.character(output_data$site_station_code)

  ## create empty datasets for loop
  do_baro_compiled <- data.frame()

  ## start site level loop
  for(i in 1:length(unique(output_data$site_station_code))){

    site_i <- unique(output_data$site_station_code)[i]
    dat_i <- filter(output_data, site_station_code==site_i)

    # order by timestamp
    dat_i <- dat_i[order(dat_i$timestamp),]

    coords_i <- filter(metadata, site_station_code == site_i)

    # make error message for missing metadata
    if(length(coords_i$site_station_code)==0){
      print(paste("ERROR: latitude and longitude not found for", site_i, input_logger_type, ". Please review metadata."))
    } else{
      coords_i <- coords_i[,c("longitude","latitude")]
    }

    # calculate shortest difference between site_i and each baro
    # new baro_link df for each i
    baro_link_i <- baro_link %>% select(site_station_code, longitude, latitude)
    baro_link_i<- baro_link_i  %>% mutate(
      dist = geosphere::distHaversine(coords_i, baro_link_i[,2:3])
    )
    # order by increasing distance
    baro_link_i <- baro_link_i[order(baro_link_i$dist),]
    # make list of nearest to farthest baro
    barolist_i <- as.vector(baro_link_i$site_station_code)

    # go through each baro in order of closest
    for(j in 1:length(barolist_i)){
      baro_j <- barolist_i[j]
      baro_dat_j <- filter(baro_data, site_station_code == baro_j)
      baro_dat_j <- baro_dat_j[order(baro_dat_j$timestamp),]

         # if baro dat timestamps overlap with site data timestamps, use this baro
        if(any(baro_dat_j$timestamp %in% dat_i$timestamp)){

          # go through site timestamps
          for(k in 1:length(dat_i$timestamp)){

            # do not overwrite data
            if(is.na(dat_i$baro_code[k])){
              ts <- dat_i$timestamp[k]
              try(dat_i$airpress_kPa[k] <- baro_dat_j$airpress_kPa[which(baro_dat_j$timestamp==ts)], silent = TRUE)
              try(dat_i$airtemp_C[k] <- baro_dat_j$airtemp_C[which(baro_dat_j$timestamp==ts)], silent = TRUE)
              try(dat_i$baro_code[k] <- baro_dat_j$site_station_code[which(baro_dat_j$timestamp==ts)], silent = TRUE)
            }

          } # end of timestamp loop k

        }  # end of if matching timestamps

    } # end of baro list loop j

    if(any(is.na(dat_i$baro_code))){
      missing_baro_ts <- min(dat_i$timestamp[is.na(dat_i$baro_code)])

      dat_i <- dat_i[!is.na(dat_i$baro_code),]

      print(paste("WARNING: Baro data missing for timestamps equal to and greater than", missing_baro_ts, ". Dropped data without matching baro data."))
    }

    # create columns
    dat_i$airpress_kPa_adj <- dat_i$airpress_kPa
    dat_i$baro_qaqc_adj <- NA #what adjustment was made?
    dat_i$baro_qaqc_code <- NA #why was an adjustment made?
    dat_i$baro_qaqc_note <- NA #why was an adjustment made?

    # if multiple baro loggers used, need to apply adjustment
    if(length(unique(dat_i$baro_code))>1 ){
      dat_i$airpress_kPa_adj <- NA

      # fill in adj values using CLOSEST/MOST OCCURRING baro as reference
      baros_used_i <- as.data.frame(unique(dat_i$baro_code))
      baros_used_i$hours <- NA

      for(l in 1:length(baros_used_i$hours)){
        baros_used_i$hours[l] <- length(dat_i$timestamp[dat_i$baro_code == baros_used_i$`unique(dat_i$baro_code)`[l]])
      }

      ref_baro_i <-  baros_used_i$`unique(dat_i$baro_code)`[which(baros_used_i$hours==max(baros_used_i$hours))]
      ref_baro_dat_i <- baro_data[baro_data$site_station_code==ref_baro_i,]

      for(l in 1:length(dat_i$timestamp)){
        if(dat_i$baro_code[l]==ref_baro_i){
          dat_i$airpress_kPa_adj[l] <- dat_i$airpress_kPa[l]
        }
      }
      # if airpress_adj = NA then it is not the reference baro and needs to be adjusted
      # get alt baro code and timestamp
      # find the first match in timestamp between alt baro and ref baro
      # calc diff in pressure
      # apply diff pressure to all alt baro values
      # do not overwrite, move to next press_adj NA
      for(l in 1:length(dat_i$timestamp)){
        if(is.na(dat_i$airpress_kPa_adj[l])){
          alt_baro <- dat_i$baro_code[l]
          alt_baro_dat <- baro_data[baro_data$site_station_code==alt_baro,]
          match_ts <- min(subset(ref_baro_dat_i$timestamp, ref_baro_dat_i$timestamp %in% alt_baro_dat$timestamp))
          ref_press <- ref_baro_dat_i$airpress_kPa[ref_baro_dat_i$timestamp==match_ts]
          alt_press <- alt_baro_dat$airpress_kPa[alt_baro_dat$timestamp==match_ts]
          press_diff <- ref_press-alt_press # difference must be added to press_adj
            for(m in l:length(dat_i$timestamp)){
              if(dat_i$baro_code[m]==alt_baro){
                dat_i$airpress_kPa_adj[m] <- dat_i$airpress_kPa[m]+press_diff
                dat_i$baro_qaqc_adj[m] <- paste("added", round(press_diff,3))
                dat_i$baro_qaqc_note[m] <- "added difference in pressure prior to change"
                dat_i$baro_qaqc_code[m] <- "ALT_BARO_CORR"
              }
            }
        }
      } # end of alt baro adjustments

  } # end of multi baro loops
    # check that there are no breaks/jumps in pressure between loggers
    dev.new()

    print(ggplot(data = dat_i, aes(x = timestamp))+
            geom_line(aes(y = airpress_kPa_adj), color = "black")+
            geom_point(aes(y = airpress_kPa_adj, color = baro_code))+
            labs(title = site_i)+
            theme_classic())

    # create headers for saved file
     if(input_logger_type == "dissolvedoxygen_U26"){
      logger_header <- "DO"
    }

    if(input_logger_type == "waterlevel_U20"){
      logger_header <- "WL"
    }

    # rename variables to user input
    names(dat_i)[names(dat_i) ==  "airpress_kPa"] <- var_airpress_kPa
    names(dat_i)[names(dat_i) ==  "airpress_kPa_adj"] <-paste0(var_airpress_kPa, "_adj")
    names(dat_i)[names(dat_i) ==  "airtemp_C"] <- var_airtemp_C

      # split by year
      years_i <- unique(lubridate::year(dat_i$timestamp))

    for(n in 1:length(years_i)){
      year_n <- years_i[n]
      site_year_n <- dat_i %>% filter(lubridate::year(timestamp)==year_n)

      #name by timestamp range
      start_n <- as.character(min(lubridate::date(site_year_n$timestamp)))
      start_n <- gsub("\\D", "", start_n)
      end_n <- as.character(max(lubridate::date(site_year_n$timestamp)))
      end_n <- gsub("\\D", "", end_n)

      # search for year folder, if does not exist, write new one
      # if(!dir.exists(paste0(path_to_output_folder, year_j))){dir.create(paste0(path_to_output_folder, year_j))}
      if(!dir.exists(paste0(path_to_output_folder, year_n))){dir.create(paste0(path_to_output_folder, year_n))}
      if(!dir.exists(paste0(path_to_output_folder, year_n,"/processed/"))){dir.create(paste0(path_to_output_folder, year_n,"/processed/"))}

      # correct missing midnight timestamps
      site_year_n$timestamp <- as.character(format(site_year_n$timestamp))

      # write csv for each year and site_station_code

      write.csv(site_year_n, paste0(path_to_output_folder, year_n,"/processed/", site_i,"_",logger_header,"_", start_n,"_", end_n, "_v0.2", ".csv"), row.names = FALSE)
      print(paste("Barometric pressure data (v0.2) from",site_i,"added to", year_n, "folder"))

    } #end of year level loop


    # add site data to compiled dataset
    do_baro_compiled <- dplyr::bind_rows(do_baro_compiled, dat_i)

    # clear site dataset to prep for next loop
    dat_i <- data.frame()


  } # end of site loop i

  return(do_baro_compiled)
} # end of function

