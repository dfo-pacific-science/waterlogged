## Purpose of script: Bind and QAQC hobo files for DO in wetlands in 2024
##
## Author: Daniella LoScerbo  
##
## Date Created: 2024-12-23
##
## Email: daniella.loscerbo@dfo-mpo.gc.ca
##
## -----------------------------

# SETUP ####

library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(readr)
library(plotly)
library(purrr)
#library(HoboDataProcessor)
source("functions/HoboDataProcessor/bind_hobo_files.R")
source("functions/HoboDataProcessor/add_nearest_baro.R")
source("functions/HoboDataProcessor/convert_do_mgl_percsat.R")

source("functions/HoboDataProcessor/hobo_qaqc_functions.R")
source("C:/Users/loscerbod/Documents/R/templates/NTSERP_palette.R")
source("C:/Users/loscerbod/Documents/R/templates/ggplot_theme.R")

# STEP 1. Get data to be processed ####

# Data is currently stored in remote Public drive - slow to work with in R
# "Z: Public\North Thompson Work\Data\dissolved-oxygen\WE\raw\2024\"
# Copy raw hobo data folder onto local computer for processing
# Bulk export of csvs from HOBOWARE
# HOBOWARE PRO > Tools > Bulk Export > Select folder containing hobo files

# STEP 2. Import metadata of logger installation and retrieval ####

## metadata of serial numbers (sn), logger location (site_station_code), timestamp_deploy, timestamp_remove, status (logging or removed)
metadat_in <- read_csv("data/site-attribute/NorthThompson_logger_deployments_20241105.csv") %>% select(site_station_code, sn, metric, timestamp_deploy, timestamp_remove, status)

## timestamps must be formatted as such
# we report all timestamps as PDT for ease of data processing - to do this, 
#   trick R to think data is in UTC so it doesnt try to account for DST and 
#   duplicate or drop timestamps
metadat_in$timestamp_deploy <- as.POSIXct(metadat_in$timestamp_deploy, format = "%m/%d/%Y %H:%M", tz = "UTC")
metadat_in$timestamp_remove <- as.POSIXct(metadat_in$timestamp_remove, format = "%m/%d/%Y %H:%M", tz = "UTC")

#check metadata coding
str(metadat_in)

# STEP 3.Bind and trim DO  data ####
do_dat <- bind_hobo_files(path_to_raw_folder = "data/dissolved-oxygen/WE/raw/2024/", 
                          path_to_output_folder = "data/dissolved-oxygen/WE/processed/", 
                          metadata = metadat_in,
                          logger_type = "dissolvedoxygen_U26")

#summary graph
summ_plot <- do_dat[3] # visual check that data for each site and desired time range is there 
# list of raw files, variable and units
summ_files <- data.frame(do_dat[2]) # check that all files are of the same variable type and in the same 
# units; if not, correct this in HOBOWARE and re-export to csv
# the goods
do_dat <- data.frame(do_dat[1]) # compiled and trimmed data for all loggers in raw folder

# you can go straight ahead and work with this or if you come back later 
# you can call in the processed files by data year with another function

# # NOTE: that if you had raw files from previous years (ie. 2023 data) in the input raw folder (ie. 2024 data), they will be 
# # output into the correct data year for processing (ie. 2023/processed not 2024/processed)
# 
#do_dat <- get_logger_data(path_to_data = "data/dissolved-oxygen/WE/processed/2024/", logger_type = "dissolvedoxygen_U26", data_processing = "processed_bound")
#do_dat2 <- get_logger_data(path_to_data = "data/dissolved-oxygen/WE/processed/2023/", logger_type = "dissolvedoxygen_U26", data_processing = "processed_bound")
# # see here how do_dat is larger than do_dat2 because when you called get_logger_data for do_dat2, you only specified year 2024
# # you can get the other files from 2023 by calling again
#do_dat3 <- rbind(do_dat, do_dat2)
#do_dat <- do_dat3
#rm(do_dat2, do_dat3)
# # now we have all the unprocessed data to work on

# STEP 4: add nearest baro to DO dat ####
do_baro_dat <- add_nearest_baro(input_data = do_dat, 
                                input_logger_type = "dissolvedoxygen_U26",
                                path_to_output_folder = "data/dissolved-oxygen/WE/processed/",
                                baro_data = baro_dat,
                                metadata = metadat_barolink)

# # get DO baro data
do_baro_dat <- get_logger_data(path_to_data = "data/dissolved-oxygen/WE/processed/2024/", 
                               logger_type = "dissolvedoxygen_U26", 
                               data_processing = "processed_withbaro")
summary(do_baro_dat)
str(do_baro_dat)

# STEP 5 Apply barometric compensation to DO data ####
# converts mg/L to percent air saturation 
do_baro_dat <- convert_do_mgl_percsat(input_data = do_baro_dat)
# do is now ready for QAQC 

# OPTIONAL STEP: Add in clean WL data to help with QAQC of DO
wl_dat <- get_logger_data(path_to_data = "data/water-level/WE/clean/2024/", 
                          logger_type = "waterlevel_U20")
summary(wl_dat)
str(wl_dat)

## join wl to do
### rename overlapping columns
wl_dat <- rename_with(wl_dat, ~paste0("wl_", .x, recycle0 = TRUE),
            starts_with("qaqc"))
### create new joining column using first 9 characters of site_station_code (to ignore floating logger code of $$$$_$$_#1)
wl_dat$link <- substr(wl_dat$site_station_code, start = 1, stop = 9)
wl_dat <- wl_dat[,-c(1)]
do_baro_dat$link <- substr(do_baro_dat$site_station_code, start = 1, stop = 9)
do_baro_dat <- rename_with(do_baro_dat, ~paste0("do_", .x, recycle0 = TRUE),
                    starts_with("qaqc"))
do_wl_dat <- left_join(do_baro_dat, wl_dat, by = c("link", "timestamp"))
do_wl_dat <- subset(do_wl_dat, select=-c(link))

do_wl_dat <- rename(do_wl_dat, "do_mgl_U26" = "do_mgl",
                         "do_percsat_U26" = "do_percsat",
                         "watertemp_C_U26" = "watertemp_C")

summary(do_wl_dat)
str(do_wl_dat)

# STEP 6: QAQC DO ####
do_sites <- unique(do_wl_dat$site_station_code)

this_site <- as.character(do_sites[1])
## BARR_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "none", var_watertemp_C_2 = "none")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[3])
## COOK_WE_40 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[4])
## COOK_WE_41 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

#dry periods
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-06-14 17:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-06-27 14:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-07-01 10:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-07-01 17:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-07-08 09:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-07-12 18:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-07-24 12:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-08-10 16:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-09-27 09:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-10-09 10:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "none", var_watertemp_C_2 = "none")

# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[5])
## COOK_WE_50 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "none", var_watertemp_C_2 = "none")
site_do[2]
site_do <- data.frame(site_do[1])

# remove end, looks like logger was removed
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-10-08 11:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-10-08 20:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "disturbance" )
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "none", var_watertemp_C_2 = "none")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[6])
## COOK_WE_60 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "none", var_watertemp_C_2 = "none")
site_do[2]
site_do <- data.frame(site_do[1])

# crazy swings but looks ok
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[7])
## GOOS_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

# flag dry due to differences in water temp between level logger and DO - check if this logger was placed higher or lower than level logger
# dry period
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-07-14 08:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-07-27 06:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# crazy swings but looks ok
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[8])
## GOOS_WE_31 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[9])
## GOOS_WE_41 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "none", var_watertemp_C_2 = "none")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-07-27 14:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-08-04 21:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-08-22 13:00:00", tz = "UTC"),
                            timestamp_end = as.POSIXct("2024-08-23 20:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj")

# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[10])
## LITT_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = site_do$timestamp[1],
                            timestamp_end = as.POSIXct("2024-05-06 15:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "ice" )

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-09-09 06:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-25 09:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[11])
## LITT_WE_31 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = site_do$timestamp[1],
                            timestamp_end = as.POSIXct("2024-05-05 09:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")

# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[12])
## LO09_WE_20 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])


site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-09-12 12:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-25 18:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")

# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[13])
## LO09_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-09-24 23:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-26 03:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[14])
## LO09_WE_31 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-09-06 16:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-26 04:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry" )
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[15])
## LO09_WE_40 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

#cut off first hour, disturbance
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = site_do$timestamp[1], 
                            timestamp_end = site_do$timestamp[2], 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "disturbance" )
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")

# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[16])
## LO10_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

#dry period
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-09-12 11:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-28 05:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry",
                            apply_to_all_data = FALSE)

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[17])
## LO10_WE_31 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])


#dry period
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-08-05 13:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-28 05:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry",
                            apply_to_all_data = FALSE)

dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[18])
## LO12_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

#dry period
site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = site_do$timestamp[1], 
                            timestamp_end = as.POSIXct("2024-04-21 22:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "ice",
                            apply_to_all_data = FALSE)
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[19])
## LO12_WE_31 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = site_do$timestamp[1], 
                            timestamp_end = as.POSIXct("2024-05-15 19:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry",
                            apply_to_all_data = FALSE)
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[20])
## MILE_WE_40 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-08-05 04:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-12 14:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry",
                            apply_to_all_data = FALSE)
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[21])
## MILE_WE_41 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-07-29 06:00:00", tz = "UTC"), 
                            timestamp_end = as.POSIXct("2024-09-12 14:00:00", tz = "UTC"), 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "dry",
                            apply_to_all_data = FALSE)
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[22])
## NWFI_WE_40 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])

site_do <- adjust_logger_NA(input_data = site_do, select_station = this_site, 
                            timestamp_start = as.POSIXct("2024-10-08 14:00:00", tz = "UTC"), 
                            timestamp_end = site_do$timestamp[length(site_do$timestamp)], 
                            logger_to_adjust = "dissolvedoxygen_U26",
                            var_watertemp_C = "watertemp_C_U26",
                            reason_to_adjust = "disturbance",
                            apply_to_all_data = FALSE)
dissox_qaqc(plot_only = TRUE, dissox_data = site_do, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


## NOTE #### 
## STIL WE 40 QAQC notes lost due to comp crash - cleaned filed available

this_site <- as.character(do_sites[26])
## STIL_WE_41 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[27])
## STIL_WE_50 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[28])
## TUMT_WE_20 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[29])
## TUMT_WE_20 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[30])
## TUMT_WE_40 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

this_site <- as.character(do_sites[31])
## TUMT_WE_50 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[32])
## WIRE_WE_30 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")


this_site <- as.character(do_sites[33])
## WIRE_WE_31 ####
site_do <- dissox_qaqc(dissox_data = do_wl_dat, select_station = this_site,  var_airtemp_C = "airtemp_C_U20_adj", var_waterlevel_m = "waterlevel_m_U20_adj", var_watertemp_C_2 = "watertemp_C_U20_adj")
site_do[2]
site_do <- data.frame(site_do[1])
# looks good
write_clean_data(site_do, path_to_output_folder = "data/dissolved-oxygen/WE/clean/", logger_type = "dissolvedoxygen_U26")

# SUMMARY ####