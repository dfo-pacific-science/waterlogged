## Purpose of script: Bind and QAQC hobo files for streams in 2024
##
## Author: Daniella LoScerbo  
##
## Date Created: 2024-12-24
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
source("functions/HoboDataProcessor/hobo_qaqc_functions.R")
source("functions/HoboDataProcessor/convert_do_mgl_percsat.R")
source("functions/HoboDataProcessor/convert_waterlevel_kPa_m.R")

source("C:/Users/loscerbod/OneDrive - DFO-MPO/R/templates/NTSERP_palette.R")
source("C:/Users/loscerbod/OneDrive - DFO-MPO/R/templates/ggplot_theme.R")

# STEP 1. Get data from HOBOWARE to be processed ####

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

# get cleaned data for 2023 AND 2024
baro_dat2 <- get_logger_data(path_to_data = "data/baro/processed/2024/", logger_type = "baro_U20", data_processing = "processed_bound")
baro_dat3 <- get_logger_data(path_to_data = "data/baro/processed/2023/", logger_type = "baro_U20", data_processing = "processed_bound")
baro_dat <- rbind(baro_dat2, baro_dat3)
rm(baro_dat2, baro_dat3)
baro_dat <- baro_dat[order(baro_dat$site_station_code, baro_dat$timestamp ), ]


# STEP 5. WL: Bind and trim WL data ####
wl_dat <- bind_hobo_files(path_to_raw_folder = "data/water-level/ST/raw/2024/", 
                          path_to_output_folder = "data/water-level/ST/processed/", 
                          metadata = metadat_in,
                          logger_type = "waterlevel_U20")

wl_dat[3] # visual check that data for each site and desired time range is there 

# list of raw files, variable and units
summ_files <- data.frame(wl_dat[2]) # check that all files are of the same variable type and in the same 
# units; if not, correct this in HOBOWARE and re-export to csv
head(summ_files)

# the goods
wl_dat <- data.frame(wl_dat[1]) # compiled and trimmed data for all loggers in raw folder

# logger error at AVOL_ST_30 in May
# round to nearest hour
fix <- wl_dat %>% filter(site_station_code=="AVOL_ST_30")
fix$timestamp <- round_date(fix$timestamp, unit = "hour")

wl_dat <-  wl_dat %>% filter(site_station_code!="AVOL_ST_30")
wl_dat <- rbind(wl_dat, fix)
wl_dat <- wl_dat[order(wl_dat$site_station_code, wl_dat$timestamp ), ]

# STEP 6: WL: bind nearest baro to WL dat ####
# REQUIRES: metadata as site_station_code, logger_type, latitude, longitude
# Links loggers to nearest logger_type==baro_U20
# Applies a barometric pressure correction when multiple baro stations are used per U26/U20 logger
site_coords <- read.csv("data/site-attribute/NT_gazette_wtshdcode.csv") 

metadat_barolink <- left_join(metadat_in, site_coords, by = "site_station_code")
metadat_barolink <- metadat_barolink %>%
  mutate(logger_type = case_when(
    metric == "barometric" ~ "baro_U20",
    metric == "waterlevel" ~ "waterlevel_U20",
    metric == "dissolvedoxygen" ~ "dissolvedoxygen_U26",
    metric == "watertemp" ~ "watertemp_TidbiT",
    metric == "airtemp" ~ "airtemp_TidbiT",
    metric == "conductivity" ~ "conductivity_U24",
    metric == "light" ~ "light_Pendant" )) %>%
  select(site_station_code, logger_type, latitude, longitude) %>% unique()

## add nearest baro
wl_baro_dat <- add_nearest_baro(input_data = wl_dat, 
                                input_logger_type = "waterlevel_U20",
                                path_to_output_folder = "data/water-level/ST/processed/",
                                baro_data = baro_dat,
                                metadata = metadat_barolink)

#wl_baro_dat <- get_logger_data(path_to_data = "data/water-level/WE/processed/2024/", logger_type = "waterlevel_U20", data_processing = "processed_withbaro")
#wl_baro_dat2 <- get_logger_data(path_to_data = "data/water-level/WE/processed/2023/", logger_type = "waterlevel_U20", data_processing = "processed_withbaro")
#wl_baro_dat <- rbind(wl_baro_dat, wl_baro_dat2)
#rm(wl_baro_dat2)
#wl_baro_dat$timestamp <- as.POSIXct(wl_baro_dat$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")
#wl_baro_dat <- wl_baro_dat[order(wl_baro_dat$site_station_code, wl_baro_dat$timestamp ), ]

# STEP 7. WL: Apply barometric compensation & QAQC WL data ####
# converts kPa to m 
# this step must be done site by site, as it requires consideration of stage/depth measurements at each site
# Requires metadata of reference water levels as stage or depth and timestamps
manual_dat <- read.csv("data/site-attribute/NT_manual_waterlevel_discharge_20241028.csv")
manual_dat$stage_timestamp <- as.POSIXct(manual_dat$stage_timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")
wl_sites <- unique(wl_baro_dat$site_station_code)

this_site <- wl_sites[1]
### ALBR ####
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 3)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# 3/4 points align, take this measurement
# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
# small period during freshet where logger reading below -1
# looks good otherwise

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[2]
### AVOL ####
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# this site's logger was dry in oct 2024, so much of this data is no good
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
# ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-16 08:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-19 13:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

#dry spots
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat, 
                                select_station = this_site,
                                timestamp_start =as.POSIXct("2024-04-22 09:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-04-24 09:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                reason_to_adjust = "dry",
                                logger_to_adjust = "waterlevel_U20")
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat, 
                                select_station = this_site,
                                timestamp_start =as.POSIXct("2024-05-04 21:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-05-11 04:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                reason_to_adjust = "dry",
                                logger_to_adjust = "waterlevel_U20")
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat, 
                                select_station = this_site,
                                timestamp_start =as.POSIXct("2024-07-24 06:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-10-11 17:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                reason_to_adjust = "dry",
                                logger_to_adjust = "waterlevel_U20")

site_wl_dat <- adjust_waterlevel_cablebreak(waterlevel_data = site_wl_dat, select_station = this_site,
                                            timestamp_start =as.POSIXct("2023-10-25 08:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                            timestamp_end = as.POSIXct("2023-10-25 10:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"))

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)
#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[3]
### CANV ####
# enter stage data manually
manual_dat2 <- data.frame(site_station_code = "CANV_ST_30", stage_timestamp = as.POSIXct("2024-10-09 17:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), stage_m = 0.465)

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat2,
                                    select_measurement = 1)

# data missing between 2023-10 and 2024-05??
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[4]
### CEDA ####
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 4)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# best alignment

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-10-27 03:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-11-03 17:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-11-28 19:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-12-27 10:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-07 07:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-11 13:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")


this_site <- wl_sites[5]
### CHAP ####
# enter stage data manually
manual_dat2 <- data.frame(site_station_code = "CHAP_ST_30", stage_timestamp = as.POSIXct("2024-10-10 11:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), stage_m = 0.410)

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat2,
                                    select_measurement = 1)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# best alignment

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-10-25 02:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-11-01 11:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-12-23 09:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-08 01:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[6]
### CHAP ####
# enter stage data manually
manual_dat2 <- data.frame(site_station_code = "CHAP_ST_30", stage_timestamp = as.POSIXct("2024-10-10 11:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), stage_m = 0.410)

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# best alignment

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-10 10:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-18 16:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-14 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-19 12:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-03-04 02:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-08 13:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[7]
### COOK ####
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-12-19 11:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-10 04:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[8]
### DARL ####

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-10 20:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-16 19:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-14 03:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-17 15:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[9]
### DARL ####
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-10-24 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-28 04:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)
# super flashy in the fall
# keep all, no reason to remove
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[10]
### DUNN ####
#manual
manual_dat2 <- data.frame(site_station_code = "DUNN_ST_30", stage_timestamp = as.POSIXct("2024-10-15 09:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), stage_m = 0.387)

#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat2,
                                    select_measurement = 1)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# no changes
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[11]
### EBAR ####
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 4)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# something is wrong with this logger?? Negative values, and no stage measurements align

this_site <- wl_sites[12]
### FENN ####
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-27 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-09 10:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[13]
### FISH ####
#manual
manual_dat2 <- data.frame(site_station_code = "FISH_ST_20", stage_timestamp = as.POSIXct("2024-10-15 16:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), stage_m = 0.0)

#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat2,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# logger is always dry
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2023-10-19 12:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-10-15 16:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       input_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "dry",
                                      logger_to_adjust = "waterlevel_U20")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[14]
### HAGG ####
# missing DL file 2023-10 to 2024-05
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[15]
### JAMI ####
# logger stolen over summer 2024
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-08 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-26 18:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-13 17:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-21 16:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-25 14:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-09 17:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-08 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-26 18:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[16]
### LIND ####
# depth not stage used
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2,
                                    reference_type = "depth")
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# use of first measurement results in negtive waterlevel values

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-08 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-19 16:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-26 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-29 10:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-03-03 06:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-08 20:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[17]
### LOUI ####
# depth not stage used
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

this_site <- wl_sites[18]
### MANN ####
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 4)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
#ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-06 03:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-23 08:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-29 16:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-09 18:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

### PAUL ####
this_site <- wl_sites[19]
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-10 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-21 01:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-14 04:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-18 15:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-27 02:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-02-27 21:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-03-04 05:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-07 13:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

### PETE ####
this_site <- wl_sites[20]
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
#ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-08 07:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-18 19:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-14 05:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-08 19:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

### REGC ####
this_site <- wl_sites[21]
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-08 19:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-21 09:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-02-13 21:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-09 23:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

### REGC ####
this_site <- wl_sites[22]
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

#ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-07 21:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-14 11:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")


### TUMT ####
this_site <- wl_sites[23]
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
#ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-12-28 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-10 00:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                select_station = this_site, 
                var_airtemp_C = "airtemp_C_U20",
                plot_only = TRUE)

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")

### WIRE ####
#super flashy
this_site <- wl_sites[24]
#convert wl
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)
site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# run qaqc flags
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, 
                               select_station = this_site, 
                               var_airtemp_C = "airtemp_C_U20")
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/ST/clean/", 
                 logger_type = "waterlevel_U20")
