## Purpose of script: Bind and QAQC hobo files for wetlands in 2024
##
## Author: Daniella LoScerbo  
##
## Date Created: 2024-10-25
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

source("C:/Users/loscerbod/Documents/R/templates/NTSERP_palette.R")
source("C:/Users/loscerbod/Documents/R/templates/ggplot_theme.R")

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

# STEP 3. BARO: Read, trim, bind and sort csvs by location and year ####
# Start wth baro - you will do this for WL and DO in later steps
baro_dat <- bind_hobo_files(path_to_raw_folder = "data/baro/raw/2024/", 
                          path_to_output_folder = "data/baro/processed/", 
                          metadata = metadat_in,
                          logger_type = "baro_U20")

# alsp need to do 2023
baro_dat23 <- bind_hobo_files(path_to_raw_folder = "data/baro/raw/2023/", 
                            path_to_output_folder = "data/baro/processed/", 
                            metadata = metadat_in,
                            logger_type = "baro_U20")

#summary graph
baro_dat[3] # visual check that data for each site and desired time range is there 

# list of raw files, variable and units
summ_files <- baro_dat[2] # check that all files are of the same variable type and in the same 
                          # units; if not, correct this in HOBOWARE and re-export to csv
head(summ_files)

# the goods
baro_dat <- data.frame(baro_dat[1]) # compiled and trimmed data for all loggers in raw folder
# NOTE: STRU_WE_40 hobo data logger seemed to fail in transfer from shuttle to computer 10/09/2024 - hopefully file is still in logger, download in spring
baro_dat23 <- data.frame(baro_dat23[1]) # compiled and trimmed data for all loggers in raw folder
baro_dat <- rbind(baro_dat, baro_dat23)

# STEP 4. BARO: QAQC (optional - needs development) ####
# Baro data is used in computing water level and dissolved oxygen percent saturation so it needs to be cleaned first
# check baro dat ranges
baro_dat %>% dplyr::group_by(site_station_code) %>% dplyr::summarise(min(timestamp), max(timestamp))
# list sites
baro_sites <- unique(baro_dat$site_station_code)
site_coords <- read.csv("data/site-attribute/NT_gazette_wtshdcode.csv") 

#run for each 2024 site
### ALBR ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[1])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
# save
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )

### CANV ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[2])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )

### CEDA ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[3])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )

### HARP ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[4])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )

### LOUI ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[5], climate_station = "KAMLOOPS")
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )

### SHAN ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[6])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )
 
### STIL ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[7])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )

### STRU ####
baro_qaqc <- barometric_qaqc(input_data = baro_dat, select_station = baro_sites[8])
baro_qaqc[2]
baro_qaqc <- data.frame(baro_qaqc[1])
write_clean_data(input_data = baro_qaqc, path_to_output_folder = "data/baro/clean/", logger_type = "baro_U20" )


# get cleaned data for 2023 AND 2024
baro_dat2 <- get_logger_data(path_to_data = "data/baro/clean/2024/", logger_type = "baro_U20", data_processing = "clean")
baro_dat3 <- get_logger_data(path_to_data = "data/baro/clean/2023/", logger_type = "baro_U20", data_processing = "clean")
baro_dat <- rbind(baro_dat2, baro_dat3)
rm(baro_dat2, baro_dat3)
baro_dat <- baro_dat[order(baro_dat$site_station_code, baro_dat$timestamp ), ]


# STEP 5. WL: Bind and trim WL data ####
wl_dat <- bind_hobo_files(path_to_raw_folder = "data/water-level/WE/raw/2024/", 
                          path_to_output_folder = "data/water-level/WE/processed/", 
                          metadata = metadat_in,
                          logger_type = "waterlevel_U20")

wl_dat[3] # visual check that data for each site and desired time range is there 

# list of raw files, variable and units
summ_files <- data.frame(wl_dat[2]) # check that all files are of the same variable type and in the same 
# units; if not, correct this in HOBOWARE and re-export to csv
head(summ_files)

# the goods
wl_dat <- data.frame(wl_dat[1]) # compiled and trimmed data for all loggers in raw folder

# wl_dat <- get_logger_data(path_to_data = "data/water-level/WE/processed/2024/", logger_type = "waterlevel_U20", data_processing = "processed_bound")
# wl_dat2 <- get_logger_data(path_to_data = "data/water-level/WE/processed/2023/", logger_type = "waterlevel_U20", data_processing = "processed_bound")
# wl_dat <- rbind(wl_dat, wl_dat2)
# rm(wl_dat2)
# wl_dat <- wl_dat[order(wl_dat$site_station_code, wl_dat$timestamp ), ]

# STEP 6: WL: bind nearest baro to WL dat ####
# REQUIRES: metadata as site_station_code, logger_type, latitude, longitude
# Links loggers to nearest logger_type==baro_U20
# Applies a barometric pressure correction when multiple baro stations are used per U26/U20 logger
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
                                path_to_output_folder = "data/water-level/WE/processed/",
                                baro_data = baro_dat,
                                metadata = metadat_barolink)

wl_baro_dat <- get_logger_data(path_to_data = "data/water-level/WE/processed/2024/", logger_type = "waterlevel_U20", data_processing = "processed_withbaro")
wl_baro_dat2 <- get_logger_data(path_to_data = "data/water-level/WE/processed/2023/", logger_type = "waterlevel_U20", data_processing = "processed_withbaro")
wl_baro_dat <- rbind(wl_baro_dat, wl_baro_dat2)
rm(wl_baro_dat2)
wl_baro_dat$timestamp <- as.POSIXct(wl_baro_dat$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")
wl_baro_dat <- wl_baro_dat[order(wl_baro_dat$site_station_code, wl_baro_dat$timestamp ), ]

# STEP 7. WL: Apply barometric compensation & QAQC WL data ####
# converts kPa to m 
# this step must be done site by site, as it requires consideration of stage/depth measurements at each site
# Requires metadata of reference water levels as stage or depth and timestamps
manual_dat <- read.csv("data/site-attribute/NT_manual_waterlevel_discharge_20241028.csv")
manual_dat$stage_timestamp <- as.POSIXct(manual_dat$stage_timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")
wl_sites <- unique(wl_baro_dat$site_station_code)

### BARR ####
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = wl_sites[1],
                                    reference_data = manual_dat,
                                    select_measurement = 1)

site_wl_dat <- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# something is wrong with this baro logger - switch it out for a new one
# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = wl_sites[1])
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

### COOK 20 ####
this_site <- wl_sites[2]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1)

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# looks like some overwinter ice spikes Nov - April
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-12-20 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-12-24 19:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-03 15:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-08 10:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-03-24 04:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-24 11:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), 
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site, 
                                       reason_to_adjust = "ice")

# dry periods
site_wl_dat <- adjust_logger_NA(timestamp_start = as.POSIXct("2024-04-20 15:00:00", tz = "UTC"),
                                timestamp_end = "2024-05-11 05:00:00 UTC", 
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site, 
                                reason_to_adjust = "dry",
                                apply_to_all_data = TRUE)

#check adjustments
waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/", 
                 logger_type = "waterlevel_U20")


### COOK 40 ####
cook_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = "COOK_WE_40",
                                    reference_data = manual_dat,
                                    select_measurement = 1)

cook_wl_dat <- as.data.frame(cook_wl[1])
cook_wl_ref <- data.frame(cook_wl[2])
cook_wl[3]

# all reference points look aligned with model so looks good!
cook_wl <- waterlevel_qaqc(waterlevel_data = cook_wl_dat, select_station = "COOK_WE_40")
cook_wl_dat <- as.data.frame(cook_wl[1])
cook_wl[2]

# looks like some overwinter ice spikes Nov - April
cook_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-10-25 02:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-10-26 03:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), waterlevel_data = cook_wl_dat, select_station = "COOK_WE_40", reason_to_adjust = "ice")
cook_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-11-28 19:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-12-31 23:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), waterlevel_data = cook_wl_dat, select_station = "COOK_WE_40", reason_to_adjust = "ice")
cook_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-29 03:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"), waterlevel_data = cook_wl_dat, select_station = "COOK_WE_40", reason_to_adjust = "ice")
# use the plot_only argument in waterlevel_QAQC to visualize the chnges you made
waterlevel_qaqc(waterlevel_data = cook_wl_dat, select_station = "COOK_WE_40", plot_only = TRUE)
# spikes in fall are real- big pulses observed
# no dry spells - no flags and water temp is well within bounds of air temp

#save wl dat
write_clean_data(input_data = cook_wl_dat, path_to_output_folder = "data/water-level/WE/clean/", logger_type = "waterlevel_U20")

### GOOS 30 ####
this_site <- wl_sites[4]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# a few ice spikes
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2023-11-23 10:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-11-29 07:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site,
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)
# one weird spike in summer, but doesnt look like a cable break or dry - leaving it
# flood of warm water, maybe an upper beaver dam broke
#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### LITT ####
this_site <- wl_sites[5]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# long frozen period
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2023-10-31 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-05-05 08:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                       input_data = site_wl_dat, 
                                       logger_to_adjust = "waterlevel_U20",
                                       select_station = this_site,
                                       reason_to_adjust = "ice")

waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### LO09_WE_20 ####
this_site <- wl_sites[6]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# long frozen period
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2023-10-19 09:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-04-08 03:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "ice")
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2024-09-12 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-09-25 17:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "dry")

waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")


### LO09_WE_30 ####
this_site <- wl_sites[7]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# long frozen period
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2023-10-22 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-04-04 08:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "ice")
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2024-09-23 19:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-09-26 02:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "dry")

waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### LO10_WE_30 ####
this_site <- wl_sites[8]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# long frozen period
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2023-11-18 21:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-01-27 09:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "ice")
#big weird ice spike - more like a lump....removing it but unsure
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-30 04:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-04-10 16:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                waterlevel_data = site_wl_dat, 
                                select_station = this_site,
                                reason_to_adjust = "ice")
# dry spot
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2024-09-21 09:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-09-28 04:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "dry")

waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")
### LO12_WE_30 ####
this_site <- wl_sites[9]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# all reference points look aligned with model so looks good!
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# long frozen period
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2023-11-30 06:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-03-27 08:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                input_data = site_wl_dat, 
                                logger_to_adjust = "waterlevel_U20",
                                select_station = this_site,
                                reason_to_adjust = "ice")
#dry period
site_wl_dat <- adjust_logger_NA(timestamp_start =as.POSIXct("2024-03-27 09:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-05-15 18:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                       input_data = site_wl_dat, 
                                       logger_to_adjust = "waterlevel_U20",
                                       select_station = this_site,
                                       reason_to_adjust = "dry")

waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### MILE_WE_20 ####
this_site <- wl_sites[10]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 2,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# looks good
site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# need to shift waterlevel up
site_wl_dat <- adjust_waterlevel_zero(timestamp_start =as.POSIXct("2023-10-24 02:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-01-12 08:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                waterlevel_data = site_wl_dat, 
                                select_station = this_site,
                                shift_up_or_down = "up")
# ice spike
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-01-12 08:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-01-24 11:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                waterlevel_data = site_wl_dat, 
                                select_station = this_site,
                                reason_to_adjust = "ice")
site_wl_dat <- adjust_waterlevel_spike(timestamp_start =as.POSIXct("2024-03-06 09:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-03-11 20:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                       waterlevel_data = site_wl_dat, 
                                       select_station = this_site,
                                       reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")


### MILE_WE_20 ####
this_site <- wl_sites[11]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

# logger was shifted 2024-09-12 14:00:00 so need to split data and deal with separately
site_wl_dat1 <- wl_baro_dat %>% filter(site_station_code=="MILE_WE_40"&timestamp <= as.POSIXct("2024-09-12 13:00:00", tz = "UTC"))
site_wl_dat2 <- wl_baro_dat %>% filter(site_station_code=="MILE_WE_40"&timestamp > as.POSIXct("2024-09-12 13:00:00", tz = "UTC"))

# prior to shift
site_wl1 <- convert_waterlevel_kPa_m(input_data = site_wl_dat1, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    select_measurement = 1,
                                    reference_type = "depth")

site_wl_dat1<- as.data.frame(site_wl1[1])
site_wl_ref <- data.frame(site_wl1[2])
site_wl1[3]

site_wl_dat1 <- waterlevel_qaqc(waterlevel_data = site_wl_dat1, select_station = this_site)
site_wl_dat1[2]
site_wl_dat1 <- as.data.frame(site_wl_dat1[1])

# dry periods at start and end
site_wl_dat1 <- adjust_logger_NA(timestamp_start =site_wl_dat1$timestamp[1],
                                      timestamp_end = as.POSIXct("2023-12-16 09:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                      input_data = site_wl_dat1, 
                                      logger_to_adjust = "waterlevel_U20",
                                      select_station = this_site,
                                      reason_to_adjust = "dry")
site_wl_dat1 <- adjust_logger_NA(timestamp_start =as.POSIXct("2024-07-30 18:00:00", format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                 timestamp_end = site_wl_dat1$timestamp[length(site_wl_dat1$timestamp)],
                                 input_data = site_wl_dat1, 
                                 logger_to_adjust = "waterlevel_U20",
                                 select_station = this_site,
                                 reason_to_adjust = "dry")

waterlevel_qaqc(waterlevel_data = site_wl_dat1, select_station = this_site, plot_only = TRUE)
# after shift
site_wl2 <- convert_waterlevel_kPa_m(input_data = site_wl_dat2, 
                                     select_station = this_site,
                                     reference_data = manual_dat,
                                     select_measurement = 1,
                                     reference_type = "depth")

site_wl_dat2<- as.data.frame(site_wl2[1])
site_wl_ref <- data.frame(site_wl2[2])
site_wl2[3]

site_wl_dat2 <- waterlevel_qaqc(waterlevel_data = site_wl_dat2, select_station = this_site)
site_wl_dat2[2]
site_wl_dat2 <- as.data.frame(site_wl_dat2[1])

#bind together
site_wl_dat <- rbind(site_wl_dat1, site_wl_dat2)
waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")


### NWFI ####
# missing depth in manual dat so manually make reference data from field datasheet
site_station_code = "NWFI_WE_30"
timestamp = as.POSIXct("2023-10-13 16:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
depth_m = 0.703
ref_dat <- data.frame(site_station_code,timestamp, depth_m)

this_site <- wl_sites[12]
site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = ref_dat,
                                    var_reference_waterlevel_m = "depth_m",
                                    var_reference_timestamp = "timestamp",
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]
# only one reference waterlevel so must accept

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
# no flags, logger didn't freeze, looks good

#save wl dat
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### PYRA ####
this_site <- wl_sites[13]

# this site was moved on 2024-09-12 11:00:00
# split into two datasets and deal with separately
site_wl1 <- wl_baro_dat %>% filter(site_station_code==this_site & timestamp<=as.POSIXct("2024-09-12 11:00:00", tz = "UTC"))
site_wl2 <- wl_baro_dat %>% filter(site_station_code==this_site &timestamp>as.POSIXct("2024-09-12 11:00:00", tz = "UTC"))

site_wl1 <- convert_waterlevel_kPa_m(input_data = site_wl1, 
                                    select_station = this_site,
                                    reference_data = manual_dat,
                                    reference_type = "depth")

site_wl_dat1<- as.data.frame(site_wl1[1])
site_wl_ref1 <- data.frame(site_wl1[2])
site_wl1[3]
# only one ref level prior to move, dry so accurate
site_wl_dat1 <- waterlevel_qaqc(waterlevel_data = site_wl_dat1, select_station = this_site)
site_wl_dat1[2]
site_wl_dat1 <- as.data.frame(site_wl_dat1[1])

# remove ice spikes
site_wl_dat1 <- adjust_waterlevel_spike(waterlevel_data = site_wl_dat1, select_station = this_site, 
                                        timestamp_start = as.POSIXct("2023-11-28 23:00:00", tz = "UTC"), 
                                        timestamp_end = as.POSIXct("2024-04-05 09:00:00"),
                                        reason_to_adjust = "ice")
waterlevel_qaqc(waterlevel_data = site_wl_dat1, select_station = this_site, plot_only = TRUE)
# dry flags dont seem dry, leave
# go on to after logger move data
site_wl2 <- convert_waterlevel_kPa_m(input_data = site_wl2, 
                                     select_station = this_site,
                                     reference_data = manual_dat,
                                     reference_type = "depth")

site_wl_dat2<- as.data.frame(site_wl2[1])
site_wl_ref2 <- data.frame(site_wl2[2])
site_wl2[3]
# ref points aligned
site_wl_dat2 <- waterlevel_qaqc(waterlevel_data = site_wl_dat2, select_station = this_site)
site_wl_dat2[2]
site_wl_dat2 <- as.data.frame(site_wl_dat2[1])
site_wl_dat2$qaqc_code <- "LOGGER_MOVED"
site_wl_dat2$qaqc_note <- "logger moved from dry location to pool downstream"
# no issues
# join 1 and 2
site_wl_dat <- rbind(site_wl_dat1, site_wl_dat2)
# view full data
waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)
# treat logger move as cable break and shift level post break to align with waterlevel prior to break

site_wl_dat <- adjust_waterlevel_cablebreak(waterlevel_data = site_wl_dat, select_station = this_site,
                                            timestamp_start = as.POSIXct("2024-09-12 11:00:00", tz = "UTC"),
                                            timestamp_end = as.POSIXct("2024-09-12 12:00:00", tz = "UTC"))
waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)
# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")


### STIL 10####
this_site <- wl_sites[14]
site_wl<- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                     select_station = this_site,
                                     reference_data = manual_dat,
                                     reference_type = "stage")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spikes
site_wl_dat <- adjust_waterlevel_spike(waterlevel_data = site_wl_dat, select_station = this_site,
  timestamp_start= as.POSIXct("2023-10-25 05:00:00", tz = "UTC"),
 timestamp_end = as.POSIXct("2024-03-08 12:00:00", tz = "UTC"),
 reason_to_adjust = "ice"
 )


# dry period
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat, select_station = this_site,
                                logger_to_adjust = "waterlevel_U20",
                                       timestamp_start= as.POSIXct("2024-03-11 13:00:00", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-05-06 00:00:00", tz = "UTC"),
                                       reason_to_adjust = "dry")

site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat, select_station = this_site,
                                logger_to_adjust = "waterlevel_U20",
                                timestamp_start= as.POSIXct("2024-09-16 17:00:00", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-09-23 05:00:00", tz = "UTC"),
                                reason_to_adjust = "dry")

# view
waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site, plot_only = TRUE)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")


### STIL 30####
this_site <- wl_sites[15]
site_wl<- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                   select_station = this_site,
                                   reference_data = manual_dat,
                                   reference_type = "stage")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spike
site_wl_dat <- adjust_waterlevel_spike(waterlevel_data = site_wl_dat, select_station = this_site,
                                       timestamp_start= as.POSIXct("2023-11-02 16:00:00", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-05-05 23:00:00", tz = "UTC"),
                                       reason_to_adjust = "ice")

site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat, select_station = this_site,
                                logger_to_adjust = "waterlevel_U20",
                                timestamp_start= as.POSIXct("2024-09-08 09:00:00", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-09-26 08:00:00", tz = "UTC"),
                                reason_to_adjust = "dry")


waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### STIL 40####
this_site <- wl_sites[16]
site_station_code = this_site
timestamp = as.POSIXct("2023-10-14 16:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
depth_m = 0.492
ref_dat <- data.frame(site_station_code,timestamp, depth_m)

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = ref_dat,
                                    var_reference_waterlevel_m = "depth_m",
                                    var_reference_timestamp = "timestamp",
                                    reference_type = "depth")


site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spike
site_wl_dat <- adjust_waterlevel_spike(waterlevel_data = site_wl_dat, select_station = this_site,
                                       timestamp_start= as.POSIXct("2023-12-01 22:00:00", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-04-18 14:00:00", tz = "UTC"),
                                       reason_to_adjust = "ice")
#cable break
site_wl_dat <- adjust_waterlevel_cablebreak(waterlevel_data = site_wl_dat, select_station = this_site,
                                       timestamp_start= as.POSIXct("2024-10-11 14:00:00", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-10-11 15:00:00", tz = "UTC"))

waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### STRU 10 ####
this_site <- wl_sites[17]
site_wl<- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                   select_station = this_site,
                                   reference_data = manual_dat,
                                   reference_type = "stage")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# dry for majority of year, except small pulses form NT mainstem
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat,
                                      select_station = this_site,
                                      timestamp_start = as.POSIXct("2023-10-12 15:00:00", tz = "UTC"),
                                      timestamp_end = as.POSIXct("2024-05-04 07:00:00", tz = "UTC"),
                                      reason_to_adjust = "dry",
                                logger_to_adjust = "waterlevel_U20",
                                apply_to_all_data = TRUE)

waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### STRU 10 ####
this_site <- wl_sites[18]
site_wl<- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                   select_station = this_site,
                                   reference_data = manual_dat,
                                   reference_type = "stage",
                                   select_measurement = 1)

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# error in site measurements, shift down to 0 when dry
site_wl_dat <- adjust_waterlevel_zero(waterlevel_data = site_wl_dat,
                                select_station = this_site,
                                timestamp_start = as.POSIXct("2023-10-12 15:00:00", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-05-04 07:00:00", tz = "UTC"),
                                shift_up_or_down = "down")

# dry for majority of year, except small pulses form NT mainstem
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat,
                                select_station = this_site,
                                timestamp_start = as.POSIXct("2023-10-12 15:00:00", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-05-04 07:00:00", tz = "UTC"),
                                reason_to_adjust = "dry",
                                logger_to_adjust = "waterlevel_U20",
                                apply_to_all_data = TRUE)

waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### STRU 40 ####
this_site <- wl_sites[19]
site_station_code = this_site
timestamp = as.POSIXct("2023-10-12 15:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
depth_m = 0.0
ref_dat <- data.frame(site_station_code,timestamp, depth_m)

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = ref_dat,
                                    var_reference_waterlevel_m = "depth_m",
                                    var_reference_timestamp = "timestamp",
                                    reference_type = "depth")

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# dry for majority of year, except small pulses form NT mainstem
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat,
                                select_station = this_site,
                                timestamp_start = as.POSIXct("2024-04-01 16:00:00", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-09-13 05:00:00", tz = "UTC"),
                                reason_to_adjust = "dry",
                                logger_to_adjust = "waterlevel_U20",
                                apply_to_all_data = TRUE)
# ice
site_wl_dat <- adjust_logger_NA(input_data = site_wl_dat,
                                select_station = this_site,
                                timestamp_start = as.POSIXct("2023-10-30 11:00:00", tz = "UTC"),
                                timestamp_end = as.POSIXct("2024-03-06 05:00:00", tz = "UTC"),
                                reason_to_adjust = "ice",
                                logger_to_adjust = "waterlevel_U20")

waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### TUMT 20 ####
this_site <- wl_sites[20]
site_station_code = this_site
timestamp = as.POSIXct("2024-10-11 12:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
depth_m = 0.47
ref_dat <- data.frame(site_station_code,timestamp, depth_m)

site_wl <- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                    select_station = this_site,
                                    reference_data = ref_dat,
                                    var_reference_waterlevel_m = "depth_m",
                                    var_reference_timestamp = "timestamp",
                                    reference_type = "depth")
site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# ice spike
site_wl_dat <- adjust_waterlevel_spike(waterlevel_data = site_wl_dat, select_station = this_site,
                                       timestamp_start= as.POSIXct("2024-01-12 23:00:00", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2024-01-24 14:00:00", tz = "UTC"),
                                       reason_to_adjust = "ice")

waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### TUMT 40 ####
this_site <- wl_sites[21]
site_wl<- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                   select_station = this_site,
                                   reference_data = manual_dat,
                                   reference_type = "depth",
                                   select_measurement = 1)

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])
# ice spike
site_wl_dat <- adjust_waterlevel_spike(waterlevel_data = site_wl_dat, select_station = this_site,
                                       timestamp_start= as.POSIXct("2023-11-23 06:00:00", tz = "UTC"),
                                       timestamp_end = as.POSIXct("2023-12-21 16:00:00", tz = "UTC"),
                                       reason_to_adjust = "ice")

waterlevel_qaqc(plot_only = TRUE, waterlevel_data = site_wl_dat, select_station = this_site)

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")

### WIRE ####
this_site <- wl_sites[22]
site_wl<- convert_waterlevel_kPa_m(input_data = wl_baro_dat, 
                                   select_station = this_site,
                                   reference_data = manual_dat,
                                   reference_type = "depth",
                                   select_measurement = 1)

site_wl_dat<- as.data.frame(site_wl[1])
site_wl_ref <- data.frame(site_wl[2])
site_wl[3]

site_wl_dat <- waterlevel_qaqc(waterlevel_data = site_wl_dat, select_station = this_site)
site_wl_dat[2]
site_wl_dat <- as.data.frame(site_wl_dat[1])

# looks good
write_clean_data(input_data = site_wl_dat, 
                 path_to_output_folder = "data/water-level/WE/clean/",
                 logger_type = "waterlevel_U20")



