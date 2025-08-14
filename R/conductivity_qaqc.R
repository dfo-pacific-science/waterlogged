#' CONDUCTIVITY LOGGER DRIFT CORRECTION & QA/QC
#' @param input_data raw data csv from barometric U20 loggers
#' @param select_station site_station_code for desired logger station, supplied barometric data will be filtered by this code
#' @param var_airtemp_C The variable for air temperature in degrees Celsius
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, values out of range are removed and flagged
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @param ref_deploy_ofset Number of hours to search between deployment and YSI measurement (between 0 and 2)
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_ref_watertemp_C Name of column for reference water temperature (in degrees Celcius)
#' @param var_conduct_uScm Name of column for reference conductivity (uScm)
#' @return
#' @example
#' library(here)
#' reference_file = here("Data", "site-attribute", "Deadman_YSI_waterchem_20241030.csv")
#' input_data = read_csv(here("Data", "conductivity", "2024", "OC", "intermediate", "DM_OC_2024_conductivity_all_RAW.csv")) %>%
#'   filter(site_station_code == "BROU_OC_30")
#' conductivity_qaqc(input_data = read_csv(here("Data", "conductivity", "2024", "OC", "intermediate", "DM_OC_2024_conductivity_all_RAW.csv")) %>%
#'                     filter(site_station_code == "BROU_OC_30"),
#'                   reference_file = here("Data", "site-attribute", "Deadman_YSI_waterchem_20241030.csv"))
#' @export

conductivity_qaqc <- function(input_data,
                              plot_only = FALSE,
                              reference_file,
                              drop_ref = NA, # specify either a positive integer or a vector c(x, y, etc.)
                              ref_deploy_offset = 1, # hours to search between deployment and YSI measurement (between 0 and 2)
                              var_conduct_uScm = "conduct_uScm_U24",
                              var_watertemp_C = "watertemp_C_U24",
                              var_airtemp_C = NA,
                              var_waterlevel_m = NA,
                              var_ref_conduct_uScm = "cond_uscm_ysi",
                              var_ref_watertemp_C = "watertemp_C_ysi"){
  # load packages
  require(tidyverse)
  require(fuzzyjoin)
  require(plotly)


  cdat <- input_data

  site <- unique(cdat$site_station_code)

  if (length(site) > 1) {
    stop("Ensure that only a single station has been selected")
  }

  # rename input columns to local columns
  names(cdat)[names(cdat) == var_conduct_uScm ] <- "conduct_uScm"
  names(cdat)[names(cdat) == var_watertemp_C ] <- "watertemp_C"
  names(cdat)[names(cdat) == paste0(var_conduct_uScm, "_adj") ] <- "conduct_uScm_adj"
  names(cdat)[names(cdat) == paste0(var_watertemp_C, "_adj" )] <- "watertemp_C_adj"

  if(!is.na(var_airtemp_C)){
    names(cdat)[names(cdat) == var_airtemp_C ] <- "airtemp_C"
  }

  # Perform drift correction and automated QA/QC if you're not just plotting the data
  if(plot_only == FALSE){
    cdat <- cdat %>%
      mutate(timestamp = round_date(timestamp, unit = "hour"), # round to the nearest hour (same for YSI) so ref_deploy_offset integer represents an hour and not minutes
             conduct_uScm_adj = conduct_uScm,
             watertemp_C_adj = watertemp_C,
             co_qaqc_note = NA_character_,
             co_qaqc_adj = NA_character_,
             co_qaqc_code = NA_character_)

    time_range <- range(cdat$timestamp)

    ## CORRECT FOR DRIFT BASED ON YSI MEASUREMENTS
    ref_dat <- read_csv(reference_file)

    # rename input columns to local columns
    names(ref_dat)[names(ref_dat) == var_ref_conduct_uScm ] <- "ref_conduct_uScm"
    names(ref_dat)[names(ref_dat) == var_ref_watertemp_C ] <- "ref_watertemp_C"

    ref_dat <- ref_dat %>%
      mutate(ref_timestamp = as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M", tz = "UTC"),
             ref_timestamp = round_date(ref_timestamp, unit = "hour"),
             timestamp = ref_timestamp) %>%
      filter(site_station_code == site) %>%
      filter(ref_timestamp >= (time_range[1] - 60*120) & ref_timestamp <= (time_range[2] + 60*120)) %>%  # up to 2 hrs between logger timestamps
      select(ref_timestamp,
             timestamp,
             ref_conduct_uScm,
             ref_watertemp_C,
             ref_spc_uScm = spc_uscm_ysi)

    # drop rows if specified
    if(!all(is.na(drop_ref))) {
      ref_dat <- ref_dat[-drop_ref, , drop = FALSE]
    }

    # Join manual YSI measurments to conductivity data frame
    cdat_ref.1 <- left_join(cdat, ref_dat, by = "timestamp")

    # Where ref timestamps are beyond the logger data (e.g., just after deployment/retrieval)
    # create a second data frame and use a fuzzy join
    ref_unmatched <- left_join(ref_dat, cdat, by = "timestamp") %>%
      filter(is.na(conduct_uScm)) %>%
      select(starts_with(("ref_")))

    cdat_ref.2 <- difference_left_join(cdat, ref_unmatched,
                                       by = c("timestamp" = "ref_timestamp"),
                                       # distance_col = "distance",
                                       max_dist = ref_deploy_offset) %>%
      filter(!is.na(ref_timestamp))

    total_time <- difftime(max(cdat$timestamp), min(cdat$timestamp), units = "hours")

    cdat_ref <- bind_rows(cdat_ref.2, cdat_ref.1) %>%
      distinct(timestamp, .keep_all = TRUE) %>%
      arrange(timestamp)

    # If there is no initial reference measurement taken at the start of the data frame
    # Then create a "dummy" reference measurement set to the values of the first logger timestamp
    # This is used to back-calculate drift assuming that logger calibration is perfect at the start of logging

    if (is.na(cdat_ref$ref_conduct_uScm[1])) {
      cdat_ref$ref_conduct_uScm[1] <- cdat_ref$conduct_uScm[1]
      cdat_ref$ref_watertemp_C[1] <- cdat_ref$watertemp_C[1]
      cdat_ref$ref_timestamp[1] <- cdat_ref$timestamp[1]

      warning("No initial YSI reference point found. Estimating drift based on perfect calibration at the start of the logger data file.")
    }

    ## DETERMINE DRIFT CORRECTION FACTOR
    ## Drift is calculated as the % difference between the reference & logger measurement

    # Isolate the reference (YSI) measurements to calculate total differences
    diff <- cdat_ref %>%
      filter(!is.na(ref_conduct_uScm)) %>%
      mutate(ref_time_intv = as.numeric(timestamp - lag(timestamp))*24, # Number of hours between each reference measurement
             ref_time_intv = replace_na(ref_time_intv, 0), # Only applies to the first measurement
             per_c_offset = ref_conduct_uScm/conduct_uScm, # Percent offset between reference & logger measurements
             per_t_offset = ref_watertemp_C/watertemp_C,
             c_offset_diff = per_c_offset - lag(per_c_offset), # Difference between offsets for sequential pairs of reference measurements
             c_offset_diff = replace_na(c_offset_diff, 0),
             t_offset_diff = per_t_offset - lag(per_t_offset),
             t_offset_diff = replace_na(t_offset_diff, 0))

    # Join the reference difference measurements with the logger data frame
    cdat_ref <- left_join(cdat_ref,
                          select(diff,
                                 ref_timestamp, ref_time_intv,
                                 per_c_offset, c_offset_diff,
                                 per_t_offset, t_offset_diff),
                          by = "ref_timestamp")

    ## Calculate the time ratio
    # This is the proportion of time that has passed since the last reference measurement
    # divided by the total amount of time elapsed between ref measurements
    cdat_ref <- cdat_ref %>%
      fill(ref_time_intv, .direction = "up") %>%
      mutate(ref_timestamp2 = ref_timestamp) %>%
      fill(ref_timestamp2, .direction = "down") %>%
      mutate(time_elapsed = as.numeric(difftime(timestamp, ref_timestamp2, units = "hours")),
             time_ratio = time_elapsed/ref_time_intv,
             time_ratio = replace_na(time_ratio,0),
             time_ratio = ifelse(is.infinite(time_ratio), 0, time_ratio)) # Replace NAs and infinite values (e.g, caused by dividing by 0) in 'time_ratio'

    ## Calculate the offset ratio
    # Fill NAs in the initial differences between reference & logger measurements (offsets)
    # And the total difference between pairs of offsets
    cdat_ref <- cdat_ref %>%
      fill(per_c_offset, .direction = "down") %>%
      fill(per_t_offset, .direction = "down") %>%
      fill(c_offset_diff, .direction = "up") %>%
      fill(t_offset_diff, .direction = "up")

    ## Calculate drift relative to time elapsed
    cdat <- cdat_ref %>%
      mutate(c_drift = per_c_offset + c_offset_diff*time_ratio,
             t_drift = per_t_offset + t_offset_diff*time_ratio) %>%
      # If there are no subsequent reference measurements, use the last known % offset
      fill(c_drift, .direction = "down") %>%
      fill(t_drift, .direction = "down") %>%
      # Calculate the drift-corrected conductivity & water temperature measurements
      mutate(conduct_uScm_adj = conduct_uScm*c_drift,
             watertemp_C_adj = watertemp_C*t_drift)


    ## DATA QA/QC FLAGS

    # Disturbance flag if conductivity changes by more than 200 uS/cm in an hour
    for(i in 1:(length(cdat$timestamp)-1)) {
      if(!is.na(cdat$conduct_uScm[i])) {
        cdiff <- cdat$conduct_uScm[i] - cdat$conduct_uScm[i+1]
        if(abs(cdiff) > 200) {
          cdat$co_qaqc_code[i] <- "FLAG_DISTURBANCE"
        }
      }
    }

    # Additional flags
    for(i in 1:(length(cdat$timestamp))) {
      if(!is.na(cdat$conduct_uScm[i])) {

        if(is.na(var_airtemp_C)){
          # if conductivity reads less than 10 uS/cm, then flag as dry
          # only applies if there is no air temp column
          if(cdat$conduct_uScm[i] < 10) {
            cdat$co_qaqc_code[i] <- "FLAG_DRY"
          }
        }

        # if water temperature is less than 0.3C, then flag as ice
        if(cdat$watertemp_C[i] < 0.3){
          cdat$co_qaqc_code[i] <- "FLAG_ICE"
        }

        # if water temperature is less than -1C, then remove
        if(cdat$watertemp_C[i]< -1){
          cdat$watertemp_C_adj[i] <- NA
          cdat$co_qaqc_note[i] <-  "water temp out of range"
          cdat$co_qaqc_adj[i] <-  "REMOVED"
          cdat$co_qaqc_code[i] <- "LOGGER_ICE"
        }

        # if water temperature is between 0C and -1C, then change to 0
        if(cdat$watertemp_C[i]<0 & cdat$watertemp_C[i]>-1){
          cdat$co_qaqc_note[i] <-  "corrected to zero"
          cdat$co_qaqc_adj[i] <-  "REPLACED"
          cdat$co_qaqc_code[i] <- "NEGATIVE_WT"
          cdat$watertemp_C_adj[i] <- 0
        }
      } # end of if data not NA
    } # end of additional flags

    # Flag dry periods using air temperature
    if(!is.na(var_airtemp_C)){
      # calc air to water temp diff
      cdat$air_watertemp_diff <- abs(cdat$airtemp_C - cdat$watertemp_C)

      # if water temp within 2 degrees of air temp and < 10 uS/cm for 12 consecutive hours:
      print("Determining dry periods...")
      counter <- 0
      threshold <- 12

      for (i in 1:length(cdat$timestamp)) {
        if (cdat$air_watertemp_diff[i] <= 2 && cdat$conduct_uScm[i] < 10) {
          counter <- counter + 1
        } else {
          counter <- 0
        }

        if (counter >= threshold) {
          cdat$co_qaqc_code[(i - threshold):i] <- "FLAG_DRY"
        }
      }
    } # end of air temp dry flag

  } # end of plot_only = FALSE


  ## PLOT DATA
  p <- ggplot(cdat, aes(x = timestamp)) +
    geom_line(aes(y = conduct_uScm, colour = "conduct_uScm"), alpha = 0.5)+
    geom_line(aes(y = conduct_uScm_adj, colour = "conduct_uScm_adj"))+
    geom_point(aes(y = ref_conduct_uScm, colour = "ref_conduct"), alpha = 0.5)
  labs(title = paste("QAQC for:", site),
       y = "Conductivity (uS/cm)",
       x = "Timestamp",
       color = "",
       shape = "") +
    theme_classic()

  # Plot if data contains water level
  # Use the QAQC codes from the water level data instead of the conductivity data
  if (!is.na(var_waterlevel_m)){

    names(cdat)[names(cdat) == var_waterlevel_m ] <- "waterlevel_m"

    p <- p +
      geom_line(data = cdat, aes(y = waterlevel_m*100, colour = "waterlevel_m"), linewidth = 0.3) +
      geom_point(data = cdat %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG|LOGGER", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black") +
      scale_colour_manual(values = c("conduct_uScm" = "#00008B",
                                     "conduct_uScm_adj" = "#00008B",
                                     "ref_conduct" = "darkred",
                                     "waterlevel_m" = "black")) +
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "LOGGER_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "LOGGER_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4,
                                    "LOGGER_DRY" = 4))
  }

  # No water level data
  # Use QAQC codes for conductivity
  else {
    p <- p +
      geom_point(data = cdat %>% drop_na(co_qaqc_code) %>% filter(grepl("FLAG", co_qaqc_code)),
                 aes(y = 0, shape = as.factor(co_qaqc_code)),
                 color = "black") +
      scale_colour_manual(values = c("conduct_uScm" = "#00008B",
                                     "conduct_uScm_adj" = "#00008B",
                                     "ref_conduct" = "darkred")) +
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4))
  }

  q <- ggplot(cdat, aes(x = timestamp)) +
    geom_line(aes(y = watertemp_C, colour = "watertemp_C"), alpha = 0.5)+
    geom_line(aes(y = watertemp_C_adj, colour = "watertemp_C_adj"))+
    geom_point(aes(y = ref_watertemp_C, colour = "ref_watertemp"))+
    labs(title = site,
         y = "Temperature (C)",
         x = "Timestamp",
         color = "",
         shape = "") +
    theme_classic()

  # Plot if the data contain air temperature
  if(!is.na(var_airtemp_C)){
    q <- q +
      geom_line(aes(y = airtemp_C, colour = "airtemp_C"), linewidth = 0.3, alpha = 0.5)+
      scale_colour_manual(values = c("watertemp_C" = "#619b8a",
                                     "watertemp_C_adj" = "#619b8a",
                                     "airtemp_C" = "lightblue",
                                     "ref_watertemp" = "green"))
  }
  else {
    q <- q +
      scale_colour_manual(values = c("watertemp_C" = "#619b8a",
                                     "watertemp_C_adj" = "#619b8a",
                                     "ref_watertemp" = "green"))
  }

  # return variables to user input naming
  names(cdat)[names(cdat) == "conduct_uScm"] <- var_conduct_uScm
  names(cdat)[names(cdat) == "watertemp_C"] <- var_watertemp_C
  names(cdat)[names(cdat) == "conduct_uScm_adj"] <- paste0(var_conduct_uScm, "_adj")
  names(cdat)[names(cdat) == "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
  if ("airtemp_C" %in% names(cdat)) names(cdat)[names(cdat) == "airtemp_C"] <- var_airtemp_C
  if ("waterlevel_m" %in% names(cdat)) names(cdat)[names(cdat) == "waterlevel_m"] <- var_waterlevel_m

  if(plot_only == FALSE) {
    print("Results returned as list: select [1] for conductivity data with QAQC flags, [2] reference YSI measurements, [3] conductivity data to visually assess for dry/ice/disturbance periods")
    return(list(cdat, ref_dat, plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
  }
  if(plot_only == TRUE){
    return(plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
  }
}
