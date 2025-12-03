#' Remove invalid logger data
#'
#' Remove data for periods when logger was out of water, in ice, or disturbed
#' @param input_data Data for desired site_station_code
#' @param select_station Unique identifier code for station
#' @param timestamp_format Formatting of timestamp column of input data, following POSTIX convention. Default is "%Y-%m-%d %H:%M:%S"
#' @param timestamp_timezone Timezone of timestamp column of input data, following POSTIX convention. Default is UTC to force ignore daylight savings time.
#' @param timestamp_start Date and time of start of dry period: must be formatted the same as timestamp in site_data
#' @param timestamp_end Date and time of end of dry period: must be formatted the same as timestamp in site_data
#' @param reason_to_adjust one of the following: "ice", "dry", or "disturbance"
#' @param logger_to_adjust one of the following: "waterlevel_U20", "dissolvedoxygen_U26", "conductivity_U24", or "watertemp_TidbiT"
#' @param apply_to_all_data logical; if TRUE all water levels lower than the max waterlevel within the specified timeperiod identified as dry will be removed and flagged as dry; if false, only waterlevels within the specified time period are removed as dry
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @param var_do_percsat Name of column for dissolved oxygen in percent air saturation in input data
#' @param var_do_mgl Name of column for dissolved oxygen in mg per litre in input data
#' @param var_conduct_uScm Name of the column for conductivity in microSiemens per centimeter in input data
#' @return Dataset with removed records being flagged as dry, ice, or disturbance,
#' @export

adjust_logger_NA <- function(input_data=site_data, select_station, timestamp_format = "%Y-%m-%d %H:%M:%S", timestamp_timezone = "UTC", timestamp_start, timestamp_end, logger_to_adjust, reason_to_adjust, apply_to_all_data = FALSE, var_do_mgl = "do_mgl_U26", var_do_percsat = "do_percsat_U26", var_waterlevel_m = "waterlevel_m_U20", var_watertemp_C = "default", var_conduct_uScm = "conduct_uScm_U24", var_airtemp_C = "airtemp_C_U20", flag_only = FALSE) {

  # ensures user is only processing one station at a time
  output_data <- input_data[input_data$site_station_code==select_station,]
  rownames(output_data) <- NULL

  #format timestamp
  timestamp_start <-as.POSIXct(timestamp_start, format = timestamp_format, tz = timestamp_timezone)
  timestamp_end <-as.POSIXct(timestamp_end, format = timestamp_format, tz = timestamp_timezone)

  # U20 Water Level Logger adjustments
  if(logger_to_adjust == "waterlevel_U20"){
    # rename user input variables
    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
    names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"

    if(var_watertemp_C == "default"){
      var_watertemp_C <- "watertemp_C_U20"
    }

    names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
    names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj") ] <- "watertemp_C_adj"


    # note all values below dry water level as dry
    if(reason_to_adjust == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$waterlevel_m_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$wl_qaqc_code[i] <- "LOGGER_ICE"
          output_data$wl_qaqc_adj[i] <- "REMOVED"
          output_data$wl_qaqc_note[i] <- "WL data removed due to logger in ice"
        }
      }
    }

    if(reason_to_adjust == "dry") {
      if(apply_to_all_data == TRUE) {
        # get highest wl in designated dry period
        dry_wl <- list()
        for(i in 1:length(output_data$timestamp)) {
          if(!is.na(output_data$waterlevel_m_adj[i]) & output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            dry_wl[i] <- output_data$waterlevel_m_adj[i]
          }
        }
        dry_wl <- unlist(dry_wl)
        dry_wl <- max(dry_wl, na.rm = TRUE)
        # make all wl values at or below this
        for(i in 1:length(output_data$timestamp)){
          if(!is.na(output_data$waterlevel_m_adj[i]) & output_data$waterlevel_m_adj[i]<=dry_wl){
            output_data$waterlevel_m_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_code[i] <- "LOGGER_DRY"
            output_data$wl_qaqc_adj[i] <- "REMOVED"
            output_data$wl_qaqc_note[i] <- paste("Data removed due to dry logger at waterlevel below", dry_wl, "m")          }
        }
      } else {
        for(i in 1:length(output_data$timestamp)) {
          if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            output_data$waterlevel_m_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_code[i] <- "LOGGER_DRY"
            output_data$wl_qaqc_adj[i] <- "REMOVED"
            output_data$wl_qaqc_note[i] <- "Data removed due to dry logger"
          }
        }
      } # apply_to_all_data == FALSE
    }

    if(reason_to_adjust == "disturbance") {
      if(keep_temp == TRUE){
        for(i in 1:length(output_data$timestamp)) {
          if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            output_data$waterlevel_m_adj[i] <- NA
            output_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
            output_data$wl_qaqc_adj[i] <- "REMOVED"
            output_data$wl_qaqc_note[i] <- "Waterlevel data removed due to disturbance to logger"
          }
        }


      } else {

      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$waterlevel_m_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
          output_data$wl_qaqc_adj[i] <- "REMOVED"
          output_data$wl_qaqc_note[i] <- "Data removed due to disturbance to logger"
        }
      }
     } # keep_temp = FALSE
    }
  } # end of U20 adjustments

  # U26 Dissolved Oxygen Logger adjustments
  if(logger_to_adjust == "dissolvedoxygen_U26"){

    names(output_data)[names(output_data) == var_do_mgl ] <- "do_mgl"
    names(output_data)[names(output_data) == paste0(var_do_mgl, "_adj") ] <- "do_mgl_adj"
    names(output_data)[names(output_data) == var_do_percsat ] <- "do_percsat"
    names(output_data)[names(output_data) == paste0(var_do_percsat, "_adj") ] <- "do_percsat_adj"
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"

    if(var_watertemp_C == "default"){
      var_watertemp_C <- "watertemp_C_U26"
    }

    names(output_data)[names(output_data) ==  var_watertemp_C] <- "watertemp_C"
    names(output_data)[names(output_data) ==  paste0(var_watertemp_C, "_adj")] <- "watertemp_C_adj"


    try(names(output_data)[names(output_data) ==  var_waterlevel_m] <- "waterlevel_m")

    # note all values below dry water level as dry
    if(reason_to_adjust == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$do_mgl_adj[i] <- NA
          output_data$do_percsat_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$do_qaqc_code[i] <- "LOGGER_ICE"
          output_data$do_qaqc_adj[i] <- "REMOVED"
          output_data$do_qaqc_note[i] <- "DO data removed due to logger in ice"
        }
      }
    }

    if(reason_to_adjust == "disturbance") {
      if(keep_temp == TRUE){
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$do_mgl_adj[i] <- NA
          output_data$do_percsat_adj[i] <- NA
          output_data$do_qaqc_code[i] <- "LOGGER_DISTURBANCE"
          output_data$do_qaqc_adj[i] <- "REMOVED"
          output_data$do_qaqc_note[i] <- "DO data removed due to disturbance to logger"
        }
      }
      } else {

        for(i in 1:length(output_data$timestamp)) {
          if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            output_data$do_mgl_adj[i] <- NA
            output_data$do_percsat_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$do_qaqc_code[i] <- "LOGGER_DISTURBANCE"
            output_data$do_qaqc_adj[i] <- "REMOVED"
            output_data$do_qaqc_note[i] <- "Data removed due to disturbance to logger"

          }
        }
      } # keep_temp = FALSE
    }

    if(reason_to_adjust == "dry") {
      if(apply_to_all_data == TRUE) {
        if(var_waterlevel_m!="none"){
          # get highest wl in designated dry period
          dry_wl <- list()
          for(i in 1:length(output_data$timestamp)) {
            if(!is.na(output_data$waterlevel_m[i]) & output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
              dry_wl[i] <- output_data$waterlevel_m[i]
            }
          }
          dry_wl <- unlist(dry_wl)
          dry_wl <- max(dry_wl, na.rm = TRUE)
          # make all wl values at or below this
          for(i in 1:length(output_data$timestamp)) {
            if(!is.na(output_data$waterlevel_m[i]) & output_data$waterlevel_m[i]<=dry_wl) {
              output_data$do_percsat_adj[i] <- NA
              output_data$do_mgl_adj[i] <- NA
              output_data$watertemp_C_adj[i] <- NA
              output_data$do_qaqc_code[i] <- "LOGGER_DRY"
              output_data$do_qaqc_adj[i] <- "REMOVED"
              output_data$do_qaqc_note[i] <- paste("DO data removed due to dry logger at waterlevel below", dry_wl, "m")
            }
          }
        } else{print("WARNING: Waterlevel variable missing from input. Unable to apply to all data.")}
      } else {
        for(i in 1:length(output_data$timestamp)) {
          if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
            output_data$do_mgl_adj[i] <- NA
            output_data$do_percsat_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$do_qaqc_code[i] <- "LOGGER_DRY"
            output_data$do_qaqc_adj[i] <- "REMOVED"
            output_data$do_qaqc_note[i] <- "DO data removed due to dry logger"

          }
        }
      } # apply_to_all_data == FALSE
    }
  } # end of U26 adjustments

  ## U24 Conductivity Logger adjustments
  if (logger_to_adjust == "conductivity_U24") {

    if(var_watertemp_C == "default"){
      var_watertemp_C <- "watertemp_C_U24"
    }

    # rename user input variables
    names(output_data)[names(output_data) == var_conduct_uScm ] <- "conduct_uScm"
    names(output_data)[names(output_data) == paste0(var_conduct_uScm, "_adj") ] <- "conduct_uScm_adj"
    # correct user entry error
    if(var_watertemp_C == "watertemp_C_U20" | var_watertemp_C == "watertemp_C_U26"){
      var_watertemp_C == "watertemp_C_U24"
    }
    names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
    names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj") ] <- "watertemp_C_adj"

    # remove data if logger is in ice
    if(reason_to_adjust == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$conduct_uScm_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$co_qaqc_code[i] <- "LOGGER_ICE"
          output_data$co_qaqc_adj[i] <- "REMOVED"
          output_data$co_qaqc_note[i] <- "Conductivity data removed due to logger in ice"
        }
      }
    }

    # remove data if logger has been disturbed
    if(reason_to_adjust == "disturbance") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          if(flag_only == FALSE) {
            output_data$conduct_uScm_adj[i] <- NA
            output_data$watertemp_C_adj[i] <- NA
            output_data$co_qaqc_code[i] <- "LOGGER_DISTURBANCE"
            output_data$co_qaqc_adj[i] <- "REMOVED"
            output_data$co_qaqc_note[i] <- "Data removed due to disturbance to logger"
          } # end of flag_only = FALSE
          else {
            output_data$co_qaqc_code[i] <- "LOGGER_DISTURBANCE"
          }
        }
      }
    }

    # remove data if logger has been removed, but still logging
    if(reason_to_adjust == "removed") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$conduct_uScm_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$co_qaqc_code[i] <- "LOGGER_REMOVED"
          output_data$co_qaqc_adj[i] <- "REMOVED"
          output_data$co_qaqc_note[i] <- "Data removed due to logger being manually removed"
        }
      }
    }

    # remove data if logger is dry
    if(reason_to_adjust == "dry") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$conduct_uScm_adj[i] <- NA
          output_data$watertemp_C_adj[i] <- NA
          output_data$co_qaqc_code[i] <- "LOGGER_DRY"
          output_data$co_qaqc_adj[i] <- "REMOVED"
          output_data$co_qaqc_note[i] <- "Data removed due to dry logger"
        }
      }
    } # end of reason == "dry"
  } # end of U24 logger adjustments

  if(logger_to_adjust == "watertemp_TidbiT"){
    # note all values below dry water level as dry

    if(var_watertemp_C == "default"){
      var_watertemp_C <- "watertemp_C_TidbiT"
    }
    names(output_data)[names(output_data) ==  var_watertemp_C] <- "watertemp_C"
    names(output_data)[names(output_data) ==  paste0(var_watertemp_C, "_adj")] <- "watertemp_C_adj"


    if(reason_to_adjust == "ice") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$watertemp_C_adj[i] <- NA
          output_data$qaqc_code[i] <- "LOGGER_ICE"
        }
      }
    }

    if(reason_to_adjust == "dry") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$watertemp_C_adj[i] <- NA
          output_data$qaqc_code[i] <- "LOGGER_DRY"
        }
      }
    }


    if(reason_to_adjust == "disturbance") {
      for(i in 1:length(output_data$timestamp)) {
        if(output_data$timestamp[i]>=timestamp_start & output_data$timestamp[i]<=timestamp_end) {
          output_data$watertemp_C_adj[i] <- NA
          output_data$qaqc_code[i] <- "DISTURBANCE"
        }
      }
    }
  }

  names(output_data)[names(output_data) == "waterlevel_m" ] <-  var_waterlevel_m
  names(output_data)[names(output_data) == "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")
  names(output_data)[names(output_data) == "watertemp_C" ] <-  var_watertemp_C
  names(output_data)[names(output_data) ==  "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
  names(output_data)[names(output_data) ==  "do_mgl"] <-  var_do_mgl
  names(output_data)[names(output_data) ==  "do_mgl_adj"] <-  paste0(var_do_mgl, "_adj")
  names(output_data)[names(output_data) ==  "do_percsat"] <-  var_do_percsat
  names(output_data)[names(output_data) ==  "do_percsat_adj"] <-  paste0(var_do_percsat, "_adj")
  names(output_data)[names(output_data) == "conduct_uScm" ] <-  var_conduct_uScm
  names(output_data)[names(output_data) == "conduct_uScm_adj"] <-  paste0(var_conduct_uScm, "_adj")
  names(output_data)[names(output_data) ==  "airtemp_C"] <-  var_airtemp_C

  return(output_data)
}
