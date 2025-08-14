#' Correct water level for spikes
#'
#' Remove water level values within a spike, and add the average change in water level per timestamp between start and end
#' @param timestamp_start Date and time at start of  spike; timestamp must be formatted the same as site_data
#' @param timestamp_end date and time end of spike; timestamp must be formatted the same as site_data
#' @param input_data The dataset with waterlevel in metres
#' @param select_station Unique identifier code for station
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @param reason_to_adjust Type of event that caused spike; Either "ice" or "disturbance"
#' @return Dataset where waterlevel is flattened within the time period defined as a spike, and qaqc_adj notes AVERAGE
#' @export

adjust_waterlevel_spike <- function(input_data, select_station,  timestamp_start, timestamp_end, reason_to_adjust, var_waterlevel_m = "waterlevel_m_U20", var_watertemp_C = "watertemp_C_U20") {

  waterlevel_data <- input_data
  waterlevel_data <- waterlevel_data[waterlevel_data$site_station_code==select_station,]

  #user input naming
  names(waterlevel_data)[names(waterlevel_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(waterlevel_data)[names(waterlevel_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"

  #remove rownumbers
  rownames(waterlevel_data) <- NULL

  # correct to average
  event_start <- round(waterlevel_data$waterlevel_m[waterlevel_data$timestamp == timestamp_start], digits = 3)
  event_end <- round(waterlevel_data$waterlevel_m[waterlevel_data$timestamp == timestamp_end], digits = 3)
  corr_event <- event_end-event_start
  n_event <- as.numeric(difftime(timestamp_end, timestamp_start, units = "hours"))
  corr_event <- corr_event/n_event

  for(i in 1:length(waterlevel_data$timestamp)) {

    # remove spikes
    if(waterlevel_data$timestamp[i] >= timestamp_start & waterlevel_data$timestamp[i] <= timestamp_end) {
      waterlevel_data$wl_qaqc_adj[i] <-  "AVERAGE"
      waterlevel_data$wl_qaqc_note[i] <- "spike flattened to average between before and after event"
      # use adj if full, if NA use water_level_m
      if(!is.na(waterlevel_data$waterlevel_m_adj[i])) {
        waterlevel_data$waterlevel_m_adj[i] <- waterlevel_data$waterlevel_m_adj[waterlevel_data$timestamp == timestamp_start]+corr_event*(i-which(waterlevel_data$timestamp == timestamp_start))
      } else {
        waterlevel_data$waterlevel_m[i] <- waterlevel_data$waterlevel_m[waterlevel_data$timestamp == timestamp_start]+corr_event*(i-which(waterlevel_data$timestamp == timestamp_start))
      }
      if(reason_to_adjust == "ice") {
        waterlevel_data$wl_qaqc_code[i] <- "LOGGER_ICE"
      }
      if(reason_to_adjust == "disturbance") {
        waterlevel_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
      }
      if(reason_to_adjust != "ice" & reason_to_adjust != "disturbance") {
        print("Uh oh! reason_to_adjust not recognized. Must be either ice or disturbance. Silly Goose!")
      }

    }
  }
  # reverse user input naming
  names(waterlevel_data)[names(waterlevel_data) ==  "waterlevel_m"] <-  var_waterlevel_m
  names(waterlevel_data)[names(waterlevel_data) ==  "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")

  output_data <- waterlevel_data

  return(output_data)
}
