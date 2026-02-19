#' Adjust reference water level
#'
#' Adjust water level to a new zero/reference level by shifting all values up or down. This is useful if a stage is observed to be dry (water level = 0) but water level is recorded as a positive or negative value and the readings need to be shifted up or down.
#' @param timestamp_start Date and time of start of section for new zero/ reference level - this will be the new zero measurement for water level; Timestamp must be formatted the same as waterlevel_data.
#' @param timestamp_end Date and time of end of section for new zero/ reference level - this will be the new zero measurement for water level; Timestamp must be formatted the same as waterlevel_data
#' @param input_data The dataset with waterlevel in metres
#' @param select_station Unique identifier code for station
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @param shift_up_or_down Use up to add the average water level between timestamp_start and timestamp_end to all data points. Us down to subtract the average water level between timestamp_start and timestamp_end to all data points.
#' @return Dataset where waterlevel_m_adj are shifted up or down to desired correction, qaqca_code set to MANUAL_ERROR, and qaqc_adj states numerical change to raw value
#' @export

adjust_waterlevel_zero <- function(input_data, select_station, timestamp_start, timestamp_end, shift_up_or_down, var_waterlevel_m = "waterlevel_m_U20") {

 output_data <- input_data
  output_data <- output_data[output_data$site_station_code==select_station,]
  #remove rownumbers
  rownames(output_data) <- NULL

  #user input naming
  names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"


  # get avg dry water level for selected time range
  corr_zero <- output_data |>
    dplyr::filter(timestamp > timestamp_start & timestamp < timestamp_end)
  corr_zero <- round(abs(mean(corr_zero$waterlevel_m, na.rm = TRUE)), digits = 3)


  # correct error in stage measurement for all obs at site
  for(i in 1:length(output_data$timestamp)) {
    output_data$wl_qaqc_code[i] <- "SHIFT_CORRECTION"
    # use stage or stage
    # use adj if full, if NA use water_level_m
    if(shift_up_or_down == "up") {
      output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i]+corr_zero+0.1 #logger height 0.1 m above bottom
      output_data$wl_qaqc_adj[i] <-  paste((corr_zero), "added to WL")
    }
    if(shift_up_or_down == "down") {
      output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i]-corr_zero+0.1 #logger height 0.1 m above bottom
      output_data$wl_qaqc_adj[i] <-  paste((corr_zero), "subtracted from WL")
    }
  }

  # revert naming
  names(output_data)[names(output_data) == "waterlevel_m"] <-  var_waterlevel_m
  names(output_data)[names(output_data) ==  "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")

  return(output_data)
}
