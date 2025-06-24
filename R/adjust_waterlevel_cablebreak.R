#' Correct water level for cable break
#'
#' Adjust water level up or down following a cable break
#' @param input_data Data for desired site_station_code
#' @param timestamp_end Date and time of first observation following cable break; timestamp must be formatted the same as site_data
#' @param timestamp_start Date and time of first observation before cable break; timestamp must be formatted the same as site_data
#' @param select_station Unique identifier code for station
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @return Dataset where waterlevel values are shifted up or down after the cable break, qaqc_code set to LOGGER_DISTURBANCE, qaqc_adj shows the value adjusted

adjust_waterlevel_cablebreak <- function(input_data, select_station, timestamp_start, timestamp_end, var_waterlevel_m = "waterlevel_m_U20") {

  output_data<- input_data
  output_data <- output_data[output_data$site_station_code==select_station,]

  #user input naming
  names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"


  #remove rownumbers
  rownames(output_data) <- NULL

  # ID timestamp of break, find change in water level, subtract from entried following break
  before_break <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_start], digits = 3)
  after_break <- round(output_data$waterlevel_m[output_data$timestamp == timestamp_end], digits = 3)
  corr_break <- after_break-before_break

  for(i in 1:length(output_data$timestamp)) {
    # corect logger cable break
    if(output_data$timestamp[i] >= timestamp_end) {
      output_data$wl_qaqc_adj[i] <-  paste(corr_break, "added")
      output_data$wl_qaqc_code[i] <- "LOGGER_DISTURBANCE"
      output_data$wl_qaqc_note[i] <- "logger cable break"

      # use adj if full, if NA use water_level_m
      if(!is.na(output_data$waterlevel_m_adj[i])) {
        output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m_adj[i]-corr_break
      } else {
        output_data$waterlevel_m_adj[i] <- output_data$waterlevel_m[i]-corr_break
      }

    }
  }
  # reverse user input naming
  names(output_data)[names(output_data) ==  "waterlevel_m"] <-  var_waterlevel_m
  names(output_data)[names(output_data) ==  "waterlevel_m_adj"] <-  paste0(var_waterlevel_m, "_adj")

  return(output_data)
  }

