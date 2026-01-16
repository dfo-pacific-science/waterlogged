#' Automatic air temperature qaqc
#'
#' Format air-temperature data for output as "clean"
#' @param input_data raw data csv from air temperature TidbiT loggers
#' @param select_station site_station_code for desired logger station, supplied air temperature data will be filtered by this code
#' @param var_airtemp_C The variable for air temperature in degrees Celsius
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, air temperature values out of range are removed and flagged
#' @return list of 2; return site dataframe with qaqc adjustments; return plot for visual qaqc
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly

airtemp_qaqc <- function(input_data, select_station,  var_airtemp_C = "airtemp_C_TidbiT", plot_only = FALSE) {

  library(dplyr)
  library(ggplot2)
  library(plotly)

  # ensures user is only processing one station at a time
  output_data <- input_data[input_data$site_station_code == select_station,]
  rownames(output_data) <- NULL

  if(length(output_data$site_station_code)==0) {print(paste("ERROR: No air temperature data found for", select_station, ". Make sure spelling and case match site_station_code column in input_data."))}

  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"


  if(plot_only == FALSE) {
    # make qaqc adjustment column
    output_data$at_qaqc_code <- NA
    output_data$at_qaqc_adj <- NA
    output_data$at_qaqc_note <- NA


    # make new column for adjustment values
    output_data$airtemp_C_adj <- output_data$airtemp_C

    for(i in 1:length(output_data$timestamp)) {


      if(output_data$airtemp_C[i]<= -40){
        output_data$at_qaqc_note[i] <- paste("air temp below -40C")
        output_data$at_qaqc_code[i] <- "TEMP_FLAG"
      }

      if(output_data$airtemp_C[i]>= 40){
        output_data$at_qaqc_note[i] <- paste("air temp above 40C")
        output_data$at_qaqc_code[i] <- "TEMP_FLAG"
      }

    }

  }

  # plot airtemp airtemp_qaqc adjustments


  p<- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = airtemp_C, color = "airtemp_C"))+
    geom_line(aes(y = airtemp_C_adj, color = "airtemp_C_adj"))+
    geom_point(data = output_data %>% drop_na(at_qaqc_code), aes(x = timestamp, y = airtemp_C), color = "red", shape = 4)+
    scale_color_manual(values = c("airtemp_C" = "orange",
                                  "airtemp_C_adj" = "yellow"))+
    labs(title =select_station,
         y = "Air temperature (C)",
         x = "Timestamp") +
    theme_classic()

  if(plot_only == FALSE){
    # reverse local name changes
    output_data <- output_data[, c("site_station_code", "sn", "timestamp", "airtemp_C", "airtemp_C_adj", "at_qaqc_code", "at_qaqc_adj", "at_qaqc_note")]


    names(output_data)[names(output_data) == "airtemp_C" ] <- var_airtemp_C
    names(output_data)[names(output_data) == "airtemp_C_adj" ] <- paste0(var_airtemp_C, "_adj")



    print("Results returned as list: select [1] for data with airtemp_qaqc flags, [2] plots to visually assess for dry/ice/disturbance periods")
    return(list(output_data, ggplotly(p)))

  } else(return(ggplotly(p)))

}
