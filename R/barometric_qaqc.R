#' Automatic barometric qaqc
#'
#' Format barometric data for output as "clean"
#' @param input_data raw data csv from barometric U20 loggers
#' @param select_station site_station_code for desired logger station, supplied barometric data will be filtered by this code
#' @param var_airtemp_C The variable for air temperature in degrees Celsius
#' @param var_airpress_kPa The variable for barometric pressure in kPa
#' @param climate_station name of desired climate station to check air temperature values. One of FORT NELSON, PRINCE GEORGE, BLUE RIVER, KAMLOOPS, or VANCOUVER.
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, barometric and temperature values out of range are removed and flagged
#' @return list of 2; return site dataframe with qaqc adjustments; return plot for visual qaqc
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly

barometric_qaqc <- function(input_data, select_station,  var_airpress_kPa = "airpress_kPa_U20", var_airtemp_C = "airtemp_C_U20", climate_station = "BLUE RIVER", plot_only = FALSE) {

  library(dplyr)
  library(ggplot2)
  library(plotly)

  # ensures user is only processing one station at a time
  output_data <- input_data[input_data$site_station_code == select_station,]
  rownames(output_data) <- NULL

  if(length(output_data$site_station_code)==0) {print(paste("ERROR: No barometric data found for", select_station, ". Make sure spelling and case match site_station_code column in input_data."))}

  names(output_data)[names(output_data) == var_airpress_kPa ] <- "airpress_kPa"
  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"


  if(plot_only == FALSE) {
    # make qaqc adjustment column
    # extreme temperatures Prince George
    #https://climate.weather.gc.ca/climate_normals/results_1991_2020_e.html?searchType=stnProv&lstProvince=BC&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongMin=0&txtCentralLongSec=0&stnID=346000000&dispBack=0
    output_data$baro_qaqc_code <- NA
    output_data$baro_qaqc_adj <- NA
    output_data$baro_qaqc_note <- NA

    climate_station <- toupper(climate_station)

    month <-  c(1:12)

    if(climate_station=="KAMLOOPS"){
      airtemp_extmax_C <- c(15.6,17.0,23.3,	30.7,	36.5,	39.1,	40.4,	40.2,	34.9,	28.1,	21.3,	15.7)
      airtemp_extmin_C <-c(-28.6,	-24.5,	-18.1,	-8.8,	-5.0,	3.6,	5.1,	4.9,	-0.6,	-12.9,	-24.9,	-28.0)
    }
    if(climate_station=="PRINCE GEORGE"){
      airtemp_extmax_C <- c(11.3,12.4,19.9,25.4,31.0,33.2,35.6,33.7,30.1,24.9,18.8,12.0)
      airtemp_extmin_C <- c(-46.1,-38.2,-32.9,-19.7,-6.4,-1.6,-0.9,-3.8,-8.2,-20.7,-33.3,-40.3)
    }
    if(climate_station=="VANCOUVER"){
      airtemp_extmax_C <- c(14.8,	16.0,	18.5,	22.1,	29.0,	30.2,	34.4,	29.8,	27.8,	23.7,	17.0,	14.3)
      airtemp_extmin_C <- c(-14.1,	-9.9,	-5.9,	-2.1,	2.0,	6.3,	8.5,	9.3,	3.6,	-4.7,	-12.0,	-15.2)
    }
    if(climate_station=="FORT NELSON"){
      airtemp_extmax_C <- c(11.6,	13.0,	17.6,	26.7,	31.5,	34.6,	34.5,	33.3,	28.9,	26.7,	12.2,	8.6)
      airtemp_extmin_C <- c(-47.3,	-43.2,	-38.7,	-26.2,	-14.0,	-1.5,	1.6,	-4.5,	-10.0,	-26.2,	-37.0,	-44.3)
    }
    if(climate_station=="BLUE RIVER"){
      airtemp_extmax_C <- c(10.1,	11.0,	19.4,	26.5,	32.2,	36.0,	36.6,	36.5,	33.6,	25.0,	16.5,	7.8	)
      airtemp_extmin_C <- c(-44.0,	-37.8,	-27.0,	-14.5,	-6.8,	-2.4,	-0.6,	-3.5,	-5.6,	-18.8,	-32.2,	-41.2)
    }
    if(climate_station!="BLUE RIVER"&climate_station!="KAMLOOPS"&climate_station!="VANCOUVER"&climate_station!="PRINCE GEORGE"&climate_station!="FORT NELSON"){
      print("Error: Climate station not found. Please select one of: FORT NELSON, PRINCE GEORGE, BLUE RIVER, KAMLOOPS, or VANCOUVER.")
    }

    ext_temp <- as.data.frame(cbind(month, airtemp_extmin_C, airtemp_extmax_C))

    # make new column for adjustment values
    output_data$airtemp_C_adj <- output_data$airtemp_C
    output_data$airpress_kPa_adj <- output_data$airpress_kPa

    for(i in 1:length(output_data$timestamp)) {

      month_i <- month(output_data$timestamp[i])
      min_temp_i <- ext_temp$airtemp_extmin_C[ext_temp$month == month_i]
      max_temp_i <- ext_temp$airtemp_extmax_C[ext_temp$month == month_i]

      if(output_data$airtemp_C[i]<=min_temp_i){
        output_data$baro_qaqc_note[i] <- paste("air temp below", climate_station,"Temp. Extreme 1991-2020")
        output_data$baro_qaqc_code[i] <- "TEMP_FLAG"
      }

      if(output_data$airtemp_C[i]>=max_temp_i){
        output_data$baro_qaqc_note[i] <- paste("air temp above", climate_station,"Temp. Extreme 1991-2020")
        output_data$baro_qaqc_code[i] <- "TEMP_FLAG"
      }

    }

  }

  # plot baro baro_qaqc adjustments
  mean_press <- mean(output_data$airpress_kPa, na.rm = TRUE)

  p<- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = airtemp_C, color = "airtemp_C"))+
    geom_line(aes(y = airtemp_C_adj, color = "airtemp_C_adj"))+
    geom_line(aes(y = (airpress_kPa-mean_press)*10, color = "airpress_kPa")) +
    geom_line(aes(y = (airpress_kPa_adj-mean_press)*10, color = "airpress_kPa_adj")) +
    geom_point(data = output_data %>% drop_na(baro_qaqc_code), aes(x = timestamp, y = airtemp_C), color = "red", shape = 4)+
    scale_color_manual(values = c("airtemp_C" = "orange",
                                  "airtemp_C_adj" = "yellow",
                                  "airpress_kPa" = "black",
                                  "airpress_kPa_adj" = "darkgreen"))+
    scale_y_continuous(limits = c(-38,41))+
    labs(title =select_station,
         y = "Air temperature (C)",
         x = "Timestamp") +
    theme_classic()

  if(plot_only == FALSE){
    # reverse local name changes
    output_data <- output_data[, c("site_station_code", "sn", "timestamp", "airpress_kPa", "airpress_kPa_adj", "airtemp_C", "airtemp_C_adj", "baro_qaqc_code", "baro_qaqc_adj", "baro_qaqc_note")]

    names(output_data)[names(output_data) == "airpress_kPa" ] <- var_airpress_kPa
    names(output_data)[names(output_data) == "airtemp_C" ] <- var_airtemp_C
    names(output_data)[names(output_data) == "airtemp_C_adj" ] <- paste0(var_airtemp_C, "_adj")
    names(output_data)[names(output_data) == "airpress_kPa_adj" ] <- paste0(var_airpress_kPa, "_adj")


    print("Results returned as list: select [1] for data with baro_qaqc flags, [2] plots to visually assess for dry/ice/disturbance periods")
    return(list(output_data, ggplotly(p)))

  } else(return(ggplotly(p)))

}
