#' Automatic watertemp qaqc
#'
#'Flag water temperatures above 19C as potentially dry, flag water temperatures below 0.3C as potentially in ice, correct temperature between -1 and 0 as 0, and remove water temperature values below -1C.
#' @param input_data Dataframe with water temperature in
#' @param select_station site_station_code for desired logger station, supplied waterlevel data will be filtered by this code
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, water level and temperature values out of range are removed and flagged
#' @return list of 2; return site dataframe with qaqc adjustments; return plot for visual qaqc

watertemp_qaqc <- function(input_data, select_station, var_airtemp_C ="airtemp_C_U20", var_watertemp_C = "watertemp_C_U20", plot_only = FALSE) {

  # ensures user is only processing one station at a time
  output_data <- input_data
  output_data <- output_data[output_data$site_station_code==select_station,]
  #remove rownumbers
  rownames(output_data) <- NULL

  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
  names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"

  if(plot_only == FALSE) {

    output_data$watertemp_C_adj <- output_data$watertemp_C

    for(i in 1:length(output_data$timestamp)) {

      if(output_data$watertemp_C[i] > 19) {
        output_data$qaqc_code[i] <- "FLAG_DRY"
      }

      if(output_data$watertemp_C[i]<0.3){
        output_data$qaqc_code[i] <- "FLAG_ICE"
      }
      if(output_data$watertemp_C[i]< -1){
        output_data$watertemp_C_adj[i] <- NA
        output_data$qaqc_note[i] <-  "water temp out of range"
        output_data$qaqc_code[i] <- "LOGGER_ICE"

      }

      if(!is.na(output_data$airtemp_C[i])) {
        if(abs(output_data$watertemp_C[i]-output_data$airtemp_C[i])<2) {
          output_data$qaqc_code[i] <- "FLAG_DRY"
        }

        if((output_data$airtemp_C[i] - output_data$watertemp_C[i])<0) {
          output_data$qaqc_code[i] <- "FLAG_DRY"
        }
      }


    }

    p<- ggplot(data=output_data)+
      geom_line(aes(x = timestamp, y = watertemp_C, color = "watertemp_C"))+
      geom_line(aes(x = timestamp, y = watertemp_C_adj, color = "watertemp_C_adj"))+
      geom_line(aes(x = timestamp, y = airtemp_C, color = "airtemp_C"), alpha = 0.2)+
      scale_color_manual(values = c("watertemp_C" = "yellow",
                                    "watertemp_C_adj" = "orange",
                                    "airtemp_C" = "red"))+
      scale_y_continuous(limits = c(-5,35))+
      labs(title =select_station,
           y = "Water temperature (C)",
           x = "Timestamp (PDT)") +
      theme_classic()

    # revert naming convention
    names(output_data)[names(output_data) ==  "watertemp_C"] <- var_watertemp_C
    names(output_data)[names(output_data) ==  "airtemp_C"] <- var_airtemp_C

    return(list(ouput_data, ggplotly(p)))

  }

  if(plot_only == TRUE) {

    output_data <- output_data %>% filter(site_station_code == select_station)


    p<- ggplot(data=output_data)+
      geom_line(aes(x = timestamp, y = watertemp_C, color = "watertemp_C"))+
      geom_line(aes(x = timestamp, y = as.numeric(watertemp_C_adj), color = "watertemp_C_adj"))+
      geom_line(aes(x = timestamp, y = airtemp_C_U20, color = "airtemp_C_U20"), alpha = 0.2) +
      scale_color_manual(values = c(
        "watertemp_C" = "yellow",
        "watertemp_C_adj" = "orange",
        "airtemp_C_U20" = "red"))+
      scale_y_continuous(limits = c(-5,35))+
      labs(title =select_station,
           y = "Water temperature (C)",
           x = "Timestamp (PDT)") +
      theme_classic()

    return(ggplotly(p))

  }

}
