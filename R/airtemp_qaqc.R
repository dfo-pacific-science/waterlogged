#' Automatic air temperature qaqc
#'
#' Format air-temperature data for output as "clean"
#' @param input_data raw data csv from air temperature TidbiT loggers
#' @param select_station site_station_code for desired logger station, supplied air temperature data will be filtered by this code
#' @param var_airtemp_C The variable for air temperature in degrees Celsius
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, air temperature values out of range are removed and flagged
#' @param return_plots Logical; if \code{TRUE} (default), an interactive
#'   \pkg{plotly} chart is included in the returned list.  Set to
#'   \code{FALSE} to skip plot creation (faster, useful in Shiny or
#'   batch pipelines).
#' @return list of 2; return site dataframe with qaqc adjustments; return plot for visual qaqc
#' @export

airtemp_qaqc <- function(input_data, select_station,  var_airtemp_C = "airtemp_C_TidbiT", plot_only = FALSE, return_plots = TRUE) {

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

  if (return_plots) {
    p<- ggplot2::ggplot(data=output_data, ggplot2::aes(x = timestamp))+
      ggplot2::geom_line(ggplot2::aes(y = airtemp_C, color = "airtemp_C"))+
      ggplot2::geom_line(ggplot2::aes(y = airtemp_C_adj, color = "airtemp_C_adj"))+
      ggplot2::geom_point(data = output_data |> tidyr::drop_na(at_qaqc_code), ggplot2::aes(x = timestamp, y = airtemp_C), color = "red", shape = 4)+
      ggplot2::scale_color_manual(values = c("airtemp_C" = "orange",
                                    "airtemp_C_adj" = "yellow"))+
      ggplot2::scale_y_continuous(limits = c(-38,41))+
      ggplot2::labs(title =select_station,
           y = "Air temperature (C)",
           x = "Timestamp") +
      ggplot2::theme_classic()
    plot_out <- plotly::ggplotly(p)
  } else {
    plot_out <- NULL
  }

  if(plot_only == FALSE){
    # reverse local name changes
    output_data <- output_data[, c("site_station_code", "sn", "timestamp", "airtemp_C", "airtemp_C_adj", "at_qaqc_code", "at_qaqc_adj", "at_qaqc_note")]


    names(output_data)[names(output_data) == "airtemp_C" ] <- var_airtemp_C
    names(output_data)[names(output_data) == "airtemp_C_adj" ] <- paste0(var_airtemp_C, "_adj")



    message("Results returned as list: select [1] for data with airtemp_qaqc flags, [2] plots to visually assess for dry/ice/disturbance periods")
    return(list(output_data, plot_out))

  } else(return(plot_out))

}
