#' Conductivity QAQC
#'
#' Perform preliminary QAQC on conductivity data
#' @param input_data raw data csv from barometric U20 loggers
#' @param select_station site_station_code for desired logger station, supplied barometric data will be filtered by this code
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, values out of range are removed and flagged
#' @param var_conduct_uScm Name of column for conductivity (uScm) in input data
#' @param var_watertemp_C_1 Name of column for water temperature (in degrees Celcius) in input data as measured by the conductivity logger
#' @param var_watertemp_C_2 Optional. Name of column for water temperature (in degrees Celcius) in input data as measured by a secondary logger
#' @param var_airtemp_C Optional. Name of column for air temperature in degrees Celsius in input data.
#' @param var_waterlevel_m Optional. Name of variable column containing water level in meters in input data.
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly


conductivity_qaqc <- function(input_data,
                              select_station,
                              plot_only = FALSE,
                              var_conduct_uScm = "conduct_uScm_U24",
                              var_watertemp_C_1 = "watertemp_C_U24",
                              var_watertemp_C_2 = "none",
                              var_airtemp_C = "none",
                              var_waterlevel_m = "none"){

  # ensures select_station is valid
  if (!(select_station %in% input_data$site_station_code)){
    stop("Invalid station. Ensure you have entered an existing site_station_code.")
  }

  # ensures user is only processing one station at a time
  output_data <- input_data
  output_data <- output_data[output_data$site_station_code==select_station,]
  # remove rownumbers
  rownames(output_data) <- NULL
  # # ensure qaqc columns are writeable
  # output_data <- output_data %>% mutate(across(c(co_qaqc_adj, co_qaqc_code, co_qaqc_note), as.character))

  # rename input columns to local columns
  names(output_data)[names(output_data) == var_conduct_uScm ] <- "conduct_uScm"
  names(output_data)[names(output_data) == var_watertemp_C_1 ] <- "watertemp_C_1"
  names(output_data)[names(output_data) == paste0(var_conduct_uScm, "_adj") ] <- "conduct_uScm_adj"
  names(output_data)[names(output_data) == paste0(var_watertemp_C_1, "_adj" )] <- "watertemp_C_1_adj"

  # ensure timestamp column is properly formatted
  output_data$timestamp <- as.POSIXct(output_data$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")

  if(var_airtemp_C != "none"){
    names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
  }

  if(var_waterlevel_m != "none") {
    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  }

  if(var_watertemp_C_2 != "none") {
    names(output_data)[names(output_data) == var_watertemp_C_2 ] <- "watertemp_C_2"
  }

  if(plot_only == FALSE){

    # if co_qaqc_code column does not already exist, make a new one
    if(length(output_data$co_qaqc_code)==0) {
      output_data$co_qaqc_code <- NA
    }
    if(length(output_data$co_qaqc_adj)==0) {
      output_data$co_qaqc_adj <- NA
    }
    if(length(output_data$co_qaqc_note)==0) {
      output_data$co_qaqc_note <- NA
    }

    output_data$conduct_uScm_adj <- output_data$conduct_uScm
    output_data$watertemp_C_1_adj <- output_data$watertemp_C_1

    # Disturbance flag if conductivity changes by more than 200 uS/cm in an hour
    for(i in 1:(length(output_data$timestamp)-1)) {
      if(!is.na(output_data$conduct_uScm[i])) {
        cdiff <- output_data$conduct_uScm[i] - output_data$conduct_uScm[i+1]
        if(abs(cdiff) > 200) {
          output_data$co_qaqc_code[i] <- "FLAG_DISTURBANCE"
        }
      }
    }

    # Additional flags
    for(i in 1:(length(output_data$timestamp))) {
      if(!is.na(output_data$conduct_uScm[i])) {

        # if water temperature is less than 5C, then flag as outside of range
        if(output_data$watertemp_C_1[i] < 5){
          output_data$co_qaqc_code[i] <- "WT_BELOW_RANGE"
        }

        # if water temperature is less than 0.1C, then flag as ice
        if(output_data$watertemp_C_1[i] < 0.1){
          output_data$co_qaqc_code[i] <- "FLAG_ICE"
        }

        # if water temperature is between 0C and -1C, then change to 0
        if(output_data$watertemp_C_1[i]<0 & output_data$watertemp_C_1[i]>-1){
          output_data$co_qaqc_note[i] <-  "corrected to zero"
          output_data$co_qaqc_adj[i] <-  "REPLACED"
          output_data$co_qaqc_code[i] <- "NEGATIVE_WT"
          output_data$watertemp_C_1_adj[i] <- 0
        }

        # if water temperature is less than -2C, then remove
        if(output_data$watertemp_C_1[i]< -2){
          output_data$watertemp_C_1_adj[i] <- NA
          output_data$co_qaqc_note[i] <- "water temp out of range"
          output_data$co_qaqc_adj[i] <-  "REMOVED"
          output_data$co_qaqc_code[i] <- "LOGGER_ICE"
        }

        if(is.na(var_airtemp_C)){
          # if conductivity reads less than 10 uS/cm, then flag as dry
          # only applies if there is no air temp column
          if(output_data$conduct_uScm[i] < 10) {
            output_data$co_qaqc_code[i] <- "FLAG_DRY"
          }
        }

      } # end of if data not NA
    } # end of additional flags

    # Flag dry periods using air temperature
    if(var_airtemp_C != "none"){
      # calculate air to water temp diff
      output_data$air_watertemp_diff <- abs(output_data$airtemp_C - output_data$watertemp_C_1)

      # if water temp within 2 degrees of air temp and < 10 uS/cm for 6 consecutive hours:
      print("Determining dry periods...")
      counter <- 0
      threshold <- 6

      for (i in 1:length(output_data$timestamp)) {
        if (output_data$air_watertemp_diff[i] <= 2 && output_data$conduct_uScm[i] < 10) {
          counter <- counter + 1
        } else {
          counter <- 0
        }

        if (counter >= threshold) {
          output_data$co_qaqc_code[(i - threshold):i] <- "FLAG_DRY"
        }
      }
    } # end of air temp dry flag

  } # end of plot_only = FALSE


  ## PLOT DATA
  ## Conductivity
  p <- ggplot(output_data, aes(x = timestamp)) +
    geom_line(aes(y = conduct_uScm, colour = "conduct_uScm"), alpha = 0.5)+
    geom_line(aes(y = conduct_uScm_adj, colour = "conduct_uScm_adj")) +
    geom_point(data = output_data %>% drop_na(co_qaqc_code) %>% filter(grepl("FLAG", co_qaqc_code)),
               aes(y = 0, shape = as.factor(co_qaqc_code)),
               color = "black") +
    scale_colour_manual(values = c("conduct_uScm" = "#00008B",
                                   "conduct_uScm_adj" = "#00008B")) +
    scale_shape_manual(values = c("FLAG_ICE" = 1,
                                  "FLAG_DISTURBANCE" = 2,
                                  "FLAG_DRY" = 4)) +
    labs(title = paste("QAQC for:", select_station),
         y = "Conductivity (uS/cm)",
         x = "Timestamp",
         colour = "",
         shape = "") +
    theme_classic()

  ## Temperature
  q <- ggplot(output_data, aes(x = timestamp)) +
    geom_line(aes(y = watertemp_C_1, colour = "watertemp_C_1"), alpha = 0.5) +
    geom_line(aes(y = watertemp_C_1_adj, colour = "watertemp_C_1_adj"))

  line_colours <- c("watertemp_C_1"     = "#619b8a",
                    "watertemp_C_1_adj" = "#619b8a")


  # Plot if the data contain air temperature
  if (var_airtemp_C != "none") {
    q <- q +
      geom_line(aes(y = airtemp_C, colour = "airtemp_C"), linewidth = 0.3, alpha = 0.5)

    line_colours["airtemp_C"] <- "#fe7f2d"
  }

  # Plot if the data contains secondary water temperature
  if (var_watertemp_C_2 != "none") {
    q <- q +
      geom_line(aes(y = watertemp_C_2, colour = "watertemp_C_2"), alpha = 0.5)

    line_colours["watertemp_C_2"] <- "#FDB840"
  }

  q <- q +
    scale_colour_manual(values = line_colours) +
    labs(title = select_station,
         y = "Temperature (C)",
         x = "Timestamp",
         colour = "") +
    theme_classic()

  # Plot if data contains water level
  # Use the QAQC codes from the water level data instead of the conductivity data
  if (var_waterlevel_m != "none"){

    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"

    r <- ggplot(output_data, aes(x = timestamp)) +
      geom_line(data = output_data, aes(y = waterlevel_m, colour = "waterlevel_m"), linewidth = 0.3) +
      geom_point(data = output_data %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG|LOGGER", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black") +
      scale_color_manual(values = c("waterlevel_m" = "#233d4d"))+
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "LOGGER_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "LOGGER_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4,
                                    "LOGGER_DRY" = 4)) +
      labs(title = select_station,
           y = "Waterl level (m)",
           x = "Timestamp",
           colour = "",
           shape = "") +
      theme_classic()
  }

  # return variables to user input naming
  names(output_data)[names(output_data) == "conduct_uScm"] <- var_conduct_uScm
  names(output_data)[names(output_data) == "watertemp_C_1"] <- var_watertemp_C_1
  names(output_data)[names(output_data) == "conduct_uScm_adj"] <- paste0(var_conduct_uScm, "_adj")
  names(output_data)[names(output_data) == "watertemp_C_1_adj"] <- paste0(var_watertemp_C_1, "_adj")
  if ("airtemp_C" %in% names(output_data)) names(output_data)[names(output_data) == "airtemp_C"] <- var_airtemp_C
  if ("waterlevel_m" %in% names(output_data)) names(output_data)[names(output_data) == "waterlevel_m"] <- var_waterlevel_m
  if ("watertemp_C_2" %in% names(output_data)) names(output_data)[names(output_data) == "watertemp_C_2"] <- var_watertemp_C_2

  if(plot_only == FALSE) {
    print("Results returned as list: select [1] for conductivity data with QAQC flags, [2] conductivity data to visually assess for dry/ice/disturbance periods")
    if (var_waterlevel_m != "none"){
      return(list(output_data,
                  plotly::subplot(suppressWarnings(ggplotly(p)),
                                  suppressWarnings(ggplotly(r)),
                                  suppressWarnings(ggplotly(q)),
                                  heights = c(0.3, 0.3, 0.3), nrows = 3, shareX = TRUE, margin = 0.05)))
    }
    else{
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(p)),
                                               suppressWarnings(ggplotly(q)),
                                               heights = c(0.5, 0.5), nrows = 2, shareX = TRUE, margin = 0.05)))
    }
  }
  if(plot_only == TRUE){
    if (var_waterlevel_m != "none"){
      return(plotly::subplot(suppressWarnings(ggplotly(p)),
                             suppressWarnings(ggplotly(r)),
                             suppressWarnings(ggplotly(q)),
                             heights = c(0.3, 0.3, 0.3), nrows = 3, shareX = TRUE, margin = 0.05))
    }
    else{
    return(plotly::subplot(suppressWarnings(ggplotly(p)),
                           suppressWarnings(ggplotly(q)),
                           heights = c(0.5, 0.5), nrows = 2, shareX = TRUE, margin = 0.05))
    }
  }
}
