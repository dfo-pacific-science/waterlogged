#' Automatic waterlevel qaqc
#'
#' Change all negative water level values to zero, flag water temperatures above 19C as potentially dry, flag water temperatures below 0.3C as potentially in ice, and remove water temperature values below -1C.
#' @param input_data Dataframe with water level in metres
#' @param select_station site_station_code for desired logger station, supplied waterlevel data will be filtered by this code
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, water level and temperature values out of range are removed and flagged
#' @param var_waterlevel_m Name of variable column containing water level in meters
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_waterpress_kPa Name of column for water pressure (in kPa) in input data
#' @param var_airpress_kPa Name of column for air pressure (in kPa) in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @return list of 2; return site dataframe with qaqc adjustments; return plot for visual qaqc
#' @export

waterlevel_qaqc <- function(input_data, select_station, plot_only = FALSE, var_waterlevel_m = "waterlevel_m_U20", var_watertemp_C = "watertemp_C_U20", var_waterpress_kPa = "waterpress_kPa_U20", var_airpress_kPa = "airpress_kPa_U20_adj", var_airtemp_C = "airtemp_C_U20") {


  # ensures user is only processing one station at a time
  output_data <- input_data
  output_data <- output_data[output_data$site_station_code==select_station,]
  #remove rownumbers
  rownames(output_data) <- NULL

  names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"
  names(output_data)[names(output_data) == paste0(var_waterlevel_m, "_adj") ] <- "waterlevel_m_adj"
  names(output_data)[names(output_data) == paste0(var_watertemp_C, "_adj" )] <- "watertemp_C_adj"
  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
  names(output_data)[names(output_data) == var_airpress_kPa ] <- "airpress_kPa"
  names(output_data)[names(output_data) == var_waterpress_kPa ] <- "waterpress_kPa"


  if(plot_only == FALSE) {
    # new columns to fill
    output_data$waterlevel_m_adj <- output_data$waterlevel_m
    output_data$watertemp_C_adj <- output_data$watertemp_C
    output_data$wl_qaqc_adj <- NA
    output_data$wl_qaqc_code <- NA
    output_data$wl_qaqc_note <- NA

    for(i in 1:(length(output_data$timestamp)-1)){
      if(!is.na(output_data$waterlevel_m[i])){
        wl_diff <- output_data$waterlevel_m[i] -  output_data$waterlevel_m[i+1]
        if(abs(wl_diff) > 0.1) {
          output_data$wl_qaqc_code[i] <- "FLAG_DISTURBANCE" #if water level changes by more than 0.1m (10 cm) in 1 hour then add DISTURBANCE flag column (logger wire break or adjusted)
        }
      }
    }
    for(i in 1:length(output_data$timestamp)) {
      if(!is.na(output_data$waterlevel_m[i])){

        if(output_data$waterlevel_m[i]<0){
          output_data$wl_qaqc_note[i] <-  "corrected to zero"
          output_data$wl_qaqc_adj[i] <-  "REPLACED"
          output_data$wl_qaqc_code[i] <- "NEGATIVE_WL"
          output_data$waterlevel_m_adj[i] <- 0
        }
        if(!is.na(output_data$watertemp_C[i])){
          # if water pressure is equal to or less than air pressure add DRY flag column
          if(output_data$watertemp_C[i] > 19 | output_data$waterpress_kPa[i] <= output_data$airpress_kPa[i]) {
            output_data$wl_qaqc_code[i] <- "FLAG_DRY"
          }
          if(output_data$watertemp_C[i]<0.3){
            output_data$wl_qaqc_code[i] <- "FLAG_ICE"
          }
          if(output_data$watertemp_C[i]< -1){
            output_data$watertemp_C_adj[i] <- NA
            output_data$wl_qaqc_note[i] <-  "water temp out of range"
            output_data$wl_qaqc_adj[i] <-  "REMOVED"
            output_data$wl_qaqc_code[i] <- "LOGGER_ICE"

          }
          if(output_data$watertemp_C[i]<0 & output_data$watertemp_C[i]>-1){
            output_data$wl_qaqc_note[i] <-  "corrected to zero"
            output_data$wl_qaqc_adj[i] <-  "REPLACED"
            output_data$wl_qaqc_code[i] <- "NEGATIVE_WT"
            output_data$watertemp_C_adj[i] <- 0
          }
        } # end of if na water temp

      } # end of if na water level

    } # end of loop for qaqc flags

  } # end of plot only = false loop

  # ensure qaqc columns are writtable
  #output_data <- output_data %>% mutate(across(c(wl_qaqc_adj, wl_qaqc_code, wl_qaqc_note), as.character))


  p <- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = waterlevel_m, color = "waterlevel_m"), alpha = 0.5)+
    geom_line(aes(y = waterlevel_m_adj, color = "waterlevel_m_adj"))+
    geom_point(data = output_data %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG", wl_qaqc_code)),
               aes(y = 0, shape = as.factor(wl_qaqc_code)),
               color = "black")+
    scale_color_manual(values = c("waterlevel_m" = "#233d4d",
                                  "waterlevel_m_adj" = "#233d4d"))+
    scale_shape_manual(values = c("FLAG_ICE" = 1,
                                  "FLAG_DISTURBANCE" = 2,
                                  "FLAG_DRY" = 4))+
    labs(title = paste("QAQC for:", select_station),
         y = "Water level (m)",
         x = "Timestamp",
         color = "",
         shape = "") +
    theme_classic()


  q <- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = airtemp_C, color = "airtemp_C"), alpha = 0.5) +
    geom_line(aes(y = watertemp_C, color = "watertemp_C"), alpha = 0.5)+
    geom_line(aes(y = as.numeric(watertemp_C_adj), color = "watertemp_C_adj"))+
    scale_color_manual(values = c("watertemp_C" = "#619b8a" ,
                                  "watertemp_C_adj" = "#619b8a" ,
                                  "airtemp_C" = "#fe7f2d"))+
    labs(title =select_station,
         y = "Temperature (C)",
         x = "Timestamp",
         color = "",
         shape = "") +
    theme_classic()


  # return variables to user input naming
  names(output_data)[names(output_data) == "waterlevel_m"] <- var_waterlevel_m
  names(output_data)[names(output_data) ==  "watertemp_C"] <- var_watertemp_C
  names(output_data)[names(output_data) == "airtemp_C"] <- var_airtemp_C
  names(output_data)[names(output_data) == "airpress_kPa"] <- var_airpress_kPa
  names(output_data)[names(output_data) == "waterlevel_m_adj"] <- paste0(var_waterlevel_m, "_adj")
  names(output_data)[names(output_data) ==  "watertemp_C_adj"] <- paste0(var_watertemp_C, "_adj")
  names(output_data)[names(output_data) ==  "waterpress_kPa"] <-var_waterpress_kPa


  if(plot_only == FALSE) {
    print("Results returned as list: select [1] for waterlevel data with QAQC flags, [2] waterlevel data to visually assess for dry/ice/disturbance periods")
    return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
  }
  if(plot_only == TRUE){
    return(plotly::subplot(suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
  }
}
