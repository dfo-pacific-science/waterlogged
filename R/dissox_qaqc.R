#' Dissolved oxygen QAQC
#'
#' Perform preliminary QAQC on dissolved oxygen data
#' @param input_data The dataset containing raw dissolved oxygen in mg/L, percent saturation, temperature in degrees Celcius and air temperature from barometric logger in degrees Celcius
#' @param select_station Unique identifier code for station
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, water level and temperature values out of range are removed and flagged
#' @param var_waterlevel_m OPTIONAL Name of variable column containing water level in meters
#' @param var_watertemp_C_1 Name of column for water temperature (in degrees Celcius) from dissolved oxygen logger in input data
#' @param var_watertemp_C_2 OPTIONAL Name of column for water temperature (in degrees Celcius) from waterlevel logger in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @param var_do_percsat Name of column for dissolved oxygen in percent air saturation in input data
#' @param var_do_mgl Name of column for dissolved oxygen in mg per litre in input data
#' @return Dataset of dissolved oxygen and waterlevel filtered for error values and flagged for ice, dry and disturbance

dissox_qaqc <- function(input_data, select_station, plot_only = FALSE, var_do_mgl = "do_mgl_U26", var_do_percsat = "do_percsat_U26", var_watertemp_C_1 = "watertemp_C_U26", var_waterlevel_m = "none", var_watertemp_C_2 = "none", var_airtemp_C = "airtemp_C_U20") {

  # ensures user is only processing one station at a time
  output_data <- input_data
  output_data <- output_data[output_data$site_station_code==select_station,]
  #remove rownumbers
  rownames(output_data) <- NULL
  # ensure qaqc columns are writtable
  output_data <- output_data %>% mutate(across(c(do_qaqc_adj, do_qaqc_code, do_qaqc_note), as.character))

  names(output_data)[names(output_data) == var_do_mgl ] <- "do_mgl"
  names(output_data)[names(output_data) == var_watertemp_C_1 ] <- "watertemp_C_1"
  names(output_data)[names(output_data) == var_do_percsat ] <- "do_percsat"
  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"

  if(var_waterlevel_m != "none") {
    names(output_data)[names(output_data) == var_waterlevel_m ] <- "waterlevel_m"
  }

  if(var_watertemp_C_2 != "none") {
    names(output_data)[names(output_data) == var_watertemp_C_2 ] <- "watertemp_C_2"
  }
  # join

  if(plot_only == FALSE) {

    # make new adj columns
    if(length(output_data$do_mgl_adj)==0) {
      output_data$do_mgl_adj <- output_data$do_mgl
    }
    if(length(output_data$do_percsat_adj)==0) {
      output_data$do_percsat_adj <- output_data$do_percsat
    }
    if(length(output_data$watertemp_C_1_adj)==0) {
      output_data$watertemp_C_1_adj <- output_data$watertemp_C_1
    }
    # if qaqc_code column does not already exist, make a new one
    if(length(output_data$do_qaqc_code)==0) {
      output_data$do_qaqc_code <- NA
    }
    if(length(output_data$do_qaqc_adj)==0) {
      output_data$do_qaqc_adj <- NA
    }
    if(length(output_data$do_qaqc_note)==0) {
      output_data$do_qaqc_note <- NA
    }

    #calc air to water temp diff
    output_data$air_watertemp_diff <- output_data$airtemp_C - output_data$watertemp_C_1

    # if second watertemp value assigned, calc diff between loggers
    if(var_watertemp_C_2!="none") {
      output_data$watertemp_diff <- output_data$watertemp_C_1 - output_data$watertemp_C_2
    }

    for(i in 1:length(output_data$timestamp)) {
      #DO outliers
      if(!is.na(output_data$do_mgl[i])) {
        # drop error codes
        if(output_data$do_mgl[i] == "-888.88") {
          output_data$do_qaqc_code[i] <- "DO_ERROR"
          output_data$do_qaqc_adj[i] <- "REMOVED"
          output_data$do_qaqc_note[i] <- "Logger error code"
          output_data$do_mgl_adj[i] <- NA
          output_data$do_percsat_adj[i] <- NA
        }
        if(output_data$watertemp_C_1[i] == "-888.88") {
          output_data$do_qaqc_code[i] <- "TEMP_ERROR"
          output_data$do_qaqc_adj[i] <- "REMOVED"
          output_data$do_qaqc_note[i] <- "Logger error code"
          output_data$watertemp_C_1_adj[i] <- NA
        }

        if(output_data$do_mgl[i]<0 & output_data$do_mgl[i]>= -1) {
          output_data$do_mgl_adj[i] <- 0
          output_data$do_percsat_adj[i] <- 0
          output_data$do_qaqc_code[i] <- "NEGATIVE_DO"
          output_data$do_qaqc_adj[i] <- "REPLACED"
          output_data$do_qaqc_note[i] <- "DO within bounds of 0 to -1 corrected to 0"
        }
        if(output_data$do_mgl[i]< -1) {
          output_data$do_mgl_adj[i] <- NA
          output_data$do_percsat_adj[i] <- NA
          output_data$do_qaqc_code[i] <- "NEGATIVE_DO"
          output_data$do_qaqc_adj[i] <- "REMOVED"
          output_data$do_qaqc_note[i] <- "DO less than -1 removed"
        }
        if(output_data$do_mgl[i] > 21) {
          output_data$do_mgl_adj[i] <- NA
          output_data$do_percsat_adj[i] <- NA
          output_data$do_qaqc_code[i] <- "OUTLIER_DO"
          output_data$do_qaqc_adj[i] <- "REMOVED"
          output_data$do_qaqc_note[i] <- "DO greater than 21 mg/L removed"
        }

      } # end of if do dat available

      # if(!is.na(output_data$do_percsat[i])) {
      #   if(output_data$do_percsat[i] >= 100) {
      #     output_data$do_qaqc_code[i] <- "FLAG_DRY"
      #   }
      #
      # } # end of if do percsat dat available

      if(!is.na(output_data$watertemp_C_1[i])) {
        # if(output_data$watertemp_C_1[i] > 19) {
        #   output_data$do_qaqc_code[i] <- "FLAG_DRY"
        # }
        if(output_data$watertemp_C_1[i]<0.3){
          output_data$do_qaqc_code[i] <- "FLAG_ICE"
        }
        if(output_data$watertemp_C_1[i]<0 & output_data$watertemp_C_1[i]> -1){
          output_data$do_qaqc_code[i] <- "NEGATIVE_TEMP"
          output_data$do_qaqc_adj[i] <- "REPLACED"
          output_data$do_qaqc_note[i] <- "Water temp within bounds of 0 to -1 corrected to 0"
        }
        if(output_data$watertemp_C_1[i]<= -1){
          output_data$watertemp_C_1_adj[i] <- NA
          output_data$do_qaqc_note[i] <-  "water temp less than or equal to -1 removed"
          output_data$do_qaqc_adj[i] <-  "REMOVED"
          output_data$do_qaqc_code[i] <- "LOGGER_ICE"

        }
      } # end if watertemp data available
    } # end of i loop

    # if water temp within 2 degrees of air temp and over 100% air sat for 12 consecutive hours
    for(j in 1:(length(output_data$timestamp)-11)){
      if(!is.na(output_data$do_mgl[j])) {
        if(any(!is.na(output_data$air_watertemp_diff[j:(j+11)]))) {
          if(max(abs(output_data$air_watertemp_diff[j:(j+11)]), na.rm = TRUE) <= 2 & min(output_data$do_percsat[j:(j+11)], na.rm = TRUE) >= 100) {
            output_data$do_qaqc_code[j:(j+11)] <- "FLAG_DRY"
          }
        }
      }
    } # end of j loop

  }else{
    names(output_data)[names(output_data) == paste0(var_do_mgl, "_adj")] <-  "do_mgl_adj"
    names(output_data)[names(output_data) == paste0(var_do_percsat, "_adj")] <-  "do_percsat_adj"
    names(output_data)[names(output_data) ==  paste0(var_watertemp_C_1, "_adj")] <- "watertemp_C_1_adj"
  }


  o <- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = do_mgl, color = "do_mgl"), alpha = 0.5)+
    geom_line(aes(y = do_mgl_adj, color = "do_mgl_adj"))+
    geom_point(data = output_data %>% drop_na(do_qaqc_code) %>% filter(grepl("FLAG", do_qaqc_code)),
               aes(y = 0, shape = as.factor(do_qaqc_code)),
               color = "black")+
    scale_color_manual(values = c("do_mgl" = "#a1c181",
                                  "do_mgl_adj" = "#a1c181"))+
    scale_shape_manual(values = c("FLAG_ICE" = 1,
                                  "FLAG_DISTURBANCE" = 2,
                                  "FLAG_DRY" = 4))+
    labs(title = paste("QAQC for:", select_station),
         y = "Dissolved Oxygen (mg/L)",
         x = "Timestamp",
         color = "",
         shape = "") +
    theme_classic()

  if(var_waterlevel_m != "none"){
    p <- ggplot(data=output_data, aes(x = timestamp))+
      geom_line(aes(y = waterlevel_m, color = "waterlevel_m"))+
      geom_point(data = output_data %>% drop_na(wl_qaqc_code) %>% filter(grepl("FLAG", wl_qaqc_code)),
                 aes(y = 0, shape = as.factor(wl_qaqc_code)),
                 color = "black")+
      scale_color_manual(values = c("waterlevel_m" = "#233d4d"))+
      scale_shape_manual(values = c("FLAG_ICE" = 1,
                                    "FLAG_DISTURBANCE" = 2,
                                    "FLAG_DRY" = 4))+
      labs(title = paste("QAQC for:", select_station),
           y = "Water level (m)",
           x = "Timestamp",
           color = "",
           shape = "") +
      theme_classic()
  }

  q <- ggplot(data=output_data, aes(x = timestamp))+
    geom_line(aes(y = airtemp_C, color = "airtemp_C"), alpha = 0.5) +
    geom_line(aes(y = watertemp_C_1, color = "watertemp_C_1"), alpha = 0.5)+
    geom_line(aes(y = as.numeric(watertemp_C_1_adj), color = "watertemp_C_1_adj"))+
    scale_color_manual(values = c("watertemp_C_1" = "#619b8a" ,
                                  "watertemp_C_1_adj" = "#619b8a" ,
                                  "airtemp_C" = "#fe7f2d"))+
    labs(title =select_station,
         y = "Temperature (C)",
         x = "Timestamp",
         color = "",
         shape = "") +
    theme_classic()

  # return variables to user input naming
  names(output_data)[names(output_data) == "do_mgl" ] <-  var_do_mgl
  names(output_data)[names(output_data) == "do_mgl_adj" ] <-  paste0(var_do_mgl, "_adj")
  names(output_data)[names(output_data) == "do_percsat" ] <-  var_do_percsat
  names(output_data)[names(output_data) == "do_percsat_adj" ] <-  paste0(var_do_percsat, "_adj")
  names(output_data)[names(output_data) == "watertemp_C_1" ] <-  var_watertemp_C_1
  names(output_data)[names(output_data) == "watertemp_C_1_adj" ] <-  paste0(var_watertemp_C_1, "_adj")
  names(output_data)[names(output_data) ==  "airtemp_C"] <-  var_airtemp_C


  if(var_watertemp_C_2 != "none") {
    names(output_data)[names(output_data) == "watertemp_C_2" ] <-  var_watertemp_C_2
  }

  if(plot_only == FALSE) {
    if(var_waterlevel_m != "none") {
      names(output_data)[names(output_data) == "waterlevel_m" ] <-  var_waterlevel_m
      print("Results returned as list: select [1] for waterlevel data with QAQC flags, [2] waterlevel data to visually assess for dry/ice/disturbance periods")
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.3, 0.3, 0.3), nrows = 3,shareX = TRUE, margin = 0.05)))
    } else{
      print("Results returned as list: select [1] for waterlevel data with QAQC flags, [2] waterlevel data to visually assess for dry/ice/disturbance periods")
      return(list(output_data, plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05)))
    }
  } # end of plot only = TRUE
  if(plot_only == TRUE){
    if(var_waterlevel_m != "none") {
      names(output_data)[names(output_data) == "waterlevel_m" ] <-  var_waterlevel_m
      return(plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(p)), suppressWarnings(ggplotly(q)), heights = c(0.3, 0.3, 0.3), nrows = 3,shareX = TRUE, margin = 0.05))
    } else{
      return(plotly::subplot(suppressWarnings(ggplotly(o)), suppressWarnings(ggplotly(q)), heights = c(0.5, 0.5), nrows = 2,shareX = TRUE, margin = 0.05))
    }
  } # end of plot_only = TRUE
}
#end of dissox_qaqc function
