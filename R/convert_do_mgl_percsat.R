#' Dissolved oxygen conversion
#'
#' Convert dissolved oxygen from mg/L to percent air saturation
#' @param input_data Dataframe of time series of dissolved oxygen in mg/L, temperature in degrees Celcius, and barometric pressure in kPa
#' @param var_watertemp_C The variable for watertemperature in degrees Celcius
#' @param var_airpress_kPa The variable for barometric pressure in kPa
#' @param var_do_mgl The variable for dissolved oxygen in mg per litre
#' @param path_to_output_folder The desired folder for output of dissolved oxygen conversion
#' @return output_data: data frame with dissolved oxygen in percent air saturation as a new column "do_percsat_U26"
#' @export
#' @import dplyr
#' @import ggplot2
#' @import plotly

convert_do_mgl_percsat <- function(input_data, var_watertemp_C = "watertemp_C_U26", var_airpress_kPa = "airpress_kPa_U20_adj", var_do_mgl = "do_mgl_U26", path_to_output_folder) {
  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  # change user input columns to local names
  names(input_data)[names(input_data) == var_do_mgl ] <- "do_mgl"
  names(input_data)[names(input_data) == var_airpress_kPa ] <- "airpress_kPa"
  names(input_data)[names(input_data) == var_watertemp_C ] <- "watertemp_C"

  # assign dataframe of hourly DO measurements in mg/L and barometric pressure measurements in kPa
  output_data <- input_data
  output_data$do_percsat <- NA

  for(i in 1:length(output_data$do_mgl)){
    if(!is.na(output_data$do_mgl[i])){
      # skip logger error
      if(output_data$do_mgl[i]== -888.88){
        output_data$do_percsat[i] <- NA
      } else{
        # first convert temp (C -> K) and pressure (kPa -> atm)
        temp_K = 273.15 + output_data$watertemp_C[i] # absolute temperature, K
        press_atm = output_data$airpress_kPa[i]/101.325

        # then calculate oxygen solublity in mg/l at 1 atm using the Benson and Krause Equations
        do_1atm = exp(-139.34411+((1.575701*10^5)/temp_K)-((6.642308*10^7)/temp_K^2)+((1.243800*10^10)/temp_K^3)-(8.621949*10^11)/(temp_K^4))

        # get vapour pressure of water in atm, Benson and Krause (1980)
        u_atm = exp(11.8571-(3840.70/temp_K)-(216961/temp_K^2))

        # calculate theta related to the second virial coefficient of oxygen
        theta_o = 0.000975-1.426*10^(-5)*output_data$watertemp_C[i] + 6.436*10^(-8)*output_data$watertemp_C[i]^2

        # calculate pressure factor
        F_p = ((press_atm-u_atm)*(1-theta_o*press_atm))/((1-u_atm)*(1-theta_o))

        # apply pressure correction to get do sat at present pressure
        do_sat_real = do_1atm*F_p

        # take ratio to get percent saturation
        output_data$do_percsat[i]<- signif(100*(output_data$do_mgl[i]/do_sat_real),4)

      } # end of else - skip errors
    } # end of skip na values in do_mgl
  } # end of for loop

  #revert naming of columns to user input
  names(output_data)[names(output_data) == "do_mgl" ] <- var_do_mgl
  names(output_data)[names(output_data) == "airpress_kPa" ] <- var_airpress_kPa
  names(output_data)[names(output_data) == "watertemp_C" ] <- var_watertemp_C

  # internal rename for our dataset: does not apply if do variable is named differently
  if(any(colnames(output_data)=="do_mgl_U26")){
    names(output_data)[names(output_data) == "do_percsat" ] <- "do_percsat_U26"
  }

  # save as new version
  # split by site and year
  years_i <- unique(lubridate::year(output_data$timestamp))

  for(j in 1:length(years_i)){
    year_j <- years_i[j]
    year_j_dat <- output_data %>% filter(lubridate::year(timestamp)==year_j)
    sites_j <- unique(year_j_dat$site_station_code)

    for(k in 1:length(sites_j)){
      site_k <- sites_j[k]
      site_year_dat <- output_data %>% filter(year_j_dat$site_station_code==site_k)

      #name by timestamp range
      start_k <- as.character(min(lubridate::date(site_year_dat$timestamp)))
      start_k <- gsub("\\D", "", start_k)
      end_k <- as.character(max(lubridate::date(site_year_dat$timestamp)))
      end_k <- gsub("\\D", "", end_k)

      # search for year folder, if does not exist, write new one
      if(!dir.exists(paste0(path_to_output_folder, year_j))){dir.create(paste0(path_to_output_folder, year_j))}
      if(!dir.exists(paste0(path_to_output_folder, year_j,"/processed/"))){dir.create(paste0(path_to_output_folder, year_j,"/processed/"))}

      # correct missing midnight timestamps
      site_year_dat$timestamp <- as.character(format(site_year_dat$timestamp))

      # write csv for each year and site_station_code
      write.csv(site_year_dat, paste0(path_to_output_folder, year_j,"/processed/", site_k,"_DO_", start_k,"_", end_k, "_v0.3", ".csv"), row.names = FALSE)
      print(paste("Dissolved oxygen conversion applied to dissox data (v0.3) from",site_k,"added to", year_j, "folder"))

    } #end of site level loop

  } #end of year level loop

  return(output_data)
}

