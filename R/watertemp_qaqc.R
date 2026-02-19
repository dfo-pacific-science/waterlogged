#' Automatic watertemp qaqc
#'
#'Flag water temperatures above 19C as potentially dry, flag water temperatures below 0.3C as potentially in ice, correct temperature between -1 and 0 as 0, and remove water temperature values below -1C.
#' @param input_data Dataframe with water temperature in
#' @param select_station site_station_code for desired logger station, supplied waterlevel data will be filtered by this code
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @param plot_only logical, TRUE or FALSE; If TRUE, only plot is displayed; if FALSE, water level and temperature values out of range are removed and flagged
#' @param return_plots Logical; if \code{TRUE} (default), an interactive
#'   \pkg{plotly} chart is included in the returned list.  Set to
#'   \code{FALSE} to skip plot creation (faster, useful in Shiny or
#'   batch pipelines).
#' @return list of 2; return site dataframe with qaqc adjustments; return plot for visual qaqc
#' @export
#'
watertemp_qaqc <- function(input_data, select_station, var_airtemp_C ="airtemp_C_U20", var_watertemp_C = "watertemp_C_U20", plot_only = FALSE, return_plots = TRUE) {

  # ---- 0. Required columns ----
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "watertemp_C",
    "airtemp_C"
  )

  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "watertemp_qaqc(): input_data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- 1. Filter to station & sort ----
  output_data <- input_data |>
    dplyr::filter(.data$site_station_code == !!select_station) |>
    dplyr::arrange(.data$timestamp)

  if (nrow(output_data) == 0) {
    warning("watertemp_qaqc(): no rows for station ", select_station)
    return(output_data)
  }

  # Ensure timestamp is POSIXct
  if (!inherits(output_data$timestamp, "POSIXct")) {
    output_data$timestamp <- lubridate::ymd_hms(output_data$timestamp, tz = "UTC")
  }

  #remove rownumbers
  rownames(output_data) <- NULL

  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C"
  names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C"

  if(plot_only == FALSE) {

    # make qaqc adjustment column
    output_data$qaqc_code <- NA
    output_data$qaqc_note <- NA
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

    if (return_plots) {
      p<- ggplot2::ggplot(data=output_data)+
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = watertemp_C, color = "watertemp_C"))+
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = watertemp_C_adj, color = "watertemp_C_adj"))+
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = airtemp_C, color = "airtemp_C"), alpha = 0.2)+
        ggplot2::scale_color_manual(values = c("watertemp_C" = "yellow",
                                      "watertemp_C_adj" = "orange",
                                      "airtemp_C" = "red"))+
        ggplot2::scale_y_continuous(limits = c(-5,35))+
        ggplot2::labs(title =select_station,
             y = "Water temperature (C)",
             x = "Timestamp (PDT)") +
        ggplot2::theme_classic()
      plot_out <- plotly::ggplotly(p)
    } else {
      plot_out <- NULL
    }

    # revert naming convention
    names(output_data)[names(output_data) ==  "watertemp_C"] <- var_watertemp_C
    names(output_data)[names(output_data) ==  "airtemp_C"] <- var_airtemp_C

    return(list(output_data, plot_out))

  }

  if(plot_only == TRUE) {

    output_data <- output_data |> dplyr::filter(site_station_code == select_station)

    if (return_plots) {
      p<- ggplot2::ggplot(data=output_data)+
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = watertemp_C, color = "watertemp_C"))+
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = as.numeric(watertemp_C_adj), color = "watertemp_C_adj"))+
        ggplot2::geom_line(ggplot2::aes(x = timestamp, y = airtemp_C_U20, color = "airtemp_C_U20"), alpha = 0.2) +
        ggplot2::scale_color_manual(values = c(
          "watertemp_C" = "yellow",
          "watertemp_C_adj" = "orange",
          "airtemp_C_U20" = "red"))+
        ggplot2::scale_y_continuous(limits = c(-5,35))+
        ggplot2::labs(title =select_station,
             y = "Water temperature (C)",
             x = "Timestamp (PDT)") +
        ggplot2::theme_classic()
      plot_out <- plotly::ggplotly(p)
    } else {
      plot_out <- NULL
    }

    return(plot_out)

  }

}
