#' Conductivity Temperature Compensation
#'
#' Applies temperature correction to conductivity measurements to calculate specific conductance
#' standardized at 25°C. Uses a non-linear temperature compensation method for natural waters,
#' equivalent to ISO 7888.
#'
#' @param input_data Data frame. The dataset containing the conductivity and temperature data.
#' @param var_conduct_uScm Character. Name of the column containing conductivity values (in µS/cm).
#' @param var_watertemp_C Character. Name of the column containing temperature values (in °C).
#' @returns A data frame with an additional column containing the corrected specific conductance.
#'
#' @references
#' Mettler Toledo. (2022). *Conductivity Measurement Theory Guide*.
#' Nänikon, Switzerland. Retrieved from
#' \url{https://www.mt.com/us/en/home/library/guides/lab-analytical-instruments/Conductivity-Measurement-Theory-Guide.html}
#'
#' @export
#' @import dplyr


conduct_temp_comp <- function(input_data,
                              var_conduct_uScm = "conduct_uScm_U24_adj",
                              var_watertemp_C = "watertemp_C_U24_adj") {

  ## Pre-processing QC ----
  # Default argument assumes data has only undergone automated conductivity_qaqc, but not correct_drift
  # Check if input data contains a _dc column,
  # implying that data has had the optional correct_drift function applied to it

  has_dc_col <- any(grepl("_dc$", names(input_data)))
  arg_is_dc <- grepl("_dc$", var_conduct_uScm)

  if (has_dc_col && !arg_is_dc) {

    message("WARNING: Drift-corrected conductivity column(s) detected in input_data, ",
            "but 'var_conduct_uScm' does not end with '_dc'.")

    user_input <- readline("Proceed anyway? (Y/N): ")

    # keep asking until input is valid
    while (!user_input %in% c("Y", "y", "N", "n")) {
      user_input <- readline("Please enter Y or N: ")
    }

    if (user_input %in% c("N", "n")) {
      message("Aborted by user because drift-corrected column was not specified.")
      return(invisible(input_data))
    }

    message("Proceeding with user-specified column...")
  }


  ## Temp comp function ----

  # Obtain temperature correction coefficients from Mettler Toledo (2022)
  path_tcomp_tbl <- system.file("extdata", "tempcomp27888.csv", package = "waterlogged")
  tcomp <- as.data.frame(read.csv(path_tcomp_tbl))

  # Tidy data frame
  rownames(tcomp) <- tcomp[,1]
  tcomp <- tcomp %>%
    pivot_longer(cols = `X0`:`X0.9`,
                 names_to = "Tsub",
                 values_to = "coef")
  tcomp$Tsub <- str_remove_all(tcomp$Tsub, "X")
  tcomp <- tcomp %>%
    mutate(temp = Temp_C + as.numeric(as.character(Tsub))) %>%
    select(temp, coef) %>%
    arrange(temp)

  # Generate model
  model5 <- lm(coef ~ temp + I(temp^2) + I(temp^3) + I(temp^4) + I(temp^5), data = tcomp)

  coefficients <- coef(model5)

  tempcomp <- function(x) {
    coefficients[1] +
      coefficients[2]*x +
      coefficients[3]*x^2 +
      coefficients[4]*x^3 +
      coefficients[5]*x^4 +
      coefficients[6]*x^5
  }

  ## Calculate specific conductance
  # rename input columns to local columns
  names(input_data)[names(input_data) == var_conduct_uScm ] <- "conduct_uScm"
  names(input_data)[names(input_data) == var_watertemp_C ] <- "watertemp_C"

  # apply temperature correction model coefficients to data
  output_data <- input_data %>%
    mutate(spc_uScm_U24 = tempcomp(watertemp_C) * conduct_uScm)


  ## Create plot of conductivity, specific conductance, and temperature ----
  p <- ggplot(output_data, aes(x = timestamp)) +
    geom_line(aes(y = conduct_uScm, colour = "conduct_uScm"), alpha = 0.5)+
    geom_line(aes(y = spc_uScm_U24, colour = "spc_uScm_U24")) +
    scale_colour_manual(values = c("conduct_uScm" = "#00008B",
                                   "spc_uScm_U24" = "#4CBB17")) +
    labs(title = paste("Specific conductance model for:", unique(output_data$site_station_code)),
         y = "Conductivity (uS/cm)",
         x = "Timestamp",
         colour = "") +
    theme_classic()

  q <- ggplot(output_data, aes(x = timestamp)) +
    geom_line(aes(y = watertemp_C, colour = "watertemp_C"), alpha = 0.5) +
    scale_colour_manual(values = ("watertemp_C" = "#619b8a")) +
    labs(y = "Temperature (C)",
         x = "Timestamp",
         colour = "") +
    theme_classic()

  plots <- plotly::subplot(suppressWarnings(ggplotly(p)),
                           suppressWarnings(ggplotly(q)),
                           heights = c(0.5, 0.5), nrows = 2, shareX = TRUE, margin = 0.05)

  print(plots)

  # return variables to user input naming
  names(output_data)[names(output_data) == "conduct_uScm"] <- var_conduct_uScm
  names(output_data)[names(output_data) == "watertemp_C"] <- var_watertemp_C

  return(output_data)
}
