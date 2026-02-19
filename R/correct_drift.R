#' Conductivity drift correction (legacy)
#'
#' \ifelse{html}{\out{<span style="color:
#' red;">[Superseded]</span>}}{\strong{[Superseded]}} -- prefer
#' \code{\link{wq_drift_correction}}, which uses standardised column names,
#' supports multiple water-quality parameters, and integrates with the QAQC
#' logging system.
#'
#' Corrects raw conductivity (and water temperature) for sensor drift using
#' discrete reference measurements. A multiplicative correction factor is
#' computed at each reference point and linearly interpolated between points
#' using elapsed-time ratios.
#'
#' @param input_data A data frame of logger observations. Must contain
#'   \code{site_station_code}, \code{timestamp}, and the columns named by
#'   \code{var_conduct_uScm} and \code{var_watertemp_C}.
#' @param ref_data A data frame of discrete reference measurements. Must
#'   contain \code{site_station_code} and the columns named by
#'   \code{var_ref_conduct_uScm}, \code{var_ref_watertemp_C}, and
#'   \code{var_ref_timestamp}.
#' @param select_station Character scalar. The \code{site_station_code} to
#'   process.
#' @param var_watertemp_C Character. Column name for water temperature in the
#'   logger data. Default \code{"watertemp_C_U24_adj"}.
#' @param var_conduct_uScm Character. Column name for conductivity in the
#'   logger data. Default \code{"conduct_uScm_U24_adj"}.
#' @param var_ref_watertemp_C Character. Column name for reference water
#'   temperature in \code{ref_data}.
#' @param var_ref_conduct_uScm Character. Column name for reference
#'   conductivity in \code{ref_data}.
#' @param var_ref_timestamp Character. Column name for the reference
#'   measurement timestamp in \code{ref_data}.
#' @param return_plots Logical; if \code{TRUE} (default), an interactive
#'   \pkg{plotly} chart is included in the returned list.  Set to
#'   \code{FALSE} to skip plot creation (faster, useful in Shiny or
#'   batch pipelines).
#'
#' @return A four-element list:
#'   \describe{
#'     \item{\code{[[1]]}}{Data frame of drift-corrected logger data.
#'       Includes \code{*_dc} columns (drift-corrected conductivity and
#'       temperature) and updated \code{co_qaqc_*} audit columns.}
#'     \item{\code{[[2]]}}{Data frame of reference measurements filtered to
#'       the logger time range.}
#'     \item{\code{[[3]]}}{A \code{plotly} interactive plot comparing raw,
#'       drift-corrected, and reference conductivity.}
#'     \item{\code{[[4]]}}{Data frame of calibration deviations > 10 percent
#'       (empty if none).}
#'   }
#'
#' @details
#' Reference measurements are matched to logger timestamps using
#' \code{fuzzyjoin::difference_left_join()} with progressively wider
#' tolerance windows (29 min, then 60 min, then 120 min). If no reference
#' measurement aligns with the first logger reading, a dummy reference is
#' created assuming perfect calibration at deployment.
#'
#' The drift factor at each reference point is the ratio of reference to
#' logger value. Between reference points the factor is linearly
#' interpolated by elapsed time, and the corrected value is
#' \code{raw * factor}.
#'
#' @seealso \code{\link{wq_drift_correction}} (preferred replacement;
#'   supports conductivity, DO, and pH with standardised column names and
#'   QAQC log integration)
#'
#' @export

correct_drift <- function(input_data,
                          ref_data,
                          select_station,
                          var_watertemp_C = "watertemp_C_U24_adj",
                          var_conduct_uScm = "conduct_uScm_U24_adj",
                          var_ref_watertemp_C,
                          var_ref_conduct_uScm,
                          var_ref_timestamp,
                          return_plots = TRUE) {

  # ensures user is only processing one station at a time
  logger_data <- input_data
  logger_data <- logger_data[logger_data$site_station_code == select_station, ]

  # remove rownumbers
  rownames(logger_data) <- NULL

  # rename input columns to column columns
  names(logger_data)[names(logger_data) == var_watertemp_C] <- "watertemp_C"
  names(logger_data)[names(logger_data) == var_conduct_uScm] <- "conduct_uScm"

  names(logger_data)[names(logger_data) == paste0(var_conduct_uScm, "_adj") ] <- "conduct_uScm_adj"
  names(logger_data)[names(logger_data) == paste0(var_watertemp_C, "_adj" )] <- "watertemp_C_adj"

  names(ref_data)[names(ref_data) == var_ref_timestamp] <- "ref_timestamp"
  names(ref_data)[names(ref_data) == var_ref_watertemp_C] <- "ref_watertemp_C"
  names(ref_data)[names(ref_data) == var_ref_conduct_uScm] <- "ref_conduct_uScm"

  # check that timestamp column is properly formatted; if not, format
  if (!inherits(logger_data$timestamp, "POSIXct")) {
    logger_data$timestamp <- as.POSIXct(logger_data$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")

    # Check if conversion was successful
    if (any(is.na(logger_data$timestamp))) {
      warning("Some timestamps could not be converted. Check the format of the timestamp column.")
    }
  }

  # get the start & end time of the logger record
  time_range <- range(logger_data$timestamp, na.rm = TRUE)


  ## Prepare the reference data dataframe ----
  # check that ref_timestamp column is properly formatted; if not, format
  if (!inherits(ref_data$ref_timestamp, "POSIXct")) {
    ref_data$ref_timestamp <- as.POSIXct(ref_data$ref_timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")

    # Check if conversion was successful
    if (any(is.na(ref_data$ref_timestamp))) {
      warning("Some timestamps could not be converted. Check the format of the ref_timestamp column.")
    }
  }

  ref_data_site <- ref_data[ref_data$site_station_code == select_station, ]
  if (nrow(ref_data) == 0) {
    stop("No corresponding site_station_code found in the reference data file.
         Check that you have at least one reference measurement for the site.")
  }

  ref_data_site <- ref_data_site |>
    # mutate(timestamp = ref_timestamp) |>
    # filter reference measurements to be within +/- 2 hours of logger record
    dplyr::filter(ref_timestamp >= (time_range[1] - 60*120) &
             ref_timestamp <= (time_range[2] + 60*120)) |>
    dplyr::select(ref_timestamp, ref_conduct_uScm, ref_watertemp_C)


  ## Join reference dataframe to logger dataframe ----
  # 1. join on reference measurements taken within 29 mins of logger timestamp
  joined_refs <- fuzzyjoin::difference_left_join(logger_data, ref_data_site,
                                                 by = c("timestamp" = "ref_timestamp"),
                                                 max_dist = lubridate::dminutes(29)) |>
    dplyr::mutate(difftime = difftime(timestamp, ref_timestamp))

  # 2. ID unmatched reference measurements (usually pre-post deployment)
  refs_unmatched <- fuzzyjoin::difference_anti_join(ref_data_site, logger_data,
                                                    by = c("ref_timestamp" = "timestamp"),
                                                    max_dist = lubridate::dminutes(29))

  # 3. join within a 1-hour tolerance window
  joined_remainder <- fuzzyjoin::difference_left_join(logger_data, refs_unmatched,
                                                      by = c("timestamp" = "ref_timestamp"),
                                                      max_dist = lubridate::dminutes(60)) |>
    dplyr::mutate(difftime = difftime(timestamp, ref_timestamp)) |>
    dplyr::filter(!is.na(ref_timestamp))

  ## if no matches found, look within 2-hour window
  if(nrow(joined_remainder) == 0){
    joined_remainder <- fuzzyjoin::difference_left_join(logger_data, refs_unmatched,
                                                        by = c("timestamp" = "ref_timestamp"),
                                                        max_dist = lubridate::dminutes(120)) |>
      dplyr::mutate(difftime = difftime(timestamp, ref_timestamp)) |>
      dplyr::filter(!is.na(ref_timestamp))
  }

  # 5. combine to form final dataset
  ## putting the joined_remainder rows first makes sure they are preserved and the unmatched duplicates are dropped
  joined_data_all <- dplyr::bind_rows(joined_remainder, joined_refs) |>
    dplyr::distinct(timestamp, .keep_all = TRUE) |>
    dplyr::arrange(timestamp) |>
    dplyr::select(-difftime)


  # If there is no initial reference measurement taken at the start of the data frame,
  # then create a "dummy" reference measurement set to the values of the first logger timestamp.
  # This is used to back-calculate drift assuming that logger calibration is perfect at the start of logging
  if (is.na(joined_data_all$ref_conduct_uScm[1])) {
    joined_data_all$ref_conduct_uScm[1] <- joined_data_all$conduct_uScm[1]
    joined_data_all$ref_watertemp_C[1] <- joined_data_all$watertemp_C[1]
    joined_data_all$ref_timestamp[1] <- joined_data_all$timestamp[1]

    warning("No initial reference point found. Estimating drift based on perfect calibration at the start of the logger data file.",
            call. = FALSE)
  }


  ## Determine drift correction factor ----
  # where drift is calculated as the % difference between
  # the reference and logger measurement

  # 1. calculate drift and time intervals between reference points
  diff_ref_points <- joined_data_all |>
    dplyr::filter(!is.na(ref_conduct_uScm)) |>
    dplyr::mutate(ref_time_intv = dplyr::coalesce(as.numeric(difftime(timestamp, dplyr::lag(timestamp), units = "hours")), 0),
           per_c_offset = ref_conduct_uScm / conduct_uScm,
           per_t_offset = ref_watertemp_C / watertemp_C,
           c_offset_diff = dplyr::coalesce(per_c_offset - dplyr::lag(per_c_offset), 0),
           t_offset_diff = dplyr::coalesce(per_t_offset - dplyr::lag(per_t_offset), 0))

  # 2. join drift info back to logger data
  joined_data_drift <- joined_data_all |>
    dplyr::left_join(dplyr::select(diff_ref_points,
                            ref_timestamp, ref_time_intv,
                            per_c_offset, c_offset_diff,
                            per_t_offset, t_offset_diff),
              by = "ref_timestamp")

  # 3. compute time ratio between reference measurements
  joined_data_drift <- joined_data_drift |>
    tidyr::fill(ref_time_intv, .direction = "up") |>
    tidyr::fill(ref_timestamp, .direction = "down") |>
    dplyr::mutate(time_elapsed = as.numeric(difftime(timestamp, ref_timestamp, units = "hours")),
           time_ratio = dplyr::coalesce(time_elapsed / ref_time_intv, 0),
           time_ratio = dplyr::if_else(is.infinite(time_ratio), 0, time_ratio))

  # 4. fill missing offsets and compute drift-corrected values
  output_data <- joined_data_drift |>
    tidyr::fill(per_c_offset, per_t_offset, .direction = "down") |>
    tidyr::fill(c_offset_diff, t_offset_diff, .direction = "up") |>
    dplyr::mutate(c_drift = per_c_offset + c_offset_diff * time_ratio,
           t_drift = per_t_offset + t_offset_diff * time_ratio) |>
    tidyr::fill(c_drift, t_drift, .direction = "down") |>
    dplyr::mutate(conduct_uScm_dc = conduct_uScm * c_drift,
           watertemp_C_dc  = watertemp_C * t_drift)

  cal_devs <- output_data |>
    dplyr::mutate(cal_deviation = round(abs(ref_conduct_uScm - conduct_uScm)/conduct_uScm, 2)) |>
    dplyr::mutate(row_index = dplyr::row_number()) |>
    dplyr::filter(!is.na(cal_deviation), cal_deviation > 0.1) |>
    dplyr::select(row_index, timestamp, ref_conduct_uScm, conduct_uScm, cal_deviation)

  # 5. update qa/qc columns
  append_qaqc <- function(existing, new) {
    ifelse(is.na(existing) | existing == "", new, paste(existing, new, sep = "; "))
  }

  output_data <- output_data |>
    dplyr::mutate(co_qaqc_code = dplyr::if_else(conduct_uScm_dc != conduct_uScm & !is.na(conduct_uScm_dc),
                                  append_qaqc(co_qaqc_code, "DRIFT_CORR"),
                                  co_qaqc_code),
           co_qaqc_note = dplyr::if_else(conduct_uScm_dc != conduct_uScm & !is.na(conduct_uScm_dc),
                                  append_qaqc(co_qaqc_note, "Applied drift correction"),
                                  co_qaqc_note),
           co_qaqc_adj = dplyr::if_else(conduct_uScm_dc != conduct_uScm & !is.na(conduct_uScm_dc),
                                 append_qaqc(co_qaqc_adj, paste0("Multiplied conductivity by ", round(c_drift,3))),
                                 co_qaqc_adj)
           ) |>
    dplyr::select(sn, timestamp, site_station_code,
           dplyr::starts_with("conduct"), dplyr::starts_with("watertemp"),
           ref_conduct_uScm, ref_watertemp_C,
           dplyr::starts_with("co_qaqc"))

  ## Plot the data to compare drift results ----
  if (return_plots) {
    p <- ggplot2::ggplot(output_data, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(ggplot2::aes(y = conduct_uScm, colour = "Uncorrected")) +
      ggplot2::geom_line(ggplot2::aes(y = conduct_uScm_dc, colour = "Drift-corrected")) +
      ggplot2::geom_point(ggplot2::aes(y = ref_conduct_uScm, colour = "Reference"), size = 4) +
      ggplot2::scale_color_manual(name = "Measurement type",
                         values = c("Uncorrected" = "cyan",
                                    "Drift-corrected" = "darkblue",
                                    "Reference" = "green")) +
      ggplot2::labs(title = paste("Conductivity drift correction for:", select_station),
           y = "Conductivity (uS/cm)",
           x = "Timestamp") +
      ggplot2::theme_classic()
    plot_out <- suppressWarnings(plotly::ggplotly(p))
  } else {
    plot_out <- NULL
  }

  # return variables to user input naming
  names(output_data)[names(output_data) == "conduct_uScm"] <- var_conduct_uScm
  names(output_data)[names(output_data) == "watertemp_C"] <- var_watertemp_C
  names(output_data)[names(output_data) == "conduct_uScm_dc"] <- paste0(var_conduct_uScm, "_dc")
  names(output_data)[names(output_data) == "watertemp_C_dc"] <- paste0(var_watertemp_C, "_dc")

  if (nrow(cal_devs) > 0) {
    warning("One or multiple reference measurements deviate from the logger reading; check if they're appropriate to use in drift correction.
            Select [4] to print them",
            call. = FALSE)
  }

  message("Results returned as list: select [1] for drift-corrected conductivity data, [2] reference measurements, [3] plot of raw and drift-corrected data")
  return(list(output_data, ref_data_site, plot_out, cal_devs))

}
