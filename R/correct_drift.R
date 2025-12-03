#' Conductivity logger drift correction
#'
#' Correct raw conductivity data for sensor drift based on discrete reference measurements
#' @param input_data The logger dataset containing raw conductivity in uS/cm and temperature in degrees Celcius
#' @param ref_data The reference dataset containing discrete conductivity and temperature measurements
#' @param select_station Unique identifier code for station
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_conduct_uScm Name of column for conductivity data (in microSiemens per centimeter) in input data
#' @param var_ref_watertemp_C Name of column for reference water temperature (in degrees Celcius) in ref data
#' @param var_ref_conduct_uScm Name of column for reference conductivity (in in microSiemens per centimeter) in ref data
#' @param var_ref_timestamp Name of column for reference measurement time
#'
#' @examples
#' data_spc <- correct_drift(
#'   input_data = readr::read_csv("inst/extdata/ex-conductivity/CO_FROG_WE_01_20240420_20240921_v0.1.csv"),
#'   ref_data = readr::read_csv("inst/extdata/ex-site-attribute/site_station_waterquality.csv"),
#'   select_station = "FROG_WE_01",
#'   var_watertemp_C = "watertemp_C_U24_adj",
#'   var_conduct_uScm = "conduct_uScm_U24_adj",
#'   var_ref_watertemp_C = "ysi_watertemp_C",
#'   var_ref_conduct_uScm = "ysi_conduct_uScm",
#'   var_ref_timestamp = "ysi_timestamp"
#' )
#' @export

correct_drift <- function(input_data,
                          ref_data,
                          select_station,
                          var_watertemp_C = "watertemp_C_U24_adj",
                          var_conduct_uScm = "conduct_uScm_U24_adj",
                          var_ref_watertemp_C,
                          var_ref_conduct_uScm,
                          var_ref_timestamp) {

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

  ref_data_site <- ref_data_site %>%
    # mutate(timestamp = ref_timestamp) %>%
    # filter reference measurements to be within +/- 2 hours of logger record
    filter(ref_timestamp >= (time_range[1] - 60*120) &
             ref_timestamp <= (time_range[2] + 60*120)) %>%
    select(ref_timestamp, ref_conduct_uScm, ref_watertemp_C)


  ## Join reference dataframe to logger dataframe ----
  # 1. join on reference measurements taken within 29 mins of logger timestamp
  joined_refs <- fuzzyjoin::difference_left_join(logger_data, ref_data_site,
                                                 by = c("timestamp" = "ref_timestamp"),
                                                 max_dist = 29*60) %>%
    mutate(difftime = difftime(timestamp, ref_timestamp))

  # 2. ID unmatched reference measurements (usually pre-post deployment)
  refs_unmatched <- difference_anti_join(ref_data_site, logger_data,
                                                    by = c("ref_timestamp" = "timestamp"),
                                                    max_dist = 29*60)

  # 3. join within a 1-hour tolerance window
  joined_remainder <- difference_left_join(logger_data, refs_unmatched,
                                                      by = c("timestamp" = "ref_timestamp"),
                                                      max_dist = 60*60) %>%
    mutate(difftime = difftime(timestamp, ref_timestamp)) %>%
    filter(!is.na(ref_timestamp))

  ## if no matches found, look within 2-hour window
  if(nrow(joined_remainder) == 0){
    joined_remainder <- difference_left_join(logger_data, refs_unmatched,
                                                        by = c("timestamp" = "ref_timestamp"),
                                                        max_dist = 2*60*60) %>%
      mutate(difftime = difftime(timestamp, ref_timestamp)) %>%
      filter(!is.na(ref_timestamp))
  }

  # 5. combine to form final dataset
  ## putting the joined_remainder rows first makes sure they are preserved and the unmatched duplicates are dropped
  joined_data_all <- bind_rows(joined_remainder, joined_refs) %>%
    distinct(timestamp, .keep_all = TRUE) %>%
    arrange(timestamp) %>%
    select(-difftime)


  # If there is no initial reference measurement taken at the start of the data frame,
  # then create a "dummy" reference measurement set to the values of the first logger timestamp.
  # This is used to back-calculate drift assuming that logger calibration is perfect at the start of logging
  if (is.na(joined_data_all$ref_conduct_uScm[1])) {
    joined_data_all$ref_conduct_uScm[1] <- joined_data_all$conduct_uScm[1]
    joined_data_all$ref_watertemp_C[1] <- joined_data_all$watertemp_C[1]
    joined_data_all$ref_timestamp[1] <- joined_data_all$timestamp[1]

    warning("No initial reference point found. Estimating drift based on perfect
            calibration at the start of the logger data file.")
  }


  ## Determine drift correction factor ----
  # where drift is calculated as the % difference between
  # the reference and logger measurement

  # 1. calculate drift and time intervals between reference points
  diff_ref_points <- joined_data_all %>%
    filter(!is.na(ref_conduct_uScm)) %>%
    mutate(ref_time_intv = coalesce(as.numeric(difftime(timestamp, lag(timestamp), units = "hours")), 0),
           per_c_offset = ref_conduct_uScm / conduct_uScm,
           per_t_offset = ref_watertemp_C / watertemp_C,
           c_offset_diff = coalesce(per_c_offset - lag(per_c_offset), 0),
           t_offset_diff = coalesce(per_t_offset - lag(per_t_offset), 0))

  # 2. join drift info back to logger data
  joined_data_drift <- joined_data_all %>%
    left_join(dplyr::select(diff_ref_points,
                            ref_timestamp, ref_time_intv,
                            per_c_offset, c_offset_diff,
                            per_t_offset, t_offset_diff),
              by = "ref_timestamp")

  # 3. compute time ratio between reference measurements
  joined_data_drift <- joined_data_drift %>%
    fill(ref_time_intv, .direction = "up") %>%
    fill(ref_timestamp, .direction = "down") %>%
    mutate(time_elapsed = as.numeric(difftime(timestamp, ref_timestamp, units = "hours")),
           time_ratio = coalesce(time_elapsed / ref_time_intv, 0),
           time_ratio = if_else(is.infinite(time_ratio), 0, time_ratio))

  # 4. fill missing offsets and compute drift-corrected values
  output_data <- joined_data_drift %>%
    fill(per_c_offset, per_t_offset, .direction = "down") %>%
    fill(c_offset_diff, t_offset_diff, .direction = "up") %>%
    mutate(c_drift = per_c_offset + c_offset_diff * time_ratio,
           t_drift = per_t_offset + t_offset_diff * time_ratio) %>%
    fill(c_drift, t_drift, .direction = "down") %>%
    mutate(conduct_uScm_dc = conduct_uScm * c_drift,
           watertemp_C_dc  = watertemp_C * t_drift)

  # 5. update qa/qc columns
  append_qaqc <- function(existing, new) {
    ifelse(is.na(existing) | existing == "", new, paste(existing, new, sep = "; "))
  }

  output_data <- output_data %>%
    mutate(co_qaqc_code = if_else(conduct_uScm_dc != conduct_uScm & !is.na(conduct_uScm_dc),
                                  append_qaqc(co_qaqc_code, "DRIFT_CORR"),
                                  co_qaqc_code),
           co_qaqc_note = if_else(conduct_uScm_dc != conduct_uScm & !is.na(conduct_uScm_dc),
                                  append_qaqc(co_qaqc_note, "Applied drift correction"),
                                  co_qaqc_note),
           co_qaqc_adj = if_else(conduct_uScm_dc != conduct_uScm & !is.na(conduct_uScm_dc),
                                 append_qaqc(co_qaqc_adj, paste0("Multiplied conductivity by ", round(c_drift,3),
                                                                 " and temperature by ", round(t_drift,3))),
                                 co_qaqc_adj)
           ) %>%
    select(sn, timestamp, site_station_code,
           starts_with("conduct"), starts_with("watertemp"),
           ref_conduct_uScm, ref_watertemp_C,
           starts_with("co_qaqc"))

  ## Plot the data to compare drift results ----
  p <- ggplot(output_data, aes(x = timestamp)) +
    geom_line(aes(y = conduct_uScm, colour = "Uncorrected")) +
    geom_line(aes(y = conduct_uScm_dc, colour = "Drift-corrected")) +
    geom_point(aes(y = ref_conduct_uScm, colour = "Reference"), size = 4) +
    scale_color_manual(name = "Measurement type",
                       values = c("Uncorrected" = "cyan",
                                  "Drift-corrected" = "darkblue",
                                  "Reference" = "green")) +
    labs(title = paste("Conductivity drift correction for:", select_station),
         y = "Conductivity (µS/cm)",
         x = "Timestamp") +
    theme_classic()

  # return variables to user input naming
  names(output_data)[names(output_data) == "conduct_uScm"] <- var_conduct_uScm
  names(output_data)[names(output_data) == "watertemp_C"] <- var_watertemp_C
  names(output_data)[names(output_data) == "conduct_uScm_dc"] <- paste0(var_conduct_uScm, "_dc")
  names(output_data)[names(output_data) == "watertemp_C_dc"] <- paste0(var_watertemp_C, "_dc")

  print("Results returned as list: select [1] for drift-corrected conductivity data, [2] reference measurements, [3] plot of raw and drift-corrected data")
  return(list(output_data, ref_data_site, suppressWarnings(plotly::ggplotly(p))))
}
