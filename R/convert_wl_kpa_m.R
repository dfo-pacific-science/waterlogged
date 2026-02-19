#' Convert water level from kPa to metres via barometric compensation
#'
#' Converts raw water-pressure logger readings (kPa) to water level in
#' metres using barometric compensation and a manual stage or depth
#' reference measurement.  This is the v0.3 step in the processing
#' pipeline, typically run after \code{\link{add_nearest_baro}} (v0.2)
#' has attached barometric data.
#'
#' @param input_data A data frame of logger observations.
#'   Must contain: \code{site_station_code}, \code{timestamp} (POSIXct or
#'   parseable by \code{lubridate::ymd_hms()}), \code{waterpress_kPa},
#'   \code{watertemp_C}, and \code{airpress_kPa}.  Optionally
#'   \code{logger_type} (used for a sanity check against
#'   \code{logger_type_expected}).  Typically the output of
#'   \code{\link{add_nearest_baro}}.
#' @param select_station Character scalar, \code{"all"}, or \code{NA}.
#'   When a single station code is supplied, only that station is
#'   processed.
#'   When \code{"all"}, \code{NA}, \code{NULL}, or missing, every unique
#'   \code{site_station_code} in \code{input_data} is processed in batch
#'   mode; failures for individual stations produce warnings rather than
#'   errors.
#' @param reference_data Character scalar (file path) or data frame.
#'   Manual stage/depth reference measurements.  When a character path is
#'   supplied, the file is read and validated by
#'   \code{\link{QAQC_reference_data}}.  When a data frame is supplied
#'   (Shiny mode), it is validated directly.  Must contain
#'   \code{site_station_code}, \code{timestamp}, and at least one of
#'   \code{stage_m} or \code{depth_m} (matching \code{reference_type}).
#' @param reference_type Character scalar: \code{"stage"} (default) or
#'   \code{"depth"}.  Determines which reference column (\code{stage_m}
#'   or \code{depth_m}) is used for the barometric compensation constant.
#' @param select_measurement Integer scalar.  Index (1-based) of the
#'   reference measurement to use as the anchor point for computing the
#'   compensation constant \emph{k}.
#'   Default \code{1} (first in-range measurement).
#' @param logger_type_expected Character scalar.  Expected pattern in the
#'   \code{logger_type} column (matched case-insensitively).
#'   Default \code{"u20"} (HOBO U20 water-level logger).
#' @param path_to_output_folder Character scalar or \code{NULL}.  Root
#'   directory where per-year v0.3 WL CSVs will be written.
#'   When \code{NULL} (default, Shiny mode), no files are written.
#' @param return_plots Logical; if \code{TRUE} (default), an interactive
#'   \pkg{plotly} chart is included in the returned list.  Set to
#'   \code{FALSE} to skip plot creation (faster, useful in Shiny or
#'   batch pipelines).
#'
#' @return A three-element named list:
#'   \describe{
#'     \item{\code{site_wl}}{Data frame of logger observations with added
#'       columns: \code{density_kgm3}, \code{density_lbft3},
#'       \code{sensor_depth_m}, \code{baro_sensor_depth_m},
#'       \code{waterlevel_m}, \code{waterlevel_reference}, and
#'       \code{ref_m} (joined reference values, \code{NA} where no
#'       reference exists).  In batch mode the data frames for all
#'       stations are row-bound.}
#'     \item{\code{ref_dat}}{Data frame of reference measurements used
#'       (\code{timestamp}, \code{ref_m}).  In batch mode the reference
#'       data for all stations are row-bound.}
#'     \item{\code{wl_plot}}{An interactive \pkg{plotly} object showing
#'       the converted water-level time series (dark line) overlaid with
#'       reference measurements (orange points).  In batch mode the plot
#'       is facetted by station.}
#'   }
#'
#' @details
#' The conversion proceeds as follows:
#' \enumerate{
#'   \item \strong{Reference data validation.}
#'     \code{\link{QAQC_reference_data}} validates the reference
#'     measurements (timestamp parsing, numeric coercion, duplicate
#'     detection).
#'   \item \strong{Input data validation.}
#'     \code{QAQC_wl_kpa_inputs()} checks required columns, parses
#'     timestamps, verifies \code{logger_type}, and warns about
#'     out-of-range values for water temperature, water pressure, and
#'     barometric pressure.
#'   \item \strong{Fluid density.}
#'     Temperature-dependent water density is calculated using the
#'     IAPWS-recommended polynomial (Kell, 1975) and converted to
#'     lb/ft\eqn{^3} for intermediate unit conversion.
#'   \item \strong{Pressure to depth.}
#'     Water and barometric pressures (kPa) are converted to equivalent
#'     depth (m) via the relation
#'     \code{D = 0.3048 * (P * 0.1450377 * 144) / rho},
#'     where \code{rho} is in lb/ft\eqn{^3}.
#'   \item \strong{Compensation constant \emph{k}.}
#'     At the reference measurement selected by
#'     \code{select_measurement}, the constant
#'     \eqn{k = L - (D_{sensor} - D_{baro})} is computed, where
#'     \eqn{L} is the manual stage or depth reading.
#'   \item \strong{Water level.}
#'     The full time series is computed as
#'     \eqn{WL = D_{sensor} - D_{baro} + k}.
#' }
#'
#' When \code{path_to_output_folder} is non-\code{NULL}, per-year v0.3
#' CSVs are written to
#' \code{<path_to_output_folder>/<year>/processed/<station>_WL_<start>_<end>_v0.3.csv}.
#'
#' @seealso \code{\link{add_nearest_baro}} (upstream, attaches barometric
#'   data), \code{\link{QAQC_reference_data}} (validates reference
#'   measurements), \code{\link{wl_set_datum}} (downstream, sets
#'   arbitrary datum), \code{\link{waterlevel_qaqc}} (downstream QAQC)
#'
#' @export
convert_waterlevel_kPa_m <- function(input_data,
                                     select_station,
                                     reference_data,      # path or data.frame
                                     reference_type = "stage",
                                     select_measurement = 1,
                                     logger_type_expected = "u20",
                                     path_to_output_folder = NULL,
                                     return_plots = TRUE) {
  ## 1) Read & QAQC reference data -------------------------------
  if (is.data.frame(reference_data)) {
    reference_data <- QAQC_reference_data(ref_data = reference_data)
  } else {
    reference_data <- QAQC_reference_data(ref_path = reference_data)
  }
  
  ## 2) QAQC input_data -----------------------------------------
  input_data <- QAQC_wl_kpa_inputs(
    input_data,
    logger_type_expected = logger_type_expected
  )
  
  ## 3) Clean reference_type ------------------------------------
  ref_type_clean <- tolower(trimws(reference_type))
  if (!ref_type_clean %in% c("stage", "depth")) {
    stop("reference_type must be either 'stage' or 'depth'.")
  }
  ref_level_col <- if (ref_type_clean == "stage") "stage_m" else "depth_m"
  
  ## 4) Ensure reference_data has needed cols -------------------
  needed_ref_cols <- c("site_station_code", "timestamp", ref_level_col)
  missing_ref_cols <- setdiff(needed_ref_cols, names(reference_data))
  if (length(missing_ref_cols) > 0) {
    stop(
      "Reference data is missing required column(s) for reference_type = '", ref_type_clean, "': ",
      paste(missing_ref_cols, collapse = ", "),
      "\nExpected (for '", ref_type_clean, "') columns: ",
      paste(needed_ref_cols, collapse = ", ")
    )
  }
  
  ## 5) Handle select_station = "all" / NA / missing ------------
  if (missing(select_station) ||
      is.null(select_station) ||
      (length(select_station) == 1 &&
       (is.na(select_station) || tolower(select_station) == "all"))) {
    
    stations <- sort(unique(input_data$site_station_code))
    if (!length(stations)) {
      stop("No site_station_code values found in input_data.")
    }
    
    message(
      "Running waterlevel conversion for ALL stations: ",
      paste(stations, collapse = ", ")
    )
    
    site_list <- list()
    ref_list  <- list()
    
    for (stn in stations) {
      message("Processing station: ", stn)
      
      res <- tryCatch(
        convert_waterlevel_single(
          input_data            = input_data,
          select_station        = stn,
          reference_data        = reference_data,
          ref_type_clean        = ref_type_clean,
          ref_level_col         = ref_level_col,
          select_measurement    = select_measurement,
          path_to_output_folder = path_to_output_folder,
          return_plots          = return_plots
        ),
        error = function(e) {
          msg <- conditionMessage(e)
          warning(
            sprintf("Station %s failed with error:\n  %s", stn, msg),
            call. = FALSE
          )
          return(NULL)
        }
      )
      
      if (!is.null(res)) {
        site_list[[stn]] <- res$site_wl
        ref_list[[stn]]  <- res$ref_dat
      }
    }
    
    if (!length(site_list)) {
      stop("No stations could be successfully processed.")
    }
    
    site_wl_all <- dplyr::bind_rows(site_list)
    ref_all     <- dplyr::bind_rows(ref_list)
    
    # Facetted QAQC plot across all stations
    if (return_plots) {
    p_all <- ggplot2::ggplot(site_wl_all,
                             ggplot2::aes(x = timestamp, y = waterlevel_m)) +
      ggplot2::geom_line(color = "#233d4d") +
      ggplot2::geom_point(
        ggplot2::aes(y = ref_m),
        color = "#fe7f2d",
        size  = 2
      ) +
      ggplot2::facet_wrap(~ site_station_code, scales = "free_y") +
      ggplot2::labs(
        title = "kPa to m conversion (all stations)",
        x     = "Timestamp",
        y     = "Water level (m)"
      ) +
      ggplot2::theme_classic()
    wl_plot <- plotly::ggplotly(p_all)
    } else {
      wl_plot <- NULL
    }

    message("Results returned as list: [[1]] all waterlevel data, [[2]] all reference data, [[3]] facetted QAQC plot")
    
    return(list(
      site_wl = site_wl_all,
      ref_dat = ref_all,
      wl_plot = wl_plot
    ))
  }
  
  ## 6) Single-station mode -------------------------------------
  
  res_single <- convert_waterlevel_single(
    input_data            = input_data,
    select_station        = select_station,
    reference_data        = reference_data,
    ref_type_clean        = ref_type_clean,
    ref_level_col         = ref_level_col,
    select_measurement    = select_measurement,
    path_to_output_folder = path_to_output_folder,
    return_plots          = return_plots
  )
  
  message("Results returned as list: [[1]] waterlevel data, [[2]] reference data, [[3]] QAQC plot")
  
  return(res_single)
}
