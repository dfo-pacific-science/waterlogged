#' Write final clean data files (v1.0)
#'
#' Subsets QAQC'd logger data to the columns relevant for a given metric,
#' splits by station and year, and writes per-station per-year v1.0 CSV
#' files to a \code{<year>/clean/} directory.
#' This is the final step in the processing pipeline, typically run after
#' all QAQC adjustments (e.g. \code{\link{qc_waterlevel}},
#' \code{\link{qc_dissox}}) are complete.
#'
#' @param input_data A data frame of QAQC'd logger observations.
#'   Must contain at minimum \code{site_station_code}, \code{timestamp},
#'   and the metric-specific required columns listed under \strong{Details}.
#'   Any columns matching \code{flag_*} or \code{edit_*} are automatically
#'   retained alongside the metric-specific columns.
#' @param path_to_output_folder Character scalar or \code{NULL}.  Root
#'   directory where per-year v1.0 CSVs will be written.  When
#'   \code{NULL} (default, Shiny mode), no files are written; the
#'   function still validates columns and returns \code{input_data}
#'   invisibly.
#' @param metric Character scalar identifying the data type.  Must be one
#'   of \code{"dissolvedoxygen"}, \code{"barometric"},
#'   \code{"waterlevel"}, \code{"watertemp"}, \code{"airtemp"}, or
#'   \code{"conductivity"}.
#'   Controls which columns are required and the file-name prefix (see
#'   \strong{Details}).
#'
#' @return \code{input_data} returned invisibly (unmodified), allowing
#'   the function to be used at the end of a pipe chain.
#'
#' @details
#' Each metric maps to a set of required and optional columns, plus a
#' file-name prefix:
#'
#' \describe{
#'   \item{\code{"dissolvedoxygen"} (DO)}{Required:
#'     \code{do_mgl}, \code{do_mgl_adj}, \code{do_percsat},
#'     \code{do_percsat_adj}.
#'     Optional: \code{watertemp_C_do}, \code{watertemp_C_do_adj}.}
#'   \item{\code{"barometric"} (BARO)}{Required:
#'     \code{airpress_kPa}, \code{airpress_kPa_adj}.
#'     Optional: \code{airtemp_C}, \code{airtemp_C_adj}.}
#'   \item{\code{"waterlevel"} (WL)}{Required:
#'     \code{waterlevel_m}, \code{waterlevel_m_adj}.
#'     Optional: \code{watertemp_C}, \code{watertemp_C_adj},
#'     \code{waterlevel_reference}.}
#'   \item{\code{"watertemp"} (WT)}{Required:
#'     \code{watertemp_C}, \code{watertemp_C_adj}.
#'     No optional columns.}
#'   \item{\code{"airtemp"} (AT)}{Required:
#'     \code{airtemp_C}.
#'     Optional: \code{airtemp_C_adj}.}
#'   \item{\code{"conductivity"} (CO)}{Required:
#'     \code{conduct_uScm}.
#'     Optional: \code{conduct_uScm_adj}, \code{spc_uScm_adj},
#'     \code{watertemp_C}, \code{watertemp_C_adj}.}
#' }
#'
#' All metrics also require \code{site_station_code} and
#' \code{timestamp}.  Output files are written to
#' \code{<path_to_output_folder>/<year>/clean/<station>_<prefix>_<start>_<end>_v1.0.csv}.
#'
#' If \code{input_data} contains a \code{metric} column whose value
#' differs from the \code{metric} argument, a warning is issued.
#'
#' @seealso \code{\link{get_logger_data}} (reads the v1.0 files produced
#'   by this function), \code{\link{qc_waterlevel}},
#'   \code{\link{qc_dissox}}, \code{\link{qc_barometric}} (upstream
#'   QAQC functions whose output is typically passed here)
#'
#' @export
write_clean_data <- function(
    input_data,
    path_to_output_folder = NULL,
    metric
) {

  # ---- config by metric ----
  metric_config <- list(
    dissolvedoxygen = list(
      header   = "DO",
      required = c(
        "site_station_code",
        "timestamp",
        "do_mgl",
        "do_mgl_adj",
        "do_percsat",
        "do_percsat_adj"
      ),
      optional = c(
        "watertemp_C_do",
        "watertemp_C_do_adj"
      )
    ),
    barometric = list(
      header   = "BARO",
      required = c(
        "site_station_code",
        "timestamp",
        "airpress_kPa",
        "airpress_kPa_adj"
      ),
      optional = c(
        "airtemp_C",
        "airtemp_C_adj"
      )
    ),
    waterlevel = list(
      header   = "WL",
      required = c(
        "site_station_code",
        "timestamp",
        "waterlevel_m",
        "waterlevel_m_adj"
      ),
      optional = c(
        "watertemp_C",
        "watertemp_C_adj",
        "waterlevel_reference"
      )
    ),
    watertemp = list(
      header   = "WT",
      required = c(
        "site_station_code",
        "timestamp",
        "watertemp_C",
        "watertemp_C_adj"
      ),
      optional = character(0)
    ),
    airtemp = list(
      header   = "AT",
      required = c(
        "site_station_code",
        "timestamp",
        "airtemp_C"
      ),
      optional = c("airtemp_C_adj")
    ),
    conductivity = list(
      header   = "CO",
      required = c(
        "site_station_code",
        "timestamp",
        "conduct_uScm"
      ),
      optional = c(
        "conduct_uScm_adj",
        "spc_uScm_adj",
        "watertemp_C",
        "watertemp_C_adj"
      )
    )
  )

  cfg <- metric_config[[metric]]
  if (is.null(cfg)) {
    stop("write_clean_data(): Unsupported metric: ", metric,
         "\nExpected metrics are: \"dissolvedoxygen\", \"barometric\", \"waterlevel\", \"watertemp\", \"airtemp\", \"conductivity\"")
  }

  # optional safety: if there's a metric column, make sure it matches
  if ("metric" %in% names(input_data)) {
    metric_in_data <- unique(input_data$metric)
    if (length(metric_in_data) > 1L) {
      stop("write_clean_data(): input_data contains multiple metrics: ",
           paste(metric_in_data, collapse = ", "))
    }
    if (!identical(metric_in_data, metric)) {
      warning("write_clean_data(): metric argument ('", metric,
              "') does not match metric column in data ('",
              metric_in_data, "'). Using argument.")
    }
  }

  # ---- required columns must exist ------------------------------------------
  missing_required <- setdiff(cfg$required, names(input_data))
  if (length(missing_required) > 0) {
    stop(
      "write_clean_data(): Missing required column(s) for metric '",
      metric, "': ",
      paste(missing_required, collapse = ", ")
    )
  }

  # ensure base output folder exists (only when writing)
  if (!is.null(path_to_output_folder) && !dir.exists(path_to_output_folder)) {
    dir.create(path_to_output_folder, recursive = TRUE)
  }

  # columns to keep: required + optional (if present) + any flag_/edit_ cols
  base_cols <- c(
    cfg$required,
    intersect(cfg$optional, names(input_data))
  )

#  flag_cols <- grep("^flag_", names(input_data), value = TRUE)
#  edit_cols <- grep("^edit_", names(input_data), value = TRUE)

  input_data$qc_code <- "REAL"

  # internal helper function to determine if column is present in input data
  has_col <- function(col) col %in% names(input_data)

  for (i in seq_len(nrow(input_data))) {

    if((has_col("OUTLIER_AP") && has_col("airpress_kPa_adj") &&
         isTRUE(input_data$OUTLIER_AP[i]) && is.na(input_data$airpress_kPa_adj[i])) ||
      (has_col("OUTLIER_AT") && has_col("airtemp_C_adj") &&
         isTRUE(input_data$OUTLIER_AT[i]) && is.na(input_data$airtemp_C_adj[i])) ||
      (has_col("OUTLIER_DO") && has_col("do_mgl_adj") &&
         isTRUE(input_data$OUTLIER_DO[i]) && is.na(input_data$do_mgl_adj[i])) ||
      (has_col("OUTLIER_WT") && has_col("watertemp_C_adj") &&
         isTRUE(input_data$OUTLIER_WT[i]) && is.na(input_data$watertemp_C_adj[i])) ||
      (has_col("OUTLIER_WT") && has_col("watertemp_C_do_adj") &&
         isTRUE(input_data$OUTLIER_WT[i]) && is.na(input_data$watertemp_C_do_adj[i])) ||
      (has_col("TEMP_RANGE") && isTRUE(input_data$TEMP_RANGE[i])) ||
      (has_col("NEGATIVE_WT") && has_col("watertemp_C_adj") &&
         isTRUE(input_data$NEGATIVE_WT[i]) && is.na(input_data$watertemp_C_adj[i])) ||
      (has_col("FLAG_DISTURBANCE") && has_col("edit_na") &&
         isTRUE(input_data$FLAG_DISTURBANCE[i]) && isTRUE(input_data$edit_na[i])) ||
      (has_col("FLAG_FLATLINE") && has_col("edit_na") &&
         isTRUE(input_data$FLAG_FLATLINE[i]) && isTRUE(input_data$edit_na[i]))){
      input_data$qc_code[i] <- "REMOVED"
    }

    if(is.na(input_data$qc_code[i]) ||
        (has_col("ERROR_DO") && isTRUE(input_data$ERROR_DO[i]))) {
      input_data$qc_code[i] <- NA
    }

      if((has_col("NEGATIVE_WT") && has_col("watertemp_C_adj") &&
         isTRUE(input_data$NEGATIVE_WT[i]) && input_data$watertemp_C_adj[i] == 0) ||
      (has_col("OUTLIER_WT") && has_col("watertemp_C_do_adj") &&
         isTRUE(input_data$OUTLIER_WT[i]) && input_data$watertemp_C_do_adj[i] == 0) ||
      (has_col("OUTLIER_DO") && has_col("do_mgl_adj") &&
         isTRUE(input_data$OUTLIER_DO[i]) && input_data$do_mgl_adj[i] == 0) ||
      (has_col("OUTLIER_DO") && has_col("do_percsat_adj") &&
         isTRUE(input_data$OUTLIER_DO[i]) && input_data$do_percsat_adj[i] == 0)){
      input_data$qc_code[i] <- "CORRECTED"
    }

    if((has_col("FLAG_RANGE") && has_col("edit_na") &&
       isTRUE(input_data$FLAG_RANGE[i]) && isTRUE(input_data$edit_na[i])) ||
      (has_col("FLAG_DRY") && has_col("edit_na") &&
       isTRUE(input_data$FLAG_DRY[i]) && isTRUE(input_data$edit_na[i]))) {
      input_data$qc_code[i] <- "DRY"
    }
  }

#  cols_to_keep <- unique(c(base_cols, flag_cols, edit_cols))
  cols_to_keep <- unique(c(base_cols, "qc_code"))

  # ---- subset and add year ---------------------------------------------------
  output_data <- input_data[, cols_to_keep, drop = FALSE]

  if (!("timestamp" %in% names(output_data))) {
    stop("write_clean_data(): 'timestamp' column missing after subsetting.")
  }
  if (!("site_station_code" %in% names(output_data))) {
    stop("write_clean_data(): 'site_station_code' column missing after subsetting.")
  }

  output_data$year <- lubridate::year(output_data$timestamp)

  # ---- split by station + year using base R ---------------------------------
  split_list <- split(
    output_data,
    list(
      output_data$site_station_code,
      output_data$year
    ),
    drop = TRUE
  )

  # helper: ensure year/clean folders exist and return clean path
  ensure_clean_folder <- function(base_path, year) {
    year_chr  <- as.character(year)
    year_path <- file.path(base_path, year_chr)
    clean_path <- file.path(year_path, "clean")
    if (!dir.exists(year_path))  dir.create(year_path)
    if (!dir.exists(clean_path)) dir.create(clean_path)
    clean_path
  }

  # ---- iterate over groups and write files (guarded for Shiny) ---------------
  if (!is.null(path_to_output_folder)) {
    for (i in seq_along(split_list)) {
      df_i <- split_list[[i]]
      if (nrow(df_i) == 0) next

      site_i <- unique(df_i$site_station_code)
      year_i <- unique(df_i$year)

      if (length(site_i) != 1L || length(year_i) != 1L) {
        stop("write_clean_data(): problem with split grouping (site/year not unique).")
      }

      # date range for filename
      start_date <- min(lubridate::as_date(df_i$timestamp))
      end_date   <- max(lubridate::as_date(df_i$timestamp))

      start_j <- gsub("\\D", "", as.character(start_date))
      end_j   <- gsub("\\D", "", as.character(end_date))

      # drop helper year, format timestamp
      df_write <- df_i
      df_write$year <- NULL
      df_write$timestamp <- format(df_write$timestamp, tz = "UTC")

      clean_path <- ensure_clean_folder(path_to_output_folder, year_i)

      file_out <- file.path(
        clean_path,
        paste0(
          site_i, "_", cfg$header, "_",
          start_j, "_", end_j, "_v1.0.csv"
        )
      )

      utils::write.csv(df_write, file_out, row.names = FALSE)

      message("Clean data (v1.0) from ", site_i,
              " written to ", year_i,
              " (", basename(file_out), ")")
    }

    message("") # add line break in console
  }

  invisible(input_data)
}
