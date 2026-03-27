#' Attach nearest barometric pressure data to logger time series
#'
#' Joins barometric pressure (\code{airpress_kPa}) and air temperature
#' (\code{airtemp_C}) to water level, dissolved oxygen, or conductivity
#' time series. The function operates in two modes:
#'
#' \describe{
#'   \item{\strong{Auto mode}}{(\code{baro_site_selection = "auto"}, the
#'     default.) Uses latitude/longitude from the metadata to find the nearest
#'     barometric stations (Haversine distance, up to \code{max_stations}). When \code{max_km} is
#'     non-\code{NULL}, candidates are limited to that radius.
#'     Overlapping series are calibrated to the nearest station via
#'     \code{\link{calibrate_baro_series}}, then merged into a single composite
#'     with \code{dplyr::coalesce()}.}
#'   \item{\strong{Manual mode}}{(\code{baro_site_selection = "<station_code>"})
#'     Uses the specified barometric station directly, without distance
#'     filtering or calibration.}
#' }
#'
#' When \code{path_to_output_folder} is non-\code{NULL}, per-site, per-year
#' v0.2 CSV files are written to
#' \code{<path_to_output_folder>/<year>/processed/}. When \code{NULL} (Shiny
#' mode), no files are written and the function is purely in-memory.
#'
#' @param input_data A data frame of logger observations. Must contain at
#'   minimum: \code{timestamp} (POSIXct or parseable by
#'   \code{lubridate::ymd_hms()}), \code{site_station_code}, and
#'   \code{metric}. Typically the output of \code{\link{bind_hobo_files}}.
#' @param path_to_output_folder Character scalar. Root directory for v0.2 CSV
#'   output. If \code{NULL} (default), no files are written (Shiny mode).
#' @param baro_data_path Character scalar. Directory containing BARO CSV files
#'   (searched recursively). Required when \code{baro_data} is \code{NULL}.
#' @param metric Character. Which metrics require barometric compensation.
#'   One of \code{"water level"}, \code{"DO"}, \code{"conductivity"},
#'   \code{"both"} (water level + DO), or \code{"all"}. Passed to
#'   \code{\link{resolve_metrics_param}} for normalisation.
#' @param max_km Numeric or \code{NULL}. Maximum search radius (km) for
#'   barometric stations in auto mode. Default is \code{NULL} (no distance
#'   limit — all barometric stations are considered). Set to a positive number
#'   to restrict candidates to stations within that radius.
#' @param max_stations Integer. Maximum number of nearest barometric stations
#'   to use per site in auto mode (default \code{5}). Increasing this value
#'   reduces the chance of NA gaps when the closest stations have incomplete
#'   temporal coverage, at the cost of slightly more computation.
#' @param baro_site_selection Character. Either \code{"auto"} (default) for
#'   nearest-neighbour selection or a specific barometric
#'   \code{site_station_code} for manual selection.
#' @param metadata_path Character scalar. Path to a metadata CSV file.
#'   Required when \code{metadata} is \code{NULL}. The file is read and
#'   validated by \code{\link{QAQC_metadata}}.
#' @param metadata A data frame of station metadata (alternative to
#'   \code{metadata_path} for Shiny mode). Must contain at least:
#'   \code{site_station_code}, \code{latitude}, \code{longitude}, and
#'   \code{metric}. Exactly one of \code{metadata_path} or \code{metadata}
#'   must be supplied.
#' @param baro_data A data frame of barometric observations (alternative to
#'   \code{baro_data_path}). This is the recommended approach for cross-year
#'   workflows: run \code{\link{barometric_qaqc_all}} on your QAQC'd baro
#'   files and pass the combined output directly. Must contain:
#'   \code{site_station_code}, \code{timestamp}, and either
#'   \code{airpress_kPa} or \code{airpress_kPa_adj}. Optionally
#'   \code{airtemp_C} or \code{airtemp_C_adj}. Exactly one of
#'   \code{baro_data_path} or \code{baro_data} must be supplied.
#'
#' @return A two-element list:
#'   \describe{
#'     \item{\code{[[1]]}}{A data frame: the input rows augmented with columns
#'       \code{airpress_kPa}, \code{airtemp_C}, \code{baro_site_stn_code},
#'       \code{baro_qaqc_adj}, \code{baro_qaqc_note}, and
#'       \code{baro_qaqc_code}.}
#'     \item{\code{[[2]]}}{A \code{ggplot2} object: faceted time series of
#'       barometric pressure coloured by contributing baro station, for visual
#'       QC of the stitch.}
#'   }
#'
#' @details
#' \strong{Auto-mode algorithm:}
#' \enumerate{
#'   \item Compute Haversine distances between every site and every barometric
#'     station listed in the metadata.
#'   \item Retain the nearest \code{max_stations} barometric stations
#'     (optionally filtered to within \code{max_km} when non-\code{NULL}).
#'   \item For each site, read (or filter) barometric data for those stations,
#'     calibrate secondary/tertiary series to the nearest station using
#'     \code{\link{calibrate_baro_series}}, and \code{coalesce()} into a single
#'     composite series.
#'   \item Left-join the composite onto the site data by \code{timestamp}.
#' }
#'
#' The function prefers \code{_adj} columns (post-QAQC) when present: it
#' selects \code{airpress_kPa_adj} over \code{airpress_kPa} and
#' \code{airtemp_C_adj} over \code{airtemp_C}.
#'
#' Timestamps are coerced to POSIXct (UTC) if not already.
#'
#' @seealso \code{\link{bind_hobo_files}} (upstream, produces v0.1 data),
#'   \code{\link{qc_barometric}} and \code{\link{barometric_qaqc_all}} (QAQC
#'   baro data before this step),
#'   \code{\link{resolve_metrics_param}},
#'   \code{\link{calibrate_baro_series}}
#'
#' @export

add_nearest_baro <- function(input_data,
                             path_to_output_folder = NULL,
                             baro_data_path = NULL,
                             metric = c("water level", "DO", "conductivity", "both", "all"),
                             max_km = NULL,
                             max_stations = 5L,
                             baro_site_selection = "auto",
                             metadata_path = NULL,
                             metadata = NULL,
                             baro_data = NULL) {

  # ===========================================================================
  # Validate inputs
  # ===========================================================================

  metadata <- QAQC_metadata(metadata_path = metadata_path, metadata = metadata)

  # Require exactly one baro data source
  if (is.null(baro_data_path) && is.null(baro_data)) {
    stop("add_nearest_baro(): Must provide either 'baro_data_path' or 'baro_data'.", call. = FALSE)
  }
  if (!is.null(baro_data_path) && !is.null(baro_data)) {
    message("add_nearest_baro(): Both 'baro_data_path' and 'baro_data' supplied; using 'baro_data'.")
  }

  if (!is.null(max_km) && !is.numeric(max_km)) {
    stop("add_nearest_baro(): `max_km` must be numeric or NULL.", call. = FALSE)
  }

  # ===========================================================================
  # Coerce timestamps
  # ===========================================================================

  if (!inherits(input_data$timestamp, "POSIXct")) {
    input_data$timestamp <- parse_dt(input_data$timestamp, "timestamp")
  }
  # ===========================================================================
  # Resolve & normalize metrics
  # ===========================================================================

  metrics_info      <- resolve_metrics_param(metric[1])
  metrics_need_baro <- metrics_info$metrics_need_baro

  # Normalize metric names in input_data (use .data$ to avoid column/param
  # name collision — the function parameter and column are both called 'metric')
  input_data <- input_data |>
    dplyr::mutate(
      metric_norm = dplyr::case_when(
        grepl("cond", tolower(.data$metric)) ~ normalize_string("conductivity"),
        TRUE                                  ~ normalize_string(.data$metric)
      )
    )

  metadata <- metadata |>
    dplyr::mutate(metric_norm = normalize_string(.data$metric))

  # ===========================================================================
  # Filter to rows / sites that need barometric data
  # ===========================================================================

  input_data <- input_data |>
    dplyr::filter(.data$metric_norm %in% metrics_need_baro)

  if (!nrow(input_data)) {
    stop(
      "add_nearest_baro(): No rows found requiring baro for metrics: ",
      paste(metrics_need_baro, collapse = ", "),
      call. = FALSE
    )
  }

  # Drop sites not present in metadata for these metrics
  valid_sites <- metadata |>
    dplyr::filter(.data$metric_norm %in% metrics_need_baro) |>
    dplyr::pull(site_station_code) |>
    unique()

  dropped_sites <- setdiff(unique(input_data$site_station_code), valid_sites)
  if (length(dropped_sites)) {
    warning(
      "add_nearest_baro(): Dropping site(s) not found in metadata for selected metrics: ",
      paste(dropped_sites, collapse = ", ")
    )
    input_data <- input_data |>
      dplyr::filter(.data$site_station_code %in% valid_sites)
  }

  if (!nrow(input_data)) {
    stop(
      "add_nearest_baro(): After removing sites not in metadata, no rows remain. ",
      "Check `metadata_path` and station codes.",
      call. = FALSE
    )
  }

  # ===========================================================================
  # Prepare output path
  # ===========================================================================

  if (!is.null(path_to_output_folder) && !endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }

  df_with_baro <- NULL

  # ===========================================================================
  # MANUAL BARO SELECTION
  # ===========================================================================

  if (baro_site_selection != "auto") {

    baro_stn <- baro_site_selection

    # Validate the selected station exists in metadata as barometric
    baro_meta <- metadata |>
      dplyr::filter(.data$metric_norm == "barometric",
                    .data$site_station_code == baro_stn)

    if (nrow(baro_meta) == 0) {
      stop("Selected barometric site '", baro_stn,
           "' not found in metadata with metric == 'barometric'.", call. = FALSE)
    }

    # ---- Read baro data via shared helper ----
    baro_df <- read_baro_station(baro_stn, baro_data, baro_data_path)
    if (is.null(baro_df)) {
      stop("add_nearest_baro(): Could not read barometric data for selected site '",
           baro_stn, "'. Check baro_data or baro_data_path.", call. = FALSE)
    }

    # ---- Build wide baro data frame with metadata columns ----
    wide_baro_df <- baro_df |>
      dplyr::arrange(timestamp) |>
      dplyr::mutate(
        baro_site_stn_code = baro_stn
        #,
        #baro_qaqc_adj      = NA_character_,
        #baro_qaqc_note     = "manual barometric selection",
        #baro_qaqc_code     = "MANUAL_BARO"
      )

    # ---- Left-join to input data ----
    df_with_baro <- input_data |>
      dplyr::left_join(
        wide_baro_df |>
          dplyr::select(
            timestamp, airpress_kPa, airtemp_C,
            baro_site_stn_code
            #, baro_qaqc_adj, baro_qaqc_note, baro_qaqc_code
          ),
        by = "timestamp"
      ) |>
      dplyr::arrange(site_station_code, timestamp)

  } else {

    # =========================================================================
    # AUTO BARO SELECTION (nearest neighbours + calibration)
    # =========================================================================

    # ---- Compute Haversine distances between sites and baro stations --------

    site_coords <- metadata |>
      dplyr::filter(.data$metric_norm %in% metrics_need_baro) |>
      dplyr::select(site_code = site_station_code, latitude, longitude) |>
      dplyr::distinct()

    baro_coords <- metadata |>
      dplyr::filter(.data$metric_norm == "barometric") |>
      dplyr::select(baro_code = site_station_code, baro_lat = latitude, baro_lon = longitude) |>
      dplyr::distinct()

    if (!nrow(site_coords)) stop("add_nearest_baro(): No site coordinates found in metadata for selected metrics.", call. = FALSE)
    if (!nrow(baro_coords)) stop("add_nearest_baro(): No barometric stations found in metadata.", call. = FALSE)

    site_baro_rank_raw <- tidyr::crossing(site_coords, baro_coords) |>
      dplyr::mutate(
        dist_m = geosphere::distHaversine(
          cbind(longitude, latitude),
          cbind(baro_lon,  baro_lat)
        )
      ) |>
      dplyr::group_by(site_code) |>
      dplyr::arrange(dist_m, .by_group = TRUE) |>
      dplyr::mutate(rank = dplyr::row_number()) |>
      dplyr::ungroup()

    # ---- Apply distance and count filters ----
    site_baro_rank <- if (is.null(max_km)) {
      site_baro_rank_raw |> dplyr::filter(rank <= max_stations)
    } else {
      site_baro_rank_raw |> dplyr::filter(dist_m <= max_km * 1000, rank <= max_stations)
    }

    # Determine which baro stations are actually needed
    input_sites <- unique(input_data$site_station_code)
    baro_site_station_codes <- site_baro_rank |>
      dplyr::filter(site_code %in% input_sites) |>
      dplyr::pull(baro_code) |>
      unique()

    # ---- Read baro data for each candidate station --------------------------

    baro_press_list <- list()
    baro_temp_list  <- list()

    for (baro_stn in baro_site_station_codes) {

      baro_df <- read_baro_station(baro_stn, baro_data, baro_data_path)
      if (is.null(baro_df)) {
        warning("add_nearest_baro(): No usable baro data for ", baro_stn, ". Skipping.")
        next
      }

      # Rename to station-keyed columns for wide format assembly
      baro_press_list[[baro_stn]] <- baro_df |>
        dplyr::select(timestamp, airpress_kPa) |>
        dplyr::rename(!!baro_stn := airpress_kPa)

      baro_temp_list[[baro_stn]] <- baro_df |>
        dplyr::select(timestamp, airtemp_C) |>
        dplyr::rename(!!baro_stn := airtemp_C)
    }

    if (!length(baro_press_list)) {
      stop("No usable barometric files found under: ", baro_data_path, call. = FALSE)
    }

    # ---- Per-site: calibrate, stitch, and join ------------------------------

    sites <- unique(input_data$site_station_code)
    df_with_baro_list <- list()

    for (site_i in sites) {

      site_data <- input_data |> dplyr::filter(.data$site_station_code == site_i)
      if (!nrow(site_data)) next

      # Get ranked baro stations for this site
      baros_for_site <- site_baro_rank |>
        dplyr::filter(site_code == site_i) |>
        dplyr::pull(baro_code) |>
        unique()

      if (!length(baros_for_site)) {
        if (is.null(max_km)) {
          warning("add_nearest_baro(): No barometric stations found for site ", site_i, ". Skipping.")
        } else {
          warning("add_nearest_baro(): No barometric stations within ", max_km, " km for site ", site_i, ". Skipping.")
        }
        next
      }

      # Filter to stations we actually have data for
      baros_for_site <- baros_for_site[baros_for_site %in% names(baro_press_list)]
      if (!length(baros_for_site)) {
        warning("add_nearest_baro(): No usable baro files available for site ", site_i, ". Skipping.")
        next
      }

      # ---- Assemble wide pressure/temperature data frames ----
      wide_press_site <- baros_for_site |>
        purrr::map(~ baro_press_list[[.x]]) |>
        purrr::reduce(dplyr::full_join, by = "timestamp") |>
        dplyr::arrange(timestamp)

      wide_temp_site <- baros_for_site |>
        purrr::map(~ baro_temp_list[[.x]]) |>
        purrr::reduce(dplyr::full_join, by = "timestamp") |>
        dplyr::arrange(timestamp)

      baro_cols <- setdiff(names(wide_press_site), "timestamp")
      if (!length(baro_cols)) next

      # ---- Calibrate alternate series to nearest (ref) station ----
      ref_col <- baro_cols[1]  # nearest station
      calib   <- calibrate_baro_series(
        wide_press  = wide_press_site,
        baro_cols   = baro_cols,
        ref_col     = ref_col,
        min_overlap = 10L
      )
      wide_press_site <- calib$wide_press

      # Surface calibration diagnostics (when there are alternate stations)
      if (nrow(calib$reg_summary)) {
        for (ri in seq_len(nrow(calib$reg_summary))) {
          row <- calib$reg_summary[ri, ]
          message(sprintf(
            "  [%s] Calibrated %s -> %s: n_overlap=%d, R2=%.4f",
            site_i, row$alt_col, row$ref_col, row$n_overlap,
            ifelse(is.na(row$r2), NA_real_, row$r2)
          ))
        }
      }

      # ---- Coalesce pressure (nearest-first) ----
      use_adj <- paste0(baro_cols, "_adj")
      wide_baro_df_site <- wide_press_site |>
        dplyr::mutate(airpress_kPa = dplyr::coalesce(!!!rlang::syms(use_adj)))

      # Track which station provided each row's value
      wide_baro_df_site$baro_site_stn_code <- apply(
        wide_baro_df_site[use_adj],
        1,
        function(row) {
          idx <- which(!is.na(row))[1]
          if (!length(idx) || is.na(idx)) NA_character_ else baro_cols[idx]
        }
      )

      # ---- Coalesce temperature (nearest-first, no calibration) ----
      # Note: temperature is not calibrated between stations. If secondary
      # stations have systematic offsets (e.g. different elevations), the
      # coalesced series may have step changes at station transitions.
      temp_syms <- rlang::syms(baro_cols)
      baro_temp_df_site <- wide_temp_site |>
        dplyr::transmute(timestamp, airtemp_C = dplyr::coalesce(!!!temp_syms))

      # ---- Attach metadata columns and join temperature ----
      wide_baro_df_site <- wide_baro_df_site |>
     #   dplyr::mutate(
    #      baro_qaqc_adj  = NA_character_,
     #     baro_qaqc_note = NA_character_,
      #    baro_qaqc_code = NA_character_
       # ) |>
        dplyr::left_join(baro_temp_df_site, by = "timestamp")

      # Warn if composite baro has gaps
      pct <- pct_missing(wide_baro_df_site$airpress_kPa)
      if (!is.na(pct) && pct > 0) {
        warning(sprintf("add_nearest_baro(): Baro data missing for %.1f%% of timestamps for site %s.", pct, site_i))
      }

      # ---- Left-join baro composite onto site data ----
      df_with_baro_list[[site_i]] <- site_data |>
        dplyr::left_join(
          wide_baro_df_site |>
            dplyr::select(
              timestamp, airpress_kPa, airtemp_C,
              baro_site_stn_code
              #, baro_qaqc_adj, baro_qaqc_note, baro_qaqc_code
            ),
          by = "timestamp"
        ) |>
        dplyr::arrange(site_station_code, timestamp)

      # ---- Check for timestamp offset (misaligned grids) ----
      detect_timestamp_offset(
        joined_airpress = df_with_baro_list[[site_i]]$airpress_kPa,
        site_timestamps = site_data$timestamp,
        baro_timestamps = wide_baro_df_site$timestamp,
        site_code       = site_i
      )

    }  # end per-site loop

    df_with_baro <- dplyr::bind_rows(df_with_baro_list)

    # Stop if nothing was produced
    if (!nrow(df_with_baro) || !"site_station_code" %in% names(df_with_baro)) {
      stop(
        "add_nearest_baro(): No outputs were produced in auto mode. ",
        "Common causes: sites missing from metadata, no nearby baro stations within max_km, or no readable BARO files.\n",
        "Try checking unique(input_data$site_station_code) and metadata station codes, or set max_km = NULL.",
        call. = FALSE
      )
    }
  }

  # ===========================================================================
  # Write v0.2 CSVs (guarded for Shiny / in-memory mode)
  # ===========================================================================

  if (!is.null(path_to_output_folder)) {
    sites_out <- unique(df_with_baro$site_station_code)

    for (site_i in sites_out) {
      site_dat <- df_with_baro[df_with_baro$site_station_code == site_i, , drop = FALSE]
      if (!nrow(site_dat)) next

      metrics_site <- unique(site_dat$metric_norm)

      for (metric_key in metrics_site) {
        metric_rows <- dplyr::filter(site_dat, .data$metric_norm == metric_key)
        if (!nrow(metric_rows)) next

        metric_header <- metric_norm_to_code(metric_key)
        years_i <- unique(lubridate::year(metric_rows$timestamp))

        for (yy in years_i) {
          site_year <- dplyr::filter(metric_rows, lubridate::year(timestamp) == yy)
          if (!nrow(site_year)) next

          start_str <- gsub("\\D", "", as.character(min(lubridate::date(site_year$timestamp))))
          end_str   <- gsub("\\D", "", as.character(max(lubridate::date(site_year$timestamp))))

          proc_dir <- file.path(path_to_output_folder, yy, "processed")
          dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

          site_year$timestamp <- format(site_year$timestamp, "%Y-%m-%d %H:%M:%S")

          out_name <- sprintf("%s_%s_%s_%s_v0.2.csv", site_i, metric_header, start_str, end_str)
          out_path <- file.path(proc_dir, out_name)

          utils::write.csv(site_year, out_path, row.names = FALSE)
          message(sprintf("Data (v0.2) from %s (%s) written to %s", site_i, metric_header, out_path))
        }
      }
    }
  }

  # ===========================================================================
  # Drop internal working columns
  # ===========================================================================

  df_with_baro <- df_with_baro |>
    dplyr::select(-dplyr::any_of("metric_norm"))

  # ===========================================================================
  # QC plot
  # ===========================================================================

  # Determine palette size from number of unique contributing baro stations.
  # NTSERP_palette() returns a list of palette options for n <= 3, so [[1]]
  # extracts the first suggested palette. For n >= 4 it returns a single vector.
  n_baro <- length(unique(stats::na.omit(df_with_baro$baro_site_stn_code)))
  pal <- if (n_baro <= 1) {
    "#233d4d"
  } else if (n_baro <= 3) {
    suppressMessages(NTSERP_palette(n_groups = n_baro))[[1]]
  } else if (n_baro <= 7) {
    suppressMessages(NTSERP_palette(n_groups = n_baro))
  } else {
    suppressMessages(NTSERP_palette(full_palette = TRUE))[seq_len(n_baro)]
  }

  qc_plot <- df_with_baro |>
    dplyr::filter(!is.na(site_station_code)) |>
    ggplot2::ggplot(ggplot2::aes(x = timestamp, y = airpress_kPa, color = baro_site_stn_code)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 0.7, alpha = 0.7) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::facet_wrap(~ site_station_code, scales = "free_x") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(
      title = "Baro stitch QC by site",
      x = "Time",
      y = "Air pressure (kPa)",
      color = "Baro code"
    )

  return(list(df_with_baro, qc_plot))
}
