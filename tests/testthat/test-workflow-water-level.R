test_that("water level workflow runs end-to-end", {

  metadata_file   <- system.file("extdata/water_level/metadata.csv", package = "waterlogged")
  baro_folder     <- system.file("extdata/water_level/baro", package = "waterlogged")
  level_folder    <- system.file("extdata/water_level/level", package = "waterlogged")
  reference_file  <- system.file("extdata/water_level/stage_readings.csv", package = "waterlogged")
  level_runs_file <- system.file("extdata/water_level/benchmarks.csv", package = "waterlogged")

  # Step 1: Bind baro files
  baro_bound <- bind_hobo_files(
    path_to_raw_folder = baro_folder,
    metadata_path      = metadata_file
  )

  baro_data <- baro_bound[[1]]
  expect_s3_class(baro_data, "data.frame")
  expect_gt(nrow(baro_data), 0)
  expect_true(all(c("timestamp", "airpress_kPa", "airtemp_C", "site_station_code") %in% names(baro_data)))
  expect_true("ALBR_ST_30" %in% baro_data$site_station_code)
  expect_s3_class(baro_bound[[2]], "ggplot")

  # Step 2: Baro QAQC
  baro_qc <- qc_barometric(
    input_data         = baro_data,
    select_station = "ALBR_ST_30",
    pressure_low_limit_kpa  = 85,
    pressure_high_limit_kpa = 105,
    temp_low_limit_C    = -40,
    temp_high_limit_C   = 50,
    spike_flag_kpa = 1.5,
    flatline_n        = 6
  )

  expect_s3_class(baro_qc, "data.frame")
  expect_equal(nrow(baro_qc[baro_qc$site_station_code=="ALBR_ST_30"]), nrow(baro_data))
  expect_true(all(c("airpress_kPa_adj", "airtemp_C_adj") %in% names(baro_qc)))
  expect_true(all(c("OUTLIER_AP", "FLAG_SPIKE", "FLAG_FLATLINE") %in% names(baro_qc)))

  # Step 3: Bind water level files
  level_bound <- bind_hobo_files(
    path_to_raw_folder = level_folder,
    metadata_path      = metadata_file
  )

  level_data <- level_bound[[1]]
  expect_s3_class(level_data, "data.frame")
  expect_gt(nrow(level_data), 0)
  expect_true(all(c("timestamp", "waterpress_kPa", "watertemp_C") %in% names(level_data)))
  expect_true("ALBR_ST_30" %in% level_data$site_station_code)

  # Step 4: Attach baro
  wl_with_baro <- add_nearest_baro(
    input_data          = level_data,
    baro_data           = baro_qc,
    metric              = "water level",
    baro_site_selection = "auto",
    metadata_path       = metadata_file
  )

  wl_data <- wl_with_baro[[1]]
  expect_s3_class(wl_data, "data.frame")
  expect_true("airpress_kPa" %in% names(wl_data))
  expect_true("ALBR_ST_30" %in% wl_data$site_station_code)

  # Step 5: Convert kPa to metres
  converted <- convert_waterlevel_kPa_m(
    input_data         = wl_data,
    select_station     = "ALBR_ST_30",
    reference_data     = reference_file,
    reference_type     = "stage",
    select_measurement = 1,
    return_plots       = FALSE
  )

  site_wl <- converted$site_wl
  expect_s3_class(site_wl, "data.frame")
  expect_gt(nrow(site_wl), 0)
  expect_true("waterlevel_m" %in% names(site_wl))
  expect_false(all(is.na(site_wl$waterlevel_m)))

  # Step 6: Water level QAQC
  wl_qc <- qc_waterlevel(
    input_data     = site_wl,
    select_station = "ALBR_ST_30"
  )

  expect_s3_class(wl_qc, "data.frame")
  expect_equal(nrow(wl_qc), nrow(site_wl))
  expect_true(all(c("waterlevel_m_adj", "watertemp_C_adj") %in% names(wl_qc)))
  expect_true(all(c("FLAG_ICE", "FLAG_SPIKE", "FLAG_DRY") %in% names(wl_qc)))
  expect_false(is.null(attr(wl_qc, "qaqc_log")))

  # Step 7: Plot (just check it doesn't error)
  p <- plot_qaqc_timeseries(
    wl_data        = wl_qc,
    select_station = "ALBR_ST_30"
  )
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))

  # Step 8: Set datum
  wl_datum <- wl_set_datum(
    input_data      = wl_qc,
    level_runs_path = level_runs_file,
    select_station  = "ALBR_ST_30"
  )

  expect_s3_class(wl_datum, "data.frame")
  expect_true("wl_elev_m" %in% names(wl_datum))
  expect_false(all(is.na(wl_datum$wl_elev_m)))
})
