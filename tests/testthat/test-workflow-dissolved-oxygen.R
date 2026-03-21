test_that("dissolved oxygen workflow runs end-to-end", {

  metadata_file  <- system.file("extdata/dissolved_oxygen/metadata.csv", package = "waterlogged")
  baro_folder    <- system.file("extdata/dissolved_oxygen/baro", package = "waterlogged")
  do_folder      <- system.file("extdata/dissolved_oxygen/DO", package = "waterlogged")
  reference_file <- system.file("extdata/dissolved_oxygen/field_references.csv", package = "waterlogged")

  # Step 1: Bind and QAQC baro
  baro_bound <- bind_hobo_files(
    path_to_raw_folder = baro_folder,
    metadata_path      = metadata_file
  )

  baro_data <- baro_bound[[1]]
  expect_s3_class(baro_data, "data.frame")
  expect_gt(nrow(baro_data), 0)
  expect_true(all(c("timestamp", "airpress_kPa", "site_station_code") %in% names(baro_data)))

  baro_qc <- barometric_qaqc_all(
    baro_data         = baro_data,
    metadata_path     = metadata_file,
    pressure_low_kpa  = 85,
    pressure_high_kpa = 105,
    temp_low_limit    = -40,
    temp_high_limit   = 50,
    spike_threshold_kpa = 1.5,
    flatline_n        = 6
  )

  expect_equal(nrow(baro_qc), nrow(baro_data))
  expect_true("airpress_kPa_adj" %in% names(baro_qc))

  # Step 2: Bind DO files
  do_bound <- bind_hobo_files(
    path_to_raw_folder = do_folder,
    metadata_path      = metadata_file
  )

  do_data <- do_bound[[1]]
  expect_s3_class(do_data, "data.frame")
  expect_gt(nrow(do_data), 0)
  expect_true(all(c("timestamp", "do_mgl", "watertemp_C") %in% names(do_data)))
  expect_equal(unique(do_data$site_station_code), "COOK_WE_40")
  expect_s3_class(do_bound[[2]], "ggplot")

  # Step 3: Attach baro
  do_with_baro <- add_nearest_baro(
    input_data          = do_data,
    baro_data           = baro_qc,
    metric              = "DO",
    baro_site_selection = "DARL_ST_30",
    metadata_path       = metadata_file
  )

  do_data <- do_with_baro[[1]]
  expect_true("airpress_kPa" %in% names(do_data))
  expect_equal(nrow(do_data), nrow(do_bound[[1]]))
  baro_na <- sum(is.na(do_data$airpress_kPa))
  expect_lt(baro_na / nrow(do_data), 0.01)

  # Step 4: Convert DO mg/L to %sat
  do_data <- convert_do_mgl_percsat(do_data)

  expect_true("do_percsat" %in% names(do_data))
  expect_false(all(is.na(do_data$do_percsat)))
  expect_true(min(do_data$do_percsat, na.rm = TRUE) > 0)
  expect_true(max(do_data$do_percsat, na.rm = TRUE) < 200)

  # Step 5: DO QAQC
  do_qc <- qc_dissox(
    input_data     = do_data,
    select_station = "COOK_WE_40"
  )

  expect_s3_class(do_qc, "data.frame")
  expect_equal(nrow(do_qc), nrow(do_data))
  expect_true(all(c("do_mgl_adj", "do_percsat_adj") %in% names(do_qc)))
  expect_true(all(c("FLAG_DRY", "FLAG_ICE") %in% names(do_qc)))
  expect_false(is.null(attr(do_qc, "qaqc_log")))

  # Step 6: Plot
  p <- plot_qaqc_timeseries(
    do_data        = do_qc,
    select_station = "COOK_WE_40"
  )
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))

  # Step 7: Drift correction
  do_qc <- wq_drift_correction(
    input_data     = do_qc,
    ref_data_path  = reference_file,
    select_station = "COOK_WE_40",
    metric         = "do",
    method         = "ratio",
    max_ref_gap_h  = 1
  )

  expect_true("edit_do_drift" %in% names(do_qc))
  expect_gt(sum(do_qc$edit_do_drift, na.rm = TRUE), 0)
  expect_false(is.null(attr(do_qc, "qaqc_log")))

  # Final checks
  expect_true(all(c("do_mgl", "do_mgl_adj", "do_percsat", "do_percsat_adj") %in% names(do_qc)))
  expect_false(all(is.na(do_qc$do_percsat_adj)))
})
