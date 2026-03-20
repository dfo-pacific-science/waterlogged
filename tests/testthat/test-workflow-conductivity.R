test_that("conductivity workflow runs end-to-end", {

  metadata_file  <- system.file("extdata/conductivity/metadata.csv", package = "waterlogged")
  cond_folder    <- system.file("extdata/conductivity/COND", package = "waterlogged")
  reference_file <- system.file("extdata/conductivity/field_references.csv", package = "waterlogged")

  # Step 1: Bind conductivity files
  cond_bound <- bind_hobo_files(
    path_to_raw_folder = cond_folder,
    metadata_path      = metadata_file
  )

  cond_data <- cond_bound[[1]]
  expect_s3_class(cond_data, "data.frame")
  expect_gt(nrow(cond_data), 0)
  expect_true(all(c("timestamp", "conduct_uScm", "watertemp_C", "site_station_code") %in% names(cond_data)))
  expect_equal(unique(cond_data$site_station_code), "TUMT_WE_40")
  expect_s3_class(cond_bound[[2]], "ggplot")
  expect_true(min(cond_data$conduct_uScm, na.rm = TRUE) > 0)

  # Step 2: Conductivity QAQC
  cond_qc <- qc_conductivity(
    input_data                 = cond_data,
    select_station             = "TUMT_WE_40",
    temp_low_limit_C           = -2,
    temp_high_limit_C          = 40,
    disturbance_threshold_uScm = 200,
    dry_threshold_uScm         = 10,
    air_water_diff_threshold_C = 2
  )

  expect_s3_class(cond_qc, "data.frame")
  expect_equal(nrow(cond_qc), nrow(cond_data))
  expect_true(all(c("conduct_uScm_adj", "watertemp_C_adj") %in% names(cond_qc)))
  expect_true(all(c("FLAG_DISTURBANCE", "FLAG_ICE", "FLAG_DRY") %in% names(cond_qc)))
  expect_false(is.null(attr(cond_qc, "qaqc_log")))

  # Step 3: Drift correction
  cond_qc <- wq_drift_correction(
    input_data     = cond_qc,
    ref_data_path  = reference_file,
    select_station = "TUMT_WE_40",
    metric         = "cond",
    method         = "ratio",
    max_ref_gap_h  = 1
  )

  expect_true("edit_cond_drift" %in% names(cond_qc))
  expect_gt(sum(cond_qc$edit_cond_drift, na.rm = TRUE), 0)

  # Step 4: Temperature compensation
  cond_qc <- conductivity_temp_compensation(cond_qc)

  expect_true("spc_uScm_adj" %in% names(cond_qc))
  expect_false(all(is.na(cond_qc$spc_uScm_adj)))
  expect_true(min(cond_qc$spc_uScm_adj, na.rm = TRUE) > 0)
  expect_true(max(cond_qc$spc_uScm_adj, na.rm = TRUE) < 500)

  # Step 5: Plot
  p <- plot_qaqc_timeseries(
    cond_data      = cond_qc,
    select_station = "TUMT_WE_40"
  )
  expect_true(inherits(p, "plotly") || inherits(p, "htmlwidget"))

  # Final state checks
  expect_true(all(c("conduct_uScm", "conduct_uScm_adj", "spc_uScm_adj",
                     "watertemp_C", "watertemp_C_adj") %in% names(cond_qc)))
})
