test_that("test watertemp qaqc", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- bind_logger_files(path_to_raw_folder = test_path("fixtures/test_convert_do/DO"),
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  baro_bound <- bind_logger_files(path_to_raw_folder = test_path("fixtures/test_convert_do/baro"),
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  DO_with_Baro = add_nearest_baro(input_data = DO_bound[[1]],
                                  path_to_output_folder = path_to_output_folder,
                                  baro_data_path = path_to_output_folder,
                                  metric = "DO",
                                  baro_site_selection = "auto",
                                  metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  sites_compiled = DO_with_Baro[[1]]

  #check modern flag columns don't exist before
  qa.columns = c("FLAG_ICE", "FLAG_DRY", "OUTLIER_WT")
  missing_cols <- setdiff(qa.columns, colnames(sites_compiled))
  expect_equal(missing_cols, qa.columns)

  watertemp_complete_QAQC = qc_watertemp(sites_compiled,
                 select_station = "ALBR_ST_30")

  #check return type is a data frame (not a list)
  expect_s3_class(watertemp_complete_QAQC, "data.frame")

  #check qaqc_log attribute is a data frame
  expect_s3_class(attr(watertemp_complete_QAQC, "qaqc_log"), "data.frame")

  #check modern flag columns exist after
  missing_cols <- setdiff(qa.columns, colnames(watertemp_complete_QAQC))
  expect_equal(length(missing_cols), 0)

})

test_that("test watertemp qaqc without airtemp_C succeeds", {

  # DO data bound directly (no add_nearest_baro) — no airtemp_C column present.
  # airtemp_C is optional in the modern API; this call must succeed.
  path_to_raw_folder = test_path("fixtures/DO")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  DO_bound <- bind_logger_files(path_to_raw_folder = path_to_raw_folder,
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = metadata_path)

  sites_compiled = DO_bound[[1]]

  # airtemp_C absent — should succeed (not error)
  expect_no_error(
    result <- qc_watertemp(sites_compiled, select_station = "COOK_WE_40")
  )

  # flag_wt_dry should exist but contain only the temperature-based signal
  # (no air-water comparison since airtemp_C is absent)
  expect_true("FLAG_DRY" %in% names(result))

  # confirm no air-water comparison columns leaked into the output
  expect_false("wt_air_close"  %in% names(result))
  expect_false("wt_air_warmer" %in% names(result))

})
