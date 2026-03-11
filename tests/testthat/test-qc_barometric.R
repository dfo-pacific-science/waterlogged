test_that("test barometric qaqc", {

  #get baro data
  path_to_raw_folder = test_path("fixtures/baro")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")
  baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)
  sites_compiled = baro_bound[[1]]

  #check qa flags doesnt exist before
  qa.columns = c("edit_press_high","edit_press_low","edit_flatline",
                 "edit_spike","edit_temp_range")
  missing_cols <- setdiff(qa.columns, colnames(sites_compiled))
  expect_equal(missing_cols, qa.columns)

  #check log doesnt exists
  expect_false(file.exists(file.path(path_to_output_folder, "logs/DARL_ST_30_BARO_qaqc_log.csv")))

  #set temp high limit to 39 to make sure there is flags and log is written
  barometric_complete_QAQC = qc_barometric(sites_compiled,
                                       log_root = path_to_output_folder,
                                       select_station = "DARL_ST_30",
                                       temp_high_limit=39)

  #check outputs
  expect_s3_class(barometric_complete_QAQC, "data.frame")

  #check qa flags exist after
  missing_cols <- setdiff(qa.columns, colnames(barometric_complete_QAQC))
  expect_equal(length(missing_cols), 0)

  #check log  exists
  expect_true(file.exists(file.path(path_to_output_folder, "logs/DARL_ST_30_BARO_qaqc_log.csv")))

})
