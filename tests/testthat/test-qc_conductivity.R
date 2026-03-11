test_that("multiplication works", {

  path_to_raw_folder = test_path("fixtures/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  cond_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)

  sites_compiled = cond_bound[[1]]

  #check qa flags doesnt exist before
  qa.columns = c("flag_co_spike","flag_co_temp_ice","flag_co_temp_low","flag_co_temp_high","flag_co_temp_neg_near","flag_co_dry")
  missing_cols <- setdiff(qa.columns, colnames(sites_compiled))
  expect_equal(missing_cols, qa.columns)

  #check log doesnt exists
  expect_false(file.exists(file.path(path_to_output_folder, "logs/MILE_WE_40_COND_qaqc_log.csv")))

  #set temp high limit to 39 to make sure there is flags and log is written
  cond_complete_QAQC = qc_conductivity(sites_compiled,
                                             log_root = path_to_output_folder,
                                             select_station = "MILE_WE_40")

  #check outputs
  expect_s3_class(cond_complete_QAQC, "data.frame")

  #check qa flags exist after
  missing_cols <- setdiff(qa.columns, colnames(cond_complete_QAQC))
  expect_equal(length(missing_cols), 0)

  #check log exists
  expect_true(file.exists(file.path(path_to_output_folder, "logs/MILE_WE_40_COND_qaqc_log.csv")))

})
