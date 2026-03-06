test_that("test airtemp qaqc", {

  #get baro data
  path_to_raw_folder = test_path("fixtures/baro")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")
  baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)
  sites_compiled = baro_bound[[1]]

  #check modern flag columns don't exist before
  qa.columns = c("edit_temp_range", "airtemp_C_adj")
  missing_cols <- setdiff(qa.columns, colnames(sites_compiled))
  expect_equal(missing_cols, qa.columns)

  airtemp_complete_QAQC = airtemp_qaqc(sites_compiled,
               select_station = "ALBR_ST_30")

  #check return type is a data frame (not a list)
  expect_s3_class(airtemp_complete_QAQC, "data.frame")

  #check qaqc_log attribute is a data frame
  expect_s3_class(attr(airtemp_complete_QAQC, "qaqc_log"), "data.frame")

  #check modern flag columns exist after
  missing_cols <- setdiff(qa.columns, colnames(airtemp_complete_QAQC))
  expect_equal(length(missing_cols), 0)

})
