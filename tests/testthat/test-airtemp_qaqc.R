test_that("test airtemp qaqc", {

  #get baro data
  path_to_raw_folder = test_path("fixtures/data/testing/raw/baro")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/data/testing/raw/testing_metadata_FP.csv")
  baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)
  sites_compiled = baro_bound[[1]]

  #check qa flags doesnt exist before
  qa.columns = c("at_qaqc_code", "at_qaqc_note")
  missing_cols <- setdiff(qa.columns, colnames(sites_compiled))
  expect_equal(missing_cols, qa.columns)


  airtemp_complete_QAQC = airtemp_qaqc(sites_compiled,
               select_station = "ALBR_ST_30")

  #check outputs
  expect_s3_class(airtemp_complete_QAQC[[1]], "data.frame")
  expect_s3_class(airtemp_complete_QAQC[[2]], "plotly")

  #check qa flags exist after
  missing_cols <- setdiff(qa.columns, colnames(airtemp_complete_QAQC[[1]]))
  expect_equal(length(missing_cols), 0)

})
