test_that("test watertemp qaqc", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- bind_hobo_files(path_to_raw_folder = test_path("fixtures/data/testing/raw/test_convert_do_mgl_percsat/DO"),
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = test_path("fixtures/data/testing/metadata/test_convert_do_mgl_percsat.csv"))

  baro_bound <- bind_hobo_files(path_to_raw_folder = test_path("fixtures/data/testing/raw/test_convert_do_mgl_percsat/baro"),
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = test_path("fixtures/data/testing/metadata/test_convert_do_mgl_percsat.csv"))

  DO_with_Baro = add_nearest_baro(input_data = DO_bound[[1]],
                                  path_to_output_folder = path_to_output_folder,
                                  baro_data_path = path_to_output_folder,
                                  metric = "DO",
                                  baro_site_selection = "auto",
                                  metadata_path = test_path("fixtures/data/testing/metadata/test_convert_do_mgl_percsat.csv"))

  sites_compiled = DO_with_Baro[[1]]

  #check qa flags doesnt exist before
  qa.columns = c("qaqc_code", "qaqc_note")
  missing_cols <- setdiff(qa.columns, colnames(sites_compiled))
  expect_equal(missing_cols, qa.columns)

  watertemp_complete_QAQC = watertemp_qaqc(sites_compiled,
                 select_station = "ALBR_ST_30")

  #check outputs
  expect_s3_class(watertemp_complete_QAQC[[1]], "data.frame")
  expect_s3_class(watertemp_complete_QAQC[[2]], "plotly")

  #check qa flags exist after
  missing_cols <- setdiff(qa.columns, colnames(watertemp_complete_QAQC[[1]]))
  expect_equal(length(missing_cols), 0)

})

test_that("test watertemp qaqc with air temp", {

  path_to_raw_folder = test_path("fixtures/data/testing/raw/DO")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/data/testing/raw/testing_metadata_FP.csv")

  DO_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = metadata_path)

  sites_compiled = DO_bound[[1]]

  expect_error(watertemp_qaqc(sites_compiled,
                 select_station = "COOK_WE_40"),
               "input_data is missing required columns")
})
