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
