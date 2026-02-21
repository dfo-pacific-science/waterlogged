test_that("Test no usable barometric files", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- suppressMessages(bind_hobo_files(path_to_raw_folder = test_path("fixtures/no_usable_baro/DO"),
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = test_path("fixtures/metadata/test_no_usable_barometric_files.csv")))

  baro_bound <- suppressMessages(bind_hobo_files(path_to_raw_folder = test_path("fixtures/no_usable_baro/baro"),
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = test_path("fixtures/metadata/test_no_usable_barometric_files.csv")))

  expect_error(add_nearest_baro(input_data = DO_bound[[1]],
                                   path_to_output_folder = path_to_output_folder,
                                   baro_data_path = path_to_output_folder,
                                   metric = "DO",
                                   baro_site_selection = "auto",
                                   metadata_path = test_path("fixtures/metadata/test_no_usable_barometric_files.csv")),
               "No usable barometric files")
})

test_that("Test add nearest baro", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- suppressMessages(bind_hobo_files(path_to_raw_folder = test_path("fixtures/test_add_nearest_baro/DO"),
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = test_path("fixtures/metadata/test_add_nearest_baro.csv")))

  baro_bound <- suppressMessages(bind_hobo_files(path_to_raw_folder = test_path("fixtures/test_add_nearest_baro/baro"),
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = test_path("fixtures/metadata/test_add_nearest_baro.csv")))

  DO_with_Baro = suppressMessages(add_nearest_baro(input_data = DO_bound[[1]],
                                path_to_output_folder = path_to_output_folder,
                                baro_data_path = path_to_output_folder,
                                metric = "DO",
                                baro_site_selection = "auto",
                                metadata_path = test_path("fixtures/metadata/test_add_nearest_baro.csv")))

  sites_compiled = DO_with_Baro[[1]]
  multi_plots = DO_with_Baro[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})
