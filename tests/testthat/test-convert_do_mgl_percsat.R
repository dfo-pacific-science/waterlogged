test_that("Test convert do mgl percsat", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- bind_hobo_files(path_to_raw_folder = test_path("fixtures/test_convert_do/DO"),
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  baro_bound <- bind_hobo_files(path_to_raw_folder = test_path("fixtures/test_convert_do/baro"),
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  DO_with_Baro = add_nearest_baro(input_data = DO_bound[[1]],
                                  path_to_output_folder = path_to_output_folder,
                                  baro_data_path = path_to_output_folder,
                                  metric = "DO",
                                  baro_site_selection = "auto",
                                  metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  converted_percSat <- convert_do_mgl_percsat(DO_with_Baro[[1]],
                                              output_dir = path_to_output_folder,
                                              version_label = "v0.3")

  expect_contains(colnames(converted_percSat), "do_percsat")
})

test_that("Check for required air pressure column", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- bind_hobo_files(path_to_raw_folder = test_path("fixtures/test_convert_do/DO"),
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv"))

  expect_error(converted_percSat <- convert_do_mgl_percsat(DO_bound[[1]],
                                                           output_dir = path_to_output_folder,
                                                           version_label = "v0.3"), "Missing required columns: airpress_kPa")
})
