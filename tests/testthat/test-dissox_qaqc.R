test_that("Test dissox QAQC", {

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

  #check qa flags doesnt exist before
  qa.columns = c("do_error", "do_neg_between", "do_neg_lt_minus1", "do_high", "temp_flag_ice", "temp_neg_between", "temp_neg_leq_minus1", "flag_do_dry")
  missing_cols <- setdiff(qa.columns, colnames(converted_percSat))
  expect_equal(missing_cols, qa.columns)

  #check log doesnt exists
  expect_false(file.exists(file.path(path_to_output_folder, "logs/ALBR_ST_30_DO_qaqc_log.csv")))

  dissox_complete_QAQC = dissox_qaqc(converted_percSat,
              select_station = "ALBR_ST_30",
              log_root = path_to_output_folder)

  #check outputs
  expect_s3_class(dissox_complete_QAQC, "data.frame")

  #check qa flags exist after
  missing_cols <- setdiff(qa.columns, colnames(dissox_complete_QAQC))
  expect_equal(length(missing_cols), 0)

  list.files(path_to_output_folder, recursive = T)

  #check log exists
  expect_true(file.exists(file.path(path_to_output_folder, "logs/ALBR_ST_30_DO_qaqc_log.csv")))
})

test_that("Test dissox QAQC missing baro data", {

  path_to_raw_folder = test_path("fixtures/DO")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")
  DO_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = metadata_path)

  sites_compiled = DO_bound[[1]]

  expect_error(dissox_qaqc(sites_compiled),
               "input_data is missing required columns: do_percsat, airtemp_C")

})

test_that("Test dissox QAQC missing conversion", {

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

  expect_error(dissox_qaqc(DO_with_Baro[[1]]),
               "input_data is missing required columns: do_percsat")
})
