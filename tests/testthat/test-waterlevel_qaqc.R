test_that("test waterlevel qaqc", {

  #bind level data
  path_to_output_folder = withr::local_tempdir()
  path_to_raw_folder = test_path("fixtures/level")
  metadata_path = test_path("fixtures/metadata/test_convert_wl_kpa_m.csv")
  level_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                 path_to_output_folder = path_to_output_folder,
                                 metadata_path = metadata_path)

  #bind baro data
  path_to_raw_folder = test_path("fixtures/baro")
  baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)

  #add baro data to level data
  wl_data_with_baro <- add_nearest_baro(input_data = level_bound[[1]],
                                        path_to_output_folder = path_to_output_folder,
                                        baro_data_path = path_to_output_folder,
                                        metric = "both",
                                        baro_site_selection = "auto",
                                        metadata_path = metadata_path)

  #convert pressure to water level
  reference_data = test_path("fixtures/NT_manual_waterlevel_20251119.csv")
  converted_data <- convert_waterlevel_kPa_m(input_data = wl_data_with_baro[[1]],
                                             select_station = "all",
                                             reference_data = reference_data,
                                             reference_type = "stage",
                                             select_measurement = 1,
                                             logger_type_expected = "u20",
                                             path_to_output_folder= path_to_output_folder)

  #check QA columns are not present
  qa.columns = c("flag_disturbance", "flag_ice", "neg_lt_minus1", "neg_between", "flag_wl_dry")
  missing_cols <- setdiff(qa.columns, colnames(converted_data))
  expect_equal(missing_cols, qa.columns)

  #check log doesnt exists
  expect_false(file.exists(file.path(path_to_output_folder, "logs/ALBR_ST_30_WL_qaqc_log.csv")))

  waterlevel_complete_QAQC <- waterlevel_qaqc(converted_data[[1]],
                                              log_root = path_to_output_folder,
                                              select_station = "ALBR_ST_30")

  #check QA columns are present
  missing_cols <- setdiff(qa.columns, colnames(waterlevel_complete_QAQC))
  expect_equal(length(missing_cols), 0)

  #check log doesnt exists
  expect_true(file.exists(file.path(path_to_output_folder, "logs/ALBR_ST_30_WL_qaqc_log.csv")))

})
