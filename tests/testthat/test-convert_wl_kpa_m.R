test_that("test convert wl kpa m", {

  #bind level data
  path_to_output_folder = withr::local_tempdir()
  path_to_raw_folder = test_path("fixtures/level")
  metadata_path = test_path("fixtures/metadata/test_convert_wl_kpa_m.csv")
  level_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = metadata_path)

  #check that level data doesnt have waterlevel column
  wl_data = level_bound[[1]]
  has_waterlevel_column = "waterlevel_m" %in% colnames(wl_data)
  expect_false(has_waterlevel_column)

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

  #check that level data still doesnt have waterlevel column
  has_waterlevel_column = "waterlevel_m" %in% colnames(wl_data_with_baro)
  expect_false(has_waterlevel_column)

  #convert pressure to water level
  reference_data = test_path("fixtures/NT_manual_waterlevel_20251119.csv")
  converted_data <- convert_waterlevel_kPa_m(input_data = wl_data_with_baro[[1]],
                                             select_station = "all",
                                             reference_data = reference_data,
                                             reference_type = "stage",
                                             select_measurement = 1,
                                             logger_type_expected = "u20",
                                             path_to_output_folder= path_to_output_folder)

  #check outputs from conversion
  expect_s3_class(converted_data$site_wl, "data.frame")
  expect_s3_class(converted_data$ref_dat, "data.frame")
  expect_s3_class(converted_data$wl_plot, "plotly")

  #check we now have waterlevel column
  has_waterlevel_column = "waterlevel_m" %in% colnames(converted_data$site_wl)
  expect_true(has_waterlevel_column)

})

test_that("test convert wl kpa m without baro data", {

  #bind level data
  path_to_output_folder = withr::local_tempdir()
  path_to_raw_folder = test_path("fixtures/level")
  metadata_path = test_path("fixtures/metadata/test_convert_wl_kpa_m.csv")
  level_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                 path_to_output_folder = path_to_output_folder,
                                 metadata_path = metadata_path)

  #check that convert_waterlevel_kPa_m throws an error if there is no baro data
  reference_data = test_path("fixtures/NT_manual_waterlevel_20251119.csv")
  expect_error(convert_waterlevel_kPa_m(input_data = level_bound[[1]],
                                        select_station = "all",
                                        reference_data = reference_data,
                                        reference_type = "stage",
                                        select_measurement = 1,
                                        logger_type_expected = "u20",
                                        path_to_output_folder= path_to_output_folder),
               "input_data is missing required column")
})
