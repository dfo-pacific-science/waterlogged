test_that("test write clean data", {

  #bind level data
  path_to_output_folder = withr::local_tempdir()
  path_to_raw_folder = test_path("fixtures/data/testing/raw/level")
  metadata_path = test_path("fixtures/data/testing/metadata/test_convert_wl_kpa_m.csv")
  level_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                 path_to_output_folder = path_to_output_folder,
                                 metadata_path = metadata_path)

  #bind baro data
  path_to_raw_folder = test_path("fixtures/data/testing/raw/baro")
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
  reference_data = test_path("fixtures/data/testing/raw/NT_manual_waterlevel_20251119.csv")
  converted_data <- convert_waterlevel_kPa_m(input_data = wl_data_with_baro[[1]],
                                             select_station = "all",
                                             reference_data = reference_data,
                                             reference_type = "stage",
                                             select_measurement = 1,
                                             logger_type_expected = "u20",
                                             path_to_output_folder= path_to_output_folder)

  waterlevel_complete_QAQC <- waterlevel_qaqc(converted_data[[1]],
                                              log_root = path_to_output_folder,
                                              select_station = "ALBR_ST_30")

  #check clean data doesnt exists before
  expect_false(dir.exists(file.path(path_to_output_folder, "2024/clean")))
  expect_false(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  output.files = list.files(path_to_output_folder, recursive = TRUE)
  matches = grepl("2024/clean/ALBR_ST_30_WL", output.files, ignore.case = FALSE, perl = TRUE)
  expect_false(any(matches))
  matches = grepl("2025/clean/ALBR_ST_30_WL", output.files, ignore.case = FALSE, perl = TRUE)
  expect_false(any(matches))

  clean_WL <- write_clean_data(
      input_data = waterlevel_complete_QAQC,
      path_to_output_folder = path_to_output_folder,
      metric = "waterlevel"
  )

  #check clean data exists after
  expect_true(dir.exists(file.path(path_to_output_folder, "2024/clean")))
  expect_true(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  output.files = list.files(path_to_output_folder, recursive = TRUE)
  matches = grepl("2024/clean/ALBR_ST_30_WL", output.files, ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))
  matches = grepl("2025/clean/ALBR_ST_30_WL", output.files, ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))

})

test_that("test write clean data without qaqc", {

  #bind level data
  path_to_output_folder = withr::local_tempdir()
  path_to_raw_folder = test_path("fixtures/data/testing/raw/level")
  metadata_path = test_path("fixtures/data/testing/metadata/test_convert_wl_kpa_m.csv")
  level_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                 path_to_output_folder = path_to_output_folder,
                                 metadata_path = metadata_path)

  #bind baro data
  path_to_raw_folder = test_path("fixtures/data/testing/raw/baro")
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
  reference_data = test_path("fixtures/data/testing/raw/NT_manual_waterlevel_20251119.csv")
  converted_data <- convert_waterlevel_kPa_m(input_data = wl_data_with_baro[[1]],
                                             select_station = "all",
                                             reference_data = reference_data,
                                             reference_type = "stage",
                                             select_measurement = 1,
                                             logger_type_expected = "u20",
                                             path_to_output_folder= path_to_output_folder)

  expect_error(write_clean_data(
    input_data = converted_data,
    path_to_output_folder = path_to_output_folder,
    metric = "waterlevel"),
    "Missing required column")

  expect_error(write_clean_data(
    input_data = converted_data$site_wl,
    path_to_output_folder = path_to_output_folder,
    metric = "waterlevel"),
    "Missing required column")

})
