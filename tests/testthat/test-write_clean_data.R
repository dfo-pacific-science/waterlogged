test_that("test write clean data", {

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

test_that("test write clean data for conductivity", {

  path_to_raw_folder = test_path("fixtures/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  expect_warning(cond_bound <- suppressMessages(bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                                                path_to_output_folder = path_to_output_folder,
                                                                metadata_path = metadata_path)),
                 "Some files are in different units")

  sites_compiled = cond_bound[[1]]


  #set temp high limit to 39 to make sure there is flags and log is written
  cond_complete_QAQC = suppressMessages(conductivity_qaqc(sites_compiled,
                                                          log_root = path_to_output_folder,
                                                          select_station = "MILE_WE_40"))

  expect_false(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  output.files = list.files(path_to_output_folder, recursive = TRUE)
  matches = grepl("2024/clean/ALBR_ST_30_WL", output.files, ignore.case = FALSE, perl = TRUE)
  expect_false(any(matches))
  matches = grepl("2025/clean/ALBR_ST_30_WL", output.files, ignore.case = FALSE, perl = TRUE)
  expect_false(any(matches))

  write_clean_data(
    input_data = sites_compiled,
    path_to_output_folder = path_to_output_folder,
    metric = "conductivity")

  #check clean data exists after
  expect_true(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  output.files = list.files(path_to_output_folder, recursive = TRUE)
  matches = grepl("2025/clean/MILE_WE_40_CO", output.files, ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))
  matches = grepl("2025/clean/TUMT_WE_40_CO", output.files, ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))
})

test_that("test write clean data for wrong metric", {

  path_to_raw_folder = test_path("fixtures/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  expect_warning(cond_bound <- suppressMessages(bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                                                path_to_output_folder = path_to_output_folder,
                                                                metadata_path = metadata_path)),
                 "Some files are in different units")

  sites_compiled = cond_bound[[1]]


  #set temp high limit to 39 to make sure there is flags and log is written
  cond_complete_QAQC = suppressMessages(conductivity_qaqc(sites_compiled,
                                                          log_root = path_to_output_folder,
                                                          select_station = "MILE_WE_40"))

  expect_error(write_clean_data(
    input_data = sites_compiled,
    path_to_output_folder = path_to_output_folder,
    metric = "fish"), "Unsupported metric")



})

