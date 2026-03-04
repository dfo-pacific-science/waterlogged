test_that("test plot qaqc water level", {

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

  plots = plot_qaqc_timeseries(
    wl_data   = waterlevel_complete_QAQC,
    do_data   = NULL,
    baro_data = NULL,
    select_station = "ALBR_ST_30"
  )

  expect_s3_class(plots, "plotly")

})

test_that("plot_qaqc_timeseries static mode returns patchwork object", {
  skip_if_not_installed("patchwork")

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

  static_plot <- plot_qaqc_timeseries(
    wl_data   = waterlevel_complete_QAQC,
    do_data   = NULL,
    baro_data = NULL,
    select_station = "ALBR_ST_30",
    static = TRUE
  )

  expect_s3_class(static_plot, "patchwork")
  expect_true(inherits(static_plot, "gg"))
})
