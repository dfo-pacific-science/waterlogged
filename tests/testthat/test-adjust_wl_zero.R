test_that("test adjust wl zero", {

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

  plot_qaqc_timeseries(waterlevel_complete_QAQC,
                       select_station = "ALBR_ST_30")

  timestamp_start = "2024-11-01 00:00:00"
  timestamp_end =  "2024-12-31 00:00:00"

  adjusted_wl = adjust_WL_zero(waterlevel_complete_QAQC,
                            select_station = "ALBR_ST_30",
                            change_value = 1,
                            timestamp_start = timestamp_start,
                            timestamp_end =  timestamp_end,
                            manual_note = "stilling well moved up to clear mud",
                            log_root =  path_to_output_folder)

  plot_qaqc_timeseries(adjusted_wl,
                       select_station = "ALBR_ST_30")


  adjusted_wl = adjusted_wl %>% dplyr::mutate(adjustment = waterlevel_m_adj - waterlevel_m)

  ts_start <- lubridate::ymd_hms(timestamp_start)
  ts_end   <- lubridate::ymd_hms(timestamp_end)

  wl_unadjusted_time = adjusted_wl %>% dplyr::filter(!is.na(waterlevel_m) & (timestamp < ts_start | timestamp > ts_end))
  expect_all_equal(wl_unadjusted_time$adjustment, 0)

  wl_adjusted_time = adjusted_wl %>% dplyr::filter(!is.na(waterlevel_m) & timestamp >= ts_start & timestamp <= ts_end)
  expect_all_equal(wl_adjusted_time$adjustment, 1)

  adjusted_wl_no_timestamps = adjust_WL_zero(waterlevel_complete_QAQC,
                               select_station = "ALBR_ST_30",
                               change_value = 1,
                               manual_note = "stilling well moved up to clear mud",
                               log_root =  path_to_output_folder)

  wl_adjusted_time = adjusted_wl_no_timestamps %>% dplyr::filter(!is.na(waterlevel_m)) %>% dplyr::mutate(adjustment = waterlevel_m_adj - waterlevel_m)
  expect_all_equal(wl_adjusted_time$adjustment, 1)

})
