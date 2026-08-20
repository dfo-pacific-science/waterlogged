test_that("Test snap logger timestamps", {

  path_to_raw_folder = test_path("fixtures/test_cond_dry/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")

  cond_bound <- suppressMessages(
    bind_logger_files(
      path_to_raw_folder = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path = metadata_path
    )
  )

  baro_bound <- suppressMessages(
    bind_logger_files(
      path_to_raw_folder = test_path("fixtures/test_cond_dry/baro"),
      path_to_output_folder = path_to_output_folder,
      metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")
    )
  )

  #test expected warning when baro data is offset
  expect_warning(suppressMessages(
    add_nearest_baro(
      input_data = cond_bound[[1]],
      path_to_output_folder = path_to_output_folder,
      baro_data_path = path_to_output_folder,
      metric = "conductivity",
      baro_site_selection = "auto",
      metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")
    )
  ), "Baro data didnt match timestamps for 0.0% of timestamps for site DARL_ST_30.")


  aligned_cond <- align_timestamps(input_data     = cond_bound[[1]],
                                      interval       = "1 hour",
                                      align          = "round")

  aligned_cond_with_baro = suppressMessages(
    add_nearest_baro(
      input_data = aligned_cond,
      path_to_output_folder = path_to_output_folder,
      baro_data_path = path_to_output_folder,
      metric = "conductivity",
      baro_site_selection = "auto",
      metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")
    )
  )

  expect_s3_class(aligned_cond_with_baro[[1]], "data.frame")
  expect_s3_class(aligned_cond_with_baro[[2]], "ggplot")

})
