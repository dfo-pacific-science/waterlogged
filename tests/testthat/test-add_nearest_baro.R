test_that("Test no usable barometric files", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = test_path("fixtures/no_usable_baro/DO"),
      path_to_output_folder = path_to_output_folder,
      metadata_path = test_path("fixtures/metadata/test_no_usable_barometric_files.csv")
    )
  )

  baro_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = test_path("fixtures/no_usable_baro/baro"),
      path_to_output_folder = path_to_output_folder,
      metadata_path = test_path("fixtures/metadata/test_no_usable_barometric_files.csv")
    )
  )

  anb <- add_nearest_baro(
      input_data = DO_bound[[1]],
      path_to_output_folder = path_to_output_folder,
      baro_data_path = path_to_output_folder,
      metric = "DO",
      baro_site_selection = "auto",
      metadata_path = test_path("fixtures/metadata/test_no_usable_barometric_files.csv")
    )

  # Just make sure the above runs
  expect_true(length(anb) == 2)



})

test_that("Test add nearest baro", {

  path_to_output_folder = withr::local_tempdir()

  DO_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = test_path("fixtures/test_add_nearest_baro/DO"),
      path_to_output_folder = path_to_output_folder,
      metadata_path = test_path("fixtures/metadata/test_add_nearest_baro.csv")
    )
  )

  baro_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = test_path("fixtures/test_add_nearest_baro/baro"),
      path_to_output_folder = path_to_output_folder,
      metadata_path = test_path("fixtures/metadata/test_add_nearest_baro.csv")
    )
  )

  DO_with_Baro = suppressMessages(
    add_nearest_baro(
      input_data = DO_bound[[1]],
      path_to_output_folder = path_to_output_folder,
      baro_data_path = path_to_output_folder,
      metric = "DO",
      baro_site_selection = "auto",
      metadata_path = test_path("fixtures/metadata/test_add_nearest_baro.csv")
    )
  )

  sites_compiled = DO_with_Baro[[1]]
  multi_plots = DO_with_Baro[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test offset baro data", {

  path_to_raw_folder = test_path("fixtures/test_cond_dry/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")

  cond_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path = metadata_path
    )
  )

  baro_bound <- suppressMessages(
    bind_hobo_files(
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

  #test expected warning when baro data is offset by a negative time
  cond_data_5_min_offset = cond_bound[[1]] %>% dplyr::mutate(timestamp = timestamp - (5 *
                                                                                        60))

  expect_warning(suppressMessages(
    add_nearest_baro(
      input_data = cond_data_5_min_offset,
      path_to_output_folder = path_to_output_folder,
      baro_data_path = path_to_output_folder,
      metric = "conductivity",
      baro_site_selection = "auto",
      metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")
    )
  ), "Baro data didnt match timestamps for 0.0% of timestamps for site DARL_ST_30.")

  #test expected warning when baro data is offset by a negative time
  cond_data_random_offset = cond_bound[[1]] %>% dplyr::mutate(timestamp = timestamp - runif(nrow(cond_bound[[1]]), min = 0, max = (30 *
                                                                                                                                     60)))

  expect_warning(suppressMessages(
    add_nearest_baro(
      input_data = cond_data_random_offset,
      path_to_output_folder = path_to_output_folder,
      baro_data_path = path_to_output_folder,
      metric = "conductivity",
      baro_site_selection = "auto",
      metadata_path = test_path("fixtures/metadata/test_cond_dry.csv")
    )
  ),
  "Baro data didnt match timestamps for 100.0% of timestamps for site DARL_ST_30.")

})
