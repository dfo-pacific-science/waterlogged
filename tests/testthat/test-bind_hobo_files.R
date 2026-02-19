#source("../../R/utils.R")
#source("../../R/bind_hobo_files_helpers.R")
#source("../../R/bind_hobo_files.r")
library(ggplot2)

#see https://r-pkgs.org/testing-design.html#storing-test-data for best practices for storing data

test_that("Test binding barometric files", {

  path_to_raw_folder = test_path("fixtures/baro")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)

  sites_compiled = baro_bound[[1]]
  multi_plots = baro_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test binding DO files", {

  path_to_raw_folder = test_path("fixtures/DO")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  DO_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = metadata_path)

  sites_compiled = DO_bound[[1]]
  multi_plots = DO_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test binding COND files", {

  path_to_raw_folder = test_path("fixtures/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  cond_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)

  sites_compiled = cond_bound[[1]]
  multi_plots = cond_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")
})

test_that("Test missing metadata", {

  path_to_raw_folder = test_path("fixtures/test_missing_metadata")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/test_missing_metadata.csv")


  expect_error(bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                               path_to_output_folder = path_to_output_folder,
                               metadata_path = metadata_path),
               "Metadata missing for logger serial number:")

})

test_that("Test multiple metric types", {

  path_to_raw_folder = test_path("fixtures/test_multiple_metric_types")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/test_multiple_metric_types.csv")


  expect_error(bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                               path_to_output_folder = path_to_output_folder,
                               metadata_path = metadata_path),
               "Multiple metric types found for these loggers:")

})

test_that("Test no csv files in input folder", {

  path_to_raw_folder = test_path("fixtures/no_csv_files_in_input_folder")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/no_csv_files_in_input_folder.csv")

  expect_error(bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                               path_to_output_folder = path_to_output_folder,
                               metadata_path = metadata_path),
               "No CSV files found in input folder.")

})

test_that("Test multiple date formats", {

  path_to_raw_folder = test_path("fixtures/test_multiple_date_formats/wrong_format")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/test_multiple_date_formats.csv")

  timestamp_wrong_format_bound = bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                  path_to_output_folder = path_to_output_folder,
                  metadata_path = metadata_path)

  timestamp_wrong_format_compiled = timestamp_wrong_format_bound[[1]] |> dplyr::select(-file_name) #remove file name since it wont be identical between the two files

  path_to_raw_folder = test_path("fixtures/test_multiple_date_formats/correct_format")

  timestamp_correct_format_bound = bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                                 path_to_output_folder = path_to_output_folder,
                                                 metadata_path = metadata_path)

  timestamp_correct_format_compiled = timestamp_correct_format_bound[[1]] |> dplyr::select(-file_name) #remove file name since it wont be identical between the two files

  expect_equal(timestamp_wrong_format_compiled, timestamp_correct_format_compiled)

})
