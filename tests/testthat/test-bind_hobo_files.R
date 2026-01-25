#source("../../R/utils.R")
#source("../../R/bind_hobo_files_helpers.R")
#source("../../R/bind_hobo_files.r")
library(ggplot2)

#see https://r-pkgs.org/testing-design.html#storing-test-data for best practices for storing data

test_that("Test binding barometric files", {

  path_to_raw_folder = test_path("fixtures/data/testing/raw/baro")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/data/testing/raw/testing_metadata_FP.csv")

  baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                                path_to_output_folder = path_to_output_folder,
                                metadata_path = metadata_path)

  sites_compiled = baro_bound[[1]]
  multi_plots = baro_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test binding DO files", {

  path_to_raw_folder = test_path("fixtures/data/testing/raw/DO")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/data/testing/raw/testing_metadata_FP.csv")

  DO_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                              path_to_output_folder = path_to_output_folder,
                              metadata_path = metadata_path)

  sites_compiled = DO_bound[[1]]
  multi_plots = DO_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test missing metadata", {

  path_to_raw_folder = test_path("fixtures/data/testing/raw/test_missing_metadata")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/data/testing/metadata/test_missing_metadata.csv")


  expect_error(bind_hobo_files(path_to_raw_folder = path_to_raw_folder,
                               path_to_output_folder = path_to_output_folder,
                               metadata_path = metadata_path),
               "Metadata missing for logger serial number:")

})
