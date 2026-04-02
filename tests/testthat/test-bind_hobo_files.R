#source("../../R/utils.R")
#source("../../R/bind_hobo_files_helpers.R")
#source("../../R/bind_hobo_files.r")
library(ggplot2)

#see https://r-pkgs.org/testing-design.html#storing-test-data for best practices for storing data

test_that("Test binding barometric files", {
  path_to_raw_folder = test_path("fixtures/baro")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  baro_bound <- bind_hobo_files(
    path_to_raw_folder = path_to_raw_folder,
    path_to_output_folder = path_to_output_folder,
    metadata_path = metadata_path
  )

  sites_compiled = baro_bound[[1]]
  multi_plots = baro_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test binding DO files", {
  path_to_raw_folder = test_path("fixtures/DO")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  DO_bound <- bind_hobo_files(
    path_to_raw_folder = path_to_raw_folder,
    path_to_output_folder = path_to_output_folder,
    metadata_path = metadata_path
  )

  sites_compiled = DO_bound[[1]]
  multi_plots = DO_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})

test_that("Test binding COND files", {
  path_to_raw_folder = test_path("fixtures/COND")
  path_to_output_folder = withr::local_tempdir()
  metadata_path = test_path("fixtures/testing_metadata_FP.csv")

  cond_bound <- bind_hobo_files(
    path_to_raw_folder = path_to_raw_folder,
    path_to_output_folder = path_to_output_folder,
    metadata_path = metadata_path
  )

  sites_compiled = cond_bound[[1]]
  multi_plots = cond_bound[[2]]

  expect_s3_class(sites_compiled, "data.frame")
  expect_s3_class(multi_plots, "ggplot")

})
#
# test_that("Test missing metadata", {
#   path_to_raw_folder = test_path("fixtures/test_missing_metadata")
#
#   path_to_output_folder = withr::local_tempdir()
#
#   metadata_path = test_path("fixtures/metadata/test_missing_metadata.csv")
#
#
#   expect_error(
#     bind_hobo_files(
#       path_to_raw_folder = path_to_raw_folder,
#       path_to_output_folder = path_to_output_folder,
#       metadata_path = metadata_path
#     ),
#     "Multiple metric types found for these loggers:
# Make sure your input folder only contains one metric type (e.g., all barometric or all waterlevel).
# If a serial number was redeployed for a different metric, check that the raw files and metadata deployment timestamps are consistent."
#   )
#
# })

test_that("Test multiple metric types", {
  path_to_raw_folder = test_path("fixtures/test_multiple_metric_types")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/test_multiple_metric_types.csv")


  expect_error(
    bind_hobo_files(
      path_to_raw_folder = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path = metadata_path
    ),
    "Multiple metric types found for these loggers:"
  )

})

test_that("Multi-metric SN resolved by deployment timestamp overlap (#49)",
          {
            # SN 20810501 appears in issue 02 metadata as both barometric (2022-ongoing

            # at ALBR) and waterlevel (Jul-Sep 2021 at CANV). The baro files contain
            # data from 2023-2024, so only the barometric metadata row overlaps.
            # bind_hobo_files() should filter out the non-overlapping waterlevel row and
            # succeed.

            issue_dir <- normalizePath(file.path(test_path(), "..", "..", "..", "issues", "02", "data"),
                                       mustWork = FALSE)

            # Skip if issue data not available (CI or other machines)
            skip_if_not(dir.exists(file.path(issue_dir, "baro")), "Issue 02 test data not available")

            result <- suppressMessages(bind_hobo_files(
              path_to_raw_folder = file.path(issue_dir, "baro"),
              metadata_path      = file.path(
                issue_dir,
                "NorthThompson_logger_deployments_20251105.csv"
              )
            ))

            df <- result[[1]]

            # Should succeed and return barometric data for 3 stations
            expect_s3_class(df, "data.frame")
            expect_true("airpress_kPa" %in% names(df))
            expect_equal(unique(df$metric), "barometric")

            # SN 20810501 should be assigned to ALBR_ST_30 (baro), not CANV_ST_30 (WL)
            sn_501 <- df[df$sn == "20810501", ]
            expect_true(all(sn_501$site_station_code == "ALBR_ST_30"))
          })

test_that("Mixed Low/Full Range conductivity files merge correctly (#49)", {
  # Two conductivity files: BARR has Low Range, PYRA has Full Range.
  # bind_hobo_files() should merge both into a single conduct_uScm column
  # and drop the raw Low Range / Full Range columns.

  issue_dir <- normalizePath(file.path(test_path(), "..", "..", "..", "issues", "02", "data"),
                             mustWork = FALSE)

  skip_if_not(file.exists(
    file.path(issue_dir, "CO_BARR_WE_30_21245028_20251024.csv")
  ),
  "Issue 02 conductivity test data not available")

  result <- suppressWarnings(suppressMessages(bind_hobo_files(
    file_paths    = c(
      file.path(issue_dir, "CO_BARR_WE_30_21245028_20251024.csv"),
      file.path(issue_dir, "CO_PYRA_WE_30_21245033_20251022.csv")
    ),
    metadata_path = file.path(
      issue_dir,
      "NorthThompson_logger_deployments_20251105.csv"
    )
  )))

  df <- result[[1]]

  # conduct_uScm should exist and be fully populated for both stations
  expect_true("conduct_uScm" %in% names(df))
  expect_true("conduct_source" %in% names(df))
  expect_true("watertemp_C" %in% names(df))
  expect_false(any(is.na(df$conduct_uScm)))

  # No leftover raw range columns
  expect_false(any(grepl("Low Range", names(df))))
  expect_false(any(grepl("Full Range", names(df))))
  expect_false(any(grepl("^Temp", names(df))))

  # Both stations present with correct sources
  by_station <- split(df$conduct_source, df$site_station_code)
  expect_true(all(by_station[["BARR_WE_30"]] == "low"))
  expect_true(all(by_station[["PYRA_WE_30"]] == "full"))
})

test_that("No csv files in input folder", {
  path_to_raw_folder = test_path("fixtures/no_csv_files_in_input_folder")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/no_csv_files_in_input_folder.csv")

  expect_error(
    bind_hobo_files(
      path_to_raw_folder = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path = metadata_path
    ),
    "No CSV files found in input folder."
  )

})

test_that("Test multiple date formats", {
  path_to_raw_folder = test_path("fixtures/test_multiple_date_formats/wrong_format")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/test_multiple_date_formats.csv")

  timestamp_wrong_format_bound = bind_hobo_files(
    path_to_raw_folder = path_to_raw_folder,
    path_to_output_folder = path_to_output_folder,
    metadata_path = metadata_path
  )

  timestamp_wrong_format_compiled = timestamp_wrong_format_bound[[1]]

  path_to_raw_folder = test_path("fixtures/test_multiple_date_formats/correct_format")

  timestamp_correct_format_bound = bind_hobo_files(
    path_to_raw_folder = path_to_raw_folder,
    path_to_output_folder = path_to_output_folder,
    metadata_path = metadata_path
  )

  timestamp_correct_format_compiled = timestamp_correct_format_bound[[1]]

  expect_equal(timestamp_wrong_format_compiled,
               timestamp_correct_format_compiled)

})

test_that("Test issue 70", {
  path_to_raw_folder = test_path("fixtures/test_issue_70")

  path_to_output_folder = withr::local_tempdir()

  metadata_path = test_path("fixtures/metadata/NorthThompson_logger_deployments_20251105.csv")

  expect_no_warning(suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path = metadata_path
    )
  ), message = "Some files are in different units")

  at_bound = suppressWarnings(suppressMessages(
    bind_hobo_files(
      path_to_raw_folder = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path = metadata_path
    )
  ))


  at_dat = at_bound[[1]]

  expect_contains(colnames(at_dat), "airtemp_C")


})
