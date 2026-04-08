# ============================================================================
# Tests for write_clean_data()
#
# This function exports the final "v1.0" clean data CSV files, split by
# station and year. It only includes the columns relevant to each metric
# (waterlevel, dissolved oxygen, conductivity, or barometric).
#
# PERFORMANCE NOTE: Water level tests use cached pipeline data from
# helper-pipeline-fixtures.R. Conductivity tests run their own pipeline
# because conductivity uses different fixtures and a separate processing path.
# ============================================================================

test_that("test write clean data", {
  # Get QAQC'd water-level data from the shared cache
  waterlevel_complete_QAQC <- get_wl_qaqc_data()

  # Use a FRESH temp directory for output — we need to verify that
  # write_clean_data() creates the correct folder structure and files.
  path_to_output_folder <- withr::local_tempdir()

  # --- Verify clean data folders do NOT exist yet ---------------------------
  expect_false(dir.exists(file.path(path_to_output_folder, "2024/clean")))
  expect_false(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  output.files <- list.files(path_to_output_folder, recursive = TRUE)
  matches <- grepl("2024/clean/ALBR_ST_30_WL", output.files,
                   ignore.case = FALSE, perl = TRUE)
  expect_false(any(matches))
  matches <- grepl("2025/clean/ALBR_ST_30_WL", output.files,
                   ignore.case = FALSE, perl = TRUE)
  expect_false(any(matches))

  # --- Write clean data and verify files were created -----------------------
  clean_WL <- write_clean_data(
    input_data            = waterlevel_complete_QAQC,
    path_to_output_folder = path_to_output_folder,
    metric                = "waterlevel"
  )

  # Clean data folders should now exist for both years in the test data
  expect_true(dir.exists(file.path(path_to_output_folder, "2024/clean")))
  expect_true(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  # Check that the output CSV files were created with correct naming
  output.files <- list.files(path_to_output_folder, recursive = TRUE)
  matches <- grepl("2024/clean/ALBR_ST_30_WL", output.files,
                   ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))
  matches <- grepl("2025/clean/ALBR_ST_30_WL", output.files,
                   ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))
})

test_that("test write clean data without qaqc", {
  # write_clean_data() requires QAQC'd data (with flag columns).
  # Passing un-QAQC'd data should produce a clear error message.

  # Get the conversion result — this is a named list, NOT a data frame.
  # Passing the list directly should error.
  converted_data <- get_wl_converted_data()
  path_to_output_folder <- withr::local_tempdir()

  expect_error(
    write_clean_data(
      input_data            = converted_data,
      path_to_output_folder = path_to_output_folder,
      metric                = "waterlevel"
    ),
    "Missing required column"
  )

  # Passing just the site_wl data frame (still missing QAQC columns)
  # should also error.
  expect_error(
    write_clean_data(
      input_data            = converted_data$site_wl,
      path_to_output_folder = path_to_output_folder,
      metric                = "waterlevel"
    ),
    "Missing required column"
  )
})

test_that("test write clean data for conductivity", {
  # Conductivity uses a DIFFERENT pipeline (no baro step), so we run
  # it from scratch here. This is not duplicated elsewhere in the tests.

  path_to_raw_folder    <- test_path("fixtures/COND")
  path_to_output_folder <- withr::local_tempdir()
  metadata_path         <- test_path("fixtures/testing_metadata_FP.csv")

  # Bind raw conductivity logger files
  cond_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder    = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path         = metadata_path
    )
  )

  sites_compiled <- cond_bound[[1]]

  # Run conductivity QAQC (flags out-of-range values, etc.)
  cond_complete_QAQC <- suppressMessages(
    qc_conductivity(
      sites_compiled,
      log_root       = path_to_output_folder,
      select_station = "MILE_WE_40"
    )
  )

  # Temperature compensation converts raw conductivity to specific conductance
  cond_with_spc <- convert_conductivity_spc_uScm(cond_complete_QAQC)
  expect_true("spc_uScm_adj" %in% names(cond_with_spc))

  # Verify no clean data exists before writing
  expect_false(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  # Write clean conductivity data
  write_clean_data(
    input_data            = cond_with_spc,
    path_to_output_folder = path_to_output_folder,
    metric                = "conductivity"
  )

  # Verify clean data was created with correct naming
  expect_true(dir.exists(file.path(path_to_output_folder, "2025/clean")))

  output.files <- list.files(path_to_output_folder, recursive = TRUE)
  matches <- grepl("2025/clean/MILE_WE_40_CO", output.files,
                   ignore.case = FALSE, perl = TRUE)
  expect_true(any(matches))

  # Verify that specific conductance (spc_uScm_adj) is in the written CSV
  csv_path <- file.path(path_to_output_folder, output.files[matches][1])
  written <- utils::read.csv(csv_path)
  expect_true("spc_uScm_adj" %in% names(written))
})

test_that("test write clean data for wrong metric", {
  # Passing an unsupported metric name should produce a clear error.

  path_to_raw_folder    <- test_path("fixtures/COND")
  path_to_output_folder <- withr::local_tempdir()
  metadata_path         <- test_path("fixtures/testing_metadata_FP.csv")

  cond_bound <- suppressMessages(
    bind_hobo_files(
      path_to_raw_folder    = path_to_raw_folder,
      path_to_output_folder = path_to_output_folder,
      metadata_path         = metadata_path
    )
  )

  sites_compiled <- cond_bound[[1]]

  expect_error(
    write_clean_data(
      input_data            = sites_compiled,
      path_to_output_folder = path_to_output_folder,
      metric                = "fish"
    ),
    "Unsupported metric"
  )
})
