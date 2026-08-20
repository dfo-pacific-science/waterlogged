# ============================================================================
# Tests for qc_dissox()
#
# This function runs automated quality checks on converted dissolved oxygen
# data. It adds flag columns for common issues (sensor errors, ice cover,
# dry periods, negative values, etc.) and writes a QAQC log CSV.
#
# PERFORMANCE NOTE: The main test uses cached converted DO data from
# helper-pipeline-fixtures.R. Error-path tests that use different fixtures
# (e.g., missing baro data) run their own lightweight bind step.
# ============================================================================

test_that("Test dissox QAQC", {
  # Get converted DO data from the shared cache.
  # This data has do_mgl, do_percsat, and airpress_kPa columns.
  converted_percSat <- get_do_converted_df()

  # --- Verify QAQC flag columns do NOT exist before running QAQC -----------
  qa.columns <- c(
    "ERROR_DO",
    "OUTLIER_DO",
    "OUTLIER_WT",
    "FLAG_RANGE",
    "FLAG_ICE",
    "FLAG_DRY"
  )
  missing_cols <- setdiff(qa.columns, colnames(converted_percSat))
  expect_equal(missing_cols, qa.columns)

  # Use a fresh temp directory so we can verify log file creation
  path_to_output_folder <- withr::local_tempdir()

  # The QAQC log should NOT exist yet
  expect_false(file.exists(
    file.path(path_to_output_folder, "logs/ALBR_ST_30_DO_qaqc_log.csv")
  ))

  # --- Run QAQC on the converted DO data -----------------------------------
  dissox_complete_QAQC <- qc_dissox(
    converted_percSat,
    select_station = "ALBR_ST_30",
    log_root       = path_to_output_folder
  )

  # Should return a data frame
  expect_s3_class(dissox_complete_QAQC, "data.frame")

  # --- Verify all QAQC flag columns now exist -------------------------------
  missing_cols <- setdiff(qa.columns, colnames(dissox_complete_QAQC))
  expect_equal(length(missing_cols), 0)

  # --- Verify the QAQC log file was created ---------------------------------
  expect_true(file.exists(
    file.path(path_to_output_folder, "logs/ALBR_ST_30_DO_qaqc_log.csv")
  ))
})

# Removed these tests because users may not want/need to convert do to percsat...
# they should still be able to process do data
#
# test_that("Test dissox QAQC missing baro data", {
#   # This test uses DIFFERENT fixtures (fixtures/DO with testing_metadata_FP)
#   # to verify the error when baro data is completely missing.
#   # It's a quick bind step, not worth caching.
#
#   path_to_raw_folder    <- test_path("fixtures/DO")
#   path_to_output_folder <- withr::local_tempdir()
#   metadata_path         <- test_path("fixtures/testing_metadata_FP.csv")
#
#   DO_bound <- bind_hobo_files(
#     path_to_raw_folder    = path_to_raw_folder,
#     path_to_output_folder = path_to_output_folder,
#     metadata_path         = metadata_path
#   )
#
#   sites_compiled <- DO_bound[[1]]
#
#   # Without baro data, qc_dissox should error because it needs
#   # do_percsat (from conversion) and airtemp_C (from baro).
#   expect_error(
#     qc_dissox(sites_compiled, select_station = "ALBR_ST_30"),
#     "input_data is missing required columns: do_percsat, airtemp_C"
#   )
# })

<<<<<<< HEAD
# test_that("Test dissox QAQC missing conversion", {
#   # If DO data has baro attached but convert_do_mgl_percsat() was never
#   # called, qc_dissox should error about the missing do_percsat column.
#   do_with_baro_df <- get_do_with_baro_df()
#
#   expect_error(
#     qc_dissox(do_with_baro_df, select_station = "ALBR_ST_30"),
#     "input_data is missing required columns: do_percsat"
#   )
# })
=======
  path_to_raw_folder    <- test_path("fixtures/DO")
  path_to_output_folder <- withr::local_tempdir()
  metadata_path         <- test_path("fixtures/testing_metadata_FP.csv")

  DO_bound <- bind_logger_files(
    path_to_raw_folder    = path_to_raw_folder,
    path_to_output_folder = path_to_output_folder,
    metadata_path         = metadata_path
  )

  sites_compiled <- DO_bound[[1]]

  # Without baro data, qc_dissox should error because it needs
  # do_percsat (from conversion) and airtemp_C (from baro).
  expect_error(
    qc_dissox(sites_compiled),
    "input_data is missing required columns: do_percsat, airtemp_C"
  )
})

test_that("Test dissox QAQC missing conversion", {
  # If DO data has baro attached but convert_do_mgl_percsat() was never
  # called, qc_dissox should error about the missing do_percsat column.
  do_with_baro_df <- get_do_with_baro_df()

  expect_error(
    qc_dissox(do_with_baro_df),
    "input_data is missing required columns: do_percsat"
  )
})
>>>>>>> fd5e8d1 (update tests with name changes. All pass.)
