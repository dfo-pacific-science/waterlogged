# ============================================================================
# Tests for qc_waterlevel()
#
# This function runs automated quality checks on converted water-level data.
# It adds flag columns (flag_ice, flag_disturbance, flag_wl_dry, etc.) and
# writes a QAQC log CSV summarizing what was flagged.
#
# PERFORMANCE NOTE: We use cached conversion results from
# helper-pipeline-fixtures.R, but run qc_waterlevel() here with a fresh
# temp directory so we can verify that the QAQC log file gets created.
# ============================================================================

test_that("test waterlevel qaqc", {
  # Get pre-computed converted data (the expensive pipeline already ran).
  # This is the output of convert_waterlevel_kPa_m() — a list where [[1]]
  # is the data frame with waterlevel_m but NO QAQC flags yet.
  converted_data <- get_wl_converted_data()

  # --- Verify that QAQC flag columns do NOT exist before running QAQC ------
  qa.columns <- c("FLAG_DISTURBANCE", "FLAG_ICE", "FLAG_DRY",
                   "OUTLIER_WT")
  missing_cols <- setdiff(qa.columns, colnames(converted_data))
  expect_equal(missing_cols, qa.columns)

  # --- Use a fresh temp directory so we can check log file creation ---------
  # We need our OWN output folder because we want to verify that
  # qc_waterlevel() creates the log file from scratch.
  path_to_output_folder <- withr::local_tempdir()

  # The log should NOT exist yet in our fresh directory
  expect_false(file.exists(
    file.path(path_to_output_folder, "logs/ALBR_ST_30_WL_qaqc_log.csv")
  ))

  # --- Run QAQC on the converted data --------------------------------------
  waterlevel_complete_QAQC <- qc_waterlevel(
    converted_data[[1]],
    log_root       = path_to_output_folder,
    select_station = "ALBR_ST_30"
  )

  # --- Verify that all QAQC flag columns now exist --------------------------
  missing_cols <- setdiff(qa.columns, colnames(waterlevel_complete_QAQC))
  expect_equal(length(missing_cols), 0)

  # --- Verify that the QAQC log file was written ----------------------------
  expect_true(file.exists(
    file.path(path_to_output_folder, "logs/ALBR_ST_30_WL_qaqc_log.csv")
  ))
})
