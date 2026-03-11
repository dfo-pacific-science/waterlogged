# ============================================================================
# Tests for convert_do_mgl_percsat()
#
# This function converts raw dissolved oxygen concentration (mg/L) to
# percent saturation using barometric pressure compensation. It requires
# air pressure data from add_nearest_baro() to be present.
#
# PERFORMANCE NOTE: Uses cached DO pipeline data from
# helper-pipeline-fixtures.R.
# ============================================================================

test_that("Test convert do mgl percsat", {
  # Get the fully converted DO data from the shared cache.
  # This has already been through: bind -> add_baro -> convert.
  converted_percSat <- get_do_converted_df()

  # The do_percsat column should now exist (it was computed from do_mgl
  # and airpress_kPa using Henry's Law and temperature correction).
  expect_contains(colnames(converted_percSat), "do_percsat")
})

test_that("Check for required air pressure column", {
  # If a user tries to convert DO without first attaching barometric data,
  # the function should throw a clear error about the missing column.
  # We use the raw bound DO data (no baro attached) to trigger this.
  do_bound_df <- get_do_bound_df()

  path_to_output_folder <- withr::local_tempdir()

  expect_error(
    convert_do_mgl_percsat(
      do_bound_df,
      path_to_output_folder = path_to_output_folder,
      version_label         = "v0.3"
    ),
    "Missing required columns: airpress_kPa"
  )
})
