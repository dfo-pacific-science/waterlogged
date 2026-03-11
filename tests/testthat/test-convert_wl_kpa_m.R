# ============================================================================
# Tests for convert_waterlevel_kPa_m()
#
# This function converts raw pressure readings (kPa) to water level (metres)
# using field reference measurements for calibration.
#
# PERFORMANCE NOTE: The expensive pipeline steps (bind + add_nearest_baro)
# are loaded from the shared cache in helper-pipeline-fixtures.R.
# ============================================================================

test_that("test convert wl kpa m", {
  # --- Check that raw bound data does NOT have waterlevel_m yet -------------
  # At this stage the data only has waterpressure_kPa (raw logger reading).
  wl_data <- get_wl_level_bound_df()
  has_waterlevel_column <- "waterlevel_m" %in% colnames(wl_data)
  expect_false(has_waterlevel_column)

  # --- Check that adding baro still doesn't create waterlevel_m -------------
  # add_nearest_baro attaches air pressure but does NOT convert to depth.
  wl_with_baro <- get_wl_with_baro_df()
  has_waterlevel_column <- "waterlevel_m" %in% colnames(wl_with_baro)
  expect_false(has_waterlevel_column)

  # --- Check outputs from the conversion step -------------------------------
  # convert_waterlevel_kPa_m returns a list with three elements:
  #   $site_wl  — the data frame with new waterlevel_m column
  #   $ref_dat  — the reference measurement data used for calibration
  #   $wl_plot  — a plotly plot comparing logger vs reference readings
  converted_data <- get_wl_converted_data()

  expect_s3_class(converted_data$site_wl, "data.frame")
  expect_s3_class(converted_data$ref_dat, "data.frame")
  expect_s3_class(converted_data$wl_plot, "plotly")

  # --- After conversion, waterlevel_m should now exist ----------------------
  has_waterlevel_column <- "waterlevel_m" %in% colnames(converted_data$site_wl)
  expect_true(has_waterlevel_column)
})

test_that("test convert wl kpa m without baro data", {
  # If a user tries to convert water level without first attaching
  # barometric data, the function should throw a clear error.
  # We use the raw bound data (no baro) to trigger this error.
  level_bound_df <- get_wl_level_bound_df()

  # convert_waterlevel_kPa_m needs airpress_kPa from add_nearest_baro.
  # Without it, it should error with a helpful message.
  path_to_output_folder <- withr::local_tempdir()
  reference_data <- test_path("fixtures/NT_manual_waterlevel_20251119.csv")

  expect_error(
    convert_waterlevel_kPa_m(
      input_data            = level_bound_df,
      select_station        = "all",
      reference_data        = reference_data,
      reference_type        = "stage",
      select_measurement    = 1,
      logger_type_expected  = "u20",
      path_to_output_folder = path_to_output_folder
    ),
    "input_data is missing required column"
  )
})
