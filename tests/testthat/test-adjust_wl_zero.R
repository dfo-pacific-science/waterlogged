# ============================================================================
# Tests for adjust_waterlevel_zero()
#
# This function shifts the water level baseline (zero datum) by a specified
# amount, either across the full time series or within a specific time window.
# Common use case: the stilling well was physically moved, so all readings
# after that date need a vertical offset correction.
#
# PERFORMANCE NOTE: Uses cached QAQC data from helper-pipeline-fixtures.R.
# ============================================================================

test_that("test adjust wl zero", {
  # Get QAQC'd water-level data from the shared cache.
  # This is a data frame with waterlevel_m, waterlevel_m_adj, and flag columns.
  waterlevel_complete_QAQC <- get_wl_qaqc_data()

  # Create a fresh output folder for the QAQC log that adjust_waterlevel_zero writes
  path_to_output_folder <- withr::local_tempdir()

  # --- Test 1: Adjust within a specific time window -------------------------
  # Shift water level UP by 1 metre between Nov 1 and Dec 31, 2024.
  # This simulates a stilling well that was physically raised.
  timestamp_start <- "2024-11-01 00:00:00"
  timestamp_end   <- "2024-12-31 00:00:00"

  adjusted_wl <- adjust_waterlevel_zero(
    waterlevel_complete_QAQC,
    select_station  = "ALBR_ST_30",
    change_value    = 1,
    timestamp_start = timestamp_start,
    timestamp_end   = timestamp_end,
    manual_note     = "stilling well moved up to clear mud",
    log_root        = path_to_output_folder
  )

  # Verify the plot still works after adjustment
  plot_qaqc_timeseries(adjusted_wl, select_station = "ALBR_ST_30")

  # Calculate the actual adjustment applied (difference between adj and raw)
  adjusted_wl <- adjusted_wl %>%
    dplyr::mutate(adjustment = waterlevel_m_adj - waterlevel_m)

  ts_start <- lubridate::ymd_hms(timestamp_start)
  ts_end   <- lubridate::ymd_hms(timestamp_end)

  # Outside the time window: adjustment should be exactly 0
  wl_unadjusted_time <- adjusted_wl %>%
    dplyr::filter(!is.na(waterlevel_m) &
                    (timestamp < ts_start | timestamp > ts_end))
  expect_true(all(wl_unadjusted_time$adjustment == 0))

  # Inside the time window: adjustment should be exactly 1
  wl_adjusted_time <- adjusted_wl %>%
    dplyr::filter(!is.na(waterlevel_m) &
                    timestamp >= ts_start & timestamp <= ts_end)
  expect_true(all(wl_adjusted_time$adjustment == 1))

  # --- Test 2: Adjust entire time series (no timestamp bounds) --------------
  # When no start/end timestamps are given, ALL readings should be shifted.
  adjusted_wl_no_timestamps <- adjust_waterlevel_zero(
    waterlevel_complete_QAQC,
    select_station = "ALBR_ST_30",
    change_value   = 1,
    manual_note    = "stilling well moved up to clear mud",
    log_root       = path_to_output_folder
  )

  wl_adjusted_time <- adjusted_wl_no_timestamps %>%
    dplyr::filter(!is.na(waterlevel_m)) %>%
    dplyr::mutate(adjustment = waterlevel_m_adj - waterlevel_m)
  wl_adjusted_time$adjustment <- round(wl_adjusted_time$adjustment, 3)

  # Every single reading should have the +1 adjustment
  expect_true(all(wl_adjusted_time$adjustment == 1))
})
