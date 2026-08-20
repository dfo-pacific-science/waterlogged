# ============================================================================
# Tests for plot_qc_timeseries()
#
# These tests verify that the QAQC review plot can be generated in both
# interactive (plotly) and static (patchwork) modes.
#
# PERFORMANCE NOTE: Instead of running the full water level pipeline here
# (which takes ~15 seconds), we use pre-computed data from
# helper-pipeline-fixtures.R. See that file for details on what data
# is cached and how.
# ============================================================================

test_that("test plot qaqc water level", {
  # Retrieve QAQC'd water-level data from the shared cache.
  # This data has already been through: bind -> add_baro -> convert -> QAQC.
  waterlevel_complete_QAQC <- get_wl_qaqc_data()

  # Generate an interactive plotly review plot for station ALBR_ST_30
  plots <- plot_qc_timeseries(
    wl_data        = waterlevel_complete_QAQC,
    do_data        = NULL,
    baro_data      = NULL,
    select_station = "ALBR_ST_30"
  )

  # The result should be a plotly object (interactive HTML widget)
  expect_s3_class(plots, "plotly")
})

test_that("plot_qc_timeseries static mode returns patchwork object", {
  skip_if_not_installed("patchwork")

  # Same cached data, but request a static (non-interactive) plot
  waterlevel_complete_QAQC <- get_wl_qaqc_data()

  static_plot <- plot_qc_timeseries(
    wl_data        = waterlevel_complete_QAQC,
    do_data        = NULL,
    baro_data      = NULL,
    select_station = "ALBR_ST_30",
    static         = TRUE
  )

  # Static plots use the patchwork package (which extends ggplot2)
  expect_s3_class(static_plot, "patchwork")
  expect_true(inherits(static_plot, "gg"))
})
