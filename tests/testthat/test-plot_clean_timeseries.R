#move to plot_clean_data
# Helper: build minimal in-memory logger data
make_logger_df <- function(metric_cols) {
  n <- 24
  df <- data.frame(
    site_station_code = c(rep("TEST_ST_01", n), rep("TEST_ST_02", n*2)),
    time_group        = c((as.POSIXct("2024-06-01", tz = "UTC") + (0:(n - 1)) * 86400), (as.POSIXct("2024-12-31", tz = "UTC") + (0:(n*2 - 1)) * 86400)),
    timestamp = c((as.POSIXct("2024-06-01", tz = "UTC") + (0:(n - 1)) * 3600), (as.POSIXct("2024-12-31", tz = "UTC") + (0:(n*2 - 1)) * 3600)),
    stringsAsFactors  = FALSE
  )
  for (nm in names(metric_cols)) {
    df[[nm]] <- metric_cols[[nm]]
  }
  df
}

test_that("#53 - plot_clean_timeseries returns summary plots when requested", {
  df <- make_logger_df(list(
    waterlevel_m_adj_mean = runif(24, 0.5, 1.5),
    waterlevel_m_adj_q5 = runif(24, 0.3, 1.1),
    waterlevel_m_adj_q95 = runif(24, 0.9, 1.9),
    watertemp_C_adj  = runif(24, 10, 20)
  ))
 result <- plot_clean_timeseries(
    input_data      = df)

  expect_true(is.list(result))
  # has timegroup _mean _q5 _q95 columns
  expect_true(inherits(result$plot1, "ggplot"))
  # missing required _q5 _q95 columns
  expect_true(is.character(result$plot2))
  expect_null(result$plot3)
})

test_that("#53 - plot_clean_timeseries returns clean plots when requested", {
  df <- make_logger_df(list(
    watertemp_C  = runif(24, 10, 20),
    watertemp_C_adj  = runif(24, 10, 20)
  ))
  df <- subset(df, select =-time_group)

  result <- plot_clean_timeseries(
    input_data      = df)

  expect_true(is.list(result))
  expect_true(inherits(result$plot1, "ggplot"))
  expect_null(result$plot2)
})
