# --- Issue #53: get_logger_data missing helpers + conductivity ----------------

# Helper: build minimal in-memory logger data
make_logger_df <- function(metric_cols) {
  n <- 24
  df <- data.frame(
    site_station_code = c(rep("TEST_ST_01", n), rep("TEST_ST_02", n*2)),
    timestamp         = c((as.POSIXct("2024-06-01", tz = "UTC") + (0:(n - 1)) * 3600), (as.POSIXct("2024-12-31", tz = "UTC") + (0:(n*2 - 1)) * 3600)),
    stringsAsFactors  = FALSE
  )
  for (nm in names(metric_cols)) {
    df[[nm]] <- metric_cols[[nm]]
  }
  df
}

# --- normalise_temporal_scale ------------------------------------------------
# move this to new test-summarize_logger_data
# test_that("#53 - normalise_temporal_scale returns canonical values", {
#   expect_equal(normalise_temporal_scale("none"), "none")
#   expect_equal(normalise_temporal_scale("Hourly"), "hourly")
#   expect_equal(normalise_temporal_scale(" Daily "), "daily")
#   expect_equal(normalise_temporal_scale("week"), "weekly")
#   expect_equal(normalise_temporal_scale("MONTHLY"), "monthly")
#   expect_equal(normalise_temporal_scale("annual"), "yearly")
# })

# test_that("#53 - normalise_temporal_scale rejects invalid input", {
#   expect_error(normalise_temporal_scale("biweekly"), "Unknown")
#   expect_error(normalise_temporal_scale(NULL), "non-NULL")
# })

# --- get_logger_data uses read path -----------

test_that("#53 - get_logger_data works with in-memory data", {
  df <- make_logger_df(list(waterlevel_m_adj = runif(24*3)))
  result <- get_logger_data(
    input_data      = df,
    data_processing = "v1.0",
    select_station = "TEST_ST_02",
    year = 2024
  )
  expect_equal(nrow(result), 24)
  expect_true("site_station_code" %in% names(result))
  expect_true(unique(result$site_station_code)=="TEST_ST_02")
})

test_that("get_logger_data works with file_path", {
  local_path <- test_path("fixtures", "test_get_logger_data")
  results <- get_logger_data(path_to_data = local_path,
                  metric = "baro",
                  select_station = "all",
                  data_processing = "clean")
  expect_equal(nrow(results), 20151)
})


test_that("get_logger_data works with file_path", {
  local_path <- testthat::test_path("fixtures", "test_get_logger_data")
  expect_error(get_logger_data(path_to_data = local_path,
                               metric = NULL,
                               select_station = "all",
                               data_processing = "v0.1"))
})

# --- summarisation path (uses summarise_clean_data + build_plots) ------------
# move to new test-summarize_logger_data
# test_that("#53 - get_logger_data summarises waterlevel daily", {
#   df <- make_logger_df(list(
#     waterlevel_m_adj = runif(24, 0.5, 1.5),
#     watertemp_C_adj  = runif(24, 10, 20)
#   ))
#   result <- get_logger_data(
#     input_data      = df,
#     data_processing = "v1.0",
#     metric          = "waterlevel",
#     temporal_scale  = "daily",
#     return_plots    = FALSE
#   )
#   expect_equal(nrow(result), 1)
#   expect_true("mean_waterlevel_m" %in% names(result))
# })
#
# test_that("#53 - get_logger_data summarises conductivity daily", {
#   df <- make_logger_df(list(
#     conduct_uScm_adj = runif(24, 100, 500),
#     spc_uScm_adj     = runif(24, 100, 500),
#     watertemp_C_adj  = runif(24, 10, 20)
#   ))
#   result <- get_logger_data(
#     input_data      = df,
#     data_processing = "v1.0",
#     metric          = "conductivity")
#
#   expect_equal(nrow(result), 1)
#   expect_true("mean_conduct_uScm" %in% names(result))
#   expect_true("mean_spc_uScm" %in% names(result))
#   expect_true("mean_watertemp_C" %in% names(result))
# })
#
# test_that("#53 - get_logger_data returns plots when requested", {
#   df <- make_logger_df(list(
#     waterlevel_m_adj = runif(24, 0.5, 1.5),
#     watertemp_C_adj  = runif(24, 10, 20)
#   ))
#   result <- get_logger_data(
#     input_data      = df,
#     data_processing = "v1.0",
#     metric          = "waterlevel",
#     temporal_scale  = "daily",
#     return_plots    = TRUE
#   )
#   expect_true(is.list(result))
#   expect_true(inherits(result$plot1, "ggplot"))
#   expect_true(inherits(result$plot2, "ggplot"))
#   expect_null(result$plot3)
# })

# --- normalise_metric with conductivity --------------------------------------

test_that("#53 - normalise_metric accepts conductivity aliases", {
  expect_equal(normalise_metric("conductivity")$metric_norm, "conductivity")
  expect_equal(normalise_metric("cond")$metric_norm, "conductivity")
  expect_equal(normalise_metric("spc")$metric_norm, "conductivity")
})
