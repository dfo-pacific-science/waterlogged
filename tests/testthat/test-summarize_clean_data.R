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
 test_that("#53 - normalise_temporal_scale returns canonical values", {
   expect_equal(normalise_temporal_scale("Hourly"), "hourly")
   expect_equal(normalise_temporal_scale(" Daily "), "daily")
   expect_equal(normalise_temporal_scale("week"), "weekly")
   expect_equal(normalise_temporal_scale("MONTHLY"), "monthly")
   expect_equal(normalise_temporal_scale("annual"), "yearly")
 })

 test_that("#53 - normalise_temporal_scale rejects invalid input", {
   expect_error(normalise_temporal_scale("biweekly"), "Unknown")
   expect_error(normalise_temporal_scale(NULL), "non-NULL")
 })
# --- summarisation path (uses summarise_clean_data + build_plots) ------------
 test_that("#53 - summarise_clean_data summarises waterlevel daily", {
   df <- make_logger_df(list(
     waterlevel_m_adj = runif(24, 0.5, 1.5),
     watertemp_C_adj  = runif(24, 10, 20)
   ))
   result <- summarise_clean_data(
     input_data      = df
   )
   expect_equal(nrow(result), 3)
   expect_true("waterlevel_m_adj_mean" %in% names(result))
 })
#
 test_that("#53 - summarise_clean_data summarises conductivity daily", {
   df <- make_logger_df(list(
     conduct_uScm_adj = runif(24, 100, 500),
     spc_uScm_adj     = runif(24, 100, 500),
     watertemp_C_adj  = runif(24, 10, 20)
   ))
   result <- summarise_clean_data(
     input_data      = df)

   expect_equal(nrow(result), 3)
   expect_true("conduct_uScm_adj_mean" %in% names(result))
   expect_true("spc_uScm_adj_mean" %in% names(result))
   expect_true("watertemp_C_adj_mean" %in% names(result))
 })

 #
 test_that("#53 - summarise_clean_data throws error if temporal scale same level as data", {
   df <- make_logger_df(list(
     conduct_uScm_adj = runif(24, 100, 500),
     spc_uScm_adj     = runif(24, 100, 500),
     watertemp_C_adj  = runif(24, 10, 20)
   ))
   expect_error(summarise_clean_data(
     input_data      = df,
     temporal_scale = "hourly"))

 })

