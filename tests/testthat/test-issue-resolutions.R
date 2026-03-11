# --- Issue #78: make_qaqc_log_row non-contiguous flag aggregation -------------

test_that("#78 - contiguous timestamps produce a single log row", {
  ts <- as.POSIXct(c("2024-06-01 00:00", "2024-06-01 01:00", "2024-06-01 02:00"),
                   tz = "UTC")
  result <- make_qaqc_log_row(
    timestamps  = ts,
    station     = "TEST_ST_01",
    metric      = "WL",
    field       = "waterlevel_m_adj",
    action      = "AUTO_QAQC",
    code        = "FLAG_TEST",
    action_note = "test",
    manual_note = "test",
    fun_name    = "test_fun",
    user        = "tester"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$ts_start, ts[1])
  expect_equal(result$ts_end, ts[3])
})

test_that("#78 - non-contiguous timestamps produce multiple log rows", {
  # Two runs separated by a large gap:
  # Run 1: 00:00, 01:00   Run 2: 10:00, 11:00
  ts <- as.POSIXct(c("2024-06-01 00:00", "2024-06-01 01:00",
                      "2024-06-01 10:00", "2024-06-01 11:00"),
                   tz = "UTC")
  result <- make_qaqc_log_row(
    timestamps  = ts,
    station     = "TEST_ST_01",
    metric      = "WL",
    field       = "waterlevel_m_adj",
    action      = "AUTO_QAQC",
    code        = "FLAG_TEST",
    action_note = "test",
    manual_note = "test",
    fun_name    = "test_fun",
    user        = "tester"
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$ts_start[1], ts[1])
  expect_equal(result$ts_end[1],   ts[2])
  expect_equal(result$ts_start[2], ts[3])
  expect_equal(result$ts_end[2],   ts[4])
})

test_that("#78 - empty / all-NA timestamps return NULL", {
  result_empty <- make_qaqc_log_row(
    timestamps  = as.POSIXct(character(0)),
    station     = "TEST_ST_01",
    metric      = "WL",
    field       = "waterlevel_m_adj",
    action      = "AUTO_QAQC",
    code        = "FLAG_TEST",
    action_note = "test",
    manual_note = "test",
    fun_name    = "test_fun",
    user        = "tester"
  )
  expect_null(result_empty)

  result_na <- make_qaqc_log_row(
    timestamps  = as.POSIXct(c(NA, NA)),
    station     = "TEST_ST_01",
    metric      = "WL",
    field       = "waterlevel_m_adj",
    action      = "AUTO_QAQC",
    code        = "FLAG_TEST",
    action_note = "test",
    manual_note = "test",
    fun_name    = "test_fun",
    user        = "tester"
  )
  expect_null(result_na)
})

test_that("#78 - three separate runs are split correctly", {
  # Run 1: 00:00, 01:00  |  Run 2: 06:00  |  Run 3: 12:00, 13:00
  ts <- as.POSIXct(c("2024-06-01 00:00", "2024-06-01 01:00",
                      "2024-06-01 06:00",
                      "2024-06-01 12:00", "2024-06-01 13:00"),
                   tz = "UTC")
  result <- make_qaqc_log_row(
    timestamps  = ts,
    station     = "TEST_ST_01",
    metric      = "WL",
    field       = "waterlevel_m_adj",
    action      = "AUTO_QAQC",
    code        = "FLAG_TEST",
    action_note = "test",
    manual_note = "test",
    fun_name    = "test_fun",
    user        = "tester"
  )
  expect_equal(nrow(result), 3)
  expect_equal(result$ts_start[2], ts[3])
  expect_equal(result$ts_end[2],   ts[3])
  expect_equal(result$duration_hours[2], 0)
})

# --- Issue #73: adjust_logger_NA multiple fixes -------------------------------

# Minimal helper to build test data for adjust_logger_NA tests
make_test_df <- function(extra_cols = list()) {
  n <- 5
  df <- data.frame(
    site_station_code = rep("TEST_ST_01", n),
    timestamp = as.POSIXct("2024-06-01 00:00:00", tz = "UTC") +
      (seq_len(n) - 1) * 3600
  )
  for (nm in names(extra_cols)) {
    df[[nm]] <- extra_cols[[nm]]
  }
  df
}

test_that("#73 - COND uses conduct_uScm_adj (not conductivity_uScm_adj)", {
  df <- make_test_df(list(
    conduct_uScm_adj = c(100, 200, 300, 400, 500),
    watertemp_C_adj  = c(10, 11, 12, 13, 14)
  ))
  result <- suppressWarnings(adjust_logger_NA(
    input_data      = df,
    select_station  = "TEST_ST_01",
    metric          = "conductivity",
    reason_to_adjust = "disturbance",
    timestamp_start = "2024-06-01 01:00:00",
    timestamp_end   = "2024-06-01 02:00:00",
    manual_note     = "test"
  ))
  # Rows 2-3 should be NA'd
  expect_true(is.na(result$conduct_uScm_adj[2]))
  expect_true(is.na(result$conduct_uScm_adj[3]))
  # Row 1 untouched
  expect_equal(result$conduct_uScm_adj[1], 100)
})

test_that("#73 - DO uses watertemp_C_do_adj (not watertemp_C_adj)", {
  df <- make_test_df(list(
    do_mgl_adj         = c(8, 9, 10, 9, 8),
    do_percsat_adj     = c(80, 90, 100, 90, 80),
    watertemp_C_do_adj = c(10, 11, 12, 13, 14)
  ))
  result <- suppressWarnings(adjust_logger_NA(
    input_data      = df,
    select_station  = "TEST_ST_01",
    metric          = "DO",
    reason_to_adjust = "disturbance",
    timestamp_start = "2024-06-01 01:00:00",
    timestamp_end   = "2024-06-01 02:00:00",
    manual_note     = "test"
  ))
  # DO temp should be NA'd in the window (rows 2-3)
  expect_true(is.na(result$watertemp_C_do_adj[2]))
  expect_true(is.na(result$watertemp_C_do_adj[3]))
  # Outside window untouched
  expect_equal(result$watertemp_C_do_adj[1], 10)
})

test_that("#73 - BARO rejects ice and dry reasons", {
  df <- make_test_df(list(
    airpress_kPa_adj = c(101, 101, 101, 101, 101),
    airtemp_C_adj    = c(20, 21, 22, 23, 24)
  ))
  expect_error(
    adjust_logger_NA(
      input_data = df, select_station = "TEST_ST_01",
      metric = "BARO", reason_to_adjust = "ice",
      timestamp_start = "2024-06-01 01:00:00",
      timestamp_end   = "2024-06-01 02:00:00",
      manual_note = "test"
    ),
    "not applicable"
  )
  expect_error(
    adjust_logger_NA(
      input_data = df, select_station = "TEST_ST_01",
      metric = "BARO", reason_to_adjust = "dry",
      timestamp_start = "2024-06-01 01:00:00",
      timestamp_end   = "2024-06-01 02:00:00",
      manual_note = "test"
    ),
    "not applicable"
  )
})

test_that("#73 - BARO disturbance also removes airtemp by default", {
  df <- make_test_df(list(
    airpress_kPa_adj = c(101, 101, 101, 101, 101),
    airtemp_C_adj    = c(20, 21, 22, 23, 24)
  ))
  result <- suppressWarnings(adjust_logger_NA(
    input_data      = df,
    select_station  = "TEST_ST_01",
    metric          = "BARO",
    reason_to_adjust = "disturbance",
    timestamp_start = "2024-06-01 01:00:00",
    timestamp_end   = "2024-06-01 02:00:00",
    manual_note     = "test"
  ))
  # airtemp should be NA'd in window
  expect_true(is.na(result$airtemp_C_adj[2]))
  expect_true(is.na(result$airtemp_C_adj[3]))
  # keep_temp = TRUE should preserve it
  result2 <- suppressWarnings(adjust_logger_NA(
    input_data      = df,
    select_station  = "TEST_ST_01",
    metric          = "BARO",
    reason_to_adjust = "disturbance",
    timestamp_start = "2024-06-01 01:00:00",
    timestamp_end   = "2024-06-01 02:00:00",
    keep_temp       = TRUE,
    manual_note     = "test"
  ))
  expect_equal(result2$airtemp_C_adj[2], 21)
  expect_equal(result2$airtemp_C_adj[3], 22)
})

test_that("#73 - airtemp metric works for disturbance", {
  df <- make_test_df(list(
    airtemp_C_adj = c(20, 21, 22, 23, 24)
  ))
  result <- suppressWarnings(adjust_logger_NA(
    input_data      = df,
    select_station  = "TEST_ST_01",
    metric          = "airtemp",
    reason_to_adjust = "disturbance",
    timestamp_start = "2024-06-01 01:00:00",
    timestamp_end   = "2024-06-01 02:00:00",
    manual_note     = "test"
  ))
  expect_true(is.na(result$airtemp_C_adj[2]))
  expect_true(is.na(result$airtemp_C_adj[3]))
  expect_equal(result$airtemp_C_adj[1], 20)
  # ice should be rejected
  expect_error(
    adjust_logger_NA(
      input_data = df, select_station = "TEST_ST_01",
      metric = "airtemp", reason_to_adjust = "ice",
      timestamp_start = "2024-06-01 01:00:00",
      timestamp_end   = "2024-06-01 02:00:00",
      manual_note = "test"
    ),
    "not applicable"
  )
})

test_that("#73 - keep_temp = TRUE preserves temp for ice (WL)", {
  df <- make_test_df(list(
    waterlevel_m     = c(1, 1.1, 1.2, 1.1, 1),
    waterlevel_m_adj = c(1, 1.1, 1.2, 1.1, 1),
    watertemp_C_adj  = c(5, 4, 3, 4, 5)
  ))
  result <- suppressWarnings(adjust_logger_NA(
    input_data      = df,
    select_station  = "TEST_ST_01",
    metric          = "WL",
    reason_to_adjust = "ice",
    timestamp_start = "2024-06-01 01:00:00",
    timestamp_end   = "2024-06-01 02:00:00",
    keep_temp       = TRUE,
    manual_note     = "test"
  ))
  # WL should be NA'd
  expect_true(is.na(result$waterlevel_m_adj[2]))
  # Temp should be preserved
  expect_equal(result$watertemp_C_adj[2], 4)
  expect_equal(result$watertemp_C_adj[3], 3)
})

test_that("#73 - message when log_root is NULL and rows modified", {
  df <- make_test_df(list(
    waterlevel_m     = c(1, 1.1, 1.2, 1.1, 1),
    waterlevel_m_adj = c(1, 1.1, 1.2, 1.1, 1),
    watertemp_C_adj  = c(5, 4, 3, 4, 5)
  ))
  expect_message(
    adjust_logger_NA(
      input_data      = df,
      select_station  = "TEST_ST_01",
      metric          = "WL",
      reason_to_adjust = "disturbance",
      timestamp_start = "2024-06-01 01:00:00",
      timestamp_end   = "2024-06-01 02:00:00",
      log_root        = NULL,
      manual_note     = "test"
    ),
    "log_root is NULL"
  )
})

# --- Issue #65: conductivity_temp_compensation flexible column resolution -----

test_that("#65 - works with _adj columns (post-QAQC)", {
  df <- data.frame(
    conduct_uScm_adj = c(100, 200, 300),
    watertemp_C_adj  = c(10,  20,  25)
  )
  result <- conductivity_temp_compensation(df)
  expect_true("spc_uScm_adj" %in% names(result))
  expect_equal(nrow(result), 3)
  # At 25C the coefficient is ~1.0, so SPC should be close to raw EC
  expect_equal(result$spc_uScm_adj[3], 300, tolerance = 1)
})

test_that("#65 - falls back to raw columns (pre-QAQC)", {
  df <- data.frame(
    conduct_uScm = c(100, 200, 300),
    watertemp_C  = c(10,  20,  25)
  )
  result <- conductivity_temp_compensation(df)
  expect_true("spc_uScm_adj" %in% names(result))
  expect_equal(result$spc_uScm_adj[3], 300, tolerance = 1)
})

test_that("#65 - prefers _adj over raw when both exist", {
  df <- data.frame(
    conduct_uScm     = c(999, 999, 999),
    conduct_uScm_adj = c(100, 200, 300),
    watertemp_C      = c(0,   0,   0),
    watertemp_C_adj  = c(10,  20,  25)
  )
  result <- conductivity_temp_compensation(df)
  # Should use _adj (300 at 25C) not raw (999 at 0C)
  expect_equal(result$spc_uScm_adj[3], 300, tolerance = 1)
})

test_that("#65 - errors when no conductivity column exists", {
  df <- data.frame(watertemp_C = c(10, 20))
  expect_error(
    conductivity_temp_compensation(df),
    "conduct_uScm"
  )
})

test_that("#65 - errors when no temperature column exists", {
  df <- data.frame(conduct_uScm = c(100, 200))
  expect_error(
    conductivity_temp_compensation(df),
    "watertemp_C"
  )
})
