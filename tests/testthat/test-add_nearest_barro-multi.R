# --- Issue #58: add_nearest_baro max_stations parameter -----------------------

# Helper: build minimal metadata with required columns
make_baro_metadata <- function() {
  data.frame(
    site_station_code  = c("SITE_ST_01", "BARO_ST_01", "BARO_ST_02", "BARO_ST_03"),
    timestamp_deploy   = rep("1/1/2024 00:00", 4),
    timestamp_remove   = rep("12/31/2024 00:00", 4),
    model              = rep("U20", 4),
    sn                 = c("100", "200", "300", "400"),
    metric             = c("water level", "barometric", "barometric", "barometric"),
    status             = rep("active", 4),
    # Site at origin; baros at increasing distances (east along equator)
    latitude           = c(49.0, 49.0, 49.0, 49.0),
    longitude          = c(-123.0, -123.01, -123.02, -123.03),
    stringsAsFactors   = FALSE
  )
}

# Helper: build input WL data (hourly, 6 timestamps)
make_wl_input <- function() {
  ts <- as.POSIXct("2024-06-01 00:00", tz = "UTC") + (0:5) * 3600
  data.frame(
    site_station_code = rep("SITE_ST_01", 6),
    timestamp         = ts,
    metric            = rep("water level", 6),
    waterlevel_kPa    = c(110, 111, 112, 113, 114, 115),
    stringsAsFactors  = FALSE
  )
}

# Helper: build baro data with controlled gaps
# BARO_ST_01 (nearest):  has data at hours 0,1,4,5 — gap at hours 2,3
# BARO_ST_02 (2nd near): has data at hours 2,3 only — fills the gap
# BARO_ST_03 (3rd near): has data at all hours — full backup
make_baro_data <- function() {
  ts_all <- as.POSIXct("2024-06-01 00:00", tz = "UTC") + (0:5) * 3600

  baro1 <- data.frame(
    site_station_code = "BARO_ST_01",
    timestamp         = ts_all[c(1, 2, 5, 6)],
    airpress_kPa      = c(101.0, 101.1, 101.4, 101.5),
    airtemp_C         = c(20, 21, 24, 25),
    stringsAsFactors  = FALSE
  )
  baro2 <- data.frame(
    site_station_code = "BARO_ST_02",
    timestamp         = ts_all[c(3, 4)],
    airpress_kPa      = c(101.2, 101.3),
    airtemp_C         = c(22, 23),
    stringsAsFactors  = FALSE
  )
  baro3 <- data.frame(
    site_station_code = "BARO_ST_03",
    timestamp         = ts_all,
    airpress_kPa      = c(101.0, 101.1, 101.2, 101.3, 101.4, 101.5),
    airtemp_C         = c(20, 21, 22, 23, 24, 25),
    stringsAsFactors  = FALSE
  )
  rbind(baro1, baro2, baro3)
}

test_that("#58 - max_stations = 1 leaves NAs when nearest baro has gaps", {
  result <- suppressWarnings(add_nearest_baro(
    input_data  = make_wl_input(),
    metric      = "water level",
    max_stations = 1L,
    metadata    = make_baro_metadata(),
    baro_data   = make_baro_data()
  ))
  out <- result[[1]]
  # Hours 2 and 3 should be NA because only BARO_ST_01 was used and it has gaps

  expect_true(is.na(out$airpress_kPa[3]))
  expect_true(is.na(out$airpress_kPa[4]))
  # Hours 0,1 should have data
  expect_false(is.na(out$airpress_kPa[1]))
  expect_false(is.na(out$airpress_kPa[2]))
})

test_that("#58 - max_stations = 2 fills gaps from second-nearest station", {
  result <- suppressWarnings(add_nearest_baro(
    input_data  = make_wl_input(),
    metric      = "water level",
    max_stations = 2L,
    metadata    = make_baro_metadata(),
    baro_data   = make_baro_data()
  ))
  out <- result[[1]]
  # All 6 rows should now have barometric pressure (no NAs)
  expect_equal(sum(is.na(out$airpress_kPa)), 0)
})

test_that("#58 - default max_stations (5) uses all available stations", {
  result <- suppressWarnings(add_nearest_baro(
    input_data  = make_wl_input(),
    metric      = "water level",
    metadata    = make_baro_metadata(),
    baro_data   = make_baro_data()
  ))
  out <- result[[1]]
  # Default should fill everything — no NAs
  expect_equal(sum(is.na(out$airpress_kPa)), 0)
  expect_equal(nrow(out), 6)
})
