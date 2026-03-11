# ============================================================================
# helper-pipeline-fixtures.R
#
# WHY THIS FILE EXISTS
# --------------------
# Several test files need to run the same expensive data-processing pipeline
# (bind raw files -> add barometric data -> convert units -> run QAQC).
# Each pipeline run takes ~15 seconds. Without this helper, 7 different test
# files ran the pipeline from scratch — adding over 4 minutes of redundant
# computation to the test suite.
#
# HOW IT WORKS
# ------------
# testthat automatically sources any file named "helper-*.R" before running
# tests. This file runs each pipeline ONCE and stores the results in cache
# environments. Test files call get_wl_*() or get_do_*() functions to
# retrieve the pre-computed data instantly.
#
# WHAT GETS CACHED
# ----------------
# 1. Water Level pipeline: bind -> add_nearest_baro -> convert_kPa_to_m -> QAQC
# 2. Dissolved Oxygen pipeline: bind -> add_nearest_baro -> convert_do
#
# IMPORTANT: These caches are READ-ONLY for tests. If your test needs to
# modify the data (e.g., write files to an output folder, run QAQC with a
# fresh log directory), make a copy or use the cached data as input to
# your own pipeline step with a local temp directory.
# ============================================================================

# ---------------------------------------------------------------------------
# Cache environments
#
# We use R environments (not lists) because environments are passed by
# reference. This means every call to get_wl_qaqc_data() returns the
# SAME object without copying it — fast and memory-efficient.
# ---------------------------------------------------------------------------
.wl_cache <- new.env(parent = emptyenv())
.do_cache <- new.env(parent = emptyenv())

# ===========================================================================
# WATER LEVEL PIPELINE
#
# This pipeline processes raw HOBO U20 water-level logger data:
#   1. bind_hobo_files()        — parse raw CSVs into a tidy data frame
#   2. bind_hobo_files() (baro) — same for barometric pressure loggers
#   3. add_nearest_baro()       — attach nearest baro station to each WL row
#   4. convert_waterlevel_kPa_m() — convert pressure (kPa) to depth (metres)
#   5. qc_waterlevel()        — flag ice, dry periods, disturbances, etc.
#
# Test fixtures used:
#   - Raw level data:  fixtures/level/
#   - Raw baro data:   fixtures/baro/
#   - Metadata:        fixtures/metadata/test_convert_wl_kpa_m.csv
#   - Reference data:  fixtures/NT_manual_waterlevel_20251119.csv
#   - Station tested:  ALBR_ST_30
# ===========================================================================

.run_wl_pipeline <- function() {
  # Only run once — skip if the cache already has results

if (!is.null(.wl_cache$done)) return(invisible())

  # Create a temporary folder that persists for the whole test session.
  # Individual tests that need their own output folder (e.g., to check
  # whether specific files were created) should use withr::local_tempdir().
  cache_dir <- file.path(tempdir(), "wl_pipeline_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  # -- Step 1: Bind raw water-level logger files into one data frame ---------
  level_bound <- bind_hobo_files(
    path_to_raw_folder    = test_path("fixtures/level"),
    path_to_output_folder = cache_dir,
    metadata_path         = test_path("fixtures/metadata/test_convert_wl_kpa_m.csv")
  )

  # -- Step 2: Bind barometric pressure logger files -------------------------
  baro_bound <- bind_hobo_files(
    path_to_raw_folder    = test_path("fixtures/baro"),
    path_to_output_folder = cache_dir,
    metadata_path         = test_path("fixtures/metadata/test_convert_wl_kpa_m.csv")
  )

  # -- Step 3: Attach nearest barometric station to each water-level row -----
  #    add_nearest_baro() reads the baro CSVs that bind_hobo_files() wrote
  #    to cache_dir, then joins the closest baro readings by timestamp.
  wl_data_with_baro <- add_nearest_baro(
    input_data            = level_bound[[1]],
    path_to_output_folder = cache_dir,
    baro_data_path        = cache_dir,
    metric                = "both",
    baro_site_selection   = "auto",
    metadata_path         = test_path("fixtures/metadata/test_convert_wl_kpa_m.csv")
  )

  # -- Step 4: Convert raw pressure (kPa) to water level (metres) -----------
  #    Uses field reference measurements (manual stage readings) to calibrate
  #    the conversion from absolute pressure to water depth.
  converted_data <- convert_waterlevel_kPa_m(
    input_data            = wl_data_with_baro[[1]],
    select_station        = "all",
    reference_data        = test_path("fixtures/NT_manual_waterlevel_20251119.csv"),
    reference_type        = "stage",
    select_measurement    = 1,
    logger_type_expected  = "u20",
    path_to_output_folder = cache_dir
  )

  # -- Step 5: Run automated QAQC on converted water level data --------------
  #    Flags potential issues like ice cover, dry periods, and sensor
  #    disturbances. Also writes a QAQC log CSV to log_root.
  qaqc_data <- qc_waterlevel(
    converted_data[[1]],
    log_root       = cache_dir,
    select_station = "ALBR_ST_30"
  )

  # -- Store all intermediate results in the cache ---------------------------
  .wl_cache$level_bound_df      <- level_bound[[1]]   # v0.1 raw bound data
  .wl_cache$wl_with_baro_df     <- wl_data_with_baro[[1]]  # v0.2 with baro
.wl_cache$converted_data      <- converted_data      # v0.3 full result list
  .wl_cache$qaqc_data           <- qaqc_data           # after QAQC
  .wl_cache$cache_dir           <- cache_dir
  .wl_cache$done                <- TRUE
}

# ---------------------------------------------------------------------------
# Public accessor functions
#
# Each function triggers the pipeline (if it hasn't run yet) then returns
# the requested cached object. Call these from your test files.
# ---------------------------------------------------------------------------

#' Get raw bound water-level data (v0.1 — before baro or conversion).
#' Returns a data frame with columns like waterpressure_kPa, watertemp_C, etc.
get_wl_level_bound_df <- function() {
  .run_wl_pipeline()
  .wl_cache$level_bound_df
}

#' Get water-level data with barometric data attached (v0.2).
#' Returns a data frame with airpress_kPa column added.
get_wl_with_baro_df <- function() {
  .run_wl_pipeline()
  .wl_cache$wl_with_baro_df
}

#' Get the full conversion result (v0.3).
#' Returns a named list with:
#'   $site_wl  — data frame with waterlevel_m column
#'   $ref_dat  — reference measurement data frame
#'   $wl_plot  — plotly calibration plot
get_wl_converted_data <- function() {
  .run_wl_pipeline()
  .wl_cache$converted_data
}

#' Get QAQC'd water-level data for station ALBR_ST_30.
#' Returns a data frame with flag columns (flag_ice, flag_disturbance, etc.)
get_wl_qaqc_data <- function() {
  .run_wl_pipeline()
  .wl_cache$qaqc_data
}

# ===========================================================================
# DISSOLVED OXYGEN PIPELINE
#
# This pipeline processes raw HOBO U26 dissolved-oxygen logger data:
#   1. bind_hobo_files()          — parse raw DO CSVs
#   2. bind_hobo_files() (baro)   — parse raw baro CSVs
#   3. add_nearest_baro()         — attach nearest baro station
#   4. convert_do_mgl_percsat()   — compute % saturation from mg/L + baro
#
# Test fixtures used:
#   - Raw DO data:   fixtures/test_convert_do/DO/
#   - Raw baro data: fixtures/test_convert_do/baro/
#   - Metadata:      fixtures/metadata/test_convert_do_mgl_percsat.csv
#   - Station tested: ALBR_ST_30
# ===========================================================================

.run_do_pipeline <- function() {
  if (!is.null(.do_cache$done)) return(invisible())

  cache_dir <- file.path(tempdir(), "do_pipeline_cache")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  metadata <- test_path("fixtures/metadata/test_convert_do_mgl_percsat.csv")

  # -- Step 1: Bind raw dissolved oxygen logger files ------------------------
  DO_bound <- bind_hobo_files(
    path_to_raw_folder    = test_path("fixtures/test_convert_do/DO"),
    path_to_output_folder = cache_dir,
    metadata_path         = metadata
  )

  # -- Step 2: Bind barometric logger files ----------------------------------
  baro_bound <- bind_hobo_files(
    path_to_raw_folder    = test_path("fixtures/test_convert_do/baro"),
    path_to_output_folder = cache_dir,
    metadata_path         = metadata
  )

  # -- Step 3: Attach nearest barometric station to each DO reading ----------
  DO_with_Baro <- add_nearest_baro(
    input_data            = DO_bound[[1]],
    path_to_output_folder = cache_dir,
    baro_data_path        = cache_dir,
    metric                = "DO",
    baro_site_selection   = "auto",
    metadata_path         = metadata
  )

  # -- Step 4: Compute percent saturation from mg/L using baro pressure ------
  converted_df <- convert_do_mgl_percsat(
    DO_with_Baro[[1]],
    path_to_output_folder = cache_dir,
    version_label         = "v0.3"
  )

  # -- Store results ---------------------------------------------------------
  .do_cache$do_bound_df      <- DO_bound[[1]]       # v0.1 raw bound DO
  .do_cache$do_with_baro_df  <- DO_with_Baro[[1]]   # v0.2 with baro attached
  .do_cache$converted_df     <- converted_df         # v0.3 with do_percsat
  .do_cache$cache_dir        <- cache_dir
  .do_cache$done             <- TRUE
}

# ---------------------------------------------------------------------------
# Public accessor functions for DO pipeline
# ---------------------------------------------------------------------------

#' Get raw bound dissolved oxygen data (v0.1).
get_do_bound_df <- function() {
  .run_do_pipeline()
  .do_cache$do_bound_df
}

#' Get DO data with barometric data attached (v0.2).
get_do_with_baro_df <- function() {
  .run_do_pipeline()
  .do_cache$do_with_baro_df
}

#' Get DO data after percent saturation conversion (v0.3).
#' Returns a data frame with do_percsat column.
get_do_converted_df <- function() {
  .run_do_pipeline()
  .do_cache$converted_df
}
