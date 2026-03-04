# =============================================================================
# TUTORIAL: Full Dissolved Oxygen Pipeline — COOK_WE_40
# =============================================================================
#
# This script walks through the complete dissolved oxygen (DO) processing
# pipeline using the `waterlogged` R package. We use real test data from
# the COOK_WE_40 wetland station as our example.
#
# WHAT THIS SCRIPT DOES:
#   1. Reads and quality-checks barometric (air pressure) data
#   2. Reads raw dissolved oxygen logger files
#   3. Attaches barometric pressure to the DO data
#   4. Converts DO concentration (mg/L) to percent saturation (%sat)
#   5. Runs automatic quality checks on the DO data
#   6. Corrects sensor drift using field reference measurements
#   7. Shows how to manually remove bad data (ice, dry, disturbance)
#   8. Exports the final clean dataset
#
# WHY BAROMETRIC DATA MATTERS FOR DO:
#   Dissolved oxygen saturation depends on atmospheric pressure. A DO
#   reading of 8 mg/L at sea level means something different than 8 mg/L
#   at high elevation. By attaching barometric pressure, we can convert
#   the raw mg/L reading into percent saturation (%sat) — a measure that
#   accounts for altitude and weather conditions.
#
# WHY DRIFT CORRECTION MATTERS:
#   DO sensors gradually lose accuracy over time as the sensing element
#   degrades ("fouling"). Field reference measurements taken during site
#   visits tell us how far the sensor has drifted. The drift correction
#   adjusts the logged data so it matches the field references.
#
# DATA USED (all in dissolved_oxygen/input_data/):
#   - metadata.csv          — logger deployment records
#   - baro/                 — barometric logger CSVs (DARL, HARP, STIL)
#   - DO/                   — 1 DO logger CSV from COOK_WE_40 (SN 21473863)
#   - field_references.csv  — discrete field measurements for drift correction
#
# =============================================================================

# -----------------------------------------------------------------------------
# PART 0: Setup
# -----------------------------------------------------------------------------

# Load the waterlogged package.
# You must install from the develop branch:
#   install.packages("pak")
#   pak::pkg_install("dfo-pacific-science/waterlogged@develop")

library(waterlogged)

# Set the path to the input data folder.
# All paths are relative to the demos/ project root (where demos.Rproj lives).

# TODO update directory to \waterlogged\inst\extdata\dissolved_oxygen
data_dir <- "dissolved_oxygen/input_data"

# Define paths to input files.

metadata_file   <- file.path(data_dir, "metadata.csv")
baro_folder     <- file.path(data_dir, "baro")
do_folder       <- file.path(data_dir, "DO")
reference_file  <- file.path(data_dir, "field_references.csv")

# Verify everything exists
stopifnot(
  "Metadata file not found"        = file.exists(metadata_file),
  "Baro folder not found"          = dir.exists(baro_folder),
  "DO folder not found"            = dir.exists(do_folder),
  "Field references file not found" = file.exists(reference_file)
)


# =============================================================================
# STEP 1: Bind and QAQC Barometric Data
# =============================================================================
#
# WHAT: Reads all barometric logger CSVs, combines them, and runs quality
#       checks (out-of-range, spikes, flatlines).
#
# WHY:  We need clean barometric data to convert DO from mg/L to %sat.
#       If the baro data has errors, they'll contaminate the %sat values.
#
# This is the same process used in the water level pipeline — baro data
# is the foundation for both.
# -----------------------------------------------------------------------------

?waterlogged::bind_hobo_files

# 1a. Bind raw baro files
baro_bound <- bind_hobo_files(
  path_to_raw_folder = baro_folder,
  metadata_path      = metadata_file
)

baro_data <- baro_bound[[1]]

cat("Baro data:", nrow(baro_data), "rows,",
    length(unique(baro_data$site_station_code)), "stations\n")

# 1b. Run automatic QAQC on all baro stations
?barometric_qaqc_all

baro_qc <- barometric_qaqc_all(
  baro_data         = baro_data,
  metadata_path     = metadata_file,
  pressure_low_kpa  = 85,
  pressure_high_kpa = 105,
  temp_low_limit    = -40,
  temp_high_limit   = 50,
  spike_threshold_kpa = 1.5,
  flatline_n        = 6
)

cat("Baro QAQC complete:", nrow(baro_qc), "rows\n")
cat("  Spikes flagged: ", sum(baro_qc$edit_spike, na.rm = TRUE), "\n")
cat("  Flatlines:      ", sum(baro_qc$edit_flatline, na.rm = TRUE), "\n")


# =============================================================================
# STEP 2: Bind Raw Dissolved Oxygen Logger Files (v0.1)
# =============================================================================
#
# WHAT: Reads the raw HOBO U26 DO logger CSV and links it to the metadata.
#
# WHY:  The HOBO U26 records two channels:
#         - DO concentration in mg/L
#         - Water temperature in degrees C
#       This step parses them into standard column names: do_mgl and watertemp_C.
#
# ABOUT THE COOK_WE_40 DATA:
#   - Logger: HOBO U26 (SN 21473863)
#   - Deployed: April 14 to October 21, 2025
#   - Location: COOK wetland, station WE_40
#   - One download file covering the full deployment
# -----------------------------------------------------------------------------


do_bound <- bind_hobo_files(
  path_to_raw_folder = do_folder,
  metadata_path      = metadata_file
)

do_data <- do_bound[[1]]
do_plot <- do_bound[[2]]

cat("DO data:", nrow(do_data), "rows\n")
cat("Stations:", paste(unique(do_data$site_station_code), collapse = ", "), "\n")
cat("Date range:", as.character(min(do_data$timestamp)),
    "to", as.character(max(do_data$timestamp)), "\n")
cat("DO range:", round(min(do_data$do_mgl, na.rm = TRUE), 2), "to",
    round(max(do_data$do_mgl, na.rm = TRUE), 2), "mg/L\n")

# Display the overview plot
print(do_plot)


# =============================================================================
# STEP 3: Attach Barometric Data (v0.2)
# =============================================================================
#
# WHAT: Finds the nearest barometric station to COOK_WE_40 and joins the
#       air pressure and temperature readings to each DO timestamp.
#
# WHY:  We need atmospheric pressure at each timestamp to:
#       1. Convert DO from mg/L to percent saturation
#       2. Help detect dry-sensor conditions (comparing air and water temp)
#
# BARO STATION SELECTION:
#   You can select the baro station in two ways:
#
#   "auto" — the function calculates distance (Haversine formula) between
#            the DO station and all baro stations, picks up to 3 nearest
#            within max_km, calibrates and coalesces them.
#
#   "<station_code>" — you specify exactly which baro station to use.
#            This is simpler and avoids the calibration overhead.
#
#   Here we use DARL_ST_30 directly, since it's the closest baro station
#   to COOK_WE_40 and provides full temporal coverage.
# -----------------------------------------------------------------------------

?add_nearest_baro

do_with_baro <- add_nearest_baro(
  input_data          = do_data,
  baro_data           = baro_qc,
  metric              = "DO",
  baro_site_selection = "DARL_ST_30",   # use nearest baro station directly
  metadata_path       = metadata_file
)

do_data <- do_with_baro[[1]]

cat("After baro attach:", nrow(do_data), "rows\n")
cat("Baro station(s) used:",
    paste(unique(na.omit(do_data$baro_site_stn_code)), collapse = ", "), "\n")

# Check for gaps in baro coverage
baro_na <- sum(is.na(do_data$airpress_kPa))
cat("Missing baro data:", baro_na, "of", nrow(do_data), "rows",
    paste0("(", round(baro_na / nrow(do_data) * 100, 1), "%)"), "\n")


# =============================================================================
# STEP 4: Convert DO mg/L to Percent Saturation (v0.3)
# =============================================================================
#
# WHAT: Calculates percent air saturation (%sat) from the DO concentration,
#       water temperature, and atmospheric pressure.
#
# WHY:  Percent saturation tells you how much oxygen is in the water
#       relative to what the water COULD hold at that temperature and
#       pressure. It's a better indicator of ecosystem health than mg/L
#       alone because it accounts for physical conditions.
#
#       For example, cold water holds more oxygen than warm water. So
#       8 mg/L in cold water might be only 70% saturated, while 8 mg/L
#       in warm water might be 95% saturated.
#
# THE MATH (Weiss 1970/1974):
#   1. Calculate the theoretical maximum DO at the given temperature
#      and pressure using the Weiss oxygen solubility equation
#   2. Divide the measured DO by the theoretical maximum
#   3. Multiply by 100 to get percent
#
# NEW COLUMN: do_percsat (percent air saturation)
# -----------------------------------------------------------------------------

?convert_do_mgl_percsat

do_data <- convert_do_mgl_percsat(do_data)

cat("DO %sat range:", round(min(do_data$do_percsat, na.rm = TRUE), 1), "to",
    round(max(do_data$do_percsat, na.rm = TRUE), 1), "%\n")

# Preview the key columns
head(do_data[, c("timestamp", "do_mgl", "watertemp_C",
                  "airpress_kPa", "do_percsat")])


# =============================================================================
# STEP 5: Automatic DO QAQC
# =============================================================================
#
# WHAT: Runs automatic quality checks on the dissolved oxygen data and
#       creates adjusted columns with problematic values corrected or
#       removed.
#
# WHY:  DO sensors can produce artifacts from logger errors, negative
#       readings, ice formation, sensor fouling, or exposure to air (dry).
#       This step catches common problems automatically.
#
# WHAT IT CHECKS:
#   1. Logger errors  — the code -888.88 means the sensor malfunctioned
#   2. Negative DO    — DO between 0 and -1 mg/L corrected to 0;
#                       DO below -1 mg/L removed (set to NA)
#   3. High outliers  — DO above 21 mg/L removed (physically implausible)
#   4. Ice detection  — water temp < 0.3 C flagged; <= -1 C removed
#   5. Dry detection  — a 12-point sliding window checks if %sat >= 100
#                       AND air-water temp diff <= 2 C (sensor in air)
#
# ADJUSTED COLUMNS CREATED:
#   do_mgl_adj         — adjusted DO in mg/L
#   do_percsat_adj     — adjusted DO in %sat (tracks the mg/L changes)
#   watertemp_C_do_adj — adjusted water temperature from the DO logger
#
# FLAGS vs EDITS (same pattern as water level):
#   flag_* = "we noticed something — please review" (data NOT changed)
#   edit_* = "we changed the data" (values set to NA or corrected)
# -----------------------------------------------------------------------------

?dissox_qaqc

do_qc <- dissox_qaqc(
  input_data     = do_data,
  select_station = "COOK_WE_40"
)

# Summarize the flags
cat("QAQC results for COOK_WE_40:\n")
cat("  Total rows:       ", nrow(do_qc), "\n")
cat("  Logger errors:    ", sum(do_qc$do_error, na.rm = TRUE), "\n")
cat("  DO < -1 removed:  ", sum(do_qc$do_neg_lt_minus1, na.rm = TRUE), "\n")
cat("  DO -1 to 0 fixed: ", sum(do_qc$do_neg_between, na.rm = TRUE), "\n")
cat("  DO > 21 removed:  ", sum(do_qc$do_high, na.rm = TRUE), "\n")
cat("  Ice flagged:      ", sum(do_qc$temp_flag_ice, na.rm = TRUE), "\n")
cat("  Dry flagged:      ", sum(do_qc$flag_do_dry, na.rm = TRUE), "\n")

# Show the QAQC log
qaqc_log <- attr(do_qc, "qaqc_log")
if (!is.null(qaqc_log) && nrow(qaqc_log) > 0) {
  cat("\nQAQC log entries:\n")
  print(qaqc_log[, c("field", "action", "code", "ts_start", "ts_end")])
}


# =============================================================================
# STEP 6: Visual Review
# =============================================================================
#
# WHAT: Creates an interactive multi-panel plot for reviewing the DO data.
#
# WHY:  Before applying drift correction or manual adjustments, you need
#       to see the data. The plot shows raw vs adjusted values with flag
#       indicators, so you can identify problem periods.
#
# HOW TO READ THE PLOT:
#   - Light traces  = raw values (original, unmodified)
#   - Dark traces   = adjusted values (after auto QAQC)
#   - Colored dots  = flag locations (ice, dry, error)
#   - Use the range slider at the bottom to zoom into specific periods
#   - Hover over points for exact values
# -----------------------------------------------------------------------------

?plot_qaqc_timeseries

plot_qaqc_timeseries(
  do_data        = do_qc,
  baro_data      = baro_qc,
  select_station = "COOK_WE_40"
)


# =============================================================================
# STEP 7: Drift Correction
# =============================================================================
#
# WHAT: Adjusts the logged DO data to match field reference measurements,
#       correcting for gradual sensor drift between site visits.
#
# WHY:  DO sensors slowly lose accuracy over their deployment. The sensing
#       membrane gets fouled (coated with biofilm, sediment, etc.), causing
#       readings to drift away from true values. During field visits, a
#       handheld reference instrument (e.g., a calibrated YSI) provides
#       a "truth" measurement. Drift correction uses these reference
#       points to straighten out the logged data.
#
# HOW IT WORKS:
#   The reference file contains discrete field measurements with an
#   "event_type" for each:
#
#   - "calibration" = the sensor was recalibrated during this visit.
#     The function corrects data BEFORE the calibration and trusts data
#     AFTER it (since the sensor was just cleaned and calibrated).
#
#   - "check" = the reference was measured but the sensor was NOT touched.
#     The function uses this to quantify drift at that point in time.
#
#   - "ignore" = skip this measurement (e.g., the reference was suspect).
#
# DRIFT CORRECTION METHODS:
#   - "ratio" (default): Multiplicative correction. Better for sensors
#     where drift is proportional (e.g., a 10% fouling reduces all
#     readings by 10%). This is typical for DO sensors.
#
#   - "offset": Additive correction. Better when drift is a constant
#     shift regardless of the measurement value.
#
# OUR COOK_WE_40 REFERENCE DATA:
#   - 5/2/2025:  calibration (99 %sat) — sensor calibrated in the field
#   - 7/15/2025: ignore (98 %sat) — this measurement will be skipped
#   - 7/20/2025: check (87 %sat) — a check visit, no sensor touching
#   - 9/21/2025: check (100 %sat) — another check visit
#
# COLUMN MODIFIED: do_percsat_adj (the adjusted %sat values)
# NEW FLAG:        edit_do_drift (TRUE for rows that were corrected)
# -----------------------------------------------------------------------------


# Preview the reference data
ref_data <- read.csv(reference_file)

print(ref_data[ref_data$metric == "do", ])

?wq_drift_correction

do_qc <- wq_drift_correction(
  input_data     = do_qc,
  ref_data_path  = reference_file,
  select_station = "COOK_WE_40",
  metric         = "do",
  method         = "ratio",        # multiplicative correction
  max_ref_gap_h  = 1               # match logger to reference within 1 hour
)

# Check results
if ("edit_do_drift" %in% names(do_qc)) {
  drift_count <- sum(do_qc$edit_do_drift, na.rm = TRUE)
  cat("Rows corrected by drift:", drift_count, "of", nrow(do_qc), "\n")
} else {
  cat("No drift correction column found — check reference data alignment.\n")
}

# Show the drift correction log
drift_log <- attr(do_qc, "qaqc_log")
if (!is.null(drift_log) && nrow(drift_log) > 0) {
  cat("\nDrift correction log:\n")
  print(drift_log)
}


# =============================================================================
# STEP 8: Manual Adjustments (Examples)
# =============================================================================
#
# This section demonstrates how to manually remove bad data. In a real
# project, you would apply these ONLY after reviewing the plot and
# identifying specific problem periods.
#
# For DO data, the three reasons for removal are:
#
#   "ice"         — sensor frozen in ice; DO readings are unreliable
#                   Sets do_mgl_adj, do_percsat_adj, watertemp_C_adj to NA
#
#   "dry"         — sensor exposed to air (water level dropped below it)
#                   Sets do_mgl_adj, do_percsat_adj, watertemp_C_adj to NA
#
#   "disturbance" — physical disturbance (e.g., debris, maintenance)
#                   Sets do_mgl_adj, do_percsat_adj to NA
#                   Keeps watertemp_C_adj unless keep_temp = FALSE
#
# IMPORTANT: Each function returns a NEW data frame. Always assign the
# result back to your working variable so corrections accumulate.
# -----------------------------------------------------------------------------


# ---- 8a. Remove a dry period ----
#
# If the plot shows the sensor was exposed to air (DO near 100%sat and
# air-water temperature converge), remove that period:

# do_qc <- adjust_logger_NA(
#   input_data       = do_qc,
#   select_station   = "COOK_WE_40",
#   metric           = "dissolvedoxygen",
#   reason_to_adjust = "dry",
#   timestamp_start  = "2025-08-01 00:00:00",
#   timestamp_end    = "2025-08-15 00:00:00",
#   manual_note      = "Sensor exposed during low-flow period"
# )


# ---- 8b. Remove an ice period ----

# do_qc <- adjust_logger_NA(
#   input_data       = do_qc,
#   select_station   = "COOK_WE_40",
#   metric           = "dissolvedoxygen",
#   reason_to_adjust = "ice",
#   timestamp_start  = "2025-01-01 00:00:00",
#   timestamp_end    = "2025-03-15 00:00:00",
#   manual_note      = "Ice confirmed from water temperature data"
# )


# ---- 8c. Remove a disturbance (keep temperature) ----
#
# Sometimes you want to remove DO readings but KEEP the temperature data
# (e.g., during a brief maintenance event where the temp sensor was fine).

# do_qc <- adjust_logger_NA(
#   input_data       = do_qc,
#   select_station   = "COOK_WE_40",
#   metric           = "dissolvedoxygen",
#   reason_to_adjust = "disturbance",
#   timestamp_start  = "2025-06-01 12:00:00",
#   timestamp_end    = "2025-06-01 14:00:00",
#   keep_temp        = TRUE,            # preserve temperature readings
#   manual_note      = "Brief logger maintenance — DO affected, temp OK"
# )


# ---- After any manual adjustments, always re-plot to verify ----

# plot_qaqc_timeseries(
#   do_data        = do_qc,
#   baro_data      = baro_qc,
#   select_station = "COOK_WE_40"
# )


# =============================================================================
# STEP 9: Final Review
# =============================================================================
#
# Re-plot the data after all corrections to verify the final result.
# The adjusted traces should look reasonable with no obvious artifacts.
# Compare raw (light) vs adjusted (dark) to see exactly what changed.
# -----------------------------------------------------------------------------

?plot_qaqc_timeseries

plot_qaqc_timeseries(
  do_data        = do_qc,
  baro_data      = baro_qc,
  select_station = "COOK_WE_40"
)



# =============================================================================
# STEP 10: Export Clean Data (v1.0)
# =============================================================================
#
# WHAT: Writes the final cleaned data to CSV files, one per station per year.
#
# WHAT GETS EXPORTED:
#   - site_station_code, timestamp (identifiers)
#   - do_mgl, do_mgl_adj (raw and adjusted DO in mg/L)
#   - do_percsat, do_percsat_adj (raw and adjusted DO in %sat)
#   - watertemp_C, watertemp_C_do_adj (raw and adjusted water temperature)
#   - All flag_* and edit_* columns (the full audit trail)
#
# For analysis, always use the *_adj columns (do_percsat_adj, do_mgl_adj).
# Compare raw vs _adj to see what the QAQC process changed.
# -----------------------------------------------------------------------------


# Preview the final columns
cat("Final dataset:", nrow(do_qc), "rows x", ncol(do_qc), "columns\n\n")

cat("Summary of adjusted DO (%sat):\n")
print(summary(do_qc$do_percsat_adj))

cat("\nSummary of adjusted DO (mg/L):\n")
print(summary(do_qc$do_mgl_adj))

# To write the final files, uncomment and set your output folder:

?write_clean_data

# write_clean_data(
#   input_data            = do_qc,
#   path_to_output_folder = "output",
#   metric                = "dissolvedoxygen"
# )
#
# cat("Clean data exported to: output/<year>/clean/\n")


# =============================================================================
# RECAP: What Happened at Each Step
# =============================================================================
#
#   Step 1:  barometric_qaqc_all()       -> baro_qc    (cleaned baro)
#   Step 2:  bind_hobo_files() on DO     -> do_data     (raw DO)
#   Step 3:  add_nearest_baro()          -> do_data     (DO + baro attached)
#   Step 4:  convert_do_mgl_percsat()    -> do_data     (adds %sat)
#   Step 5:  dissox_qaqc()               -> do_qc       (auto QAQC'd)
#   Step 6:  plot_qaqc_timeseries()      -> (visual review)
#   Step 7:  wq_drift_correction()       -> do_qc       (drift corrected)
#   Step 8:  Manual adjustments          -> do_qc       (updated)
#   Step 9:  plot_qaqc_timeseries()      -> (final review)
#   Step 10: write_clean_data()          -> CSV files on disk
#
# KEY COLUMNS:
#   do_mgl            = raw DO in mg/L (never modified)
#   do_mgl_adj        = adjusted DO in mg/L (all corrections applied)
#   do_percsat        = raw DO in percent saturation (never modified)
#   do_percsat_adj    = adjusted DO in %sat (all corrections applied)
#   watertemp_C       = raw water temperature (never modified)
#   watertemp_C_do_adj = adjusted water temperature
#   flag_*            = "something looks off — review" (data not changed)
#   edit_*            = "data was changed" (set to NA or corrected)
#   edit_do_drift     = TRUE where drift correction was applied
#
# Use do_percsat_adj for analysis — it incorporates all corrections
# including sensor drift.
# =============================================================================

