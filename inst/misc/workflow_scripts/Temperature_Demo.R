# =============================================================================
# TUTORIAL: Temperature Pipeline — TidbiT Loggers (Water & Air Temperature)
# =============================================================================
#
# This script walks through the complete temperature processing pipeline
# using the `waterlogged` R package. We use real test data from two TidbiT
# V2 loggers as our examples:
#
#   - LEMI_ST_20  (water temperature, SN 20712099)
#   - HAGG_ST_30  (air temperature, SN 20764236)
#
# WHAT THIS SCRIPT DOES:
#   1. Reads raw TidbiT logger files and links them to deployment metadata
#   2. Runs automatic quality checks on water temperature
#   3. Runs automatic quality checks on air temperature
#   4. Shows how to visually review the QAQC results
#   5. Demonstrates manual corrections (ice, dry sensor, disturbance)
#   6. Exports the final clean datasets
#
# WHAT ARE TIDBIT LOGGERS?
#   Onset TidbiT V2 (UTBI-001) loggers are small, standalone temperature
#   loggers. Unlike multi-metric HOBO loggers (U20 for water level, U26
#   for dissolved oxygen, U24 for conductivity), TidbiTs measure ONLY
#   temperature. They are commonly deployed in streams (water temp) or
#   mounted in radiation shields (air temp).
#
# HOW THIS DIFFERS FROM OTHER PIPELINES:
#   - NO barometric pressure correction needed (unlike WL or DO)
#   - NO unit conversion step (unlike kPa -> metres for WL)
#   - The metric IS the temperature, so there's no "associated temp"
#   - Simpler pipeline: bind -> QAQC -> manual adjustments -> export
#
# DATA USED (all in water_temperature/):
#   - metadata_wt.csv                            — deployment records
#   - watertemp/                                 — water temp HOBO CSVs
#     - WT_NT_LEMI_ST_20_20712099_20230414.csv   — water temp (download 1)
#     - WT_NT_LEMI_ST_20_20712099_20231017.csv   — water temp (download 2)
#   - airtemp/                                   — air temp HOBO CSVs
#     - WT_HAGG_ST_30_20764236_20230411.csv      — air temp
#
# IMPORTANT: bind_hobo_files() requires one metric type per folder. Water
# temperature and air temperature files must live in separate subfolders.
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

# Set the path to our input data folder.
# system.file() locates the installed package's extdata directory.
data_dir <- system.file("extdata", "water_temperature", package = "waterlogged")

# Define paths to the key input files.
# Raw HOBO files are in separate subfolders per metric — bind_hobo_files()
# requires all files in a folder to be the same metric type.
metadata_file   <- file.path(data_dir, "metadata_wt.csv")
watertemp_folder <- file.path(data_dir, "watertemp")
airtemp_folder   <- file.path(data_dir, "airtemp")

stopifnot(
  "Metadata file not found"      = file.exists(metadata_file),
  "Water temp folder not found"  = dir.exists(watertemp_folder),
  "Air temp folder not found"    = dir.exists(airtemp_folder)
)

cat("Setup complete. Data directory:", data_dir, "\n")

# Preview the metadata to understand what's deployed where.
meta <- read.csv(metadata_file)
print(meta)

# KEY THINGS TO NOTICE:
#   - HAGG_ST_30 has metric = "airtemp"  (TidbiT measuring air temperature)
#   - LEMI_ST_20 has metric = "watertemp" (TidbiT measuring water temperature)
#   - LEMI_ST_20 has TWO deployment rows (same SN, different deploy timestamps)
#     because the logger was redeployed — bind_hobo_files() handles this
#   - The "model" column says "TidBit V2" — this tells bind_hobo_files()
#     to expect a single "Temp" column instead of pressure channels


# =============================================================================
# STEP 1a: Bind Raw Water Temperature Logger Files (v0.1)
# =============================================================================
#
# WHAT: Reads raw HOBO TidbiT CSV files for WATER temperature and links
#       them to the deployment metadata.
#
# WHY:  Raw HOBO files are individual downloads — one CSV per field visit.
#       LEMI_ST_20 has two download files from different dates that need
#       to be combined into a single continuous time series.
#
# HOW IT WORKS:
#   - Reads the HOBO header (row 2) to extract the serial number (SN)
#   - Matches SN + timestamps to the metadata deployments
#   - For TidbiT loggers: renames "Temp, deg C" to "watertemp_C"
#   - Auto-detects Fahrenheit and converts to Celsius if needed
#   - Removes duplicate rows from overlapping downloads
#
# RETURNS: A list with 2 items:
#   [[1]] = data frame of all combined readings
#   [[2]] = a quick-look plot (ggplot) to visually check the data
# -----------------------------------------------------------------------------

# ?bind_hobo_files

wt_bound <- bind_hobo_files(
  path_to_raw_folder = watertemp_folder,
  metadata_path      = metadata_file
)

wt_data <- wt_bound[[1]]
wt_plot <- wt_bound[[2]]

cat("\n--- Water Temperature Data Summary ---\n")
cat("Total rows:  ", nrow(wt_data), "\n")
cat("Stations:    ", paste(unique(wt_data$site_station_code), collapse = ", "), "\n")
cat("Date range:  ", as.character(min(wt_data$timestamp)),
    "to", as.character(max(wt_data$timestamp)), "\n")

# Check what columns we have
cat("Columns:", paste(names(wt_data), collapse = ", "), "\n")

# Temperature range
cat("Water temp range:",
    round(min(wt_data$watertemp_C, na.rm = TRUE), 2), "to",
    round(max(wt_data$watertemp_C, na.rm = TRUE), 2), "deg C\n")

# Print the overview plot
print(wt_plot)


# =============================================================================
# STEP 1b: Bind Raw Air Temperature Logger Files (v0.1)
# =============================================================================
#
# WHAT: Same as Step 1a but for AIR temperature files.
#
# NOTE: bind_hobo_files() reads the metadata and sees metric = "airtemp"
#       for the HAGG_ST_30 logger, so it names the column "airtemp_C"
#       (not "watertemp_C").
# -----------------------------------------------------------------------------

at_bound <- bind_hobo_files(
  path_to_raw_folder = airtemp_folder,
  metadata_path      = metadata_file
)

at_data <- at_bound[[1]]
at_plot <- at_bound[[2]]

cat("\n--- Air Temperature Data Summary ---\n")
cat("Total rows:  ", nrow(at_data), "\n")
cat("Stations:    ", paste(unique(at_data$site_station_code), collapse = ", "), "\n")
cat("Date range:  ", as.character(min(at_data$timestamp)),
    "to", as.character(max(at_data$timestamp)), "\n")
cat("Columns:", paste(names(at_data), collapse = ", "), "\n")

# Temperature range
cat("Air temp range:",
    round(min(at_data$airtemp_C, na.rm = TRUE), 2), "to",
    round(max(at_data$airtemp_C, na.rm = TRUE), 2), "deg C\n")

print(at_plot)


# =============================================================================
# STEP 2: QC Water Temperature — LEMI_ST_20
# =============================================================================
#
# WHAT: Runs automatic quality checks on water temperature data.
#
# WHY:  Water temperature sensors can be affected by:
#       - Ice encasement (frozen creek) — readings near 0 deg C
#       - Dry conditions (sensor out of water) — reads air temperature
#       - Sensor malfunction — impossible negative values
#
# DETECTION RULES:
#   1. edit_temp_lt_minus1  — water temp < -1 C -> set to NA (impossible)
#   2. edit_temp_neg_near   — water temp in [-1, 0) C -> corrected to 0 C
#   3. flag_ice             — water temp < 0.3 C -> flagged (possible ice)
#   4. flag_wt_dry          — water temp > 19 C -> flagged (possible dry)
#                             If airtemp_C is present: also flags when
#                             air-water temp difference < 2 C or air > water
#
# ADJUSTED COLUMN CREATED:
#   watertemp_C_adj — working column (raw values never modified)
#
# NOTE: qc_watertemp() returns data for the selected station only.
#       If you have multiple stations, run it once per station and
#       bind_rows() the results back together.
# -----------------------------------------------------------------------------

# ?qc_watertemp

wt_qc <- qc_watertemp(
  input_data            = wt_data,
  select_station        = "LEMI_ST_20",
  temp_ice_threshold_C  = 0.3,    # flag below this as possible ice
  temp_high_dry_C       = 19,     # flag above this as possible dry
  temp_air_water_diff_C = 2       # air-water proximity threshold (if airtemp present)
)

cat("\n--- Water Temperature QAQC: LEMI_ST_20 ---\n")
cat("Total rows:          ", nrow(wt_qc), "\n")
cat("Temp < -1 C (-> NA): ", sum(wt_qc$edit_temp_lt_minus1, na.rm = TRUE), "\n")
cat("Temp -1 to 0 (-> 0): ", sum(wt_qc$edit_temp_neg_near, na.rm = TRUE), "\n")
cat("Ice flagged:         ", sum(wt_qc$flag_ice, na.rm = TRUE), "\n")
cat("Dry flagged:         ", sum(wt_qc$flag_wt_dry, na.rm = TRUE), "\n")
cat("Adjusted NAs:        ", sum(is.na(wt_qc$watertemp_C_adj)), "\n")

# Show the QAQC log
qaqc_log_wt <- attr(wt_qc, "qaqc_log")
if (!is.null(qaqc_log_wt) && nrow(qaqc_log_wt) > 0) {
  cat("\nQAQC log entries:\n")
  print(qaqc_log_wt[, c("field", "action", "code", "ts_start", "ts_end")])
}

# Compare raw vs adjusted
cat("\nRaw water temperature summary:\n")
print(round(summary(wt_qc$watertemp_C), 2))
cat("\nAdjusted water temperature summary:\n")
print(round(summary(wt_qc$watertemp_C_adj), 2))


# =============================================================================
# STEP 3: Visual Review — Water Temperature
# =============================================================================
#
# WHAT: Creates an interactive plot for reviewing the QAQC results.
#
# HOW TO READ THE PLOT:
#   - Light trace  = raw values (original, unmodified)
#   - Dark trace   = adjusted values (after QAQC)
#   - Colored dots = flag locations
#
# NOTE: For standalone temperature data, pass data via watertemp_data
#       (not wl_data or do_data — those expect multi-metric loggers).
# -----------------------------------------------------------------------------

# ?plot_qaqc_timeseries

plot_qaqc_timeseries(
  watertemp_data = wt_qc,
  select_station = "LEMI_ST_20"
)

# Look for:
#   - Winter periods with sustained near-zero temps (ice)
#   - Summer spikes above ~19 C (sensor may be in shallow/dry water)
#   - Any impossible negative values that were corrected


# =============================================================================
# STEP 4: QC Air Temperature — HAGG_ST_30
# =============================================================================
#
# WHAT: Runs automatic range-check QAQC on air temperature data.
#
# WHY:  Air temperature loggers are simpler to QC — the main concern is
#       values outside the physically plausible range for the region.
#       There's no ice or dry detection because air temperature sensors
#       are always "in air" by design.
#
# DETECTION RULE:
#   edit_temp_range — temp outside [-40, 40] C -> set to NA
#
# NOTE: The column name for air temperature is airtemp_C (not watertemp_C).
#       qc_airtemp() expects this column and creates airtemp_C_adj.
#
# IMPORTANT: bind_hobo_files() names the column based on the metadata
#            metric field. For metric = "airtemp", the column is airtemp_C.
#            For metric = "watertemp", it's watertemp_C.
# -----------------------------------------------------------------------------

# The air temperature data is in at_data from Step 1b, with column
# "airtemp_C" for the HAGG_ST_30 station.

# ?qc_airtemp

at_qc <- qc_airtemp(
  input_data      = at_data,
  select_station  = "HAGG_ST_30",
  temp_low_limit  = -40,    # lower bound for plausible air temp
  temp_high_limit =  40     # upper bound for plausible air temp
)

cat("\n--- Air Temperature QAQC: HAGG_ST_30 ---\n")
cat("Total rows:        ", nrow(at_qc), "\n")
cat("Out of range:      ", sum(at_qc$edit_temp_range, na.rm = TRUE), "\n")
cat("Adjusted NAs:      ", sum(is.na(at_qc$airtemp_C_adj)), "\n")

# Show the QAQC log
qaqc_log_at <- attr(at_qc, "qaqc_log")
if (!is.null(qaqc_log_at) && nrow(qaqc_log_at) > 0) {
  cat("\nQAQC log entries:\n")
  print(qaqc_log_at[, c("field", "action", "code", "ts_start", "ts_end")])
}

# Compare raw vs adjusted
cat("\nRaw air temperature summary:\n")
print(round(summary(at_qc$airtemp_C), 2))
cat("\nAdjusted air temperature summary:\n")
print(round(summary(at_qc$airtemp_C_adj), 2))


# =============================================================================
# STEP 5: Visual Review — Air Temperature
# =============================================================================

plot_qaqc_timeseries(
  airtemp_data   = at_qc,
  select_station = "HAGG_ST_30"
)

# Air temperature plots typically show:
#   - Strong diurnal (day/night) cycles
#   - Seasonal trends (warmer summers, colder winters)
#   - Any out-of-range values that were removed


# =============================================================================
# STEP 6: Manual Adjustments (Examples)
# =============================================================================
#
# This section demonstrates how to manually remove bad data. Apply these
# ONLY after reviewing the plot and identifying specific problem periods.
#
# FOR WATER TEMPERATURE, the three reasons for removal are:
#
#   "ice"         — sensor encased in ice; readings unreliable
#                   Sets watertemp_C_adj to NA
#
#   "dry"         — sensor out of water; reads air temperature instead
#                   Sets watertemp_C_adj to NA
#
#   "disturbance" — sensor moved, debris on sensor, maintenance
#                   Sets watertemp_C_adj to NA
#
# FOR AIR TEMPERATURE, only one reason is supported:
#
#   "disturbance" — sensor malfunction or physical disturbance
#                   Sets airtemp_C_adj to NA
#
# NOTE: For temperature metrics, the keep_temp parameter is ignored
#       because the metric IS temperature — there's no secondary channel.
# -----------------------------------------------------------------------------


# ---- 6a. Remove an ice period from water temperature ----
# Uncomment and adjust timestamps after reviewing the plot.

# ?adjust_logger_NA

# wt_qc <- adjust_logger_NA(
#   input_data       = wt_qc,
#   select_station   = "LEMI_ST_20",
#   metric           = "watertemp",
#   reason_to_adjust = "ice",
#   timestamp_start  = "2022-12-01 00:00:00",
#   timestamp_end    = "2023-03-15 00:00:00",
#   manual_note      = "Sustained near-zero temps — creek frozen"
# )


# ---- 6b. Remove a dry period from water temperature ----

# wt_qc <- adjust_logger_NA(
#   input_data       = wt_qc,
#   select_station   = "LEMI_ST_20",
#   metric           = "watertemp",
#   reason_to_adjust = "dry",
#   timestamp_start  = "2023-08-01 00:00:00",
#   timestamp_end    = "2023-08-15 00:00:00",
#   manual_note      = "Sensor exposed during low water"
# )


# ---- 6c. Remove a disturbance from air temperature ----

# at_qc <- adjust_logger_NA(
#   input_data       = at_qc,
#   select_station   = "HAGG_ST_30",
#   metric           = "airtemp",
#   reason_to_adjust = "disturbance",
#   timestamp_start  = "2023-01-10 00:00:00",
#   timestamp_end    = "2023-01-12 00:00:00",
#   manual_note      = "Logger knocked off mount by wildlife"
# )


# ---- After adjustments, re-plot to verify ----

# plot_qaqc_timeseries(
#   watertemp_data = wt_qc,
#   select_station = "LEMI_ST_20"
# )

# plot_qaqc_timeseries(
#   airtemp_data   = at_qc,
#   select_station = "HAGG_ST_30"
# )


# =============================================================================
# STEP 7: Final Review
# =============================================================================

plot_qaqc_timeseries(
  watertemp_data = wt_qc,
  select_station = "LEMI_ST_20"
)

plot_qaqc_timeseries(
  airtemp_data   = at_qc,
  select_station = "HAGG_ST_30"
)


# =============================================================================
# STEP 8: Export Clean Data
# =============================================================================
#
# WHAT: Writes the final processed data to v1.0 CSV files, one file per
#       station per year, stored in <output_folder>/<year>/clean/.
#
# WATER TEMPERATURE FILES:
#   Filename pattern: LEMI_ST_20_WT_YYYYMMDD_YYYYMMDD_v1.0.csv
#   Required columns: watertemp_C (raw), watertemp_C_adj (adjusted)
#   Auto-included: any flag_* and edit_* columns
#
# AIR TEMPERATURE FILES:
#   Filename pattern: HAGG_ST_30_AT_YYYYMMDD_YYYYMMDD_v1.0.csv
#   Required columns: airtemp_C (raw)
#   Optional: airtemp_C_adj (adjusted)
#   Auto-included: any flag_* and edit_* columns
# -----------------------------------------------------------------------------

output_folder <- file.path(tempdir(), "temperature_output")

# Export water temperature
write_clean_data(
  input_data            = wt_qc,
  path_to_output_folder = output_folder,
  metric                = "watertemp"
)
cat("Water temperature data written to:", output_folder, "\n")

# Export air temperature
write_clean_data(
  input_data            = at_qc,
  path_to_output_folder = output_folder,
  metric                = "airtemp"
)
cat("Air temperature data written to:", output_folder, "\n")

# List the exported files
exported <- list.files(output_folder, recursive = TRUE, pattern = "\\.csv$")
cat("\nExported files:\n")
cat(paste(" ", exported, collapse = "\n"), "\n")


# =============================================================================
# RECAP: What Happened at Each Step
# =============================================================================
#
#   Step 1:  bind_hobo_files()         -> wt_data  (raw temps, all stations)
#   Step 2:  qc_watertemp()            -> wt_qc    (LEMI_ST_20, water temp)
#   Step 3:  plot_qaqc_timeseries()    -> (visual review, water temp)
#   Step 4:  qc_airtemp()              -> at_qc    (HAGG_ST_30, air temp)
#   Step 5:  plot_qaqc_timeseries()    -> (visual review, air temp)
#   Step 6:  Manual adjustments        -> wt_qc / at_qc (updated)
#   Step 7:  plot_qaqc_timeseries()    -> (final review)
#   Step 8:  write_clean_data()        -> CSV files (v1.0)
#
# KEY COLUMNS:
#
#   Water temperature:
#     watertemp_C       = raw water temperature (never modified)
#     watertemp_C_adj   = adjusted water temperature (QAQC applied)
#     flag_ice          = possible ice (data NOT changed, review needed)
#     flag_wt_dry       = possible dry sensor (data NOT changed)
#     edit_temp_lt_minus1 = TRUE where temp < -1 C was set to NA
#     edit_temp_neg_near  = TRUE where temp -1 to 0 C was set to 0
#
#   Air temperature:
#     airtemp_C         = raw air temperature (never modified)
#     airtemp_C_adj     = adjusted air temperature (QAQC applied)
#     edit_temp_range   = TRUE where out-of-range values set to NA
#
# COMPARED TO OTHER PIPELINES:
#   - No barometric pressure step (not needed for temperature)
#   - No unit conversion step (already in deg C)
#   - No drift correction step (temperature loggers don't drift)
#   - Different QAQC function per metric (qc_watertemp vs qc_airtemp)
#   - adjust_logger_NA() for air temp only supports "disturbance"
#
# =============================================================================

cat("\n--- Tutorial complete! ---\n")
