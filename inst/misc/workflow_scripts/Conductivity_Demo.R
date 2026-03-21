# =============================================================================
# TUTORIAL: Full Conductivity Pipeline — TUMT_WE_40
# =============================================================================
#
# This script walks through the complete conductivity processing pipeline
# using the `waterlogged` R package. We use real test data from the
# TUMT_WE_40 wetland station as our example.
#
# WHAT THIS SCRIPT DOES (in plain English):
#   1. Reads raw conductivity logger files and links them to metadata
#   2. Runs automatic quality checks (spikes, ice, dry sensor)
#   3. Corrects sensor drift using field reference measurements
#   4. Converts electrical conductivity to specific conductance (SPC)
#   5. Shows how to manually remove bad data
#   6. Exports the final clean dataset
#
# WHAT IS CONDUCTIVITY?
#   Electrical conductivity (EC) measures how well water conducts
#   electricity, reported in microsiemens per centimetre (uS/cm).
#   Higher conductivity means more dissolved ions (salts, minerals).
#   It's a quick indicator of water chemistry changes — runoff events,
#   groundwater inputs, or pollution can all shift conductivity.
#
# WHY TEMPERATURE COMPENSATION?
#   Conductivity varies with water temperature — warmer water conducts
#   better. To compare readings across seasons (or between sites at
#   different temperatures), we normalize to a standard temperature
#   (25 deg C). This normalized value is called "specific conductance"
#   (SPC) and is the standard for reporting.
#
# NO BAROMETRIC DATA NEEDED:
#   Unlike water level and DO, conductivity does not require atmospheric
#   pressure correction. This makes the pipeline shorter.
#
# DATA USED (all in conductivity/input_data/):
#   - metadata.csv          — logger deployment records
#   - COND/                 — 1 conductivity CSV from TUMT_WE_40 (SN 21245027)
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
# system.file() locates the installed package's extdata directory.
data_dir <- system.file("extdata", "conductivity", package = "waterlogged")

metadata_file   <- file.path(data_dir, "metadata.csv")
cond_folder     <- file.path(data_dir, "COND")
reference_file  <- file.path(data_dir, "field_references.csv")

stopifnot(
  "Metadata file not found"         = file.exists(metadata_file),
  "COND folder not found"           = dir.exists(cond_folder),
  "Field references file not found" = file.exists(reference_file)
)

cat("Setup complete. All input files found.\n")


# =============================================================================
# STEP 1: Bind Raw Conductivity Logger Files (v0.1)
# =============================================================================
#
# WHAT: Reads the raw HOBO U24 conductivity logger CSV and links it to
#       the deployment metadata.
#
# WHY:  The HOBO U24 records two (or three) channels:
#         - Low Range conductivity (uS/cm) — used for most freshwater
#         - Full Range conductivity (uS/cm) — for higher conductivity water
#         - Water temperature (deg C)
#       This step parses them into: conduct_uScm and watertemp_C.
#       When both Low and Full Range are present, the function selects
#       the appropriate one and records the source in conduct_source.
#
# ABOUT THE TUMT_WE_40 DATA:
#   - Logger: HOBO U24 (SN 21245027)
#   - Deployed: April 17 to May 28, 2025 (~6 weeks)
#   - Location: TUMT wetland, station WE_40
#   - One download file covering the full deployment
# -----------------------------------------------------------------------------

cond_bound <- bind_hobo_files(
  path_to_raw_folder = cond_folder,
  metadata_path      = metadata_file
)

cond_data <- cond_bound[[1]]
cond_plot <- cond_bound[[2]]

cat("Conductivity data:", nrow(cond_data), "rows\n")
cat("Stations:", paste(unique(cond_data$site_station_code), collapse = ", "), "\n")
cat("Date range:", as.character(min(cond_data$timestamp)),
    "to", as.character(max(cond_data$timestamp)), "\n")
cat("Conductivity range:",
    round(min(cond_data$conduct_uScm, na.rm = TRUE), 1), "to",
    round(max(cond_data$conduct_uScm, na.rm = TRUE), 1), "uS/cm\n")
cat("Water temp range:",
    round(min(cond_data$watertemp_C, na.rm = TRUE), 1), "to",
    round(max(cond_data$watertemp_C, na.rm = TRUE), 1), "deg C\n")

# Check for dual-range channel handling
if ("conduct_source" %in% names(cond_data)) {
  cat("Conductivity source channels used:\n")
  print(table(cond_data$conduct_source))
}

# Preview
head(cond_data[, c("timestamp", "conduct_uScm", "watertemp_C",
                    "site_station_code")])

# Display overview plot
print(cond_plot)


# =============================================================================
# STEP 2: Automatic Conductivity QAQC
# =============================================================================
#
# WHAT: Runs automatic quality checks on the conductivity data and flags
#       suspicious values.
#
# WHY:  Conductivity sensors can be affected by ice, air exposure (dry),
#       biofouling, or physical disturbances. This step catches common
#       problems automatically.
#
# WHAT IT CHECKS:
#   1. Spikes     — sudden large jumps in conductivity between consecutive
#                   readings (> disturbance_threshold_uScm, default 200)
#   2. Ice        — water temperature below 0.1 deg C (sensor may be frozen)
#   3. Temp range — temperature outside plausible limits (-2 to 40 deg C)
#   4. Dry sensor — very low conductivity (< dry_threshold_uScm, default 10)
#                   or air-water temp difference < 2 deg C (sensor in air)
#   5. Near-zero temp — water temp between -1 and 0 deg C (corrected to 0)
#
# ADJUSTED COLUMNS CREATED:
#   conduct_uScm_adj  — adjusted conductivity (working column)
#   watertemp_C_adj   — adjusted water temperature
#
# FLAG COLUMNS:
#   FLAG_DISTURBANCE  — TRUE where a conductivity spike was detected
#   FLAG_ICE          — TRUE where water temp < 0.1 C (possible ice)
#   FLAG_RANGE        — TRUE where temp is outside the range limits
#   OUTLIER_WT        — TRUE where temp was between -1 and 0 (set to 0)
#   FLAG_DRY          — TRUE where sensor appears to be out of water
# -----------------------------------------------------------------------------

?qc_conductivity

cond_qc <- qc_conductivity(
  input_data                 = cond_data,
  select_station             = "TUMT_WE_40",
  temp_low_limit_C           = -2,
  temp_high_limit_C          = 40,
  disturbance_threshold_uScm = 200,
  dry_threshold_uScm         = 10,
  air_water_diff_threshold_C = 2
)

# Summarize flags
cat("QAQC results for TUMT_WE_40:\n")
cat("  Total rows:       ", nrow(cond_qc), "\n")
cat("  Spikes:           ", sum(cond_qc$FLAG_DISTURBANCE, na.rm = TRUE), "\n")
cat("  Ice flagged:      ", sum(cond_qc$FLAG_ICE, na.rm = TRUE), "\n")
cat("  Temp out of range:", sum(cond_qc$FLAG_RANGE, na.rm = TRUE), "\n")
cat("  Temp outlier:     ", sum(cond_qc$OUTLIER_WT, na.rm = TRUE), "\n")
cat("  Dry flagged:      ", sum(cond_qc$FLAG_DRY, na.rm = TRUE), "\n")

# Show the QAQC log
qaqc_log <- attr(cond_qc, "qaqc_log")
if (!is.null(qaqc_log) && nrow(qaqc_log) > 0) {
  cat("\nQAQC log entries:\n")
  print(qaqc_log[, c("field", "action", "code", "ts_start", "ts_end")])
}


# =============================================================================
# STEP 3: Visual Review
# =============================================================================
#
# WHAT: Creates an interactive plot for reviewing the conductivity data.
#
# HOW TO READ THE PLOT:
#   - Light traces  = raw values (original, unmodified)
#   - Dark traces   = adjusted values (after QAQC)
#   - Colored dots  = flag locations
#   - Use the range slider to zoom into specific time periods
# -----------------------------------------------------------------------------

?plot_qaqc_timeseries

plot_qaqc_timeseries(
  cond_data      = cond_qc,
  select_station = "TUMT_WE_40"
)

# cat("Review the plot. Look for:\n")
# cat("  - Sudden jumps (disturbance, debris on sensor)\n")
# cat("  - Flat low readings (sensor out of water)\n")
# cat("  - Gradual drift from biofouling\n")


# =============================================================================
# STEP 4: Drift Correction
# =============================================================================
#
# WHAT: Adjusts the logged conductivity to match field reference
#       measurements, correcting for sensor drift between site visits.
#
# WHY:  Conductivity sensors can drift due to biofouling (algae/biofilm
#       coating the electrodes), mineral deposits, or calibration drift.
#       Field measurements taken with a calibrated handheld meter give
#       us "truth" values to correct against.
#
# OUR TUMT_WE_40 REFERENCE DATA:
#   - 4/21/2025: check (26 uS/cm) — initial check, no sensor touching
#   - 5/7/2025:  calibration (25 uS/cm) — sensor cleaned and recalibrated
#   - 5/16/2025: calibration (28 uS/cm) — second recalibration
#   - 5/26/2025: check (27 uS/cm) — final check visit
#
# HOW DRIFT CORRECTION WORKS:
#   Between each pair of reference events, the function calculates how
#   far the logger drifted from truth and applies a linear correction:
#
#   "ratio" method (default):
#     correction_factor = reference_value / logger_value
#     Interpolated linearly from 1.0 at segment start to the ratio at
#     the reference timestamp. Good when drift is proportional.
#
#   "offset" method:
#     correction_offset = reference_value - logger_value
#     Interpolated linearly from 0 at segment start. Good when drift
#     is a constant shift.
#
# COLUMN MODIFIED: conduct_uScm_adj
# NEW FLAG:        edit_cond_drift (TRUE where correction was applied)
# -----------------------------------------------------------------------------


# Preview the reference data for this station
ref_data <- read.csv(reference_file)
cat("Field reference measurements for TUMT_WE_40:\n")
print(ref_data[ref_data$metric == "cond", ])

?wq_drift_correction

cond_qc <- wq_drift_correction(
  input_data     = cond_qc,
  ref_data_path  = reference_file,
  select_station = "TUMT_WE_40",
  metric         = "cond",
  method         = "ratio",        # multiplicative correction
  max_ref_gap_h  = 1               # match logger to reference within 1 hour
)

# Check drift correction results
if ("edit_cond_drift" %in% names(cond_qc)) {
  drift_count <- sum(cond_qc$edit_cond_drift, na.rm = TRUE)
  cat("Rows corrected by drift:", drift_count, "of", nrow(cond_qc), "\n")
} else {
  cat("No drift correction column found — check reference data alignment.\n")
}

# Show the drift correction log
drift_log <- attr(cond_qc, "qaqc_log")
if (!is.null(drift_log) && nrow(drift_log) > 0) {
  cat("\nDrift correction log:\n")
  print(drift_log)
}


# =============================================================================
# STEP 5: Temperature Compensation (Specific Conductance)
# =============================================================================
#
# WHAT: Converts electrical conductivity (EC) to specific conductance (SPC)
#       normalized to 25 deg C.
#
# WHY:  Conductivity changes with water temperature — warmer water
#       conducts electricity better. A reading of 100 uS/cm at 10 deg C
#       represents a different amount of dissolved ions than 100 uS/cm
#       at 25 deg C.
#
#       Specific conductance removes this temperature effect, making it
#       possible to:
#         - Compare readings across seasons (cold winter vs warm summer)
#         - Compare readings between sites at different temperatures
#         - Detect real water chemistry changes vs temperature changes
#
# THE MATH (ISO 27888):
#   Uses a 5th-order polynomial to model the temperature dependence
#   of conductivity. The function calculates what the conductivity
#   WOULD be if the water were at exactly 25 deg C.
#
# INPUTS:  conduct_uScm_adj and watertemp_C_adj
# OUTPUT:  spc_uScm_adj (specific conductance in uS/cm at 25 deg C)
#
# IMPORTANT: Run this AFTER drift correction, so the temperature
#            compensation uses the drift-corrected conductivity values.
# -----------------------------------------------------------------------------

?conductivity_temp_compensation


cond_qc <- conductivity_temp_compensation(cond_qc)

cat("Temperature compensation complete.\n")
cat("Raw conductivity range:   ",
    round(min(cond_qc$conduct_uScm_adj, na.rm = TRUE), 1), "to",
    round(max(cond_qc$conduct_uScm_adj, na.rm = TRUE), 1), "uS/cm\n")
cat("Specific conductance range:",
    round(min(cond_qc$spc_uScm_adj, na.rm = TRUE), 1), "to",
    round(max(cond_qc$spc_uScm_adj, na.rm = TRUE), 1), "uS/cm @ 25C\n")

# Preview: compare EC vs SPC at different temperatures
cat("\nSample comparison (EC vs SPC at varying temperatures):\n")
sample_rows <- cond_qc[as.integer(seq(1, nrow(cond_qc), length.out = 6)),
                        c("timestamp", "watertemp_C_adj",
                          "conduct_uScm_adj", "spc_uScm_adj")]
print(sample_rows)


# =============================================================================
# STEP 6: Manual Adjustments (Examples)
# =============================================================================
#
# This section demonstrates how to manually remove bad data. Apply these
# ONLY after reviewing the plot and identifying specific problem periods.
#
# For conductivity data, the three reasons for removal are:
#
#   "ice"         — sensor frozen; conductivity unreliable
#                   Sets conduct_uScm_adj and watertemp_C_adj to NA
#
#   "dry"         — sensor out of water; readings are meaningless
#                   Sets conduct_uScm_adj and watertemp_C_adj to NA
#
#   "disturbance" — physical disturbance (e.g., debris, maintenance)
#                   Sets conduct_uScm_adj and watertemp_C_adj to NA
#
# NOTE: adjust_logger_NA() expects metric = "conductivity" and targets
#       the column conduct_uScm_adj (matching qc_conductivity output).
# -----------------------------------------------------------------------------


# ---- 6a. Remove a dry period ----


?adjust_logger_NA

# cond_qc <- adjust_logger_NA(
#   input_data       = cond_qc,
#   select_station   = "TUMT_WE_40",
#   metric           = "conductivity",
#   reason_to_adjust = "dry",
#   timestamp_start  = "2025-05-20 00:00:00",
#   timestamp_end    = "2025-05-22 00:00:00",
#   manual_note      = "Sensor exposed during low water"
# )


# ---- 6b. Remove a disturbance ----

# cond_qc <- adjust_logger_NA(
#   input_data       = cond_qc,
#   select_station   = "TUMT_WE_40",
#   metric           = "conductivity",
#   reason_to_adjust = "disturbance",
#   timestamp_start  = "2025-05-10 12:00:00",
#   timestamp_end    = "2025-05-10 14:00:00",
#   manual_note      = "Debris caught on sensor during storm"
# )


# ---- After adjustments, re-plot to verify ----

# plot_qaqc_timeseries(
#   cond_data      = cond_qc,
#   select_station = "TUMT_WE_40"
# )


# =============================================================================
# STEP 7: Final Review
# =============================================================================


plot_qaqc_timeseries(
  cond_data      = cond_qc,
  select_station = "TUMT_WE_40"
)



# =============================================================================
# STEP 8: Export Clean Data
# =============================================================================
#
# WHAT: Writes the final processed data to a v1.0 CSV file, one file per
#       station per year, stored in <output_folder>/<year>/clean/.
#
# COLUMNS WRITTEN:
#   Required: conduct_uScm (raw, never modified)
#   Optional (if present): conduct_uScm_adj, spc_uScm_adj,
#                          watertemp_C, watertemp_C_adj
#   Auto-included: any flag_* and edit_* columns
# -----------------------------------------------------------------------------

# Set this to your desired output folder. We use tempdir() for the demo.
output_folder <- file.path(tempdir(), "conductivity_output")

write_clean_data(
  input_data            = cond_qc,
  path_to_output_folder = output_folder,
  metric                = "conductivity"
)

cat("Clean data written to:", output_folder, "\n")


# =============================================================================
# RECAP: What Happened at Each Step
# =============================================================================
#
#   Step 1:  bind_hobo_files() on COND    -> cond_data  (raw conductivity)
#   Step 2:  qc_conductivity()          -> cond_qc    (auto QAQC'd)
#   Step 3:  plot_qaqc_timeseries()       -> (visual review)
#   Step 4:  wq_drift_correction()        -> cond_qc    (drift corrected)
#   Step 5:  conductivity_temp_compensation() -> cond_qc (adds SPC)
#   Step 6:  Manual adjustments           -> cond_qc    (updated)
#   Step 7:  plot_qaqc_timeseries()       -> (final review)
#   Step 8:  write_clean_data()            -> CSV file (v1.0)
#
# KEY COLUMNS:
#   conduct_uScm      = raw electrical conductivity (never modified)
#   conduct_uScm_adj  = adjusted EC (QAQC + drift corrections applied)
#   spc_uScm_adj      = specific conductance at 25 C (temp-compensated)
#   watertemp_C       = raw water temperature (never modified)
#   watertemp_C_adj   = adjusted water temperature
#   flag_co_*         = "something looks off — review" (data not changed)
#   edit_cond_drift   = TRUE where drift correction was applied
#
# For analysis, use spc_uScm_adj — it's the fully corrected, temperature-
# compensated specific conductance. Compare with conduct_uScm to see
# what the processing changed.
# =============================================================================

cat("\n--- Tutorial complete! ---\n")
