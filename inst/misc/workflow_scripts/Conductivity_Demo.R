# =============================================================================
# TUTORIAL: Full Conductivity Pipeline
# =============================================================================
#
# This script walks through the complete conductivity processing pipeline
# using the `waterlogged` R package. We use dummy data files from 3
# sites/loggers to simulate how the package processes data from multiple
# stations simultaneously. We will then focus on simulated data from the
# BUGS_WE_01 site to demonstrate the full QC pipeline.
#
# WHAT THIS SCRIPT DOES (in plain English):
#   1. Reads raw conductivity logger files (from multiple stations) and links
#      them to deployment metadata
#   2. Runs automatic quality checks (spikes, ice, dry sensor)
#   3. Visually review plots to identify problem periods
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
# DATA USED:
#   From metadata/:
#     - metadata.csv          — logger deployment records
#
#   From conductivity/:
#     - raw/                  — 3 raw conductivity CSVs:
#                                 cond-1.csv: BUGS_WE_01 (SN 82345678)
#                                 cond-2.csv: BIRD_WE_01 (SN 89502162)
#                                 cond-3.csv: FISH_ST_01 (SN 88741256)
#     - baro/                 — 2 clean barometric data files:
#                                 BUGS_WE_01_BARO_2024_v1.0.csv
#                                 BUGS_WE_01_BARO_2025_v1.0.csv
#                                 (optional files used for augmenting automated QC)
#
# =============================================================================


# -----------------------------------------------------------------------------
# PART 0: Setup
# -----------------------------------------------------------------------------

# Load the waterlogged package.
# You must install from the main branch:
#   install.packages("pak")
#   pak::pkg_install("dfo-pacific-science/waterlogged@main")

library(waterlogged)

# Set the path to the input data folder.
# system.file() locates the installed package's extdata directory.
data_dir <- system.file("extdata", "conductivity", package = "waterlogged")
meta_dir <- system.file("extdata", "metadata", package = "waterlogged")

metadata_file   <- file.path(meta_dir, "metadata.csv")
cond_folder     <- file.path(data_dir, "raw")
# reference_file  <- file.path(data_dir, "field_references.csv")

stopifnot(
  "Metadata file not found"         = file.exists(metadata_file),
  "COND folder not found"           = dir.exists(cond_folder)
  # "Field references file not found" = file.exists(reference_file)
)

cat("Setup complete. All input files found.\n")


# =============================================================================
# STEP 1: Bind Raw Conductivity Logger Files (v0.1)
# =============================================================================
#
# WHAT: Reads the raw HOBO U24 conductivity logger CSVs and links them to
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
# ABOUT THE MULTI-STATION DATA:
#   This script processes data from three separate HOBO U24 loggers at once:
#     - BUGS_WE_01 (cond-1.csv, SN 82345678): April 2024 to April 2025 (~1 year)
#     - BIRD_WE_01 (cond-2.csv, SN 89502162): April 2024 to August 2024 (~4 months)
#     - FISH_ST_01 (cond-3.csv, SN 88741256): October 2023 to October 2024 (~1 year)
#
#   Our primary focus for exploring the QC outputs will be BUGS_WE_01,
#   which has a continuous deployment record in a wetland environment.
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
#                   readings (> spike_flag_uScm, default 200)
#   2. Ice        — water temperature below 0.3 °C (sensor may be frozen)
#   3. Temp range — temperature outside plausible limits (< -2 or > 40 deg C);
#                   these extreme values are converted to NA.
#   4. Cond range – conductivity outside of plausible limits (< 0 or >
#                   2500 µS/cm); the upper range should be set according to the
#                   specific calibration or measurement threshold of the logger
#                   (refer to device specifications).
#   5. Dry sensor — very low conductivity (<= dry_flag_uScm, default 10)
#                   and air-water temp difference <= 2 deg C (if air temp is
#                   available) for a rolling 12-hour window.
#
# ADJUSTED COLUMNS CREATED:
#   conduct_uScm_adj  — adjusted conductivity (working column)
#   watertemp_C_adj   — adjusted water temperature
#
# FLAG COLUMNS:
#   FLAG_DISTURBANCE  — TRUE where a conductivity spike was detected
#   FLAG_ICE          — TRUE where water temp indicates possible ice/slush
#   OUTLIER_WT        — TRUE where temp was out of bounds (set to NA)
#   OUTLIER_CO        - TRUE where conductivity was out of bounds (set to NA)
#   FLAG_DRY          — TRUE where sensor appears to be out of water
# -----------------------------------------------------------------------------

?qc_conductivity

cond_qc <- qc_conductivity(
  input_data                 = cond_data,
  select_station             = "BUGS_WE_01",
  temp_low_limit_C           = -2,
  temp_high_limit_C          = 40,
  temp_ice_flag_C            = 0.3,
  spike_flag_uScm            = 200,
  dry_flag_uScm              = 10,
  temp_air_water_diff_C      = 2
)

# Summarize flags
cat("QAQC results for BUGS_WE_01:\n")
cat("  Total rows:       ", nrow(cond_qc), "\n")
cat("  Spikes:           ", sum(cond_qc$FLAG_SPIKE, na.rm = TRUE), "\n")
cat("  Ice flagged:      ", sum(cond_qc$FLAG_ICE, na.rm = TRUE), "\n")
cat("  Temp outlier:     ", sum(cond_qc$OUTLIER_WT, na.rm = TRUE), "\n")
cat("  Dry flagged:      ", sum(cond_qc$FLAG_DRY, na.rm = TRUE), "\n")

# Show the QAQC log
qaqc_log <- attr(cond_qc, "qaqc_log")
if (!is.null(qaqc_log) && nrow(qaqc_log) > 0) {
  cat("\nQAQC log entries:\n")
  print(qaqc_log[, c("field", "action", "code", "ts_start", "ts_end")])
}


# You can improve dry event detection (distinguishing from ice events)
# by adding continuous air temperature data, such as from a TidbiT or
# barometric logger

baro_folder     <- file.path(data_dir, "baro")
baro_files      <- list.files(path = baro_folder, pattern = "\\.csv$", full.names = TRUE)
baro_filenames  <- basename(baro_files)

cat("Found", length(baro_files), "CSV file(s):\n")
cat(paste(" -", baro_filenames), sep = "\n")
cat("\n")

baro_data <- lapply(baro_files, read.csv) |>
  dplyr::bind_rows()

cat("Barometric data:", nrow(baro_data), "rows\n")
cat("Stations:", paste(unique(baro_data$site_station_code), collapse = ", "), "\n")
cat("Date range:", as.character(min(baro_data$timestamp, na.rm = TRUE)),
    "to", as.character(max(baro_data$timestamp, na.rm = TRUE)), "\n")
cat("Temperature range:",
    round(min(baro_data$airtemp_C_adj, na.rm = TRUE), 1), "to",
    round(max(baro_data$airtemp_C_adj, na.rm = TRUE), 1), "deg C\n")

# Re-run qc_conductivity with baro (air temperature) data added
cond_with_baro <- add_nearest_baro(
  input_data = cond_data,
  baro_data_path = baro_folder,
  metric = "conductivity",
  baro_site_selection = "auto",
  metadata_path = metadata_file
)

cond_airtemp <- cond_with_baro[[1]]

cond_qc_at <- qc_conductivity(
  input_data                 = cond_airtemp,
  select_station             = "BUGS_WE_01",
  temp_low_limit_C           = -2,
  temp_high_limit_C          = 40,
  temp_ice_flag_C            = 0.3,
  spike_flag_uScm            = 200,
  dry_flag_uScm              = 10,
  temp_air_water_diff_C      = 2
)

cat("  Dry flagged:      ", sum(cond_qc_at$FLAG_DRY, na.rm = TRUE), "\n")

# =============================================================================
# STEP 3: Visual Review
# =============================================================================
#
# WHAT: Creates an interactive plot for reviewing the conductivity data.
#
# HOW TO READ THE PLOT:
#   - Light traces  = raw values (original, unmodified)
#   - Dark traces   = adjusted values (after QAQC)
#   - Black shapes  = flag locations
#   - Use the range slider to zoom into specific time periods
# -----------------------------------------------------------------------------

?plot_qaqc_timeseries

plot_qaqc_timeseries(
  cond_data      = cond_qc,
  select_station = "BUGS_WE_01"
)

# Notice the difference in FLAG_DRY when qc is run on data containing air temp
plot_qaqc_timeseries(
  cond_data      = cond_qc_at,
  select_station = "BUGS_WE_01"
)

# cat("Review the plot. Look for:\n")
# cat("  - Sudden jumps (disturbance, debris on sensor)\n")
# cat("  - Flat low readings (sensor out of water)\n")


# =============================================================================
# STEP 4: Temperature Compensation (Specific Conductance)
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
# THE MATH (EN 27888):
#   Uses a 5th-order polynomial to model the temperature dependence
#   of conductivity. The function calculates what the conductivity
#   WOULD be if the water were at exactly 25 deg C.
#
# INPUTS:  conduct_uScm_adj and watertemp_C_adj
# OUTPUT:  spc_uScm_adj (specific conductance in uS/cm at 25 deg C)
#
# -----------------------------------------------------------------------------

?convert_conductivity_spc_uScm

cond_qc_spc <- convert_conductivity_spc_uScm(cond_qc_at)

cat("Temperature compensation complete.\n")
cat("Raw conductivity range:   ",
    round(min(cond_qc_spc$conduct_uScm_adj, na.rm = TRUE), 1), "to",
    round(max(cond_qc_spc$conduct_uScm_adj, na.rm = TRUE), 1), "uS/cm\n")
cat("Specific conductance range:",
    round(min(cond_qc_spc$spc_uScm_adj, na.rm = TRUE), 1), "to",
    round(max(cond_qc_spc$spc_uScm_adj, na.rm = TRUE), 1), "uS/cm @ 25C\n")

# Preview: compare EC vs SPC at different temperatures
cat("\nSample comparison (EC vs SPC at varying temperatures):\n")
sample_rows <- cond_qc_spc[as.integer(seq(1, nrow(cond_qc), length.out = 6)),
                        c("timestamp", "watertemp_C_adj",
                          "conduct_uScm_adj", "spc_uScm_adj")]
print(sample_rows)


# =============================================================================
# STEP 5: Manual Adjustments (Examples)
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


# ---- 5a. Remove a dry period ----

?adjust_logger_NA

cond_qc_adj <- adjust_logger_NA(
  input_data       = cond_qc_spc,
  select_station   = "BUGS_WE_01",
  metric           = "conductivity",
  reason_to_adjust = "dry",
  timestamp_start  = "2024-08-07 14:00:00",
  timestamp_end    = "2024-09-12 12:00:00",
  manual_note      = "Sensor exposed during low water"
)


# ---- 5b. Remove an ice period ----

cond_qc_adj <- adjust_logger_NA(
  input_data       = cond_qc_adj,
  select_station   = "BUGS_WE_01",
  metric           = "conductivity",
  reason_to_adjust = "ice",
  timestamp_start  = "2025-01-17 02:00:00",
  timestamp_end    = "2025-02-18 11:00:00",
  manual_note      = "Sensor encased in ice"
)


# ---- 5c. Remove a disturbance ----

cond_qc_adj <- adjust_logger_NA(
  input_data       = cond_qc_adj,
  select_station   = "BUGS_WE_01",
  metric           = "conductivity",
  reason_to_adjust = "disturbance",
  timestamp_start  = "2024-05-31 03:50:00",
  timestamp_end    = "2024-05-31 04:10:00",
  manual_note      = "Remove sensor spike"
)


# ---- After adjustments, re-plot to verify ----

# plot_qaqc_timeseries(
#   cond_data      = cond_qc_adj,
#   select_station = "BUGS_WE_01"
# )


# =============================================================================
# STEP 6: Final Review
# =============================================================================


plot_qaqc_timeseries(
  cond_data      = cond_qc_adj,
  select_station = "BUGS_WE_01"
)



# =============================================================================
# STEP 7: Export Clean Data
# =============================================================================
#
# WHAT: Writes the final processed data to a v1.0 CSV file, one file per
#       station per year, stored in <output_folder>/<year>/clean/.
#
# COLUMNS WRITTEN:
#   Required: conduct_uScm (raw, never modified)
#   Optional (if present): conduct_uScm_adj, spc_uScm_adj,
#                          watertemp_C, watertemp_C_adj
#   Auto-included: qc_code column
# -----------------------------------------------------------------------------

# Set this to your desired output folder. We use tempdir() for the demo.
output_folder <- file.path(tempdir(), "conductivity_output")

write_clean_data(
  input_data            = cond_qc_adj,
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
#   Step 4:  convert_conductivity_spc_uScm() -> cond_qc (adds SPC)
#   Step 5:  Manual adjustments           -> cond_qc    (updated)
#   Step 6:  plot_qaqc_timeseries()       -> (final review)
#   Step 7:  write_clean_data()            -> CSV file (v1.0)
#
# KEY COLUMNS:
#   conduct_uScm      = raw electrical conductivity (never modified)
#   conduct_uScm_adj  = adjusted EC (QAQC + drift corrections applied)
#   spc_uScm_adj      = specific conductance at 25 C (temp-compensated)
#   watertemp_C       = raw water temperature (never modified)
#   watertemp_C_adj   = adjusted water temperature
#   qc_code           = Data quality status indicator:
#                         "REAL"      - Verified and accurate
#                         "REMOVED"   - Removed via automatic or manual QC
#                         "NA"        - Data produced a logger error code
#                         "CORRECTED" - Manually adjusted (shifted, not removed)
#                         "DRY"       - Manually removed due to a dry event
#
# For analysis, use spc_uScm_adj — it's the fully corrected, temperature-
# compensated specific conductance. Compare with conduct_uScm to see
# what the processing changed.
# =============================================================================

cat("\n--- Tutorial complete! ---\n")
