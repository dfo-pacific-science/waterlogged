# =============================================================================
# TUTORIAL: Full Water Level Pipeline — ALBR_ST_30
# =============================================================================
#
# This script walks through the complete water level processing pipeline
# using the `waterlogged` R package. We use real test data from the
# ALBREDA RIVER station (ALBR_ST_30) as our example.
#
# WHAT THIS SCRIPT DOES:
#   1. Reads raw pressure logger files and links them to deployment metadata
#   2. Quality-checks the barometric (air pressure) data
#   3. Reads raw water level logger files
#   4. Attaches the nearest barometric data to the water level data
#   5. Converts pressure readings (kPa) into water level (metres)
#   6. Runs automatic quality checks on the water level data
#   7. Shows how to make manual corrections (ice, dry sensor, spikes)
#   8. Exports the final clean dataset
#
# WHY BAROMETRIC DATA MATTERS:
#   HOBO U20 loggers measure absolute pressure — that includes BOTH
#   the weight of water above the sensor AND the weight of the atmosphere.
#   To get just the water depth, we need to subtract the atmospheric
#   pressure. That's why we process barometric data first.
#
# DATA USED (all in water_level/input_data/):
#   - metadata.csv        — logger deployment records
#   - baro/               — 4 CSVs from ALBR_ST_30 baro logger (SN 20810501)
#   - level/              — 2 CSVs from ALBR_ST_30 water level logger (SN 20533550)
#   - stage_readings.csv  — manual field measurements (staff gauge readings)
#   - benchmarks.csv      — survey benchmark data for datum setting
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
# All paths are relative to the demos/ project root (where demos.Rproj lives).

# TODO update directory to waterlogged\inst\extdata\water_level
data_dir <- "water_level/input_data"

# Define paths to the key input files.
# We keep these in one place so they're easy to change.

metadata_file   <- file.path(data_dir, "metadata.csv")
baro_folder     <- file.path(data_dir, "baro")
level_folder    <- file.path(data_dir, "level")
reference_file  <- file.path(data_dir, "stage_readings.csv")
level_runs_file <- file.path(data_dir, "benchmarks.csv")

# =============================================================================
# STEP 1: Bind Raw Barometric Logger Files (v0.1)
# =============================================================================
#
# WHAT: Reads all raw HOBO CSV files from the baro folder, parses them,
#        and links each reading to its deployment in the metadata.
#
# WHY:  Raw HOBO files are individual downloads — one CSV per field visit.
#       This step combines them into a single tidy data frame with
#       consistent column names and deployment info attached.
#
# HOW IT WORKS:
#   - Reads the HOBO header (row 2) to extract the serial number (SN)
#   - Matches the SN + timestamp window to the metadata deployments
#   - Renames columns to standard names (e.g., "Abs Pres kPa" -> "airpress_kPa")
#   - Removes duplicate rows and trims data to deployment windows
#
# RETURNS: A list with 2 items:
#   [[1]] = data frame of all combined readings
#   [[2]] = a quick-look plot (ggplot) to visually check the data
# -----------------------------------------------------------------------------

# ?bind_hobo_files
baro_bound <- bind_hobo_files(
  path_to_raw_folder = baro_folder,
  metadata_path      = metadata_file
)

# Extract the data frame (item 1) and the plot (item 2)
baro_data <- baro_bound[[1]]
baro_plot <- baro_bound[[2]]

# Take a look at what we got
cat("Baro data dimensions:", nrow(baro_data), "rows x", ncol(baro_data), "columns\n")
cat("Stations found:", paste(unique(baro_data$site_station_code), collapse = ", "), "\n")
cat("Date range:", as.character(min(baro_data$timestamp)),
    "to", as.character(max(baro_data$timestamp)), "\n")

# Preview the first few rows
head(baro_data)

# Display the overview plot (shows pressure and temperature over time)
print(baro_plot)


# =============================================================================
# STEP 2: Quality-Check All Barometric Data
# =============================================================================
#
# WHAT: Runs automatic quality checks on the barometric data and flags
#       problematic readings.
#
# WHY:  Barometric data feeds into every water level and dissolved oxygen
#       calculation. If the baro data has errors (spikes, flatlines, or
#       out-of-range values), those errors will propagate into the final
#       water levels. Cleaning baro first protects everything downstream.
#
# WHAT IT CHECKS (basic checks):
#   - Pressure too high (> 105 kPa)  or too low (< 85 kPa)
#   - Temperature outside plausible range (-40 to 50 C)
#   - Sudden spikes (> 1.5 kPa jump between consecutive readings)
#   - Flatlines (6 or more identical readings in a row)
#
# HOW CORRECTIONS WORK:
#   The function NEVER modifies the raw data. Instead, it creates new
#   "_adj" (adjusted) columns that start as copies of the raw values.
#   Any values that fail a check are set to NA in the _adj column.
#   Boolean "edit_*" columns record exactly what was changed.
#
#   Raw column:     airpress_kPa      (original, never touched)
#   Adjusted column: airpress_kPa_adj  (corrected copy — this is what we use)
#   Flag column:     edit_press_high   (TRUE where pressure was too high)
#
# RETURNS: A single data frame with all baro stations, sorted by timestamp,
#          with the new _adj and edit_ columns added.
# -----------------------------------------------------------------------------

?barometric_qaqc_all

baro_qc <- barometric_qaqc_all(
  baro_data         = baro_data,       # pass the data frame directly
  metadata_path     = metadata_file,   # metadata for station lookup
  pressure_low_kpa  = 85,              # flag if pressure below this
  pressure_high_kpa = 105,             # flag if pressure above this
  temp_low_limit    = -40,             # flag if temperature below this
  temp_high_limit   = 50,              # flag if temperature above this
  spike_threshold_kpa = 1.5,           # flag if pressure jumps more than this
  flatline_n        = 6                # flag if this many identical readings
)

# Check results: how many values were flagged?
cat("Baro QAQC results:\n")
cat("  Total rows:      ", nrow(baro_qc), "\n")
cat("  Pressure high:   ", sum(baro_qc$edit_press_high, na.rm = TRUE), "flagged\n")
cat("  Pressure low:    ", sum(baro_qc$edit_press_low, na.rm = TRUE), "flagged\n")
cat("  Temp out of range:", sum(baro_qc$edit_temp_range, na.rm = TRUE), "flagged\n")
cat("  Spikes:          ", sum(baro_qc$edit_spike, na.rm = TRUE), "flagged\n")
cat("  Flatlines:       ", sum(baro_qc$edit_flatline, na.rm = TRUE), "flagged\n")

# Compare raw vs adjusted to see the effect of QAQC
cat("  Raw NAs in airpress_kPa:     ",
    sum(is.na(baro_qc$airpress_kPa)), "\n")
cat("  Adjusted NAs in airpress_kPa_adj:",
    sum(is.na(baro_qc$airpress_kPa_adj)), "\n")


# =============================================================================
# STEP 3: Bind Raw Water Level Logger Files (v0.1)
# =============================================================================
#
# WHAT: Same as Step 1, but for the water level loggers instead of baro.
#
# WHY:  The ALBR_ST_30 water level logger (HOBO U20, SN 20533550) was
#       downloaded twice — once in April 2025 and once in October 2025.
#       This step combines both downloads into one continuous record.
#
# NOTE: The water level logger measures the SAME physical quantity as the
#       baro logger (absolute pressure in kPa), but because it's submerged,
#       its readings include the pressure of the water column above it.
#       At this stage, the column is named "waterpress_kPa" to distinguish
#       it from the baro logger's "airpress_kPa".
# -----------------------------------------------------------------------------

level_bound <- bind_hobo_files(
  path_to_raw_folder = level_folder,
  metadata_path      = metadata_file
)

level_data <- level_bound[[1]]
level_plot <- level_bound[[2]]

cat("Water level data dimensions:", nrow(level_data), "rows x",
    ncol(level_data), "columns\n")
cat("Stations found:", paste(unique(level_data$site_station_code),
    collapse = ", "), "\n")

# Preview
head(level_data)

# Display the overview plot
print(level_plot)


# =============================================================================
# STEP 4: Attach Barometric Data to Water Level (v0.2)
# =============================================================================
#
# WHAT: For each water level reading, finds the nearest barometric station
#       and attaches the matching air pressure and temperature values.
#
# WHY:  To convert pressure to water depth, we need to know the atmospheric
#       pressure at the same time. This step pairs each water level
#       timestamp with the closest baro reading.
#
# HOW "AUTO" SELECTION WORKS:
#   1. Calculates the geographic distance (Haversine formula) between each
#      water level station and every barometric station in the metadata
#   2. Selects up to 3 nearest baro stations (within 30 km by default)
#   3. If multiple baro stations overlap in time, calibrates them against
#      the closest one and combines them to fill gaps
#   4. Prefers the QAQC-adjusted values (airpress_kPa_adj) when available
#
# NEW COLUMNS ADDED:
#   airpress_kPa       — atmospheric pressure at each timestamp
#   airtemp_C           — air temperature from the baro station
#   baro_site_stn_code  — which baro station provided this value
#
# RETURNS: A list with 2 items:
#   [[1]] = data frame with baro columns added
#   [[2]] = a plot showing which baro station was used
# -----------------------------------------------------------------------------

?add_nearest_baro

wl_with_baro <- add_nearest_baro(
  input_data         = level_data,      # our water level data
  baro_data          = baro_qc,         # the QAQC'd baro data from Step 2
  metric             = "water level",   # tells the function what we're processing
  baro_site_selection = "auto",         # auto-select nearest baro station
  metadata_path      = metadata_file    # for coordinate lookup
)

wl_data  <- wl_with_baro[[1]]
wl_baro_plot <- wl_with_baro[[2]]

cat("After baro attach:", nrow(wl_data), "rows x", ncol(wl_data), "columns\n")
cat("Baro station(s) used:",
    paste(unique(wl_data$baro_site_stn_code), collapse = ", "), "\n")

# Check for any timestamps where baro data couldn't be matched
baro_na_count <- sum(is.na(wl_data$airpress_kPa))
cat("Timestamps with missing baro data:", baro_na_count,
    paste0("(", round(baro_na_count / nrow(wl_data) * 100, 1), "%)"), "\n")

# Display the baro assignment plot
print(wl_baro_plot)


# >>> STRANGE BEHAVIOUR TO NEW NEW USERS <<<
# Stations are totally dropped if the function can't find a baro match.
unique(level_data$site_station_code)
unique(wl_data$site_station_code)


# =============================================================================
# STEP 5: Convert Pressure (kPa) to Water Level (metres) (v0.3)
# =============================================================================
#
# WHAT: Converts the raw pressure readings into actual water depth/stage
#       in metres, using physics and a field reference measurement.
#
# WHY:  The logger records pressure, but what we care about is how deep
#       the water is. This step does the math to convert.
#
# HOW THE CONVERSION WORKS:
#   1. Calculates water density based on temperature (Kell 1975 equation)
#   2. Converts water pressure to a depth using: depth = pressure / (density * g)
#   3. Converts baro pressure to an equivalent depth the same way
#   4. Subtracts baro depth from water depth to get the water column height
#   5. Adds a constant "k" to anchor the result to a real-world reference
#      (e.g., a staff gauge reading taken during a field visit)
#
# WHAT IS "select_measurement"?
#   The reference file may contain multiple field visits. This parameter
#   selects which visit to use as the anchor point. For example,
#   select_measurement = 1 uses the first available reference measurement
#   for this station. The conversion constant "k" is calibrated so that
#   the logger's computed water level matches the reference at that time.
#
# WHAT IS "reference_type"?
#   - "stage" = reference is a staff gauge reading (water surface elevation
#     above a fixed point, e.g., the stream bottom or a benchmark)
#   - "depth" = reference is a direct depth measurement (distance from
#     water surface to the sensor)
#
# RETURNS: A named list with 3 items:
#   $site_wl  = data frame with waterlevel_m column added
#   $ref_dat  = the reference measurement data used
#   $wl_plot  = interactive plotly chart of water level + reference points
# -----------------------------------------------------------------------------


?convert_waterlevel_kPa_m

converted <- convert_waterlevel_kPa_m(
  input_data       = wl_data,
  select_station   = "ALBR_ST_30",     # process just this station
  reference_data   = reference_file,    # manual field measurements
  reference_type   = "stage",           # our references are staff gauge readings
  select_measurement = 1,              # use the first valid reference as anchor
  return_plots     = TRUE               # generate the interactive plot
)

# Extract results from the named list
site_wl <- converted$site_wl    # the water level data
ref_dat <- converted$ref_dat    # reference measurements used
wl_plot <- converted$wl_plot    # interactive plot

cat("Converted data:", nrow(site_wl), "rows\n")
cat("Water level range:",
    round(min(site_wl$waterlevel_m, na.rm = TRUE), 3), "to",
    round(max(site_wl$waterlevel_m, na.rm = TRUE), 3), "metres\n")
cat("Reference measurements used:", nrow(ref_dat), "\n")

# Show the interactive plot — it overlays the computed water level
# (blue line) with the field reference points (orange dots). If the
# logger is well-calibrated, the dots should sit on or near the line.
print(wl_plot)

# Preview the key columns
head(site_wl[, c("timestamp", "waterpress_kPa", "airpress_kPa",
                  "watertemp_C", "waterlevel_m", "ref_m")])


# =============================================================================
# STEP 6: Automatic Water Level QAQC
# =============================================================================
#
# WHAT: Runs automatic quality checks on the computed water level data
#       and flags suspicious values.
#
# WHY:  Even after a good conversion, the data may contain artifacts from
#       ice formation, sensor drying out, or physical disturbances (e.g.,
#       a flood moving the logger). This step catches common problems.
#
# WHAT IT CHECKS:
#   - Negative water temperatures (< -1 C removed; -1 to 0 C set to 0)
#   - Ice conditions (water temp < 0.3 C flagged but NOT removed)
#   - Step disturbances (water level jump > 0.1 m between readings)
#   - Dry sensor indicators (warm water temp + low pressure pattern)
#
# IMPORTANT DISTINCTION — "flag" vs "edit":
#   - flag_* columns = "we noticed something, please review"
#     The data is NOT changed. It's just marked for your attention.
#   - edit_* columns = "we changed the data"
#     The _adj value was modified (usually set to NA).
#
# RETURNS: A data frame (filtered to ALBR_ST_30 only) with new columns:
#   waterlevel_m_adj   — adjusted water level (the "working" column)
#   watertemp_C_adj    — adjusted water temperature
#   flag_disturbance   — TRUE where a big step change was detected
#   flag_ice           — TRUE where temp suggests ice
#   flag_wl_dry        — TRUE where sensor may be out of water
#   neg_lt_minus1      — TRUE where temp was < -1 C (removed in adj)
#   neg_between        — TRUE where temp was -1 to 0 C (set to 0 in adj)
# -----------------------------------------------------------------------------

?waterlevel_qaqc

wl_qc <- waterlevel_qaqc(
  input_data     = site_wl,
  select_station = "ALBR_ST_30"
)

# Summarize what was flagged
cat("QAQC results for ALBR_ST_30:\n")
cat("  Total rows:        ", nrow(wl_qc), "\n")
cat("  Ice flagged:       ", sum(wl_qc$flag_ice, na.rm = TRUE), "readings\n")
cat("  Disturbance flagged:", sum(wl_qc$flag_disturbance, na.rm = TRUE), "readings\n")
cat("  Dry flagged:       ", sum(wl_qc$flag_wl_dry, na.rm = TRUE), "readings\n")
cat("  Temp < -1 C removed:", sum(wl_qc$neg_lt_minus1, na.rm = TRUE), "readings\n")
cat("  Temp -1 to 0 C fixed:", sum(wl_qc$neg_between, na.rm = TRUE), "readings\n")

# The QAQC log is stored as an attribute on the result.
# It records every automated action taken.
qaqc_log <- attr(wl_qc, "qaqc_log")

if (!is.null(qaqc_log) && nrow(qaqc_log) > 0) {
  cat("\nQAQC log entries:\n")
  print(qaqc_log[, c("field", "action", "code", "ts_start", "ts_end")])
}


# =============================================================================
# STEP 7: Visual Review
# =============================================================================
#
# WHAT: Creates an interactive multi-panel plot for reviewing the data.
#
# WHY:  Before making manual corrections, you need to SEE the data.
#       The plot shows raw vs adjusted values, with flag indicators,
#       so you can decide what needs fixing.
#
# HOW TO USE THE PLOT:
#   - Light traces = raw values (original, unmodified)
#   - Dark traces  = adjusted values (after QAQC corrections)
#   - Colored markers = flag locations (ice, dry, disturbance)
#   - Use the range slider at the bottom to zoom in on specific periods
#   - Hover over points to see exact values and timestamps
#
# This is an iterative process. You'll typically:
#   1. View the plot
#   2. Identify a problem period
#   3. Apply a correction (Steps 8a-8d below)
#   4. Re-plot to verify the correction looks right
#   5. Repeat until satisfied
# -----------------------------------------------------------------------------

?plot_qaqc_timeseries

# Show water level and baro data side by side
plot_qaqc_timeseries(
  wl_data        = wl_qc,        # water level data (with QAQC flags)
  baro_data      = baro_qc,      # barometric data (for context)
  select_station = "ALBR_ST_30"
)



# =============================================================================
# STEP 8: Manual Adjustments (Examples)
# =============================================================================
#
# This section demonstrates the manual correction tools available.
# In a real project, you would apply these ONLY after reviewing the plot
# and identifying specific problem periods.
#
# IMPORTANT: Each function returns a NEW data frame — it does NOT modify
# the original. Always assign the result back to your working variable
# (here, wl_qc) so corrections accumulate.
#
# The examples below use placeholder timestamps. In practice, you would
# pick these from the interactive plot.
# -----------------------------------------------------------------------------


# ---- 8a. Remove ice-affected data (set to NA) ----
#
# WHAT: Sets waterlevel_m_adj and watertemp_C_adj to NA for a time window
#       where ice was affecting the sensor.
#
# WHY:  When ice forms around a pressure sensor, it creates artificial
#       pressure readings that don't represent the actual water level.
#       There's no way to correct for ice — the only option is to remove
#       the affected readings entirely.
#
# The "reason_to_adjust" parameter controls which edit_* flag is set.
# For ice, it creates: edit_remove_ice = TRUE
# For dry, it creates: edit_remove_dry = TRUE

# NOTE: Uncomment and adjust the timestamps to match your data.

?adjust_logger_NA

# wl_qc <- adjust_logger_NA(
#    input_data       = wl_qc,
#    select_station   = "ALBR_ST_30",
#    metric           = "waterlevel",
#    reason_to_adjust = "ice",
#    timestamp_start  = "2025-02-03 00:00:00",   # start of ice period
#    timestamp_end    = "2025-02-10 00:00:00",   # end of ice period
#    manual_note      = "Ice confirmed from field visit and temperature data"
# )


# ---- 8b. Correct a spike / flatline (linear interpolation) ----
#
# WHAT: Replaces waterlevel_m_adj values within a time window with a
#       straight line drawn between the start and end points.
#
# WHY:  Short-duration spikes (e.g., from ice, debris, or sensor errors)
#       can be corrected by interpolating across them. This is appropriate
#       when the spike is clearly an artifact and the true water level
#       was likely smooth through that period.
#
# The "reason_to_adjust" parameter can be "ice" or "disturbance".


?adjust_waterlevel_spike

wl_qc <- adjust_waterlevel_spike(
  input_data       = wl_qc,
  select_station   = "ALBR_ST_30",
  timestamp_start  = "2025-02-03 21:00:00",
  timestamp_end    = "2025-02-10 16:00:00",
  reason_to_adjust = "ice",
  manual_note      = "Interpolated across ice spike"
)


plot_qaqc_timeseries(
  wl_data        = wl_qc,        # water level data (with QAQC flags)
  #baro_data      = baro_qc,      # barometric data (for context)
  select_station = "ALBR_ST_30"
)


# ---- 8c. Correct a step offset (cable break / logger moved) ----
#
# WHAT: Applies a constant vertical shift to all data from a specific
#       point onward, correcting for a sudden physical displacement.
#
# WHY:  If the logger physically moves (e.g., a flood shifts the
#       mounting bracket), all readings after that point are offset by
#       a constant amount. This function calculates the offset from
#       the water level difference at two timestamps and applies it.
#
# timestamp_start = last good reading before the shift
# timestamp_end   = first reading after the shift
# The offset = WL at timestamp_start minus WL at timestamp_end

?adjust_WL_offset

# wl_qc <- adjust_WL_offset(
#   input_data      = wl_qc,
#   select_station  = "ALBR_ST_30",
#   timestamp_start = "2025-02-04 07:00:00",
#   timestamp_end   = "2025-02-09 00:00:00",
#   manual_note     = "Logger displaced by high water event"
# )


# ---- 8d. Multi-point drift correction ----
#
# WHAT: Corrects gradual sensor drift using known offsets at specific
#       timestamps. Applies piecewise-linear interpolation between points.
#
# WHY:  Some sensors slowly drift over time. If you have multiple field
#       measurements showing the growing discrepancy, this function
#       distributes the correction evenly across the record.
#
# drift_points: a tibble with two columns:
#   timestamp = when the drift was measured
#   drift_m   = how far off the logger was at that time (in metres)



drift_pts <- tibble::tibble(
   timestamp = c("2024-10-10 10:00:00", "2025-06-01 00:00:00"),
  drift_m   = c(0.0, 0.12)   # no drift at start, 12cm drift by end
)

?wl_multipoint_correction


# wl_qc <- wl_multipoint_correction(
#   input_data     = wl_qc,
#   select_station = "ALBR_ST_30",
#   drift_points   = drift_pts,
#   manual_note    = "Drift correction based on field survey checks"
# )


# ---- After any manual adjustments, always re-plot to verify ----

# plot_qaqc_timeseries(
#   wl_data        = wl_qc,
#   baro_data      = baro_qc,
#   select_station = "ALBR_ST_30"
# )


# =============================================================================
# STEP 9 (Optional): Set Datum from Level Runs
# =============================================================================
#
# WHAT: Converts water level from relative stage (metres above an arbitrary
#       reference) to absolute elevation (metres above sea level) using
#       surveyed benchmark data.
#
# WHY:  Staff gauge readings give you relative water level ("how high is
#       the water on the gauge?"). Level runs tie that gauge to real-world
#       elevation using surveying equipment. This step adds an "wl_elev_m"
#       column with the absolute elevation.
#
# NOTE: This function is marked as "still in testing" by the developers.
#       It works for initial datum establishment but has limitations for
#       multi-year reapplication.
# -----------------------------------------------------------------------------

# Uncomment if you have level run data:

wl_datum <- wl_set_datum(
  input_data      = wl_qc,
  level_runs_path = level_runs_file,
  select_station  = "ALBR_ST_30"
)

head(wl_datum$wl_elev_m)


# =============================================================================
# STEP 10: Export Clean Data (v1.0)
# =============================================================================
#
# WHAT: Writes the final cleaned data to CSV files, one per station per year.
#
# WHY:  This creates the deliverable output — the clean, quality-controlled
#       water level record ready for analysis, reporting, or sharing.
#
# WHAT GETS EXPORTED:
#   - site_station_code, timestamp (identifiers)
#   - waterlevel_m (raw, unmodified)
#   - waterlevel_m_adj (the final adjusted value — use this for analysis)
#   - watertemp_C, watertemp_C_adj (raw and adjusted temperature)
#   - All flag_* and edit_* columns (the audit trail)
#
# OUTPUT FILE NAMING:
#   <output_folder>/<year>/clean/<station>_WL_<start>_<end>_v1.0.csv
#
# NOTE: write_clean_data() only writes files if you give it an output
# folder. If you just want to inspect the final data frame without
# writing, skip the path_to_output_folder argument.
# -----------------------------------------------------------------------------



# To actually write the files, uncomment and set your output folder:

?write_clean_data

# write_clean_data(
#   input_data            = wl_qc,
#   path_to_output_folder = "processed",   # creates subfolders automatically
#   metric                = "waterlevel"
# )
#
# cat("Clean data exported to: processed/<year>/clean/\n")


# =============================================================================
# RECAP: What Happened at Each Step
# =============================================================================
#
#   Step 1:  bind_hobo_files() on baro CSVs    -> baro_data  (raw baro)
#   Step 2:  barometric_qaqc_all()             -> baro_qc   (cleaned baro)
#   Step 3:  bind_hobo_files() on level CSVs   -> level_data (raw WL)
#   Step 4:  add_nearest_baro()                -> wl_data   (WL + baro attached)
#   Step 5:  convert_waterlevel_kPa_m()        -> site_wl   (kPa -> metres)
#   Step 6:  waterlevel_qaqc()                 -> wl_qc     (auto QAQC'd)
#   Step 7:  plot_qaqc_timeseries()            -> (visual review)
#   Step 8:  Manual adjustments (as needed)    -> wl_qc     (updated)
#   Step 9:  wl_set_datum() (optional)         -> (adds elevation)
#   Step 10: write_clean_data()                -> CSV files on disk
#
# KEY COLUMNS TO UNDERSTAND:
#   waterlevel_m      = the raw computed water level (never modified)
#   waterlevel_m_adj  = the adjusted version (all corrections applied here)
#   flag_*            = "something looks off, please review" (data unchanged)
#   edit_*            = "we changed the data" (data was set to NA or corrected)
#
# ALWAYS use waterlevel_m_adj for analysis. Compare it with waterlevel_m
# to see exactly what the QAQC process changed.
# =============================================================================

