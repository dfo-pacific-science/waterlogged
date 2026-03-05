# Issue Resolutions

## Issue #78 — make_qaqc_log_row incorrectly aggregates non-continuous flags into a single event

**File:** `R/qaqc_log_helpers.R`
**Status:** Fixed

### Problem

`make_qaqc_log_row()` computed `ts_start` and `ts_end` using `min()` and `max()` across all flagged timestamps, producing a single log entry that spanned the entire range — including any gaps where the flag was FALSE. For example, a flag sequence of T, T, F, F, T would generate one log row from the first T to the last T, incorrectly implying the intermediate FALSE period was also flagged.

This affected the four auto-QAQC callers (`waterlevel_qaqc`, `barometric_qaqc`, `dissox_qaqc`, `conductivity_qaqc`) which pass `output_data$timestamp[flag_vector]` — a potentially non-contiguous set of timestamps. Manual-adjustment callers (e.g., `adjust_logger_NA`, `adjust_wl_offset`) were not affected because they pass continuous timestamp ranges from user-specified windows.

### Fix

`make_qaqc_log_row()` now sorts the incoming timestamps, computes the gaps between consecutive values, and splits the vector into contiguous runs wherever a gap exceeds `gap_factor * median(diff(timestamps))` (default `gap_factor = 1.5`). One log row is returned per run, each with its own `ts_start`, `ts_end`, and `duration_hours`.

The return type changes from a guaranteed one-row tibble to a multi-row tibble (one row per contiguous run). All existing callers already combine results via `dplyr::bind_rows()`, so no caller changes were required. For callers that pass a continuous range (all manual-adjustment functions), the function still returns a single row — behaviour is identical to before.

A new optional parameter `gap_factor` (default 1.5) controls the gap detection threshold and can be tuned if needed for irregularly-sampled data.

---

## Issue #77 — plot_qaqc_timeseries labels raw DO column as "red" instead of the metric name

**File:** `R/plot_qaqc_timeseries.R`
**Status:** Fixed

### Problem

Line 87 used `colour = "red"` inside `ggplot2::aes()` for the raw DO trace. In ggplot2, setting `colour` inside `aes()` creates a legend key — it does not set the line colour directly. This produced a legend entry literally labeled "red" and broke the `scale_color_manual()` mapping (which expected `"do_raw"` and `"do_adj"` as keys). The raw DO line received an unmapped default colour instead of the intended Okabe-Ito blue (`#0072B2`) from the `qaqc_plot_cols()` palette.

All other metric panels (WL, conductivity, baro) correctly used their `*_raw` legend key.

### Fix

Changed `colour = "red"` to `colour = "do_raw"` on line 87, matching the key defined in `scale_color_manual(values = c(do_raw = cols$do_raw, do_adj = cols$do_adj))`. The raw DO line now renders in the correct Okabe-Ito blue with proper legend labeling.

---

## Issue #73 — adjust_logger_NA() multiple bugs and missing features

**File:** `R/adjust_logger_NA.R`
**Status:** Fixed

### Problems (six sub-issues)

1. **`keep_temp` ignored for ice:** The `keep_temp` parameter only applied to `reason_to_adjust = "disturbance"`. Users requested it also apply to `"ice"` across all metrics (WL, DO, COND).
2. **No warning when log not written:** When `log_root = NULL` (Shiny mode), the function silently skipped writing the QAQC log with no feedback to the user.
3. **BARO: air temperature not removed; invalid reasons accepted:** The barometric block only NA'd `airpress_kPa_adj` — it never touched `airtemp_C_adj`. It also accepted `"ice"` and `"dry"` which are not meaningful for barometric loggers.
4. **Air temperature metric not supported:** No `metric = "airtemp"` option existed. Users needed to NA air temperature data for disturbance events.
5. **DO: wrong temperature column:** The DO block checked for and modified `watertemp_C_adj`, but `dissox_qaqc()` produces `watertemp_C_do_adj`. Temperature was never actually removed for DO data.
6. **COND: wrong column name crashes function:** Lines 355–423 referenced `conductivity_uScm_adj` throughout, but upstream functions produce `conduct_uScm_adj`. The existence check on line 358 would `stop()` immediately, making COND adjustments impossible.

### Fixes applied

1. **`keep_temp` for ice:** Added `keep_temp` branching to the ice blocks for WL, DO, and COND — same pattern already used for disturbance. When `keep_temp = TRUE`, only the primary metric column is NA'd; when `FALSE` (default), the associated temperature column is also NA'd.
2. **Log warning:** Added `warning("adjust_logger_NA(): log_root is NULL; QA/QC log was not written to disk.")` when rows are modified but `log_root` is NULL.
3. **BARO fixes:** Restricted BARO to `reason_to_adjust = "disturbance"` only (early `stop()` for ice/dry). Added `airtemp_C_adj` handling with `keep_temp` support — when `keep_temp = FALSE` (default), both pressure and air temperature are NA'd.
4. **Air temperature metric:** Added `metric = "airtemp"` (also accepts `"air temperature"`, `"AT"`, `"air_temp"`). Restricted to `reason_to_adjust = "disturbance"` only. NAs `airtemp_C_adj` in the specified window.
5. **DO temperature column:** Changed all references from `watertemp_C_adj` to `watertemp_C_do_adj` in the DO block, matching the column name produced by `dissox_qaqc()`.
6. **COND column name:** Replaced all occurrences of `conductivity_uScm_adj` with `conduct_uScm_adj` throughout the file, matching the column name produced by `conductivity_qaqc()` and all other upstream functions.

Roxygen documentation updated to reflect the new `"airtemp"` metric, reason restrictions for BARO/AT, and expanded `keep_temp` behaviour.

---

## Issue #72 — utils_plotting: update plot colors to NTSERP Palette

**File:** `R/utils_plotting.R`
**Status:** Fixed

### Problem

`qaqc_plot_cols()` used the Okabe-Ito palette, which did not match the project's established NTSERP colour scheme. Additionally, `qaqc_flag_style()` coloured QA/QC flag points using Paul Tol's vibrant palette, introducing visual confusion between flag-point colours and timeseries-line colours.

### Fix

1. **`qaqc_plot_cols()`:** Replaced the Okabe-Ito palette with the NTSERP palette. Each metric now has a dedicated colour identity: conductivity = Black (`#000000`), water level = Japanese Indigo (`#233d4d`), barometric pressure = Mushroom Leather (`#915e3d`), air temperature = Fire (`#FDB840`), dissolved oxygen = Iris Green (`#a1c181`), water temperature = Dull Sea Green (`#619b8a`). Raw and adjusted traces share the same colour (differentiated by alpha in the geom layers). Added new entries for standalone air temperature (`air_temp_raw`/`air_temp_adj`) and water temperature (`water_temp_raw`/`water_temp_adj`) panels.

2. **`qaqc_flag_style()`:** Changed all flag-point colours to black (`#000000`). Unique shapes (circle, triangle, square, plus, x, diamond, star) are retained to distinguish flag types without colour.

---

## Issue #65 — conductivity_qaqc: allow conductivity_temp_compensation() before QAQC

**File:** `R/conductivity_temp_compensation.R`
**Status:** Fixed

### Problem

`conductivity_temp_compensation()` hard-required `conduct_uScm_adj` and `watertemp_C_adj`, which only exist after `conductivity_qaqc()` has run. The developer requested that temp compensation be runnable **before** QAQC so that SPC values are available for quality review. Additionally, the error and warning messages incorrectly referenced the old function name `conduct_temp_comp()` instead of `conductivity_temp_compensation()`.

### Fix

The function now resolves input columns dynamically: it prefers `conduct_uScm_adj` when available and falls back to `conduct_uScm`; likewise prefers `watertemp_C_adj` and falls back to `watertemp_C`. An error is raised only when neither variant of a required column exists. The output column remains `spc_uScm_adj` regardless of which input columns were used. Error and warning messages now reference the correct function name `conductivity_temp_compensation()`. Roxygen documentation updated to describe the flexible column resolution and to remove the statement that the function must be run after QAQC.

---

## Issue #59 — fuzzyjoin dependency removed from CRAN halts package download

**Files:** `R/correct_drift.R`, `man/correct_drift.Rd`, `DESCRIPTION`, `NAMESPACE`, `R/drift_conductivity.R`
**Status:** Fixed

### Problem

The `fuzzyjoin` package was removed from CRAN in January 2026. Because waterlogged listed it in `Imports:`, any attempt to install waterlogged via `pak::pak()` or `install.packages()` would fail unless the user manually installed `fuzzyjoin` from GitHub first. The only code that used `fuzzyjoin` was the legacy `correct_drift()` function, which was already marked `[Superseded]` and fully replaced by `wq_drift_correction()`.

### Fix

Removed `correct_drift()` entirely as recommended by the legacy function audit:

- Deleted `R/correct_drift.R` (285 lines) and `man/correct_drift.Rd`.
- Removed `export(correct_drift)` from `NAMESPACE`.
- Removed `fuzzyjoin` from `Imports:` in `DESCRIPTION`.
- Cleaned up the `@seealso` reference to `correct_drift` in `R/drift_conductivity.R`.

The modern replacement `wq_drift_correction()` does not use `fuzzyjoin` and covers conductivity, DO, and pH drift correction.

---

## Issue #58 — add_nearest_baro() introduces NAs when nearest baro stations have temporal gaps

**File:** `R/add_nearest_baro.R`
**Status:** Fixed

### Problem

In auto mode, `add_nearest_baro()` hard-coded a limit of 3 nearest barometric stations (`rank <= 3`). The composite series was built by `dplyr::coalesce()` across only those 3 stations. When all 3 had NA gaps at the same timestamps, the result contained NAs — even when additional baro stations in the network had valid data for those periods. This caused sites like MILE_WE_40 and PYRA_WE_30 to receive NA barometric pressure at timestamps that could have been filled by a 4th or 5th nearest station.

### Fix

Added a `max_stations` parameter (default `5L`) that controls how many nearest barometric stations are used per site in auto mode. The hard-coded `rank <= 3` filter now uses `rank <= max_stations`. Raising the default from 3 to 5 reduces the likelihood of NA gaps when the closest stations have incomplete temporal coverage. Users can increase or decrease this value as needed for their network density. Roxygen documentation updated to reflect the new parameter and the configurable station limit.

---

## Issue #55 — convert_waterlevel_kPa_m: missing site_comments and duplicate timestamps from rounding

**Files:** `R/convert_wl_kpa_m.R`, `R/convert_wl_kpa_m_helpers.R`
**Status:** Fixed

### Problem (two sub-issues)

1. **`ref_dat` missing `site_comments`:** The returned `ref_dat` data frame (element `[[2]]` of the result list) only contained `timestamp` and `ref_m`. Users needed the `site_comments` column to evaluate whether the correct reference measurement was chosen (e.g., "redeployed at new location", "do logger moved").

2. **Duplicate rows from rounded timestamps:** `convert_waterlevel_single()` rounds reference timestamps to the nearest hour to match logger data. When two field measurements occur close together (e.g., before and after repositioning a logger), they can round to the same hour. The subsequent `left_join()` onto the logger time series then produces duplicate rows at that timestamp. Downstream functions like `adjust_WL_offset()` then error with "Could not find unique waterlevel_m".

### Fixes applied

1. **`site_comments` in `ref_dat`:** Added `site_comments` to the `dplyr::select()` call in `convert_waterlevel_single()`. The column is now included in the returned `ref_dat` for both single-station and batch modes. Roxygen `@return` documentation updated in both `convert_waterlevel_kPa_m()` and `convert_waterlevel_single()`.

2. **Deduplication after rounding:** After rounding reference timestamps to the nearest hour, the function now detects duplicate timestamps. When found, a warning is emitted indicating how many timestamps were collapsed, and only the last measurement at each rounded timestamp is kept (the post-repositioning value is the relevant reference going forward). This prevents the left join from creating duplicate rows in the output.

---

## Issue #54 — convert_wl_kpa_m_helpers: remove redundant range-check warnings

**File:** `R/convert_wl_kpa_m_helpers.R`
**Status:** Fixed

### Problem

`QAQC_wl_kpa_inputs()` contained three range-check warnings for `watertemp_C` ([-5, 40] °C), `waterpress_kPa` ([80, 300] kPa), and `airpress_kPa` ([80, 120] kPa), implemented via an internal `warn_by_station_year()` helper. These checks were redundant because the same values are QA/QC'd by `waterlevel_qaqc()` and `watertemp_qaqc()` in the next pipeline step. Additionally, the fixed pressure ranges were inappropriate for all deployment contexts — deep-water, sea-level, and alpine sites can produce valid readings outside these bounds. Barometric data should already be cleaned before reaching this step.

### Fix

Removed the `warn_by_station_year()` helper function and all three range-check calls from `QAQC_wl_kpa_inputs()`. The function retains its useful validation: required-columns check, logger-type sanity check, and timestamp parsing. Roxygen `@return` documentation updated to remove the reference to range warnings.

---

## Issue #53 — get_logger_data: three missing helper functions and missing conductivity metric

**Files:** `R/get_logger_data.R`, `R/get_logger_data_helpers.R`
**Status:** Fixed

### Problem

`get_logger_data()` called three helper functions that were never defined in any package source file, making the function completely non-functional:

1. **`normalise_temporal_scale()`** — called on every invocation (even with `temporal_scale = "none"`). Undefined.
2. **`read_logger_file()`** — called when loading CSV files from disk via `purrr::map_df()`. Undefined.
3. **`build_plots()`** — called when `return_plots = TRUE` and data is summarised. Undefined.

All three were declared in `globals.R` to suppress R CMD check notes, but the implementations were never written. Additionally, the `metric` parameter and `summarise_clean_data()` were missing support for conductivity, even though `write_clean_data()` produces v1.0 conductivity files.

### Fixes applied

1. **`normalise_temporal_scale()`:** Implemented in `get_logger_data_helpers.R`. Validates and normalises the `temporal_scale` argument to one of: `"none"`, `"hourly"`, `"daily"`, `"weekly"`, `"monthly"`, `"yearly"`. Accepts common aliases (e.g., `"hour"`, `"day"`, `"annual"`) case-insensitively.

2. **`read_logger_file()`:** Implemented in `get_logger_data_helpers.R`. Reads a single CSV, detects and renames timestamp columns, parses timestamps to POSIXct (UTC), and coerces known factor/code columns to character.

3. **`build_plots()`:** Implemented in `get_logger_data_helpers.R`. Generates faceted ggplot ribbon plots (mean line with min/max shading) for each metric's summary columns. Uses NTSERP palette colours. Returns up to three plots depending on the metric.

4. **Conductivity support:** Added `"conductivity"` / `"cond"` / `"spc"` to `normalise_metric()`. Added conductivity branch to `summarise_clean_data()` (summarises `conduct_uScm_adj` and `watertemp_C_adj`). Added conductivity panel to `build_plots()`. Updated `@param metric` roxygen in `get_logger_data()` to include conductivity.
