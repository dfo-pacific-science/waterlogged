# Legacy Function Audit: `main` → `develop`

**Date:** 2026-03-03
**Package:** waterlogged 0.0.0.9000
**Branches compared:** `main` (old) vs `develop` (current)

---

## Executive Summary

The `develop` branch represents a major modernization of the waterlogged package. The
key architectural changes are:

1. **Column naming:** Old branch used logger-model suffixes (`_U20`, `_U26`, `_U24`,
   `_TidbiT`). New branch uses metric-based names (`airpress_kPa`, `waterpress_kPa`,
   `do_mgl`, `conduct_uScm`).
2. **QAQC tracking:** Old branch used per-metric code/adj/note triplets
   (`wl_qaqc_code`, `wl_qaqc_adj`, `wl_qaqc_note`). New branch uses structured CSV
   log files and boolean `flag_*` / `edit_*` columns stored as attributes.
3. **API style:** Old branch required `logger_type` parameters (e.g.,
   `"waterlevel_U20"`). New branch uses `metric` parameters (e.g., `"water level"`,
   `"DO"`, `"conductivity"`).
4. **File-based workflows:** Old branch wrote versioned CSVs as a side effect of most
   functions. New branch makes file output optional (`path_to_output_folder = NULL`
   default), enabling pure in-memory workflows.
5. **New helper architecture:** New branch splits large functions into `*_helpers.R`
   companion files and adds shared utility modules (`utils.R`, `utils_plotting.R`,
   `qaqc_log_helpers.R`, `globals.R`).

Despite this modernization, **5 legacy functions from `main` persist on `develop`
unchanged or minimally modified**, alongside their modern replacements. Additionally,
there are naming inconsistencies, missing helpers, dead code, and copy-paste errors.

This report catalogues every issue found.

---

## Table of Contents

1. [Vignette & Workflow Function Usage Map](#1-vignette--workflow-function-usage-map)
2. [Legacy Functions That Should Be Removed or Deprecated](#2-legacy-functions-that-should-be-removed-or-deprecated)
3. [Duplicate / Overlapping Function Pairs](#3-duplicate--overlapping-function-pairs)
4. [Detailed Code Comparisons](#4-detailed-code-comparisons)
5. [Column Naming Inconsistencies](#5-column-naming-inconsistencies)
6. [Missing Helper Functions](#6-missing-helper-functions)
7. [Dead / Unreferenced Code](#7-dead--unreferenced-code)
8. [Copy-Paste Errors](#8-copy-paste-errors)
9. [Renamed Functions (Old Name Gone)](#9-renamed-functions-old-name-gone)
10. [New Functions (Not on main)](#10-new-functions-not-on-main)
11. [NAMESPACE and Export Cleanup](#11-namespace-and-export-cleanup)
12. [Recommendations Summary](#12-recommendations-summary)
13. [Appendix: Full File Inventory Comparison](#appendix-full-file-inventory-comparison)

---

## 1. Vignette & Workflow Function Usage Map

### Functions used in new vignettes (develop) vs old vignette (main)

This table maps every waterlogged function called in the old vignette
(`main:vignettes/waterlogged-vignette.Rmd`) and the three new vignettes
(`vignettes/water-level.Rmd`, `dissolved-oxygen.Rmd`, `conductivity.Rmd`). It shows
which API surface each vignette depends on and highlights where old functions are no
longer used.

| Function | Old vignette (main) | WL vignette | DO vignette | COND vignette | Status |
|----------|:-------------------:|:-----------:|:-----------:|:-------------:|--------|
| `bind_hobo_files()` | with `logger_type=` | modern API | modern API | modern API | **Rewritten** |
| `add_nearest_baro()` | with `input_logger_type=` | modern API | modern API | — | **Rewritten** |
| `convert_waterlevel_kPa_m()` | with `var_*` params | modern API | — | — | **Rewritten** |
| `convert_do_mgl_percsat()` | with `var_*` params | — | modern API | — | **Rewritten** |
| `barometric_qaqc_all()` | — | modern API | modern API | — | **New** |
| `barometric_qaqc()` | climate station API | (via `_all`) | (via `_all`) | — | **Rewritten** |
| `waterlevel_qaqc()` | with `var_*` params | modern API | — | — | **Rewritten** |
| `dissox_qaqc()` | with `var_*` params | — | modern API | — | **Rewritten** |
| `conductivity_qaqc()` | with `var_*` params | — | — | modern API | **Rewritten** |
| `plot_qaqc_timeseries()` | — | modern API | modern API | modern API | **New** |
| `wq_drift_correction()` | — | — | modern API | modern API | **New** |
| `conductivity_temp_compensation()` | — | — | — | modern API | **Rewritten** |
| `wl_set_datum()` | — | modern API | — | — | **New** |
| `adjust_waterlevel_spike()` | old API | modern API | — | — | **Rewritten** |
| `adjust_WL_offset()` | — | modern API | — | — | **New** |
| `wl_multipoint_correction()` | — | modern API | — | — | **New** |
| `adjust_logger_NA()` | with `logger_to_adjust=` | modern API | modern API | modern API | **Rewritten** |
| `write_clean_data()` | with `logger_type=` | modern API | modern API | — | **Rewritten** |
| `adjust_waterlevel_cablebreak()` | old API | — | — | — | **Legacy, unused in new** |
| `adjust_waterlevel_zero()` | — | — | — | — | **Legacy, unused in either** |
| `correct_drift()` | old API | — | — | — | **Legacy, unused in new** |
| `conduct_temp_comp()` | old API | — | — | — | **Removed (renamed)** |
| `get_logger_data()` | with `logger_type=` | — | — | — | **Rewritten, unused in new** |
| `watertemp_qaqc()` | — | — | — | — | **Legacy, unused in either** |
| `airtemp_qaqc()` | — | — | — | — | **Legacy, unused in either** |

### Old vignette — old-style parameter values used

Every function call in the old vignette on `main` used the legacy API:

```r
# bind_hobo_files with logger_type (old API)
bind_hobo_files(..., logger_type = "baro_U20", metadata = metadat)
bind_hobo_files(..., logger_type = "dissolvedoxygen_U26", metadata = metadat)

# add_nearest_baro with input_logger_type (old API)
add_nearest_baro(..., input_logger_type = "waterlevel_U20", metadata = baro_metadat)
add_nearest_baro(..., input_logger_type = "dissolvedoxygen_U26", metadata = baro_metadat)

# dissox_qaqc with _U20/_U26 column names (old API)
dissox_qaqc(..., var_waterlevel_m = "waterlevel_m_U20_adj",
                 var_watertemp_C_2 = "watertemp_C_U20_adj")

# adjust_logger_NA with logger_to_adjust (old API)
adjust_logger_NA(..., logger_to_adjust = "dissolvedoxygen_U26")
adjust_logger_NA(..., logger_to_adjust = "conductivity_U24",
                      var_watertemp_C = "watertemp_C_U24")

# conductivity_qaqc with _U24 column names (old API)
conductivity_qaqc(..., var_conduct_uScm = "conduct_uScm_U24",
                       var_watertemp_C = "watertemp_C_U24")

# correct_drift with _U24 column names (old API)
correct_drift(..., var_ref_conduct_uScm = "ysi_conduct_uScm")

# conduct_temp_comp with _U24 drift-corrected columns (old API)
conduct_temp_comp(..., var_conduct_uScm = "conduct_uScm_U24_adj_dc",
                       var_watertemp_C = "watertemp_C_U24_adj_dc")

# write_clean_data with logger_type (old API)
write_clean_data(..., logger_type = "dissolvedoxygen_U26")
write_clean_data(..., logger_type = "conductivity_U24")

# get_logger_data with logger_type (old API)
get_logger_data(..., logger_type = "waterlevel_U20", data_processing = "v0.3")
get_logger_data(..., logger_type = "dissolvedoxygen_U26", temporal_scale = "monthly")
```

### New vignettes — modern parameter values used

The new vignettes use **none** of the old-style parameters:

```r
# bind_hobo_files — no logger_type, uses metadata_path
bind_hobo_files(path_to_raw_folder = ..., metadata_path = ...)

# add_nearest_baro — metric string, not logger_type
add_nearest_baro(..., metric = "water level", baro_site_selection = "auto",
                      metadata_path = ...)
add_nearest_baro(..., metric = "DO", baro_site_selection = "DARL_ST_30",
                      metadata_path = ...)

# QAQC functions — no var_* params, just station
waterlevel_qaqc(input_data = ..., select_station = "ALBR_ST_30")
dissox_qaqc(input_data = ..., select_station = "COOK_WE_40")
conductivity_qaqc(input_data = ..., select_station = "TUMT_WE_40")

# adjust_logger_NA — metric string, not logger_to_adjust
adjust_logger_NA(..., metric = "waterlevel", reason_to_adjust = "ice",
                      manual_note = "...")
adjust_logger_NA(..., metric = "dissolvedoxygen", reason_to_adjust = "dry",
                      manual_note = "...")
adjust_logger_NA(..., metric = "conductivity", reason_to_adjust = "dry",
                      manual_note = "...")

# Drift correction — modern multi-metric function
wq_drift_correction(..., metric = "do", method = "ratio")
wq_drift_correction(..., metric = "cond", method = "ratio")

# write_clean_data — metric string, not logger_type
write_clean_data(..., metric = "waterlevel")
write_clean_data(..., metric = "dissolvedoxygen")
```

### Key takeaway

The old and new vignettes share **zero** old-style API calls. Every function used in
the new vignettes is either a modernized rewrite of an old function or a completely new
function. The three legacy functions called in the old vignette but absent from the new
vignettes (`adjust_waterlevel_cablebreak`, `correct_drift`, `conduct_temp_comp`) have
all been replaced by modern equivalents.

---

## 2. Legacy Functions That Should Be Removed or Deprecated

These functions exist on `develop` with **old-style** `_U20`/`_U24`/`_TidbiT` column
defaults, no QAQC log integration, and for-loop processing patterns.

### 2.1 `adjust_waterlevel_cablebreak()`

| | |
|---|---|
| **File** | `R/adjust_waterlevel_cablebreak.R` (60 lines) |
| **Replaced by** | `adjust_WL_offset()` in `R/adjust_wl_offset.R` (191 lines) |
| **Used in old vignette** | Yes — called on line 612 |
| **Used in new vignettes** | No — `adjust_WL_offset()` used instead |
| **Legacy signs** | Default `var_waterlevel_m = "waterlevel_m_U20"`, uses `wl_qaqc_code`/`wl_qaqc_adj`/`wl_qaqc_note` columns, for-loop, no QAQC log |
| **Recommendation** | **Remove** from package and NAMESPACE |

### 2.2 `adjust_waterlevel_zero()`

| | |
|---|---|
| **File** | `R/adjust_waterlevel_zero.R` (51 lines) |
| **Replaced by** | `adjust_WL_zero()` in `R/adjust_wl_zero.R` (207 lines) |
| **Used in old vignette** | No |
| **Used in new vignettes** | No — `adjust_WL_zero()` is the modern replacement |
| **Legacy signs** | Default `var_waterlevel_m = "waterlevel_m_U20"`, hardcoded `+0.1` logger height offset, uses `wl_qaqc_code`/`wl_qaqc_adj` columns, for-loop, no QAQC log |
| **Recommendation** | **Remove** from package and NAMESPACE |

### 2.3 `correct_drift()`

| | |
|---|---|
| **File** | `R/correct_drift.R` (285 lines) |
| **Replaced by** | `wq_drift_correction()` in `R/drift_conductivity.R` (520 lines) |
| **Used in old vignette** | Yes — called on line 982 |
| **Used in new vignettes** | No — `wq_drift_correction()` used instead |
| **Legacy signs** | Already marked `[Superseded]` in roxygen. Defaults `var_conduct_uScm = "conduct_uScm_U24_adj"`, creates `_dc` columns, uses `co_qaqc_*` triplet, no QAQC log |
| **Recommendation** | **Remove** from package and NAMESPACE |

### 2.4 `airtemp_qaqc()`

| | |
|---|---|
| **File** | `R/airtemp_qaqc.R` |
| **Replaced by** | No direct modern replacement exists |
| **Used in old vignette** | No |
| **Used in new vignettes** | No |
| **Legacy signs** | Default `var_airtemp_C = "airtemp_C_TidbiT"`, uses `at_qaqc_code`/`at_qaqc_adj`/`at_qaqc_note` columns, for-loop, no QAQC log |
| **Recommendation** | **Modernize** if air temperature QAQC is needed, otherwise **remove** |

### 2.5 `watertemp_qaqc()`

| | |
|---|---|
| **File** | `R/watertemp_qaqc.R` |
| **Replaced by** | No direct modern replacement (water temp QAQC handled inline by `waterlevel_qaqc()` and `dissox_qaqc()`) |
| **Used in old vignette** | No |
| **Used in new vignettes** | No |
| **Legacy signs** | Defaults `var_airtemp_C = "airtemp_C_U20"`, `var_watertemp_C = "watertemp_C_U20"`, hardcoded `airtemp_C_U20` in `plot_only = TRUE` ggplot path, uses generic `qaqc_code`/`qaqc_note` columns, for-loop, no QAQC log |
| **Recommendation** | **Modernize** if standalone water temp QAQC is needed, otherwise **remove** |

---

## 3. Duplicate / Overlapping Function Pairs

| Legacy (old API) | Modern (new API) | Overlap |
|---|---|---|
| `adjust_waterlevel_cablebreak()` | `adjust_WL_offset()` | Both shift WL after a cable break / displacement event |
| `adjust_waterlevel_zero()` | `adjust_WL_zero()` | Both shift WL to correct zero reference |
| `correct_drift()` | `wq_drift_correction()` | Both do drift correction |

Additionally, these functions **share a name** but were completely rewritten between
branches (not duplicates — the develop version replaced the main version):

| Function | main API | develop API |
|---|---|---|
| `barometric_qaqc()` | Climate station normals, `var_*` params | Configurable thresholds, fixed column names |
| `bind_hobo_files()` | `logger_type=` param, `_U20` columns | Auto-detected metric, modern columns |
| `adjust_logger_NA()` | `logger_to_adjust=` param, `var_*` params | `metric=` param, fixed column names |
| `waterlevel_qaqc()` | `var_*` params, `plot_only` | No params, no plot |
| `dissox_qaqc()` | `var_*` params, `plot_only` | No params, no plot |
| `conductivity_qaqc()` | `var_*` params, `plot_only` | Configurable thresholds, no plot |
| `write_clean_data()` | `logger_type=` param, `var_*` params | `metric=` param, config-driven |
| `convert_waterlevel_kPa_m()` | `var_*` params | Fixed column names, helpers split out |
| `convert_do_mgl_percsat()` | `var_*` params | Fixed column names |
| `adjust_waterlevel_spike()` | `var_*` params, row-index interpolation | Fixed columns, time-based interpolation |

---

## 4. Detailed Code Comparisons

### 4.1 Cable Break: `adjust_waterlevel_cablebreak()` vs `adjust_WL_offset()`

| Aspect | OLD (60 lines) | NEW (191 lines) |
|--------|-----------------|------------------|
| **Column params** | `var_waterlevel_m = "waterlevel_m_U20"` | Hardcodes `waterlevel_m` / `waterlevel_m_adj` |
| **Timestamp parsing** | `as.POSIXct()` with format string | `lubridate::ymd_hms()` |
| **Iteration** | `for` loop, row-by-row | Vectorized `dplyr::mutate()` + `case_when()` |
| **Raw data safety** | Falls through to modify raw `waterlevel_m` when `_adj` is NA (line 49) | Only ever touches `waterlevel_m_adj` |
| **QAQC tracking** | `wl_qaqc_adj/code/note` triplet | `edit_offset` boolean + structured log |
| **Validation** | None | Checks required columns, station exists, timestamps in range |
| **Error handling** | Silent failures | `stop()` with informative messages |

**Bug fixed in NEW:** Old version could write to raw `waterlevel_m` column when
`waterlevel_m_adj` was NA, violating the "raw columns never modified" principle.

### 4.2 Zero Shift: `adjust_waterlevel_zero()` vs `adjust_WL_zero()`

| Aspect | OLD (51 lines) | NEW (207 lines) |
|--------|-----------------|------------------|
| **Column params** | `var_waterlevel_m = "waterlevel_m_U20"` | Hardcodes column names |
| **Shift direction** | `shift_up_or_down = "up"/"down"` string | Computed from sign of `change_value` |
| **Dual mode** | Window-only | Explicit `change_value` OR auto-computed from window |
| **Logger height** | Hardcoded `+0.1` added to ALL values | Removed entirely |
| **Offset computation** | `abs(mean(waterlevel_m[window]))`, always positive | `-mean(waterlevel_m_adj[window])`, preserves sign |
| **Iteration** | `for` loop | Vectorized `dplyr::mutate()` |
| **QAQC tracking** | `wl_qaqc_code = "SHIFT_CORRECTION"` per row | `edit_zero` boolean + structured log |

**Bug fixed in NEW:** Old version's `+0.1` logger height assumption was hardcoded and
undocumented — a silent source of error for non-standard deployments. Old version used
`abs(mean(...))` which lost the sign.

### 4.3 Drift Correction: `correct_drift()` vs `wq_drift_correction()`

| Aspect | OLD (285 lines) | NEW (520 lines) |
|--------|------------------|------------------|
| **Metrics supported** | Conductivity only | Conductivity, DO, and pH |
| **Column params** | `var_conduct_uScm = "conduct_uScm_U24_adj"`, etc. | Determined by `metric` param |
| **Reference data** | Passed as data frame with user-named columns | Standardized CSV schema with `event_type` |
| **Event types** | None — all references treated equally | `"calibration"`, `"check"`, `"ignore"` |
| **Correction methods** | Multiplicative only | `"ratio"` (multiplicative) or `"offset"` (additive) |
| **Algorithm** | Backward-looking interpolation between reference points | Forward-linear ramps that restart at each visit boundary |
| **Fuzzy join** | 3-tier tolerance (29/60/120 min) | Single configurable `max_ref_gap_h` |
| **Output columns** | Creates new `*_dc` suffix columns | Writes directly to existing `*_adj` columns |
| **Return value** | `list(data, ref_subset, plotly, cal_devs)` | Full data frame with `attr("qaqc_log")` |
| **QAQC tracking** | `co_qaqc_*` triplet | `edit_cond_drift` / `edit_do_drift` boolean + structured log |

**Major change:** The algorithms produce different numerical results. The old algorithm
does backward interpolation between reference pairs. The new algorithm does forward-
linear ramps from each segment start, restarting at calibration boundaries.

### 4.4 Barometric QAQC: `barometric_qaqc()` (main vs develop)

| Aspect | OLD (129 lines) | NEW (170 lines) |
|--------|------------------|------------------|
| **Temperature validation** | Monthly climate station extremes from 5 BC stations | Fixed configurable bounds (default −40 to 50 °C) |
| **Pressure validation** | None | Low/high range checks (default 85–105 kPa) |
| **Spike detection** | None | Consecutive pressure jump > threshold |
| **Flatline detection** | None | RLE detection of N identical readings |
| **`library()` calls** | Inside function body (lines 22-24) | Removed — uses `dplyr::` etc. |
| **Error handling** | `print()` for errors (execution continues) | `stop()` (execution halts) |
| **Plot** | Built-in ggplotly dual-axis chart | No plotting (moved to `plot_qaqc_timeseries`) |
| **Return value** | `list(data, plot)` | Single data frame with `attr("qaqc_log")` |
| **QAQC tracking** | `baro_qaqc_code/adj/note` triplet | 5 boolean edit flags: `edit_press_high/low`, `edit_temp_range`, `edit_spike`, `edit_flatline` |

**Major additions in NEW:** Pressure range checks, spike detection, and flatline
detection — none existed in old version. The old version only flagged temperature.

### 4.5 Spike Correction: `adjust_waterlevel_spike()` (main vs develop)

| Aspect | OLD (70 lines) | NEW (228 lines) |
|--------|-----------------|------------------|
| **Column params** | `var_waterlevel_m = "waterlevel_m_U20"`, `var_watertemp_C = "watertemp_C_U20"` | Hardcodes `waterlevel_m` / `waterlevel_m_adj` |
| **Interpolation** | Row-index-based: `start + corr * (i - start_idx)` | Time-based: `start + total_change * (time_elapsed / total_time)` |
| **Raw data safety** | Falls through to modify raw `waterlevel_m` when `_adj` is NA (line 45) | Only writes to `waterlevel_m_adj` |
| **Error messages** | `print("Uh oh! ... Silly Goose!")` (line 50) | `stop()` with informative message |
| **Iteration** | `for` loop | Vectorized `dplyr::mutate()` |
| **QAQC tracking** | `wl_qaqc_adj = "AVERAGE"` | `edit_spike_flat` boolean + structured log |

**Bug fixed in NEW:** Old version interpolated by row index (line 42) rather than by
time. If timestamps had gaps or irregular spacing, the old version produced incorrect
interpolations.

### 4.6 Temp Compensation: `conduct_temp_comp()` vs `conductivity_temp_compensation()`

| Aspect | OLD (127 lines) | NEW (111 lines) |
|--------|------------------|------------------|
| **Function name** | `conduct_temp_comp()` | `conductivity_temp_compensation()` |
| **Column params** | `var_conduct_uScm = "conduct_uScm_U24_adj"`, `var_watertemp_C = "watertemp_C_U24_adj"` | Hardcodes `conduct_uScm_adj`, `watertemp_C_adj` |
| **Output column** | `spc_uScm_U24` | `spc_uScm_adj` |
| **Interactive prompt** | `readline()` asking about drift-corrected columns — crashes in non-interactive mode | Removed |
| **Polynomial fit** | `I(temp^2)` notation | `poly(temp, 5, raw = TRUE)` |
| **stringr dependency** | Uses `str_remove_all()` (undeclared dep) | No stringr |
| **Plotting** | Returns plotly subplot | No plotting |
| **NA handling** | No explicit handling | `ifelse()` returns `NA_real_` when inputs NA; warns if >5% temp values NA |

**Bug fixed in NEW:** `readline()` prompt in old version would crash in batch scripts
and Shiny apps. `stringr` was used without being declared as a dependency.

### 4.7 bind_hobo_files: main (287 lines) vs develop (489 + 367 = 856 lines)

| Aspect | OLD | NEW |
|--------|-----|-----|
| **Metric detection** | `logger_type` param: `"baro_U20"`, `"waterlevel_U20"`, etc. | Auto-detected from metadata `metric` + `model` columns |
| **Metadata input** | Pre-parsed data frame | `metadata_path` (file) or `metadata` (df) + comprehensive validation via `QAQC_metadata()` |
| **Column naming** | `airpress_kPa_U20`, `waterpress_kPa_U20`, `do_mgl_U26`, `conduct_uScm_U24` | `airpress_kPa`, `waterpress_kPa`, `do_mgl`, `conduct_uScm` |
| **Conductivity** | Single `conduct_uScm_U24` column | Merges Low/Full Range with `conduct_source` tracking |
| **Temperature units** | Assumes Celsius | Auto-converts Fahrenheit to Celsius |
| **File output** | Required `path_to_output_folder` | Optional (NULL = in-memory only) |
| **Return value** | `list(data, file_summary, plots)` | `list(data, plot)` |
| **Helpers** | 2 inline functions | Separate `bind_hobo_files_helpers.R` with `QAQC_metadata()` (exported) and `extract_alldata_from_file()` (internal) |

### 4.8 adjust_logger_NA: main (347 lines) vs develop (476 lines)

| Aspect | OLD | NEW |
|--------|-----|-----|
| **Metric param** | `logger_to_adjust`: `"waterlevel_U20"`, `"dissolvedoxygen_U26"`, `"conductivity_U24"`, `"watertemp_TidbiT"` | `metric`: `"waterlevel"`, `"dissolvedoxygen"`, `"watertemp"`, `"barometric"`, `"conductivity"` |
| **Column params** | 7 `var_*` params with `_U20`/`_U26`/`_U24` defaults | All removed; fixed column names |
| **`keep_temp` param** | Referenced in code but NOT declared as parameter (bug — crashes at runtime) | Properly declared as parameter, default `FALSE` |
| **Barometric support** | Not supported | Added — can NA barometric data for ice/dry/disturbance |
| **`flag_only` param** | Present for U24 disturbance | Removed |
| **Iteration** | `for` loops per logger type | Vectorized `dplyr::mutate()` + `if_else()` |
| **QAQC tracking** | `*_qaqc_code/adj/note` triplets | `edit_remove_ice/dry/disturbance` booleans + structured log |

**Bug in NEW:** The COND block uses `conductivity_uScm_adj` (line 358) but the column
is actually `conduct_uScm_adj`. Will crash at runtime.

**Bug fixed from OLD:** `keep_temp` was referenced without being a parameter.

### 4.9 write_clean_data: main (195 lines) vs develop (260 lines)

| Aspect | OLD | NEW |
|--------|-----|-----|
| **Metric param** | `logger_type`: `"waterlevel_U20"`, `"dissolvedoxygen_U26"`, etc. | `metric`: `"waterlevel"`, `"dissolvedoxygen"`, etc. |
| **Column params** | 8 `var_*` params | All removed; config-driven |
| **Conductivity** | Supported via `"conductivity_U24"` | **Missing** — not in `metric_config` (regression) |
| **Flag stripping** | Strips `FLAG_*` codes from `wl_qaqc_code` via `gsub()` | Does not strip flags (uses boolean columns) |
| **File output** | Required | Optional (NULL = return data only) |
| **Column detection** | Hardcoded per logger type | Auto-includes all `flag_*` and `edit_*` columns |
| **Return value** | No return (side-effect only) | `invisible(input_data)` for pipe chaining |

**Regression in NEW:** Conductivity export support is missing from the `metric_config`.

### 4.10 waterlevel_qaqc: main (140 lines) vs develop (230 lines)

| Aspect | OLD | NEW |
|--------|-----|-----|
| **Column params** | 5 `var_*` params with `_U20` defaults | All removed; fixed column names |
| **`plot_only` param** | Present | Removed (plotting moved to `plot_qaqc_timeseries()`) |
| **Dry temp threshold** | `watertemp_C > 25` | `watertemp_C > 19` |
| **Ice threshold** | `< 0.2` | `< 0.3` |
| **Dry detection** | Pressure-based only (`waterpress <= airpress`) | Combined: pressure-based + air-water temp proximity + air warmer than water |
| **Iteration** | Two `for` loops | Vectorized `dplyr::mutate()` |
| **QAQC tracking** | `wl_qaqc_code/adj/note` triplet (one code per row) | Multiple coexisting boolean flags: `flag_disturbance`, `flag_ice`, `flag_wl_dry`, `neg_lt_minus1`, `neg_between` |
| **Return value** | `list(data, plotly)` | Single data frame with `attr("qaqc_log")` |

### 4.11 dissox_qaqc: main (244 lines) vs develop (293 lines)

| Aspect | OLD | NEW |
|--------|-----|-----|
| **Column params** | 6 `var_*` params with `_U26`/`_U20` defaults | All removed; fixed column names |
| **`plot_only` param** | Present | Removed |
| **Error code check** | `do_mgl == "-888.88"` (string comparison) | `do_mgl == -888.88` (numeric) + temp check |
| **Dry detection** | Uses raw `do_percsat` | Uses `do_percsat_adj` (already-cleaned values) |
| **Output temp column** | `watertemp_C_1_adj` | `watertemp_C_do_adj` |
| **Iteration** | `for` loops (dry detection still loop-based in NEW) | Mostly vectorized, dry detection still `for` loop |
| **QAQC tracking** | `do_qaqc_code/adj/note` triplet | Multiple boolean flags: `do_error`, `do_neg_between`, `do_neg_lt_minus1`, `do_high`, `temp_flag_ice`, `flag_do_dry` |
| **Return value** | `list(data, plotly)` | Single data frame with `attr("qaqc_log")` |

### 4.12 conductivity_qaqc: main (273 lines) vs develop (281 lines)

| Aspect | OLD | NEW |
|--------|-----|-----|
| **Column params** | 5 `var_*` params with `_U24` defaults | All removed |
| **`plot_only` param** | Present | Removed |
| **Thresholds** | All hardcoded | All configurable via parameters |
| **Dry detection** | 6-point consecutive counter requiring low EC AND close air-water temp | Simpler OR: low EC alone triggers flag |
| **`WT_BELOW_RANGE`** | Present (`watertemp < 5`) | Removed |
| **Timestamp parsing** | Hardcoded `as.POSIXct(..., format = "%m/%d/%Y %H:%M")` | Checks `inherits(df$timestamp, "POSIXct")` first |
| **Iteration** | Two `for` loops + counter loop | Vectorized |
| **QAQC tracking** | `co_qaqc_code/adj/note` triplet | Multiple boolean flags: `flag_co_spike`, `flag_co_temp_ice`, `flag_co_dry`, etc. |
| **Return value** | `list(data, plotly)` | Single data frame with `attr("qaqc_log")` |

### Cross-cutting patterns across all 12 pairs

1. **Column naming:** All 12 pairs moved from `_U20`/`_U26`/`_U24`/`_TidbiT` suffixed
   columns to metric-based names without logger model suffixes.
2. **QAQC tracking:** All 12 pairs moved from `*_qaqc_code/adj/note` string triplets
   to boolean `flag_*`/`edit_*` columns + structured `make_qaqc_log_row()` logging with
   `attr(result, "qaqc_log")`.
3. **Vectorization:** All `for` loops replaced with `dplyr::mutate()` / `case_when()` /
   `if_else()` (except the dry-flag sliding window in `dissox_qaqc`).
4. **Plotting removed:** All inline `ggplot2` + `plotly` plotting removed from QAQC
   functions. No `plot_only` parameter. Plotting centralized in
   `plot_qaqc_timeseries()`.
5. **Input validation:** All new functions add proper `stop()` with informative
   messages. Old functions used `print()`.
6. **Dual mode:** Several new functions support both file-based (scripting) and
   in-memory (Shiny) workflows via optional `path_to_output_folder` and `log_root`.
7. **Timestamp parsing:** All moved from `as.POSIXct()` with format strings to
   `lubridate::ymd_hms()`.

---

## 5. Column Naming Inconsistencies

### 5.1 `adjust_logger_NA()` — COND metric uses wrong column name

**File:** `R/adjust_logger_NA.R`

The COND branch expects and modifies `conductivity_uScm_adj` (line 358), but every
other function produces `conduct_uScm_adj`:

- `conductivity_qaqc()` creates `conduct_uScm_adj`
- `wq_drift_correction()` modifies `conduct_uScm_adj`
- `conductivity_temp_compensation()` reads `conduct_uScm_adj`
- `plot_qaqc_timeseries()` reads `conduct_uScm_adj`

**Impact:** Calling `adjust_logger_NA()` with `metric = "conductivity"` will fail with
`stop("adjust_logger_NA(): COND metric expects 'conductivity_uScm_adj'.")`.

**Note:** Line 119 of the same file correctly maps COND → `conduct_uScm_adj` in the
`adj_col` lookup, contradicting the COND processing block that starts at line 358.

**Fix:** Replace `conductivity_uScm_adj` → `conduct_uScm_adj` in the COND block
(lines 358-417).

### 5.2 `conductivity_temp_compensation()` — error messages use old function name

**File:** `R/conductivity_temp_compensation.R`

- Line 51: `"conduct_temp_comp(): Input missing required column(s): "`
- Line 103: `"conduct_temp_comp(): %.1f%% of watertemp_C_adj values are NA; "`

**Fix:** Update to `conductivity_temp_compensation()`.

### 5.3 `globals.R` — stale `airtemp_C_U20` entry

**File:** `R/globals.R`, line 7

The `utils::globalVariables()` call includes `"airtemp_C_U20"`, a legacy column name
only used by `watertemp_qaqc()` (itself legacy).

**Fix:** Remove when `watertemp_qaqc()` is cleaned up or removed.

### 5.4 `dissox_qaqc()` — `watertemp_C_do` vs `watertemp_C`

**File:** `R/dissox_qaqc.R`

DO QAQC renames `watertemp_C` to `watertemp_C_do` and creates `watertemp_C_do_adj`.
This is intentional (to distinguish DO-logger temp from other temp sources) but differs
from the WL and COND pipelines that use plain `watertemp_C` / `watertemp_C_adj`.

### 5.5 `get_logger_data_helpers.R` — legacy QAQC column references

**File:** `R/get_logger_data_helpers.R`

Contains a `factor_cols` vector with old-style names:

```r
factor_cols <- c("wl_qaqc_code", "do_qaqc_code", "wt_qaqc_code",
                 "at_qaqc_code", "baro_qaqc_code")
```

These belong to the old `main` branch pattern. Modern functions produce `flag_*` and
`edit_*` boolean columns. The code will silently do nothing.

---

## 6. Missing Helper Functions

### 6.1 `get_logger_data()` references undefined functions

**File:** `R/get_logger_data.R`

| Reference | Line | Purpose |
|---|---|---|
| `normalise_temporal_scale()` | 74 | Validate/normalize the `temporal_scale` argument |
| `read_logger_file()` | 114 | Read a single processed CSV file from disk |
| `build_plots()` | 145 | Create summary plots for aggregated data |

These are declared in `globals.R` (line 37) but never defined in any source file.

**Impact:** `get_logger_data()` will fail at runtime for:
- File-based loading (`path_to_data` argument) — `read_logger_file` undefined
- Any `temporal_scale` argument — `normalise_temporal_scale` undefined
- Summarization with plots — `build_plots` undefined

**Fix:** Implement the missing helpers or restrict to working code paths.

---

## 7. Dead / Unreferenced Code

### 7.1 `plot_helpers.R` — `make_qaqc_subplot()` is never called

**File:** `R/plot_helpers.R` (41 lines)

Defines `make_qaqc_subplot()`. Not called by any function in the package. Actual
plotting is handled by `plot_qaqc_timeseries()` and `plot_qaqc_timeseries_helpers.R`.

**Recommendation:** Remove `R/plot_helpers.R` entirely.

### 7.2 `NTSERP_palette()` — not used internally

**File:** `R/NTSERP_palette.R`

Exported but never called by any function. Modern plots use Okabe-Ito palette from
`utils_plotting.R`.

**Recommendation:** Keep if users rely on it for their own plots; otherwise remove.

---

## 8. Copy-Paste Errors

### 8.1 `adjust_WL_offset()` — error messages reference wrong function

**File:** `R/adjust_wl_offset.R`

- Line 66: `"adjust_wwl_offset()"` (typo — lowercase `wl`, extra `w`)
- Line 84: `"adjust_waterlevel_cablebreak(): Missing required column(s): "`
- Line 90: `"adjust_waterlevel_cablebreak(): no rows found for station '"`
- Line 157: `fun_name <- "adjust_waterlevel_cablebreak"`

**Fix:** Replace all four occurrences with `adjust_WL_offset`.

---

## 9. Renamed Functions (Old Name Gone from develop)

| main name | develop name | File |
|---|---|---|
| `conduct_temp_comp()` | `conductivity_temp_compensation()` | `R/conductivity_temp_compensation.R` |
| `convert_waterlevel_kPa_m()` | `convert_waterlevel_kPa_m()` | Same name, but file renamed: `convert_waterlevel_kPa_m.R` → `convert_wl_kpa_m.R`, completely rewritten |

---

## 10. New Functions (Not on main)

| Function | File | Purpose |
|---|---|---|
| `adjust_WL_offset()` | `R/adjust_wl_offset.R` | Modern cable-break correction |
| `adjust_WL_zero()` | `R/adjust_wl_zero.R` | Modern zero-reference shift |
| `barometric_qaqc_all()` | `R/baro_qaqc_all.R` | Batch baro QAQC across all stations |
| `conductivity_qaqc_all()` | `R/conductivity_qaqc_all.R` | Batch cond QAQC across all stations |
| `wq_drift_correction()` | `R/drift_conductivity.R` | Modern multi-metric drift correction |
| `wl_multipoint_correction()` | `R/drift_wl.R` | Multi-point WL drift correction |
| `plot_qaqc_timeseries()` | `R/plot_qaqc_timeseries.R` | Unified QAQC review plot |
| `resample_timestamps()` | `R/snap_logger_timestamps.R` | Snap timestamps to regular grid |
| `wl_set_datum()` | `R/wl_set_datum.R` | Set elevation datum from level runs |
| `QAQC_metadata()` | `R/bind_hobo_files_helpers.R` | Validate deployment metadata |
| `QAQC_reference_data()` | `R/convert_wl_kpa_m_helpers.R` | Validate reference measurements |

New internal modules:

| File | Purpose |
|---|---|
| `R/add_nearest_baro_helpers.R` | Baro selection/calibration helpers |
| `R/bind_hobo_files_helpers.R` | File parsing and metadata validation |
| `R/convert_wl_kpa_m_helpers.R` | WL conversion helpers |
| `R/get_logger_data_helpers.R` | Data loading/summarization helpers (incomplete) |
| `R/plot_qaqc_timeseries_helpers.R` | Plot construction helpers |
| `R/qaqc_log_helpers.R` | Structured QAQC logging infrastructure |
| `R/utils.R` | General utilities |
| `R/utils_plotting.R` | Plot colour/style definitions |
| `R/globals.R` | Global variable declarations |

---

## 11. NAMESPACE and Export Cleanup

### Functions exported on `develop` that should be reviewed

| Export | Status | Action |
|---|---|---|
| `adjust_waterlevel_cablebreak` | Legacy, replaced by `adjust_WL_offset` | **Remove export** |
| `adjust_waterlevel_zero` | Legacy, replaced by `adjust_WL_zero` | **Remove export** |
| `correct_drift` | Legacy, marked `[Superseded]`, replaced by `wq_drift_correction` | **Remove export** |
| `airtemp_qaqc` | Legacy, no modern replacement | Modernize or remove |
| `watertemp_qaqc` | Legacy, no modern replacement | Modernize or remove |
| `QAQC_metadata` | Exported helper; could be internal | Consider `@keywords internal` |
| `QAQC_reference_data` | Exported helper; could be internal | Consider `@keywords internal` |

### Exports on `main` that are gone from `develop`

| Export | Status |
|---|---|
| `conduct_temp_comp` | Renamed to `conductivity_temp_compensation` |

### `import()` directives removed

The `main` branch NAMESPACE had full `import()` directives:

```
import(dplyr)
import(ggplot2)
import(lubridate)
import(plotly)
import(purrr)
import(tidyr)
```

The `develop` branch has no `import()` directives, only `export()` entries. This is
correct — the modern code uses `dplyr::`, `ggplot2::` etc. namespace-qualified calls.

### Dependency changes

| Removed from `main` | Added in `develop` |
|---|---|
| `data.table` | `rlang` |
| `plyr` | `tibble` |
| `readr` | `testthat (>= 3.0.0)` (Suggests) |
| | `withr` (Suggests) |

---

## 12. Recommendations Summary

### Priority 1 — Remove legacy duplicates

Remove the following files and their NAMESPACE exports. They have direct modern
replacements and are not used by any current vignette or workflow:

- [ ] `R/adjust_waterlevel_cablebreak.R` → replaced by `adjust_WL_offset()`
- [ ] `R/adjust_waterlevel_zero.R` → replaced by `adjust_WL_zero()`
- [ ] `R/correct_drift.R` → replaced by `wq_drift_correction()`

### Priority 2 — Fix bugs

- [ ] `R/adjust_logger_NA.R`: Change `conductivity_uScm_adj` → `conduct_uScm_adj`
  in the COND block (lines 358-417)
- [x] `R/adjust_wl_offset.R`: Change `adjust_waterlevel_cablebreak` →
  `adjust_WL_offset` in error messages (lines 66, 84, 90, 157)
- [ ] `R/conductivity_temp_compensation.R`: Change `conduct_temp_comp()` →
  `conductivity_temp_compensation()` in error/warning messages (lines 51, 103)
- [ ] `R/get_logger_data.R`: Implement missing helpers (`normalise_temporal_scale`,
  `read_logger_file`, `build_plots`) or restrict to working code paths
- [ ] `R/write_clean_data.R`: Add conductivity to `metric_config` (regression vs main)

### Priority 3 — Modernize or remove remaining legacy functions

- [ ] `R/airtemp_qaqc.R`: Update column defaults from `airtemp_C_TidbiT` to modern
  names, add QAQC log integration, or remove if not needed
- [ ] `R/watertemp_qaqc.R`: Update column defaults from `_U20` to modern names, fix
  hardcoded `airtemp_C_U20` in plot path, add QAQC log, or remove

### Priority 4 — Clean up dead code

- [ ] Remove `R/plot_helpers.R` (`make_qaqc_subplot()` is never called)
- [ ] Remove `airtemp_C_U20` from `R/globals.R` (after addressing `watertemp_qaqc`)
- [ ] Remove `normalise_temporal_scale`, `read_logger_file`, `build_plots` from
  `R/globals.R` (after implementing or removing from `get_logger_data`)
- [ ] Clean up legacy `factor_cols` in `R/get_logger_data_helpers.R`

### Priority 5 — Evaluate export surface

- [ ] Consider making `QAQC_metadata()` and `QAQC_reference_data()` internal
  (`@keywords internal`) unless they're intended for end-user use
- [ ] Review whether `NTSERP_palette()` should remain exported

---

## Appendix: Full File Inventory Comparison

### Files on both branches (18 files)

| File (main) | File (develop) | Status |
|---|---|---|
| `add_nearest_baro.R` | `add_nearest_baro.R` | **Rewritten** — new API, no `_U20` |
| `adjust_logger_NA.R` | `adjust_logger_NA.R` | **Rewritten** — new API, has COND naming bug |
| `adjust_waterlevel_cablebreak.R` | `adjust_waterlevel_cablebreak.R` | **Unchanged** — legacy, should remove |
| `adjust_waterlevel_spike.R` | `adjust_waterlevel_spike.R` | **Rewritten** — new API, fixed row-index bug |
| `adjust_waterlevel_zero.R` | `adjust_waterlevel_zero.R` | **Unchanged** — legacy, should remove |
| `airtemp_qaqc.R` | `airtemp_qaqc.R` | **Minimally changed** — still legacy |
| `barometric_qaqc.R` | `barometric_qaqc.R` | **Rewritten** — completely new implementation |
| `bind_hobo_files.R` | `bind_hobo_files.R` | **Rewritten** — new API, no `logger_type` param |
| `conductivity_qaqc.R` | `conductivity_qaqc.R` | **Rewritten** — new API |
| `conductivity_temp_compensation.R` | `conductivity_temp_compensation.R` | **Rewritten** — renamed from `conduct_temp_comp`, simplified |
| `convert_do_mgl_percsat.R` | `convert_do_mgl_percsat.R` | **Rewritten** — new API |
| `convert_waterlevel_kPa_m.R` | `convert_wl_kpa_m.R` | **Rewritten** — file renamed, new API |
| `correct_drift.R` | `correct_drift.R` | **Marked superseded** — should remove |
| `dissox_qaqc.R` | `dissox_qaqc.R` | **Rewritten** — new API |
| `get_logger_data.R` | `get_logger_data.R` | **Rewritten** — new API, missing helpers |
| `NTSERP_palette.R` | `NTSERP_palette.R` | **Unchanged** |
| `waterlevel_qaqc.R` | `waterlevel_qaqc.R` | **Rewritten** — new API |
| `watertemp_qaqc.R` | `watertemp_qaqc.R` | **Minimally changed** — still legacy |
| `write_clean_data.R` | `write_clean_data.R` | **Rewritten** — new API |

### Files only on develop (20 new files)

| File | Type |
|---|---|
| `add_nearest_baro_helpers.R` | Internal helpers |
| `adjust_wl_offset.R` | New function (replaces `adjust_waterlevel_cablebreak`) |
| `adjust_wl_zero.R` | New function (replaces `adjust_waterlevel_zero`) |
| `baro_qaqc_all.R` | New function (batch baro QAQC) |
| `bind_hobo_files_helpers.R` | Internal helpers + `QAQC_metadata()` |
| `conductivity_qaqc_all.R` | New function (batch cond QAQC) |
| `convert_wl_kpa_m_helpers.R` | Internal helpers + `QAQC_reference_data()` |
| `drift_conductivity.R` | New function (`wq_drift_correction`, replaces `correct_drift`) |
| `drift_wl.R` | New function (`wl_multipoint_correction`) |
| `get_logger_data_helpers.R` | Internal helpers (incomplete) |
| `globals.R` | Global variable declarations |
| `plot_helpers.R` | Dead code (`make_qaqc_subplot`, never called) |
| `plot_qaqc_timeseries.R` | New function (unified QAQC review plot) |
| `plot_qaqc_timeseries_helpers.R` | Internal helpers |
| `qaqc_log_helpers.R` | QAQC logging infrastructure |
| `snap_logger_timestamps.R` | New function (`resample_timestamps`) |
| `utils.R` | General utilities |
| `utils_plotting.R` | Plot styling utilities |
| `wl_set_datum.R` | New function (datum from level runs) |

### Files only on main (0 files)

All files from `main` have a counterpart on `develop` (some renamed).
