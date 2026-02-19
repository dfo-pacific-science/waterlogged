<!-- README.md is generated from README.Rmd. Please edit that file -->



# waterlogged

<!-- badges: start -->
<!-- badges: end -->

## Overview

waterlogged is a data processing package for water data from continuous
water data loggers. It streamlines the process from raw data to clean data,
documents QAQC processing via R scripts, and does so without relying on
proprietary software.

The package was built for data from Onset HOBO barometric pressure/water
level (U20), dissolved oxygen (U26), conductivity (U24), and TidbiT loggers.

All core functions support both **file-based workflows** (read from / write
to disk) and **in-memory workflows** (pass data frames directly), making the
package compatible with interactive R scripts and Shiny applications alike.

## Installation

Install the development version from
[GitHub](https://github.com/daniella-lo/waterlogged):

``` r
# install.packages("pak")
pak::pak("daniella-lo/waterlogged")
```

## Workflow

waterlogged was designed for hydrology monitoring across multiple streams
and wetlands over multiple years, with multiple loggers installed at each
site and station. File management and standardized QAQC are critical for
ensuring long-term data storage, accessibility, and integrity.

Central to this is a single metadata file listing each logger's serial
number, location, and timestamps of installation/retrieval
(*logger_manager.csv*). This metadata file is used by many of the package's
functions and should be maintained for each project.

Data processing is done by logger type from the previous field season (e.g.,
all water level loggers deployed in 2024). Processing annually ensures that
water quality reports can be produced for scientific analysis and management
in a timely manner.

Quality assurance is done by both automatic and manual processes. Automated
functions remove erroneous data and flag outliers and potential sources of
error (e.g., if water temperature or barometric pressure indicates a logger
may be out of water). Manual investigation of each station is then performed
using interactive time-series plots. All processing is recorded in QAQC log
files and as flag/edit columns in the data.

### Step 0: File management

- Download data from HOBO loggers and export files as CSV.
- Organize files by logger type/metric, year, and processing stage (raw,
  processed, clean).
- Ensure correct formatting of the logger management metadata file.
- Save an R script recording all function inputs for each logger type and
  year processed.

### Step 1: Import and standardize logger data

- **`bind_hobo_files()`** reads raw HOBO CSV files, links each logger to
  deployment metadata from the logger management file, binds data by station
  code, and trims to install/retrieval timestamps. Outputs v0.1 files.
- **`resample_timestamps()`** aligns (snaps) logger timestamps to a regular
  interval (e.g., 5, 10, 15, 30 min) without changing the sampling
  frequency.
- **`barometric_qaqc_all()`** runs automatic QAQC on all barometric
  stations listed in the metadata file.

The following steps depend on the type of data being processed:

- For dissolved oxygen or water level, continue to Step 2.
- For barometric, conductivity, or temperature (TidbiT), skip to Step 3.

### Step 2: Unit conversion

These steps require hourly barometric pressure data in kPa (from Step 1).

- **`add_nearest_baro()`** attaches the nearest barometric logger's data to
  each DO or water level station, either automatically (by geographic
  proximity) or by manual station assignment. Outputs v0.2 files.
- **`convert_do_mgl_percsat()`** computes percent air saturation of
  dissolved oxygen from mg/L, water temperature, and barometric pressure.
  Outputs v0.3 files.
- **`convert_waterlevel_kPa_m()`** converts water level from kPa to metres
  using barometric compensation and reference data. Outputs v0.3 files.

### Step 3: Automatic quality assurance

Each function applies range checks, removes error codes, flags extreme
outliers, and flags data that may be erroneous due to dry or ice conditions.

- **`waterlevel_qaqc()`** -- water level and water temperature data.
- **`dissox_qaqc()`** -- dissolved oxygen (mg/L and %sat) and water
  temperature data.
- **`conductivity_qaqc()`** -- conductivity and water temperature data.
- **`barometric_qaqc()`** -- barometric pressure and air temperature data
  (single station).
- **`watertemp_qaqc()`** -- water temperature data from TidbiT loggers.
- **`airtemp_qaqc()`** -- air temperature data.

### Step 4: Manual quality assurance

Use **`plot_qaqc_timeseries()`** to generate interactive plotly time-series
overlays for visual inspection (e.g., compare water temperature from a water
level logger to air temperature from a barometric logger at the same site).

Adjustment functions for targeted corrections:

- **`adjust_logger_NA()`** -- set values to NA over a time window (dry,
  disturbed, in ice).
- **`adjust_WL_offset()`** -- apply a constant offset to water level after
  a cable break or logger shift.
- **`adjust_WL_zero()`** -- shift water level up or down when a logger is
  known to be dry but does not read zero.
- **`adjust_waterlevel_spike()`** -- flatten water level spikes caused by
  snow/ice/sediment buildup using linear interpolation.
- **`wl_set_datum()`** -- set an arbitrary datum for water level series
  using survey-grade level run data.

Drift correction functions:

- **`wl_multipoint_correction()`** -- multi-point linear drift correction
  for water level.
- **`wq_drift_correction()`** -- forward linear drift correction for
  conductivity, dissolved oxygen, or pH.
- **`correct_drift()`** -- conductivity drift correction based on discrete
  reference measurements.

All corrections are recorded in QAQC log files. When `log_root` is
provided, logs are written to disk; otherwise, log rows are returned as a
data frame attribute (`attr(result, "qaqc_log")`).

### Step 5: Export clean data

- **`write_clean_data()`** writes the final dataset per station and year
  with the naming convention `<station>_<metric>_<start>_<end>_v1.0.csv`.

A script that includes all code up to this point should be saved. Any
revisions to the clean dataset should be saved with a new suffix (e.g.,
v1.1) and noted in the QAQC script.

### Step 6: Retrieve and summarize clean data

- **`get_logger_data()`** retrieves data by processing stage, station code,
  and logger type.
- Clean (v1.0) data can be summarized to hourly, daily, weekly, monthly, or
  yearly temporal scales (mean, min, max, range) with optional plots.
