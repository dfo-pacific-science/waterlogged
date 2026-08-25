
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waterlogged

<!-- badges: start -->
<!-- badges: end -->

## Overview

*waterlogged* is an open-source data processing tool for continuous
water logger data, which strives to increase transparency,
standardization, replicability, and longevity of aquatic environmental
data. This package allows users to process single or multiple data files
using standardized protocols that can be tailored to monitoring
objectives. The functionality of waterlogged is adaptable to multiple
logger types, and currently supports air and water temperature,
barometric pressure, water level, dissolved oxygen and conductivity.
Quality control (QC) is provided through semi-automated error detection
and outlier flagging, with thresholds that can be modified by the user
to meet specific deployment conditions. The inputs are raw data logger
files and metadata, and the outputs are ggplot objects, summary tables,
and cleaned data files. All QC processes completed in waterlogged are
recorded in a log for transparent and replicable data cleaning. Data
files processed through waterlogged are named and organized in a
consistent system, assisting with long-term data management. The package
helps aquatic researchers and practitioners apply transparent and
standardized quality control protocols to efficiently process large
amounts of environmental data.

*Disclaimer* This package is intended for processing and summarizing
data recorded using continuous data loggers (e.g., intervals in the
resolution of minutes to hours). This package is not suitable for event
logging (i.e., data recorded in irregular intervals, such as when a
temperature exceeds a threshold). This version of the package was built
based on data from Onset HOBO water level (U20), dissolved oxygen (U26),
conductivity (U24) and TidbiT loggers. However, data from any model of
datalogger can be QC’d using waterlogged if the raw input files are
formatted correctly. The information included in this report is not
endorsing the use of HOBOware equipment or programs. New functions may
be added in future versions of the package to expand use to additional
metrics as well as different makes and models of continuous data
loggers. This package was developed using R version 4.4.2.

## Installation

Install the development version from
[GitHub](https://github.com/dfo-pacific-science/waterlogged)

``` r
# install.packages("pak")
pak::pak("dfo-pacific-science/waterlogged")
```

## Workflow

The purpose of waterlogged is to efficiently process continuous
environmental data using standardized, transparent, and reproducible
protocols. The package contains an array of functions to compile and
organize logger data, perform user-defined quality control protocols, as
well as export and summarize continuous environmental data

Users of this package can expect to be guided from raw logger files to
clean, summarized environmental data by completing the workflow, as
follows:

1.  Prepare raw logger data, metadata and required reference data to
    read into waterlogged,
2.  Convert raw logger data metrics (if required),
3.  Pass data through automated quality control protocols (i.e. outlier
    and error detection),
4.  Manual inspection of data and quality control adjustments
    (i.e. identify and remove dry periods),
5.  Export, summarize and/or visualize clean data files.

<figure>
<img src="man\figures\README-workflow.png"
alt="Flowchart of workflow for processing data using waterlogged. Boxes represent specific functions within the package. The workflow sequence starts at the top, with Prepare Raw Data, and progresses in sequence down to Summarize Data. Within each sequence, functions specific to logger type are given in the coloured columns. Functions with solid outlines represent required functions to progress in the waterlogged workflow, while boxes with dashed outlines are optional functions" />
<figcaption aria-hidden="true">Flowchart of workflow for processing data
using waterlogged. Boxes represent specific functions within the
package. The workflow sequence starts at the top, with Prepare Raw Data,
and progresses in sequence down to Summarize Data. Within each sequence,
functions specific to logger type are given in the coloured columns.
Functions with solid outlines represent required functions to progress
in the waterlogged workflow, while boxes with dashed outlines are
optional functions</figcaption>
</figure>

## Prepare Raw Data

Raw data files from water loggers and a metadata file must first be
loaded into R or the online waterlogged Shiny application as
Comma-Separated Value files (CSV) . The information in the metadata file
is used to bind multiple logger files of the same logger type, remove
data recorded before deployment and or after removal and organize data
by station and year.

Example metadata file, including all required columns:

| site_station_code | timestamp_deploy | timestamp_remove | sn | metric | latitude | longitude |
|----|----|----|----|----|----|----|
| SLUG_ST_01 | 10/15/2023 8:16 | 10/9/2024 9:07 | 12345678 | barometric | 40.85979 | -119.265 |

## Convert Data Metric (if needed)

If required, raw data can be converted into other metrics for further
data processing.

Some conversions require matching barometric pressure data. Barometric
data can be bound to an existing non-baro logger dataset using the
function **‘add_nearest_baro’**.

<figure>
<img src="man\figures\add-nearest-baro.png"
alt="Example output of add_nearest_baro() for an input_data station identifier ‘FROG_WE_01’, and barometric data from the two nearest stations, ‘FROG_WE_01’ and ‘SLUG_ST_01’." />
<figcaption aria-hidden="true">Example output of add_nearest_baro() for
an input_data station identifier ‘FROG_WE_01’, and barometric data from
the two nearest stations, ‘FROG_WE_01’ and ‘SLUG_ST_01’.</figcaption>
</figure>

waterlogged currently supports the following data conversions:

- water pressure in kilopascals to depth or stage height in metres
  (requires matching barometric pressure data and field observations of
  depth or stage), **`convert_waterlevel_kPa_m()`**,
- dissolved oxygen concentration in milligrams per litre to percent air
  saturation (requires matching barometric pressure data),
  **`convert_do_mgl_percsat()`**,
- conductivity to specific conductance,
  **`convert_conductivity_uScm_SPC()`**.

Additional conversions may be added in future versions of the package.

## Automated Quality Control

Complied data can then be passed through automatic QC protocols to
identify and remove outliers and flag potential errors for user review.
For example, water temperature values greater than 25°C are flagged to
indicate the logger is potentially dry by default, but this flagging
threshold value can be changed by the user to fit the expected
conditions of their logger location.

<figure>
<img src="man\figures\qc-dissox.png"
alt="Example output of qc_dissox() and plot_qc_timeseries() for dissolved oxygen data from station identifier ‘FROG_WE_01’." />
<figcaption aria-hidden="true">Example output of qc_dissox() and
plot_qc_timeseries() for dissolved oxygen data from station identifier
‘FROG_WE_01’.</figcaption>
</figure>

A record of all data that is removed or flagged for review is saved in a
Quality Control Log file.

Example QC Log file, produced by **`qc_airtemp()`**:

| field action | action_note | ts_start | ts_end | duration_hours | fun_name | user |
|----|----|----|----|----|----|----|
| airtemp_C_adj | AUTO_QAQC | Air temperature outside \[-10, 50\] C; flagged for review | 3/4/2024 2:00 | 3/4/2024 11:00 | 9 | qc_airtemp() |

## Example Workflows by Logger Type

### Water Level

- **`bind_logger_files()`** — read raw water level CSVs → v0.1
- **`add_nearest_baro()`** — attach nearest baro station by geographic
  proximity → v0.2
- **`convert_waterlevel_kPa_m()`** — convert pressure to water level in
  metres using baro compensation and a field reference → v0.3
- **`qc_waterlevel()`** — auto QC (negative temps, ice, step
  disturbances, dry sensor)
- **`plot_qc_timeseries()`** — multi-panel time-series plot for visual
  review
- Manual corrections as needed: **`adjust_logger_NA()`**,
  **`adjust_waterlevel_offset()`**, **`adjust_waterlevel_spike()`**,
- **`adjust_waterlevel_datum()`** — (optional) convert stage to
  elevation using survey benchmarks
- **`write_clean_data()`** — export final dataset per station/year →
  v1.0

### Dissolved Oxygen

- **`bind_logger_files()`** — read raw DO CSVs → v0.1
- **`add_nearest_baro()`** — attach nearest baro station → v0.2
- **`convert_do_mgl_percsat()`** — compute percent saturation from mg/L,
  temperature, and pressure → v0.3
- **`qc_dissox()`** — auto QC (errors, negative DO, high outliers, ice,
  dry sensor)
- **`plot_qc_timeseries()`** — visual review
- Manual corrections as needed: **`adjust_logger_NA()`** (ice, dry,
  disturbance)
- **`write_clean_data()`** — export → v1.0

### Conductivity

- **`bind_logger_files()`** — read raw conductivity CSVs → v0.1
- **`convert_conductivity_uScm_SPC()`** — optional - convert EC to
  specific conductance (SPC) at 25 °C
- **`qc_conductivity()`** — auto QC (spikes, ice, temp range, dry
  sensor)
- **`plot_qc_timeseries()`** — visual review
- Manual corrections as needed: **`adjust_logger_NA()`** (ice, dry,
  disturbance)
- **`write_clean_data()`** — export → v1.0

### Shared Utilities

- **`resample_timestamps()`** — snap logger timestamps to a regular
  interval
- **`get_logger_data()`** — retrieve processed data by stage, station,
  and logger type
- **`summarise_clean_data()`** - produces summary statistics by station
  for a specified temporal scale (e.g. daily mean, range…)
- **`plot_clean_timeseries`** - visualise clean data or summary data

Example summary table produced by **`summarise_clean_data()`** for
hourly barometric data aggregated to the daily temporal scale:

| site_station_code | time_group | mean_airpress_kPa | mean_airtemp_C | min_airpress_kPa | min_airtemp_C | max_airpress_kPa | max_airtemp_C |
|----|----|----|----|----|----|----|----|
| FISH_ST_01 | 1/1/2025 | 91.8 | -6.88 | 91.75 | -8.71 | 91.9 | -5.12 |

<figure>
<img src="man\figures\barosummplot.png"
alt="Example summary plot produced by plot_clean_timeseries(). Hourly barometric pressure data is summarized by mean daily pressure; shaded area indicated 5-95% quartiles of barometric pressure per day. Plots are facetted by station identifiers" />
<figcaption aria-hidden="true">Example summary plot produced by
plot_clean_timeseries(). Hourly barometric pressure data is summarized
by mean daily pressure; shaded area indicated 5-95% quartiles of
barometric pressure per day. Plots are facetted by station
identifiers</figcaption>
</figure>
