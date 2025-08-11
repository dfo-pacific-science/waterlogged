
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waterlogged

<!-- badges: start -->
<!-- badges: end -->

## WELCOME TO waterlogged!

water-logged is a data processing package for water data from continuous
water data loggers. The goal of water-logged is to streamline the
process from raw data to clean data, document QAQC processing via r
scripts, and do so without relying on proprietal software.

This version of the package was built for data from Onset HOBO
barometric pressure/water level (U20), dissolved oxygen (U26),
conductivity (U24) and TibiT loggers. Future versions will test this
package on data from other types of loggers to try to make it a more
universal package.

## How to use this package: Workflow

This package was designed to meet the needs of hydrology monitoring of
multiple streams and wetlands over multiple years, with multiple loggers
installed at each site and station. File management and standardized
QAQC is critical for ensuring long-term data storage, accessibility, and
integrity.

Central to this, is a single dataset that lists each logger’s serial
number, location and timestamp of installation/retrieval,
*logger_manager.csv*. This is a meta-dataset that is used in many of
this package’s functions, and should be replicated for each project
using this package.

Our workflow generally follows the field season of logger installation
in early spring, data collection throughout the summer, and logger
retrieval/downloads in late fall. After data is downloaded from loggers,
raw files are organized by logger/data type and the year in which they
were downloaded For example, a waterlevel (U20) logger was downloaded in
Oct 2024. The raw file would be placed in the respective project data
folder path: *data/waterlevel/2024/raw*. All other waterlevel files
downloaded in 2024 would be located in the same folder.

Data processing is then done by logger type from the previous field
season (ie. all waterlevel loggers deployed in 2024). Processing by
logger type ensures that all data from the same logger type are being
processed using the same protocol and calculations. Processing annually
ensures that water quality reports can be produced for scientific
analysis and management in a timely manner.

Quality assurance of data is done by both automatic and manual
processes. Automated functions remove erroneous data, and flag of
outliers and potential sources of error (ie. if water
temperature/barometric pressure indicates a logger may be out of water).

From our experience with hydrology data, it is then necessary to
investigate data from each station individually. This can be done
efficiently by overlaying data from multiple loggers (if applicable)
from the same station and time period. Visual investigation of data
timeseries and automated flags allows the user to confirm periods where
loggers were dry, frozen or disturbed by human/natural cause. All manual
data processing is saved in an R script for tracking QAQC of each logger
type each year, and recorded as additional qaqc columns in clean data
files.

### Step 0: File management

- Download data from HOBO loggers; export files as csv
- Files should be organized by logger type/metric, year and processing
  stage (raw, processed, clean)
- Ensure correct formatting of logger management file
- A R script should be saved that records all function variable inputs
  for each logger type/year processed

### Step 1: Convert logger data to R friendly formatting, with cross referenceing to station/logger metadata

- **bind_hobo_files** Extract data and metadata from logger files; merge
  with metadata from logger management file; bind logger data by station
  code; trim logger data by install/retrieval timestamps

The following steps are dependent on type of data being processed:

- For dissolved oxygen, water level, continue to step 2
- For water temperature (TidbiT), skip to step 3

### Step 2: Convert raw data metrics to desired units

- requires hourly barometric pressure data in kPa (step 1)
- **add_nearest_baro** binds the closest barometric logger to the target
  logger at each station
- For dissolved oxygen, **convert_do_mgl_percsat** compute the percent
  air saturation of dissolved oxygen
- For waterlevel, **convert_waterlevel_kPa_m** converts water level from
  kPa into metres

### Step 3: Automatic quality assurance of water data

- **dissox_qaqc** takes processed dissolved oxygen data, removes error
  codes, removes extreme outliers, and flags data that may be erroneous
  due to dry or ice conditions
- **waterlevel_qaqc** takes processed waterlevel data, removes error
  codes, removes extreme outliers, and flags data that may be erroneous
  due to dry or ice conditions
- **watertemp_qaqc** takes processed water temperature data from Tidbit
  loggers, removes error codes, removes extreme outliers, and flags data
  that may be erroneous due to dry or ice conditions

### Step 4: Manual quality assurance of water data by station

Plotly is used as an interactive plotting feature to allow the user to
overlay water data from multiple loggers ie. compare water temperature
from water level logger to air temperature from barometric logger at the
same site) and zoom in on time periods of interest; this helps to
identify data where a logger has gone dry, frozen or been disturbed. A
number of helper functions are used to adjust data within selected
timestamps and label and record any corrections made in the dataset.

- **adjust_logger_NA**: to remove erroneous data, user specifies logger
  type, timestamp period where data is erroneous, and reason for
  removing data (dry/disturbed/in ice).
- **adjust_waterlevel_cablebreak**: to adjust waterlevel in the case
  that the cable holding the logger has broken (a sudden shift up in
  recorded water level, due to the logger dropping in the water column).
  User specifies time period where cable break occurrs.
- **adjust_waterlevel_zero** to shift the recorded waterlevel up or down
  if a logger is known to be dry, and the recorded waterlevel data is
  not equal to zero.
- **adjust_waterlevel_spike** to flatten spikes in waterlevel due to
  snow/ice/sediment buildup on top of logger during deployment. User
  specifies when spike starts and ends. Function uses a linear slope to
  estimate average change in waterlevel during this time period.

### Step 5: Save, export clean dataset

- **write_clean_data** exports clean data with suffix \[starting
  timestamp\]\_\[endiong timestamp\]\_v1.0

A script that includes all code up to this point should be saved. Any
revisions to the clean dataset that follow this QAQC output should be
saved with a new suffix (ie. \_v1.1) and noted in the QAQC script.

### Step 6: Retrieve, summarize clean data

- **get_logger_data** retrieves data by processing stage, station code,
  year and logger type
- Clean data can also be summarized as average, min, max, range of
  yearly, monthly, daily using this function

## (Placeholder): Installation

You can install the development version of waterlogged from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("daniella-lo/waterlogged")
```
