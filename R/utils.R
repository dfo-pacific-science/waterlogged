
#' Null-coalescing operator
#'
#' Returns \code{a} if it is non-NULL, non-NA, non-empty, and non-blank;
#' otherwise returns \code{b}.
#'
#' @param a,b Values to test and fall back from/to.
#' @return \code{a} or \code{b}.
#' @keywords internal
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) && !is.na(a) && nzchar(a)) a else b
}


#' Convert Fahrenheit to Celsius
#'
#' Converts a numeric vector of temperatures from degrees Fahrenheit
#' to degrees Celsius using the standard linear transformation.
#' Internal helper used when parsing HOBO logger files that report
#' temperature in Fahrenheit.
#'
#' @param x Numeric vector of temperatures in degrees Fahrenheit.
#'
#' @return A numeric vector of temperatures in degrees Celsius.
#'
#' @keywords internal
convert_F_to_C <- function(x) (x - 32) * (5/9)


#' Escape regex special characters
#'
#' @param x A character string.
#'
#' @return \code{x} with regex metacharacters escaped by backslash.
#'
#' @keywords internal
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
}


#' Normalise a character vector for matching
#'
#' Converts strings to lower case, trims leading/trailing whitespace,
#' and removes internal spaces.  Useful for normalising user inputs or
#' metadata fields before comparison.
#'
#' @param x A character vector.
#'
#' @return A character vector of the same length as \code{x}, normalised
#'   for case and spacing.
#'
#' @keywords internal
normalize_string <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", "", x)   # remove internal spaces
  x
}


#' Normalise a timezone string to an Olson name
#'
#' Converts a raw timezone string (from a POSIXct attribute or a logger
#' metadata column) to a valid Olson timezone name. Handles standard Olson
#' names, \code{GMT+HH:MM} / \code{GMT-HH:MM} offsets (with sign reversal
#' to match \code{Etc/GMT*} convention), and common Pacific aliases.
#' Returns \code{NULL} if the string cannot be resolved.
#'
#' @param x A single character string, or \code{NULL}/\code{NA}.
#'
#' @return A single Olson timezone name, or \code{NULL}.
#'
#' @keywords internal
normalize_tz <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  x <- trimws(x)
  if (x %in% OlsonNames()) return(x)
  if (grepl("^GMT[+-][0-9]{2}:[0-9]{2}$", x, ignore.case = TRUE)) {
    hh   <- as.integer(substr(x, 5, 6))
    sign <- substr(x, 4, 4)
    if (sign == "+") paste0("Etc/GMT-", hh) else paste0("Etc/GMT+", hh)  # Etc/* reverses sign
  } else if (tolower(x) %in% c("pst", "pdt", "pacific", "america/vancouver")) {
    "America/Vancouver"
  } else NULL
}


#' Find contiguous runs of missing values in a time series
#'
#' Identifies contiguous runs of \code{NA} values in a time series column and
#' returns their start/end timestamps, length (in points), and duration.
#'
#' The input data frame must contain a \code{timestamp} column that can be
#' ordered, and a numeric column specified by \code{value_col}.
#'
#' @param df A data frame containing at least a \code{timestamp} column and the
#'   value column to be checked for gaps.
#' @param value_col Name of the column (as a single character string) in
#'   \code{df} to inspect for \code{NA} runs. Defaults to
#'   \code{"airpress_kPa"}.
#' @param add_step Logical; if \code{TRUE}, the estimated native time step
#'   (median difference of \code{timestamp}) is added to the gap duration so
#'   that the reported duration corresponds to the full span covered by the
#'   missing values.
#'
#' @return A tibble with one row per gap, containing:
#'   \itemize{
#'     \item \code{gap_id}: integer identifier for the gap.
#'     \item \code{gap_start}: timestamp of the first missing value.
#'     \item \code{gap_end}: timestamp of the last missing value.
#'     \item \code{n_missing}: number of consecutive missing values.
#'     \item \code{duration}: time difference between \code{gap_start} and
#'       \code{gap_end}, optionally plus one time step.
#'   }
#'   If no missing values are found, an empty tibble with the same columns is
#'   returned.
#'
#' @keywords internal
find_na_runs <- function(df, value_col = "airpress_kPa", add_step = TRUE) {
  stopifnot("timestamp" %in% names(df))
  vcol <- rlang::sym(value_col)

  # ensure ordered
  df <- df |> dplyr::arrange(timestamp)

  # estimate native step (for regular series)
  dt <- suppressWarnings(stats::median(diff(df$timestamp), na.rm = TRUE))
  add_dt <- if (add_step && is.finite(as.numeric(dt))) dt else as.difftime(0, units = "secs")

  # logical NA vector
  is_na <- is.na(df[[value_col]])

  if (!any(is_na)) {
    return(tibble::tibble(
      gap_id = integer(), gap_start = as.POSIXct(character()), gap_end = as.POSIXct(character()),
      n_missing = integer(), duration = difftime(character(), character())
    ))
  }

  # start = NA now & (previous non-NA or row 1)
  starts <- which(is_na & !dplyr::lag(is_na, default = FALSE))
  # end   = NA now & (next non-NA or last row)
  ends   <- which(is_na & !dplyr::lead(is_na, default = FALSE))

  tibble::tibble(
    gap_id    = seq_along(starts),
    gap_start = df$timestamp[starts],
    gap_end   = df$timestamp[ends],
    n_missing = ends - starts + 1
  ) |>
    dplyr::mutate(duration = (gap_end - gap_start) + add_dt)
}
