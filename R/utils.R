
#' Null-coalescing operator
#'
#' Returns \code{a} if it is non-NULL, non-NA, non-empty, and non-blank;
#' otherwise returns \code{b}.
#'
#' @param a,b Values to test and fall back from/to.
#' @return \code{a} or \code{b}.
#' @keywords internal
#' @noRd
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


#' Parse Date-Time Vectors to UTC POSIXct
#'
#' Converts various date-time string formats into standard \code{POSIXct} objects
#' in the UTC timezone. It incorporates progressive parsing to handle existing
#' \code{POSIXct} objects, completely blank vectors, and multiple date-time
#' formatting orders.
#'
#' @param x A vector (typically character) containing date-time information to be parsed.
#' @param col_name A character string representing the name of the input column.
#'   Used to generate informative error messages upon parsing failure. Defaults to \code{"timestamp"}.
#'
#' @details
#' The function executes parsing in the following sequence:
#' \enumerate{
#'   \item Returns the input immediately if it already inherits from \code{"POSIXct"}.
#'   \item Checks for vectors comprised entirely of \code{NA} or blank strings,
#'         returning a vector of \code{NA} \code{POSIXct} values if true.
#'   \item Attempts an initial parse using \code{\link[lubridate]{mdy_hm}}.
#'   \item Falls back to \code{\link[lubridate]{parse_date_time}} using a predefined
#'         set of common orders (\code{"mdy HM", "mdy HMS", "mdY HM", "mdY HMS",
#'         "ymd HM", "ymd HMS"}).
#' }
#' If all parsing attempts fail for non-blank values, the function terminates.
#'
#' @return A vector of class \code{POSIXct} in the \code{"UTC"} timezone.
#'
#' @importFrom lubridate mdy_hm parse_date_time
#'
#' @examples
#' \dontrun{
#' # Parse standard character representations
#' parse_dt(c("4/30/2025 13:45", "04/30/2025 13:45:00"))
#'
#' # Handle vectors with mixed blanks/NAs
#' parse_dt(c("4/30/2025 13:45", "", "NA"))
#'
#' # Handle entirely blank vectors safely
#' parse_dt(c(NA, "", "  "))
#'
#' # Triggers custom error on complete failure
#' parse_dt(c("invalid_date"), col_name = "start_time")
#' }
#'
#' @export
parse_dt <- function(x, col_name = "timestamp") {
  if (inherits(x, "POSIXct")) return(x)

  x_chr     <- as.character(x)
  all_blank <- is.na(x_chr) | trimws(x_chr) %in% c("", "NA")

  # All values are blank/NA — valid for still-deployed loggers (timestamp_remove)
  if (all(all_blank)) {
    return(as.POSIXct(rep(NA_character_, length(x_chr)), tz = "UTC"))
  }

  p1 <- try(lubridate::mdy_hm(x_chr, tz = "UTC", quiet = TRUE), silent = TRUE)
  if (!inherits(p1, "try-error") && !all(is.na(p1[!all_blank]))) return(p1)

  p2 <- suppressWarnings(
    lubridate::parse_date_time(
      x_chr,
      orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "ymd HM", "ymd HMS"),
      tz = "UTC"
    )
  )

  if (all(is.na(p2[!all_blank]))) {
    stop("Failed to parse '", col_name, "' column. Expected format like '4/30/2025 13:45'.")
  }

  p2
}
