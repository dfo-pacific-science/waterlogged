#' Temperature-compensate conductivity to specific conductance
#'
#' Converts measured electrical conductivity (EC) to specific conductance
#' (SPC) normalised to 25 degrees C using the temperature
#' compensation factors from ISO 27888.  A 5th-order polynomial is
#' fitted to the published coefficient table and applied as
#' \code{SPC = f(T) * EC}.
#'
#' Can be run either before or after \code{\link{conductivity_qaqc}}.
#' The function prefers \code{*_adj} columns when available and falls
#' back to the raw columns (\code{conduct_uScm}, \code{watertemp_C})
#' otherwise.
#'
#' @param input_data A data frame of conductivity observations.
#'   Must contain at least one of \code{conduct_uScm_adj} or
#'   \code{conduct_uScm} (electrical conductivity in uS/cm) and at
#'   least one of \code{watertemp_C_adj} or \code{watertemp_C}
#'   (water temperature in degrees C).
#'
#' @return \code{input_data} with one column added:
#'   \describe{
#'     \item{\code{spc_uScm_adj}}{Specific conductance
#'       (uS/cm at 25 degrees C).  \code{NA} where either the
#'       conductivity or temperature input is \code{NA}.}
#'   }
#'   A warning is issued when more than 5\% of temperature values are
#'   \code{NA}.
#'
#' @details
#' The compensation coefficients are read from
#' \code{inst/extdata/tempcomp27888.csv} (shipped with the package),
#' which tabulates the ISO 27888 correction factor at 0.1 degrees C
#' resolution from 0 to 35 degrees C.
#' The table is pivoted to long format and a 5th-order polynomial
#' \eqn{f(T)} is fitted by ordinary least squares.  At
#' 25 degrees C the coefficient equals 1.0 (i.e. no correction).
#'
#' @seealso \code{\link{conductivity_qaqc}},
#'   \code{\link{wq_drift_correction}}
#'   (optional drift correction for conductivity)
#'
#' @export
conductivity_temp_compensation <- function(input_data) {

  # ---- Resolve input columns (prefer _adj, fall back to raw) -----------------
  has_cond_adj <- "conduct_uScm_adj" %in% names(input_data)
  has_cond_raw <- "conduct_uScm"     %in% names(input_data)
  has_temp_adj <- "watertemp_C_adj"  %in% names(input_data)
  has_temp_raw <- "watertemp_C"      %in% names(input_data)

  if (!has_cond_adj && !has_cond_raw) {
    stop(
      "conductivity_temp_compensation(): Input must contain ",
      "'conduct_uScm_adj' or 'conduct_uScm'."
    )
  }
  if (!has_temp_adj && !has_temp_raw) {
    stop(
      "conductivity_temp_compensation(): Input must contain ",
      "'watertemp_C_adj' or 'watertemp_C'."
    )
  }

  cond_col <- if (has_cond_adj) "conduct_uScm_adj" else "conduct_uScm"
  temp_col <- if (has_temp_adj) "watertemp_C_adj"  else "watertemp_C"

  # ---- Temperature compensation coefficients (ISO 27888 nonlinear) -----------
  path_tcomp_tbl <- system.file("extdata", "tempcomp27888.csv", package = "waterlogged")
  tcomp <- utils::read.csv(path_tcomp_tbl)

  # tidy table: expand temp range offset columns X0..X0.9
  tlong <- tidyr::pivot_longer(
    tcomp,
    cols = dplyr::starts_with("X"),
    names_to = "Tsub",
    values_to = "coef"
  )

  tlong$Tsub <- as.numeric(gsub("X", "", tlong$Tsub))
  tlong <- tlong |>
    dplyr::mutate(temp = Temp_C + Tsub) |>
    dplyr::select(temp, coef) |>
    dplyr::arrange(temp)

  # fit 5th-order polynomial (same as original)
  model5 <- stats::lm(coef ~ poly(temp, 5, raw = TRUE), data = tlong)
  coefs <- stats::coef(model5)

  tempcomp <- function(T){
    coefs[1] +
      coefs[2] * T +
      coefs[3] * T^2 +
      coefs[4] * T^3 +
      coefs[5] * T^4 +
      coefs[6] * T^5
  }

  # ---- Compute SPC -----------------------------------------------------------
  out <- input_data

  cond_vals <- out[[cond_col]]
  temp_vals <- out[[temp_col]]

  out$spc_uScm_adj <- ifelse(
    is.na(temp_vals) | is.na(cond_vals),
    NA_real_,
    tempcomp(temp_vals) * cond_vals
  )

  # Warn if many temps are missing
  pct_missing_temp <- mean(is.na(temp_vals)) * 100
  if (pct_missing_temp > 5) {
    warning(
      sprintf(
        "conductivity_temp_compensation(): %.1f%% of %s values are NA; ",
        pct_missing_temp, temp_col
      ),
      "specific conductance could not be computed for those timestamps."
    )
  }

  return(out)
}
