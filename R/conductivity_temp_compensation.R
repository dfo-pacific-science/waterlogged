#' Temperature-compensate conductivity to specific conductance
#'
#' Converts measured electrical conductivity (EC) to specific conductance
#' (SPC) normalised to 25 degrees C using the temperature
#' compensation factors from ISO 27888.  A 5th-order polynomial is
#' fitted to the published coefficient table and applied as
#' \code{SPC = f(T) * EC}.
#'
#' Typically run after \code{\link{conductivity_qaqc}} (or
#' \code{\link{conductivity_qaqc_all}}) so that the adjusted columns
#' \code{conduct_uScm_adj} and \code{watertemp_C_adj} are available.
#'
#' @param input_data A data frame of conductivity observations.
#'   Must contain \code{conduct_uScm_adj} (adjusted electrical
#'   conductivity in uS/cm) and \code{watertemp_C_adj}
#'   (adjusted water temperature in degrees C).
#'   Typically the output of \code{\link{conductivity_qaqc}}.
#'
#' @return \code{input_data} with one column added:
#'   \describe{
#'     \item{\code{spc_uScm_adj}}{Specific conductance
#'       (uS/cm at 25 degrees C).  \code{NA} where either
#'       \code{conduct_uScm_adj} or \code{watertemp_C_adj} is
#'       \code{NA}.}
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
#' @seealso \code{\link{conductivity_qaqc}} (upstream QAQC that produces
#'   the \code{_adj} columns), \code{\link{wq_drift_correction}}
#'   (optional drift correction for conductivity)
#'
#' @export
conductivity_temp_compensation <- function(input_data) {
  
  # ---- Required columns ------------------------------------------------------
  req_cols <- c("conduct_uScm_adj", "watertemp_C_adj")
  missing_cols <- setdiff(req_cols, names(input_data))
  
  if (length(missing_cols) > 0) {
    stop(
      "conduct_temp_comp(): Input missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # ---- Temperature compensation coefficients (ISO 7888 nonlinear) -----------
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
  
  out$spc_uScm_adj <- with(out, {
    ifelse(
      is.na(watertemp_C_adj) | is.na(conduct_uScm_adj),
      NA_real_,
      tempcomp(watertemp_C_adj) * conduct_uScm_adj
    )
  })
  
  # Warn if many temps are missing
  pct_missing_temp <- mean(is.na(out$watertemp_C_adj)) * 100
  if (pct_missing_temp > 5) {
    warning(
      sprintf(
        "conduct_temp_comp(): %.1f%% of watertemp_C_adj values are NA; ",
        pct_missing_temp
      ),
      "specific conductance could not be computed for those timestamps."
    )
  }
  
  return(out)
}
