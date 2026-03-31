#' Temperature-compensate conductivity to specific conductance
#'
#' Converts measured electrical conductivity (EC) to specific conductance
#' (SPC) normalised to 25 degrees C using the temperature
#' compensation factors from ISO 27888.  A 5th-order polynomial is
#' fitted to the published coefficient table and applied as
#' \code{SPC = f(T) * EC}.
#'
#' Can be run either before or after \code{\link{qc_conductivity}}.
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
#' The compensation coefficients are from ISO 27888, tabulating the
#' correction factor at 0.1 degrees C resolution from 0 to 35 degrees C.
#' The coefficients are embedded directly in the function source.
#' A 5th-order polynomial
#' \eqn{f(T)} is fitted by ordinary least squares.  At
#' 25 degrees C the coefficient equals 1.0 (i.e. no correction).
#'
#' @seealso \code{\link{qc_conductivity}},
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
  # Coefficients from ISO 27888 table (0-35 deg C at 0.1 deg resolution).
  # Hardcoded to avoid dependency on inst/extdata/tempcomp27888.csv.
  tlong <- data.frame(
    temp = seq(0, 35.9, by = 0.1),
    coef = c(
      # 0.0 - 0.9
      1.918, 1.912, 1.906, 1.899, 1.893, 1.887, 1.881, 1.875, 1.869, 1.863,
      # 1.0 - 1.9
      1.857, 1.851, 1.845, 1.840, 1.834, 1.829, 1.822, 1.817, 1.811, 1.805,
      # 2.0 - 2.9
      1.800, 1.794, 1.788, 1.783, 1.777, 1.772, 1.766, 1.761, 1.756, 1.750,
      # 3.0 - 3.9
      1.745, 1.740, 1.734, 1.729, 1.724, 1.719, 1.713, 1.708, 1.703, 1.698,
      # 4.0 - 4.9
      1.693, 1.688, 1.683, 1.678, 1.673, 1.668, 1.663, 1.658, 1.653, 1.648,
      # 5.0 - 5.9
      1.643, 1.638, 1.634, 1.629, 1.624, 1.619, 1.615, 1.610, 1.605, 1.601,
      # 6.0 - 6.9
      1.596, 1.591, 1.587, 1.582, 1.578, 1.573, 1.569, 1.564, 1.560, 1.555,
      # 7.0 - 7.9
      1.551, 1.547, 1.542, 1.538, 1.534, 1.529, 1.525, 1.521, 1.516, 1.512,
      # 8.0 - 8.9
      1.508, 1.504, 1.500, 1.496, 1.491, 1.487, 1.483, 1.479, 1.475, 1.471,
      # 9.0 - 9.9
      1.467, 1.463, 1.459, 1.455, 1.451, 1.447, 1.443, 1.439, 1.436, 1.432,
      # 10.0 - 10.9
      1.428, 1.424, 1.420, 1.416, 1.413, 1.409, 1.405, 1.401, 1.398, 1.394,
      # 11.0 - 11.9
      1.390, 1.387, 1.383, 1.379, 1.376, 1.372, 1.369, 1.365, 1.362, 1.358,
      # 12.0 - 12.9
      1.354, 1.351, 1.347, 1.344, 1.341, 1.337, 1.334, 1.330, 1.327, 1.323,
      # 13.0 - 13.9
      1.320, 1.317, 1.313, 1.310, 1.307, 1.303, 1.300, 1.297, 1.294, 1.290,
      # 14.0 - 14.9
      1.287, 1.284, 1.281, 1.278, 1.274, 1.271, 1.268, 1.265, 1.262, 1.259,
      # 15.0 - 15.9
      1.256, 1.253, 1.249, 1.246, 1.243, 1.240, 1.237, 1.234, 1.231, 1.228,
      # 16.0 - 16.9
      1.225, 1.222, 1.219, 1.216, 1.214, 1.211, 1.208, 1.205, 1.202, 1.199,
      # 17.0 - 17.9
      1.196, 1.193, 1.191, 1.188, 1.185, 1.182, 1.179, 1.177, 1.174, 1.171,
      # 18.0 - 18.9
      1.168, 1.166, 1.163, 1.160, 1.157, 1.155, 1.152, 1.149, 1.147, 1.144,
      # 19.0 - 19.9
      1.141, 1.139, 1.136, 1.134, 1.131, 1.128, 1.126, 1.123, 1.121, 1.118,
      # 20.0 - 20.9
      1.116, 1.113, 1.111, 1.108, 1.105, 1.103, 1.101, 1.098, 1.096, 1.093,
      # 21.0 - 21.9
      1.091, 1.088, 1.086, 1.083, 1.081, 1.079, 1.076, 1.074, 1.071, 1.069,
      # 22.0 - 22.9
      1.067, 1.064, 1.062, 1.060, 1.057, 1.055, 1.053, 1.051, 1.048, 1.046,
      # 23.0 - 23.9
      1.044, 1.041, 1.039, 1.037, 1.035, 1.032, 1.030, 1.028, 1.026, 1.024,
      # 24.0 - 24.9
      1.021, 1.019, 1.017, 1.015, 1.013, 1.011, 1.008, 1.006, 1.004, 1.002,
      # 25.0 - 25.9
      1.000, 0.998, 0.996, 0.994, 0.992, 0.990, 0.987, 0.985, 0.983, 0.981,
      # 26.0 - 26.9
      0.979, 0.977, 0.975, 0.973, 0.971, 0.969, 0.967, 0.965, 0.963, 0.961,
      # 27.0 - 27.9
      0.959, 0.957, 0.955, 0.953, 0.952, 0.950, 0.948, 0.946, 0.944, 0.942,
      # 28.0 - 28.9
      0.940, 0.938, 0.936, 0.934, 0.933, 0.931, 0.929, 0.927, 0.925, 0.923,
      # 29.0 - 29.9
      0.921, 0.920, 0.918, 0.916, 0.914, 0.912, 0.911, 0.909, 0.907, 0.905,
      # 30.0 - 30.9
      0.903, 0.902, 0.900, 0.898, 0.896, 0.895, 0.893, 0.891, 0.889, 0.888,
      # 31.0 - 31.9
      0.886, 0.884, 0.883, 0.881, 0.879, 0.877, 0.876, 0.874, 0.872, 0.871,
      # 32.0 - 32.9
      0.869, 0.867, 0.866, 0.864, 0.863, 0.861, 0.859, 0.858, 0.856, 0.854,
      # 33.0 - 33.9
      0.853, 0.851, 0.850, 0.848, 0.846, 0.845, 0.843, 0.842, 0.840, 0.839,
      # 34.0 - 34.9
      0.837, 0.835, 0.834, 0.832, 0.831, 0.829, 0.828, 0.826, 0.825, 0.823,
      # 35.0 - 35.9
      0.822, 0.820, 0.819, 0.817, 0.816, 0.814, 0.813, 0.811, 0.810, 0.808
    )
  )

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

  # Round spc_uScm_adj to match input conductivity decimal places
  cond_chars <- as.character(stats::na.omit(cond_vals))
  dec_matches <- grep("\\.", cond_chars, value = TRUE)
  dec_places <- max(c(0, nchar(sub(".*\\.", "", dec_matches))))
  out$spc_uScm_adj <- round(out$spc_uScm_adj, digits = dec_places)

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
