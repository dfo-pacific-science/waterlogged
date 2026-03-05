#' Colour palette for QA/QC time series plots
#'
#' Centralized colour definitions used by \code{plot_qaqc_timeseries()}.
#' Uses the NTSERP colour-blind-friendly palette.
#'
#' @return A named list of hex colour codes.
#' @keywords internal
#' @noRd
qaqc_plot_cols <- function() {
  # NTSERP Palette (colour-blind friendly)
  col_black   <- "#000000" # Black (conductivity)
  col_indigo  <- "#233d4d" # Japanese Indigo (water level)
  col_brown   <- "#915e3d" # Mushroom Leather (barometric pressure)
  col_yellow  <- "#FDB840" # Fire (air temperature)
  col_ltgreen <- "#a1c181" # Iris Green (dissolved oxygen)
  col_green   <- "#619b8a" # Dull Sea Green (water temperature)

  list(
    # DO (raw vs adj differ by alpha in geoms)
    do_raw       = col_ltgreen,
    do_adj       = col_ltgreen,
    do_temp_raw  = col_green,
    do_temp_adj  = col_green,
    do_air_temp  = col_yellow,

    # Conductivity (raw vs adj differ by alpha in geoms)
    cond_raw      = col_black,
    cond_adj      = col_black,
    cond_temp     = col_green,
    cond_temp_adj = col_green,
    cond_air_temp = col_yellow,

    # Water level (raw vs adj differ by alpha in geoms)
    wl_raw       = col_indigo,
    wl_adj       = col_indigo,
    wl_temp      = col_green,
    wl_temp_adj  = col_green,
    wl_air_temp  = col_yellow,

    # Barometric (raw vs adj differ by alpha in geoms)
    baro_air_raw = col_yellow,
    baro_air_adj = col_yellow,
    baro_p_raw   = col_brown,
    baro_p_adj   = col_brown,

    # Air Temperature (raw vs adj differ by alpha in geoms)
    air_temp_raw = col_yellow,
    air_temp_adj = col_yellow,

    # Water Temperature (raw vs adj differ by alpha in geoms)
    water_temp_raw = col_green,
    water_temp_adj = col_green
  )
}



#' Shapes & colours for QA/QC flags
#'
#' All flags are plotted in black with unique shapes to distinguish
#' flag types without introducing colour confusion with the time series
#' lines.
#'
#' @param levels_vec Character vector of flag names.
#'
#' @return A list with named vectors: $shape and $colour
#' @keywords internal
#' @noRd
qaqc_flag_style <- function(levels_vec) {

  # Strongly distinct shapes
  base_shapes <- c(
    16, # filled circle
    17, # filled triangle
    15, # filled square
    3,  # plus
    4,  # x
    7,  # diamond
    8   # star
  )

  n <- length(levels_vec)
  cols   <- rep("#000000",    length.out = n)
  shapes <- rep(base_shapes,  length.out = n)

  names(cols)   <- levels_vec
  names(shapes) <- levels_vec

  list(shape = shapes, colour = cols)
}

