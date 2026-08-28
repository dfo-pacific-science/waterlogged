#' Plot QC time series for logger data
#'
#' Creates a multi-panel plot showing water level (WL), dissolved oxygen (DO),
#' barometric pressure (BARO), conductivity (COND), standalone air temperature
#' (AT), and/or standalone water temperature (WT) time series (raw vs.
#' adjusted), corresponding logger/air temperatures, and QC flags.
#'
#' Each data set is filtered to \code{select_station}, and any available
#' QC flags are shown in separate panels.
#'
#' @param wl_data   Optional data frame of water-level records.
#' @param do_data   Optional data frame of dissolved-oxygen records.
#' @param baro_data Optional data frame of barometric records. If \code{NULL}
#'   and WL data contain barometric columns, those are used.
#' @param cond_data Optional data frame of conductivity records.
#' @param airtemp_data Optional data frame of standalone air temperature
#'   records (e.g. output of \code{\link{qc_airtemp}}). Expected columns:
#'   \code{airtemp_C}, \code{airtemp_C_adj}, and optionally
#'   \code{edit_temp_range}.
#' @param watertemp_data Optional data frame of standalone water temperature
#'   records (e.g. output of \code{\link{qc_watertemp}}). Expected columns:
#'   \code{watertemp_C}, \code{watertemp_C_adj}, and optionally flag/edit
#'   columns \code{flag_ice}, \code{flag_wt_dry}, \code{edit_temp_lt_minus1},
#'   \code{edit_temp_neg_near}.
#' @param select_station Character station code to plot.
#' @param static Logical. If \code{FALSE} (default), returns an interactive
#'   Plotly \code{htmlwidget} with a range-slider for navigation. If
#'   \code{TRUE}, returns a static \code{patchwork} / \code{gg} object
#'   suitable for PDF output. Requires the \pkg{patchwork} package.
#'
#' @return When \code{static = FALSE}, a Plotly \code{htmlwidget}. When
#'   \code{static = TRUE}, a \code{patchwork} object (inherits \code{gg}).
#' @export
#'
#' @examples
#' \dontrun{
#' plot_qc_timeseries(
#'   wl_data   = waterlevel_complete_QAQC,
#'   do_data   = station_do_dat,
#'   baro_data = all_checked_baro_data,
#'   cond_data = station_cond_dat,
#'   select_station = "ALBR_ST_30"
#' )
#'
#' # standalone temperature plots
#' plot_qc_timeseries(
#'   airtemp_data   = qc_airtemp_result,
#'   watertemp_data = qc_watertemp_result,
#'   select_station = "ALBR_ST_30"
#' )
#' }
plot_qc_timeseries <- function(wl_data        = NULL,
                                 do_data        = NULL,
                                 baro_data      = NULL,
                                 cond_data      = NULL,
                                 airtemp_data   = NULL,
                                 watertemp_data = NULL,
                                 select_station,
                                 static = FALSE) {

  cols <- qaqc_plot_cols()

  # ---- normalise / filter each dataset --------------------------------------
  wl_df   <- normalize_qaqc_df(wl_data,        select_station)
  do_df   <- normalize_qaqc_df(do_data,        select_station)
  baro_df <- normalize_qaqc_df(baro_data,      select_station)
  co_df   <- normalize_qaqc_df(cond_data,      select_station)
  at_df   <- normalize_qaqc_df(airtemp_data,   select_station)
  wt_df   <- normalize_qaqc_df(watertemp_data, select_station)

  # If no explicit baro_data, see if WL carries baro columns
  if (is.null(baro_df) && !is.null(wl_df)) {
    needed_baro_cols <- c("airpress_kPa", "airpress_kPa_adj")
    if (all(needed_baro_cols %in% names(wl_df))) {
      extra_cols <- intersect(
        c("airtemp_C", "airtemp_C_adj", "edit_baro_offset",
          "edit_press_high", "edit_press_low", "edit_flatline",
          "edit_spike", "edit_temp_range"),
        names(wl_df)
      )
      baro_df <- wl_df[, c("site_station_code", "timestamp",
                           needed_baro_cols, extra_cols), drop = FALSE]
    }
  }

  if (is.null(wl_df) && is.null(do_df) && is.null(baro_df) &&
      is.null(co_df) && is.null(at_df) && is.null(wt_df)) {
    stop("plot_qc_timeseries(): no data for station '", select_station,
         "' in wl_data, do_data, baro_data, cond_data, airtemp_data, or watertemp_data.")
  }

  has_wl   <- !is.null(wl_df)   && all(c("waterlevel_m", "waterlevel_m_adj") %in% names(wl_df))
  has_do   <- !is.null(do_df)   && all(c("do_mgl", "do_mgl_adj")           %in% names(do_df))
  has_baro <- !is.null(baro_df) && all(c("airpress_kPa", "airpress_kPa_adj") %in% names(baro_df))
  has_co   <- !is.null(co_df)   && all(c("conduct_uScm", "conduct_uScm_adj") %in% names(co_df))
  has_at   <- !is.null(at_df)   && "airtemp_C" %in% names(at_df)
  has_wt   <- !is.null(wt_df)   && "watertemp_C" %in% names(wt_df)

  # x-ranges for each dataset (used so flag panels don't shrink the view)
  if (!is.null(do_df))   do_x_range   <- range(do_df$timestamp,   na.rm = TRUE)
  if (!is.null(wl_df))   wl_x_range   <- range(wl_df$timestamp,   na.rm = TRUE)
  if (!is.null(co_df))   co_x_range   <- range(co_df$timestamp,   na.rm = TRUE)
  if (!is.null(baro_df)) baro_x_range <- range(baro_df$timestamp, na.rm = TRUE)
  if (!is.null(at_df))   at_x_range   <- range(at_df$timestamp,   na.rm = TRUE)
  if (!is.null(wt_df))   wt_x_range   <- range(wt_df$timestamp,   na.rm = TRUE)

  panels  <- list()
  heights <- numeric(0)

  # ---- AIR TEMPERATURE PANELS ------------------------------------------------
  if (has_at) {
    # 1) Air temperature timeseries
    p_at <- ggplot2::ggplot(at_df, ggplot2::aes(x = timestamp))
    at_line_cols <- c()

    p_at <- p_at +
      ggplot2::geom_line(
        ggplot2::aes(y = airtemp_C, colour = "airtemp_raw"),
        alpha = 0.5
      )
    at_line_cols["airtemp_raw"] <- cols$air_temp_raw

    if ("airtemp_C_adj" %in% names(at_df)) {
      p_at <- p_at +
        ggplot2::geom_line(
          ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj")
        )
      at_line_cols["airtemp_adj"] <- cols$air_temp_adj
    }

    p_at <- p_at +
      ggplot2::scale_color_manual(values = at_line_cols) +
      ggplot2::labs(
        title  = paste("Air Temperature QC flags -", select_station),
        y      = "Air temperature (\u00b0C)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()

    panels  <- c(panels, list("p_at"=p_at))
    heights <- c(heights, 0.20)

    # 2) Air temperature QC flags panel
    at_flag_cols <- c("OUTLIER_AT","FLAG_RANGE")
    at_flags     <- build_flag_long(at_df, at_flag_cols)

    if (!is.null(at_flags)) {
      levels_at <- levels(at_flags$flag_type)
      style_at  <- qaqc_flag_style(levels_at)

      p_at_qaqc <- ggplot2::ggplot(
        at_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = flag_type,
          shape  = flag_type,
          colour = flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_at$shape) +
        ggplot2::scale_color_manual(values = style_at$colour) +
        ggplot2::scale_x_datetime(limits = at_x_range) +
        ggplot2::labs(
          title  = paste("Air Temperature QC flags -", select_station),
          x      = "",
          y      = "",
          shape  = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list("p_at_qaqc"=p_at_qaqc))
      heights <- c(heights, 0.10)
    }
  }


  # ---- DO PANELS -------------------------------------------------------------
  if (has_do) {
    # 1) DO timeseries (mg/L)
    p_do <- ggplot2::ggplot(do_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = do_mgl, colour = "do_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = do_mgl_adj, colour = "do_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        do_raw = cols$do_raw,
        do_adj = cols$do_adj
      )) +
      ggplot2::labs(
        title  = paste("Dissolved Oxygen QC flags - ", select_station),
        y      = "DO (mg/L)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()

    panels  <- c(panels, list(p_do))
    heights <- c(heights, 0.22)

    # 2) DO temperature panel
    has_do_temp      <- "watertemp_C_do"      %in% names(do_df)
    has_do_temp_adj  <- "watertemp_C_do_adj"  %in% names(do_df)
    has_air_baro     <- "airtemp_C_baro"      %in% names(do_df)

    if (has_do_temp || has_do_temp_adj || has_air_baro) {
      p_do_temp <- ggplot2::ggplot(do_df, ggplot2::aes(x = timestamp))
      line_cols <- c()

      if (has_do_temp) {
        p_do_temp <- p_do_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = watertemp_C_do, colour = "water_temp"),
            alpha = 0.5
          )
        line_cols["water_temp"] <- cols$do_temp_raw
      }

      if (has_do_temp_adj) {
        p_do_temp <- p_do_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = watertemp_C_do_adj, colour = "water_temp_adj")
          )
        line_cols["water_temp_adj"] <- cols$do_temp_adj
      }

      if (has_air_baro) {
        p_do_temp <- p_do_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = airtemp_C_baro, colour = "airtemp"),
            alpha = 0.5
          )
        line_cols["airtemp"] <- cols$do_air_temp
      }

      # optionally overlay air temperature if present as additional data source
      if (has_at) {
        # use adjusted air temp as priority in overlay
        if("airtemp_C_adj" %in% names(at_df)) {
          p_do_temp <- p_do_temp +
            ggplot2::geom_line(data = at_df,
                               ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj"),
                               alpha = 0.5)
          line_cols["airtemp_adj"] <- cols$air_temp_adj
        }else{
          # if not present then use raw
          p_do_temp <- p_do_temp +
            ggplot2::geom_line(data = at_df,
                               ggplot2::aes(y = airtemp_C, colour = "airtemp"),
                               alpha = 0.5)

          line_cols["airtemp"] <- cols$air_temp_raw
        }
        #drop separate air temp panels, since now overlaid
        panels <- within(panels, rm(p_at, p_at_qaqc))
        heights <- heights[-match(0.2, heights)]
        heights <- heights[-match(0.1, heights)]
      }

      p_do_temp <- p_do_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = "DO logger & air temperature",
          y      = "Temperature (\u00b0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_do_temp))
      heights <- c(heights, 0.18)
    }

    # 3) DO QC flags panel
    do_flag_cols <- c(
      "ERROR_DO",
      "OUTLIER_DO",
      "OUTLIER_WT",
      "FLAG_ICE",
      "FLAG_DRY",
      "FLAG_RANGE"
    )

    do_flags <- build_flag_long(do_df, do_flag_cols)

    if (!is.null(do_flags)) {
      levels_do <- levels(do_flags$flag_type)
      style_do  <- qaqc_flag_style(levels_do)

      p_do_qaqc <- ggplot2::ggplot(
        do_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = flag_type,
          shape  = flag_type,
          colour = flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_do$shape) +
        ggplot2::scale_color_manual(values = style_do$colour) +
        ggplot2::scale_x_datetime(limits = do_x_range) +
        ggplot2::labs(
          title = paste("Dissolved Oxygen QC flags - ", select_station),
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_do_qaqc))
      heights <- c(heights, 0.10)
    }
  }

  # ---- CONDUCTIVITY PANELS ---------------------------------------------------
  if (has_co) {
    # 1) Conductivity timeseries
    p_co <- ggplot2::ggplot(co_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = conduct_uScm, colour = "cond_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = conduct_uScm_adj, colour = "cond_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        cond_raw = cols$cond_raw,
        cond_adj = cols$cond_adj
      )) +
      ggplot2::labs(
        title  = paste("Conductivity -", select_station),
        y      = "Conductivity (\u00b5S/cm)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()

    panels  <- c(panels, list(p_co))
    heights <- c(heights, 0.22)

    # 2) Conductivity temperature panel
    has_co_temp      <- "watertemp_C"      %in% names(co_df)
    has_co_temp_adj  <- "watertemp_C_adj"  %in% names(co_df)
    has_co_airtemp   <- "airtemp_C"        %in% names(co_df)

    if (has_co_temp || has_co_temp_adj || has_co_airtemp) {
      p_co_temp <- ggplot2::ggplot(co_df, ggplot2::aes(x = timestamp))
      line_cols <- c()

      if (has_co_temp) {
        p_co_temp <- p_co_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = watertemp_C, colour = "water_temp"),
            alpha = 0.5
          )
        line_cols["water_temp"] <- cols$cond_temp
      }

      if (has_co_temp_adj) {
        p_co_temp <- p_co_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = watertemp_C_adj, colour = "water_temp_adj")
          )
        line_cols["water_temp_adj"] <- cols$cond_temp_adj
      }

      if (has_co_airtemp) {
        p_co_temp <- p_co_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = airtemp_C, colour = "airtemp"),
            alpha = 0.5
          )
        line_cols["airtemp"] <- cols$cond_air_temp
      }

      # optionally overlay air temperature if present as additional data source
      if (has_at) {
        # use adjusted air temp as priority in overlay
        if("airtemp_C_adj" %in% names(at_df)) {
          p_co_temp <- p_co_temp +
            ggplot2::geom_line(data = at_df,
                               ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj"),
                               alpha = 0.5)
          line_cols["airtemp_adj"] <- cols$air_temp_adj
        }else{
          # if not present then use raw
          p_co_temp <- p_co_temp +
            ggplot2::geom_line(data = at_df,
                               ggplot2::aes(y = airtemp_C, colour = "airtemp"),
                               alpha = 0.5)

          line_cols["airtemp"] <- cols$air_temp_raw
        }

        #drop separate air temp panels, since now overlaid
        panels <- within(panels, rm(p_at, p_at_qaqc))
        heights <- heights[-match(0.2, heights)]
        heights <- heights[-match(0.1, heights)]

      }

      p_co_temp <- p_co_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = "Conductivity logger temperature",
          y      = "Temperature (\u00b0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_co_temp))
      heights <- c(heights, 0.18)
    }

    # 3) Conductivity QC flags panel
    co_flag_cols <- c(
      "FLAG_SPIKE",
      "FLAG_ICE",
      "FLAG_DRY",
      "FLAG_DRY_WT",
      "OUTLIER_WT",
      "OUTLIER_CO"
    )
    co_flags <- build_flag_long(co_df, co_flag_cols)

    if (!is.null(co_flags)) {
      levels_co <- levels(co_flags$flag_type)
      style_co  <- qaqc_flag_style(levels_co)

      p_co_qaqc <- ggplot2::ggplot(
        co_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = flag_type,
          shape  = flag_type,
          colour = flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_co$shape) +
        ggplot2::scale_color_manual(values = style_co$colour) +
        ggplot2::scale_x_datetime(limits = co_x_range) +
        ggplot2::labs(
          title  = "Conductivity QC flags",
          x      = "",
          y      = "",
          shape  = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_co_qaqc))
      heights <- c(heights, 0.10)
    }
  }

  # ---- WATER LEVEL PANELS -------------------------------------------------------------
  if (has_wl) {
    # WL time series
    p_wl <- ggplot2::ggplot(wl_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = waterlevel_m, colour = "wl_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = waterlevel_m_adj, colour = "wl_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        wl_raw = cols$wl_raw,
        wl_adj = cols$wl_adj
      )) +
      ggplot2::labs(
        title  = paste("Water level -", select_station),
        y      = "Water level (m)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()

    panels  <- c(panels, list(p_wl))
    heights <- c(heights, 0.20)

    # WL temperature panel
    p_wl_temp <- ggplot2::ggplot(wl_df, ggplot2::aes(x = timestamp))
    line_cols <- c()

    if ("watertemp_C" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = watertemp_C, colour = "water_temp"),
          alpha = 0.5
        )
      line_cols["water_temp"] <- cols$wl_temp
    }
    if ("watertemp_C_adj" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = watertemp_C_adj, colour = "water_temp_adj")
        )
      line_cols["water_temp_adj"] <- cols$wl_temp_adj
    }
    if ("airtemp_C" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = airtemp_C, colour = "airtemp"),
          alpha = 0.5
        )
      line_cols["airtemp"] <- cols$wl_air_temp
    }
    if ("airtemp_C_adj" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj")
        )
      line_cols["airtemp_adj"] <- cols$baro_air_adj
    }
    # optionally overlay air temperature if present as additional data source
    if (has_at) {
      # use adjusted air temp as priority in overlay
      if("airtemp_C_adj" %in% names(at_df)) {
        p_wl_temp <- p_wl_temp +
          ggplot2::geom_line(data = at_df,
                             ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj"),
                             alpha = 0.5)
        line_cols["airtemp_adj"] <- cols$air_temp_adj
      }else{
        # if not present then use raw
        p_wl_temp <- p_wl_temp +
          ggplot2::geom_line(data = at_df,
                             ggplot2::aes(y = airtemp_C, colour = "airtemp"),
                             alpha = 0.5)

        line_cols["airtemp"] <- cols$air_temp_raw
      }
      #drop separate air temp panels, since now overlaid
      panels <- within(panels, rm(p_at, p_at_qaqc))
      heights <- heights[-match(0.2, heights)]
      heights <- heights[-match(0.1, heights)]
    }

    if (length(line_cols) > 0) {
      p_wl_temp <- p_wl_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = "WL logger temperatures",
          y      = "Temperature (\u00b0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_wl_temp))
      heights <- c(heights, 0.18)
    }

    # WL flags
    wl_flag_cols <- c(
      "FLAG_SPIKE",
      "FLAG_ICE",
      "OUTLIER_WT",
      "FLAG_DRY"
    )

    wl_flags <- build_flag_long(wl_df, wl_flag_cols)

    if (!is.null(wl_flags)) {
      levels_wl <- levels(wl_flags$flag_type)
      style_wl  <- qaqc_flag_style(levels_wl)

      p_wl_qaqc <- ggplot2::ggplot(
        wl_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = flag_type,
          shape  = flag_type,
          colour = flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_wl$shape) +
        ggplot2::scale_color_manual(values = style_wl$colour) +
        ggplot2::scale_x_datetime(limits = wl_x_range) +
        ggplot2::labs(
          title = paste("Water Level QC flags - ", select_station),
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_wl_qaqc))
      heights <- c(heights, 0.10)
    }
  }

  # ---- BARO PANELS -----------------------------------------------------------
  if (has_baro) {
    # Air temperature panel
    has_baro_air     <- "airtemp_C"     %in% names(baro_df)
    has_baro_air_adj <- "airtemp_C_adj" %in% names(baro_df)

    if (has_baro_air || has_baro_air_adj) {
      p_baro_temp <- ggplot2::ggplot(baro_df, ggplot2::aes(x = timestamp))
      line_cols   <- c()

      if (has_baro_air) {
        p_baro_temp <- p_baro_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = airtemp_C, colour = "airtemp"),
            alpha = 0.6
          )
        line_cols["airtemp"] <- cols$baro_air_raw
      }

      if (has_baro_air_adj) {
        p_baro_temp <- p_baro_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj"),
            alpha = 0.9
          )
        line_cols["airtemp_adj"] <- cols$baro_air_adj
      }

      p_baro_temp <- p_baro_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = paste("Barometric air temperature -", select_station),
          y      = "Air temperature (\u00b0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_baro_temp))
      heights <- c(heights, 0.18)
    }

    # Air pressure panel
    p_baro_press <- ggplot2::ggplot(baro_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = airpress_kPa, colour = "press_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = airpress_kPa_adj, colour = "press_adj"),
        alpha = 0.9
      ) +
      ggplot2::scale_color_manual(values = c(
        press_raw = cols$baro_p_raw,
        press_adj = cols$baro_p_adj
      )) +
      ggplot2::labs(
        title  = paste("Barometric air temperature -", select_station),
        y      = "Air pressure (kPa)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()

    panels  <- c(panels, list(p_baro_press))
    heights <- c(heights, 0.20)

    # BARO flags panel — prefer modern boolean columns; fall back to legacy code column
    baro_bool_cols <- c("OUTLIER_AP",
                        "OUTLIER_AT",
                        "FLAG_FLATLINE",
                        "FLAG_SPIKE")
    baro_flags     <- build_flag_long(baro_df, baro_bool_cols)
    use_code_col   <- FALSE
    if (is.null(baro_flags)) {
      baro_flags   <- build_qaqc_code_points(baro_df, "edit_baro_offset")
      use_code_col <- !is.null(baro_flags)
    }

    if (!is.null(baro_flags)) {
      flag_aes <- if (use_code_col) "code" else "flag_type"
      flag_sym <- rlang::sym(flag_aes)
      levels_baro <- levels(baro_flags[[flag_aes]])
      style_baro  <- qaqc_flag_style(levels_baro)

      p_baro_qaqc <- ggplot2::ggplot(
        baro_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = !!flag_sym,
          shape  = !!flag_sym,
          colour = !!flag_sym
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_baro$shape) +
        ggplot2::scale_color_manual(values = style_baro$colour) +
        ggplot2::scale_x_datetime(limits = baro_x_range) +
        ggplot2::labs(
          title  = paste("Barometric Pressure QC flags -", select_station),
          x      = "Timestamp",
          y      = "",
          shape  = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_baro_qaqc))
      heights <- c(heights, 0.10)
    }
  }

  # ---- WATER TEMPERATURE PANELS ----------------------------------------------
  if (has_wt) {
    # 1) Water temperature timeseries
    p_wt <- ggplot2::ggplot(wt_df, ggplot2::aes(x = timestamp))
    wt_line_cols <- c()

    p_wt <- p_wt +
      ggplot2::geom_line(
        ggplot2::aes(y = watertemp_C, colour = "watertemp_raw"),
        alpha = 0.5
      )
    wt_line_cols["watertemp_raw"] <- cols$water_temp_raw

    if ("watertemp_C_adj" %in% names(wt_df)) {
      p_wt <- p_wt +
        ggplot2::geom_line(
          ggplot2::aes(y = watertemp_C_adj, colour = "watertemp_adj")
        )
      wt_line_cols["watertemp_adj"] <- cols$water_temp_adj
    }

    # optionally overlay air temperature if present
    if ("airtemp_C" %in% names(wt_df)) {
      p_wt <- p_wt +
        ggplot2::geom_line(
          ggplot2::aes(y = airtemp_C, colour = "airtemp"),
          alpha = 0.5
        )
      wt_line_cols["airtemp"] <- cols$air_temp_raw
    }

    # optionally overlay air temperature if present as additional data source

    if (has_at) {
      # use adjusted air temp as priority in overlay
      if("airtemp_C_adj" %in% names(at_df)) {
        p_wt <- p_wt +
          ggplot2::geom_line(data = at_df,
                             ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_adj"),
                             alpha = 0.5)
        wt_line_cols["airtemp_adj"] <- cols$air_temp_adj
      }else{
        # if not present then use raw
        p_wt <- p_wt +
          ggplot2::geom_line(data = at_df,
                             ggplot2::aes(y = airtemp_C, colour = "airtemp"),
                             alpha = 0.5)

        wt_line_cols["airtemp"] <- cols$air_temp_raw
      }

      #drop separate air temp panels, since now overlaid
      panels <- within(panels, rm(p_at, p_at_qaqc))
      heights <- heights[-match(0.2, heights)]
      heights <- heights[-match(0.1, heights)]

    }

    p_wt <- p_wt +
      ggplot2::scale_color_manual(values = wt_line_cols) +
      ggplot2::labs(
        title  = paste("Water Temperature QC flags - ", select_station),
        y      = "Water temperature (\u00b0C)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()

    panels  <- c(panels, list(p_wt))
    heights <- c(heights, 0.20)

    # 2) Water temperature QC flags panel
    wt_flag_cols <- c(
      "FLAG_ICE",
      "FLAG_DRY",
      "OUTLIER_WT",
      "edit_temp_neg_near",
      "FLAG_RANGE"
    )
    wt_flags <- build_flag_long(wt_df, wt_flag_cols)

    if (!is.null(wt_flags)) {
      levels_wt <- levels(wt_flags$flag_type)
      style_wt  <- qaqc_flag_style(levels_wt)

      p_wt_qaqc <- ggplot2::ggplot(
        wt_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = flag_type,
          shape  = flag_type,
          colour = flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_wt$shape) +
        ggplot2::scale_color_manual(values = style_wt$colour) +
        ggplot2::scale_x_datetime(limits = wt_x_range) +
        ggplot2::labs(
          title  = paste("Water Temperature QC flags - ", select_station),
          x      = "",
          y      = "",
          shape  = "",
          colour = ""
        ) +
        ggplot2::theme_classic()

      panels  <- c(panels, list(p_wt_qaqc))
      heights <- c(heights, 0.10)
    }
  }

  # ---- combine panels --------------------------------------------------------
  if (!length(panels)) {
    stop("plot_qc_timeseries(): no panels were built.")
  }

  heights <- heights / sum(heights)

  if (static) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("plot_qc_timeseries(static = TRUE) requires the 'patchwork' ",
           "package. Install it with install.packages('patchwork').")
    }
    patchwork::wrap_plots(panels, ncol = 1, heights = heights)
  } else {
    plotly_panels <- lapply(panels, function(p) {
      suppressWarnings(plotly::ggplotly(p))
    })
    plotly::subplot(
      plotly_panels,
      nrows   = length(plotly_panels),
      shareX  = TRUE,
      heights = heights,
      margin  = 0.03
    ) |>
      plotly::layout(
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type        = "date"
        ),
        plot_bgcolor = "#e5ecf6"
      ) |>
      htmlwidgets::onRender("
  function(el) {
    el.on('plotly_relayout', function(eventData) {
      // Check if the slider moved and modified the x-axis range
      if (eventData['xaxis.range[0]'] !== undefined) {
        var xMin = new Date(eventData['xaxis.range[0]']);
        var xMax = new Date(eventData['xaxis.range[1]']);

        // Target the first data trace
        var trace = el._fullData[0];
        var visibleY = [];

        // Loop over data points to find what is currently visible on screen
        for (var i = 0; i < trace.x.length; i++) {
          var currentX = new Date(trace.x[i]);
          if (currentX >= xMin && currentX <= xMax) {
            visibleY.push(trace.y[i]);
          }
        }

        // If we found visible points, recalculate min/max and update layout
        if (visibleY.length > 0) {
          var yMin = Math.min.apply(null, visibleY);
          var yMax = Math.max.apply(null, visibleY);

          // Add a small 5% padding padding so lines don't hug the graph edges
          var padding = (yMax - yMin) * 0.05;

          Plotly.relayout(el, {
            'yaxis.range': [yMin - padding, yMax + padding]
          });
        }
      }

      // Reset y-axis if the user double-clicks to reset the slider entirely
      if (eventData['xaxis.autorange'] === true) {
        Plotly.relayout(el, { 'yaxis.autorange': true });
      }
    });
  }
")


  }
}
