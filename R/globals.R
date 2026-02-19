# Suppress R CMD check NOTEs for column names used in NSE (dplyr/tidyr pipes)
# and rlang operators (.data, :=)
utils::globalVariables(c(
  ".data", ":=",
  # Column names used across multiple functions
  "action", "airpress_kPa", "airpress_kPa_adj",
  "airtemp_C", "airtemp_C_U20", "airtemp_C_adj", "airtemp_C_baro",
  "at_qaqc_code",
  "baro_code", "baro_lat", "baro_lon", "baro_qaqc_adj", "baro_qaqc_code",
  "baro_qaqc_note", "baro_sensor_depth_m", "baro_site_stn_code",
  "c_drift", "c_offset_diff", "cal_deviation",
  "co_qaqc_adj", "co_qaqc_code", "co_qaqc_note",
  "code", "col_index", "coef",
  "conduct_uScm", "conduct_uScm_adj", "conduct_uScm_dc",
  "conductivity_uScm_adj",
  "density_kgm3", "density_lbft3", "dist_m",
  "do_1atm", "do_mgl", "do_mgl_adj", "do_percsat_adj", "do_sat_real",
  "edit_na", "edit_offset", "edit_spike_flat",
  "F_p", "field", "flag_dry_temp", "flag_dry_wl", "flag_type", "fun_name",
  "gap_end", "gap_start",
  "in_event", "in_window", "interpolated_value", "is_dry",
  "latitude", "longitude",
  "metric", "metric_norm",
  "parsed", "per_c_offset", "per_t_offset", "press_atm",
  "raw_header", "ref_conduct_uScm", "ref_m", "ref_time_intv",
  "ref_timestamp", "ref_watertemp_C", "row_index", "run_at",
  "sensor_depth_m", "site_code", "site_station_code", "sn",
  "t_drift", "t_offset_diff", "temp", "temp_K", "Temp_C",
  "theta_o", "time_elapsed", "time_ratio",
  "timestamp", "timestamp_deploy", "ts_end", "ts_start", "Tsub",
  "u_atm", "valid", "visit_dt",
  "waterlevel_m", "waterlevel_m_adj", "waterpress_kPa",
  "watertemp_C", "watertemp_C_adj", "watertemp_C_do", "watertemp_C_do_adj",
  "wl_diff", "wl_lead", "wt_air_close", "wt_air_warmer", "wt_high_dry",
  "year",
  # Internal helper functions referenced indirectly (e.g. passed to purrr::map)
  "build_plots", "normalise_temporal_scale", "read_logger_file"
))
