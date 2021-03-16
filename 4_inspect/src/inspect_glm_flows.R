# Code for developing a new inspection function:
# library(targets)
# library(tidyverse)
# source('3_run/src/process_glm_output.R')
# res_id <- p2_reservoir_ids[1]
# sim_res_dir <- file.path('3_run/out/210314c_io_evap', res_id)
# plot_id <- sprintf('%s_%s', res_id, basename(dirname(sim_res_dir)))
# out_dir <- 'tmp'

#' Plot water balance - total inflows, total outflows, overflows
plot_water_budget <- function(sim_res_dir, res_id, plot_id, out_dir) {

  lake_preds <- locate_out_files(sim_res_dir, file_type='lake') %>%
    read_csv(col_types=cols(time = col_character())) %>%
    mutate(date = as.Date(substring(time, 1, 10)))

  # Local Runoff seems to be exactly 0?
  vol_types <- c('Tot Inflow Vol', 'Tot Outflow Vol', 'Overflow Vol', 'Rain', 'Snowfall', 'Local Runoff', 'Evaporation')
  lake_preds %>%
    select(date, all_of(vol_types)) %>%
    mutate_at(.vars=vol_types, cumsum) %>%
    pivot_longer(cols=all_of(vol_types), names_to='Type', values_to='CumSumVol') %>%
    mutate(Type = ordered(Type, levels=vol_types)) %>%
    ggplot(aes(x=date, y=CumSumVol, color=Type)) + geom_line() +
    scale_color_discrete() +
    theme_bw() +
    xlab('Date') +
    ylab('Cumulative Volume') +
    ggtitle(sprintf('Water budget for %s (%s)', names(res_id), res_id))

  # Save the plot
  out_file <- file.path(out_dir, sprintf('water_budget_%s.png', plot_id))
  ggsave(out_file, width=6, height=4)
  return(out_file)
}

add_id_cols <- function(df, res_id, sim_id) {
  df %>% mutate(
    res_id = res_id,
    res_name = names(res_id),
    sim_id = sim_id,
    .before = 1)
}

#' Summarize water budget components as mean daily flows (m^3 / s)
assess_water_budget <- function(sim_res_dir, res_id, sim_id) {

  lake_preds <- locate_out_files(sim_res_dir, file_type='lake') %>%
    read_csv(col_types=cols(time = col_character())) %>%
    mutate(date = as.Date(substring(time, 1, 10)))

  # Local Runoff seems to be exactly 0?
  vol_types <- c('Tot Inflow Vol', 'Tot Outflow Vol', 'Overflow Vol', 'Rain', 'Snowfall', 'Local Runoff', 'Evaporation')
  daily_means <- lake_preds %>%
    select(date, all_of(vol_types)) %>%
    summarize_at(.vars=vol_types, mean) %>%
    add_id_cols(res_id, sim_id)

  return(daily_means)
}

#' Compute residence time
assess_residence_time <- function(sim_res_dir, res_id, sim_id) {

  lake_preds <- locate_out_files(sim_res_dir, file_type='lake') %>%
    read_csv(col_types=cols(time = col_character())) %>%
    mutate(date = as.Date(substring(time, 1, 10)))

  # residence time is total volume divided by total inflow or outflow rate. I
  # believe Volume is in units of m^3 and Tot Xflow Vol are in units of m^3/d
  years_per_day <- 1/365.25
  residence_times_days <- lake_preds %>%
    mutate(water_year = lubridate::year(date - as.difftime(61, units='days'))) %>%
    group_by(water_year) %>%
    summarize(
      n_obs_yr = n(),
      lake_vol = mean(Volume),
      inflow_vol = mean(`Tot Inflow Vol`),
      outflow_vol = mean(`Tot Outflow Vol`),
      .groups = 'drop') %>%
    filter(n_obs_yr > 364) %>%
    transmute(
      res_time_meanflow_yr = (lake_vol / (0.5*(inflow_vol + outflow_vol))) * years_per_day
    ) %>%
    summarize_at(
      .vars=vars(starts_with('res_time')),
      .funs=list(
        min_res_time_yr = min, mean_res_time_yr = mean,
        median_res_time_yr = median, max_res_time_yr = max)) %>%
    add_id_cols(res_id, sim_id)
}

#' Plot predicted and observed water levels
plot_lake_levels <- function(sim_res_dir, obs_res_levels, res_id, plot_id, out_dir) {

  lake_pred_obs <- get_level_pred_obs(sim_res_dir, obs_res_levels)

  ggplot(lake_pred_obs, aes(x=date)) +
    geom_line(aes(y=level_pred)) +
    geom_point(aes(y=level_obs)) +
    theme_bw() +
    ylab('Surface elevation (m)') +
    xlab('Date') +
    ggtitle(sprintf('Preds (lines) and obs (points) for %s (%s)', names(res_id), res_id))

  # Save the plot
  out_file <- file.path(out_dir, sprintf('lake_levels_%s.png', plot_id))
  ggsave(out_file, width=6, height=4)
  return(out_file)
}

#' Combine predictions and observations of lake levels
get_level_pred_obs <- function(sim_res_dir, obs_res_levels) {

  nml_obj <- locate_in_files(sim_res_dir, 'nml') %>% glmtools::read_nml()
  base_elev <- glmtools::get_nml_value(nml_obj, 'crest_elev') -
    glmtools::get_nml_value(nml_obj, 'lake_depth')

  lake_preds <- locate_out_files(sim_res_dir, file_type='lake') %>%
    read_csv(col_types=cols(time = col_character())) %>%
    mutate(
      date = as.Date(substring(time, 1, 10)),
      level_pred = `Lake Level` + base_elev)

  lake_pred_obs <- left_join(
    select(lake_preds, date, level_pred),
    select(obs_res_levels, date, level_obs = surface_elevation_m),
    by = c('date')) %>%
    filter(date >= min(obs_res_levels$date), date <= max(obs_res_levels$date))

  return(lake_pred_obs)
}

#' Compute RMSE/NSE of predicted and observed water levels
assess_lake_levels <- function(sim_res_dir, obs_res_levels, res_id, sim_id) {

  lake_pred_obs <- get_level_pred_obs(sim_res_dir, obs_res_levels)

  lake_pred_obs %>%
    filter(!is.na(level_obs)) %>%
    summarize(
      min_date = min(date),
      max_date = max(date),
      n_obs = length(level_obs),
      n_obs_per_day = n_obs / (1 + as.numeric(max_date - min_date, units='days')),
      RMSE = sqrt(mean((level_pred - level_obs)^2)),
      NSE = 1 - sum((level_pred - level_obs)^2) / sum((level_obs - mean(level_obs))^2),
      .groups = 'drop') %>%
    add_id_cols(res_id, sim_id)
}
