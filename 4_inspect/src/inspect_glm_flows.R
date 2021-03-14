# library(targets)
# library(tidyverse)
# source('3_run/src/process_glm_output.R')
# tar_load(c('p1_ltmp_temps.rds','p2_reservoir_ids','p2_meteo','p2_nml_objects','p3_glm','p1_io_res_io_obs.feather'))
# res_id <- p2_reservoir_ids[2]
# sim_res_dir <- file.path('3_run/out/io_evap', res_id)
# plot_id <- sprintf('%s_%s', res_id, basename(dirname(sim_res_dir)))
# nml_obj <- p2_nml_objects[[res_id]]
# out_dir <- '4_inspect/out'


# Plot water balance - total inflows, total outflows, overflows
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

# Assess something (what?) about water balance accuracy, total overflows (don't want those), etc.
assess_water_budget <- function(sim_res_dir, res_id, sim_id) {

  lake_preds <- locate_out_files(sim_res_dir, file_type='lake') %>%
    read_csv(col_types=cols(time = col_character())) %>%
    mutate(date = as.Date(substring(time, 1, 10)))

  # Local Runoff seems to be exactly 0?
  vol_types <- c('Tot Inflow Vol', 'Tot Outflow Vol', 'Overflow Vol', 'Rain', 'Snowfall', 'Local Runoff', 'Evaporation')
  daily_means <- lake_preds %>%
    select(date, all_of(vol_types)) %>%
    summarize_at(.vars=vol_types, mean) %>%
    mutate(
      res_id = res_id,
      res_name = names(res_id),
      sim_id = sim_id,
      .before = 1)

  return(daily_means)
}

# Plot outflow flow predictions and observations
plot_outflows <- function() {

}
# Compute RMSEs of outflow pred vs obs for flow
assess_outflows <- function() {

}

# Plot predicted and observed water levels
plot_lake_levels <- function(sim_res_dir, p1_ltmp_levels.rds, res_id, plot_id, out_dir) {

  nml_obj <- file.path(sim_res_dir, 'glm3.nml') %>% glmtools::read_nml()
  crest_elev <- glmtools::get_nml_value(nml_obj, 'crest_elev')
  lake_depth <- glmtools::get_nml_value(nml_obj, 'lake_depth')
  base_elev <- crest_elev - lake_depth

  lake_preds <- locate_out_files(sim_res_dir, file_type='lake') %>%
    read_csv(col_types=cols(time = col_character())) %>%
    mutate(
      date = as.Date(substring(time, 1, 10)),
      level_pred = `Lake Level` + base_elev)

  lake_obs <- read_rds(p1_ltmp_levels.rds) %>%
    filter(site_id == res_id)

  lake_pred_obs <- left_join(
    select(lake_preds, date, level_pred),
    select(lake_obs, date, level_obs = surface_elevation_m),
    by = c('date')) %>%
    filter(date >= min(lake_obs$date), date <= max(lake_obs$date))

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

# Compute RMSE/NSE of predicted and observed water levels
assess_lake_levels <- function() {

}
