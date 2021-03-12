
# tar_load(c('p1_ltmp_temps.rds','p2_reservoir_ids','p2_meteo','p2_nml_objects','p3_glm','p1_io_res_io_obs.feather'))
# sim_dir <- '3_run/out/no_io'
# res_id <- p2_reservoir_ids[2]
# out_dir <- '4_inspect/out'


# Plot water balance - total inflows, total outflows, overflows
# Plot outflow flow predictions and observations
# Compute RMSE/NSE of predicted and observed water levels
# Compute RMSEs of outflow pred vs obs for flow

# Plot predicted and observed water levels
plot_lake_levels <- function(sim_dir, nml_obj, p1_ltmp_levels.rds, res_id, plot_id, out_dir) {

  crest_elev <- glmtools::get_nml_value(nml_obj, 'crest_elev')
  lake_depth <- glmtools::get_nml_value(nml_obj, 'lake_depth')
  base_elev <- crest_elev - lake_depth

  lake_preds <- locate_out_files(sim_dir, nml_obj, file_type='lake') %>%
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

