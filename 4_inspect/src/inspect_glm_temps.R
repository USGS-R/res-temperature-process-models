
# tar_load(c('p1_ltmp_temps.rds','p2_reservoir_ids','p2_meteo','p2_nml_objects','p3_glm','p1_io_res_io_obs.feather'))
# res_id <- p2_reservoir_ids[2]
# sim_res_dir <- file.path('3_run/out/no_io', res_id)
# out_dir <- '4_inspect/out'

# Plot temperature predictions - heat map for all depths, with reservoir observations for comparison
plot_temps_all_depths <- function(sim_res_dir, plot_id, out_dir) {
  out_file <- file.path(out_dir, sprintf('temps_all_depths_%s.png', plot_id))
  glmtools::plot_temp(
    locate_out_files(sim_res_dir, file_type='depthwise'),
    reference = 'surface',
    fig_path = out_file,
    width=1100, height=400, res=150, units='px')
  return(out_file)
}

plot_temps_all_depths_obspred <- function(sim_res_dir, obs_res_temps, plot_id, out_dir) {
  out_file <- file.path(out_dir, sprintf('temps_all_depths_obspred_%s.png', plot_id))
  nc_file <- locate_out_files(sim_res_dir, file_type='depthwise')
  obs_tmp_tsv <- write_tsv_tempfile(obs_res_temps)
  glmtools::plot_temp_compare(
    locate_out_files(sim_res_dir, file_type='depthwise'),
    field_file = obs_tmp_tsv,
    fig_path = out_file)
  return(out_file)
}

#' Write a data.frame to tsv. This is useful for preparing temperature
#' observations, which glmtools can resample_to_field once they're written as a
#' tsv file
write_tsv_tempfile <- function(dat) {
  obs_tsv <- tempfile(fileext = '.tsv')
  write_tsv(dat, obs_tsv)
  return(obs_tsv)
}

#' Compute RMSE of reservoir pred vs obs temperatures, all depths
assess_temps_all_depths <- function(sim_res_dir, obs_res_temps, res_id, sim_id) {
  temp_matchups <- get_temps_sensor_depths(sim_res_dir, obs_res_temps)

  temp_matchups %>%
    filter(!is.na(Observed_temp), !is.na(Modeled_temp)) %>%
    summarize(
      min_date = min(DateTime),
      max_date = max(DateTime),
      n_obs = length(Observed_temp),
      n_obs_per_day = n_obs / (1 + as.numeric(max_date - min_date, units='days')),
      RMSE = sqrt(mean((Modeled_temp - Observed_temp)^2)),
      NSE = 1 - sum((Modeled_temp - Observed_temp)^2) / sum((Observed_temp - mean(Observed_temp))^2),
      .groups = 'drop') %>%
    mutate(
      res_id = res_id,
      res_name = names(res_id),
      sim_id = sim_id,
      .before = 1)
}

#' Compare predictions to observations on specific dates and times
get_temps_sensor_depths <- function(sim_res_dir, obs_res_temps) {
  nc_file <- locate_out_files(sim_res_dir, file_type='depthwise')
  obs_tmp_tsv <- write_tsv_tempfile(obs_res_temps)
  temp_matchups <- glmtools::resample_to_field(
    nc_file,
    obs_tmp_tsv,
    var_name = 'temp') %>%
    group_by(DateTime) %>%
    mutate(depth_rank = ordered(3 - length(Depth) + order(Depth), levels=1:3)) %>% # 1,2,3 when n=3; 2,3 otherwise (because the first depth category is the one that gets cut off when water levels drop)
    ungroup()
  return(temp_matchups)
}

#' Plot temperature predictions - time series for the three depth monitoring categories
plot_temps_sensor_depths <- function(sim_res_dir, obs_res_temps, res_id, plot_id, out_dir) {

  temp_matchups <- get_temps_sensor_depths(sim_res_dir, obs_res_temps)

  # Plot preds and obs
  temp_matchups %>%
    ggplot(aes(x=DateTime, color=depth_rank)) +
    geom_point(aes(y=Observed_temp), na.rm=TRUE) +
    geom_line(aes(y=Modeled_temp), size=1, na.rm=TRUE) +
    scale_color_manual('Depth Rank', values=RColorBrewer::brewer.pal(3, "Set2")) +
    theme_classic() +
    xlab('Date') +
    ylab('Temperature (deg C)') +
    xlim(as.POSIXct('2019-08-01'), as.POSIXct('2021-01-01')) +
    ylim(0, 28) +
    ggtitle(sprintf('Preds (lines) and obs (points) for %s (%s)', names(res_id), res_id))

  # Save the plot
  out_file <- file.path(out_dir, sprintf('temps_sensor_depths_%s.png', plot_id))
  ggsave(out_file, width = 7, height = 5)
  return(out_file)
}

#' Compute RMSE by depth category
assess_temps_sensor_depths <- function(sim_res_dir, obs_res_temps, res_id, sim_id) {

  temp_matchups <- get_temps_sensor_depths(sim_res_dir, obs_res_temps)

  temp_matchups %>%
    filter(!is.na(Modeled_temp), !is.na(Observed_temp)) %>%
    group_by(depth_rank) %>%
    summarize(
      min_date = min(DateTime),
      max_date = max(DateTime),
      n_obs = length(Observed_temp),
      n_obs_per_day = n_obs / (1 + as.numeric(max_date - min_date, units='days')),
      min_depth = min(Depth),
      max_depth = max(Depth),
      RMSE = sqrt(mean((Modeled_temp - Observed_temp)^2)),
      NSE = 1 - sum((Modeled_temp - Observed_temp)^2) / sum((Observed_temp - mean(Observed_temp))^2),
      .groups = 'drop') %>%
    add_id_cols(res_id, sim_id)

}

#' Plot temperature predictions - time series for ~three constant depths

#' Plot temperature predictions - time series for the outlet depths
plot_temps_outlet_depths <- function(sim_res_dir, plot_id, out_dir) {
}

#' Compute expected total flow and temperature for the combined outflows
#' Plot outflow temperature predictions and observations
#' Compute RMSEs of outflow pred vs obs for temperature




# analyze_res <- function(
#   model_log = p3_glm,
#   sim_dir = 'tmp/io',
#   all_io_obs = arrow::read_feather(p1_io_res_io_obs.feather),
#   all_res_obs = read_rds(p1_ltmp_temps.rds),
#   res_id = p2_reservoir_ids[2]) {
#
#   # read the extracted output file (fixed depths)
#   temp_preds <- model_log %>%
#     filter(site_id == !!res_id) %>%
#     pull(export_fl) %>%
#     arrow::read_feather()
#
#   # plot temperatures at a few fixed depths over time
#   preds_long <- temp_preds %>%
#     select(-ice) %>%
#     pivot_longer(names_to='temp_depth', values_to='wtemp', -time) %>%
#     mutate(depth = as.numeric(gsub('temp_', '', temp_depth))) %>%
#     select(-temp_depth)
#   preds_long %>%
#     filter(depth %in% c(0, 10, 40)) %>%
#     mutate(Depth = as.factor(depth)) %>%
#     ggplot(aes(x=time, y=wtemp, color=Depth)) +
#     geom_line() +
#     theme_classic()
#
#   # outflow_obs <-
#   #   preds_long %>% filter(depth %in% c(0, 39)) %>% # 39 is approx cannonsville outflow depth
#
#   # Compare predictions to observations on specific dates and times
#   res_obs <- all_res_obs %>%
#     filter(site_id == res_id) %>%
#     select(datetime = date, depth, temp)
#   obs_tsv <- sprintf('tmp/res_obs_%s.tsv', res_id)
#   write_tsv(obs_data, obs_tsv)
#
#   nc_file <- sprintf(
#     '%s/%s/%s/%s.nc',
#     sim_dir,
#     res_id,
#     get_nml_value(p2_nml_objects[[res_id]], 'out_dir'),
#     get_nml_value(p2_nml_objects[[res_id]], 'out_fn'))
#
#   temp_matchups <- resample_to_field(
#     nc_file,
#     obs_tsv,
#     var_name = 'temp') %>%
#     group_by(DateTime) %>%
#     mutate(depth_rank = ordered(3 - length(Depth) + order(Depth), levels=1:3)) %>% # 1,2,3 when n=3; 2,3 otherwise (because the first depth category is the one that gets cut off when water levels drop)
#     ungroup()
#   temp_matchups %>%
#     #filter(depth_rank == 1) %>%
#     select(-Depth) %>%
#     ggplot(aes(x=DateTime, color=depth_rank)) +
#     geom_point(aes(y=Observed_temp)) +
#     geom_line(aes(y=Modeled_temp), size=1) +
#     scale_color_manual(values=RColorBrewer::brewer.pal(3, "Set2")) +
#     theme_classic() +
#     ylab('Temperature (deg C)') +
#     ggtitle(sprintf('Preds (lines) and obs (points) for %s', res_id))
#   ggsave(sprintf('4_inspect/out/predobsts_%s.png', res_id), width=5, height=5)
#
#   # make plot of temperature predictions for a couple of years
#   png(sprintf('4_inspect/out/preds2D_%s.png', res_id), width=1100, height=400, res=150, units='px')
#   glmtools::plot_temp(sprintf(nc_file, res_id))
#   dev.off()
#
#   # return table of RMSEs
#   rmses <- temp_matchups %>%
#     filter(!is.na(Modeled_temp), !is.na(Observed_temp)) %>%
#     group_by(depth_rank) %>%
#     summarize(
#       n = length(Modeled_temp),
#       MinDepth = min(Depth),
#       MaxDepth = max(Depth),
#       RMSE = sqrt(mean((Modeled_temp - Observed_temp)^2)))
#
#   # plot predicted vs observed lake levels
#   lake_preds <- read_csv(sprintf('tmp/io/%s/out/lake.csv', res_id))
#   ggplot(lake_preds, aes(x=time, y=`Lake Level`)) + geom_line()
#
#   return(rmses)
#
# }
# analyze_res(res_id=p2_reservoir_ids[1])
# # depth_rank     n MinDepth MaxDepth  RMSE
# # <ord>     <int>    <dbl>    <dbl> <dbl>
# # 1            23    0.128     2.02 0.593
# # 2           110    8.39     13.9  3.55
# # 3           110   19.7      25.2  1.32
# analyze_res(res_id=p2_reservoir_ids[2])
# # depth_rank     n MinDepth MaxDepth  RMSE
# # <ord>     <int>    <dbl>    <dbl> <dbl>
# # 2            26     7.47     10.9  1.78
# # 3            31     6.33     27.3  1.74
