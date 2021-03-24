# Code for developing a new inspection function:
# library(targets)
# library(tidyverse)
# source('3_run/src/process_glm_output.R')
# sim_id <- '210314b_io'
# res_num <- 1
# res_id <- tar_read(p2_reservoir_ids)[res_num]
# sim_res_dir <- file.path('3_run/out', sim_id, res_id)
# plot_id <- sprintf('%s_%s', res_id, sim_id)
# out_dir <- 'tmp'
# inouts = tar_read(p2_inouts)[[res_num]]
# obs_inouts = tar_read(p2_obs_inouts)[[res_num]]

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
# (not doing this for now)

#' Get predictions at outlet depths. This is different from get_outlet_preds
#' because that function reads the outlet_xx.csv files from the output, and
#' those files contain NA (OK, -9999) for temperatures on days when predicted
#' flow = 0. We'd like to have temperature predictions at the relevant depths on
#' all days.
get_temps_outlet_depths <- function(sim_res_dir) {

  nml_obj <- locate_in_files(sim_res_dir, 'nml') %>% glmtools::read_nml()
  base_elev <- glmtools::get_nml_value(nml_obj, 'crest_elev') - glmtools::get_nml_value(nml_obj, 'lake_depth')
  nc_file <- locate_out_files(sim_res_dir, file_type='depthwise')

  outlet_info <- get_outlet_info(sim_res_dir) %>%
    mutate(
      reference = case_when(
        outlet_type == 1 ~ 'bottom',
        outlet_type == 2 ~ 'surface'),
      z_out = case_when(
        reference == 'bottom' ~ outlet_elev - base_elev,
        reference == 'surface' ~ outlet_elev))

  # Make one call to glmtools::get_temp for each unique reference-z_out combo,
  # then join the results and merge back the outlet metadata
  extraction_depths <- outlet_info %>%
    select(reference, z_out) %>%
    distinct()
  temp_matchups <- if(nrow(extraction_depths) > 0) {
    purrr::pmap_df(
      extraction_depths, function(reference, z_out) {
        glmtools::get_temp(nc_file, reference = reference, z_out = z_out) %>%
          set_names(c('DateTime', 'temp')) %>%
          mutate(
            time = as.Date(format(DateTime, '%Y-%m-%d')),
            reference = reference,
            z_out = z_out) %>%
          select(-DateTime) %>%
          left_join(
            outlet_info %>% select(outlet_label, reference, z_out),
            by=c('reference', 'z_out'))
      }) %>%
      as_tibble() %>%
      select(outlet_label, time, temp, reference, z_out)
  } else {
    tibble(outlet_label = '', time = Sys.Date(), temp = 0.0, reference = '', z_out = 0.0)[c(),]
  }

  return(temp_matchups)

}

#' Make a blank plot to represent the absence of info
plot_blank <- function(title, message = '(no data)') {
  ggplot() +
    annotate('text', x = 0, y = 0, label = message) +
    ggtitle(title) +
    xlab('') + ylab('') +
    theme_bw() + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
}

#' Plot temperature predictions - time series for the outlet depths
plot_temps_outlet_depths <- function(sim_res_dir, res_id, plot_id, out_dir) {

  # Get two types of predictions: those exported in the outlet_xx.csv files
  # (which have temp = -9999 when flow = 0, and which integrate over a range of
  # depths to simulate withdrawal) and temperature predictions at specific
  # reservoir depths that correspond to outlet depths
  outlet_preds <- get_outlet_preds(sim_res_dir)
  outlet_depth_preds <- get_temps_outlet_depths(sim_res_dir)

  plot_title <- sprintf('Predicted temps at outflows (points) and outflow depths (lines)\nfor %s (%s)', names(res_id), res_id)
  if(nrow(outlet_depth_preds) == 0) {
    plot_blank(
      title = plot_title,
      message = '(no outflows defined)')
  } else {
    outlet_depth_preds %>%
      ggplot(aes(x = time, y = temp, color = outlet_label)) +
      geom_line(alpha = 0.5) +
      geom_point(data = outlet_preds, size = 0.1, alpha = 1, na.rm = TRUE) +
      scale_color_discrete(guide = 'none') +
      facet_grid(outlet_label ~ .) +
      theme_bw() +
      ggtitle(plot_title)
  }

  # Save the plot
  out_file <- file.path(out_dir, sprintf('temps_outlet_depths_%s.png', plot_id))
  num_outlets <- length(unique(outlet_depth_preds$outlet_label))
  ggsave(out_file, width = 7, height = 2 + num_outlets)
  return(out_file)
}

#' Plot predicted and observed temperatures downstream of the reservoir
plot_temps_downstream_predobs <- function(sim_res_dir, inouts, obs_inouts, res_id, plot_id, out_dir) {

  # Get the predictions and observations
  outlet_predobs <- get_downstream_predobs(sim_res_dir, inouts, obs_inouts)

  # Prepare to save plots
  ts_out_file <- file.path(out_dir, sprintf('temps_downstream_ts_%s.png', plot_id))
  pairs_out_file <- file.path(out_dir, sprintf('temps_downstream_pairs_%s.png', plot_id))
  plot_title <- sprintf('Downstream temperatures for %s (%s)', names(res_id), res_id) # used in both plots

  # If GLM predictions are absent, make dummy plots and leave
  if(nrow(filter(outlet_predobs, source == 'glm')) == 0) {
    plot_blank(
      title = plot_title,
      message = '(no outflows defined)')
    ggsave(ts_out_file, width = 7, height = 2)
    ggsave(pairs_out_file, width = 7, height = 2)
    return(c(ts_out_file, pairs_out_file))
  }

  # Make and save timeseries plot
  outlet_predobs %>%
    filter(time >= min(time[source == 'glm'])) %>%
    ggplot(aes(x=time, y=temp, color=source_id)) +
    geom_point(size=0.2, na.rm = TRUE) +
    scale_color_discrete(guide = FALSE) +
    theme_bw() +
    facet_grid(source_id ~ .) +
    ggtitle(plot_title)
  ggsave(ts_out_file, width = 7, height = 7)

  # Make and save pairs plot
  png(pairs_out_file, width = 7, height = 7, units = 'in', res = 300)
  suppressWarnings({
    outlet_predobs %>%
      select(-id, -source, -flow) %>%
      pivot_wider(id_cols = c(time), names_from = source_id, values_from = c(temp)) %>%
      { suppressWarnings(
        GGally::ggpairs(., columns=c(4,3,2), lower = list(continuous = GGally::wrap('points', alpha = 0.3, size=0.1))) +
          geom_abline() +
          theme_bw() +
          ggtitle(plot_title)
      )} %>%
      print()
  })
  dev.off()

  # Return the filenames of both plots
  return(c(ts_out_file, pairs_out_file))
}

#' Compute RMSEs and NSEs of outflow (downstream) temperature and flow
assess_downstream_predobs <- function(sim_res_dir, inouts, obs_inouts, res_id, sim_id) {

  # Get the predictions and observations
  outlet_predobs <- get_downstream_predobs(sim_res_dir, inouts, obs_inouts)

  assessment <- purrr::map_df(setNames(nm=c('temp','flow','flow_log')), function(var) {
    outlet_predobs %>%
      mutate(flow_log = log(flow)) %>%
      pivot_wider(id_cols = c(time), names_from = source_id, values_from = all_of(var)) %>%
      pivot_longer(cols=contains(c('glm','nores')), values_to = 'pred', names_to = 'model') %>%
      rename_with(.cols = starts_with('obs'), .fn = function(colname) 'obs') %>%
      filter(!is.na(obs), !is.na(pred)) %>%
      group_by(model) %>%
      summarize(
        min_date = min(time),
        max_date = max(time),
        n_obs = length(obs),
        n_obs_per_day = n_obs / (1 + as.numeric(max_date - min_date, units='days')),
        RMSE = sqrt(mean((pred - obs)^2)),
        NSE = 1 - sum((pred - obs)^2) / sum((obs - mean(obs))^2),
        .groups = 'drop')
  }, .id = 'variable') %>%
    add_id_cols(res_id, sim_id)

  assessment

}
