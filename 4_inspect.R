source('3_run/src/process_glm_output.R')
source('4_inspect/src/inspect_glm_flows.R')
source('4_inspect/src/inspect_glm_temps.R')

# These mapped targets are insensitive to whether p3_glm_report.csv is up to
# date, so make sure you call tar_make(p3_glm_report.csv) first if you want the
# plots to reflect new model changes
sim_summary <- tibble(
  sim_id = basename(glm_sims$dir),
  sim_dir = file.path('3_run/out', sim_id),
  sim_report = file.path(sim_dir, 'glm_report.csv')) %>%
  mutate(sim_hash = purrr::map_chr(sim_report, function(sim_rep) {
    tryCatch(
      suppressWarnings(read_csv(sim_rep, col_types=cols())$glm_hash) %>%
        digest::digest(),
      error = function(e) NA)
  }))
mapped_targets <- tar_map(
  values = sim_summary,
  names = starts_with('sim_id'),

  # Water balance predictions - within reservoir

  tar_target(
    p4_plot_water_budget,
    {
      # re: dependencies: # the example at
      # https://books.ropensci.org/targets/static.html#when-to-use-static-branching
      # has the second target map()ing over the first mapped target, but that
      # seems to be throwing errors now. Documentation out of date?
      dependencies <- sim_hash
      plot_water_budget(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        res_id = p2_reservoir_ids,
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids),
    packages = c('glmtools'),
    format = 'file'),

  tar_target(
    p4_assess_water_budget,
    {
      dependencies <- sim_hash
      assess_water_budget(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        res_id = p2_reservoir_ids,
        sim_id = basename(sim_dir))
    },
    pattern = map(p2_reservoir_ids),
    packages = c('glmtools')),

  tar_target(
    p4_assess_residence_time,
    {
      dependencies <- sim_hash
      assess_residence_time(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        res_id = p2_reservoir_ids,
        sim_id = basename(sim_dir))
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_levels),
    packages = c('glmtools')),

  tar_target(
    p4_plot_lake_levels,
    {
      dependencies <- sim_hash
      plot_lake_levels(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_res_levels = p2_obs_res_levels,
        res_id = p2_reservoir_ids,
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_levels),
    packages = c('glmtools'),
    format = 'file'),

  tar_target(
    p4_assess_lake_levels,
    {
      dependencies <- sim_hash
      assess_lake_levels(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_res_levels = p2_obs_res_levels,
        res_id = p2_reservoir_ids,
        sim_id = basename(sim_dir))
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_levels),
    packages = c('glmtools')),

  tar_target(
    p4_plot_flows_downstream_predobs,
    plot_flows_downstream_predobs(
      sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
      inouts = p2_inouts,
      obs_inouts = p2_obs_inouts,
      res_id = p2_reservoir_ids,
      plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
      out_dir = '4_inspect/out'),
    pattern = map(p2_reservoir_ids, p2_inouts, p2_obs_inouts),
    packages = c('GGally'),
    format = 'file'
  ),

  # Temperature predictions - within reservoir

  tar_target(
    p4_plot_temps_all_depths,
    {
      dependencies <- sim_hash
      plot_temps_all_depths(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids),
    packages = c('glmtools'),
    format = 'file'),

  tar_target(
    p4_plot_temps_all_depths_obspred,
    {
      dependencies <- sim_hash
      plot_temps_all_depths_obspred(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_res_temps = p2_obs_res_temps,
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_temps),
    packages = c('glmtools'),
    format = 'file'),

  tar_target(
    p4_assess_temps_all_depths,
    {
      dependencies <- sim_hash
      assess_temps_all_depths(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_res_temps = p2_obs_res_temps,
        res_id = p2_reservoir_ids,
        sim_id = basename(sim_dir))
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_temps),
    packages = c('glmtools')),

  tar_target(
    p4_plot_temps_sensor_depths,
    {
      dependencies <- sim_hash
      plot_temps_sensor_depths(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_res_temps = p2_obs_res_temps,
        res_id = p2_reservoir_ids,
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_temps),
    packages = c('glmtools'),
    format = 'file'),

  tar_target(
    p4_assess_temps_sensor_depths,
    {
      dependencies <- sim_hash
      assess_temps_sensor_depths(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_res_temps = p2_obs_res_temps,
        res_id = p2_reservoir_ids,
        sim_id = basename(sim_dir))
    },
    pattern = map(p2_reservoir_ids, p2_obs_res_temps),
    packages = c('glmtools')),

  # Temperature predictions - below reservoir

  tar_target(
    p4_plot_temps_outlet_depths,
    plot_temps_outlet_depths(
      sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
      res_id = p2_reservoir_ids,
      plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
      out_dir = '4_inspect/out'),
    pattern = map(p2_reservoir_ids),
    format = 'file'
  ),

  tar_target(
    p4_plot_temps_downstream_predobs,
    plot_temps_downstream_predobs(
      sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
      inouts = p2_inouts,
      obs_inouts = p2_obs_inouts,
      res_id = p2_reservoir_ids,
      plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
      out_dir = '4_inspect/out'),
    pattern = map(p2_reservoir_ids, p2_inouts, p2_obs_inouts),
    packages = c('GGally'),
    format = 'file'
  ),

  # Temperature and discharge predictions below reservoir
  tar_target(
    p4_assess_downstream_predobs,
    assess_downstream_predobs(
      sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
      inouts = p2_inouts,
      obs_inouts = p2_obs_inouts,
      res_id = p2_reservoir_ids,
      sim_id = basename(sim_dir)),
    pattern = map(p2_reservoir_ids, p2_inouts, p2_obs_inouts)
  )
)

combine_assessments <- function(...) {
  dplyr::bind_rows(...) %>%
    arrange(res_name, sim_id)
}

p4 <- list(

  tar_target(
    p4_glm_report_df,
    command = purrr::map(sim_summary$sim_report, .f=function(report.csv) {
      read_csv(report.csv, col_types=cols())
    }) %>%
      bind_rows()
  ),

  mapped_targets,

  tar_combine(
    p4_assess_water_budget_df,
    mapped_targets$p4_assess_water_budget,
    command = combine_assessments(!!!.x)),

  tar_combine(
    p4_assess_lake_levels_df,
    mapped_targets$p4_assess_lake_levels,
    command = combine_assessments(!!!.x)),

  tar_combine(
    p4_assess_residence_time_df,
    mapped_targets$p4_assess_residence_time,
    command = combine_assessments(!!!.x)),

  tar_combine(
    p4_assess_temps_all_depths_df,
    mapped_targets$p4_assess_temps_all_depths,
    command = combine_assessments(!!!.x)),

  tar_combine(
    p4_assess_temps_sensor_depths_df,
    mapped_targets$p4_assess_temps_sensor_depths,
    command = combine_assessments(!!!.x)),

  tar_combine(
    p4_assess_downstream_predobs_df,
    mapped_targets$p4_assess_downstream_predobs,
    command = combine_assessments(!!!.x) %>%
      arrange(variable, res_name, desc(model), sim_id))

)
