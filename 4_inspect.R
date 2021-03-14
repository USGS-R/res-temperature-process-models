source('3_run/src/process_glm_output.R')
source('4_inspect/src/inspect_glm_flows.R')
source('4_inspect/src/inspect_glm_temps.R')

# These mapped targets are insensitive to whether p3_glm_report.csv is up to
# date, so make sure you call tar_make(p3_glm_report.csv) first if you want the
# plots to reflect new model changes
sim_summary <- tibble(
  sim_id = c('210314a_no_io', '210314b_io', '210314c_io_evap', '210314d_iox_evap'),
  sim_dir = file.path('3_run/out', sim_id),
  sim_report = file.path(sim_dir, 'glm_report.csv')) %>%
  mutate(sim_hash = purrr::map_chr(sim_report, function(sim_rep) {
    suppressWarnings(paste(read_csv(sim_rep, col_types=cols())$glm_hash, collapse='|'))
  }))
mapped_targets <- tar_map(
  values = sim_summary,
  names = starts_with('sim_id'),

  # Water balance predictions - within reservoir

  tar_target(
    p4_plot_lake_levels,
    {
      # re: dependencies: # the example at
      # https://books.ropensci.org/targets/static.html#when-to-use-static-branching
      # has the second target map()ing over the first mapped target, but that
      # seems to be throwing errors now. Documentation out of date?
      dependencies <- sim_hash
      plot_lake_levels(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        p1_ltmp_levels.rds,
        res_id = p2_reservoir_ids,
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids),
    packages = c('glmtools'),
    format = 'file'),

  tar_target(
    p4_plot_water_budget,
    {
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
    p4_plot_temps_sensor_depths,
    {
      dependencies <- sim_hash
      plot_temps_sensor_depths(
        sim_res_dir = file.path(sim_dir, p2_reservoir_ids),
        obs_temps_rds = p1_ltmp_temps.rds,
        res_id = p2_reservoir_ids,
        plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
        out_dir = '4_inspect/out')
    },
    pattern = map(p2_reservoir_ids),
    packages = c('glmtools'),
    format = 'file')
)
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
    command = dplyr::bind_rows(!!!.x) %>%
      arrange(res_name, sim_id))

)
