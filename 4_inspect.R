source('3_run/src/process_glm_output.R')
source('4_inspect/src/inspect_glm_flows.R')
source('4_inspect/src/inspect_glm_temps.R')


p4 <- list(
  tar_map(
    values = tibble(
      target_names = c('io','no_io','io_noevap'),
      sim_dir = file.path('3_run/out', target_names)),
    names = starts_with('target_names'),

    # Water balance predictions - within reservoir

    tar_target(
      p4_plot_lake_levels,
      { p3_glm; # add the most recent models as a dependency
        plot_lake_levels(
          sim_dir = file.path(sim_dir, p2_reservoir_ids),
          nml_obj = p2_nml_objects,
          p1_ltmp_levels.rds,
          res_id = p2_reservoir_ids,
          plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
          out_dir = '4_inspect/out')
      },
      pattern = map(p2_nml_objects, p2_reservoir_ids),
      packages = c('glmtools'),
      format = 'file'),

    # Temperature predictions - within reservoir

    tar_target(
      p4_plot_temps_all_depths,
      { p3_glm; # add the most recent models as a dependency
        plot_temps_all_depths(
          sim_dir = file.path(sim_dir, p2_reservoir_ids),
          nml_obj = p2_nml_objects,
          plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
          out_dir = '4_inspect/out')
      },
      pattern = map(p2_nml_objects, p2_reservoir_ids),
      packages = c('glmtools'),
      format = 'file'),

    tar_target(
      p4_plot_temps_sensor_depths,
      { p3_glm; # add the most recent models as a dependency
        plot_temps_sensor_depths(
          sim_dir = file.path(sim_dir, p2_reservoir_ids),
          nml_obj = p2_nml_objects,
          obs_temps_rds = p1_ltmp_temps.rds,
          res_id = p2_reservoir_ids,
          plot_id = sprintf('%s_%s', p2_reservoir_ids, basename(sim_dir)),
          out_dir = '4_inspect/out')
      },
      pattern = map(p2_nml_objects, p2_reservoir_ids),
      packages = c('glmtools'),
      format = 'file')
  )
)
