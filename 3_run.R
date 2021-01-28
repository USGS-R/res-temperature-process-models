source('3_run/src/run_glm3.R')

p3 <- list(
  tar_target(
    # glm output files
    p3_glm_out,
    run_glm3_model(
      site_id = p1_reservoir_ids,
      nml_obj = p2_nml_objects,
      meteo_obj = p2_meteo,
      export_fl = sprintf('3_run/out/%s', p1_reservoir_ids)),
    packages = c('glmtools'),
    pattern = map(p1_reservoir_ids, p2_nml_objects, p2_meteo))
)
