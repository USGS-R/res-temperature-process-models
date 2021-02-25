source('3_run/src/run_glm3.R')

p3 <- list(
  tar_target(
    # glm output files generated with local (Mac) GLM
    p3_glm_mac_out,
    run_glm3_model(
      sim_dir = 'tmp/mac', # tmp because at some point we won't want to keep the raw (and large) model outputs
      site_id = p1_reservoir_ids,
      nml_obj = p2_nml_objects,
      meteo_obj = p2_meteo,
      export_fl = sprintf('3_run/out/mac/%s.feather', p1_reservoir_ids)),
    packages = c('glmtools'),
    pattern = map(p1_reservoir_ids, p2_nml_objects, p2_meteo)),

  tar_target(
    # glm output files generated in Shifter container
    p3_glm_sh_out,
    run_glm3_model(
      sim_dir = 'tmp/sh',
      site_id = p1_reservoir_ids,
      nml_obj = p2_nml_objects,
      meteo_obj = p2_meteo,
      export_fl = sprintf('3_run/out/sh/%s.feather', p1_reservoir_ids)),
    packages = c('glmtools'),
    pattern = map(p1_reservoir_ids, p2_nml_objects, p2_meteo))
)
