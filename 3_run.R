source('3_run/src/run_glm3.R')

p3 <- list(


  tar_target(
    # glm output files generated with local (Mac) GLM
    p3_glm,
    run_glm3_model(
      sim_dir = 'tmp/io', # tmp because at some point we won't want to keep the raw (and large) model outputs
      site_id = p2_reservoir_ids,
      nml_obj = p2_nml_objects,
      inouts_obj = p2_inouts,
      releases_obj = p2_releases,
      # Iterating on p2_meteo_xwalk is special: Whereas with site_id, nml_obj,
      # and inouts_obj we're iterating over the data, here we're iterating over
      # rows of a descriptive table instead. The currentness of the table
      # depends on the currentness of all meteo files, so we know they're up to
      # date by the time this runs. The contents of the table allow us to decide
      # which models need to be rerun, because the table includes a hash of the
      # meteo file specific to this reservoir (`meteo_data` column). Inside
      # `run_glm3_model()`, we will call `tar_read_raw(meteo_branch)` to get the
      # actual data.
      meteo_xwalk = p2_meteo_xwalk,
      export_fl = sprintf('3_run/out/mac/%s.feather', p2_reservoir_ids)),
    packages = c('glmtools', 'GLM3r'),
    pattern = map(p2_reservoir_ids, p2_nml_objects, p2_inouts, p2_releases, p2_meteo_xwalk))

  # tar_target(
  #   p2_glm,
  #   run_glm3_model(
  #     sim_dir = 'tmp/tar_map',
  #     site_id = p2_reservoir_ids,
  #     nml_obj = p2_nml_objects,
  #     inouts_obj = p2_inouts,
  #     meteo_obj = munge_meteo(
  #       meteo_fl = p2_meteo_files,
  #       n_max = 15320),
  #     export_fl = sprintf('3_run/out/mac/%s.feather', p2_reservoir_ids))
  # )

  # tar_target(
  #   # glm output files generated in Shifter container
  #   p3_glm_sh_out,
  #   run_glm3_model(
  #     sim_dir = 'tmp/sh',
  #     site_id = p1_reservoir_ids,
  #     nml_obj = p2_nml_objects,
  #     meteo_obj = p2_meteo,
  #     export_fl = sprintf('3_run/out/sh/%s.feather', p1_reservoir_ids)),
  #   packages = c('glmtools'),
  #   pattern = map(p1_reservoir_ids, p2_nml_objects, p2_meteo))
)
