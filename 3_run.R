source('3_run/src/run_glm3.R')
source('3_run/src/process_glm_output.R')

p3 <- list(

  tar_target(
    # glm output files generated with local (Mac) GLM
    p3_glm,
    run_glm3_model(
      sim_dir = '3_run/out/io_noevap', # eventually 'tmp/something' because at some point we won't want to keep the raw (and large) model outputs
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
      export_fl = sprintf('3_run/out/io_noevap/%s/out/exported.feather', p2_reservoir_ids)),
    packages = c('glmtools', 'GLM3r'),
    pattern = map(p2_reservoir_ids, p2_nml_objects, p2_inouts, p2_releases, p2_meteo_xwalk))

)
