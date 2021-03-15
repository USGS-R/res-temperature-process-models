source('3_run/src/run_glm3.R')
source('3_run/src/process_glm_output.R')

glm_sim <- tribble(
  ~dir, ~info
  # ,'3_run/out/210314a_no_io', 'No inflows or outflows; disable_evap = .true.'
  # ,'3_run/out/210314b_io', 'PRMS-based inflows and release-based outflows; disable_evap = .true.'
  # ,'3_run/out/210314c_io_evap', 'PRMS-based inflows and release-based outflows; disable_evap = .false.'
  ,'3_run/out/210314d_io_evap', 'same as previous so far'
)

p3 <- list(

  tar_target(
    p3_glm,
    glm_report <- run_glm3_model(
      sim_dir = glm_sim$dir, # eventually 'tmp/something' because at some point we won't want to keep the raw (and large) model outputs
      res_id = p2_reservoir_ids,
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
      export_fl = sprintf('%s/%s/out/exported.feather', glm_sim$dir, p2_reservoir_ids)),
    packages = c('glmtools', 'GLM3r'),
    pattern = map(p2_reservoir_ids, p2_nml_objects, p2_inouts, p2_releases, p2_meteo_xwalk)),

  # Create a single report for all models that ran in p3_glm above
  tar_target(
    p3_glm_report.csv,
    {
      out_file <- file.path(glm_sim$dir, 'glm_report.csv')
      p3_glm %>%
        mutate(
          sim_res_dir = file.path(glm_sim$dir, res_id),
          sim_id = basename(glm_sim$dir),
          sim_info = glm_sim$info) %>%
        write_csv(out_file)
      return(out_file)
    },
    format = 'file'
  )#,

  # Not sure what we'd need this target for; so far just practicing getting branch names
  # tar_target(
  #   res_model_branches,
  #   {
  #     res_ids <- p2_reservoir_ids # this seems to work better than referring to p2_reservoir_ids directly within the mutate() below
  #     tar_branches(p3_glm, pattern = map(p2_reservoir_ids, p2_nml_objects, p2_inouts, p2_releases, p2_meteo_xwalk)) %>%
  #       mutate(res_id = res_ids, res_name = names(res_ids), .before=1) %>%
  #       left_join(select(p2_meteo_xwalk, res_id, meteo_fl, meteo_branch), by='res_id')
  #   })

)
