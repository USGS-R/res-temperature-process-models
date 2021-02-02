source('2_prep/src/munge_nmls.R')
source('2_prep/src/munge_meteo.R')

p2 <- list(
  tar_target(p2_glm_template.nml, '2_prep/in/glm3_template.nml', format = 'file'),
  tar_target(
    # a list of complete nml objects
    p2_nml_objects,
    write_glm3_nml_files(
      nml_list_rds = p1_ltmp_nml_list.rds,
      nml_edits = p1_nml_edits,
      site_ids = p1_reservoir_ids,
      base_nml = p2_glm_template.nml,
      nml_dir = '2_prep/out/nml',
      nml_hashes_yml = '2_prep/out/glm3_nml_files.yml'),
    packages = c('glmtools'),
    iteration = 'list'),

  # a list of munged meteo data.frames
  tar_target(
    p2_meteo,
    munge_meteo(
      meteo_fl = p1_meteo,
      n_max = 14975), # TODO: when the meteo date range changes, update this number
    pattern = map(p1_meteo),
    format = 'fst',
    iteration = 'list'
  )
)
