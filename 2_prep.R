source('2_prep/src/munge_nmls.R')

p2 <- list(
  tar_target(
    nml_group_summary,
    write_glm3_nml_files(
      nml_list_rds = p1_ltmp_nml_list.rds,
      site_ids = reservoir_ids,
      base_nml = '2_prep/in/glm3_template.nml',
      nml_dir = '2_prep/tmp',
      nml_hashes_yml = '2_prep/out/glm3_nml_group.yml'),
    packages = c('glmtools', 'tidyverse'))
)
