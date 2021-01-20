library(targets)
library(tarchetypes)
library(tidyverse)
tar_option_set(packages = 'tidyverse')
source('1_fetch/src/sbtools_utils.R')

# Phase 1_fetch
p1 <- list(
  # Download temperature observation and GLM config files from ScienceBase,
  # using static branching over several files with prefix ltmp
  tar_map(
    values = tibble(
      ltmp_filenames = file.path('1_fetch/out', c('ltmp_levels.rds', 'ltmp_temps.rds', 'ltmp_nml_list.rds')),
      target_names = gsub('ltmp_', '', basename(ltmp_filenames))),
    names = starts_with('target_names'),
    tar_target(
      p1_ltmp,
      { sb_secret_login()
        item_file_download(
          sb_id = '6006eec9d34e592d867201d0',
          names = basename(ltmp_filenames),
          destinations = ltmp_filenames,
          overwrite_file = TRUE)
       return(ltmp_filenames)
      },
      packages = c('sbtools'),
      format = 'file')
  ),
    
  # Download meteo files from ScienceBase, using dynamic branching over the filenames mentioned in p1_ltmp_nml_list.rds
  tar_target(
    meteo_filenames,
    readRDS(p1_ltmp_nml_list.rds) %>% purrr::map_chr('meteo_fl') %>% unique() %>% file.path('1_fetch/out', .)
  ),
  tar_target(
    p1_NLDAS,
    { sb_secret_login()
      item_file_download(
        sb_id = '6006eec9d34e592d867201d0',
        names = basename(meteo_filenames),
        destinations = meteo_filenames,
        overwrite_file = TRUE)
    },
    packages = c('sbtools'),
    format = 'file',
    pattern = map(meteo_filenames)
  ),
    
  tar_target(
    reservoir_ids,
    readRDS(p1_ltmp_nml_list.rds) %>% purrr::map_chr('site_id')
  )
)

# Return the complete list of targets
c(p1)
