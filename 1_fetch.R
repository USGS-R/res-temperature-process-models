source('1_fetch/src/sbtools_utils.R')
source('1_fetch/src/zip_to_sf.R')
source('1_fetch/src/build_nml_edits.R')

data_release_sb_id <- '6006eec9d34e592d867201d0'
sb_status_csv_name <- '1_fetch/log/sb_status.csv'

p1 <- list(
  tar_target(
    sb_status_csv,
    update_sb_status(
      sb_id = data_release_sb_id,
      status_file = sb_status_csv_name, # targets doesn't treat this argument as a file target, but it does become the filename stored by sb_status_csv
      wait_interval = as.difftime(1, units='days'), # make this huge to ~never check
      ignore_files = c('fgdc_metadata.xml', 'reservoir_polygons.zip')),
    cue = tar_cue('always'), # update_sb_status always runs, but only goes to ScienceBase after waiting at least as long as "wait_interval" above
    format = 'file',
    packages = 'sbtools'
  ),
  # to view status:
  # source('1_fetch/src/sbtools_utils.R'); read_sb_status(tar_read(sb_status_csv))
  tar_target(sb_outdated, get_sb_outdated(sb_status_csv)),

  # Download and unzip the reservoir polygons...but we're not directly using
  # these after all. If uncommenting, remove reservoir_polygons.zip from the ignore_files() argument to update_sb_status
  # tar_target(
  #   p1_reservoir_polygons,
  #   sb_download_if_needed(
  #     sb_id = data_release_sb_id,
  #     names = 'reservoir_polygons.zip',
  #     destinations = '1_fetch/out/reservoir_polygons.zip',
  #     outdated = sb_outdated,
  #     status_file = sb_status_csv_name) %>%
  #     zip_to_sf()
  # ),

  # Download temperatures, water levels, and GLM config files from ScienceBase,
  # using static branching over several files with prefix ltmp
  tar_map(
    values = tibble(
      ltmp_filenames = file.path('1_fetch/out', c('ltmp_temps.rds', 'ltmp_levels.rds', 'ltmp_nml_list.rds')),
      target_names = gsub('ltmp_', '', basename(ltmp_filenames))),
    names = starts_with('target_names'),
    tar_target(
      p1_ltmp,
      sb_download_if_needed(
        sb_id = data_release_sb_id,
        names = basename(ltmp_filenames),
        destinations = ltmp_filenames,
        outdated = sb_outdated, # can set outdated = ltmp_filenames to make this a target that only rebuilds on force
        status_file = sb_status_csv_name), # rely on a non-target string so that updates to sb_status_csv don't trigger rebuilds here
      packages = c('sbtools'),
      format = 'file')
  ),

  # Download meteo files from ScienceBase, using dynamic branching over the
  # filenames mentioned in p1_ltmp_nml_list.rds
  tar_target(
    p1_meteo_filenames,
    readRDS(p1_ltmp_nml_list.rds) %>%
      purrr::map_chr('meteo_fl') %>%
      unique() %>%
      file.path('1_fetch/out', .)
  ),
  tar_target(
    p1_meteo_files,
    sb_download_if_needed(
      sb_id = data_release_sb_id,
      names = basename(p1_meteo_filenames),
      destinations = p1_meteo_filenames,
      outdated = sb_outdated, # can set outdated = ltmp_filenames to make this a target that only rebuilds on force
      status_file = sb_status_csv_name), # rely on a non-target string so that updates to sb_status_csv don't trigger rebuilds here
    packages = c('sbtools'),
    format = 'file',
    pattern = map(p1_meteo_filenames)
  ),

  # Specify the source of inflow-outflow data (SNTemp predictions, currently)
  # TODO - use res-temperature-data-prep to transfer this file via ScienceBase
  tar_target(
    p1_inout_feather,
    '../delaware-model-prep/9_collaborator_data/res/res_io_sntemp.feather',
    format = 'file'),

  # Get reservoir releases from SB at https://www.sciencebase.gov/catalog/item/5f6a287382ce38aaa2449131
  # Because this is a different SB item from the res-temperature-data-sharing item, we can't pull only if needed very conveniently.
  # TODO: pull reservoir release data from Sam's repo, add to res-temperature-data-sharing pipeline, then pull from there to here
  # sb_id = '5f6a287382ce38aaa2449131'
  # 'reservoir_releases.csv'
  tar_target(
    p1_nml_edits,
    list(
      nhdhr_151957878 = list(
        site_name = 'Pepacton',
        crest_elev = 390.0 # from Sam's slides on 1/28/21
      ),
      nhdhr_120022743 = list(
        site_name = 'Cannonsville',
        crest_elev = 350.6 # from Sam's slides on 1/28/21
      )
    )
  ),

  tar_target(
    p1_inflows,
    get_inflow_data(
      p1_reservoir_ids
    )
  )

)
