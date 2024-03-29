source('2_prep/src/munge_nmls.R')
source('2_prep/src/munge_meteo.R')
source('2_prep/src/munge_inouts.R')
source('2_prep/src/harmonize_dates.R')
source('2_prep/src/visualize_inputs.R')

p2 <- list(

  #### Munge model inputs ####

  # Set the list of reservoirs to proceed with. As currently coded, we extract the reservoir IDs from the nml list,
  # but this code may be overridden by a vector or subsetting code to reduce the number of models being processed in
  # p2 and subsequent phases
  tar_target(
    p2_reservoir_ids,
    readRDS(p1_ltmp_nml_list.rds) %>%
      purrr::map_chr('site_id') %>%
      {setNames(., purrr::map_chr(p1_nml_edits, 'site_name')[.])} %>% # add layperson reservoir names to the vector
      .[] # .[c(1,1,2)] # subset/customize the list if desired
  ),
  # Identify the set of meteo files corresponding to the [potentially subsetted] p2_reservoir_ids
  tar_files(
    p2_meteo_files,
    {
      depends <- p1_meteo_files
      read_rds(p1_ltmp_nml_list.rds) %>%
        purrr::map_chr('meteo_fl') %>%
        .[p2_reservoir_ids] %>%
        unique() %>%
        file.path('1_fetch/out', .)
    }
  ),
  # tar_target(p2_meteo_files, p2_meteo_filenames, pattern=map(p2_meteo_filenames), format='file'),

  # Create a list of munged meteo tables, one per NLDAS cell. Here we map over
  # the meteo files, reading and munging each file into an NLDAS-cell-specific
  # table, which `targets` then can present as an iterable list
  tar_target(
    p2_meteo,
    munge_meteo(
      meteo_fl = p2_meteo_files,
      n_max = 15320), # TODO: when the meteo date range changes, update this number
    pattern = map(p2_meteo_files),
    format = 'fst', # fst is about twice as fast to load as rds, 10% faster than qs
    iteration = 'list'
  ),
  # Build a dplyr table with tar_grouping that has one row per element of
  # p2_reservoir_ids and columns for the (1) tar branch name and (2) data hash
  # for the meteo data for each reservoir. We can use this to run models. Note
  # that we're accepting a storage-parallelizability tradeoff here: this xwalk
  # table allows us to retain just one meteo object per NLDAS cell rather than
  # one per reservoir, but a consequence is that all branches of p2_meteo must
  # be built before the xwalk can be created and thus before any models can be
  # run (thus losing flexibility in parallelism that targets could otherwise
  # provide if requested).
  tar_target(
    p2_meteo_xwalk,
    build_meteo_xwalk(p2_meteo_files, p2_meteo, p1_ltmp_nml_list.rds, p2_reservoir_ids),
    iteration = 'group'
  ),

  # Create a list of munged inputs and outputs, one table per reservoir. Map
  # over the reservoir ids to subset p1_io_res_io_sntemp.feather into individual tables
  # which `targets` then can present as an iterable list
  # PS - Here's how you could find the branch and reservoir names corresponding to each inouts file:
  # tar_target(p2_inouts_names, setNames(names(p2_inouts), p2_reservoir_ids)),
  tar_target(
    p2_inouts,
    munge_inouts_sntemp(
      inout_feather = p1_io_res_io_sntemp.feather,
      res_id = p2_reservoir_ids),
    pattern = map(p2_reservoir_ids),
    format = 'fst', # fst is about twice as fast to load as rds, 10% faster than qs
    packages = c('arrow'),
    iteration = 'list'
  ),

  # Read in the release data
  tar_target(
    p2_releases,
    munge_releases(
      releases_csv = p1_releases_csv,
      res_id = p2_reservoir_ids),
    pattern = map(p2_reservoir_ids),
    format = 'fst', # fst is about twice as fast to load as rds, 10% faster than qs
    packages = c('arrow'),
    iteration = 'list'
  ),

  # Identify the maximum date range that's completely covered by the input
  # driver files, or override with a shorter time period. Data objects will be
  # subset to this range at GLM runtime
  tar_target(
    p2_date_range,
    find_shared_date_range(
      meteo = p2_meteo,
      inouts = p2_inouts,
      releases = p2_releases)
    # Or comment out the above command and use this override instead:
    # tibble(
    #   start = as.Date('2004-01-01'),
    #   stop = as.Date('2005-01-01'))
  ),

  # Create a list of complete nml objects. Transform a single file of all
  # reservoirs to a single list of all reservoirs (subset to p2_reservoir_ids),
  # then tell `targets` to think of that list as an iterable list
  tar_target(p2_glm_template.nml, '2_prep/in/glm3_template.nml', format = 'file'),
  tar_target(
    p2_nml_objects,
    munge_nmls(
      nml_list_rds = p1_ltmp_nml_list.rds,
      nml_edits = p1_nml_edits,
      start_stop = p2_date_range,
      res_ids = p2_reservoir_ids,
      base_nml = p2_glm_template.nml),
    packages = c('glmtools'),
    iteration = 'list'),

  #### Munge observation data ####

  tar_target(
    p2_obs_res_temps,
    read_rds(p1_ltmp_temps.rds) %>%
      filter(site_id == p2_reservoir_ids) %>%
      select(datetime = date, depth, temp),
    pattern = map(p2_reservoir_ids),
    format = 'fst', # fst is about twice as fast to load as rds, 10% faster than qs
    iteration = 'list'),

  tar_target(
    p2_obs_res_levels,
    read_rds(p1_ltmp_levels.rds) %>%
      filter(site_id == p2_reservoir_ids) %>%
      select(date, surface_elevation_m),
    pattern = map(p2_reservoir_ids),
    format = 'fst', # fst is about twice as fast to load as rds, 10% faster than qs
    iteration = 'list'),

  tar_target(
    p2_obs_inouts,
    munge_inouts_obs(p1_io_res_io_obs.feather, p2_reservoir_ids),
    pattern = map(p2_reservoir_ids),
    format = 'fst', # fst is about twice as fast to load as rds, 10% faster than qs
    packages = 'arrow',
    iteration = 'list'),

  ##### Visualize ####

  tar_target(
    p2_plot_water_budget,
    plot_outflows(
      p2_inouts,
      p2_releases,
      res_id = p2_reservoir_ids,
      out_dir = '2_prep/out'),
    pattern = map(p2_reservoir_ids, p2_inouts, p2_releases),
    packages = c('glmtools'),
    format = 'file')
)
