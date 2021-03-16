run_glm3_model <- function(sim_dir, res_id, nml_obj, inouts_obj, releases_obj, meteo_xwalk, inout_obj, export_fl){ # kw_data,
  # prepare to write inputs and results locally for quick I/O
  sim_res_dir <- file.path(sim_dir, res_id)
  dir.create(sim_res_dir, recursive=TRUE, showWarnings=FALSE)
  # on.exit(unlink(sim_res_dir, recursive = TRUE))

  # extract the start and stop dates from nml
  start_date <- as.Date(nml_obj$time$start)
  stop_date <- as.Date(nml_obj$time$stop)

  # write the nml to sim_res_dir
  glmtools::write_nml(nml_obj, file.path(sim_res_dir, 'glm3.nml'))

  # write the meteo data to within the sim_res_dir
  meteo_fl <- glmtools::get_nml_value(nml_obj, arg_name = 'meteo_fl')
  meteo_obj <- tar_read_raw(meteo_xwalk$meteo_branch)
  meteo_dest <- file.path(sim_res_dir, meteo_fl)
  dir.create(dirname(meteo_dest), showWarnings=FALSE)
  meteo_obj %>%
    filter(time >= start_date, time <= stop_date) %>%
    readr::write_csv(file.path(sim_res_dir, meteo_fl))

  # write the inflows to within the sim_res_dir
  inflow_files <- tibble(
    seg_id_nat = strsplit(glmtools::get_nml_value(nml_obj, 'names_of_strms'), split=',')[[1]],
    inflow_fl = strsplit(glmtools::get_nml_value(nml_obj, 'inflow_fl'), split=',')[[1]]
  )
  inouts_obj %>%
    filter(location == 'inflow') %>%
    filter(time >= start_date, time <= stop_date) %>%
    select(-location) %>%
    left_join(inflow_files, by='seg_id_nat') %>%
    group_by(seg_id_nat, inflow_fl) %>%
    dplyr::group_walk(function(inflow_data, group_keys) {
      write_csv(
        inflow_data %>% select(time, flow, temp),
        file = file.path(sim_res_dir, paste0(group_keys$inflow_fl)))
    })

  # write the outflows to within the sim_res_dir
  outflow_files <- tibble(
    outflow_fl = strsplit(glmtools::get_nml_value(nml_obj, 'outflow_fl'), split=',')[[1]],
    release_type = stringr::str_extract(outflow_fl, pattern='(?<=in/out_).+(?=.csv)')
  )
  releases_obj %>%
    filter(time >= start_date, time <= stop_date) %>%
    left_join(outflow_files, by='release_type') %>%
    group_by(release_type, outflow_fl) %>%
    dplyr::group_walk(function(release_data, group_keys) {
      write_csv(
        release_data %>% select(time, flow),
        file = file.path(sim_res_dir, paste0(group_keys$outflow_fl)))
    })

  # munge the clarity (kw) data and write to sim_res_dir
  # start <- get_nml_value(nml_obj, 'start') %>% as.Date()
  # stop <- get_nml_value(nml_obj, 'stop') %>% as.Date()
  # kw_data %>%
  #   filter(res_id == !!res_id, time >= start & time <= stop) %>%
  #   select(time, Kd) %>%
  #   mutate(Kd = median(Kd, na.rm = TRUE)) %>%
  #   readr::write_csv(file.path(sim_res_dir, 'Kw_file.csv'))

  # run GLM and write output, all in sim_res_dir
  # if(!any(grepl('3.1.0', GLM3r::glm_version()))) {
  #   warning("You need cutting-edge GLM3 (>3.0.5). Try remotes::install_github('GLEON/GLM3r@GLMv.3.1.0a3')")
  # }
  glm_time <- system.time({
    glm_code <- GLM3r::run_glm(sim_res_dir, verbose = FALSE)
  })[['elapsed']]
  message(sprintf('GLM ran in %0.0f:%02.0f (M:S)', floor(glm_time/60), glm_time %% 60))
  if(glm_code != 0) {
    warning(sprintf("GLM code for %s suggests a problem: %s", res_id, glm_code))
    return(tibble(
      run_date = Sys.time(),
      res_id = res_id,
      glm_time_s = glm_time,
      glm_success = FALSE,
      glm_code = glm_code,
      glm_hash = NA))#,
      # export_fl = NA,
      # export_fl_hash = NA))
  }

  #' extract temperature and ice data at fixed depths (every 0.5m) and write to
  #' a feather file
  export_temp_ice_data(sim_res_dir, export_fl)

  # return a 1-row tibble of information about this model run
  return(tibble(
    run_date = Sys.time(),
    res_id = res_id,
    glm_time_s = glm_time,
    glm_success = TRUE,
    glm_code = glm_code,
    glm_hash = dir(file.path(sim_res_dir, 'out'), full.names = TRUE) %>%
      purrr::map_chr(tools::md5sum) %>% # hash each of the output files in the sim directory
      digest::digest('sha1') # combine the file hashes into a single object hash
    )) #,
    # export_fl = export_fl,
    # export_fl_hash = tools::md5sum(export_fl)))
}

