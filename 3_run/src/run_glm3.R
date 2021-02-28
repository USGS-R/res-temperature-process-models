run_glm3_model <- function(sim_dir, site_id, nml_obj, inouts_obj, meteo_xwalk, inout_obj, export_fl){ # kw_data,
  # prepare to write inputs and results locally for quick I/O
  site_dir <- file.path(sim_dir, site_id)
  dir.create(site_dir, recursive=TRUE, showWarnings=FALSE)
  # on.exit(unlink(site_dir, recursive = TRUE))


  # write the nml to site_dir
  glmtools::write_nml(nml_obj, file.path(site_dir, 'glm3.nml'))

  # write the meteo data to site_dir
  meteo_fl <- glmtools::get_nml_value(nml_obj, arg_name = 'meteo_fl')
  meteo_obj <- tar_read_raw(meteo_xwalk$meteo_branch)
  readr::write_csv(meteo_obj, file.path(site_dir, meteo_fl))

  # munge the clarity (kw) data and write to site_dir
  # start <- get_nml_value(nml_obj, 'start') %>% as.Date()
  # stop <- get_nml_value(nml_obj, 'stop') %>% as.Date()
  # kw_data %>%
  #   filter(site_id == !!site_id, time >= start & time <= stop) %>%
  #   select(time, Kd) %>%
  #   mutate(Kd = median(Kd, na.rm = TRUE)) %>%
  #   readr::write_csv(file.path(site_dir, 'Kw_file.csv'))

  # run GLM and write output, all in site_dir
  # if(!any(grepl('3.1.0', GLM3r::glm_version()))) {
  #   warning("You need cutting-edge GLM3 (>3.0.5). Try remotes::install_github('GLEON/GLM3r@GLMv.3.1.0a3')")
  # }
  glm_time <- system.time({
    glm_code <- GLM3r::run_glm(site_dir, verbose = FALSE)
  })[['elapsed']]
  message(sprintf('GLM ran in %0.0f:%02.0f (M:S)', floor(glm_time/60), glm_time %% 60))
  if(glm_code != 0) {
    warning(sprintf("GLM code for %s suggests a problem: %s", site_id, glm_code))
    return(tibble(
      site_id = site_id,
      glm_time_s = glm_time,
      glm_success = FALSE,
      glm_code = glm_code,
      export_fl = NA,
      export_fl_hash = NA))
  }

  # learn where the output was written
  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_basename <- paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc')
  nc_filepath <- file.path(site_dir, out_dir, out_basename)

  # extract temperatures and ice estimates at regular depths
  lake_depth <- glmtools::get_nml_value(nml_obj, arg_name = 'lake_depth')
  export_depths <- seq(0, lake_depth, by = 0.5)
  temp_data <- glmtools::get_temp(nc_filepath, reference = 'surface', z_out = export_depths) %>%
    mutate(date = as.Date(lubridate::floor_date(DateTime, 'days'))) %>% select(-DateTime)
  ice_data <- glmtools::get_var(nc_filepath, var_name = 'hice') %>%
    dplyr::mutate(ice = hice > 0, date = as.Date(lubridate::ceiling_date(DateTime, 'days'))) %>%
    dplyr::select(-hice, -DateTime)

  # combine ice and temperature data and write to file
  out_dir <- dirname(export_fl)
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
  ice_data %>%
    dplyr::left_join(temp_data, ., by = 'date') %>%
    select(time = date, everything()) %>%
    feather::write_feather(export_fl)

  # return a 1-row tibble of information about this model run
  return(tibble(
    site_id = site_id,
    glm_time_s = glm_time,
    glm_success = TRUE,
    glm_code = glm_code,
    export_fl = export_fl,
    export_fl_hash = tools::md5sum(export_fl)))
}
