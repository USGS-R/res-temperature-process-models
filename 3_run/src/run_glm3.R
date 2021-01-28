run_glm3_model <- function(site_id, nml_obj, meteo_obj, export_fl){ # kw_data,
  # prepare to write inputs and results locally for quick I/O
  sim_dir <- file.path('.sim-scratch', site_id)
  dir.create(sim_dir, recursive=TRUE)
  # on.exit(unlink(sim_dir, recursive = TRUE))

  # write the nml and meteo data to sim_dir
  glmtools::write_nml(nml_obj, file.path(sim_dir, 'glm3.nml'))
  meteo_fl <- glmtools::get_nml_value(nml_obj, arg_name = 'meteo_fl')
  readr::write_csv(meteo_obj, file.path(sim_dir, meteo_fl))

  # munge the clarity (kw) data and write to sim_dir
  # start <- get_nml_value(nml_obj, 'start') %>% as.Date()
  # stop <- get_nml_value(nml_obj, 'stop') %>% as.Date()
  # kw_data %>%
  #   filter(site_id == !!site_id, time >= start & time <= stop) %>%
  #   select(time, Kd) %>%
  #   mutate(Kd = median(Kd, na.rm = TRUE)) %>%
  #   readr::write_csv(file.path(sim_dir, 'Kw_file.csv'))

  # run GLM and write output, all in sim_dir
  # if(!any(grepl('3.1.0', GLM3r::glm_version()))) {
  #   warning("You need cutting-edge GLM3 (>3.0.5). Try remotes::install_github('GLEON/GLM3r@GLMv.3.1.0a3')")
  # }
  GLM3r::run_glm(sim_dir, verbose = FALSE)

  # learn where the output was written
  out_dir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_basename <- paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc')
  nc_filepath <- file.path(sim_dir, out_dir, out_basename)

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
  if(!dir.exists(out_dir)) dir.create(out_dir)
  ice_data %>%
    dplyr::left_join(temp_data, ., by = 'date') %>%
    select(time = date, everything()) %>%
    feather::write_feather(export_fl)
}
