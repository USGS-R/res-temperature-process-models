#' Learn where output files can be found
#' @param sim_dir the simulation directory for a single reservoir and single GLM
#'   model run
#' @param nml_obj an NML object that can be read with glmtools
#' @param file_type just one option from c('lake','depthwise','outflow','overflow')
locate_out_files <- function(sim_dir, nml_obj, file_type=c('lake','depthwise','outflow','overflow')) {
  file_type <- match.arg(file_type)
  out_subdir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_dir <- file.path(sim_dir, out_subdir)
  out_files <- switch(
    file_type,
    'lake' = paste0(glmtools::get_nml_value(nml_obj, 'csv_lake_fname'), '.csv'),
    'depthwise' = paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc'),
    'outflow' = dir(out_dir, pattern=glmtools::get_nml_value(nml_obj, 'csv_outlet_fname')),
    'overflow' = paste0(glmtools::get_nml_value(nml_obj, 'csv_ovrflw_fname'), '.csv'))
  file.path(out_dir, out_files)
}

#' Learn where output files can be found
#' @param sim_dir the simulation directory for a single reservoir and single GLM
#'   model run
#' @param nml_obj an NML object that can be read with glmtools
#' @param file_type just one option from c('inflow','outflow')
locate_in_files <- function(sim_dir, nml_obj, file_type=c('meteo','inflow','outflow')) {
  file_type <- match.arg(file_type)
  in_files <- switch(
    file_type,
    'meteo' = glmtools::get_nml_value(nml_obj, 'meteo_fl'),
    'inflow' = strsplit(glmtools::get_nml_value(nml_obj, 'inflow_fl'), split=',')[[1]],
    'outflow' = strsplit(glmtools::get_nml_value(nml_obj, 'outflow_fl'), split=',')[[1]])
  file.path(sim_dir, in_files)
}

#' Extract temperature and ice data at fixed depths (every 0.5m) and write to a
#' feather file
#' @param sim_dir the simulation directory for a single reservoir and single GLM
#'   model run
#' @param nml_obj an NML object that can be read with glmtools
#' @param export_fl feather filename where the exported data should be written
export_temp_ice_data <- function(sim_dir, nml_obj, export_fl) {
  nc_filepath <- locate_out_files(sim_dir, nml_obj, file_type='depthwise')

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
}

