#' Learn where input files can be found
#' @param sim_res_dir the simulation directory for a single reservoir and single GLM
#'   model run
#' @param file_type just one option from those offered as defaults
locate_in_files <- function(sim_res_dir, file_type=c('nml', 'meteo', 'inflow', 'outflow')) {
  file_type <- match.arg(file_type)
  nml_filename <- 'glm3.nml'
  nml_obj <- file.path(sim_res_dir, nml_filename) %>% glmtools::read_nml()
  in_files <- switch(
    file_type,
    'nml' = nml_filename,
    'meteo' = glmtools::get_nml_value(nml_obj, 'meteo_fl'),
    'inflow' = strsplit(glmtools::get_nml_value(nml_obj, 'inflow_fl'), split=',')[[1]],
    'outflow' = strsplit(glmtools::get_nml_value(nml_obj, 'outflow_fl'), split=',')[[1]])
  file.path(sim_res_dir, in_files)
}

#' Learn where output files can be found
#' @param sim_res_dir the simulation directory for a single reservoir and single
#'   GLM model run
#' @param file_type just one option from those offered as defaults
locate_out_files <- function(sim_res_dir, file_type=c('lake','depthwise','outflow','overflow')) {
  file_type <- match.arg(file_type)
  nml_obj <- locate_in_files(sim_res_dir, 'nml') %>% glmtools::read_nml()
  out_subdir <- glmtools::get_nml_value(nml_obj, arg_name = 'out_dir')
  out_dir <- file.path(sim_res_dir, out_subdir)
  out_files <- switch(
    file_type,
    'lake' = paste0(glmtools::get_nml_value(nml_obj, 'csv_lake_fname'), '.csv'),
    'depthwise' = paste0(glmtools::get_nml_value(nml_obj, 'out_fn'), '.nc'),
    'outflow' = dir(out_dir, pattern=glmtools::get_nml_value(nml_obj, 'csv_outlet_fname')),
    'overflow' = paste0(glmtools::get_nml_value(nml_obj, 'csv_ovrflw_fname'), '.csv'))
  file.path(out_dir, out_files)
}

#' Extract temperature and ice data at fixed depths (every 0.5m) and write to a
#' feather file
#' @param sim_res_dir the simulation directory for a single reservoir and single
#'   GLM model run
#' @param export_fl feather filename where the exported data should be written
export_temp_ice_data <- function(sim_res_dir, export_fl) {

  nml_obj <- locate_in_files(sim_res_dir, 'nml') %>% glmtools::read_nml()
  nc_filepath <- locate_out_files(sim_res_dir, file_type='depthwise')

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

get_outlet_info <- function(sim_res_dir) {

  nml_obj <- locate_in_files(sim_res_dir, 'nml') %>% glmtools::read_nml()
  keepers <- seq_len(glmtools::get_nml_value(nml_obj, 'num_outlet'))
  outlet_info <-
    tibble(
      outlet_outfl = locate_out_files(sim_res_dir, file_type='outflow')[keepers],
      outlet_id = outlet_outfl %>% basename() %>%
        str_extract('(?<=outlet_)(.*)(?=.csv)') %>% as.integer(),
      outlet_name = locate_in_files(sim_res_dir, file_type='outflow')[keepers] %>%
        basename() %>% str_extract('(?<=out_)(.*)(?=.csv)'),
      # outflow_factor = glmtools::get_nml_value(nml_obj, 'outflow_factor')[keepers],
      outlet_type = glmtools::get_nml_value(nml_obj, 'outlet_type')[keepers],
      outlet_elev = glmtools::get_nml_value(nml_obj, 'outl_elvs')[keepers]) %>%
    mutate(
      outlet_name = ordered(outlet_name, outlet_name[outlet_id+1]),
      outlet_label = sprintf('%s (%02d)', outlet_name, outlet_id))

  return(outlet_info)
}

#' Read in the predictions of flow and temperature at the outlets
#' @param sim_res_dir the simulation directory for a single reservoir and single
#'   GLM model run
get_outlet_preds <- function(sim_res_dir) {

  # get outlet info
  outlets <- get_outlet_info(sim_res_dir)

  # get flows and temps at the outlets
  nc_file <- locate_out_files(sim_res_dir, file_type='depthwise')
  outlet_preds <- if(nrow(outlets) > 0) {
    purrr::pmap_dfr(
      outlets,
      function(outlet_outfl, outlet_label, outlet_id, ...) {
        read_csv(outlet_outfl, col_types=cols()) %>%
          mutate(
            time = as.Date(time),
            flow = flow / (24*60*60), # I can't find units documentation for this, but the data in outlet_xx.csv sure seem to be off by sec/day=86400
            temp = na_if(temp, -9999)) %>%
          mutate(outlet_label = outlet_label, .before=1)
      })
  } else {
    tibble(outlet_label = '', time = Sys.Date(), flow = 0.0, temp = 0.0)[c(),]
  }

  return(outlet_preds)
}

#' Compile/compute predicted and observed temperatures downstream of the reservoir
get_downstream_predobs <- function(sim_res_dir, inouts, obs_inouts) {

  # Compute GLM predictions for temperatures downstream of the reservoir, based on
  # outlet_xx.csv files
  outlet_preds_glm <- get_outlet_preds(sim_res_dir) %>%
    group_by(time) %>%
    summarize(
      temp = sum((flow[!is.na(temp)] * temp[!is.na(temp)]))/sum(flow[!is.na(temp)]),
      flow = sum(flow),
      .groups='drop') %>%
    mutate(id = 'outlets')

  # Get SNTemp (or reservoir-free PGDL) predictions
  outlet_preds_inouts <- inouts %>%
    filter(location == 'outflow') %>%
    select(id = seg_id_nat, time, flow, temp)

  # Get observations downstream of the reservoir
  outlet_obs <- obs_inouts %>%
    filter(location == 'outflow') %>%
    select(id = site_id, time, flow, temp) %>%
    # get rid of duplicates (27 for cannonsville)
    group_by(time, id) %>%
    summarize(n = n(), temp = if(any(!is.na(temp))) mean(temp[!is.na(temp)]) else NA, flow = mean(flow), .groups = 'drop')
  # filter(outlet_obs, n > 1) # here's how to inspect those duplicates

  # Combine GLM preds, reservoir-free model preds, and obs temperatures
  outlet_predobs <- bind_rows(
    glm = outlet_preds_glm,
    nores = outlet_preds_inouts,
    obs = outlet_obs,
    .id = 'source') %>%
    mutate(
      source_id = {
        source_id <- paste(source, id, sep='_')
        source_id_vals <- unique(source_id)
        source_id_levels <- purrr::map(
          c('obs','nores','glm'),
          ~ which(!is.na(str_match(source_id_vals, .x)[,1]))) %>%
          flatten_int() %>% source_id_vals[.]
        ordered(source_id, source_id_levels)
      })

  return(outlet_predobs)
}
