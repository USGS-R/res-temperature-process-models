# time, flow, and temp are the column names expected by GLM


munge_inouts_sntemp <- function(inout_feather, res_id) {
  arrow::read_feather(inout_feather) %>%
    # PRMS exports as CFS, but delaware-model-prep::gather_sntemp_output() has already converted to CMS for us
    select(res_id, location = direction, seg_id_nat, time = date, flow = seg_outflow, temp = seg_tave_water) %>%
    subset_to_res_id(res_id)
}

munge_inouts_obs <- function(inout_feather, res_id) {
  arrow::read_feather(inout_feather) %>%
    select(res_id, location, site_id, time = date, flow = flow_cms, temp = mean_temp_degC) %>%
    subset_to_res_id(res_id)
}

munge_releases <- function(releases_csv, res_id) {
  readr::read_csv(releases_csv, col_types=cols()) %>%
    left_join(tibble(res_id = res_id, reservoir = names(res_id)), by='reservoir') %>% # add reservoir IDs
    mutate(flow = release_volume_cfs * 0.028316847) %>% # convert from CFS to CMS
    select(res_id, release_type, time = date, flow) %>%
    subset_to_res_id(res_id)
}

munge_diversions <- function(diversions_csv, res_id) {

}


# Both list_by_res_id and subset_to_res_id are helpers to munge_ functions. See
# https://github.com/USGS-R/res-temperature-process-models/issues/3 for
# discussion of approaches that use either list_by_res_id or subset_to_res_id;
# I'm currently running with subset_to_res_id approaches but might want to
# switch again later

#' Use a res_id column in df to group all the other data in df by res_id, filter
#' to just those ids in res_ids, and produce a list of the res_id-based groups
#' @param df tibble to group
#' @param res_ids vector of reservoir IDs to keep
list_by_res_id <- function(df, res_ids) {
  df %>%
    filter(res_id %in% !!res_ids) %>%
    tidyr::nest(data = -res_id) %>%
    mutate(data = purrr::set_names(data, res_id)) %>%
    pull(data)
}
#' Use a res_id column in df to subset to just one res_id and then remove that
#' column, returning the rest
#' @param res_id single reservoir ID to keep
subset_to_res_id <- function(df, res_id) {
  df %>%
    filter(res_id == !!res_id) %>%
    select(-res_id)
}
