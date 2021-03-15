munge_inouts_sntemp <- function(inout_feather, res_id) {
  arrow::read_feather(inout_feather) %>%
    filter(res_id == !!res_id) %>%
    # PRMS exports as CFS, but delaware-model-prep::gather_sntemp_output() has already converted to CMS for us
    select(direction, seg_id_nat, time = date, flow = seg_outflow, temp = seg_tave_water) %>%
    return()
}

munge_releases <- function(releases_csv, res_id) {
  readr::read_csv(releases_csv, col_types=cols()) %>%
    filter(reservoir == names(res_id)) %>%
    mutate(flow = release_volume_cfs * 0.028316847) %>% # convert from CFS to CMS
    select(release_type, time = date, flow)
}

munge_diversions <- function(diversions_csv, res_id) {

}
