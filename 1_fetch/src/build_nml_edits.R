build_nml_edits <- function(inout_feather, releases_csv) {

  # input dir where we'll plan to write all the input files (for inflows and outflows and meteo data)
  input_dir <- 'in'

  # summarize the inout data file for automatically populating some fields
  # below, though we may choose to revise some of those fields later (e.g., use
  # a unique strmbd_slope for each inflow)
  inout_info <- inout_feather %>%
    arrow::read_feather() %>%
    rename(location = direction) %>%
    group_by(res_id, location, seg_id_nat) %>%
    summarize(n_dates=n(), min_date=min(date), max_date=max(date), .groups='drop')
  pep_inflows <- inout_info %>% filter(res_id == 'nhdhr_151957878', location == 'inflow')
  can_inflows <- inout_info %>% filter(res_id == 'nhdhr_120022743', location == 'inflow')
  # pep_outflows <- inout_info %>% filter(res_id == 'nhdhr_151957878', location == 'outflow') # we'll use release data instead of PRMS predictions for outflows
  # can_outflows <- inout_info %>% filter(res_id == 'nhdhr_120022743', location == 'outflow') # we'll use release data instead of PRMS predictions for outflows
  release_info <- releases_csv %>%
    readr::read_csv(col_types=cols()) %>%
    group_by(reservoir, release_type) %>%
    summarize(n_dates=n(), min_date=min(date), max_date=max(date), .groups='drop')
  pep_releases <- release_info %>% filter(reservoir == 'Pepacton')
  can_releases <- release_info %>% filter(reservoir == 'Cannonsville')

  list(
    nhdhr_151957878 = list(
      site_name = 'Pepacton',
      crest_elev = 390.0, # from "spillway" in Sam's slides on 1/28/21
      bsn_len = 35000, # guess from Google Maps
      bsn_wid = 1000, # guess from Google Maps

      # Inflow info
      num_inflows    = nrow(pep_inflows),
      names_of_strms = pep_inflows$seg_id_nat,
      # subm_flag      = rep(FALSE, nrow(pep_inflows)), # default is FALSE
      strm_hf_angle  = rep(80, nrow(pep_inflows)),
      strmbd_slope   = rep(0.5, nrow(pep_inflows)),
      strmbd_drag    = rep(0.016, nrow(pep_inflows)),
      # coef_inf_entrain = 0.
      inflow_factor  = rep(1, nrow(pep_inflows)), # default should be 1 but values seem to be required anyway
      inflow_fl      = file.path(input_dir, sprintf('in_%s.csv', pep_inflows$seg_id_nat)),
      inflow_varnum  = 2,
      inflow_vars    = c('FLOW', 'TEMP'),
      time_fmt = 'YYYY-MM-DD', # default is 'YYYY-MM-DD hh:mm:ss'

      # Outflow info
      num_outlet = { stopifnot(nrow(pep_releases) == 3); 3 },
      outflow_fl = file.path(input_dir, sprintf('out_%s.csv', pep_releases$release_type)),
      # time_fmt = 'YYYY-MM-DD', # default is 'YYYY-MM-DD hh:mm:ss' # duplicated in &inflow and &outflow sections
      outflow_factor = rep(1.1, 3),
      # outflow_thick_limit = Inf?
      # single_layer_draw = FALSE
      # flt_off_sw = c(FALSE, TRUE, TRUE), # redundant with outlet_type
      outlet_type = c(1, 2, 2), # 1 = fixed outlet height (subsurface); 2 = floating offtake (over-the-top releases)
      outl_elvs = c(343, 0, 0) # 343 from "release" in Sam's slides; 390 from crest elevation, but floating offtakes are specified as depth relative to surface
      # bsn_len_outl, bsn_wid_outl # hoping GLM will calculate these for us

      # Info from Sam's slides that's not directly used
      # min_level = 376,
      # max_level = 390,
      # max_depth = 49, # lake_depth is 60 in nml
      # diversion_elev = 348
    ),
    nhdhr_120022743 = list(
      site_name = 'Cannonsville',
      crest_elev = 350.5, # from "spillway" in Sam's slides
      bsn_len = 18000, # this is a guess from Google Maps
      bsn_wid = 800, # this is a guess from Google Maps

      # Inflow info
      num_inflows    = nrow(can_inflows),
      names_of_strms = can_inflows$seg_id_nat,
      # subm_flag      = rep(FALSE, nrow(can_inflows)), # default is FALSE
      strm_hf_angle  = rep(80, nrow(can_inflows)),
      strmbd_slope   = rep(0.5, nrow(can_inflows)),
      strmbd_drag    = rep(0.016, nrow(can_inflows)),
      # coef_inf_entrain = 0.
      inflow_factor  = rep(1.5, nrow(can_inflows)), # default should be 1 but values seem to be required anyway
      inflow_fl      = file.path(input_dir, sprintf('in_%s.csv', can_inflows$seg_id_nat)),
      inflow_varnum  = 2,
      inflow_vars    = c('FLOW', 'TEMP'),
      time_fmt = 'YYYY-MM-DD', # default is 'YYYY-MM-DD hh:mm:ss'

      # Outflow info
      num_outlet = { stopifnot(nrow(can_releases) == 3); 3 },
      outflow_fl = file.path(input_dir, sprintf('out_%s.csv', can_releases$release_type)),
      # time_fmt = 'YYYY-MM-DD', # default is 'YYYY-MM-DD hh:mm:ss' # duplicated in &inflow and &outflow sections
      outflow_factor = rep(1.1, 3),
      # outflow_thick_limit = Inf?
      # single_layer_draw = FALSE
      # flt_off_sw = c(FALSE, TRUE, TRUE), # redundant with outlet_type
      outlet_type = c(1, 2, 2), # 1 = fixed outlet height (subsurface); 2 = floating offtake (over-the-top releases)
      outl_elvs = c(311, 10, 10) # 311 from "release" in Sam's slides; 350.5 from crest elevation, but floating offtakes are specified as depth relative to surface
      # bsn_len_outl, bsn_wid_outl # hoping GLM will calculate these for us

      # Info from Sam's slides that's not directly used
      # min_level = 329,
      # max_level = 351,
      # max_depth = 52, # lake_depth is 48.76 in nml
      # diversion_elev = 315m
    )
  )
}
