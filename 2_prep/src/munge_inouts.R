munge_inouts <- function(inout_feather, res_id) {
  arrow::read_feather(inout_feather) %>%
    filter(res_id == !!res_id) %>%
    select(direction, seg_id_nat, date, flow = seg_outflow, temp = seg_tave_water) %>%
    return()
}

get_names <- function(x) {
  return(names(x))
}
