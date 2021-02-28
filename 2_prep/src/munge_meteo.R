#' @param meteo_fl filepath+name of the raw meteo file
#' @param n_max the total (maximum) number of days of meteo data to read in.
#'   This argument compensates for a known bug in lake-temperature-model-prep
#'   where sometimes meteo files are mysteriously too long; see
#'   https://github.com/USGS-R/lake-temperature-model-prep/issues/134. Currenty
#'   implemented as a stopifnot rather than sending n_max to read_csv for more
#'   awareness of raw data file status.
#' @return tibble of meteo data
munge_meteo <- function(meteo_fl, n_max = 15320){
  meteo <- readr::read_csv(meteo_fl, col_types = 'Dddddddd') %>% #, n_max = n_max) %>%
    mutate(Rain = case_when(Snow > 0 ~ 0, TRUE ~ Rain))
  stopifnot(nrow(meteo) == n_max)
  return(meteo)
}


build_meteo_xwalk <- function(p2_meteo_files, p2_meteo, p1_ltmp_nml_list.rds, p2_reservoir_ids) {
  meteo_branches <- tibble(
    meteo_fl = p2_meteo_files, # file name for more human readability
    # A trick: You can't see the names of p1_meteo_files just by calling
    # tar_read(p2_meteo) at the command prompt, but you can extract them
    # into a target.
    meteo_branch = names(p2_meteo), # branch name for direct revence to just one meteo table
    meteo_data = map_chr(meteo_branch, ~ tar_meta(starts_with(.x))$data) # data hash for pipeline integrity
  )
  nml_list <- read_rds(p1_ltmp_nml_list.rds)
  site_meteo_xwalk <- tibble(
    res_id = map_chr(nml_list, 'site_id'),
    meteo_fl = map_chr(nml_list, 'meteo_fl') %>% file.path('1_fetch/out', .))
  tibble(res_id = p2_reservoir_ids) %>%
    left_join(site_meteo_xwalk, by='res_id') %>%
    left_join(meteo_branches, by='meteo_fl') %>%
    rowwise() %>% # better than group_by(res_id) if res_id isn't unique
    tar_group()
}
