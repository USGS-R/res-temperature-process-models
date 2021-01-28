#' @param meteo_fl filepath+name of the raw meteo file
#' @param n_days the total (maximum) number of days of meteo data to read in.
#'   This argument compensates for a known bug in lake-temperature-model-prep
#'   where sometimes meteo files are mysteriously too long; see
#'   https://github.com/USGS-R/lake-temperature-model-prep/issues/134
#' @return tibble of meteo data
munge_meteo <- function(meteo_fl, n_max = 14975){
  readr::read_csv(meteo_fl, col_types = 'Dddddddd', n_max = n_max) %>%
    mutate(Rain = case_when(Snow > 0 ~ 0, TRUE ~ Rain))
}
