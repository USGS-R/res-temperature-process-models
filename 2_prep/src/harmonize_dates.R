find_shared_date_range <- function(meteo, inouts, releases) {
  # find the unique date ranges for each data file type
  meteo_range <- purrr::map_df(meteo, ~ tibble(start=min(.$time), stop=max(.$time))) %>% distinct()
  inouts_range <- purrr::map_df(inouts, ~ tibble(start=min(.$date), stop=max(.$date))) %>% distinct()
  releases_range <- purrr::map_df(releases, ~ tibble(start=min(.$date), stop=max(.$date))) %>% distinct()

  # we could support different date ranges for each reservoir, but since we're
  # not currently doing that, at least notice when date ranges differ so we can
  # deal with it then.
  stopifnot(nrow(meteo_range) == 1 && nrow(inouts_range) == 1 && nrow(releases_range) == 1)

  # find the date range for which data exists in all files
  bind_rows(meteo_range, inouts_range, releases_range) %>%
    summarize(start = max(start), stop = min(stop)) %>%
    return()
}
