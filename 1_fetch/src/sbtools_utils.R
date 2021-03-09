#' Login to ScienceBase using the "cidamanager" user credentials from dssecrets package
#'
#' @details To use `dssecrets`, ensure you have file system access to the cidamanager
#' secret among the files where `dssecrets` is installed.
#' If you prefer, you can bypass `dssecrets` by manually calling
#' sbtools::authenticate_sb('[my_sb_user]'),
#' provided your account has permission to access the SB item(s).
sb_secret_login <- function(){
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
}

#' @return The read-in contents of `status_file`: a tibble with columns
#'   `filename` (as on SB), `date_uploaded` (as on SB), `date_checked` (time of
#'   query to SB), and `fetch_datestamp` (date_uploaded of the file on SB at the
#'   time we fetched it into this repo)
read_sb_status <- function(status_file) {
  read_csv(status_file, col_types = cols(filename=col_character(), .default=col_datetime()))
}


#' Writes or modifies a csv file with columns `filename` (as on SB),
#' `date_uploaded` (as on SB), `date_checked` (time of query to SB), and
#' `fetch_datestamp` (date_uploaded of the file on SB at the time we fetched it
#' into this repo)
#' @param sb_id the ID of the SB item whose file information we want to query
#' @param status_file the status information file to create/modify
#' @param wait_interval time to wait before querying SB again. a difftime
#'   object, e.g., `as.difftime(1, units='days')`
#' @param update_files vector of filenames to update, or missing to update all
#' @param ignore_files vector of filenames to omit from the status tibble
update_sb_status <- function(sb_id, status_file, wait_interval, update_files, ignore_files=c()) {
  # get some starting status info into memory, reading in the previous status
  # information if available
  status <- if(file.exists(status_file)) {
    read_sb_status(status_file) %>%
      filter(!filename %in% ignore_files)
  } else {
    if(!dir.exists(dirname(status_file))) dir.create(dirname(status_file), recursive=TRUE)
    tibble(filename='', date_uploaded=Sys.time(), date_checked=Sys.time(), date_fetched=Sys.time(), fetch_datestamp=Sys.time())[c(),]
  }

  # decide whether we have to do the update
  has_update_files <- !missing(update_files)
  prev_date_checked <- status %>% # find the earliest check date of those files
    {if(has_update_files) { filter(., filename %in% update_files) } else .} %>%
    pull(date_checked) %>%
    {suppressWarnings(min(., na.rm = TRUE))}
  if(nrow(status) == 0 || (has_update_files && length(update_files) == 0) || (Sys.time() - prev_date_checked) > wait_interval) {

    # we need it, so query SB for status of all files (couldn't request info file-by-file without adding API calls)
    message('Updating SB file status')
    sb_secret_login()
    new_status <- as.sbitem(sb_id)$files %>%
      purrr::map_df(function(file_info) tibble::as_tibble(file_info[c('name','dateUploaded')])) %>%
      rename(filename = name, date_uploaded = dateUploaded) %>%
      filter(!filename %in% ignore_files) %>%
      mutate(date_uploaded = as.POSIXct(date_uploaded, format='%Y-%m-%dT%H:%M:%SZ'), date_checked = Sys.time())

    # adjust the status information based on the new query results
    if(missing(update_files)) {
      update_files <- new_status$filename
    }
    updates <- status %>%
      select(filename, fetch_datestamp) %>%
      full_join(new_status, by='filename') %>%
      filter(filename %in% update_files)
    out_status <- status %>%
      filter(!(filename %in% update_files)) %>%
      bind_rows(updates) %>%
      arrange(filename)

    # write the revised file
    write_csv(out_status, status_file)

  } else {
    message('Using old SB file status')
  }

  return(status_file)
}

get_sb_outdated <- function(status_file) {
  outdated <- read_sb_status(status_file) %>%
    # I tried but can't find a hash field in the SB file info, so we'll use dateUploaded to determine currentness
    mutate(outdated = (is.na(fetch_datestamp) | date_uploaded != fetch_datestamp)) %>%
    filter(outdated) %>%
    pull(filename)
  # if(length(outdated) > 0) message("Outdated:\n  ", paste(outdated, collapse='\n  '))
  outdated
}

sb_download_if_needed <- function(sb_id, names, destinations, outdated, status_file) {

  # if the files aren't outdated, don't change them
  to_download <- !file.exists(destinations) | (names %in% outdated)
  if(!any(to_download)) {
    message('Skipping re-download of ', paste(names, collapse=', '))
    return(destinations)
  }

  sb_secret_login()
  item_file_download(
    sb_id = sb_id,
    names = names[to_download],
    destinations = destinations[to_download],
    overwrite_file = TRUE)

  # update the status information for just these files (hidden side effect of the function; needs to happen at ~ same time as download)
  update_sb_status(sb_id, status_file, wait_interval = as.difftime(0, units = 'secs'), update_files = names[to_download])
  read_sb_status(status_file) %>%
    mutate(
      date_fetched = case_when(filename %in% names[to_download] ~ Sys.time(), TRUE ~ date_fetched),
      fetch_datestamp = case_when(filename %in% names[to_download] ~ date_uploaded, TRUE ~ fetch_datestamp)
    ) %>%
    write_csv(status_file)

  # return the filenames of the data files
  return(destinations)
}
