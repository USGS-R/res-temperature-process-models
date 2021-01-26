hash_files <- function(files, out_yml) {
  # hash the files into a list
  info_list <- files %>%
    purrr::map_chr(~ unname(tools::md5sum(.x))) %>%
    as.list()

  # save the list as a .yml file
  if(!dir.exists(dirname(out_yml))) dir.create(dirname(out_yml), recursive=TRUE)
  readr::write_lines(yaml::as.yaml(info_list), out_yml)
  return(out_yml)
}
