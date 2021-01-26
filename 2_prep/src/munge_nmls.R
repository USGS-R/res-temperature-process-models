write_glm3_nml_files <- function(nml_list_rds, site_ids, base_nml, nml_dir, nml_hashes_yml){
  nml_list <- readRDS(nml_list_rds)
  nml_base <- read_nml(base_nml)

  if(!dir.exists(nml_dir)) dir.create(nml_dir, recursive=TRUE)
  if(!dir.exists(dirname(nml_hashes_yml))) dir.create(dirname(nml_hashes_yml), recursive=TRUE)

  nml_files <- nml_list[site_ids] %>%
    purrr::imap(function(nml_args, site_id) {

      # calculate, add, and remove arguments
      nml_args <- append(nml_args, list(
        sim_name = nml_args$site_id,
        nsave = 1,
        start = '1979-04-01',
        stop = '2018-12-31',
        max_layers = max(30, ceiling(7 * nml_args$lake_depth)),
        bsn_vals = length(nml_args$H),
        the_depths = c(0, floor(nml_args$lake_depth * 100)/100)
      ))
      nml_args$site_id <- NULL # removed after copying this info to sim_name above

      # merge into base nml, checking arguments along the way
      nml_obj <- set_nml(nml_base, arg_list = nml_args)

      # write this site-specific nml file
      file_out <- file.path(nml_dir, paste0(nml_args$sim_name, '_glm3.nml'))
      write_nml(glm_nml = nml_obj, file = file_out)
      return(file_out)
    })

  # write a file summarizing the nml files; return the name of the summary file
  hash_files(files = nml_files, out_yml = nml_hashes_yml)
}
