write_glm3_nml_files <- function(nml_list_rds, nml_edits, site_ids, base_nml){
  nml_list <- readRDS(nml_list_rds)

  if(!dir.exists(dirname(nml_hashes_yml))) dir.create(dirname(nml_hashes_yml), recursive=TRUE)

  # nml_files <-
  nml_objs <- nml_list[site_ids] %>%
    purrr::imap(function(nml_args, site_id) {

      # make any manual edits specified in 1_fetch
      edits <- nml_edits[[site_id]]
      for(ed in names(edits)) {
        nml_args[[ed]] <- edits[[ed]]
      }

      # calculate, add, and remove arguments
      nml_args <- append(nml_args, list(
        sim_name = gsub('nhdhr', nml_args$site_name, nml_args$site_id),
        nsave = 24, # use nsave = 24 for daily output or 1 for hourly
        start = '1979-01-02',
        stop = '2020-12-11',
        max_layers = max(30, ceiling(7 * nml_args$lake_depth)),
        bsn_vals = length(nml_args$H),
        the_depths = c(0, floor(nml_args$lake_depth * 100)/100)
      ))
      nml_args$site_name <- nml_args$site_id <- NULL # removed after copying this info to sim_name above

      # Cap the number of H and A values at 200 apiece because if I don't subset, I get errors like this when running GLM:
      #   Error. H and A in morphometry must be monotonically increasing
      #   A[870] = 16266.790000; A[871] = 16322.530000; H[870] = 336.380320; H[871] = 8.000000
      # (even though H[871] is not actually 8.000 in the nml_args)
      if(nml_args$bsn_vals > 200) {
        morph <- tibble(H=nml_args$H, A=nml_args$A) %>%
          slice(round(seq(1, n(), length.out=200))) %>%
          arrange(H)
        nml_args$H <- morph$H
        nml_args$A <- morph$A
        nml_args$bsn_vals = length(nml_args$H)
      }

      # merge into base nml, checking arguments along the way
      nml_template <- read_nml(base_nml)
      nml_obj <- set_nml(nml_template, arg_list = nml_args)

      # return the nml object
      return(nml_obj)
    })

  # return the list of nml objects
  return(nml_objs)
}