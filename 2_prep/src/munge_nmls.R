#' @param res_ids the IDs to include in the munged nml list; used to subset the
#'   nml_list_rds and nml_edits list here.
munge_nmls <- function(nml_list_rds, nml_edits, start_stop, res_ids, base_nml){
  nml_list <- readRDS(nml_list_rds)[res_ids]
  nml_edits <- nml_edits[res_ids] # this subsetting isn't strictly necessary because we'll extract elements 1x1 below, but it keeps the working data smaller

  # create the munged nml objects
  nml_objs <- nml_list %>%
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
        start = format(start_stop[['start']], '%Y-%m-%d'),
        stop = format(start_stop[['stop']], '%Y-%m-%d'),
        max_layers = max(30, ceiling(7 * nml_args$lake_depth)),
        bsn_vals = length(nml_args$H),
        the_depths = c(0, floor(nml_args$lake_depth * 100)/100)
      ))
      nml_args$site_name <- nml_args$site_id <- NULL # removed after copying this info to sim_name above
      # move the meteo filepath into the same input directory we're using for inflows and outflows
      input_dir <- unique(dirname(nml_args$outflow_fl))
      nml_args$meteo_fl <- file.path(input_dir, nml_args$meteo_fl)

      # Calculate Loutf and Woutf (vectorized for multiple outl_elvs)
      # LWoutf <- calc_LWoutf(H=nml_args$H, A=nml_args$A, Eoutf = nml_args$outl_elvs, Lcrest = nml_args$bsn_len, nml_args$bsn_wid)
      # nml_args <- append(nml_args, list(
      #   bsn_len_outl = LWoutf$L,
      #   bsn_wid_outl = LWoutf$W,
      #   outflow_factor = 1))

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
        nml_args$bsn_vals <- length(nml_args$H)
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

#' @param H vector of elevations
#' @param A vector of lake areas at elevations corresponding to H
#' @param Eoutf outflow elevation
#' @param Lcrest lake length (?) at the outflow at the crest elevation
#' @param Wcrest lake width (?) at the outflow at the crest elevation
calc_LWoutf <- function(H, A, Eoutf, Lcrest, Wcrest) {

  Aoutf <- approx(x=H, y=A, xout=Eoutf)$y # Aoutf = lake area at the outflow elevation
  Loutf <- sqrt(Aoutf*(4/pi)*(Lcrest/Wcrest)) # Eq 71 in Hipsey et al.
  Woutf <- Loutf*Wcrest/Lcrest # Eq 72 in Hipsey et al.

  return(list(L=Loutf, W=Woutf))
}

