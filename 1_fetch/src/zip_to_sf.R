# unzip a zipped shapefile into a tempdir, read it as sf, and return the sf object
zip_to_sf <- function(zipfile) {
  # prepare directories
  zippath <- normalizePath(zipfile)
  unzipdir <- tempfile()
  dir.create(unzipdir)

  # change directories
  oldwd <- getwd()
  setwd(unzipdir)
  on.exit(setwd(oldwd))

  # do the unzipping
  unzip(zippath)
  unzipped <- dir(unzipdir)

  # read in and return the shapefile
  unzipped_shp <- grep('\\.shp$', unzipped, value=TRUE)
  shp <- sf::read_sf(unzipped_shp)
}
