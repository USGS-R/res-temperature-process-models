# unzip a zipped shapefile into a tempdir, read it as sf, and return the sf object
zip_to_sf <- function(zipfile) {
  zippath <- normalizePath(zipfile)
  unzipdir <- tempfile()
  dir.create(unzipdir)
  oldwd <- setwd(unzipdir)
  on.exit(setwd(oldwd))
  unzip(zippath)
  unzipped <- dir(unzipdir)
  unzipped_shp <- grep('\\.shp$', unzipped, value=TRUE)
  shp <- sf::read_sf(unzipped_shp)
}
