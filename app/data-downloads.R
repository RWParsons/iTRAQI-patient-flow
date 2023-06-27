# data-downloads.R
source(file.path(here::here(), "app", "constants.R"))

download_iTRAQI_raster <- function() {
  githubURL <- ("https://raw.githubusercontent.com/RWParsons/iTRAQI_app/main/input/layers/acute_raster.rds")
  download.file(githubURL, file.path(fixtures_path, "acute_raster.rds"), method="curl")
  invisible(file.path(fixtures_path, "acute_raster.rds"))
}

get_iTRAQI_raster <- function() {
  if (file.exists(file.path(fixtures_path, "acute_raster.rds"))) {
    readRDS(file.path(fixtures_path, "acute_raster.rds"))
  } else {
    readRDS(download_iTRAQI_raster())
  }
}
