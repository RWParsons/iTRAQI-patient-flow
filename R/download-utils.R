readRDS_github <- function(url) {
  path <- tempfile(fileext = ".rds")
  utils::download.file(url, destfile = path, method = "curl")
  readRDS(path)
}
