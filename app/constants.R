# constants.R

fixtures_path <- file.path(here::here(), "app", "fixtures")

map_bounds <- list(
  lng1 = 200,
  lat1 = -100,
  lng2 = 250,
  lat2 = -100
)

clean_transport_mode <- function(x) {
  x |>
    toupper() |>
    str_remove("\\/") |>
    (\(x) {
      ifelse(str_detect(x, "AMBULANCE"), "QAS", x)
    })()
}
