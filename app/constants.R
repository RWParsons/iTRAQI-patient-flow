# constants.R

fixtures_path <- file.path(here::here(), "app", "fixtures")

map_bounds <- list(
  lng1 = 75,
  lat1 = -60,
  lng2 = 175,
  lat2 = 20
)

clean_transport_mode <- function(x) {
  x |>
    toupper() |>
    str_remove("\\/") |>
    (\(x) {
      ifelse(str_detect(x, "AMBULANCE"), "QAS", x)
    })()
}
