# constants.R

fixtures_path <- file.path(here::here(), "app", "fixtures")

map_bounds <- list(
  lng1 = 115,
  lat1 = -45.00,
  lng2 = 170,
  lat2 = -5
)

clean_transport_mode <- function(x) {
  x |>
    toupper() |>
    str_remove("\\/") |>
    (\(x) {
      ifelse(str_detect(x, "AMBULANCE"), "QAS", x)
    })()
}
