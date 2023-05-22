# constants.R

map_bounds <- list(
  lng1 = 115,
  lat1 = -45.00,
  lng2 = 170,
  lat2 = -5
)

fixtures_path <- "app/fixtures"

process_iTRAQI_paths <- function(iTRAQI_paths) {
  iTRAQI_paths |> 
    mutate(
      n_legs = ifelse(destination1 == destination2, 1, 2),
      across(c(destination1, destination2), str_to_title),
      destinations_popup = ifelse(n_legs == 1, 
                                  glue::glue("Destination: {destination1} ({transport_mode1})"),
                                  glue::glue("Destination 1: {destination1} ({transport_mode1})<br>",
                                             "Destination 2: {destination2} ({transport_mode2})")),
      popup = glue::glue(
        "<b>{town_name}</b><br>",
        "Travel legs: {n_legs}<br>",
        "{destinations_popup}<br>",
        "Travel time: {total_transport_time_min} mins"
      ))
}


process_facilities <- function(facilities) {
  facilities |> 
    filter(!is.na(Latitude), !is.na(Longitude)) |> 
    rename(xcoord = Longitude, ycoord = Latitude)
}

process_polyline_paths <- function(iTRAQI_paths, facilities) {
  
  if ("polylines_df.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "polylines_df.rds")))
  }
  polylines_df <- lapply(
    unique(iTRAQI_paths$town_point),
    \(x) {
      iTRAQI_paths |> 
        filter(town_point == x) |> 
        select(town_point, destination1, destination2) |>
        pivot_longer(!town_point, names_to = "destination_order", values_to = "destination") |> 
        mutate(destination = toupper(destination)) |> 
        left_join(
          select(facilities, FACILITY_NAME_Clean, FCLTY_ID, xcoord, ycoord), 
          by = c("destination" = "FACILITY_NAME_Clean")
        ) |> 
        bind_rows(select(filter(iTRAQI_paths, town_point == x), town_point, xcoord, ycoord))
    }
  ) |> 
    (\(x) {do.call("rbind", x)})()
  
  saveRDS(polylines_df, file.path(fixtures_path, "polylines_df.rds"))
  
  polylines_df
}
