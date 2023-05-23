# constants.R

map_bounds <- list(
  lng1 = 115,
  lat1 = -45.00,
  lng2 = 170,
  lat2 = -5
)

fixtures_path <- "app/fixtures"

clean_transport_mode <- function(x) {
  x |>
    toupper() |>
    str_remove("\\/") |>
    (\(x) {
      ifelse(str_detect(x, "AMBULANCE"), "QAS", x)
    })()
}

process_iTRAQI_paths <- function(iTRAQI_paths) {
  iTRAQI_paths |>
    mutate(
      n_legs = ifelse(destination1 == destination2, 1, 2),
      across(c(destination1, destination2), str_to_title),
      across(c(transport_mode1, transport_mode2), clean_transport_mode),
      destinations_popup = ifelse(n_legs == 1,
        glue::glue("Destination: {destination1} ({transport_mode1})"),
        glue::glue(
          "Destination 1: {destination1} ({transport_mode1})<br>",
          "Destination 2: {destination2} ({transport_mode2})"
        )
      ),
      popup = glue::glue(
        "<b>{town_name}</b><br>",
        "Travel legs: {n_legs}<br>",
        "{destinations_popup}<br>",
        "Travel time: {total_transport_time_min} mins"
      )
    )
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

  palFac <- colorFactor("viridis", levels = 1:6, ordered = TRUE)

  polylines_df <- lapply(
    unique(iTRAQI_paths$town_point),
    \(x) {
      iTRAQI_paths |>
        filter(town_point == x) |>
        mutate(
          destination1 = paste0(destination1, "|", transport_mode1),
          destination2 = paste0(destination2, "|", transport_mode2)
        ) |>
        select(town_point, destination1, destination2) |>
        pivot_longer(!town_point, names_to = "destination_order", values_to = "destination") |>
        mutate(destination = toupper(destination)) |>
        separate(destination, c("destination", "transport_mode"), "\\|") |>
        mutate(destination = str_trim(destination)) |> 
        left_join(
          select(facilities, FACILITY_NAME_Clean, FCLTY_ID, xcoord, ycoord),
          by = c("destination" = "FACILITY_NAME_Clean")
        ) |>
        (\(path) {
          bind_rows(
            select(filter(iTRAQI_paths, town_point == x), town_point, xcoord, ycoord),
            path
          )
        })()
    }
  ) |>
    (\(x) {do.call("rbind", x)})() |>
    mutate(transport_col = palFac(as.numeric(as.factor(transport_mode))))


  saveRDS(polylines_df, file.path(fixtures_path, "polylines_df.rds"))
  polylines_df
}

process_observed_paths <- function(observed_paths) {
  observed_paths |> 
    rename(xcoord = X_COORD, ycoord = Y_COORD) |> 
    mutate(Arrival_ReferralPathway = haven::as_factor(Arrival_ReferralPathway),
           facility_and_mode = glue::glue("{FACILITY_NAME_Clean} ({Arrival_ReferralPathway})")) |> 
    group_by(pu_id) |> 
    mutate(popup = glue::glue(
      "<b>pu_id</b>: {pu_id} <br><br>",
      "<b>centres</b>: <br>{paste0(facility_and_mode[!is.na(FACILITY_NAME_Clean)], collapse = ',<br>')}"
    )) |> 
    ungroup()
}

process_observed_polyline_paths <- function(observed_paths) {
  if ("observed_polylines_df.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "observed_polylines_df.rds")))
  }
  palFac <- colorFactor("viridis", levels = 1:6, ordered = TRUE)
  observed_paths <- 
    observed_paths |> 
    rename(transport_mode = Arrival_ReferralPathway) |> 
    select(pu_id, xcoord, ycoord, transport_mode, FACILITY_NAME_Clean, FCLTY_ID) |> 
    mutate(transport_col = palFac(as.numeric(transport_mode)))
  
  saveRDS(observed_paths, file.path(fixtures_path, "observed_polylines_df.rds"))
  observed_paths
}
