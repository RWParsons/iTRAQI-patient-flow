# base-map.R

base_map <- function(map_bounds, facilities, iTRAQI_paths, polyline_paths, observed_paths, observed_polyline_paths) {
  if ("base-map.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "base-map.rds")))
  }

  sample_town_points <- unique(iTRAQI_paths$town_point) # [1:50]
  sample_pu_ids <- unique(observed_paths$pu_id)

  iTRAQI_paths <- filter(iTRAQI_paths, town_point %in% sample_town_points)

  map <- leaflet(options = leafletOptions(minZoom = 5)) |>
    setMaxBounds(
      lng1 = map_bounds$lng1, lat1 = map_bounds$lat1,
      lng2 = map_bounds$lng2, lat2 = map_bounds$lat2
    ) |>
    addMapPane(name = "layers", zIndex = 200) |>
    addMapPane(name = "maplabels", zIndex = 400) |>
    addMapPane(name = "markers", zIndex = 206) |>
    addProviderTiles("CartoDB.VoyagerNoLabels") |>
    addProviderTiles("CartoDB.VoyagerOnlyLabels",
      options = leafletOptions(pane = "maplabels"),
      group = "map labels"
    ) |>
    hideGroup(paste0("F", facilities$FCLTY_ID)) |>
    hideGroup(paste0("PL", polyline_paths$town_point)) |>
    hideGroup(paste0("PL-obs", sample_pu_ids)) |>
    addCircleMarkers(
      layerId = iTRAQI_paths$town_point,
      lng = iTRAQI_paths$xcoord,
      lat = iTRAQI_paths$ycoord,
      radius = 2,
      fillOpacity = 0,
      fillColor = "blue",
      popup = iTRAQI_paths$popup
    ) |>
    addCircleMarkers(
      group = paste0("F", facilities$FCLTY_ID),
      lng = facilities$xcoord,
      lat = facilities$ycoord,
      radius = 3,
      fillOpacity = 0.2,
      fillColor = "red",
      color = "red"
    ) |> 
    addCircleMarkers(
      layerId = observed_paths$pu_id,
      lng = observed_paths$xcoord,
      lat = observed_paths$ycoord,
      radius = 2,
      fillOpacity = 0,
      fillColor = "orange",
      color = "orange",
      popup = observed_paths$popup
    )

  pb <- progress_bar$new(total = length(sample_town_points))

  cat("Adding polylines for iTRAQI paths\n\n")
  for (tp in sample_town_points) {
    polyline_select <- polyline_paths |>
      filter(town_point == tp) |>
      select(town_point, xcoord, ycoord, transport_col) |>
      na.omit()

    for (r in 1:(nrow(polyline_select) - 1)) {
      polyline_select_single_path <- polyline_select[c(r, r + 1), ]
      map <- map |>
        addPolylines(
          group = paste0("PL", tp),
          lng = polyline_select_single_path$xcoord,
          lat = polyline_select_single_path$ycoord,
          col = polyline_select_single_path$transport_col[2]
        )
    }
    pb$tick()
  }
  
  cat("\n\nAdding polylines for observed data\n\n")
  pb <- progress_bar$new(total = length(sample_pu_ids))
  
  for (pu_id_ in sample_pu_ids) {
      polyline_select <- observed_polyline_paths |>
        filter(pu_id == pu_id_) |>
        select(pu_id, xcoord, ycoord, transport_col) |>
        na.omit()
      # if(nrow(polyline_select) <= 1) next
      
      for (r in 1:(nrow(polyline_select) - 1)) {
        
        polyline_select_single_path <- polyline_select[c(r, r + 1), ]
        map <- map |>
          addPolylines(
            group = paste0("PL-obs", pu_id_),
            lng = polyline_select_single_path$xcoord,
            lat = polyline_select_single_path$ycoord,
            col = polyline_select_single_path$transport_col[2]
          )
      }
      pb$tick()
  }
  
  
  saveRDS(map, file.path(fixtures_path, "base-map.rds"))
  return(map)
}
