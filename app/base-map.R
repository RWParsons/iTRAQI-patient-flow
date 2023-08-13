# base-map.R

base_map <- function(map_bounds, facilities, iTRAQI_paths, polyline_paths, observed_paths, observed_polyline_paths) {
  if ("base-map.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "base-map.rds")))
  }

  # load palettes and acute raster from iTRAQI GitHub repo
  source(file.path(here::here(), "app", "palettes.R"))

  acute_raster <- get_iTRAQI_raster()

  sample_town_points <- unique(iTRAQI_paths$town_point)
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
    addProviderTiles(
      "CartoDB.VoyagerOnlyLabels",
      options = leafletOptions(pane = "maplabels"),
      group = "map labels"
    ) |>
    hideGroup(facilities$grp_id) |>
    hideGroup(polyline_paths$grp_id) |>
    hideGroup(str_replace(sample_pu_ids, "ID-", "PL-obs")) |>
    addCircleMarkers(
      layerId = iTRAQI_paths$town_point,
      group = iTRAQI_paths$town_point,
      lng = iTRAQI_paths$xcoord,
      lat = iTRAQI_paths$ycoord,
      radius = 2,
      fillOpacity = 0,
      fillColor = "blue",
      popup = iTRAQI_paths$popup
    ) |>
    addCircleMarkers(
      group = facilities$grp_id,
      lng = facilities$xcoord,
      lat = facilities$ycoord,
      radius = 3,
      fillOpacity = 0.2,
      fillColor = "black",
      color = "black"
    ) |>
    addCircleMarkers(
      layerId = as.numeric(str_remove(observed_paths$pu_id, "ID-")),
      group = observed_paths$pu_id,
      lng = observed_paths$xcoord,
      lat = observed_paths$ycoord,
      radius = 2,
      fillOpacity = 0,
      fillColor = "red",
      color = "red",
      popup = observed_paths$popup
    ) |>
    addCircleMarkers(
      layerId = as.numeric(str_remove(observed_paths$pu_id, "ID-")),
      group = paste0("traveltime-", observed_paths$pu_id),
      lng = observed_paths$xcoord,
      lat = observed_paths$ycoord,
      radius = 2,
      fillOpacity = 0,
      fillColor = palNum(observed_paths$total_time),
      color = palNum(observed_paths$total_time),
      popup = observed_paths$popup
    ) |>
    addRasterImage(
      data = acute_raster,
      x = raster::raster(acute_raster, layer = 1),
      group = "acute_raster",
      colors = palNum
    )

  pb <- progress_bar$new(total = length(sample_town_points))

  cat("Adding polylines for iTRAQI paths\n\n")
  for (tp in sample_town_points) {
    polyline_select <- polyline_paths |>
      filter(town_point == tp) |>
      select(town_point, xcoord, ycoord, transport_col, grp_id) |>
      na.omit()

    for (r in 1:(nrow(polyline_select) - 1)) {
      polyline_select_single_path <- polyline_select[c(r, r + 1), ]

      map <- map |>
        addPolylines(
          group = polyline_select_single_path$grp_id[1],
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
          group = str_replace(pu_id_, "ID-", "PL-obs"),
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
