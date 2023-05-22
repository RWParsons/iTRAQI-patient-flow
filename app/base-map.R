# base-map.R

base_map <- function(map_bounds, facilities, iTRAQI_paths) {
  leaflet(options = leafletOptions(minZoom = 5)) |> 
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
    addCircleMarkers(
      layerId = iTRAQI_paths$town_point,
      lng = iTRAQI_paths$xcoord, 
      lat = iTRAQI_paths$ycoord,
      radius = 2, 
      fillOpacity = 0,
      popup = iTRAQI_paths$popup
    ) |> 
    addCircleMarkers(
      layerId = paste0("F", facilities$FCLTY_ID),
      group = paste0("F", facilities$FCLTY_ID),
      lng = facilities$xcoord,
      lat = facilities$ycoord,
      radius = 3, 
      fillOpacity = 0.2,
      color = "red"
    )
}
