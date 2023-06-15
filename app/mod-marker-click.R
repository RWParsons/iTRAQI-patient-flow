# mod-marker-click.R
server_mapclick <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$map_marker_click, {
      group_ids <- get_groups_marker_click(
        marker_id = input$map_marker_click$id,
        polyline_paths = polyline_paths,
        observed_polyline_paths = observed_polyline_paths,
        observed_paths = observed_paths,
        facilities = facilities,
        iTRAQI_paths = iTRAQI_paths
      )

      leafletProxy("map") |>
        hideGroup(group_ids$hide_groups) |>
        showGroup(group_ids$show_groups)
    })
  })
}
