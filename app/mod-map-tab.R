# mod-map-tab.R
### main map
ui_map <- function(id) {
  ns <- NS(id)
  div(
    class = "outer",
    tags$head(
      includeCSS(file.path(here::here(), "app", "styles.css")),
      tags$script(src = file.path(here::here(), "app", "script.js"))
    ),
    tagList(
      leafletOutput(ns("map"), width = "100%", height = "100%"),
      ui_map_filters(ns("filter"))
    )
  )
}

server_map <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      base_map(
        map_bounds = map_bounds,
        facilities = facilities,
        iTRAQI_paths = iTRAQI_paths,
        polyline_paths = polyline_paths,
        observed_paths = observed_paths,
        observed_polyline_paths = observed_polyline_paths
      )
    })

    proxymap <- reactive(leafletProxy("map"))
    server_map_filters("filter", proxymap)
  })
}
