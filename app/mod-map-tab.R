# mod-map-tab.R
# Main module - UI 1 #
uimod_map_tab <- function(id) {
  ns <- NS(id)
  div(
    class = "outer",
    tags$head(
      includeCSS("styles.css"),
      tags$script(src = "script.js")
    ),
    tagList(
      leafletOutput(ns("map"), width = "100%", height = "100%"),
      uimod_map_filters(ns("other"))
    )
  )
}

# Main module - Server 1 #
servmod_map_tab <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    passMap <- reactive({
      input$map
    })

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
    servmod_map_filters("other", proxymap)
    
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

