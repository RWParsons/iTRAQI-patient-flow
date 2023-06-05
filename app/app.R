iTRAQI_vis_app <- function(iTRAQI_paths, facilities, observed_paths) {
  source("app/packages.R")
  source("app/constants.R")
  source("app/map-builders.R")
  source("app/base-map.R")
  source("app/marker-click.R")


  iTRAQI_paths <- process_iTRAQI_paths(iTRAQI_paths)
  facilities <- process_facilities(facilities)
  polyline_paths <- process_polyline_paths(iTRAQI_paths, facilities)
  observed_paths <- process_observed_paths(observed_paths, iTRAQI_paths)
  observed_polyline_paths <- process_observed_polyline_paths(observed_paths)
  # return(polyline_paths)
  ui <-
    navbarPage(
      "iTRAQI-patient-flow",
      id = "nav",
      tabPanel(
        "Map",
        useShinyjs(),
        div(
          class = "outer",
          tags$head(
            includeCSS("styles.css"),
            tags$script(src = "script.js")
          ),
          leafletOutput("map", width = "100%", height = "100%"),
        )
      )
    )

  server <- function(input, output, session) {
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
  }
  shinyApp(ui = ui, server = server)
}
