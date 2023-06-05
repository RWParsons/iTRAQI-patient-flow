iTRAQI_vis_app <- function(iTRAQI_paths, facilities, observed_paths) {
  source("app/packages.R")
  source("app/constants.R")
  source("app/map-builders.R")
  source("app/base-map.R")
  source("app/compare-paths.R")


  iTRAQI_paths <- process_iTRAQI_paths(iTRAQI_paths)
  facilities <- process_facilities(facilities)
  polyline_paths <- process_polyline_paths(iTRAQI_paths, facilities)
  observed_paths <- process_observed_paths(observed_paths)
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
      selected_marker <- input$map_marker_click$id

      if (class(selected_marker) == "character") {
        # iTRAQI path
        polyline_selected <-
          polyline_paths |>
          filter(town_point == selected_marker)
      } else {
        # Observed data path
        polyline_selected <-
          observed_polyline_paths |>
          filter(pu_id == selected_marker)
      }
      # browser()
      hide_fcltys <- facilities$FCLTY_ID[!facilities$FCLTY_ID %in% polyline_selected$FCLTY_ID]
      hide_town_points <- iTRAQI_paths$town_point[iTRAQI_paths$town_point != selected_marker]
      hide_observed_points <- observed_polyline_paths$pu_id[observed_polyline_paths$pu_id != selected_marker]

      leafletProxy("map") |>
        hideGroup(paste0("F", hide_fcltys)) |>
        showGroup(paste0("F", polyline_selected$FCLTY_ID)) |>
        hideGroup(paste0("PL", hide_town_points)) |>
        showGroup(paste0("PL", selected_marker)) |>
        hideGroup(paste0("PL-obs", hide_observed_points)) |>
        showGroup(paste0("PL-obs", selected_marker))
    })
  }
  shinyApp(ui = ui, server = server)
}
