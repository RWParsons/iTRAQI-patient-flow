library(shiny)
library(leaflet)

# app.R
iTRAQI_paths <- df_itraqi_times
facilities <- df_facilities
observed_paths <- df_times_short

app_dir <- file.path(here::here(), "app")

source(file.path(app_dir, "packages.R"))
source(file.path(app_dir, "constants.R"))
source(file.path(app_dir, "map-builders.R"))
source(file.path(app_dir, "base-map.R"))
source(file.path(app_dir, "marker-click.R"))
source(file.path(app_dir, "path-category-controls.R"))

iTRAQI_paths <- process_iTRAQI_paths(iTRAQI_paths)
facilities <- process_facilities(facilities)
polyline_paths <- process_polyline_paths(iTRAQI_paths, facilities)
observed_paths <- process_observed_paths(observed_paths, iTRAQI_paths, polyline_paths)
observed_polyline_paths <- process_observed_polyline_paths(observed_paths)

source(file.path(app_dir, "mod-filters.R"))
source(file.path(app_dir, "mod-map-tab.R"))
source(file.path(app_dir, "mod-marker-click.R"))

moduleServer <- function(id, module) {
  callModule(module, id)
}

# Modules ------------------------


### main map
ui_map <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
    ui_map_filters(ns("filter"))
  )
}

server_map <- function(id) {
  moduleServer(id, function(input, output, session) {
    # output$map <- renderLeaflet(quake_map(QUAKES))
    
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
    
    proxymap <- reactive(leafletProxy('map'))
    server_map_filters("filter", proxymap)
    # server_mapclick("map", proxymap)
  })
}


### mapclick
server_mapclick <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$map_marker_click, {
      print(input$map_marker_click)
      leafletProxy('map') |> hideGroup(input$map_marker_click$group)
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
      
      leafletProxy('map') |>
        hideGroup(group_ids$hide_groups) |>
        showGroup(group_ids$show_groups)
    })
  })
}


# App ---------------------
mapApp <- function() {
  ui <- fluidPage(
    ui_map("main")
  )
  server <- function(input, output, session) {
    server_map("main")
    server_mapclick("main") # Same namespace!
  }
  shinyApp(ui, server)  
}

mapApp()