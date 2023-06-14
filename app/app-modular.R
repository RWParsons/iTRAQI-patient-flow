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

moduleServer <- function(id, module) {
  callModule(module, id)
}

ui <-
  navbarPage(
    "iTRAQI-patient-flow",
    id = "nav",
    tabPanel(
      "Map",
      useShinyjs(),
      tagList(
        uimod_map_tab("main-map")
      )
    )
  )

server <- function(input, output, session) {
  servmod_map_tab("main-map")
}

shinyApp(ui = ui, server = server)
