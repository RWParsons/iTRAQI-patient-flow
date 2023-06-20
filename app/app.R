# app.R

# TODO
#   - add time to acute care to observed_paths df
#   - add button for colouring observed points by palNum vs all orange (default)


# saveRDS(df_itraqi_times, "app/fixtures/df_itraqi_times.rds")
# saveRDS(df_facilities, "app/fixtures/df_facilities.rds")
# saveRDS(df_times_short, "app/fixtures/df_times_short.rds")

iTRAQI_paths <- readRDS(file.path(here::here(), "app/fixtures/df_itraqi_times.rds"))
facilities <- readRDS(file.path(here::here(), "app/fixtures/df_facilities.rds"))
observed_paths <- readRDS(file.path(here::here(), "app/fixtures/df_times_short.rds"))

app_dir <- file.path(here::here(), "app")

source(file.path(app_dir, "packages.R"))
source(file.path(app_dir, "constants.R"))
source(file.path(app_dir, "map-builders.R"))
source(file.path(app_dir, "base-map.R"))
source(file.path(app_dir, "marker-click.R"))
source(file.path(app_dir, "path-category-controls.R"))

iTRAQI_paths <<- process_iTRAQI_paths(iTRAQI_paths)
facilities <<- process_facilities(facilities)
polyline_paths <<- process_polyline_paths(iTRAQI_paths, facilities)
observed_paths <<- process_observed_paths(observed_paths, iTRAQI_paths, polyline_paths)
observed_polyline_paths <<- process_observed_polyline_paths(observed_paths)

source(file.path(app_dir, "mod-filters.R"))
source(file.path(app_dir, "mod-map-tab.R"))
source(file.path(app_dir, "mod-marker-click.R"))


moduleServer <- function(id, module) {
  callModule(module, id)
}

ui <- navbarPage(
  "iTRAQI-patient-flow",
  id = "nav",
  tabPanel(
    "Map",
    useShinyjs(),
    tagList(
      ui_map("main")
    )
  )
)

server <- function(input, output, session) {
  server_map("main")
  server_mapclick("main")
}
shinyApp(ui, server)
