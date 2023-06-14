# test-app.R
library(shiny)
library(sf)
library(leaflet)

quakes2 <- quakes
quakes2$id <- 1:nrow(quakes)
quakes2$depth_cat <- cut(quakes$depth, breaks = 5)

# mod-filters.R
uimod_map_filters <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = FALSE, top = 370, left = "auto", right = 10, bottom = "auto",
      width = 250, height = 120,
      h4("Markers"),
      checkboxGroupInput(
        inputId = ns("depth_filter"),
        label = NULL,
        choices = sort(unique(quakes2$depth_cat)),
        selected = sort(unique(quakes2$depth_cat))
      )
    )
  )
}
servmod_map_filters <- function(id, passMap) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    observeEvent(input$depth_filter, ignoreNULL = FALSE, {
      input$depth_filter
      
      hide_groups <- quakes2$id[!quakes2$depth_cat %in% input$depth_filter]
      show_groups <- quakes2$id[quakes2$depth_cat %in% input$depth_filter]
      
      passMap() |>
        hideGroup(hide_groups) |>
        showGroup(show_groups)
    })
  })
}


################# 
# mod-map-tab.R
# main module ui
uimod_map_tab <- function(id) {
  ns <- NS(id)
  div(
    class = "outer",
    tagList(
      leafletOutput(ns("map"), width = "100%", height = "100%"),
      uimod_map_filters(ns("other"))
    )
  )
}

# main module server
servmod_map_tab <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    passMap <- reactive({
      input$map
    })
    
    coords <- quakes2 %>%
      sf::st_as_sf(coords = c("long","lat"), crs = 4326)
    
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>% 
        leaflet::addTiles() %>% 
        leaflet::setView(172.972965,-35.377261, zoom = 4) %>%
        leaflet::addCircleMarkers(
          data = coords,
          # group = quakes2$id,
          stroke = FALSE,
          radius = 6)
    })
    proxymap <- reactive(leafletProxy("map"))
    # servmod_map_filters("other", proxymap)
    # servmod_map_marker_click("test", proxymap)
  })
}


##### app.R
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
