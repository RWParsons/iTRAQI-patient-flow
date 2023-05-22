iTRAQI_vis_app <- function(iTRAQI_paths, facilities) {
  source("app/packages.R")
  source("app/constants.R")
  source("app/base-map.R")
  
  iTRAQI_paths <- process_iTRAQI_paths(iTRAQI_paths)
  facilities <- process_facilities(facilities)
  polyline_paths <- process_polyline_paths(iTRAQI_paths, facilities)
  # return(polyline_paths)
  ui <- 
    navbarPage(
      "iTRAQI",
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
        polyline_paths = polyline_paths
      )
    })
    
    observeEvent(input$map_marker_click, {
      
      selected_town_point <- input$map_marker_click$id
      # print(input$map_marker_click)
      
      polyline_selected <-
        polyline_paths |> 
        filter(town_point == selected_town_point)
      
      hide_fcltys <- facilities$FCLTY_ID[!facilities$FCLTY_ID %in% polyline_selected$FCLTY_ID]
      hide_town_points <- iTRAQI_paths$town_point[iTRAQI_paths$town_point != selected_town_point]
      
      leafletProxy("map") |> 
        hideGroup(paste0("F", hide_fcltys)) |>
        showGroup(paste0("F", polyline_selected$FCLTY_ID)) |>
        hideGroup(paste0("PL", hide_town_points)) |>
        showGroup(paste0("PL", selected_town_point))
        
    })
    
  }
  shinyApp(ui = ui, server = server)
}
