iTRAQI_vis_app <- function(iTRAQI_paths, facilities) {
  source("app/packages.R")
  source("app/constants.R")
  source("app/base-map.R")
  
  iTRAQI_paths <- process_iTRAQI_paths(iTRAQI_paths)
  facilities <- process_facilities(facilities)
  polyline_paths <- process_polyline_paths(iTRAQI_paths, facilities)
  
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
      base_map(map_bounds, facilities, iTRAQI_paths)
    })
    
    observeEvent(input$map_marker_click, {
      print(input$map_marker_click)
      df_destinations <-
        iTRAQI_paths |> 
        filter(town_point == input$map_marker_click$id) |> 
        select(town_point, destination1, destination2) |>
        pivot_longer(!town_point, names_to = "destination_order", values_to = "destination") |> 
        mutate(destination = toupper(destination)) |> 
        left_join(
          select(facilities, FACILITY_NAME_Clean, FCLTY_ID, xcoord, ycoord), 
          by = c("destination" = "FACILITY_NAME_Clean")
        )
      
      df_destinations_plus_origin <- bind_rows(
        select(filter(iTRAQI_paths, town_point == input$map_marker_click$id), xcoord, ycoord),
        df_destinations
      )
      
      # df_paths_i <- df_paths |> filter(id == paste0("P", input$map_marker_click$id))
      
      hide_fcltys <- facilities$FCLTY_ID[!facilities$FCLTY_ID %in% df_destinations$FCLTY_ID]
      
      leafletProxy("map") |> 
        hideGroup(paste0("F", hide_fcltys)) |> 
        showGroup(paste0("F", df_destinations$FCLTY_ID)) |> 
        addPolylines(
          lng = df_destinations_plus_origin$xcoord,
          lat = df_destinations_plus_origin$ycoord
        )
      browser()
    })
    
  }
  shinyApp(ui = ui, server = server)
}
