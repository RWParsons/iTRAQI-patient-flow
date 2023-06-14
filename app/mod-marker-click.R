# mod-marker-click.R

# Other module - UI 2 #
# uimod_map_marker_click <- function(id) {
#   ns <- NS(id)
#   tagList(
#     absolutePanel(
#       id = "controls", class = "panel panel-default", fixed = TRUE,
#       draggable = FALSE, top = 370, left = "auto", right = 10, bottom = "auto",
#       width = 250, height = 120,
#       h4("Markers"),
#       checkboxGroupInput(
#         inputId = ns("path_categories"),
#         label = NULL,
#         choices = path_cats,
#         selected = path_cats
#       )
#     )
#   )
# }


# Other module - Server 2 #
servmod_map_marker_click <- function(id, passMap) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    v <- reactiveValues(timer = Sys.time()+5)
    
    observe({
      invalidateLater(100)
      browser()
      if(v$timer <= Sys.time()){
        v$timer <- Sys.time()+5
        print("timer fires") 
      }
    })
    
    observeEvent(input$`main-map-map_marker_click`, {
      browser()
      group_ids <- get_groups_marker_click(
        marker_id = input$map_marker_click$id,
        polyline_paths = polyline_paths,
        observed_polyline_paths = observed_polyline_paths,
        observed_paths = observed_paths,
        facilities = facilities,
        iTRAQI_paths = iTRAQI_paths
      )
      
      passMap |>
        hideGroup(group_ids$hide_groups) |>
        showGroup(group_ids$show_groups)
    })
  })
}
