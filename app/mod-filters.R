# mod-filters.R


# Other module - UI 2 #
ui_map_filters <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = FALSE, top = 370, left = "auto", right = 10, bottom = "auto",
      width = 250, height = 120,
      h4("Markers"),
      checkboxGroupInput(
        inputId = ns("path_categories"),
        label = NULL,
        choices = path_cats,
        selected = path_cats
      )
    )
  )
}


# Other module - Server 2 #
server_map_filters <- function(id, passMap) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    observeEvent(input$path_categories, ignoreNULL = FALSE, {
      group_ids <- get_groups_path_cats(
        path_cats = input$path_categories,
        observed_paths = observed_paths
      )

      passMap() |>
        hideGroup(group_ids$hide_groups) |>
        showGroup(group_ids$show_groups)
    })
  })
}
