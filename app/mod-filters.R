# mod-filters.R


# Other module - UI 2 #
ui_map_filters <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = FALSE, top = 370, left = "auto", right = 10, bottom = "auto",
      width = 250, height = 220,
      h4("Path adherence"),
      checkboxGroupInput(
        inputId = ns("path_categories"),
        label = NULL,
        choices = path_cats,
        selected = path_cats
      ),
      h4("Death flag"),
      checkboxGroupInput(
        inputId = ns("death_flags_cb"),
        label = NULL,
        choices = death_flags,
        selected = death_flags
      )
    )
  )
}


# Other module - Server 2 #
server_map_filters <- function(id, passMap) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    observeEvent(list(input$path_categories, input$death_flags_cb), ignoreNULL = FALSE, {
      group_ids <- get_groups_path_cats(
        observed_paths = observed_paths,
        path_cats = input$path_categories,
        death_flag_select = input$death_flags_cb
      )

      passMap() |>
        hideGroup(group_ids$hide_groups) |>
        showGroup(group_ids$show_groups)
    })
  })
}
