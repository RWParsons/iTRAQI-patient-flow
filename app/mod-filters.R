# mod-filters.R
source(file.path(here::here(), "app", "setShapeStyle.R"))

# Other module - UI 2 #
ui_map_filters <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = FALSE, top = 370, left = "auto", right = 10, bottom = "auto",
      width = 450, height = 500,
      h4("Path adherence"),
      materialSwitch(
        inputId = ns("acute_raster_select"),
        label = "iTRAQI acute raster",
        status = "danger"
      ),
      materialSwitch(
        inputId = ns("travel_time_marker_col"),
        label = "Colour markers by travel time",
        status = "danger"
      ),
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
      ),
      tableOutput(ns("freq_table"))
    )
  )
}


# Other module - Server 2 #
server_map_filters <- function(id, passMap) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    observeEvent(input$acute_raster_select, {
      if(input$acute_raster_select) {
        passMap() |> showGroup("acute_raster")
      } else {
        passMap() |> hideGroup("acute_raster")
      }
    })
    
    observeEvent(list(input$path_categories, input$death_flags_cb, input$travel_time_marker_col), ignoreNULL = FALSE, {
      group_ids <- get_groups_path_cats(
        observed_paths = observed_paths,
        path_cats = input$path_categories,
        death_flag_select = input$death_flags_cb
      )
      
      if(input$travel_time_marker_col) {
        group_ids$show_groups <- paste0("traveltime-", group_ids$show_groups)
        group_ids$hide_groups <- unique(c( paste0("traveltime-", group_ids$hide_groups), observed_paths$pu_id))
      } else {
        group_ids$hide_groups <- unique(c(group_ids$hide_groups, paste0("traveltime-", observed_paths$pu_id)))
      }

      passMap() |>
        hideGroup(group_ids$hide_groups) |>
        showGroup(group_ids$show_groups)
      
      output$freq_table <- renderTable({
        df_op <-
          observed_paths |>
          group_by(pu_id) |>
          slice(1) |>
          ungroup() |>
          mutate(across(c(path_category, death_flag), as.factor))

        df_op_freq <-
          df_op |>
          group_by(path_category, death_flag) |>
          summarize(n_all = n())

        df_op_freq <-
          df_op |>
          group_by(path_category, death_flag) |>
          summarize(n_all = n()) |>
          pivot_wider(names_from = death_flag, values_from = n_all) |>
          janitor::adorn_totals("row") |>
          janitor::adorn_totals("col") |>
          pivot_longer(!path_category, values_to = "all")

        df_filtered_freq <-
          df_op |>
          filter(path_category %in% input$path_categories, death_flag %in% input$death_flags_cb) |>
          group_by(path_category, death_flag, .drop=FALSE) |>
          summarize(n_filtered = n()) |>
          pivot_wider(names_from = death_flag, values_from = n_filtered) |>
          janitor::adorn_totals("row") |>
          janitor::adorn_totals("col") |>
          pivot_longer(!path_category, values_to = "filtered")

        inner_join(df_op_freq, df_filtered_freq, by = c("path_category", "name")) |>
          mutate(cell_content = paste0(filtered, "/", all)) |>
          select(-all_of(c("all", "filtered"))) |>
          pivot_wider(names_from = "name", values_from = "cell_content")
      })
    })
  })
}
