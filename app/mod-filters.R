# mod-filters.R


# Other module - UI 2 #
ui_map_filters <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = FALSE, top = 370, left = "auto", right = 10, bottom = "auto",
      width = 330, height = 440,
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
      ),
      tableOutput(ns("freq_table"))
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
      
      df_filtered_freq <-
        df_op |> 
        filter(path_category %in% input$path_categories, death_flag %in% input$death_flags_cb) |> 
        group_by(path_category, death_flag, .drop=FALSE) |> 
        summarize(n_filtered = n())
        
      df_freq_combined <- 
        inner_join(df_op_freq, df_filtered_freq, by = c("path_category", "death_flag")) |> 
        mutate(cell_content = paste0(n_filtered, "/", n_all)) |> 
        select(-n_all, -n_filtered) |> 
        pivot_wider(names_from = death_flag, values_from = cell_content)
      
      df_combined_for_totals <- 
        cbind(df_filtered_freq, select(ungroup(df_op_freq), n_all)) |> 
        ungroup()
      
      path_cat_totals <-
        df_combined_for_totals |> 
        group_by(path_category) |> 
        summarize(
          pc_totals = sum(n_all),
          filtered_totals = sum(n_filtered)
        ) |> 
        mutate(cell_content = paste0(filtered_totals, "/", pc_totals)) |> 
        select(path_category, total = cell_content)
      
      death_flag_totals <-
        df_combined_for_totals |> 
        group_by(death_flag) |> 
        summarize(
          pc_totals = sum(n_all),
          filtered_totals = sum(n_filtered)
        ) |> 
        mutate(cell_content = paste0(filtered_totals, "/", pc_totals)) |> 
        select(death_flag, cell_content)
      
      # TODO: add death_flag and grand totals row to freq_table
      
      output$freq_table <- renderTable({
        df_freq_combined |> 
          inner_join(path_cat_totals)
        
      })
    })
  })
}
