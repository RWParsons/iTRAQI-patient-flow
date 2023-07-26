# mod-filters.R
source(file.path(here::here(), "app", "setShapeStyle.R"))

# Other module - UI 2 #
ui_map_filters <- function(id) {
  ns <- NS(id)
  tagList(
    absolutePanel(
      width = "calc(100vw - 70px)", top = 20, left = 35, right = 35,
      fluidRow(
        box(
          title = "Map controls",
          id = ns("panelLeft"),
          height = "100%",
          width = 6,
          collapsible = TRUE,
          collapsed = TRUE,
          # actionButton(ns("browser"), "browser"),
          tableOutput(ns("freq_table")),
          bs4Accordion(
            id = ns("acc"),
            width = 10,
            bs4AccordionItem(
              id = ns("acc-colours"),
              title = "Colours",
              collapsed = TRUE,
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
            ),
            bs4AccordionItem(
              id = ns("acc-markers"),
              title = "Markers",
              checkboxGroupInput(
                inputId = ns("marker_groups"),
                label = NULL,
                choices = c("iTRAQI points", "observed points"),
                selected = c("iTRAQI points", "observed points")
              )
            ),
            bs4AccordionItem(
              id = ns("acc-paths"),
              title = "Path Adherence",
              checkboxGroupInput(
                inputId = ns("path_categories"),
                label = NULL,
                choices = path_cats,
                selected = path_cats
              )
            ),
            bs4AccordionItem(
              id = ns("acc-death"),
              title = "Death flag",
              checkboxGroupInput(
                inputId = ns("death_flags_cb"),
                label = NULL,
                choices = death_flags,
                selected = death_flags
              )
            ),
            bs4AccordionItem(
              id = ns("acc-ages"),
              title = "Age",
              checkboxGroupInput(
                inputId = ns("age_category"),
                label = NULL,
                choices = age_cats,
                selected = age_cats
              )
            )
          )
        ),
        box(
          title = "Plot",
          id = ns("plotPanel"),
          height = "100%",
          collapsible = TRUE,
          collapsed = TRUE,
          radioGroupButtons(
            inputId = ns("plot_time_col"),
            label = "",
            choices = c("iTRAQI predicted time", "Observed time"),
            status = "primary"
          ),
          plotOutput(
            ns("plot"),
            brush = brushOpts(ns("plot_brush"))
          )
        )
      )
    )
  )
}


# Other module - Server 2 #
server_map_filters <- function(id, passMap) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    observeEvent(input$browser, {
      browser()
    })

    observeEvent(input$acute_raster_select, {
      if (input$acute_raster_select) {
        passMap() |> showGroup("acute_raster")
      } else {
        passMap() |> hideGroup("acute_raster")
      }
    })

    observeEvent(list(
      input$path_categories,
      input$death_flags_cb,
      input$travel_time_marker_col,
      input$marker_groups,
      input$age_category
    ), ignoreNULL = FALSE, {
      group_ids <- get_groups_path_cats(
        observed_paths = observed_paths,
        path_cats = input$path_categories,
        death_flag_select = input$death_flags_cb,
        age_cats_select = input$age_category
      )

      if (input$travel_time_marker_col) {
        group_ids$show_groups <- paste0("traveltime-", group_ids$show_groups)
        group_ids$hide_groups <- unique(c(paste0("traveltime-", group_ids$hide_groups), observed_paths$pu_id))
      } else {
        group_ids$hide_groups <- unique(c(group_ids$hide_groups, paste0("traveltime-", observed_paths$pu_id)))
      }


      if ("observed points" %in% input$marker_groups) {
        passMap() |>
          hideGroup(group_ids$hide_groups) |>
          showGroup(group_ids$show_groups)
      } else {
        passMap() |>
          hideGroup(c(paste0("traveltime-", observed_paths$pu_id), observed_paths$pu_id))
      }

      if ("iTRAQI points" %in% input$marker_groups) {
        passMap() |> showGroup(iTRAQI_paths$town_point)
      } else {
        passMap() |> hideGroup(iTRAQI_paths$town_point)
      }

      output$plot <- renderPlot({
        # browser()

        group_ids_plot <- group_ids
        # group_ids_plot$hide_groups <- str_remove(group_ids$hide_groups, "traveltime-")
        # group_ids_plot$show_groups <- str_remove(group_ids$show_groups, "traveltime-")
        plot_df <-
          observed_paths |>
          filter(
            !pu_id %in% group_ids_plot$hide_groups,
            pu_id %in% group_ids_plot$show_groups
          ) |>
          select(pu_id, itraqi_pred, total_time) |>
          distinct()


        make_plot(plot_df, input$plot_time_col)
      })

      observeEvent(input$plot_brush, {
        plot_df <- observed_paths |>
          filter(
            !pu_id %in% group_ids$hide_groups,
            pu_id %in% group_ids$show_groups
          ) |>
          # select(pu_id, itraqi_pred, total_time) |>
          distinct()

        brushed_points <- brushedPoints(plot_df, input$plot_brush)
        print(brushed_points)
        # browser()
      })

      output$freq_table <- renderTable({
        age_cats_select <- input$age_category
        age_cats_select[age_cats_select == ""] <- NA

        # browser()
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
          group_by(path_category, death_flag, .drop = FALSE) |>
          summarize(n_all = n()) |>
          pivot_wider(names_from = death_flag, values_from = n_all) |>
          janitor::adorn_totals("row") |>
          janitor::adorn_totals("col") |>
          pivot_longer(!path_category, values_to = "all")

        df_filtered_freq <-
          df_op |>
          filter(
            path_category %in% input$path_categories,
            death_flag %in% input$death_flags_cb,
            cut(PAT_AGE, breaks = age_cat_breaks) %in% age_cats_select
          ) |>
          group_by(path_category, death_flag, .drop = FALSE) |>
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

make_plot <- function(plot_df, plot_time_col) {
  hours_show <- 8

  if (plot_time_col == "iTRAQI predicted time") {
    col_vec <- palNum(plot_df$itraqi_pred)
  } else {
    col_vec <- palNum(plot_df$total_time)
  }

  ggplot() +
    geom_point(data = plot_df, aes(itraqi_pred, total_time), col = col_vec) +
    geom_abline() +
    scale_x_continuous(
      limits = c(0, hours_show * 60),
      breaks = seq(0, hours_show * 60, 60)
    ) +
    scale_y_continuous(
      limits = c(0, hours_show * 60),
      breaks = seq(0, hours_show * 60, 60)
    ) +
    coord_equal() +
    theme_bw() +
    labs(
      x = "iTRAQI predicted time (mins)",
      y = "Observed travel time (mins)"
    )
}
