# path-category-controls.R

path_cats <- c("NO HLC", "FOLLOWED ITRAQI", "DID NOT FOLLOW ITRAQI")

ui_panel <-
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 370, left = "auto", right = 10, bottom = "auto",
    width = 250, height = 120,
    h4("Markers"),
    checkboxGroupInput(
      "path_categories",
      label = NULL,
      choices = path_cats,
      selected = path_cats
    )
  )


get_groups_path_cats <- function(path_cats, observed_paths) {
  # browser()
  list(
    hide_groups = observed_paths$pu_id[!observed_paths$path_category %in% path_cats],
    show_groups = observed_paths$pu_id[observed_paths$path_category %in% path_cats]
  )
}
