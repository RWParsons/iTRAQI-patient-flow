# path-category-controls.R

path_cats <- c("NO HLC", "FOLLOWED ITRAQI", "DID NOT FOLLOW ITRAQI")
death_flags <- c("Survived", "QAS", "Hospital", "ED")
get_groups_path_cats <- function(observed_paths, path_cats, death_flag_select) {
  show_groups <- observed_paths |>
    filter(
      path_category %in% path_cats,
      death_flag %in% death_flag_select
    ) |>
    pull(pu_id)

  hide_groups <- observed_paths$pu_id[!observed_paths$pu_id %in% show_groups]

  list(
    hide_groups = hide_groups,
    show_groups = show_groups
  )
}
