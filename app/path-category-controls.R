# path-category-controls.R

path_cats <- c("NO HLC", "FOLLOWED ITRAQI", "DID NOT FOLLOW ITRAQI")
get_groups_path_cats <- function(path_cats, observed_paths) {
  list(
    hide_groups = observed_paths$pu_id[!observed_paths$path_category %in% path_cats],
    show_groups = observed_paths$pu_id[observed_paths$path_category %in% path_cats]
  )
}
