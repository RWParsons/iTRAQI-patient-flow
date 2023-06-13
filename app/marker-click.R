# marker-click.R

get_groups_marker_click <- function(marker_id,
                                    polyline_paths,
                                    observed_polyline_paths,
                                    observed_paths,
                                    facilities,
                                    iTRAQI_paths) {
  if (str_detect(marker_id, "^TWP")) {
    # iTRAQI path
    polyline_selected <-
      polyline_paths |>
      filter(town_point == marker_id)
    is_itraqi <- TRUE
  } else {
    # Observed data path
    polyline_selected <-
      observed_polyline_paths |>
      filter(pu_id == paste0("ID-", marker_id))
    is_itraqi <- FALSE
  }
  
  hide_fcltys <- facilities$FCLTY_ID[!facilities$FCLTY_ID %in% polyline_selected$FCLTY_ID]
  hide_town_points <- iTRAQI_paths$town_point[iTRAQI_paths$town_point != marker_id]
  hide_observed_points <- observed_polyline_paths$pu_id[observed_polyline_paths$pu_id != paste0("ID-", marker_id)]
  
  if(!is_itraqi) {
    itraqi_tp <- observed_paths |> 
      filter(pu_id == paste0("ID-", marker_id)) |> 
      pull(closest_tp)
    
    marker_id <- c(marker_id, itraqi_tp)
  }
  
  list(
    hide_groups = c(
      paste0("F", hide_fcltys),
      paste0("PL-", hide_town_points),
      str_replace(hide_observed_points, "ID-", "PL-obs")
    ),
    show_groups = c(
      paste0("F", polyline_selected$FCLTY_ID),
      paste0("PL-", marker_id),
      paste0("PL-obs", marker_id)
    )
  )
}
