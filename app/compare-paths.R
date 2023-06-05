# compare-paths.R

is_itraqi <- function(marker, iTRAQI_paths) {
  marker %in% iTRAQI_paths$town_point
}

locate_nearest_itraqi_point <- function(x, y, iTRAQI_paths) {
  iTRAQI_points <- 
    iTRAQI_paths[,c("xcoord", "ycoord")] |> 
    split(1:nrow(iTRAQI_paths)) |> 
    map(\(x){
      unlist(x) |> 
        st_point()
    }) |> 
    st_sfc()
  
  sf::st_distance(st_point(c(x,y)), iTRAQI_points) |> 
    t() |> 
    as.data.frame.matrix() |> 
    cbind(iTRAQI_paths$town_point) |>
    rename(distance=1, town_point = 2) |> 
    na.omit() |> 
    arrange(distance) |> 
    slice(1) |> 
    pull(town_point) |> 
    unlist()
}
