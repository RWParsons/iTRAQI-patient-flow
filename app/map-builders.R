# map-builders.R

process_iTRAQI_paths <- function(iTRAQI_paths) {
  iTRAQI_paths |>
    mutate(
      n_legs = ifelse(destination1 == destination2, 1, 2),
      across(c(destination1, destination2), str_to_title),
      across(c(transport_mode1, transport_mode2), clean_transport_mode),
      destinations_popup = ifelse(n_legs == 1,
        glue::glue("Destination: {destination1} ({transport_mode1})"),
        glue::glue(
          "Destination 1: {destination1} ({transport_mode1})<br>",
          "Destination 2: {destination2} ({transport_mode2})"
        )
      ),
      popup = glue::glue(
        "<b>{town_name}</b><br>",
        "Travel legs: {n_legs}<br>",
        "{destinations_popup}<br>",
        "Travel time: {total_transport_time_min} mins"
      )
    )
}

process_facilities <- function(facilities) {
  facilities |>
    filter(!is.na(Latitude), !is.na(Longitude)) |>
    rename(xcoord = Longitude, ycoord = Latitude)
}

process_polyline_paths <- function(iTRAQI_paths, facilities) {
  if ("polylines_df.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "polylines_df.rds")))
  }

  palFac <- colorFactor("viridis", levels = 1:6, ordered = TRUE)

  polylines_df <- lapply(
    unique(iTRAQI_paths$town_point),
    \(x) {
      iTRAQI_paths |>
        filter(town_point == x) |>
        mutate(
          destination1 = paste0(destination1, "|", transport_mode1),
          destination2 = paste0(destination2, "|", transport_mode2)
        ) |>
        select(town_point, destination1, destination2) |>
        pivot_longer(!town_point, names_to = "destination_order", values_to = "destination") |>
        mutate(destination = toupper(destination)) |>
        separate(destination, c("destination", "transport_mode"), "\\|") |>
        mutate(destination = str_trim(destination)) |>
        left_join(
          select(facilities, FACILITY_NAME_Clean, FCLTY_ID, xcoord, ycoord),
          by = c("destination" = "FACILITY_NAME_Clean")
        ) |>
        (\(path) {
          bind_rows(
            select(filter(iTRAQI_paths, town_point == x), town_point, xcoord, ycoord),
            path
          )
        })()
    }
  ) |>
    (\(x) {
      do.call("rbind", x)
    })() |>
    mutate(transport_col = palFac(as.numeric(as.factor(transport_mode))))


  saveRDS(polylines_df, file.path(fixtures_path, "polylines_df.rds"))
  polylines_df
}

process_observed_paths <- function(observed_paths, iTRAQI_paths, polyline_paths) {
  if ("observed_paths.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "observed_paths.rds")))
  }
  
  iTRAQI_points <- 
    iTRAQI_paths[,c("xcoord", "ycoord")] |> 
    split(1:nrow(iTRAQI_paths)) |> 
    map(\(x){
      unlist(x) |> 
        st_point()
    }) |> 
    st_sfc()
  
  f_closest_tp <- function(x, y) {
    sf::st_distance(st_point(c(x[1],y[1])), iTRAQI_points) |> 
      t() |> 
      as.data.frame.matrix() |> 
      cbind(iTRAQI_paths$town_point) |>
      rename(distance = 1, town_point = 2) |> 
      na.omit() |> 
      arrange(distance) |> 
      slice(1) |> 
      select(closest_tp = town_point)
  }

  observed_paths_clean <-
    observed_paths|>
    rename(xcoord = X_COORD, ycoord = Y_COORD) |> 
    filter(!is.na(xcoord), !is.na(ycoord)) 
  
  df_closest_tp <-
    observed_paths_clean |>
    split(observed_paths_clean$pu_id) |>
    map(\(x) {
      x |>
        slice(1) |>
        (\(x) cbind(pu_id = x$pu_id, f_closest_tp(x$xcoord, x$ycoord)))()
    }) |> 
    (\(x)do.call("rbind", x))() |> 
    remove_rownames() |> 
    left_join(
      na.omit(select(iTRAQI_paths, town_point, closest_town_name = town_name)),
      by = c("closest_tp" = "town_point")
    )
  
  observed_paths_clean <-
    observed_paths_clean |> 
    left_join(df_closest_tp, by = "pu_id") |> 
    ungroup()
  
  categorise_path <- function(df_path) {
    # categories
    #     - DIDN'T MAKE IT TO HIGH LEVEL CARE
    #     - DID MAKE IT TO HIGH LEVEL CARE & FOLLOWED ITRAQI PATH
    #     - DID MAKE IT TO HIGH LEVEL CARE & DID NOT FOLLOW ITRAQI PATH
    highest_level <- max(as.numeric(as.character(df_path$NeuroSurgMajor)), na.rm=TRUE)
    
    if(is.na(highest_level)|highest_level != 1) {
      return("NO HLC")
    }
    
    twp <- df_path$closest_tp[1]
    
    iTRAQI_path_facilities <- 
      polyline_paths |> 
      filter(town_point == twp) |> 
      mutate(destination = ifelse(destination == "NA", NA, destination)) |> 
      pull(destination) |> 
      unique() |> 
      na.omit()
    
    observed_path_facilities <-
      df_path |> 
      mutate(FACILITY_NAME_Clean = ifelse(FACILITY_NAME_Clean == "NA", NA, FACILITY_NAME_Clean)) |> 
      pull(FACILITY_NAME_Clean) |> 
      unique() |> 
      na.omit()
    
    if(all(iTRAQI_path_facilities== observed_path_facilities)) {
      return("FOLLOWED ITRAQI")
    } else {
      return("DID NOT FOLLOW ITRAQI")
    }
  }
  
  path_categories <- observed_paths_clean |> 
    split(observed_paths_clean$pu_id) |> 
    map(categorise_path) |> 
    (\(x) do.call("c", x))() 
  
  df_path_categories <- data.frame(
    pu_id = as.numeric(names(path_categories)),
    path_category = unlist(path_categories)
  ) |> 
    remove_rownames()
  
  observed_paths_processed <-
    observed_paths_clean |> 
    left_join(df_path_categories) |> 
    mutate(
      Arrival_ReferralPathway = haven::as_factor(Arrival_ReferralPathway),
      facility_and_mode = glue::glue("{FACILITY_NAME_Clean} ({Arrival_ReferralPathway})")
    ) |>
    group_by(pu_id) |>
    mutate(popup = glue::glue(
      "<b>pu_id</b>: {pu_id} <br><br>",
      "<b>closest iTRAQI point</b>: {closest_town_name} ({path_category})<br><br>",
      "<b>centres</b>: <br>{paste0(facility_and_mode[!is.na(FACILITY_NAME_Clean)], collapse = ',<br>')}"
    )) |>
    ungroup()
  
  saveRDS(observed_paths_processed, file.path(fixtures_path, "observed_paths.rds"))
  observed_paths_processed
}

process_observed_polyline_paths <- function(observed_paths) {
  if ("observed_polylines_df.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "observed_polylines_df.rds")))
  }
  palFac <- colorFactor("viridis", levels = 1:6, ordered = TRUE)
  observed_paths <-
    observed_paths |>
    rename(transport_mode = Arrival_ReferralPathway) |>
    select(pu_id, xcoord, ycoord, transport_mode, FACILITY_NAME_Clean, FCLTY_ID) |>
    mutate(transport_col = palFac(as.numeric(transport_mode)))

  saveRDS(observed_paths, file.path(fixtures_path, "observed_polylines_df.rds"))
  observed_paths
}
