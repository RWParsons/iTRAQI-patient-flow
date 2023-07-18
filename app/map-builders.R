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
    rename(xcoord = Longitude, ycoord = Latitude) |>
    mutate(grp_id = paste0("F", FCLTY_ID))
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
    mutate(
      transport_col = palFac(as.numeric(as.factor(transport_mode))),
      grp_id = paste0("PL-", town_point)
    )


  saveRDS(polylines_df, file.path(fixtures_path, "polylines_df.rds"))
  polylines_df
}

process_observed_paths <- function(observed_paths, iTRAQI_paths, polyline_paths) {
  if ("observed_paths.rds" %in% list.files(fixtures_path)) {
    return(readRDS(file.path(fixtures_path, "observed_paths.rds")))
  }

  iTRAQI_points <-
    iTRAQI_paths[, c("xcoord", "ycoord")] |>
    split(1:nrow(iTRAQI_paths)) |>
    map(\(x){
      unlist(x) |>
        st_point()
    }) |>
    st_sfc()

  f_closest_tp <- function(x, y) {
    sf::st_distance(st_point(c(x[1], y[1])), iTRAQI_points) |>
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
    observed_paths |>
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
    # if(any(df_path$pu_id == "ID-1001323")) browser()
    if (all(is.na(df_path$NeuroSurgMajor))) {
      highest_level <- NA
    } else {
      highest_level <- min(as.numeric(as.character(df_path$NeuroSurgMajor)), na.rm = TRUE)
    }

    if (is.na(highest_level) | highest_level != 1) {
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

    brisbane_hospitals <- c("RBWH", "PAH", "QCH", "PRINCESS ALEXANDRA")

    iTRAQI_path_facilities[iTRAQI_path_facilities %in% brisbane_hospitals] <- "BRISBANE-MAJOR"
    observed_path_facilities[observed_path_facilities %in% brisbane_hospitals] <- "BRISBANE-MAJOR"


    if (all(iTRAQI_path_facilities == observed_path_facilities)) {
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
    pu_id = names(path_categories),
    path_category = unlist(path_categories)
  ) |>
    remove_rownames()
  
  add_predicted_iTRAQI_times <- function(x, y) {
    acute_raster_df <- 
      get_iTRAQI_raster() |> 
      subset(1) |> 
      raster::as.data.frame(xy = TRUE) |> 
      na.omit()
    
    acute_raster_sf_pt <- 
      acute_raster_df |> 
      split(1:nrow(acute_raster_df)) |>
      map(\(x){
        unlist(x) |>
          st_point()
      }) |>
      st_sfc()
    
    f_get_pred_from_coord <- function(coord_pair, xcoord, ycoord, ...) {
      if(!missing(coord_pair)) {
        xcoord <- coord_pair$x
        ycoord <- coord_pair$y
      }
      
      distances_from_point <- 
        c(xcoord, ycoord) |> 
        (\(coord_pair)sf::st_distance(sf::st_point(coord_pair), acute_raster_sf_pt))() |> 
        t() |> 
        as.data.frame.matrix() |> 
        cbind(acute_raster_df) |> 
        dplyr::rename(distance = 1)
      
      # return closest predicted value
      distances_from_point$var1.pred[which.min(distances_from_point$distance)] 
    }
    
    cl <- parallel::makeCluster(parallel::detectCores())
    pbapply::pblapply(
      split(data.frame(x, y), 1:length(x)),
      f_get_pred_from_coord,
      cl=cl
    ) |> unlist()
  }
  
  itraqi_pred_times_df <- 
    observed_paths_clean |> 
    group_by(pu_id) |> 
    slice(1) |> 
    ungroup() |> 
    (\(.data) cbind(.data, itraqi_pred = add_predicted_iTRAQI_times(x=.data$xcoord, y = .data$ycoord)))()
  
  # TODO:
    # > add variable for final facility 
    # > add variable for age group
  
  # age_cats <- cut(0, 100)
  
  observed_paths_processed <-
    observed_paths_clean |>
    left_join(select(itraqi_pred_times_df, pu_id, itraqi_pred), by = "pu_id") |> 
    left_join(df_path_categories) |>
    mutate(
      Arrival_ReferralPathway = haven::as_factor(Arrival_ReferralPathway),
      facility_and_mode = glue::glue(
        "{FACILITY_NAME_Clean} ",
        "(via {Arrival_ReferralPathway}; ",
        "duration: {round(stay_duration)}mins)")
    ) |>
    group_by(pu_id) |>
    mutate(start_time = min(DateTimePoints),
           end_time = max(DateTimePoints),
           total_time = as.numeric(difftime(end_time, start_time, units = "mins"))) |> 
    mutate(popup = glue::glue(
      "<b>pu_id</b>: {pu_id} <br><br>",
      "<b>travel time</b>: {round(total_time)}<br><br>",
      "{ifelse(death_flag == 'Survived', '', paste0('<b>Died (', death_flag, ')'))}</b><br><br>",
      "<b>closest iTRAQI point</b>: {closest_town_name} ({path_category}; iTRAQI predicted time: {round(itraqi_pred)}mins)<br><br>",
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
