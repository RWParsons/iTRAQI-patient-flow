# read-and-wrangle.R

library(tidyverse)
library(sp)
library(sf)
source("R/download-utils.R")
data_dir <- "U:\\Research\\Projects\\health\\jti_research_data\\jti_itraqi"



df_facilities <- haven::read_spss(file.path(data_dir, "D_Table_FacilityDescription.sav"))
df_times <- haven::read_spss(file.path(data_dir, "D_Table_TimeAndLocation_LongFileSELECT.sav"))


df_itraqi_times <- openxlsx::read.xlsx("https://github.com/RWParsons/iTRAQI/blob/main/input/drive_times/Qld_towns_RSQ%20pathways%20V3.xlsx?raw=TRUE", startRow = 3) |> janitor::clean_names()

# get flags for death (and where along path)
df_death_flags <- haven::read_spss(file.path(data_dir, "F_TableFlagsANDOutcomesSELECT.sav"))


df_death_flags <- haven::read_spss(file.path(data_dir, "F_TableFlagsANDOutcomesSELECT.sav")) |>
  select(pu_id, QASDeceasedEnc, EDDiedFlag, HospDiedFlag) # take earliest flag (order: QAS-ED-Hosp) and add label to popup if any=1

df_amb <- haven::read_spss(file.path(data_dir, "D_Table_AmbulanceContent.sav"))
df_ages <- haven::read_spss(file.path(data_dir, "D_Table_HospitalContent.sav"), col_select = c("pu_id", "PAT_AGE"))


### add death flags to observed data
# take earliest flag (order: QAS-ED-Hosp) and add label to popup if any=1
df_death_flags2 <-
  df_death_flags |>
  mutate(across(!pu_id, \(x) ifelse(is.na(x), 0, x))) |>
  pivot_longer(!pu_id) |>
  group_by(pu_id) |>
  slice(1:which.max(value)) |>
  ungroup() |>
  filter(value %in% c(1, 2)) |>
  select(pu_id, death_flag = name) |>
  mutate(
    pu_id = paste0("ID-", pu_id),
    death_flag = case_when(
      death_flag == "QASDeceasedEnc" ~ "QAS",
      death_flag == "HospDiedFlag" ~ "Hospital",
      TRUE ~ "ED"
    )
  ) |>
  mutate(death_flag = factor(death_flag, levels = c("QAS", "ED", "Hospital"))) |>
  group_by(pu_id) |>
  arrange(death_flag) |>
  slice(1) |>
  ungroup() |>
  mutate(death_flag = as.character(death_flag))


### rearrange observed data (`df_times`) into the same/similar format to df_itraqi_times, so that it can be used in the app


fx_shorten_path <- function(pu_id_) {
  d <-
    df_times |>
    filter(pu_id == pu_id_) |>
    mutate(FACILITY_NAME_Clean = ifelse(FACILITY_NAME_Clean == "", NA, FACILITY_NAME_Clean))

  origin_row <-
    d |>
    filter(!is.na(X_COORD)) |>
    slice(1) |>
    mutate(NeuroSurgMajor = NA_integer_, FCLTY_ID = NA_integer_, stay_duration = NA)

  centre_rows <-
    d |>
    filter(!is.na(FACILITY_NAME_Clean)) |>
    group_by(FACILITY_NAME_Clean) |>
    arrange(DateTimePoints) |>
    slice(1) |>
    ungroup()

  if (nrow(centre_rows) == 0) {
    return(select(origin_row, pu_id, TimeWayPoint:FACILITY_NAME_Clean, NeuroSurgMajor, FCLTY_ID, stay_duration, Arrival_ReferralPathway))
  }

  # print(pu_id_)
  centre_rows <- centre_rows |>
    select(-NeuroSurgMajor) |>
    mutate(FACILITY_NAME_Clean = as.character(FACILITY_NAME_Clean)) |>
    left_join(select(df_facilities, FACILITY_NAME_Clean, Latitude, Longitude, NeuroSurgMajor, FCLTY_ID), by = "FACILITY_NAME_Clean") |>
    mutate(X_COORD = Longitude, Y_COORD = Latitude) |>
    select(-Latitude, -Longitude) |>
    slice(1:which.min(NeuroSurgMajor))

  f_get_duration <- function(d, time_wp, date_time) {
    d_working <-
      d |>
      mutate(rn = row_number())

    start_row <-
      d_working |>
      filter(TimeWayPoint == time_wp, DateTimePoints == date_time)

    if (nrow(start_row) != 1) {
      cat("row num fail")
      return(NA)
    }

    loc_type <- case_when(
      start_row$TimeWayPoint == "ED_START_DATETIME_Formatted" ~ "ED",
      start_row$TimeWayPoint == "Hosp_START_DATETIME_Formatted" ~ "Hospital",
      TRUE ~ NA
    )

    if (is.na(loc_type)) {
      cat("location is NA")
      return(NA)
    }

    later_rows <-
      d_working |>
      filter(rn > start_row$rn)

    if (loc_type == "ED") {
      end_time <-
        later_rows |>
        slice(which.max(TimeWayPoint == "ED_End_DATETIME_Formatted")) |>
        pull(DateTimePoints)
    } else if (loc_type == "Hospital") {
      end_time <- later_rows |>
        slice(which.max(TimeWayPoint == "Hosp_END_DATETIME_Formatted")) |>
        pull(DateTimePoints)
    }

    return(as.numeric(difftime(end_time, start_row$DateTimePoints)))
  }


  centre_rows <-
    centre_rows |>
    rowwise() |>
    mutate(stay_duration = f_get_duration(d = d, time_wp = TimeWayPoint, date_time = DateTimePoints))

  bind_rows(origin_row, centre_rows) |>
    select(pu_id, TimeWayPoint:FACILITY_NAME_Clean, NeuroSurgMajor, FCLTY_ID, stay_duration, Arrival_ReferralPathway)
}


df_times_short <- lapply(
  unique(df_times$pu_id),
  fx_shorten_path
) |>
  (\(x) do.call("rbind", x))() |>
  mutate(pu_id = paste0("ID-", pu_id)) # make pu_id a character so that it works nicer as an ID for leaflet

f_clean_final_facility <- function(x) {
  case_when(
    x %in% c("RBWH", "PRINCESS ALEXANDRA", "QCH") ~ "Brisbane (RBWH/PA/QCH)",
    x == "TOWNSVILLE" ~ x,
    x == "GOLD COAST" ~ x,
    .default = "OTHER"
  )
}

df_final_facility <-
  df_times_short |>
  group_by(pu_id) |>
  select(pu_id, FACILITY_NAME_Clean) |>
  slice(n()) |>
  ungroup() |>
  rename(final_facility = FACILITY_NAME_Clean) |>
  mutate(
    final_facility_grp = f_clean_final_facility(final_facility),
    final_facility_lab = case_when(
      final_facility_grp == "OTHER" ~ glue::glue("OTHER ({final_facility})"),
      final_facility_grp == "Brisbane (RBWH/PA/QCH)" ~ glue::glue("BRISBANE ({final_facility})"),
      .default = final_facility
    )
  )


df_ages2 <-
  df_ages |>
  mutate(pu_id = paste0("ID-", pu_id)) |>
  group_by(pu_id) |>
  slice(1) |>
  ungroup()

# add ages and final destination(!!) to df_times_short
df_times_short2 <-
  df_times_short |>
  left_join(df_death_flags2, by = "pu_id") |>
  mutate(death_flag = replace_na(death_flag, "Survived")) |>
  left_join(df_ages2, by = "pu_id") |>
  left_join(df_final_facility, by = "pu_id")


# add facilities and their coords for those referred to in the RSQ pathways (iTRAQI) but not in the df_facilities
df_new_facilities_with_coords <- list(
  # FACILITY_NAME_Clean; latitude; longitude
  c("MAPOON PHC", -12.016632475931772, 141.9003317386928),
  c("COEN PHC", -13.944773159917592, 143.20149205490424),
  c("PORMPURAAW PHC", -14.899640079895562, 141.6221141698491),
  c("CHILLAGOE PRIMARY HEALTH CENTRE", -17.14997184798341, 144.52796889646453),
  c("CROYDON PRIMARY HEALTH CENTRE", -18.21097767957882, 142.2401596829941),
  c("GEORGETOWN PRIMARY HEALTH CENTRE", -18.29203373558961, 143.54786222660186),
  c("PALM ISLAND HOSPITAL", -18.730124669326756, 146.57884580498148),
  c("BOULIA PRIMARY HEALTH CENTRE", -22.908133626306732, 139.90838848311506),
  c("TEXAS HOSPITAL", -28.849736273857026, 151.17710790186513),
  c("BIRDSVILLE PRIMARY HEALTH CENTRE", -25.897497103362916, 139.35551489854396)
) |>
  (\(x) do.call("rbind", x))() |>
  data.frame() |>
  setNames(c("FACILITY_NAME_Clean", "Latitude", "Longitude")) |>
  mutate(across(!FACILITY_NAME_Clean, as.numeric),
    FCLTY_ID = as.numeric(paste0("999", row_number()))
  )

df_facilities <-
  df_facilities |>
  filter(!FACILITY_NAME_Clean %in% df_new_facilities_with_coords$FACILITY_NAME_Clean) |>
  bind_rows(df_new_facilities_with_coords)

saveRDS(df_itraqi_times, "app/fixtures/_df_itraqi_times.rds")
saveRDS(df_facilities, "app/fixtures/_df_facilities.rds")
saveRDS(df_times_short2, "app/fixtures/_df_times_short.rds")
