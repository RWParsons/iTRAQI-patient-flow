get_files <- function(data_dir) {
  # list.files(data_dir)
  file.path(data_dir, c(
    "D_Table_FacilityDescription.sav",
    "D_Table_HealthService_Time_UniqueLocation_Transport1stEnc_Summary.sav",
    "D_Table_TimeAndLocation_LongFileSELECT.sav"
  ))
}

load_data <- function(data_files) {
  data <- map(data_files, haven::read_sav)
  names(data) <- str_remove(basename(data_files), "\\.sav")
  return(data)
}

select_cohort <- function(data) {
  data # placeholder

  # filtered_data <- map(data) {
  #
  # }
}
