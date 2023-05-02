# wrangle CSCF data to add flag to facilities data
library(tidyverse)


data_dir <- "U:\\Research\\Projects\\health\\jti_research_data\\jti_itraqi"
minimum_cscf_for_ct_flag <- 4
cscf_service_for_flag <- "Alcohol & Other Drugs_Emergency"


df_facilities <- haven::read_spss(file.path(data_dir, "D_Table_FacilityDescription.sav")) 
df_cscf <- readxl::read_xlsx(file.path(data_dir, "CSCF_PublicHospitals_20220502.xlsx"), skip = 1) %>%
  rename("cscf_service"=1)

header_flag <- 
  df_cscf |> 
  select(cscf_service) |> 
  mutate(flag = str_detect(cscf_service, "·")) |> 
  pull(flag) |> 
  zoo::rollapply(
    FUN = function(vector, sequence) {
      all(vector == sequence)
    },
    sequence = c(FALSE, TRUE), width = 2) |> 
  append(FALSE)

for(i in 1:length(which(header_flag))) {
  h_idx <- which(header_flag)[i]
  header_text <- df_cscf$cscf_service[h_idx]
  
  segment_start <- h_idx + 1
  segment_end <- ifelse(
    i == length(which(header_flag)),
    nrow(df_cscf), 
    which(header_flag)[i+1] - 1
  )
  
  df_segment <- df_cscf[segment_start:segment_end, ]
  df_segment$cscf_service <- str_replace(
    df_segment$cscf_service, 
    "·[[:space:]]+", 
    paste0(header_text, "_")
  )
  
  df_cscf[segment_start: segment_end, ] <- df_segment
}

df_cscf <- df_cscf[-which(header_flag), ]


name_correction_pairs <- list(
  c("Caloundra Health Service", "caloundra"),
  c("Domadgee Hospital", "doomadgee"),
  c("Cooktown Multipurpose Health Service", "cooktown"),
  c("Gold Coast University Hospital", "gold coast"),
  c("Gordon", "GORDONVALE"),
  c("Hopevale PHC", "hope vale"),
  c("Mackay Base / Community", "mackay"),
  c("Maleny Soldiers Memorial Hospital", "maleny"),
  c("Mt Isa Hospital", "mount isa"),
  c("Mt Morgan MPHS", "mount morgan"),
  c("Mungidi", "mungindi"),
  c("Nambour General Hospital", "nambour"),
  c("Lady Cilento Children’s Hospital", "QCH"),
  c("QEII Hospital", "QUEEN ELIZABETH II"),
  c("Sunshine Coast University Hospital", "sunshine coast"),
  c("Thargo minda", "thargomindah"),
  c("TPCH", "PRINCE CHARLES"),
  c("The Townsville Hospital", "townsville"),
  c("Weipa Integrated Health Service", "weipa"),
  c("Wall umbilla", "wallumbilla"),
  c("Wynnum Health Service", "wynnum")
)

df_cscf_name_corrections <- 
  do.call("rbind", name_correction_pairs) |> 
  data.frame() |> 
  rename("cscf_name_old" = 1, "cscf_name_new" = 2)
  

df_cscf_long <-
  df_cscf |> 
  mutate(across(everything(), as.character)) |> 
  pivot_longer(!cscf_service) |> 
  left_join(df_cscf_name_corrections, by=c("name" = "cscf_name_old")) |> 
  mutate(
    name = ifelse(is.na(cscf_name_new), name, cscf_name_new),
    name = str_trim(str_remove(tolower(name), "hospital|phc|mphs"))
  ) |> 
  select(-cscf_name_new)

df_emerg_cscf <-
  df_cscf_long |> 
  filter(cscf_service == cscf_service_for_flag) |> 
  select(-cscf_service) |> 
  rename("emergency_cscf" = value) |> 
  mutate(
    emergency_cscf = ifelse(as.numeric(emergency_cscf) >= minimum_cscf_for_ct_flag, 1, 0),
    emergency_cscf = replace_na(emergency_cscf, 0),
    name = toupper(name)
  ) |> 
  group_by(name) |> 
  arrange(desc(emergency_cscf)) |> 
  slice(1) |> 
  ungroup()

df_facilities |> filter(!tolower(FACILITY_NAME_Clean) %in% tolower(df_cscf_long$name)) # check which ones don't have a match

df_facilities_with_flag <- 
  df_facilities |> 
  left_join(df_emerg_cscf, by = c("FACILITY_NAME_Clean" = "name"))
