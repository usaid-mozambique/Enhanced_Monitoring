library(tidyverse)
library(mozR)
library(glamr)
library(googlesheets4)
library(googledrive)
library(fs)
library(lubridate)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(gt)
load_secrets()


# TESTING -----------------------------------------------------------------



# folder where monthly submissions are stored. Update monthly!
folder_month <- "2024_09"
path_monthly_input_repo <- glue::glue("Data/Ajuda/ER_DSD_TPT_VL/test/")
input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")

# paths for saving monthly datasets on local drive
path_monthly_txtb_output_file <- glue::glue("Dataout/TXTB/monthly_processed/TXTB_{folder_month}.txt")

path_monthly_txtb_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3")

path_historic_txtb_output_file <- "Dataout/em_txtb.txt"

path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")

# 1. LOAD SITE META DATA ---------------------------------------------------------------

ajuda_site_map <- pull_sitemap()


# txtb not working
df_em_txtb <- input_files %>%
  map(~ reshape_em_txtb(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind) |> 
  filter(value > 0)

unique(df_em_txtb$indicator)

# txtb
readr::write_tsv(
  df_em_txtb,
  "Dataout/TXTB/monthly_processed_historic_sept24_change/TXTB_2024_09.txt")


tx_tb_historic_files <- dir("Dataout/TXTB/monthly_processed_historic_sept24_change/", pattern = "*.txt")




txtb_historic <- tx_tb_historic_files %>%
  map(~ read_tsv(file.path("Dataout/TXTB/monthly_processed_historic_sept24_change/", .))) %>%
  reduce(rbind) %>%
  mutate(row_n = row_number()) |> 
  tidyr::pivot_wider(
    names_from = indicator,
    values_from = value,
    values_fn = sum  # Or another function like `sum` or `first`
  ) |> 
  dplyr::group_by(partner, snu, psnu, sitename, datim_uid, period, disaggregate, disaggregate_sub, sex, age) %>% 
  summarise(across(starts_with("TX_"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

clean_em_txtb()


df0 <- reshape_em_txtb("Data/Ajuda/ER_DSD_TPT_VL/2024_09/ARIEL MonthlyEnhancedMonitoringTemplates_FY24_SEP.xlsx")





df <- read_excel("Data/Ajuda/ER_DSD_TPT_VL/test/ARIEL MonthlyEnhancedMonitoringTemplates_FY24_SEP.xlsx", 
                 sheet = "TX_TB", skip = 7) |> 
  dplyr::filter(partner == "ARIEL") |>  # ARGUMENT
  dplyr::select(!c(contains(c("remove", "tot")),
                   sisma_nid,
                   No)) %>%
  
  dplyr::rename(snu = snu1) %>%
  
  tidyr::pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk',
                      names_to = c("indicator", "disaggregate", "sex", "age", "disaggregate_sub"),
                      names_sep = "_",
                      values_to = "value") |> 
  dplyr::mutate(period = "2024 09",
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = dplyr::recode(age,
                                    "Unk" = "Unknown Age"), # new code to correct age
                disaggregate = dplyr::recode(disaggregate,
                                             "newART" = "New on ART",
                                             "alreadyART" = "Already on ART",
                                             "mWRD" = "GX"))
  

unique(df0$indicator)












ip_temp <- extract_em_meta(filename, type = "ip")
month_temp <- extract_em_meta(filename, type = "month")

df <- read_excel(filename,
                 sheet = "TX_TB",
                 skip = 7,
                 .name_repair = "unique_quiet") %>%
  
  dplyr::filter(partner == ip_temp) %>%
  
  dplyr::select(!c(contains(c("remove", "tot")),
                   sisma_nid,
                   No)) %>%
  
  dplyr::rename(snu = snu1) %>%
  
  tidyr::pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk',
                      names_to = c("indicator", "disaggregate", "sex", "age", "disaggregate_sub"),
                      names_sep = "_",
                      values_to = "value") %>%
  
  dplyr::mutate(period = month_temp,
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = dplyr::recode(age,
                                    "Unk" = "Unknown Age"), # new code to correct age
                disaggregate = dplyr::recode(disaggregate,
                                             "newART" = "New on ART",
                                             "alreadyART" = "Already on ART",
                                             "mWRD" = "GX"))
