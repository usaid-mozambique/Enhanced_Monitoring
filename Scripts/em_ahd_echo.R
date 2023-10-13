
# write_tsv NA values to ""
# check monthly submission headers
# need to update script to include new MI indicators (CD4 and Revelacao)

# DEPENDENCIES ------------------------------------------------------------


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

folder_month <- "2023_09"

path_monthly_input_repo <- glue::glue("Data/AHD/{folder_month}/")
input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")
path_monthly_ahd_output_file <- glue::glue("Dataout/AHD/monthly_processed_test/AHD_{folder_month}.txt")
path_historic_ahd_output_file <- "Dataout/em_ahd.txt"

ajuda_site_map <- pull_sitemap()

# imer
df_em_ahd <- input_files %>%
  map(~ reshape_em_ahd(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)



# txtb
readr::write_tsv(
  df_em_ahd,
  path_monthly_ahd_output_file)


ahd_historic_files <- dir("Dataout/AHD/monthly_processed_test/", pattern = "*.txt")

ahd_historic <- ahd_historic_files %>% 
  map(~ read_tsv(file.path("Dataout/AHD/monthly_processed_test/", .))) %>%
  reduce(rbind) %>%
  clean_em_ahd()

readr::write_tsv(
  ahd_historic,
  path_historic_ahd_output_file)
