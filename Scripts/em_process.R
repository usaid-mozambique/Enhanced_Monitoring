
# write_tsv NA values to ""
# check monthly submission headers

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


# DEFINE GLOBAL VARIABLES ---------------------------------------------------------------


folder_month <- "2023_03"

path_monthly_input_repo <- glue::glue("Data/Ajuda/ER_DSD_TPT_VL/{folder_month}/") # paths for inmporting monthly ip submissions

input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")

# paths for saving monthly datasets on local drive
path_monthly_imer_output_file <- glue::glue("Dataout/IMER/monthly_processed/IMER_{folder_month}.txt")
path_monthly_prep_output_file <- glue::glue("Dataout/PrEP/monthly_processed/PREP_{folder_month}.txt")
path_monthly_dsd_output_file <- glue::glue("Dataout/DSD/monthly_processed/DSD_{folder_month}.txt")
path_monthly_mi_output_file <- glue::glue("Dataout/MI/monthly_processed/MI_{folder_month}.txt")
path_monthly_tpt_output_file <- glue::glue("Dataout/TPT/monthly_processed/TPT_{folder_month}.txt")
path_monthly_txtb_output_file <- glue::glue("Dataout/TXTB/monthly_processed/TXTB_{folder_month}.txt")

# paths for saving monthly datasets on google drive
path_monthly_imer_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU")
path_monthly_prep_output_gdrive <- as_id("https://drive.google.com/drive/folders/1BYq-xdMhxw8sOUHYwFiZH8_w2UsQmBun") 
path_monthly_dsd_output_gdrive <- as_id("https://drive.google.com/drive/folders/15x2biGIIYrY_eW-zrKQbrvMT47cI5yOE")
path_monthly_mi_output_gdrive <- as_id("https://drive.google.com/drive/folders/1RC5VFhD7XkuptW7o3zd21ujY6CefcTyv")
path_monthly_tpt_output_gdrive <- as_id("https://drive.google.com/drive/folders/1JobyoQqeTP3M5VvZWMC4AMBW04nVwDeD") 
path_monthly_txtb_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3")

# paths for saving historic datasets on local drive
path_historic_imer_output_file <- "Dataout/em_imer.txt"
path_historic_prep_output_file <- "Dataout/em_prep.txt"
path_historic_dsd_output_file <- "Dataout/em_dsd.txt"
path_historic_mi_output_file <- "Dataout/em_mi.txt"
path_historic_tpt_output_file <- "Dataout/em_tpt.txt"
path_historic_txtb_output_file <- "Dataout/em_txtb.txt"


# path for saving historical datasets on google drive
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")


# LOAD DATA ---------------------------------------------------------------

ajuda_site_map <- pull_sitemap()

# PROCESS MONTHLY SUBMISSIONS ----------------------------------------------------------

# imer
df_em_imer <- input_files %>%
  map(~ reshape_em_imer(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# prep
df_em_prep <- input_files %>%
  map(~ reshape_em_prep(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# dsd
df_em_dsd <- input_files %>%
  map(~ reshape_em_dsd(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# mi
df_em_mi <- input_files %>%
  map(~ reshape_em_mi(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# tpt
df_em_tpt <- input_files %>%
  map(~ reshape_em_tpt(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# txtb
df_em_txtb <- input_files %>%
  map(~ reshape_em_txtb(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)


# VALIDATE MONTHLY DATIM_UIDS --------------------------------------------------


df_em_imer %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")

df_em_prep %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")

df_em_dsd %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")

df_em_mi %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")

df_em_tpt %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")

df_em_txtb %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")


# WRITE MONTHLY DATASETS TO DISK ---------------------------------------------------

# write to local
readr::write_tsv(
  df_em_imer,
  path_monthly_imer_output_file)

# write to local
readr::write_tsv(
  df_em_prep,
  path_monthly_prep_output_file)

# write to local
readr::write_tsv(
  df_em_dsd,
  path_monthly_dsd_output_file)

# write to local
readr::write_tsv(
  df_em_mi,
  path_monthly_mi_output_file)

# write to local
readr::write_tsv(
  df_em_tpt,
  path_monthly_tpt_output_file)

# write to local
readr::write_tsv(
  df_em_txtb,
  path_monthly_txtb_output_file)



# BUILD HISTORICAL DATASETS -----------------------------------------------

imer_historic_files <- dir("Dataout/IMER/monthly_processed/", pattern = "*.txt")
prep_historic_files <- dir("Dataout/PrEP/monthly_processed/", pattern = "*.txt")
dsd_historic_files <- dir("Dataout/DSD/monthly_processed/", pattern = "*.txt")
mi_historic_files <- dir("Dataout/MI/monthly_processed/", pattern = "*.txt")
tpt_historic_files <- dir("Dataout/TPT/monthly_processed/", pattern = "*.txt")
tx_tb_historic_files <- dir("Dataout/TXTB/monthly_processed/", pattern = "*.txt")


imer_historic <- imer_historic_files %>% 
  map(~ read_tsv(file.path("Dataout/IMER/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_imer()

prep_historic <- prep_historic_files %>% 
  map(~ read_tsv(file.path("Dataout/PrEP/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_prep()

dsd_historic <- dsd_historic_files %>% 
  map(~ read_tsv(file.path("Dataout/DSD/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_dsd()

mi_historic <- mi_historic_files %>% 
  map(~ read_tsv(file.path("Dataout/MI/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_mi() 

tpt_historic <- tpt_historic_files %>%
  map(~ read_tsv(file.path("Dataout/TPT/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_tpt()

txtb_historic <- tx_tb_historic_files %>%
  map(~ read_tsv(file.path("Dataout/TXTB/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_txtb()


# REVIEW HISTORICAL DATASETS ------------------------------------------------------------


imer_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)

prep_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)

dsd_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)

mi_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)

tpt_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)

txtb_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)


plot_em_imer(imer_historic)
plot_em_prep(prep_historic)
plot_em_dsd(dsd_historic)
plot_em_mi(mi_historic)
plot_em_tpt(tpt_historic)
plot_em_txtb(txtb_historic)



# WRITE HISTORIC TO LOCAL DRIVE --------------------------------------------------

readr::write_tsv(
  imer_historic,
  path_historic_imer_output_file)

readr::write_tsv(
  prep_historic,
  path_historic_prep_output_file)

readr::write_tsv(
  dsd_historic,
  path_historic_dsd_output_file)

readr::write_tsv(
  mi_historic,
  path_historic_mi_output_file)

readr::write_tsv(
  tpt_historic,
  path_historic_tpt_output_file)

readr::write_tsv(
  txtb_historic,
  path_historic_txtb_output_file)


# WRITE HISTORIC TO GOOGLE DRIVE --------------------------------------------------

# imer
drive_put(path_historic_imer_output_file,
          path = path_historic_output_gdrive)
# prep
drive_put(path_historic_prep_output_file,
          path = path_historic_output_gdrive)
# dsd
drive_put(path_historic_dsd_output_file,
          path = path_historic_output_gdrive)
# mi
drive_put(path_historic_mi_output_file,
          path = path_historic_output_gdrive)
# tpt
drive_put(path_historic_tpt_output_file,
          path = path_historic_output_gdrive)
# txtb
drive_put(path_historic_txtb_output_file,
          path = path_historic_output_gdrive)
