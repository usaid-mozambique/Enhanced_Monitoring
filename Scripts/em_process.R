
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


# GLOBAL VARIABLES ---------------------------------------------------------------

# folder where monthly submissions are stored. Update monthly!
folder_month <- "2024_05"

path_monthly_input_repo <- glue::glue("Data/Ajuda/ER_DSD_TPT_VL/{folder_month}/")
input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")

# paths for saving monthly datasets on local drive
path_monthly_imer_output_file <- glue::glue("Dataout/IMER/monthly_processed/IMER_{folder_month}.txt")
path_monthly_prep_output_file <- glue::glue("Dataout/PrEP/monthly_processed/PREP_{folder_month}.txt")
path_monthly_dsd_output_file <- glue::glue("Dataout/DSD/monthly_processed/DSD_{folder_month}.txt")
path_monthly_mi_output_file <- glue::glue("Dataout/MI/monthly_processed/MI_{folder_month}.txt")
path_monthly_tpt_output_file <- glue::glue("Dataout/TPT/monthly_processed/TPT_{folder_month}.txt")
path_monthly_txtb_output_file <- glue::glue("Dataout/TXTB/monthly_processed/TXTB_{folder_month}.txt")
path_monthly_ahd_output_file <- glue::glue("Dataout/AHD/monthly_processed/AHD_{folder_month}.txt")
path_monthly_ahdhiv_output_file <- glue::glue("Dataout/AHD_HIV/monthly_processed/AHD_HIV_{folder_month}.txt")

# paths for saving monthly datasets on google drive
path_monthly_imer_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU")
path_monthly_prep_output_gdrive <- as_id("https://drive.google.com/drive/folders/1BYq-xdMhxw8sOUHYwFiZH8_w2UsQmBun") 
path_monthly_dsd_output_gdrive <- as_id("https://drive.google.com/drive/folders/15x2biGIIYrY_eW-zrKQbrvMT47cI5yOE")
path_monthly_mi_output_gdrive <- as_id("https://drive.google.com/drive/folders/1RC5VFhD7XkuptW7o3zd21ujY6CefcTyv")
path_monthly_tpt_output_gdrive <- as_id("https://drive.google.com/drive/folders/1JobyoQqeTP3M5VvZWMC4AMBW04nVwDeD") 
path_monthly_txtb_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3")
path_monthly_ahd_output_gdrive <- as_id("https://drive.google.com/drive/folders/1lVao3i2vP6BW41A5T52jKgPbbaOVgNKG")
path_monthly_ahdhiv_output_gdrive <- as_id("https://drive.google.com/drive/folders/18V88s6oB7bPFh3Sv4rJU_oMhJgMqXyCi")

# paths for saving historic datasets on local drive
path_historic_imer_output_file <- "Dataout/em_imer.txt"
path_historic_prep_output_file <- "Dataout/em_prep.txt"
path_historic_dsd_output_file <- "Dataout/em_dsd.txt"
path_historic_mi_output_file <- "Dataout/em_mi.txt"
path_historic_tpt_output_file <- "Dataout/em_tpt.txt"
path_historic_txtb_output_file <- "Dataout/em_txtb.txt"
path_historic_ahd_output_file <- "Dataout/em_ahd.txt"
path_historic_ahdhiv_output_file <- "Dataout/em_ahdhiv.txt"

# path for saving historical datasets on google drive
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")

# 1. LOAD SITE META DATA ---------------------------------------------------------------

ajuda_site_map <- pull_sitemap()

# 2.1 PROCESS MONTHLY SUBMISSIONS ----------------------------------------------------------

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

# mi  the miscalc looks ok through this step
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
  reduce(rbind) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
  dplyr::group_by(partner, snu, psnu, sitename, datim_uid, period, disaggregate, sex, age) %>% 
  summarise(across(starts_with("TX_"), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup()

# ahd
df_em_ahd <- input_files %>%
  map(~ reshape_em_ahd(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# ahdhiv
df_em_ahdhiv <- input_files %>%
  map(~ reshape_em_ahdhiv(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)

# 2.2 VALIDATE MONTHLY DATIM_UIDS --------------------------------------------------

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

df_em_ahd %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")

df_em_ahdhiv %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")


# 2.3 WRITE MONTHLY DATASETS TO DISK ---------------------------------------------------

# imer
readr::write_tsv(
  df_em_imer,
  path_monthly_imer_output_file)

# prep
readr::write_tsv(
  df_em_prep,
  path_monthly_prep_output_file)

# dsd
readr::write_tsv(
  df_em_dsd,
  path_monthly_dsd_output_file)

# mi
readr::write_tsv(
  df_em_mi,
  path_monthly_mi_output_file)

# tpt
readr::write_tsv(
  df_em_tpt,
  path_monthly_tpt_output_file)

# txtb
readr::write_tsv(
  df_em_txtb,
  path_monthly_txtb_output_file)

# ahd
readr::write_tsv(
  df_em_ahd,
  path_monthly_ahd_output_file)

# ahdhiv
readr::write_tsv(
  df_em_ahdhiv,
  path_monthly_ahdhiv_output_file)

# 2.4 WRITE MONTHLY TO GOOGLE DRIVE --------------------------------------------------


# imer
drive_put(path_monthly_imer_output_file,
          path = path_monthly_imer_output_gdrive)
# prep
drive_put(path_monthly_prep_output_file,
          path = path_monthly_prep_output_gdrive)
# dsd
drive_put(path_monthly_dsd_output_file,
          path = path_monthly_dsd_output_gdrive)
# mi
drive_put(path_monthly_mi_output_file,
          path = path_monthly_mi_output_gdrive)
# tpt
drive_put(path_monthly_tpt_output_file,
          path = path_monthly_tpt_output_gdrive)
# txtb
drive_put(path_monthly_txtb_output_file,
          path = path_monthly_txtb_output_gdrive)
# ahd
drive_put(path_monthly_ahd_output_file,
          path = path_monthly_ahd_output_gdrive)

# ahdhiv
drive_put(path_monthly_ahdhiv_output_file,
          path = path_monthly_ahdhiv_output_gdrive)


# 3.1 BUILD HISTORICAL DATASETS -----------------------------------------------


imer_historic_files <- dir("Dataout/IMER/monthly_processed/", pattern = "*.txt")
prep_historic_files <- dir("Dataout/PrEP/monthly_processed/", pattern = "*.txt")
dsd_historic_files <- dir("Dataout/DSD/monthly_processed/", pattern = "*.txt")
mi_historic_files <- dir("Dataout/MI/monthly_processed/", pattern = "*.txt")
tpt_historic_files <- dir("Dataout/TPT/monthly_processed/", pattern = "*.txt")
tx_tb_historic_files <- dir("Dataout/TXTB/monthly_processed/", pattern = "*.txt")
ahd_historic_files <- dir("Dataout/AHD/monthly_processed/", pattern = "*.txt")
ahdhiv_historic_files <- dir("Dataout/AHD_HIV/monthly_processed/", pattern = "*.txt")

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

ahd_historic <- ahd_historic_files %>% 
  map(~ read_tsv(file.path("Dataout/AHD/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_ahd() %>% 
  filter(value > 0) # consider putting in the monthly 

ahdhiv_historic <- ahdhiv_historic_files %>%
  map(~ read_tsv(file.path("Dataout/AHD_HIV/monthly_processed/", .))) %>%
  reduce(rbind) %>%
  clean_em_ahdhiv()


# 3.2 REVIEW HISTORICAL DATASETS ------------------------------------------------------------


imer_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

prep_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

dsd_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

mi_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

tpt_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

txtb_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

ahd_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

ahdhiv_historic %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename, period)

plot_em_imer(imer_historic)
plot_em_prep(prep_historic)
plot_em_dsd(dsd_historic)
plot_em_mi(mi_historic)
plot_em_tpt(tpt_historic)
plot_em_txtb(txtb_historic)


# 3.3 REMOVE DATIM_UID NA DATA LINES --------------------------------------


imer_historic <- imer_historic %>% 
  filter(!is.na(datim_uid))

prep_historic <- prep_historic %>% 
  filter(!is.na(datim_uid))

dsd_historic <- dsd_historic %>% 
  filter(!is.na(datim_uid))

mi_historic <- mi_historic %>% 
  filter(!is.na(datim_uid))

tpt_historic <- tpt_historic %>% 
  filter(!is.na(datim_uid))

txtb_historic <- txtb_historic %>% 
  filter(!is.na(datim_uid))

ahd_historic <- ahd_historic %>% 
  filter(!is.na(datim_uid))

ahdhiv_historic <- ahdhiv_historic %>% 
  filter(!is.na(datim_uid))


# 3.4 WRITE HISTORIC TO LOCAL DRIVE --------------------------------------------------


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
  mi_historic,
  path_historic_mi_output_file,
  na = "")

readr::write_tsv(
  tpt_historic,
  path_historic_tpt_output_file)

readr::write_tsv(
  txtb_historic,
  path_historic_txtb_output_file)

readr::write_tsv(
  ahd_historic,
  path_historic_ahd_output_file)

readr::write_tsv(
  ahdhiv_historic,
  path_historic_ahdhiv_output_file)


# 3.4 WRITE HISTORIC TO GOOGLE DRIVE --------------------------------------------------

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
# ahd
drive_put(path_historic_ahd_output_file,
          path = path_historic_output_gdrive)
# ahdhiv
drive_put(path_historic_ahdhiv_output_file,
          path = path_historic_output_gdrive)


# 4. USAID SPECIFIC OUTPUTS --------------------------------------------------


df_sitemap <- pull_sitemap()
df_psnuuid <- pull_sitemap(sheetname = "list_psnu")

mi_historic_vl_dashboard <- mi_historic %>% 
  select(datim_uid:program_mi, starts_with("cv")) %>% 
  pivot_longer(starts_with("cv"), names_to = "indicator", values_to = "value") %>% 
  filter(!is.na(value),
         period > "2022-05-20") %>% 
  select(!c(sisma_uid, sisma_uid_datim_map, site_nid, numdenom, program_ap3, program_mi)) %>% 
  rename(ageasentered = age) %>% 
  mutate(source = "MI",
         sex = NA_character_) %>% 
  left_join(df_sitemap %>% select(datim_uid, sisma_uid, site_nid, his_emr, his_epts, his_idart, his_disa), by = "datim_uid") %>% 
  left_join(df_psnuuid %>% select(psnu, psnuuid), by = "psnu") %>% 
  select(datim_uid,
         sisma_uid,
         snu,
         psnu,
         psnuuid,
         sitename, 
         period,
         starts_with("his_"),
         source,
         partner,
         pop_type,
         sex,
         ageasentered,
         indicator,
         value) %>% 
  glimpse()


readr::write_tsv(
  mi_historic_vl_dashboard,
  "Dataout/vl_dashboard_mi.txt")



sims_indicator <- tpt_historic %>% 
  filter(indicator %in% c("TPT Completed/Active", "TX_CURR"),
         period == max(period)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(period, datim_uid, snu, psnu, sitename) %>% 
  summarize(TX_CURR = sum(TX_CURR, na.rm = TRUE),
            TPT_CUM = sum(`TPT Completed/Active`, na.rm = TRUE)) %>% 
  mutate(TPT_CUM_PER = TPT_CUM / TX_CURR) %>% 
  ungroup() %>% 
  select(snu, 
         psnu, 
         sitename,
         orgunituid = datim_uid,
         TPT_CUM_PER)

readr::write_tsv(
  sims_indicator,
  "~/GitHub/MER/Data/tpt_comp.txt")
