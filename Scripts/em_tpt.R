rm(list = ls())

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


# DEFINE VALUES AND PATHS ---------------------------

# update each month
month <- "2022-12-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2022_12/"


# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

# auto-generated file name used to write monthly dataset to disk
file <- glue::glue("TPT_{dt}")

# value for filtering gt table
month_lag6 <- as.Date(month) - months(5)

# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_FGH.xlsx")


# do not update each month
path_monthly_output_repo <- "Dataout/TPT/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1JobyoQqeTP3M5VvZWMC4AMBW04nVwDeD") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_tpt.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# LOAD METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap()


# IMPORT & RESHAPE TPT SUBMISSIONS -------------------------------------------------


dod <- reshape_em_tpt(DOD, "JHPIEGO-DoD")
echo <- reshape_em_tpt(ECHO, "ECHO")
ariel <- reshape_em_tpt(ARIEL, "ARIEL")
ccs <- reshape_em_tpt(CCS, "CCS")
egpaf <- reshape_em_tpt(EGPAF, "EGPAF")
fgh <- reshape_em_tpt(FGH, "FGH")
icap <- reshape_em_tpt(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


tpt_monthly <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)


# detect lines not coded with datim_uids
tpt_monthly %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


# write to local
readr::write_tsv(
  tpt_monthly,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# HISTORIC DATASET BUILD ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")

tpt_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) 


# JOIN METADATA & CLEAN DATAFRAME -----------------------


tpt_historic_meta <- clean_em_tpt(tpt_historic)

tpt_historic_meta %>% 
  distinct(partner)


# PLOTS & TABLES ---------------------------------------------------------------


tbl <- tpt_historic_meta %>%
  select(indicator, period, value) %>% 
  filter(period >= month_lag6) %>% 
  arrange((period)) %>% 
  mutate(row_n = row_number(),
         period = as.character(period, format = "%b %y")) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  group_by(indicator) %>%
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  gt(rowname_col = "indicator") %>% 
  
  fmt_number(
    columns = !c(indicator), 
    rows = everything(),
    sep_mark = ",",
    decimals = 0) %>% 
  
  cols_width(
    indicator ~ px(200),
    everything() ~ px(100)) %>% 
  
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1),),
    locations = cells_body(
      columns = everything(),
      rows = everything())) %>% 
  
  tab_options(
    table.font.size = 18,
    table.font.names = "SourceSansPro-Regular",
    footnotes.font.size = 8) %>% 
  
  tab_header(title = "Mozambique TPT Enhanced Monitoring - 6 Month Trend") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 


tbl


# WRITE FINAL OUTPUTS ----------------------------------------------


# write to local
readr::write_tsv(
  tpt_historic_meta,
  "Dataout/em_tpt.txt")

# write to Google Drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)


# OUTPUT DATASET FOR SIMS PRIOTIZATION ------------------------------------


sims_indicator <- tpt_historic_meta %>% 
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
  "~/GitHub/SIMS/Data/tpt_comp.txt")

