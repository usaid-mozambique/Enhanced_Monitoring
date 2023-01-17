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

# DEFINE REPORTING MONTH AND FILE PATHS -------------------------------------------

# update each month
month <- "2022-12-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2022_12/"

# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("MQ_{dt}")

month_lag6 <- as.Date(month) - months(5) # value for filtering gt table


# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_FGH.xlsx")


# do not update each month
path_monthly_output_repo <- "Dataout/MQ_CV/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1RC5VFhD7XkuptW7o3zd21ujY6CefcTyv") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_mqcv.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# LOAD DATASETS -----------------------------------------------------------

ajuda_site_map <- pull_sitemap() %>% 
  select(datim_uid,
         sisma_uid,
         site_nid,
         snu,
         psnu, 
         sitename,
         partner = partner_pepfar_clinical,
         program_ap3)

# IMPORT & RESHAPE MQ SUBMISSIONS -----------------------------------------------------------

dod <- reshape_em_mq(DOD, "JHPIEGO-DoD")
echo <- reshape_em_mq(ECHO, "ECHO")
fgh <- reshape_em_mq(FGH, "FGH")
ariel <- reshape_em_mq(ARIEL, "ARIEL")
icap <- reshape_em_mq(ICAP, "ICAP")
ccs <- reshape_em_mq(CCS, "CCS")
egpaf <- reshape_em_mq(EGPAF, "EGPAF")

# COMPILE IP SUMBISSIONS --------------------------------------------------


cv_tidy <- bind_rows(dod, echo, fgh, ariel, icap, ccs, egpaf)


cv_tidy %>% # TABLE HF SUBMISSION LINES BY PARTNER.  CAUTION - BLANK SUBMISSION LINES ARE INCLUDED!
  group_by(Partner) %>% 
  distinct(DATIM_code) %>% 
  summarise(n())


# detect lines not coded with datim_uids
cv_tidy %>% 
  filter(is.na(DATIM_code)) %>% 
  distinct(DATIM_code, Province, District, `Health Facility`) %>% 
  anti_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid"))

rm(echo, fgh, ariel, icap, ccs, egpaf, dod)


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


readr::write_tsv(
  cv_tidy,
  na = "",
  {path_monthly_output_file})


# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")


historic_import <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) %>% 
  mutate(month = as.Date(month, "%Y/%m/%d")) 


# JOIN METADATA ---------------------------------


cv_tidy_historic <- clean_em_mq(historic_import)

# check that all monthly data is coded to a partner
cv_tidy_historic %>% 
  pivot_longer(cols = dpi.colheu.pcr_D:mds.cv.estaveis, names_to = "indicator", values_to = "value") %>% 
  filter(period == month) %>% 
  group_by(partner) %>% 
  distinct(datim_uid) %>% 
  summarise(n())


# GT TABLES ---------------------------------------------------------------


tbl <- cv_tidy_historic %>%
  pivot_longer(cols = dpi.colheu.pcr_D:mds.cv.estaveis, names_to = "indicator", values_to = "value") %>% 
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
    indicator ~ px(220),
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
  
  tab_header(title = "Mozambique MQ Enhanced Monitoring - 6 Month Trend") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 


tbl


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------

# write to local
write_tsv(
  cv_tidy_historic,
  na = "",
  path_historic_output_file)

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

  
