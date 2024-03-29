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

 
# VALUES & PATHS ---------------------------

# update each month
month <- "2023-02-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2023_02/"


# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("TXTB_{dt}")


month_lag6 <- as.Date(month) - months(5) # value for filtering gt table


# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Fev_2023_FGH.xlsx")



# do not update each month
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_txtb.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved


# METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap()


# FUNCTIONS RUN -------------------------------------------------


dod <- reshape_em_txtb(DOD, "JHPIEGO-DoD")
echo <- reshape_em_txtb(ECHO, "ECHO")
ariel <- reshape_em_txtb(ARIEL, "ARIEL")
ccs <- reshape_em_txtb(CCS, "CCS")
egpaf <- reshape_em_txtb(EGPAF, "EGPAF")
fgh <- reshape_em_txtb(FGH, "FGH")
icap <- reshape_em_txtb(ICAP, "ICAP")


# COMPILE DATASETS --------------------------------------------------

txtb_monthly <- bind_rows(dod, ariel, ccs, egpaf, icap, echo, fgh)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
txtb_monthly %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")


# MONTHLY FILE WRITE ------------------------------------

# write to local
readr::write_tsv(
  txtb_monthly,
  {path_monthly_output_file})

# write to Google Drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# HISTORIC DATASET BUILD ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")

txtb_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind)


# JOIN METADATA & CLEAN DATAFRAME -----------------------


txtb_historic_meta <- clean_em_txtb(txtb_historic)

# detect lines not coded with datim_uids
txtb_historic_meta %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)


# PLOTS & TABLES ---------------------------------------------------------------


tbl <- txtb_historic_meta %>%
  pivot_longer(cols = TX_CURR:TX_TB_CURR_N, names_to = "indicator", values_to = "value") %>% 
  filter(period >= month_lag6) %>% 
  select(indicator, period, value) %>% 
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
  
  tab_header(title = "Mozambique TX_TB Enhanced Monitoring - 6 Month Trend") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring") 


tbl


# OUTPUT WRITE ----------------------------------------------


readr::write_tsv(
  txtb_historic_meta,
  "Dataout/em_txtb.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

