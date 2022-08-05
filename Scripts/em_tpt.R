rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
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
month <- "20/06/2022" 
file <- "TPT_2022_06"

# update each month
DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/DoD_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ARIEL_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/CCS_MonthlyEnhancedMonitoringTemplates_FY22_June2022 080722.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ECHO_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/EGPAF_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ICAP-JUN_22-MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/FGH-JUN_22-MonthlyEnhancedMonitoringTemplates_FY22_June2022_July_12_2022.xlsx"

# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/TPT/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1JobyoQqeTP3M5VvZWMC4AMBW04nVwDeD") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_tpt.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# LOAD METADATA -----------------------------------------------------------


ajuda_site_map <- read_sheet(path_ajuda_site_map) %>%
  select(sisma_uid = sisma_id,
         datim_uid =  orgunituid,
         site_nid,
         partner = `IP FY20`,
         snu = SNU,
         psnu = Psnu,
         sitename = Sitename,
         his_epts = epts,
         his_emr = emr,
         his_idart = idart,
         his_disa = disa,
         support_ovc = ovc,
         support_ycm = ycm,
         ovc,
         ycm,
         latitude = Lat,
         longitude = Long)


# CREATE FUNCTION TPT RESHAPE ---------------------------------------------


tpt_reshape <- function(filename, ip){
  
  df <- read_excel(filename, sheet = "TPT Completion", 
                   col_types = c("numeric", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"),
                   skip = 7) %>%
    select(c(No,
             Partner,
             Province,
             District,
             `Health Facility`,
             DATIM_code,
             SISMA_code,
             Period,
             TX_CURR,
             TX_CURR_TPT_Com,
             TX_CURR_TPT_Not_Comp,
             TX_CURR_TB_tto,
             TX_CURR_TPT_Not_Comp_POS_Screen,
             TX_CURR_Eleg_TPT_Comp,
             TX_CURR_W_TPT_last7Mo,
             TX_CURR_Eleg_TPT_Init)) %>%
    filter(Partner == ip)
  
}


# IMPORT & RESHAPE TPT SUBMISSIONS -------------------------------------------------


dod <- tpt_reshape(DOD, "JHPIEGO-DoD")
echo <- tpt_reshape(ECHO, "ECHO")
ariel <- tpt_reshape(ARIEL, "ARIEL")
ccs <- tpt_reshape(CCS, "CCS")
egpaf <- tpt_reshape(EGPAF, "EGPAF")
fgh <- tpt_reshape(FGH, "FGH")
icap <- tpt_reshape(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


tpt <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)


# CALCULATE NEW VARIABLES, PIVOT AND RENAME VARIABLES ---------------------


tpt_tidy <- tpt %>%
  mutate(TPT_candidates = TX_CURR - (TX_CURR_TPT_Com + TX_CURR_W_TPT_last7Mo) - (TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen),
         TPT_ineligible = TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen,
         TPT_active_complete = TX_CURR_W_TPT_last7Mo + TX_CURR_TPT_Com) %>%
  pivot_longer(TX_CURR:TPT_active_complete, names_to = "attribute", values_to = "value") %>%
  mutate(indicator = attribute) %>%
  mutate(indicator = dplyr::recode(indicator,
                                   "TX_CURR_W_TPT_last7Mo"= "Actively on TPT", # use to create new indicator
                                   "TX_CURR_TB_tto" = "Recent Active TB TX",
                                   "TX_CURR_TPT_Not_Comp_POS_Screen" = "Recent Pos TB Screen",
                                   "TX_CURR_TPT_Com" = "TPT Completed",  # use to create new indicator
                                   "TPT_candidates" = "TPT Candidates",
                                   "TPT_ineligible" = "TPT Ineligible",
                                   "TX_CURR_TPT_Not_Comp" = "TPT Not Comp",
                                   "TPT_active_complete" = "TPT Completed/Active"),
         Period = {month}
  ) %>%
  filter(!indicator %in% c("TX_CURR_Eleg_TPT_Init", "TX_CURR_Eleg_TPT_Comp")) %>%
  select(-c(No))


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------

# write to local
readr::write_csv(
  tpt_tidy,
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.csv'))


#---- SURVEY AND COMBINE ALL MONTHLY TPT DATASETS TO BUILD HISTORIC DATASET ---------------------------------


# historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE
# 
# chr_files <- historic_files[historic_files %in% c("TPT_2021_03.csv", "TPT_2021_04.csv")]
# non_chr_files <- historic_files[historic_files %ni% c("TPT_2021_03.csv", "TPT_2021_04.csv")]
# 
# temp_chr_files <- chr_files %>%
#   map(~ read_csv(file.path(path_monthly_output_repo, .))) %>%
#   reduce(rbind) %>% 
#   mutate(Period = as.Date(Period, "%d/%m/%Y")) 
# 
# temp_non_chr_files <- non_chr_files %>%
#   map(~ read_csv(file.path(path_monthly_output_repo, .))) %>%
#   reduce(rbind) %>% 
#   mutate(Period = as.Date(Period, "%d/%m/%Y")) 
# 
# tpt_tidy_history <- bind_rows(temp_non_chr_files, temp_chr_files)


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE


tpt_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) %>% 
  mutate(Period = as.Date(Period, "%d/%m/%Y")) 


# write_tsv(
#   tpt_tidy_history,
#   "Dataout/TPT/outdated/TPT_2021_03_2022_06.txt")

#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- tpt_tidy_history %>% 
  mutate(date = as.Date(Period, format =  "%y/%m/%d")) %>% 
  select(DATIM_code, date, indicator, value) %>% 
  filter(date == max(date),
         indicator == "TX_CURR") %>% 
  mutate(site_volume = case_when(
    value < 1000 ~ "Low",
    between(value, 1000, 5000) ~ "Medium",
    value > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(DATIM_code, site_volume) %>% 
  glimpse()


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


tpt_tidy_history_2 <- tpt_tidy_history %>%
  left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>% 
  left_join(volumn_period) %>% 
  select(datim_uid = DATIM_code,
         sisma_uid,
         site_nid,
         period = Period,
         partner,
         snu,
         psnu,
         sitename,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         indicator,
         attribute,
         value) %>% 
  glimpse()


# WRITE FINAL OUTPUT TO DISK ----------------------------------------------

# write to local
readr::write_tsv(
  tpt_tidy_history_2,
  "Dataout/em_tpt.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)


# GT TABLES ---------------------------------------------------------------


tbl <- tpt_tidy_history_2 %>%
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
  
  tab_header(title = "Mozambique TPT Enhanced Monitoring") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 
  

tbl


# OUTPUT DATASET FOR SIMS PRIOTIZATION ------------------------------------


sims_indicator <- tpt_tidy_history_2 %>% 
  filter(indicator %in% c("TPT Completed/Active", "TX_CURR"),
         period == max(period),
         partner == "ECHO") %>% 
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
  "~/GitHub/SIMS/Dataout/tpt_comp.txt")
  
  