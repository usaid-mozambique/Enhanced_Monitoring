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


# DEFINE MONTH AND PATHS ---------------------------

# update each month
month <- "20/06/2022" 
file <- "TXTB_2022_06"

# update each month
DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/DoD_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ARIEL_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/CCS_MonthlyEnhancedMonitoringTemplates_FY22_June2022 080722.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ECHO_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/EGPAF_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ICAP_Junho_2022_Monitoria Intensiva_ Template_FY22Q3_updated 12072022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/FGH-JUN_22-MonthlyEnhancedMonitoringTemplates_FY22_June2022_July_12_2022.xlsx"

# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/TXTB/_CompileHistoric/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_txtb.txt" # folder path where monthly dataset archived
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


# CREATE FUNCTION TXTB RESHAPE ---------------------------------------------


txtb_reshape <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "TX_TB", col_types = c("text", "text", "text",
                                                  "text", "text", "text", "text", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), skip = 7) %>% 
    filter(partner == ip) %>%  
    select(!c(contains(c("remove", "tot")))) %>%
    pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk', 
                 names_to = c("indicator", "disaggregate", "sex", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    mutate(period = as.Date(month, "%d/%m/%Y"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = recode(age, "Unk" = "Unknown"),
           disaggregate = recode(disaggregate, 
                                 "newART" = "New on ART",
                                 "alreadyART" = "Already on ART")) %>% 
    pivot_wider(names_from =  indicator, values_from = value) %>%
    glimpse()
  
}


# IMPORT & RESHAPE TXTB SUBMISSIONS -------------------------------------------------


dod <- txtb_reshape(DOD, "JHPIEGO-DoD")
echo <- txtb_reshape(ECHO, "ECHO")
ariel <- txtb_reshape(ARIEL, "ARIEL")
ccs <- txtb_reshape(CCS, "CCS")
egpaf <- txtb_reshape(EGPAF, "EGPAF")
fgh <- txtb_reshape(FGH, "FGH")
icap <- txtb_reshape(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS & CHECK FOR NA'S IN SITE DATIMUIDS --------------------------------------------------


txtb <- bind_rows(dod, ariel, ccs, egpaf, icap, echo, fgh)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
txtb %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu1, psnu, sitename)


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------

# write to local
readr::write_tsv(
  txtb,
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


#---- SURVEY ALL MONTHLY TXTB DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")

txtb_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind)


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- txtb_tidy_history %>% 
  select(datim_uid, period, TX_CURR) %>%
  filter(period == max(period)) %>% 
  group_by(datim_uid, .drop = TRUE) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(site_volume = case_when(
    TX_CURR < 1000 ~ "Low",
    between(TX_CURR, 1000, 5000) ~ "Medium",
    TX_CURR > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(datim_uid, site_volume) %>% 
  glimpse()


#---- JOIN AJUDA SITEMAP, CLEAN DATAFRAME & CHECK FOR NA'S IN SITE DATIMUIDS -----------------------


txtb_tidy_history_2 <- txtb_tidy_history %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>% 
  left_join(volumn_period) %>% 
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner.y,
         snu,
         psnu = psnu.y,
         sitename = sitename.y,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         sex,
         age,
         disaggregate,
         starts_with("TX_")) %>% 
  glimpse()

# detect lines not coded with datim_uids
txtb_tidy_history_2 %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  txtb_tidy_history_2,
  "Dataout/em_txtb.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

# GT TABLES ---------------------------------------------------------------


tbl <- txtb_tidy_history_2 %>%
  pivot_longer(cols = TX_CURR:TX_TB_CURR_N, names_to = "indicator", values_to = "value") %>% 
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
  
  tab_header(title = "Mozambique TX_TB Enhanced Monitoring") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 


tbl

