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

# DEFINE REPORTING MONTH AND FILE PATHS -------------------------------------------

month <- "2022-06-20"
file <- "MQ_2022_06"


# do not update each month
DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/DoD_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ARIEL_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/CCS_MonthlyEnhancedMonitoringTemplates_FY22_June2022 080722.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ECHO_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/EGPAF_MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/ICAP-JUN_22-MonthlyEnhancedMonitoringTemplates_FY22_June2022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_06/FGH-JUN_22-MonthlyEnhancedMonitoringTemplates_FY22_June2022_July_12_2022.xlsx"

# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/MQ_CV/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1RC5VFhD7XkuptW7o3zd21ujY6CefcTyv") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_mqcv.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved


# LOAD DATASETS -----------------------------------------------------------


ajuda_site_map <- read_sheet(path_ajuda_site_map) %>%
  select(orgunituid,
         sisma_id,
         SNU,
         Psnu, 
         Sitename,
         partner = `IP FY20`) %>% 
  select_all(str_to_lower)


# CREATE FUNCTION TO TIDY CV DATASETS ---------------------------------------------------------

# old function to use May '22 back
cv_tidy <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "Monitoria Intensiva",
                   skip = 9,
                   col_types = c("text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric")) %>% 
    rename(dpi.colheu.pcr_d__all = dpi.colheu.pcr_d_total,
           dpi.colheu.pcr_n__all = dpi.colheu.pcr_n_total,
           dpi.colheu.pcr_prop__all = dpi.colheu.pcr_prop_total,
           dpi.pcr.enviado_d__all = dpi.pcr.enviado_d_total,
           dpi.pcr.enviado_n__all = dpi.pcr.enviado_n_total,
           dpi.pcr.enviado_prop__all = dpi.pcr.enviado_prop_total,
           dpi.pcr.entregue_d__all = dpi.pcr.entregue_d_total,
           dpi.pcr.entregue_n__all = dpi.pcr.entregue_n_total,
           dpi.pcr.entregue_prop__all = dpi.pcr.entregue_prop_total,
           dpi.pcr.tarv_d__all = dpi.pcr.tarv_d_total,
           dpi.pcr.tarv_n__all = dpi.pcr.tarv_n_total,
           dpi.pcr.tarv_prop__all = dpi.pcr.tarv_prop_total) %>% 
    
    pivot_longer('dpi.colheu.pcr_d__all':'mds.cv.supressao_prop_mds', 
                 names_to = c("indicator", "numdenom", "pop_type", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    filter(!numdenom == "prop",
           !pop_type == "total") %>% 
    mutate(age = recode(age,
                        "menor2" = "<2 Months",
                        "0.2" = "0-2",
                        "0.4" = "0-4",
                        "5.9" = "5-9",
                        "10.14" = "10-14",
                        "0.14" = "0-14",
                        "1.14" = "1-14",
                        "2.14" = "2-14"),
           numdenom = recode(numdenom,
                             "n" = "N",
                             "d" = "D"),
           pop_type = recode(pop_type,
                             "all" = "All",
                             "mg" = "MG",
                             "mds" = "MDS"),
           indicator = paste0(indicator,
                              if_else(numdenom %in% c("D"), "_D", "")),
           month ={month}) %>% 
    filter(Partner == ip) %>% 
    select(-c(Data))
  
}

# new function to use June '22
cv_tidy_new <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "Monitoria Intensiva",
                   skip = 9,
                   col_types = c("text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
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
                                 "numeric", "numeric", "numeric")) %>% 
    rename(dpi.colheu.pcr_d__all = dpi.colheu.pcr_d_total,
           dpi.colheu.pcr_n__all = dpi.colheu.pcr_n_total,
           dpi.pcr.enviado_d__all = dpi.pcr.enviado_d_total,
           dpi.pcr.enviado_n__all = dpi.pcr.enviado_n_total,
           dpi.pcr.entregue_d__all = dpi.pcr.entregue_d_total,
           dpi.pcr.entregue_n__all = dpi.pcr.entregue_n_total,
           dpi.pcr.tarv_d__all = dpi.pcr.tarv_d_total,
           dpi.pcr.tarv_n__all = dpi.pcr.tarv_n_total) %>% 
    pivot_longer('dpi.colheu.pcr_d__all':'mds.cv.supressao_n_mds', 
                 names_to = c("indicator", "numdenom", "pop_type", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    filter(!numdenom == "prop",
           !pop_type == "total") %>% 
    mutate(age = recode(age,
                        "menor2" = "<2 Months",
                        "0.1" = "<1",
                        "0.2" = "0-2",
                        "0.4" = "0-4",
                        "1.4" = "1-4",
                        "5.9" = "5-9",
                        "10.14" = "10-14",
                        "15.19" = "15-19",
                        "0.14" = "0-14",
                        "1.14" = "1-14",
                        "2.14" = "2-14"),
           numdenom = recode(numdenom,
                             "n" = "N",
                             "d" = "D"),
           pop_type = recode(pop_type,
                             "all" = "All",
                             "mg" = "MG",
                             "ml" = "ML",
                             "mds" = "MDS"),
           indicator = paste0(indicator,
                              if_else(numdenom %in% c("D"), "_D", "")),
           month ={month}) %>% 
    filter(Partner == ip) %>% 
    select(-c(Data))
  
}

# new function to use July '22 forward
cv_tidy_newest <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "Monitoria Intensiva",
                   skip = 9,
                   col_types = c("text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
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
                                 "numeric", "numeric")) %>% 
    rename(dpi.colheu.pcr_d__all = dpi.colheu.pcr_d_total,
           dpi.colheu.pcr_n__all = dpi.colheu.pcr_n_total,
           dpi.pcr.enviado_d__all = dpi.pcr.enviado_d_total,
           dpi.pcr.enviado_n__all = dpi.pcr.enviado_n_total,
           dpi.pcr.entregue_d__all = dpi.pcr.entregue_d_total,
           dpi.pcr.entregue_n__all = dpi.pcr.entregue_n_total,
           dpi.pcr.tarv_d__all = dpi.pcr.tarv_d_total,
           dpi.pcr.tarv_n__all = dpi.pcr.tarv_n_total) %>% 
    pivot_longer('dpi.colheu.pcr_d__all':'mds.cv.estaveis_n_mds', 
                 names_to = c("indicator", "numdenom", "pop_type", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    filter(!numdenom == "prop",
           !pop_type == "total") %>% 
    mutate(age = recode(age,
                        "menor2" = "<2 Months",
                        "0.1" = "<1",
                        "0.2" = "0-2",
                        "0.4" = "0-4",
                        "1.4" = "1-4",
                        "5.9" = "5-9",
                        "10.14" = "10-14",
                        "15.19" = "15-19",
                        "0.14" = "0-14",
                        "1.14" = "1-14",
                        "2.14" = "2-14"),
           numdenom = recode(numdenom,
                             "n" = "N",
                             "d" = "D"),
           pop_type = recode(pop_type,
                             "all" = "All",
                             "mg" = "MG",
                             "ml" = "ML",
                             "mds" = "MDS"),
           indicator = paste0(indicator,
                              if_else(numdenom %in% c("D"), "_D", "")),
           month ={month}) %>% 
    filter(Partner == ip) %>% 
    select(-c(Data))
  
}


# IMPORT & RESHAPE MQ SUBMISSIONS -----------------------------------------------------------

dod <- cv_tidy(DOD, "JHPIEGO-DoD")
echo <- cv_tidy_new(ECHO, "ECHO")
fgh <- cv_tidy_new(FGH, "FGH")
ariel <- cv_tidy_new(ARIEL, "ARIEL")
icap <- cv_tidy(ICAP, "ICAP")
ccs <- cv_tidy_new(CCS, "CCS")
egpaf <- cv_tidy(EGPAF, "EGPAF")

# COMPILE IP SUMBISSIONS --------------------------------------------------

cv_tidy <- bind_rows(dod, echo, fgh, ariel, icap, ccs, egpaf)

cv_tidy %>% # TABLE HF SUBMISSION LINES BY PARTNER.  CAUTION - BLANK SUBMISSION LINES ARE INCLUDED!
  group_by(Partner) %>% 
  distinct(DATIM_code) %>% 
  summarise(n())

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

cv_tidy_historic <- historic_import %>% 
  select(-c(No,
            Partner,
            Province,
            District,
            `Health Facility`)) %>%
  left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>%
  rename(datim_uid = DATIM_code,
         sisma_uid = sisma_id,
         sisma_nid = SISMA_code,
         period = month) %>%
  relocate(sisma_uid, .after = datim_uid) %>%
  relocate(period:partner, .after = sisma_nid) %>%
  pivot_wider(names_from =  indicator, values_from = value) %>%
  glimpse()

# check that all monthly data is coded to a partner
cv_tidy_historic %>% 
  pivot_longer(cols = dpi.colheu.pcr_D:mds.cv.estaveis, names_to = "indicator", values_to = "value") %>% 
  filter(period == month) %>% 
  group_by(partner) %>% 
  distinct(datim_uid) %>% 
  summarise(n())
  glimpse()


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------

# write to local
write_tsv(
  cv_tidy_historic,
  na = "",
  path_historic_output_file)

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

  
