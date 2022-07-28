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
file <- "PREP_2022_06"

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
path_monthly_output_repo <- "Dataout/PrEP/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1BYq-xdMhxw8sOUHYwFiZH8_w2UsQmBun") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_prep.txt" # folder path where monthly dataset archived
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

prep_reshape <- function(filename, ip){
  
  df <- read_excel(filename, # Function argument
                   sheet = "Resumo Mensal de PrEP", 
                   col_types = c("text", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "text", "text", "numeric", 
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
                                 "numeric", "numeric", "numeric"), 
                   skip = 7) %>% 
    filter(Partner == ip) %>%  # Function argument
    select(!c(Elegible.to.PrEP_All_Total_Total,
              Elegible.to.PrEP_Casos.Especiais_Male_Total,
              Elegible.to.PrEP_Casos.Especiais_Female_Total,
              Elegible.to.PrEP_All_PW_Total,
              Elegible.to.PrEP_All_LW_Total,
              PrEP.NEW_All_Total_Total,
              PrEP.NEW_Casos.Especiais_Male_Total,
              PrEP.NEW_Casos.Especiais_Female_Total,
              PrEP.NEW_All_PW_Total,
              PrEP.NEW_All_LW_Total,
              PrEP.New.Who.RTT_All_Total_Total,
              PrEP.New.Who.RTT_Casos.Especiais_Male_Total,
              PrEP.New.Who.RTT_Casos.Especiais_Female_Total,
              PrEP.New.Who.RTT_All_PW_Total,
              PrEP.New.Who.RTT_All_LW_Total,
              PrEP.CT_All_Total_Total,
              PrEP.CT_Casos.Especiais_Male_Total,
              PrEP.CT_Casos.Especiais_Female_Total,
              PrEP.CT_All_PW_Total,
              PrEP.CT_All_LW_Total,
              PrEP.CT.3months_All_Total_Total,
              PrEP.CT.3months_Casos.Especiais_Male_Total,
              PrEP.CT.3months_Casos.Especiais_Female_Total,
              PrEP.CT.3months_All_PW_Total,
              PrEP.CT.3months_All_LW_Total)) %>%
    pivot_longer('Elegible.to.PrEP_Casos.Especiais_Male_10.14':'PrEP.CT.3months_TP_People.who.Injected.Drugs_Total', 
                 names_to = c("indicator", "pop_type", "disaggregate", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    mutate(period = as.Date(month, "%d/%m/%Y"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           indicator = str_replace_all(indicator, "Elegible_to_PrEP", "PrEP_Eligible"),
           indicator = str_replace_all(indicator, "PrEP_New_Who_RTT", "PrEP_NEW_RTT"),
           age = str_replace_all(age, "\\.", "-"),
           age = str_replace_all(age, "Total", "Unknown"),
           sex = case_when(
             disaggregate == "Female" ~ "Female",
             disaggregate == "Male" ~ "Male",
             TRUE ~ as.character("Unknown")),
           pop_type = recode(pop_type, 
                             "Casos.Especiais" = "Special Cases",
                             "TP.AtRisk" = "TP at Risk",
                             "All" = "PLW"),
           disaggregate = recode(disaggregate,
                        "Long.Distance.driver" = "Long Distance Drivers",
                        "military" = "Military",
                        "miner" = "Miners",
                        "People.who.Injected.Drugs" = "PWID",
                        "Sero.Discordante.Couples" = "Sero-Discordante Couples",
                        "Sex.workers" = "Sex Workers",
                        "TG" = "Transgender")) %>% 
    pivot_wider(names_from =  indicator, values_from = value)
  
}

# IMPORT & RESHAPE TPT SUBMISSIONS -------------------------------------------------


dod <- prep_reshape(DOD, "JHPIEGO-DoD")
echo <- prep_reshape(ECHO, "ECHO")
ariel <- prep_reshape(ARIEL, "ARIEL")
ccs <- prep_reshape(CCS, "CCS")
egpaf <- prep_reshape(EGPAF, "EGPAF")
fgh <- prep_reshape(FGH, "FGH")
icap <- prep_reshape(ICAP, "ICAP")

glimpse(ccs)

# COMPILE IP SUMBISSIONS --------------------------------------------------


prep <- bind_rows(dod, echo, ariel, ccs, egpaf, fgh)
rm(dod, echo, ariel, ccs, egpaf, fgh)

# detect lines not coded with datim_uids
prep %>% 
  filter(is.na(`Datim Code`)) %>% 
  distinct(`Datim Code`, Province, District, `Health Facility`)


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------

# write to local
readr::write_tsv(
  prep,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


#---- SURVEY AND COMBINE ALL MONTHLY TPT DATASETS TO BUILD HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

historic_import <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind)


#---- JOIN METADATA ---------------------------------

prep_tidy_historic <- historic_import %>% 
  select(-c(No,
            Partner,
            Province,
            District,
            `Health Facility`,
            `SISMA Code`,
            Relatorio_period,
            Relatorio_Date)) %>%
  mutate(across(starts_with('PrEP_'), ~ replace_na(., 0))) %>% 
  left_join(ajuda_site_map, by = c("Datim Code" = "datim_uid")) %>%
  rename(datim_uid = `Datim Code`) %>%
  glimpse()


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


prep_tidy_historic_2 <- prep_tidy_historic %>%
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner,
         snu,
         psnu,
         sitename,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         pop_type,
         disaggregate,
         sex,
         age,
         starts_with("PrEP_")) %>% 
  glimpse()


# WRITE FINAL OUTPUT TO DISK ----------------------------------------------

# write to local
readr::write_tsv(
  prep_tidy_historic_2,
  "Dataout/em_prep.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

