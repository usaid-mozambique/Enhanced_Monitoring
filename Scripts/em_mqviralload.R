#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

# rm(list = ls())

#---- DEFINE MONTH AND LOAD DATASETS - NEEDS UPDATING EVERY MONTH! --------------------------

month <- "2021-10-20" # UPDATE
monthly_dataset <- ("Data/Ajuda/ER_DSD_TPT_VL/_CompileHistoric/mqvl_2021_10.csv") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/DOD__Oct_2021final 23102021 DOD Jhpiego Included Monitoria Intensiva de CV tab (1).xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Fundação ARIEL Oct_21 (Retention Template)_FY22.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/CCS_Oct_21 (Retention Template)_FY22.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Oct_21 (Retention Template)_FY22_ECHO.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/EGPAF_Oct_21 (Retention Template)_FY22 (003)_11 11 2021.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/ICAP_Oct_21 (Retention Template)_FY22_05112021.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/FGH_Oct_21 (Retention Template)_FY22.xlsx"

#---- DEFINE PATHS AND OUTPUT NAMES - DOES NOT NEED UPDATING --------------------------------


ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/ajuda_site_map_fy21q4.xlsx") %>%
  select(-c(sisma_id,
            `IP FY20`,
            ajuda,
            ajuda_phase,
            epts_date,
            idart_date)) %>%
  dplyr::mutate(conflict = replace_na(conflict, 0),
                corridor = replace_na(corridor, 0))

historic_files_path <- "Data/Ajuda/ER_DSD_TPT_VL/_CompileHistoric/"  # PATH USED TO CREATE A LIST OF ALL .CSV FILES PREVIOUSLY CREATED

historic_dataset <- ("Dataout/em_mqvl.txt")  # PATH AND NAME OF COMPILED USAID DATASET FOR USE IN TABLEAU DASHBOARD.  THE ONLY DIFFERENCE BETWEEN THIS AND ABOVE INTERAGENCY IS THE JOIN OF AJUDA SITE MAP

#---- IMPORT AND MERGE DOD DATA -------------------------------------------------------

dod <- read_excel({DOD}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- IMPORT AND MERGE ECHO DATA -------------------------------------------------------

echo <- read_excel({ECHO}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- IMPORT AND MERGE ARIEL DATA -------------------------------------------------------

ariel <- read_excel({ARIEL}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- IMPORT AND MERGE CCS DATA -------------------------------------------------------

ccs <- read_excel({CCS}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- IMPORT AND MERGE EGPAF DATA -------------------------------------------------------

egpaf <- read_excel({EGPAF}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- IMPORT AND MERGE FGH DATA -------------------------------------------------------

fgh <- read_excel({FGH}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- IMPORT AND MERGE ICAP DATA -------------------------------------------------------

icap <- read_excel({ICAP}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  dplyr::filter(`13_consulta_1a_d_total`>0)

#---- COMPILE IP SUMBISSIONS --------------------------------------------

mqvl <- dplyr::bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)

rm(dod,ariel, ccs, echo, egpaf, fgh, icap)

#---- CALCULATE NEW VARIABLES, PIVOT AND RENAME VARIABLES -----------------------

mqvl_tdy <- mqvl%>%
 tidyr::pivot_longer ('13_consulta_1a_d_total':'pepfar_altocv12meses_2a_n_mg',names_to = "attribute", values_to = "value") %>%
 dplyr::mutate(indicator = attribute) 
 # dplyr::mutate(indicator = dplyr::recode(indicator,
 

#---- WRITE MONTHLY ALL IP TPT CSV TO DISK -----------------------

readr::write_csv(
  mqvl_tdy,
  {monthly_dataset})

#---- DEFINE PATH AND SURVEY ALL MONTHLY MQVL DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.csv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

#---- ROW BIND ALL IP SUBMISSION AND GENERATE INTER-AGENCY OUTPUT (THIS OUTPUT IS SHARED WITH CDC) -----------------------

mqvl_tidy_history <- historic_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind)
  dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  dplyr::rename(orgunituid = DATIM_code,
                Site = `Health Facility`)

#---- WRITE TPT CSV TO DISK -----------------------

readr::write_tsv(
  mqvl_tidy_history,
  {historic_dataset})
