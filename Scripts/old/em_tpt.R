#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

rm(list = ls())

#---- DEFINE MONTH AND LOAD DATASETS - NEEDS UPDATING EVERY MONTH! --------------------------

month <- "2021-09-20" # UPDATE
monthly_dataset <- ("Data/Ajuda/ER_DSD_TPT/_CompileHistoric/TPT_2021_09.csv") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK

DOD <- "Data/Ajuda/ER_DSD_TPT/2021_09/DOD_Sept_2021final 07102021DOD Jhpiego.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT/2021_09/ARIEL_Sep_2021 (Retention Template).xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT/2021_09/CCS_Sep_2021 (Retention Template).xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT/2021_09/ECHO_PartnerName_Sept_2021 (Retention Template).xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT/2021_09/EGPAF_Sept_2021 (Retention Template)_11 10 2021.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT/2021_09/ICAP_Setembro_2021 (Retention Template)_10102021.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT/2021_09/FGH_Sep_2021 (Retention Template).xlsx"

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

historic_files_path <- "Data/Ajuda/ER_DSD_TPT/_CompileHistoric/"  # PATH USED TO CREATE A LIST AND COMPILE ALL .CSV FILES PREVIOUSLY CREATED

historic_dataset <- ("Dataout/em_tpt.txt")  # PATH AND NAME OF COMPILED INTER-AGENCY DATASET THAT IS SHARED WITH CDC EVERY MONTH

#---- IMPORT AND MERGE DOD DATA -------------------------------------------------------
# 

dod <- read_excel({DOD}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))

#---- IMPORT AND MERGE ECHO DATA -------------------------------------------------------

echo <- read_excel({ECHO}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))
 
#---- IMPORT AND MERGE ARIEL DATA -------------------------------------------------------

ariel <- read_excel({ARIEL}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))
 
#---- IMPORT AND MERGE CCS DATA -------------------------------------------------------

ccs <- read_excel({CCS}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))

#---- IMPORT AND MERGE EGPAF DATA -------------------------------------------------------

egpaf <- read_excel({EGPAF}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))

#---- IMPORT AND MERGE FGH DATA -------------------------------------------------------

fgh <- read_excel({FGH}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))

#---- IMPORT AND MERGE ICAP DATA -------------------------------------------------------

icap <- read_excel({ICAP}, sheet = "TB", skip = 7) %>%
  dplyr::select(c(No,
                  Partner,
                  Province,
                  District,
                  `Health Facility`,
                  DATIM_code,
                  SISMA_code,
                  Type,
                  Period,
                  TX_CURR,
                  TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init)) %>%
  filter(across(c(TX_CURR_TPT_Com,
                  TX_CURR_TPT_Not_Comp,
                  TX_CURR_TB_tto,
                  TX_CURR_TPT_Not_Comp_POS_Screen,
                  TX_CURR_Eleg_TPT_Comp,
                  TX_CURR_W_TPT_last7Mo,
                  TX_CURR_Eleg_TPT_Init), ~ !is.na(.x)))

#---- COMPILE IP SUMBISSIONS --------------------------------------------

tpt <- dplyr::bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)

rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

#---- CALCULATE NEW VARIABLES, PIVOT AND RENAME VARIABLES -----------------------

tpt_tidy <- tpt %>%
  dplyr::mutate(TPT_candidates = TX_CURR - (TX_CURR_TPT_Com + TX_CURR_W_TPT_last7Mo) - (TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen),
                TPT_ineligible = TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen) %>%
  tidyr::pivot_longer(TX_CURR:TPT_ineligible, names_to = "attribute", values_to = "value") %>%
  dplyr::mutate(indicator = attribute) %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR_W_TPT_last7Mo"= "Actively on TPT",
                                          "TX_CURR_TB_tto" = "Recent Active TB TX",
                                          "TX_CURR_TPT_Not_Comp_POS_Screen" = "Recent Pos TB Screen",
                                          "TX_CURR_TPT_Com" = "TPT Completed",
                                          "TPT_candidates" = "TPT Candidates",
                                          "TPT_ineligible" = "TPT Ineligible",
                                          "TX_CURR_TPT_Not_Comp" = "TPT Not Comp"),
                Period = {month}
  ) %>%
  dplyr::filter(!indicator %in% c("TX_CURR_Eleg_TPT_Init", "TX_CURR_Eleg_TPT_Comp")) %>%
  dplyr::select(-c(No))

#---- WRITE MONTHLY ALL IP TPT CSV TO DISK -----------------------

readr::write_csv(
  tpt_tidy,
  {monthly_dataset})

#---- DEFINE PATH AND SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.csv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------

tpt_tidy_history <- historic_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>%
  dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  dplyr::select(-c(Province, District, `Health Facility`)) %>% 
  dplyr::rename(orgunituid = DATIM_code,
                Province = SNU,
                District = Psnu,
                Site = Sitename) %>% 
  dplyr::relocate(Province:Site, .after = Partner)



#---- WRITE TPT CSV TO DISK -----------------------

readr::write_tsv(
  tpt_tidy_history,
  {historic_dataset})




#---- TROUBLESHOOTING -----------------------

m3_9 <- read_csv("Data/Ajuda/ER_DSD_TPT/_CompileHistoric/TPT_2021_03_09.csv")

#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------

tpt_tidy_history <- m3_9 %>%
  dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  dplyr::select(-c(Province, District, `Health Facility`)) %>% 
  dplyr::rename(orgunituid = DATIM_code,
                Province = SNU,
                District = Psnu,
                Site = Sitename) %>% 
  dplyr::relocate(Province:Site, .after = Partner)

readr::write_tsv(
  tpt_tidy_history,
  "Dataout/em_tpt_alt.txt")



tpt_tidy_history <- rbind(m3, m4, m5, m6, m7, m8, m9)

