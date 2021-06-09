#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

#---- DEFINE MONTH AND LOAD DATASETS - NEEDS UPDATING EVERY MONTH! --------------------------

month <- "2021-05-20" # UPDATE
monthly_dataset <- ("Data/Ajuda/ER_DSD_TPT/_CompileHistoric/TPT_2021_05.csv") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK

ARIEL <- "Data/Ajuda/ER_DSD_TPT/2021_05/Ariel_May_2021 (Retention Template).xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT/2021_05/CCS_May_2021 (Retention Template).xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT/2021_05/PartnerName_Jun_2021 (Retention Template)_ECHO_V2.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT/2021_05/EGPAF_May_2021 (Retention Template).xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT/2021_05/ICAP_Maio_2021 (Retention Template)_08JUN2021.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT/2021_05/FGH_Jun_2021 (Retention Template).xlsx"

#---- DEFINE PATHS AND OUTPUT NAMES - DOES NOT NEED UPDATING --------------------------------


ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/AJUDA Site Map.xlsx") %>%
  select(-c(sisma_id,
            SNU,
            Psnu,
            Sitename,
            `IP FY20`,
            ajuda,
            ajuda_phase)) %>%
  dplyr::mutate(conflict = replace_na(conflict, 0),
                corridor = replace_na(corridor, 0))

historic_files_path <- "Data/Ajuda/ER_DSD_TPT/_CompileHistoric/"  # PATH USED TO CREATE A LIST OF ALL .CSV FILES PREVIOUSLY CREATED
data_path <- "Data/Ajuda/ER_DSD_TPT/_CompileHistoric/"  # PATH USED IN SPECIFIC CODE TO COMPILE THE ABOVE LIST OF .CSV FILES

historic_dataset <- ("Dataout/em_tpt_interagency.txt")  # PATH AND NAME OF COMPILED INTER-AGENCY DATASET THAT IS SHARED WITH CDC EVERY MONTH
historic_dataset_usaid <- ("Dataout/em_tpt.txt")  # PATH AND NAME OF COMPILED USAID DATASET FOR USE IN TABLEAU DASHBOARD.  THE ONLY DIFFERENCE BETWEEN THIS AND ABOVE INTERAGENCY IS THE JOIN OF AJUDA SITE MAP

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

tpt <- dplyr::bind_rows(ariel, ccs, echo, egpaf, fgh, icap)

rm(ariel, ccs, echo, egpaf, fgh, icap)

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

#---- ROW BIND ALL IP SUBMISSION AND GENERATE INTER-AGENCY OUTPUT -----------------------

tpt_tidy_history <- historic_files %>%
  map(~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

#---- JOIN AJUDA SITE MAP AND GENERATE USAID OUTPUT -----------------------

tpt_tidy_history_usaid <- tpt_tidy_history %>%
  dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  dplyr::rename(orgunituid = DATIM_code,
                Site = `Health Facility`)

#---- WRITE TPT CSV TO DISK -----------------------

readr::write_tsv(
  tpt_tidy_history,
  {historic_dataset})

readr::write_tsv(
  tpt_tidy_history_usaid,
  {historic_dataset_usaid})
