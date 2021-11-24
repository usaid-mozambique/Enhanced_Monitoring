#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

rm(list = ls())


# DEFINE MONTH AND LOAD DATASETS - NEEDS UPDATING EVERY MONTH! ------------

month <- "2021-10-20" # UPDATE
monthly_dataset <- ("Data/Ajuda/ER_DSD_TPT_VL/TPT/_CompileHistoric/TPT_2021_10t.csv") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/DOD__Oct_2021final 23102021 DOD Jhpiego Included Monitoria Intensiva de CV tab (1).xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Fundação ARIEL Oct_21 (Retention Template)_FY22.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Oct_21 (Retention Template)_FY22 (1).xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Oct_21 (Retention Template)_FY22_ECHO.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/EGPAF_Oct_21 (Retention Template)_FY22 (003)_11 11 2021.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/ICAP_Oct_21 (Retention Template)_FY22_05112021.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/FGH_Oct_21 (Retention Template)_FY22_Updated_CS_Namagola_lugea_and CS_Palane.xlsx"

ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/ajuda_site_map_fy22q1.xlsx") %>%
  select(-c(sisma_id,
            `IP FY20`,
            ajuda,
            ajuda_phase,
            epts_date,
            idart_date)) %>%
  dplyr::mutate(conflict = replace_na(conflict, 0),
                corridor = replace_na(corridor, 0))

# DEFINE PATHS AND OUTPUT NAMES - DOES NOT NEED UPDATING ------------------

historic_files_path <- "Data/Ajuda/ER_DSD_TPT_VL/TPT/_CompileHistoric/"  # PATH USED TO CREATE A LIST AND COMPILE ALL .CSV FILES PREVIOUSLY CREATED

historic_dataset <- ("Dataout/em_tpt.txt")  # PATH AND NAME OF COMPILED INTER-AGENCY DATASET THAT IS SHARED WITH CDC EVERY MONTH

# CREATE FUNCTION TPT RESHAPE ---------------------------------------------

tpt_reshape <- function(filename, ip){
  
  df <- read_excel(filename, sheet = "TB", skip = 7) %>%
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

tpt <- dplyr::bind_rows(ariel, ccs, echo, egpaf, fgh, icap)

rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# CALCULATE NEW VARIABLES, PIVOT AND RENAME VARIABLES ---------------------

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

# WRITE MONTHLY TPT CSV TO DISK ------------------------------------

readr::write_csv(
  tpt_tidy,
  {monthly_dataset})

# SURVEY ALL MONTHLY TPT DATASETS AND REDUCE --------------------------

# historic_files <- dir({historic_files_path}, pattern = "*.csv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE
# 
# tpt_tidy_history <- historic_files %>%
#   map(~ read_csv(file.path(historic_files_path, .),
#                  col_types = cols(Period = col_date(format = "%Y-%m-%d")))) %>%
#   reduce(rbind) %>%
#   dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
#   dplyr::select(-c(Province, District, `Health Facility`)) %>% 
#   dplyr::rename(orgunituid = DATIM_code,
#                 Province = SNU,
#                 District = Psnu,
#                 Site = Sitename) %>% 
#   dplyr::relocate(Province:Site, .after = Partner)

# # WRITE TPT CSV TO DISK ---------------------------------------------------
# 
# readr::write_tsv(
#   tpt_tidy_history,
#   {historic_dataset})


# TEMPORARY WORKAROUND ----------------------------------------------------

temp <- read_csv("Data/Ajuda/ER_DSD_TPT/_CompileHistoric/manual_compile/TPT_2021_03_10.csv")

#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------

tpt_tidy_history <- temp %>%
  dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  dplyr::select(-c(Province, District, `Health Facility`)) %>% 
  dplyr::rename(orgunituid = DATIM_code,
                Province = SNU,
                District = Psnu,
                Site = Sitename) %>% 
  dplyr::relocate(Province:Site, .after = Partner)

readr::write_tsv(
  tpt_tidy_history,
  "Dataout/em_tpt.txt")


