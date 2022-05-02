#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)

rm(list = ls())

# SET PATHS & VALUES -----------------------------------------------------------

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx"

historical <- "Dataout/KP_MISAU/old/kp_misau.txt"



# LOAD DATASETS -----------------------------------------------------------



kp_misau <- read_delim("Dataout/KP_MISAU/old/kp_misau.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

kp_clean <- kp_misau %>% 
  rename(period = Date,
         keypop = KeyPop,
         orgunituid = DATIM_code) %>% 
  mutate(pop_type = case_when(keypop == "Total" ~ "General",
                                TRUE ~ "Key Pop"),
         keypop = case_when(keypop == "Total" ~ "",
                            TRUE ~ keypop),
         age = "",
         TX_NEW_6MO = NA,
         TX_CURR_6MO = NA,
         TX_PVLS_D_6MO = NA,
         TX_PVLS_N_6MO = NA) %>% 
  select(c(Province,
           District,
           `Health Facility` = `DATIM_HF Name`,
           DATIM_code = orgunituid, 
           pop_type, 
           keypop, 
           keypop,
           age,
           period,
           TX_NEW, 
           TX_CURR, 
           TX_PVLS_D, 
           TX_PVLS_N, 
           TX_RET_D_12MO = TX_RET_D, 
           TX_RET_N_12MO = TX_RET_N,
           TX_NEW_6MO,
           TX_CURR_6MO,
           TX_PVLS_D_6MO,
           TX_PVLS_N_6MO))


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  kp_clean,
  "Dataout/KP_MISAU/_CompileHistoric/old/misau_kp_fy20fy21.txt")  
