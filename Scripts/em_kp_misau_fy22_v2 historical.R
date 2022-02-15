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

historical <- "Dataout/KP_Quarterly/kp_misau.txt"



# LOAD DATASETS -----------------------------------------------------------


ajuda_meta <- read_excel(ajuda_path) %>% 
  select(
    snu = SNU,
    psnu = Psnu,
    site = Sitename,
    orgunituid,
    sisma_id,
    latitude = Lat,
    longitude = Long,
    clinical_ip = `IP FY20`
  )


kp_misau <- read_delim("Dataout/KP_Quarterly/kp_misau.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

kp_clean <- kp_misau %>% 
  rename(period = Date,
         keypop = KeyPop,
         orgunituid = DATIM_code) %>% 
  mutate(population = case_when(keypop == "Total" ~ "General",
                                TRUE ~ "Key Pop"),
         keypop = case_when(keypop == "Total" ~ "",
                            TRUE ~ keypop)) %>% 
  select(c(orgunituid, period, population, keypop, TX_NEW, TX_CURR, TX_PVLS_D, TX_PVLS_N, TX_RET_D, TX_RET_N)) %>% 
  left_join(ajuda_meta) 
  
