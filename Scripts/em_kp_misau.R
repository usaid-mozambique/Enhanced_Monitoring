#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)

rm(list = ls())

# LOAD DATASETS -----------------------------------------------------------

month <- "2021-09-01"
period <- "FY21Q4"

ajuda_meta_data <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/ajuda_site_map_fy22q1.xlsx") %>%
  select(sisma_id:`IP FY20`,
         clinical_ip = `IP FY20`) %>%
  select_all(str_to_lower) %>% 
  glimpse()

ECHO <- read_excel("Data/MISAU/KP/ECHO_KeyPop_MISAU_DataRequest_FY21Qtr4_V4.1.xlsx", 
                   sheet = "FY21Qtr4 Data Entry Form", skip = 7)

EGPAF <- read_excel("Data/MISAU/KP/EGPAF_KeyPop_MISAU_DataRequest_FY21Qtr4_V4.1.xlsx", 
                    sheet = "FY21Qtr4 Data Entry Form", skip = 7)

FGH <- read_excel("Data/MISAU/KP/FGH_KeyPop_MISAU_DataRequest_FY21Qtr4_V4.1.xlsx", 
                    sheet = "FY21Qtr4 Data Entry Form", skip = 7)

ARIEL <- read_excel("Data/MISAU/KP/ARIEL KeyPop_MISAU_DataRequest_FY21Qtr4_V4.1.xlsx", 
                  sheet = "FY21Qtr4 Data Entry Form", skip = 7)

CCS <- read_excel("Data/MISAU/KP/CCS_KeyPop_MISAU_DataRequest_FY21Qtr4_V4.1.xlsx", 
                    sheet = "FY21Qtr4 Data Entry Form", skip = 7)

ICAP <- read_excel("Data/MISAU/KP/ICAP_KeyPop_MISAU_DataRequest_FY21Qtr4_V4.1.xlsx", 
                  sheet = "FY21Qtr4 Data Entry Form", skip = 7)

# COMPILE & MUNGE KP DATASET ----------------------------------------------

ip_compile <- bind_rows(ECHO, EGPAF, FGH, ARIEL, CCS, ICAP) %>% 
  filter(across(c(TX_NEW_Total:`TX_RET_N_REC_25+`), ~ !is.na(.x))) %>%
  select(-c("MoH_ID", "Health Facility_EPTS", "Reporting Period_EPTS")) %>% 
  pivot_longer(cols = TX_NEW_Total:`TX_RET_N_REC_25+`,
               names_to = "temp",
               values_to = "value") %>% 
  mutate(indicator = case_when(str_detect(temp, "TX_NEW")  ~ "TX_NEW",
                               str_detect(temp, "TX_CURR")  ~ "TX_CURR",
                               str_detect(temp, "TX_PVLS_D")  ~ "TX_PVLS_D",
                               str_detect(temp, "TX_PVLS_N")  ~ "TX_PVLS_N",
                               str_detect(temp, "TX_RET_D")  ~ "TX_RET_D",
                               str_detect(temp, "TX_RET_N")  ~ "TX_RET_N"),
         population = case_when(str_detect(temp, "Total")  ~ "Total",
                               str_detect(temp, "PID")  ~ "PID",
                               str_detect(temp, "HSH")  ~ "HSH",
                               str_detect(temp, "MTS")  ~ "MTS",
                               str_detect(temp, "REC")  ~ "REC"),
         age = case_when(str_detect(temp, "15-19") ~ "15-19",
                         str_detect(temp, "20-24") ~ "20-24",
                         str_detect(temp, "25+") ~ "25+"),
         period = month) %>% 
  select(-c(temp, Province, District, `DATIM_HF Name`, `Implementing Partner`)) %>%
  left_join(ajuda_meta_data, by = c("DATIM_code" = "orgunituid")) %>%
  select(-c(DATIM_code)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  glimpse()

# COMPILE & MUNGE KP DATASET ----------------------------------------------

filename <- "em_kp_misau.txt"
filename <- tag(filename)
filename <- tag(filename, period)
filename <- set_fpath(filename, "Dataout/KP_Quarterly")

readr::write_tsv(
  ip_compile,
  filename,
  na ="")


