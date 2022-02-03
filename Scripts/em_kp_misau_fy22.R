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

month <- "2022-12-20"
period <- "FY22Q1"

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx"

ariel_path <- "Data/MISAU/KP/FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2 (1).xlsx"
fgh_path <- "Data/MISAU/KP/FGH_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
icap_path <- "Data/MISAU/KP/ICAP_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2_20012022.xlsx"
ccs_path <- "Data/MISAU/KP/FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
egpaf_path <- "Data/MISAU/KP/EGPAF_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2 (003)_19 01 2022.xlsx"
echo_path <- "Data/MISAU/KP/ECHO_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"

# LOAD DATASETS -----------------------------------------------------------

ajuda_meta <- read_excel(ajuda_path) %>% 
  select(
    orgunituid,
    sisma_id,
    latitude = Lat,
    longitude = Long,
    clinical_ip = `IP FY20`,
    his_emr = emr,
    his_epts = epts,
    his_idart = idart,
    his_disa = disa,
    ovc_support = ovc,
    ycm_support = ycm
  )

# CREATE FUNCTION RESHAPE ---------------------------------------------

kp_reshape <- function(filename, ip){
  df <- read_excel(ariel_path, sheet = "POP CHAVES - Trimestre", skip = 9) %>% 
    filter(Partner == ip) %>% 
    pivot_longer(TX_New_KP_total:`TX_PVLS_Num_6meses_REC&MTS_25+`, 
                 names_to = "temp",
                 values_to = "value")
}

df <- kp_reshape(ariel_path, "ARIEL")




df <- read_excel(ariel_path, sheet = "POP CHAVES - Trimestre", skip = 9) %>% 
  filter(Partner == "ARIEL") %>%  # UPDATE WHEN COPIED TO FUNCTION
  select(-c(No, SISMA_code, Type, Data, ...256)) %>% 
  pivot_longer(TX_New_KP_total:`TX_PVLS_Num_6meses_REC&MTS_25+`, 
               names_to = "temp",
               values_to = "value") %>% 
  mutate(indicator = case_when(str_detect(temp, "TX_CURR_6meses")  ~ "TX_CURR_6MO",
                               str_detect(temp, "TX_New_6meses")  ~ "TX_NEW_6MO",
                               str_detect(temp, "TX_CURR_12meses")  ~ "TX_CURR_12MO",
                               str_detect(temp, "TX_New_12meses")  ~ "TX_NEW_12MO",
                               str_detect(temp, "TX_New")  ~ "TX_NEW",
                               str_detect(temp, "TX_CURR")  ~ "TX_CURR",
                               str_detect(temp, "TX_PVLS")  ~ "TX_PVLS")
  ) %>% 
  view()




  


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


