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

period <- "2021-12-20"

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx"

ariel <- "Data/MISAU/KP/FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2 (1).xlsx"
fgh <- "Data/MISAU/KP/FGH_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
icap <- "Data/MISAU/KP/ICAP_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2_20012022.xlsx"
ccs <- "Data/MISAU/KP/FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
egpaf <- "Data/MISAU/KP/EGPAF_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2 (003)_19 01 2022.xlsx"
echo <- "Data/MISAU/KP/ECHO_FY22_Q1_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"


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


# EXTRACT FUNCTION RESHAPE ---------------------------------------------


kp_reshape <- function(filename, ip){

  df <- read_excel(filename, sheet = "POP CHAVES - Trimestre", skip = 9) %>% 
    select(-c(No, SISMA_code, Type, Data, ...256)) %>% 
    pivot_longer(TX_New_KP_total:`TX_PVLS_Num_6meses_REC&MTS_25+`, 
                 names_to = "temp",
                 values_to = "value") %>% 
    filter(Partner == ip) %>% 
    mutate(indicator = case_when(str_detect(temp, "TX_CURR_6meses")  ~ "TX_CURR_6MO",
                                 str_detect(temp, "TX_CURR_meses")  ~ "TX_CURR_6MO",  # CORRECT IN REPORTING TEMPLATE?
                                 str_detect(temp, "TX_New_6meses")  ~ "TX_NEW_6MO",
                                 str_detect(temp, "TX_CURR_12meses")  ~ "TX_RET_N_12MO",
                                 str_detect(temp, "TX_New_12meses")  ~ "TX_RET_D_12MO",
                                 str_detect(temp, "TX_New")  ~ "TX_NEW",  # CORRECT IN REPORTING TEMPLATE?
                                 str_detect(temp, "TX_CURR")  ~ "TX_CURR",
                                 str_detect(temp, "TX_PVLS_Den_6meses")  ~ "TX_PVLS_D_6MO",
                                 str_detect(temp, "TX_PVLS_Den_meses")  ~ "TX_PVLS_D_6MO",   # CORRECT IN REPORTING TEMPLATE?
                                 str_detect(temp, "TX_PVLS_Num_6meses")  ~ "TX_PVLS_N_6MO",
                                 str_detect(temp, "TX_PVLS_Num")  ~ "TX_PVLS_N",
                                 str_detect(temp, "TX_PVLS_Den")  ~ "TX_PVLS_D"
                                 ),
    keypop = case_when(str_detect(temp, "otal")  ~ "All (KP & non-KP)",
                       str_detect(temp, "_PID_")  ~ "PID",
                       str_detect(temp, "_HSH_")  ~ "HSH",
                       str_detect(temp, "_MTS_")  ~ "MTS",
                       str_detect(temp, "_Rec_")  ~ "REC",  # CORRECT IN REPORTING TEMPLATE?
                       str_detect(temp, "_PID&HSH_")  ~ "PID/HSH",
                       str_detect(temp, "_PID&MST_")  ~ "PID/MTS",  # CORRECT IN REPORTING TEMPLATE?
                       str_detect(temp, "_PID&Rec_")  ~ "PID/REC", 
                       str_detect(temp, "_HSH&PID_")  ~ "HSH/PID",
                       str_detect(temp, "_HSH&Rec_")  ~ "HSH/REC",
                       str_detect(temp, "_MTS&PID_")  ~ "MTS/PID",
                       str_detect(temp, "_MTS&Rec_")  ~ "MTS/REC",
                       str_detect(temp, "_REC&PID_")  ~ "REC/PID",
                       str_detect(temp, "_REC&HSH_")  ~ "REC/HSH",
                       str_detect(temp, "_REC&MTS_")  ~ "REC/MTS",
                       str_detect(temp, "_MST")  ~ "MTS",  # CORRECT IN REPORTING TEMPLATE?
                       str_detect(temp, "_REC_")  ~ "REC"),
    age = case_when(str_detect(temp, "15_19")  ~ "15-19",
                    str_detect(temp, "20_24")  ~ "20-24",
                    str_detect(temp, "25+")  ~ "25+")
    ) %>% 
    pivot_wider(names_from = indicator, values_from = value)
  
}


# PROCESS IP SUBMISSIONS --------------------------------------------------


df_ariel <- kp_reshape(ariel, "ARIEL")
df_fgh <- kp_reshape(fgh, "FGH")
df_icap <- kp_reshape(icap, "ICAP")
df_ccs <- kp_reshape(ccs, "CCS")
df_egpaf <- kp_reshape(egpaf, "EGPAF")
df_echo <- kp_reshape(echo, "ECHO")


# JOIN METADATA -----------------------------------------------------------


df <- bind_rows(df_ariel, df_fgh, df_icap, df_ccs, df_egpaf, df_echo) %>%  
  left_join(ajuda_meta, by = c("DATIM_code" = "orgunituid")) %>% 
  mutate(date = {period},
         population = case_when(keypop == "All (KP & non-KP)" ~ "General",
                                TRUE ~ "Key Pop"),
         keypop = case_when(keypop == "All (KP & non-KP)" ~ "",
                            TRUE ~ keypop)) %>% 
  select(-c(Partner, Province, District, `Health Facility`, temp)) %>% 
  relocate(date, clinical_ip, snu, psnu, site, orgunituid = DATIM_code, sisma_id, latitude, longitude, population, everything()) %>% 
  glimpse()


# PRINT OUTPUT TO DISK ------------------------------------------------------


readr::write_tsv(
  df,
  "Dataout/misau_kp_fy22.txt",
  na ="")

