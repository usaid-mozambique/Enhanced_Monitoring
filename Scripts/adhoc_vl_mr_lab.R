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

ariel <- "Data/Other/ARIEL_TX_PVLS_FY22Q1_Report Check.xlsx"
fgh <- "Data/Other/FGH_TX_PVLS_FY22Q1_Report Check_20012022_v1.xlsx"
icap <- "Data/Other/ICAP_TX_PVLS_FY22Q1_Report Check_20012022.xlsx"
ccs <- "Data/Other/CCS_TX_PVLS_FY22Q1_Report Check.xlsx"
egpaf <- "Data/Other/EGPAF_TX_PVLS_FY22Q1_Report Check (003).xlsx"
echo <- "Data/Other/ECHO_TX_PVLS_FY22Q1_Report Check.xlsx"


# CREATE FUNCTION FOR RESHAPING IP SUBMISSIONS ----------------------------

lab_reshape <- function(filename, ip){
  
  df_1 <- read_excel(filename, 
                     sheet = "TX_PVLS by Medical Records",
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
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
                                   "numeric", "numeric"), skip = 7) %>% 
    select(-c(No, SISMA_code, Type, Data, Column1)) %>% 
    pivot_longer(TX_PVLS_D_FM.Total:TX_PVLS_N_FM.T.Prison.all, 
                 names_to = "temp",
                 values_to = "value") %>% 
    filter(!str_detect(temp, "otal"),
           Partner == ip) %>% 
    separate(temp,
             c("indicator", "motive", "group", "age"),
             sep = "\\.") %>%
    separate(indicator,
             c("indicator"),
             sep = "_FM") %>%
    mutate(motive = recode(motive,
                           "R" = "Routine",
                           "T" = "Targeted"),
           age = recode(age,
                        "Less1" = "<1",
                        "50" = "50+",
                        "Unk" = "Unknown",
                        "all" = "All"),
           age = str_replace(age, "_", "-"),
           sex = case_when(group == "M" ~ "Male",
                           group == "F" ~ "Female"),
           population = case_when(group == "MG" ~ "PW",
                                  group == "Lac" ~ "LW"),
           keypop = case_when(group == "PWID" ~ "PWID",
                              group == "MSM" ~ "MSM",
                              group == "FSW" ~ "FSW",
                              group == "Prison" ~ "Prison"),
           standarddisag = case_when(
             (group %in% c("M", "F")) ~ "Age/Sex",
             (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
             TRUE ~ "KeyPop"),
           source = "Clinical Module"
    ) %>% 
    select(-c(group)) %>% 
    pivot_wider(names_from = indicator, values_from = value)
  
  
  
  df_2 <- read_excel(filename, 
                     sheet = "TX_PVLS by Laboratory Records",
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
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
                                   "numeric", "numeric"), skip = 7) %>% 
    select(-c(No, SISMA_code, Type, Data, Column1)) %>% 
    pivot_longer(TX_PVLS_D_Lab.Total:TX_PVLS_N_Lab.T.Prison.all, 
                 names_to = "temp",
                 values_to = "value") %>% 
    filter(!str_detect(temp, "otal"),
           Partner == ip) %>% 
    separate(temp,
             c("indicator", "motive", "group", "age"),
             sep = "\\.") %>%
    separate(indicator,
             c("indicator"),
             sep = "_Lab") %>%
    mutate(motive = recode(motive,
                           "R" = "Routine",
                           "T" = "Targeted"),
           age = recode(age,
                        "Less1" = "<1",
                        "50" = "50+",
                        "Unk" = "Unknown",
                        "all" = "All"),
           age = str_replace(age, "_", "-"),
           sex = case_when(group == "M" ~ "Male",
                           group == "F" ~ "Female"),
           population = case_when(group == "MG" ~ "PW",
                                  group == "Lac" ~ "LW"),
           keypop = case_when(group == "PWID" ~ "PWID",
                              group == "MSM" ~ "MSM",
                              group == "FSW" ~ "FSW",
                              group == "Prison" ~ "Prison"),
           standarddisag = case_when(
             (group %in% c("M", "F")) ~ "Age/Sex",
             (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
             TRUE ~ "KeyPop"),
           source = "Lab Module"
    ) %>% 
    select(-c(group)) %>% 
    pivot_wider(names_from = indicator, values_from = value)
  
  bind_rows(df_1, df_2)
  
}

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

df_ariel <- lab_reshape(ariel, "ARIEL")
df_fgh <- lab_reshape(fgh, "FGH")
df_icap <- lab_reshape(icap, "ICAP")
df_ccs <- lab_reshape(ccs, "CCS")
df_egpaf <- lab_reshape(egpaf, "EGPAF")
df_echo <- lab_reshape(echo, "ECHO")


df <- bind_rows(df_ariel, df_fgh, df_icap, df_ccs, df_egpaf, df_echo) %>%  
  left_join(ajuda_meta, by = c("DATIM_code" = "orgunituid")) %>% 
  select(-c(Partner, Province, District, `Health Facility`)) %>% 
  relocate(clinical_ip, snu, psnu, site, orgunituid = DATIM_code, sisma_id, latitude, longitude, standarddisag, source, motive, sex, age) %>% 
  glimpse()


# PRINT OUTPUT TO DISK ------------------------------------------------------

readr::write_tsv(
  df,
  "Dataout/test.txt",
  na ="")
