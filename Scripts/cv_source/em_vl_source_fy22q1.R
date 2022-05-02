
rm(list = ls())

#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)

# SET PATHS & VALUES -----------------------------------------------------------

period_value <- "2021-12-20"
quarterly_dataset <- ("Dataout/VL_source/_CompileHistoric/em_vl_source_fy21q1.txt") # UPDATE EACH QUARTER

ARIEL <- "Data/Other/ARIEL_TX_PVLS_FY22Q1_Report Check.xlsx"
FGH <- "Data/Other/FGH_TX_PVLS_FY22Q1_Report Check_20012022_v1.xlsx"
ICAP <- "Data/Other/ICAP_TX_PVLS_FY22Q1_Report Check_20012022.xlsx"
CCS <- "Data/Other/CCS_TX_PVLS_FY22Q1_Report Check.xlsx"
EGPAF <- "Data/Other/EGPAF_TX_PVLS_FY22Q1_Report Check (003).xlsx"
ECHO <- "Data/Other/ECHO_TX_PVLS_FY22Q1_Report Check.xlsx"


# EXTRACT FUNCTION RESHAPE ---------------------------------------------


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
           pop_subtype = case_when(group == "MG" ~ "PW",
                                  group == "Lac" ~ "LW"),
           keypop = case_when(group == "PWID" ~ "PWID",
                              group == "MSM" ~ "MSM",
                              group == "FSW" ~ "FSW",
                              group == "Prison" ~ "Prison"),
           pop_type = case_when(
             (group %in% c("M", "F")) ~ "Age/Sex",
             (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
             TRUE ~ "KeyPop"),
           source = "Clinical Module",
           period = period_value
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
           pop_subtype = case_when(group == "MG" ~ "PW",
                                  group == "Lac" ~ "LW"),
           keypop = case_when(group == "PWID" ~ "PWID",
                              group == "MSM" ~ "MSM",
                              group == "FSW" ~ "FSW",
                              group == "Prison" ~ "Prison"),
           pop_type = case_when(
             (group %in% c("M", "F")) ~ "Age/Sex",
             (group %in% c("Lac", "MG")) ~ "Pregnant/Breastfeeding",
             TRUE ~ "KeyPop"),
           source = "Lab Module",
           period = period_value
    ) %>% 
    select(-c(group)) %>% 
    pivot_wider(names_from = indicator, values_from = value)
  
  bind_rows(df_1, df_2)
  
}

# PROCESS IP SUBMISSIONS --------------------------------------------------


df_ariel <- lab_reshape(ARIEL, "ARIEL")
df_fgh <- lab_reshape(FGH, "FGH")
df_icap <- lab_reshape(ICAP, "ICAP")
df_ccs <- lab_reshape(CCS, "CCS")
df_egpaf <- lab_reshape(EGPAF, "EGPAF")
df_echo <- lab_reshape(ECHO, "ECHO")


# JOIN METADATA -----------------------------------------------------------


df <- bind_rows(df_ariel, df_fgh, df_icap, df_ccs, df_egpaf, df_echo)


# PRINT OUTPUT TO DISK ------------------------------------------------------

readr::write_tsv(
  df,
  {quarterly_dataset},
  na ="")















# LOAD METADATA -----------------------------------------------------------


ajuda_meta <- read_excel(ajuda_path) %>% 
  select(sisma_uid = sisma_id,
         datim_uid =  orgunituid,
         site_nid,
         partner = `IP FY20`,
         snu = SNU,
         psnu = Psnu,
         sitename = Sitename,
         his_epts = epts,
         his_emr = emr,
         his_idart = idart,
         his_disa = disa,
         support_ovc = ovc,
         support_ycm = ycm,
         ovc,
         ycm,
         latitude = Lat,
         longitude = Long)



left_join(ajuda_meta, by = c("DATIM_code" = "datim_uid")) %>% 
  select(-c(Partner, Province, District, `Health Facility`)) %>% 
  select(datim_uid = DATIM_code,
         sisma_uid,
         site_nid,
         period,
         partner,
         snu,
         psnu,
         sitename,
         motive,
         age,
         sex,
         pop_type,
         pop_subtype,
         keypop,
         source,
         TX_PVLS_D,
         TX_PVLS_N) %>% 
  glimpse()
