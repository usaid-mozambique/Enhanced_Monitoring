
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


period_value <- "2022-03-20"
quarterly_dataset <- ("Dataout/VL_source/_CompileHistoric/em_vl_source_fy22q2.txt") # UPDATE EACH QUARTER

ARIEL <- "Data/Other/ARIEL TX_PVLS_FY22Q2_Report Check_21.04.2022.xlsx"
FGH <- "Data/Other/FGH TX_PVLS_FY22Q2_Report Check_22042022.xlsx"
ICAP <- "Data/Other/ICAP_2022_TX_PVLS_FY22Q2_Report Check.xlsx"
CCS <- "Data/Other/CCS TX_PVLS_FY22Q2_Report Check.xlsx"
EGPAF <- "Data/Other/EGPAF TX_PVLS_FY22Q2_Report Check 2022.xlsx"
ECHO <- "Data/Other/ECHO_TX_PVLS_FY22Q2_Report Check_V2.xlsx"
DOD <- "Data/Other/DOD TX_PVLS_FY22Q2_Report Check.xlsx"

historic_files_path <- "Dataout/VL_source/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH


# LOAD METADATA -----------------------------------------------------------


ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx") %>%
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


# EXTRACT FUNCTION RESHAPE ---------------------------------------------


lab_reshape <- function(filename, ip){
  
  df_1 <- read_excel(filename, 
                     sheet = "TX_PVLS by Medical Records",
                     col_types = c("numeric", "text", "text", 
                                   "text", "text", "text", "text", "text", 
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
                                   "numeric", "numeric", "numeric", 
                                   "numeric"), 
                     
                     skip = 7) %>%  
    select(-c(No, SISMA_code)) %>% 
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
                                   "text", "text", "text", "text", "text", 
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
                                   "numeric", "numeric", "numeric", 
                                   "numeric"), 
                     skip = 7) %>%   
    select(-c(No, SISMA_code)) %>% 
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
df_dod <- lab_reshape(DOD, "JHPIEGO-DoD")

# JOIN METADATA -----------------------------------------------------------


df <- bind_rows(df_ariel, df_fgh, df_icap, df_ccs, df_egpaf, df_echo, df_dod) %>% # UPDATE WITH ECHO!!!
  select(!c(`Reporting period`, `reporting period`, Column1))


# PRINT OUTPUT TO DISK ------------------------------------------------------


readr::write_tsv(
  df,
  {quarterly_dataset},
  na ="")


#---- SURVEY ALL MONTHLY TXTB DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.txt")

vl_source_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind)


#---- JOIN AJUDA SITEMAP AND CLEAN DATAFRAME -----------------------

vl_source_tidy_history_1 <- vl_source_tidy_history %>%
  left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>% 
  select(datim_uid = DATIM_code,
         sisma_uid,
         site_nid,
         period,
         partner,
         snu,
         psnu,
         sitename,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         sex,
         age,
         pop_type,
         pop_subtype,
         keypop,
         motive,
         source,
         starts_with("TX_")) %>% 
  glimpse()


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  vl_source_tidy_history_1,
  "Dataout/em_vl_source.txt")
