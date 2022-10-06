rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(glamr)
library(googlesheets4)
library(googledrive)
library(fs)
library(lubridate)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(gt)
load_secrets() 


# VALUES & PATHS ---------------------------

# update each month
month <- "20/09/2022" 
file <- "IMER_2022_09"

# update each month

TEST <- "Data/Ajuda/ER_DSD_TPT_VL/2022_09/MonthlyEnhancedMonitoringTemplates_FY22_Oct 5 2022 Submission_Tete.xlsx"

# DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_08/MonthlyEnhancedMonitoringTemplates_FY22_August2022_DOD.xlsx"
# ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_09/MonthlyEnhancedMonitoringTemplates_FY22_Oct 5 2022 Submission_Tete.xlsx"
# CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_08/MonthlyEnhancedMonitoringTemplates_FY22_August2022 CCS.xlsx"
# ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_08/MonthlyEnhancedMonitoringTemplates_FY22_August2022_ECHO.xlsx"
# EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_08/MonthlyEnhancedMonitoringTemplates_FY22_August2022 EGPAF.xlsx"
# ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_08/MonthlyEnhancedMonitoringTemplates_FY22_August2022_ICAP.xlsx"
# FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_08/MonthlyEnhancedMonitoringTemplates_FY22_August2022_FGH.xlsx"




# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/IMER/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_imer.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------

erdsd_var_mapping <- read_excel("Documents/erdsd_var_mapping.xlsx", sheet = "Sheet5")

ajuda_site_map <- read_sheet(path_ajuda_site_map) %>%
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


# FUNCTIONS ---------------------------------------------


imer_reshape <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "TX NEW, TX CURR AND IMER", 
                   skip = 8,
                   col_types = "text") %>% 
    select(!c(No, SISMA_code, Period)) %>% 
    pivot_longer(TX_NEWTot:I4_ER4_40_RetCalc, 
                 names_to = "indicator", 
                 values_to = "value") %>% 
    inner_join(erdsd_var_mapping, by = "indicator") %>% 
    filter(!indicator_new == "remove") %>% 
    separate(indicator_new, 
             c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
             sep = "_") %>% 
    mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
           value = as.numeric(value),
           period = as.Date(month, "%d/%m/%Y"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = str_replace_all(age, "\\.", "-"),
           age = recode(age,
                        "unknown" = "Unknown"),
           sex = recode(sex,
                        "M" = "Male",
                        "F" = "Female")) %>% 
    filter(Partner == ip) %>% 
    select(partner = Partner,
           snu = Province,
           psnu = District,
           sitename = `Health Facility`,
           datim_uid = DATIM_code,
           period,
           indicator,
           sex,
           age,
           pop_type,
           dispensation,
           numdenom,
           er_status,
           dsd_eligibility,
           value)
  
  tx_curr_prev <- df %>%
    filter(indicator == "TX_CURR") %>% 
    mutate(indicator = recode(indicator,
                              "TX_CURR" = "TX_CURR_Previous"),
           period = period + months(1))
  
  df <- bind_rows(df, tx_curr_prev)
  
  return(df)
  
}



df <- read_excel(TEST, 
                 sheet = "TX NEW, TX CURR AND IMER", 
                 skip = 8,
                 col_types = "text") %>% 
  select(!c(No, SISMA_code, Period)) %>% 
  pivot_longer(TX_NEWTot:I4_ER4_40_RetCalc, 
               names_to = "indicator", 
               values_to = "value") %>% 
  inner_join(erdsd_var_mapping, by = "indicator") %>% 
  filter(!indicator_new == "remove") %>% 
  separate(indicator_new, 
           c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
           sep = "//.") %>% 
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
         value = as.numeric(value),
         period = as.Date(month, "%d/%m/%Y"),
         indicator = str_replace_all(indicator, "\\.", "_"),
         age = str_replace_all(age, "\\.", "-"),
         age = recode(age,
                      "unknown" = "Unknown"),
         sex = recode(sex,
                      "M" = "Male",
                      "F" = "Female"),
         key_pop = case_when(pop_type == "FSW" ~ "FSW",
                             pop_type == "MSM" ~ "MSM",
                             pop_type == "PWID" ~ "PWID",
                             pop_type == "PPCS" ~ "PPCS"),
         pop_type = recode(pop_type,
                           "FSW" = "KP",
                           "MSM" = "KP",
                           "PWID" = "KP",
                           "PPCS" = "KP"),
         pop_type = case_when(age %in% c("<15", "<1", "1-4", "5-9", "10-14") ~ "Pediatric",
                              age %in% c("15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+") ~ "Adult")) %>% 
  filter(Partner == "ECHO") %>% 
  select(partner = Partner,
         snu = Province,
         psnu = District,
         sitename = `Health Facility`,
         datim_uid = DATIM_code,
         period,
         indicator,
         sex,
         age,
         pop_type,
         key_pop,
         dispensation,
         numdenom,
         er_status,
         dsd_eligibility,
         value)

tx_curr_prev <- df %>%
  filter(indicator == "TX_CURR") %>% 
  mutate(indicator = recode(indicator,
                            "TX_CURR" = "TX_CURR_Previous"),
         period = period + months(1))

df <- bind_rows(df, tx_curr_prev)


df %>% distinct(indicator, sex, age, pop_type) %>% print(n=150)
df %>% distinct(indicator, key_pop, er_status) %>% print(n=150)

test <- df %>% filter(is.na(pop_type)) %>% distinct(indicator, pop_type)
test <- df %>% filter(indicator == "TX_NEW")

  
# FUNCTIONS RUN -------------------------------------------------

# test <- imer_reshape(TEST, "ECHO")


# dod <- imer_reshape(DOD, "JHPIEGO-DoD")
# echo <- imer_reshape(ECHO, "ECHO")
# ariel <- imer_reshape(ARIEL, "ARIEL")
# ccs <- imer_reshape(CCS, "CCS")
# egpaf <- imer_reshape(EGPAF, "EGPAF")
# fgh <- imer_reshape(FGH, "FGH")
# icap <- imer_reshape(ICAP, "ICAP")

glimpse(test)

# NEED TO CREATE key_pop var and recode pop_type key pop to KP

# COMPILE DATASETS --------------------------------------------------




# MONTHLY FILE WRITE ------------------------------------




# HISTORIC DATASET BUILD ---------------------------------





# METADATA JOIN ---------------------------------

imer_tidy_historic <- ariel %>% 
  select(-c(partner,
            snu,
            psnu,
            sitename)) %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>%
  glimpse()


# OUTPUT CLEAN -----------------------


imer_tidy_historic_2 <- imer_tidy_historic %>%
  select(datim_uid,
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
         indicator,
         pop_type,
         dispensation,
         numdenom,
         er_status,
         dsd_eligibility,
         sex,
         age,
         value) %>% 
  glimpse()




# OUTPUT WRITE ----------------------------------------------



# PLOTS & TABLES ---------------------------------------------------------------




