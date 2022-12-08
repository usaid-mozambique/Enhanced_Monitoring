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
month <- "2023-01-20" # replace with value from meta tab
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2023_01/"


# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("TXTB_{dt}")


# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Jan_2023_IP.xlsx")


# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_txtb.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved


# METADATA -----------------------------------------------------------


ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "list_ajuda") %>%  
  select(
    datim_uid,
    ends_with("tude"),
    starts_with("partner_"),
    grm_sernap,
    starts_with("his_"),
    starts_with("program_"),
    security_insecure)




df <- read_excel(DOD, 
                 sheet = "TX_CTER", 
                 skip = 9,
                 col_types = "text") %>% 
  select(!observation) %>% 
  pivot_longer(remove...7:remove...197, 
               names_to = "indicator", 
               values_to = "value") %>% 
  filter(!indicator == "remove") %>% 
  glimpse()



imer_reshape <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "TX_CTER", 
                   skip = 9,
                   col_types = "text") %>% 
    pivot_longer(TX_NEWTot:I4_ER4_40_RetCalc, 
                 names_to = "indicator", 
                 values_to = "value") %>% 
    inner_join(erdsd_var_mapping, by = "indicator") %>% 
    filter(!indicator_new == "remove") %>% 
    separate(indicator_new, 
             c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
             sep = "\\.") %>% 
    mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
           value = as.numeric(value),
           period = as.Date(month, "%Y-%m-%d"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = str_replace_all(age, "\\_", "-"),
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
           pop_type = case_when(age %in% c("<15", "<1", "1-4", "5-9", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown") ~ "By Age",
                                TRUE ~ pop_type),
           numdenom = replace_na(numdenom, "N"),
           er_status = recode(er_status,
                              "Initiated ART" = NA_character_)) %>% 
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
  
  return(df)
  
}


reshape_wide_em_txtb <- function(filename, ip){
  
  df <- read_excel(filename, # argument
                   sheet = "TB_TX", 
                   skip = 9) %>% 
    filter(partner == ip) %>%  # argument
    select(!c(contains(c("observation", "remove", "tot")))) %>%
    pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk', 
                 names_to = c("indicator", "disaggregate", "sex", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    mutate(period = as.Date(month, "%Y-%m-%d"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = recode(age, "Unk" = "Unknown"),
           disaggregate = recode(disaggregate, 
                                 "newART" = "New on ART",
                                 "alreadyART" = "Already on ART")) %>% 
    pivot_wider(names_from = indicator, 
                values_from = value) %>%
    relocate(period, .before = everything()) %>% 
    glimpse()
  
}
dod <- reshape_wide_em_txtb(DOD, "Jhpiego-DoD")


reshape_wide_em_tpt <- function(filename, ip){
  
  df <- read_excel(filename, sheet = "TB_TPT", 
                   col_types = c("text", "text", "text", "text", "text", "text",
                                 "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric"),
                   skip = 9) %>%
    filter(partner == ip) %>% 
    mutate(period = as.Date(month, "%Y-%m-%d")) %>% 
    select(c(period,
             partner,
             snu1,
             psnu,
             sitename,
             datim_uid,
             TX_CURR,
             TX_CURR_TPT_Com,
             TX_CURR_TPT_Not_Comp,
             TX_CURR_TB_tto,
             TX_CURR_TPT_Not_Comp_POS_Screen,
             TX_CURR_Eleg_TPT_Comp,
             TX_CURR_W_TPT_last7Mo,
             TX_CURR_Eleg_TPT_Init)) %>% 
    glimpse()
  
}
dod <- reshape_wide_em_tpt(DOD, "Jhpiego-DoD")


# FUNCTIONS RUN -------------------------------------------------


dod <- reshape_em_txtb(DOD, "Jhpiego-DoD", shape = "wide")
dod <- reshape_wide_em_tpt(DOD, "Jhpiego-DoD")
