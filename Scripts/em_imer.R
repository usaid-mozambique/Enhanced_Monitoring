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
month <- "2022-09-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2022_09/"

# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("IMER_{dt}")

# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Oct_2022_FGH.xlsx")


# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/IMER/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_imer.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------


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

erdsd_var_mapping <- read_excel("Documents/erdsd_var_mapping.xlsx", sheet = "Sheet5")


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


# FUNCTIONS RUN -------------------------------------------------


dod <- imer_reshape(DOD, "JHPIEGO-DoD")
echo <- imer_reshape(ECHO, "ECHO")
ariel <- imer_reshape(ARIEL, "ARIEL")
ccs <- imer_reshape(CCS, "CCS")
egpaf <- imer_reshape(EGPAF, "EGPAF")
fgh <- imer_reshape(FGH, "FGH")
icap <- imer_reshape(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


imer <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
imer %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = c("datim_uid" = "datim_uid"))


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


# write to local
readr::write_tsv(
  imer,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# SURVEY ALL MONTHLY DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

imer_tidy_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) 


# METADATA JOIN ---------------------------------

imer_tidy_historic_2 <- imer_tidy_historic %>% 
  filter(period <= as.Date(month)) %>% 
  select(-c(partner,
            snu,
            psnu,
            sitename)) %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>%
  glimpse()


# OUTPUT CLEAN -----------------------


imer_tidy_historic_3 <- imer_tidy_historic_2 %>%
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
         numdenom,
         pop_type,
         key_pop,
         dispensation,
         er_status,
         dsd_eligibility,
         sex,
         age,
         value) %>% 
  glimpse()


# OUTPUT WRITE ----------------------------------------------


readr::write_tsv(
  imer_tidy_historic_3,
  "Dataout/em_imer.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)
