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
month <- "2022-11-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2022_11/"

# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("DSD_{dt}")

# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov 2022_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_FGH.xlsx")



# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/DSD/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/15x2biGIIYrY_eW-zrKQbrvMT47cI5yOE") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_dsd.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------


ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "list_ajuda")


# FUNCTIONS ---------------------------------------------


dsd_reshape <- function(df, ip) {
  
  df <- read_excel(df, # function argument
                   sheet = "MDS", 
                   skip = 8) %>% 
    select(!c(No, SISMA_code, Period)) %>% 
    pivot_longer(remove.1:DSD.AHD__LW_15p, 
                 names_to = c("indicator", "dsd_eligibility", "pop_type", "age"),
                 names_sep = "_",
                 values_to = "value") %>% 
    filter(Partner == ip, # function argument
           !str_detect(indicator, "remove")) %>% 
    mutate(period = as.Date(month, "%Y-%m-%d"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = str_replace_all(age, "\\.", "-"),
           age = case_when(age == "15p" ~ "15+",
                           age == "2u" ~ "<2",
                           TRUE ~ age),
           dsd_eligibility = recode(dsd_eligibility,
                                    ELI = "Eligible",
                                    NEL = "Non-Eligible",
                                    TOTAL = NA_character_),
           pop_type = recode(pop_type, 
                             ADULT = "Adult",
                             PED = "Pediatric")) %>% 
    select(partner = Partner,
           snu = Province,
           psnu = District,
           sitename = `Health Facility`,
           datim_uid = DATIM_code,
           period,
           indicator,
           dsd_eligibility,
           pop_type,
           age,
           value)

}


# FUNCTIONS RUN -------------------------------------------------


dod <- dsd_reshape(DOD, "JHPIEGO-DoD")
echo <- dsd_reshape(ECHO, "ECHO")
ariel <- dsd_reshape(ARIEL, "ARIEL")
ccs <- dsd_reshape(CCS, "CCS")
egpaf <- dsd_reshape(EGPAF, "EGPAF")
fgh <- dsd_reshape(FGH, "FGH")
icap <- dsd_reshape(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


dsd <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
dsd %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = c("datim_uid" = "datim_uid"))


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


# write to local
readr::write_tsv(
  dsd,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# SURVEY ALL MONTHLY DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

dsd_tidy_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) 



# METADATA JOIN ---------------------------------


dsd_tidy_historic_2 <- dsd_tidy_historic %>% 
  filter(period <= as.Date(month)) %>% 
  select(-c(partner,
            snu,
            psnu,
            sitename)) %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>%
  glimpse()



# OUTPUT CLEAN -----------------------


dsd_tidy_historic_3 <- dsd_tidy_historic_2 %>%
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner_pepfar_clinical,
         snu,
         psnu,
         sitename,
         ends_with("tude"),
         starts_with("program_"),
         starts_with("his_"),
         indicator,
         pop_type,
         dsd_eligibility,
         age,
         value) %>% 
  mutate(temp_indicator = indicator,
         temp_value = value) %>% 
  pivot_wider(
    names_from = temp_indicator,
    values_from = temp_value)


# OUTPUT WRITE ----------------------------------------------


readr::write_tsv(
  dsd_tidy_historic_3,
  "Dataout/em_dsd.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

