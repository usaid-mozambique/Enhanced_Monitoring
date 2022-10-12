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

file <- glue::glue("DSD_{dt}")

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
path_historic_output_file <- "Dataout/em_dsd.txt" # folder path where monthly dataset archived
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


# FUNCTIONS ---------------------------------------------

df <- read_excel(ECHO, 
                 sheet = "MDS", 
                 skip = 8) %>% 
  select(!c(No, SISMA_code, Period)) %>% 
  pivot_longer(remove.1:DSD.AHD__LW_15p, 
               names_to = c("indicator", "dsd_eligibility", "pop_type", "age"),
               names_sep = "_",
               values_to = "value") %>% 
  filter(!str_detect(indicator, "remove")) %>% 
  mutate(indicator = str_replace_all(indicator, "\\.", "_"),
         age = str_replace_all(age, "\\.", "-"),
         age = case_when(age == "15p" ~ "15+",
                         age == "2u" ~ "<2",
                         TRUE ~ age),
         dsd_eligibility = recode(dsd_eligibility,
                                  ELI = "Eligible",
                                  NEL = "Non-Eligible",
                                  TOTAL = NA_character_),
         # dsd_eligibility = case_when(
         #   dsd_eligibility == "ELI" ~ "Eligible",
         #   dsd_eligibility == "NEL" ~ "Non-Eligible",
         #   age == "<2" & indicator %in% c("DSD_PCD", "DSD_MB", "DSD_MC", "DSD_FA", "DSD_AC", "DSD_HE", "DSD_1STB", "DSD_1S", "DSD_1SYF", "DSD_1SMCH", "DSD_AHD") ~ "Non-Eligible",
         #   pop_type %in% c("PW", "LW") & indicator %in% c("DSD_PCD", "DSD_MB", "DSD_MC", "DSD_FA", "DSD_AC", "DSD_HE", "DSD_1STB", "DSD_1S", "DSD_1SYF", "DSD_1SMCH", "DSD_AHD") ~ "Non-Eligible",
         #   NA ~ "Unknown"),
         pop_type = recode(pop_type, 
                           ADULT = "Adult",
                           PED = "Pediatric")) %>% 
  glimpse()

# report headers need updating as a broad set of dsd models are not coded for eligibility
df %>% distinct(indicator, dsd_eligibility) %>% print(n=100)
sum(df$value, na.rm=T)
temp <- df %>% filter(dsd_eligibility == "")
unique(temp$indicator)
ind <- unique(temp$indicator)

temp <- df %>% filter(indicator == "TX_ACTIVE")
