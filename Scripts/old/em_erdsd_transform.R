rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(glamr)
library(googlesheets4)
library(googledrive)
library(fs)
library(lubridate)
library(janitor)
library(ggthemes)
library(readxl)
library(openxlsx)
library(glue)
library(gt)
load_secrets() 



# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_historic_input_file <- "Data/Ajuda/ERDSD/imer_historic.csv"
path_historic_output_file <- "Dataout/IMER/monthly_processed/IMER_Historic.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU") # google drive folder where historic dataset saved

imer_indicators <- c("previous_TX_CURR",
          "TX_CURR",
          "TX_NEW",
          "MMD",
          "ER1Month",
          "ER4Month",
          "IMER1",
          "IMER1B")

# LOAD METADATA -----------------------------------------------------------


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
         latitude = Lat,
         longitude = Long)

# PROCESS DATAFRAME ------------------------------------------------

df0 <- read_csv(path_historic_input_file) %>% 
  filter(Indicator %in% imer_indicators,
         !PatientType == "Total") %>% 
  mutate(Months = as.Date(Months, "%d/%m/%Y")) %>% 
  rename_with(tolower, .cols = everything()) %>% 
  select(partner,
         snu = province,
         psnu = district,
         sitename = health.facility,
         datim_uid = datim_code,
         period = months,
         indicator,
         sex,
         age = ageasentered,
         pop_type = patienttype,
         key_pop = keypop,
         dispensation,
         numdenom = numden,
         er_status,
         dsd_eligibility,
         value) %>% 
  mutate(datim_uid = recode(datim_uid, EjFYleP5G9K = "LqB6YZq9sG2"), # correct historic chicavane datim_uid
         partner = recode(partner, "FADM" = "JHPIEGO-DoD"),
         indicator = recode(indicator, previous_TX_CURR = "TX_CURR_Previous"),
         age = str_replace(age, " to ", "-"), 
         age = recode(age,
                      Pediatrics = "<15",
                      Adults = "15+",
                      `01-04` = "1-4",
                      `05-09` = "5-9",
                      `>=65` = "65+"),
         age = case_when(str_detect(indicator, "IMER")  ~ NA_character_,
                         str_detect(indicator, "ER1")  ~ NA_character_,
                         str_detect(indicator, "ER4")  ~ NA_character_,
                         TRUE ~ age),
         dispensation = recode(dispensation,
                      `1 Month` = "<3",
                      `3 Months` = "3-5",
                      `6 Months` = "6+"),
         pop_type = recode(pop_type,
                           Pediatrics = "Pediatric",
                           KeyPop = "KP"),
         pop_type = case_when(age %in% c("<15", "<1", "1-4", "5-9", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown") & indicator %in% c("TX_CURR", "TX_CURR_Previous", "TX_NEW", "MMD") ~ "By Age",
                              TRUE ~ pop_type), 
         numdenom = recode(numdenom,
                           Numerator = "N",
                           Denominator = "D"),
         numdenom = replace_na(numdenom, "N"),
         indicator = case_when(indicator == "ER1Month" & numdenom == "N" ~ "ER_1_N",
                               indicator == "ER1Month" & numdenom == "D" ~ "ER_1_D",
                               indicator == "ER4Month" & numdenom == "N" ~ "ER_4_N",
                               indicator == "ER4Month" & numdenom == "D" ~ "ER_4_D",
                               indicator == "IMER1" & numdenom == "N" ~ "IMER_1_N",
                               indicator == "IMER1" & numdenom == "D" ~ "IMER_1_D",
                               indicator == "IMER1B" & numdenom == "N" ~ "IMER_1B_N",
                               indicator == "IMER1B" & numdenom == "D" ~ "IMER_1B_D",
                               indicator == "MMD" ~ "TX_MMD",
                               TRUE ~ indicator),
         er_status = recode(er_status,
                            "StoppedART" = "Stopped ART",
                            "TransferredOut" = "Transferred Out"),
         er_status = case_when(str_detect(er_status, "Denominator")  ~ NA_character_,
                               str_detect(er_status, "Numerator")  ~ NA_character_,
                               TRUE ~ er_status))

# PRINT DATAFRAME TO DISK -------------------------------------------------
  

write_tsv(
  df0,
  {path_historic_output_file})



  
