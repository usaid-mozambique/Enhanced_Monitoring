#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES
library(readr)
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(janitor)
library(readxl)
library(glue)

rm(list = ls())

#-----------------------------------------------------------------------------------
# IMPORT ECHO SUBMISSION
# NOTE THAT THE MONTH COLUMN NEEDS TO BE IN 5 DIGIT EXCEL FORMAT IN ORDER FOR CODE TO RUN WITHOUT RETURNING ERRORS.  

df0 <- read_excel("Ajuda/data_source/ERDSD/AJUDA_NewStructure_May11.xlsx", 
                                       sheet = "Jan_Jun2021")


df1 <- read_excel("Ajuda/data_source/ERDSD/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                  sheet = "Aug_Dec2020", col_types = c("text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "numeric", "text", "numeric", "text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "numeric"))

df2 <- read_excel("Ajuda/data_source/ERDSD/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                  sheet = "Feb_Jul2020", col_types = c("text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "numeric", "text", "numeric", "text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "numeric"))

AJUDA_Site_Map <- read_excel("~/GitHub/AjudaSiteMap/AJUDA Site Map.xlsx") %>% 
  dplyr::select(-c(SNU, Psnu, Sitename, em_wave))


em_erdsd <- ("Ajuda/output/em_erdsd.txt") 

#-----------------------------------------------------------------------------------
# UNION DATA FROM IMPORTS

df <- dplyr::bind_rows(df0, df1, df2)

#-----------------------------------------------------------------------------------
# COERCE 5 DIGIT NUMBER TO DATE AND REMOVE UNNEEDED VARIABLES

df <- df %>% mutate(Date = excel_numeric_to_date(Months, date_system = "modern")) %>%
  dplyr::rename(Orgunituid = DATIM_code,
                Site = `Health Facility`)

#-----------------------------------------------------------------------------------
# PROCESS DATAFRAME AND CALCULATE VARIABLES

df_new <- df %>% 
  tidyr::pivot_wider(names_from = Indicator, values_from = Value, values_fill = NULL) %>% 
  dplyr::mutate(TX_NET_NEW = TX_CURR - Previous_TX_CURR,
                ER1Month_N = case_when(!PatientType == "Total" & NumDen == "Numerator" ~ ER1Month),
                ER1Month_D = case_when(!PatientType == "Total" & NumDen == "Denominator" ~ ER1Month),
                ER1Month_Retained = case_when(!PatientType == "Total" & ER_Status == "Retained" ~ ER1Month),
                ER1Month_TransferredOut = case_when(!PatientType == "Total" & ER_Status == "TransferredOut" ~ ER1Month),
                ER4Month_N = case_when(!PatientType == "Total" & NumDen == "Numerator" ~ ER4Month),
                ER4Month_D = case_when(!PatientType == "Total" & NumDen == "Denominator" ~ ER4Month),
                ER4Month_Retained = case_when(!PatientType == "Total" & ER_Status == "Retained" ~ ER4Month),
                ER4Month_TransferredOut = case_when(!PatientType == "Total" & ER_Status == "TransferredOut" ~ ER4Month),
                ER4Month_LTFU = case_when(!PatientType == "Total" & ER_Status == "LTFU" ~ ER4Month),
                IMER1_N = case_when(!PatientType == "Total" & NumDen == "Numerator" ~ IMER1),
                IMER1_D = case_when(!PatientType == "Total" & NumDen == "Denominator" ~ IMER1),
                IMER1B_N = case_when(!PatientType == "Total" & NumDen == "Numerator" ~ IMER1B),
                IMER1B_D = case_when(!PatientType == "Total" & NumDen == "Denominator" ~ IMER1B),
                TX_CURR_KP = case_when(PatientType == "KeyPop" ~ TX_CURR),
                TX_NEW_KP = case_when(PatientType == "KeyPop" ~ TX_NEW),
                TX_CURR_2 = case_when(!PatientType == "KeyPop" & !PatientType == "Total" ~ TX_CURR),
                TX_NEW_2 = case_when(!PatientType == "KeyPop" & !PatientType == "Total" ~ TX_NEW)) %>% 
  dplyr::left_join(AJUDA_Site_Map, by = c("Orgunituid" = "orgunituid")) %>% 
  dplyr::select(-c(SISMA_code, Period, `IP FY20`, Type, ajuda, ajuda_phase, Months, `Source.Name`, HF_Export, province_HF, IMER1, IMER1B, ER1Month, ER4Month, TX_CURR, TX_NEW)) %>% 
  dplyr::rename(TX_CURR = TX_CURR_2,
                TX_NEW = TX_NEW_2) %>% 
  dplyr::filter(!PatientType == "Total") %>% 
  dplyr::relocate(Date, .before = 1) %>% 
  dplyr::relocate(sisma_id, Lat, Long, .before = 7) %>% 
  dplyr::relocate(emr, epts, idart, disa, conflict, corridor, ovc, ycm, pmtct_pda, .before = 10)

#-----------------------------------------------------------------------------------
# PRINT DATAFRAME TO DISK

write_excel_csv(
  df,
  "~/R/r_projects/Hfr/output/em_erdsd.csv",
  na = "NA",
  append = FALSE,
  delim = ",",
  quote_escape = "double",
  eol = "\n"
)

write_tsv(
  df_new,
  {em_erdsd})
