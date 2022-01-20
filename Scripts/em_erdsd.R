#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

rm(list = ls())

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(scales)


#-----------------------------------------------------------------------------------
# IMPORT ECHO SUBMISSION
# NOTE THAT THE MONTH COLUMN NEEDS TO BE IN 5 DIGIT EXCEL FORMAT IN ORDER FOR CODE TO RUN WITHOUT RETURNING ERRORS.  

df0 <- read_excel("Data/Ajuda/ERDSD/AJUDA_Transformed_Dez21.xlsx", 
                                       sheet = "Jul_Dec2021",
                  col_types = c("text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))

df1 <- read_excel("Data/Ajuda/ERDSD/AJUDA_Transformed_July12.xlsx", 
                  sheet = "Jan_Jun2021",
                  col_types = c("text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))


df2 <- read_excel("Data/Ajuda/ERDSD/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                  sheet = "Aug_Dec2020",
                  col_types = c("text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))

df3 <- read_excel("Data/Ajuda/ERDSD/AJUDA_NewStructure_Mar16_Revised.xlsx", 
                  sheet = "Feb_Jul2020",
                  col_types = c("text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric"))

AJUDA_Site_Map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx") %>% 
  dplyr::select(-c(SNU, Psnu, Sitename, em_wave))

#-------------------------------------------------------------------  ----------------
# DEFINE PATH FOR OUTPUT

em_erdsd <- ("Dataout/em_erdsd.txt") 

#-----------------------------------------------------------------------------------
# UNION DATA FROM IMPORTS

df <- dplyr::bind_rows(df0, df1, df2, df3)

#-----------------------------------------------------------------------------------
# COERCE 5 DIGIT NUMBER TO DATE AND REMOVE UNNEEDED VARIABLES

df <- df %>% mutate(Date = excel_numeric_to_date(Months, date_system = "modern")) %>%
  dplyr::rename(Orgunituid = DATIM_code,
                Site = `Health Facility`)

#-----------------------------------------------------------------------------------
# PROCESS DATAFRAME AND CALCULATE VARIABLES

df_tidy <- df %>% 
  dplyr::mutate(row_n = row_number()) %>% 
  tidyr::pivot_wider(names_from = Indicator, values_from = Value, values_fill = 0) %>% 
  dplyr::mutate(ER1Month_N = case_when(!PatientType == "Total" & NumDen == "Numerator" ~ ER1Month),
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
                TX_NEW_2 = case_when(!PatientType == "KeyPop" & !PatientType == "Total" ~ TX_NEW),
                AgeCoarse = if_else(PatientType %in% c("Pediatrics"), "<15", 
                                    if_else(PatientType %in% c("Adults", "Non-Pregnant Adults", "Pregnant", "Breastfeeding"), "15+", ""))) %>% 
  dplyr::left_join(AJUDA_Site_Map, by = c("Orgunituid" = "orgunituid")) %>% 
  dplyr::select(-c(SISMA_code, Period, `IP FY20`, Type, ajuda, ajuda_phase, Months, `Source.Name`, HF_Export, province_HF, IMER1, IMER1B, ER1Month, ER4Month, TX_CURR, TX_NEW, row_n)) %>% 
  dplyr::rename(TX_CURR = TX_CURR_2,
                TX_NEW = TX_NEW_2) %>% 
  dplyr::filter(!PatientType == "Total") %>% 
  dplyr::relocate(Date, .before = 1) %>% 
  dplyr::relocate(sisma_id, Lat, Long, .before = 7) %>% 
  dplyr::relocate(emr, epts, idart, disa, conflict, corridor, ovc, ycm, pmtct_pda, .before = 10) %>% 
  dplyr::relocate(AgeCoarse, .after = 20) %>% 
  glimpse()

test <- df_tidy %>% 
  filter(Date == "2021-11-20")

sum(test$TX_NET_NEW, na.rm = T)

#-----------------------------------------------------------------------------------
# PRINT DATAFRAME TO DISK

write_tsv(
  df_tidy,
  {em_erdsd})

#-----------------------------------------------------------------------------------
# VISUALS WITH GGPLOT2

df_tidy %>% 
  ggplot(aes(x = Date, y = TX_CURR, color = Partner)) + 
  geom_col() + 
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text())

ggplot(data = df_tidy) +
  geom_col(
    mapping = aes(x = Date, y = TX_CURR, color = Partner)
  ) + 
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text())
  

df_tidy_group <- df_tidy %>% 
  group_by(Date, Partner) %>%
  summarize(TX_CURR = sum(TX_CURR, na.rm = TRUE))


ggplot(df_tidy_group, aes(Date, TX_CURR, color = Partner)) +
  geom_line() +
  theme_wsj()

ggplot(df_tidy_group, aes(Date, TX_CURR, color = Partner)) +
  geom_col() +
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text())


  theme_wsj()


