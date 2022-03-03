
# LOAD DEPENDENCIES -------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(scales)


# DEFINE PATHS FOR INPUTS & OUTPUT ----------------------------------------


erdsd_path <- ("Data/Ajuda/ERDSD/AJUDA_Transformed_Jan22.txt")
ajuda_meta_path <- ("~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx")
erdsd_output <- ("Dataout/em_erdsd.txt") 


# IMPORT DATASETS ---------------------------------------------------------


ajuda_meta <- read_excel({ajuda_meta_path})

df <- read_csv({erdsd_path})


# CLEAN IMPORTS -----------------------------------------------------------


df_1 <- df %>% 
  select(-c(...1, 
            Type,
            Partner, 
            Province, 
            District, 
            `Health Facility`, 
            SISMA_code, 
            Period)) %>% 
  rename(datim_uid = DATIM_code,
         period = Months,
         num_den = NumDen,
         sex = Sex,
         age = AgeAsEntered,
         dsd_eligibility = DSD_Eligibility,
         patient_type = PatientType,
         er_status = ER_Status,
         dispensation = Dispensation,
         keypop = KeyPop,
         indicator = Indicator
         ) %>% 
  glimpse()


ajuda_meta_1 <- ajuda_meta %>%  
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
         longitude = Long
         ) %>% 
  glimpse()



# JOIN METADATA --------------------------------------------------------------

df_2 <- df_1 %>% 
  left_join(ajuda_meta_1, by = "datim_uid") %>% 
  select(ends_with("uid"),
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
         num_den,
         patient_type,
         everything()) %>% 
  glimpse()
  
  
  # PIVOT WIDE & RENAME --------------------------------------------------------------
  
df_3 <- df_2 %>% 
  mutate(row_n = row_number()) %>% 
  pivot_wider(names_from = indicator, values_from = value, values_fill = 0) %>% 
  select(-c(row_n)) %>% 
  rename(TX_CURR_prev = previous_TX_CURR,
         TX_MMD = MMD,
         TX_3MD = `6MDD`,
         TX_6MD = `3MDD`) %>% 
  glimpse()


# CALCULATE COLUMN INDICATORS & FILTER TOTAL PATIENT TYPE ---------------------------------------------

df_4 <- df_3 %>% 
  mutate(ER1Month_N = case_when(!patient_type == "Total" & num_den == "Numerator" ~ ER1Month),
         ER1Month_D = case_when(!patient_type == "Total" & num_den == "Denominator" ~ ER1Month),
         ER1Month_Retained = case_when(!patient_type == "Total" & er_status == "Retained" ~ ER1Month),
         ER1Month_TransferredOut = case_when(!patient_type == "Total" & er_status == "TransferredOut" ~ ER1Month),
         ER4Month_N = case_when(!patient_type == "Total" & num_den == "Numerator" ~ ER4Month),
         ER4Month_D = case_when(!patient_type == "Total" & num_den == "Denominator" ~ ER4Month),
         ER4Month_Retained = case_when(!patient_type == "Total" & er_status == "Retained" ~ ER4Month),
         ER4Month_TransferredOut = case_when(!patient_type == "Total" & er_status == "TransferredOut" ~ ER4Month),
         ER4Month_LTFU = case_when(!patient_type == "Total" & er_status == "LTFU" ~ ER4Month),
         IMER1_N = case_when(!patient_type == "Total" & num_den == "Numerator" ~ IMER1),
         IMER1_D = case_when(!patient_type == "Total" & num_den == "Denominator" ~ IMER1),
         IMER1B_N = case_when(!patient_type == "Total" & num_den == "Numerator" ~ IMER1B),
         IMER1B_D = case_when(!patient_type == "Total" & num_den == "Denominator" ~ IMER1B),
         TX_CURR_KP = case_when(patient_type == "KeyPop" ~ TX_CURR),
         TX_NEW_KP = case_when(patient_type == "KeyPop" ~ TX_NEW),
         TX_CURR = case_when(!patient_type == "KeyPop" & !patient_type == "Total" ~ TX_CURR),
         TX_NEW = case_when(!patient_type == "KeyPop" & !patient_type == "Total" ~ TX_NEW),
         age_coarse = if_else(patient_type %in% c("Pediatrics"), "<15", 
                             if_else(patient_type %in% c("Adults", "Non-Pregnant Adults", "Pregnant", "Breastfeeding"), "15+", ""))) %>% 
  filter(!patient_type == "Total") %>% 
  glimpse()

# sum(df_3$TX_CURR, na.rm = T)
# # sum(df_3$TX_CURR_2, na.rm = T)
# sum(df_3$TX_NEW, na.rm = T)
# # sum(df_3$TX_NEW_2, na.rm = T)


# FINAL CLEANING ----------------------------------------------------------


df_5 <- df_4 %>% 
  relocate(age_coarse, .after =  age) %>% 
  relocate(starts_with("TX"), .after = keypop) %>% 
  relocate(starts_with(c("ER1Month", "ER4Month", "IMER")), .after = TX_NEW_KP) %>% 
  glimpse()

# PRINT TO DISK -----------------------------------------------------------

write_tsv(
  df_5,
  {erdsd_output})

