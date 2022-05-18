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


# IMPORT DATA -------------------------------------------------------------


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


df0 <- read_tsv("Data/Ajuda/ERDSD/AJUDA_transformed.txt") %>% 
  mutate(Months = as.Date(Months, "%d/%m/%Y"))


# df1 <- read_csv("Data/Ajuda/ERDSD/AJUDA_transformed_Mar22.txt") %>% 
#   select(!c(`...1`, id)) %>% 
#   rename(Health.Facility = `Health Facility`)
# 
# df2 <- bind_rows(df1, df0)


#-------------------------------------------------------------------  ----------------
# DEFINE PATH FOR OUTPUT


em_erdsd <- ("Dataout/em_erdsd.txt") 


# PROCESS ER/DSD DATAFRAME ------------------------------------------------


df_tidy <- df2 %>% 
  mutate(row_n = row_number()) %>% 
  pivot_wider(names_from = Indicator, values_from = value, values_fill = 0) %>% 
  mutate(ER1Month_N = case_when(!PatientType == "Total" & NumDen == "Numerator" ~ ER1Month),
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
  filter(!PatientType == "Total") %>%
  select(-c(TX_CURR, TX_NEW, row_n)) %>% 
  rename(TX_CURR = TX_CURR_2,
         TX_NEW = TX_NEW_2,
         TX_CURR_prev = previous_TX_CURR,
         TX_MMD = MMD,
         DSD_One = OneDSD,
         DSD = DSD,
         DSD_3MD = `3MDD`,
         DSD_6MD = `6MDD`,
         DSD_FluxoRa = FluxoRa,
         DSD_GAAC = GAAC,
         DSD_AFam = AFam,
         DSD_ClubA = ClubA,
         DSD_DCom = DComm) %>% 
  glimpse()


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- df_tidy %>% 
  select(DATIM_code, Months, TX_CURR) %>% 
  filter(Months == max(Months)) %>%
  group_by(DATIM_code, Months) %>% 
  summarize(TX_CURR = sum(TX_CURR, na.rm = T)) %>% 
  mutate(site_volume = case_when(
    TX_CURR < 1000 ~ "Low",
    between(TX_CURR, 1000, 5000) ~ "Medium",
    TX_CURR > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(DATIM_code, site_volume) %>% 
  glimpse()
  

# JOIN META DATA AND CLEAN DATAFRAME --------------------------------


df_tidy_2 <- df_tidy %>% 
  left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>% 
  left_join(volumn_period, by = c("DATIM_code" = "DATIM_code")) %>% 
  mutate(
    agency = case_when(
      partner == "ECHO" ~ "USAID",
      partner == "JHPIEGO-DoD" ~ "DoD",
      TRUE ~ "CDC")) %>% 
  select(datim_uid = DATIM_code,
         sisma_uid,
         site_nid = SISMA_code,
         period = Months,
         agency,
         partner,
         snu,
         psnu,
         sitename,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         num_den = NumDen,
         patient_type = PatientType,
         dsd_eligibility = DSD_Eligibility,
         keypop = KeyPop,
         er_status = ER_Status,
         dispensation = Dispensation,
         age = AgeAsEntered,
         age_coarse = AgeCoarse,
         sex = Sex,
         starts_with("TX"),
         starts_with(c("ER1Month", "ER4Month", "IMER")),
         starts_with("DSD")) %>% 
  glimpse()



# PRINT DATAFRAME TO DISK -------------------------------------------------


write_tsv(
  df_tidy_2,
  {em_erdsd})


# TABLES & GRAPHS --------------------------------------------------

max_period <- max(df_tidy_2$period)

df_tidy_2 %>% 
  filter(period == max_period) %>% 
  count(snu,
        sort = TRUE,
        wt = TX_CURR,
        name = "TX_CURR")

df_tidy_2 %>% 
  count(period,
        sort = TRUE,
        wt = TX_CURR,
        name = "TX_CURR")


df_tidy_graph <- df_tidy_2 %>% 
  group_by(period, partner, snu) %>% 
  summarize(TX_CURR = sum(TX_CURR, na.rm = T)) %>% 
  ungroup()


df_tidy_graph %>% 
  ggplot(aes(x = period, y = TX_CURR, color = partner)) + 
  geom_col() + 
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text())
