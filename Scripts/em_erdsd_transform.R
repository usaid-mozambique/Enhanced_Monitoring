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
path_historic_input_file <- "Data/Ajuda/ERDSD/erdsd.csv"
path_historic_output_file <- "Dataout/em_erdsd_historic_transform.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

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
         pop_type = recode(pop_type,
                           KeyPop = "KP"),
         numdenom = recode(numdenom,
                           Numerator = "N",
                           Denominator = "D"),
         numdenom = replace_na(numdenom, "N"))


# df1 <- df0 %>% 
#   mutate(row_n = row_number()) %>% 
#   pivot_wider(names_from = indicator, values_from = value, values_fill = 0) %>% 
#   mutate(ER_1_N = case_when(numdenom == "Numerator" ~ ER1Month),
#          ER_1_D = case_when(numdenom == "Denominator" ~ ER1Month),
#          
#          ER_4_N = case_when(numdenom == "Numerator" ~ ER4Month),
#          ER_4_D = case_when(numdenom == "Denominator" ~ ER4Month),
#          
#          IMER_1_N = case_when(numdenom == "Numerator" ~ IMER1),
#          IMER_1_D = case_when(numdenom == "Denominator" ~ IMER1),
#          
#          IMER_1B_N = case_when(numdenom == "Numerator" ~ IMER1B),
#          IMER_1B_D = case_when(numdenom == "Denominator" ~ IMER1B),
# 
#          TX_CURR = case_when(!pop_type == "KeyPop" ~ TX_CURR),
#          TX_NEW = case_when(!pop_type == "KeyPop" ~ TX_NEW)) %>% 
#   glimpse()
# 
# 
# df2 <- df1 %>% 
#   select(partner:dsd_eligibility,
#          starts_with("ER_"),
#          starts_with("IMER_"),
#          starts_with("TX_CURR"),
#          TX_MMD = starts_with("MMD"),
#          starts_with("TX_NEW")) %>% 
#   pivot_longer(ER_1_N:IMER_1B_D, names_to = "indicator", values_to = "value") %>% 
#   relocate(indicator, .after = period) %>% 
#   glimpse()
# 
# df_print <- df3 %>% 
#   filter(period > "2022-01-19") %>% 
#   filter(!is.na(value))
# 
# df_print %>% 
#   count(indicator)
  

write_tsv(
  df0,
  {path_historic_output_file})


# NOTES FROM VALIDATION OF ABOVE...
# TX_CURR is ok
# TX_CURR kp disag is ok
# TX_CURR_previous is ok
# TX_CURR_previous kp disag is ok
# TX_NEW is ok
# TX_NEW kp disag is ok
# IMER1 num disag is ok
# IMER1 den disag is ok
# IMER1B num disag is ok
# IMER1B den disag is ok#
# TX_MDD is ok

table <- df0 %>% 
  distinct(indicator, pop_type, numdenom)


df3 %>% 
  count(indicator, pop_type) %>% 
  print(n =100)
         
  



# PROCESS ER/DSD DATAFRAME ------------------------------------------------


df_tidy <- df0 %>% 
  mutate(row_n = row_number(),
         DATIM_code = recode(DATIM_code, EjFYleP5G9K = "LqB6YZq9sG2")) %>% 
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
  {path_historic_output_file})


# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

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
  ggplot(aes(x = period, y = TX_CURR, fill = partner)) + 
  geom_col() + 
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text())

