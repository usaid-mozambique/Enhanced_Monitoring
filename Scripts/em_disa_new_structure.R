
#------LOAD CORE TIDYVERSE & OTHER PACKAGES-------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(janitor)
library(glue)
library(readxl)
library(openxlsx)

rm(list = ls())

#---- DEFINE PATHS AND VALUES - REQUIREs UPDATING WITH EACH NEW DATASET! -------------------------------------------------------

file_monthly <- "Data/Disa/annual/Relatorio de Carga Viral (October 1, 2020 - September 31, 2021)[85].xlsx"
period <- "2021 Q4"

#---- DEFINE PATHS AND VALUES - COULD REQUIRE UPDATING EACH MONTH -------------------------------------------------------

final_output <- "Dataout/DISA_annual/disa_fy21.txt"

#---- LOAD DATASETS AND UNION -------------------------------------------------------

disa_datim_map <- read_excel("Documents/disa_datim_map_lt.xlsx") %>% 
  select(-c(Notes))

datim_ou_map <- read_excel("Documents/tx_site_reference.xlsx")

xAge <- read_excel({file_monthly}, 
                   sheet = "Age & Sex", 
                   col_types = c("text", "text", "text", 
                                 "text", "text", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"), 
                   skip = 2) %>% 
  mutate(group = "Age") %>%
  glimpse()

xPW <- read_excel({file_monthly}, 
                  sheet = "S. Viral (M. Gravidas)",
                  col_types = c("text", 
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"), 
                  skip = 2) %>% 
  mutate(group = "PW") %>% 
  rename(US = HF,
         PROVINCIA = PROVINCE,
         DISTRITO = DISTRICT) %>% 
  glimpse

xLW <- read_excel({file_monthly}, 
                  sheet = "S. Viral (M. Lactantes)",
                  col_types = c("text", 
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"),
                  skip = 2) %>% 
  mutate(group = "LW") %>%
  rename(US = HF,
         PROVINCIA = PROVINCE,
         DISTRITO = DISTRICT) %>% 
  glimpse

#---- NOTE THAT THE TRL TAB OF THE VL REPORT SOMETIMES COMES WITH TWO TABLES AND TOTAL FOR FACILITY LIST.  I ELIMINATE FIRST TABLE AND TAKE TOTAL OUT TO MAKE THE FIRST MUNGE BELOW WORK ------

df_tat <- read_excel({file_monthly}, 
                     sheet = "TRL", col_types = c("text", 
                                                  "text", "text", "text", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), 
                     skip = 2) %>% 
  select(-c(TOTAL))

df_vl <- bind_rows(xAge, xPW, xLW)

rm(xAge, xPW, xLW)

#---- PROCESS VL DATAFRAME -------------------------------------------------------

df_vl_1 <- df_vl %>% 
  select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  rename(sisma_id = `SISMA ID`,
         province = PROVINCIA,
         district = DISTRITO,
         site = US,
         age = Age,
         sex = Sex) %>% 
  relocate(c(group), .before = site) %>% 
  pivot_longer(`Rotina (<1000)`:`Motivo de Teste não especificado (>1000)`, names_to = "indicator", values_to = "value") %>% 
  mutate(motive = dplyr::case_when(grepl("Rotina", indicator) ~ "Routine",
                                   grepl("Fal", indicator) ~ "Theraputic Failure",
                                   grepl("Repetir", indicator) ~ "Post Breastfeeding",
                                   grepl("Motivo de Teste NS", indicator) ~ "Not Specified"),
         result = dplyr::case_when(grepl("<1000", indicator) ~ "<1000",
                                   grepl(">1000", indicator) ~ ">1000"),
         tat_step = "temp") %>% 
  select(-c(indicator)) %>% 
  glimpse()

#---- RECODE VL AGE/SEX VALUES -----------------------------------------------

df_vl_2 <- df_vl_1 %>% 
  dplyr::mutate(age = recode(age, "Idade não especificada" = "Unknown Age"),
                age = recode(age, "No Age Specified" = "Unknown Age"),
                age = recode(age, "Não especificada" = "Unknown Age"),
                age = recode(age, "NS" = "Unknown Age"),
                age = recode(age, "<1" = "<01"),
                age = replace_na(age, "Unknown Age"),
                
                
                sex = recode(sex, "UNKNOWN" = "Unknown"),
                sex = recode(sex, "Not Specified" = "Unknown"),
                sex = recode(sex, "Não especificado" = "Unknown"),
                sex = recode(sex, "F" = "Female"),
                sex = recode(sex, "M" = "Male"),
                sex = replace_na(sex, "Unknown")
                )

#---- FILTER VL LINES ONLY >0 -----------------------------------------------

df_vl_3 <- df_vl_2 %>% 
  filter(value > 0) %>% 
  mutate(indicator = "VL",
         period = {period})

#---- PROCESS TAT DATAFRAME -----------------------------------------------

df_tat_1 <- df_tat %>% 
  rename(sisma_id = `SISMA ID`,
         province = PROVINCIA,
         district = DISTRITO,
         site = US) %>% 
  pivot_longer((`COLHEITA À RECEPÇÃO`:`ANÁLISE À VALIDAÇÃO`), names_to = "tat_step", values_to = "value") %>% 
  mutate(tat_step = recode(tat_step, 
                           "COLHEITA À RECEPÇÃO" = "S1: Collection to Receipt",
                           "RECEPÇÃO AO REGISTO" = "S2: Receipt to Registration",
                           "REGISTO À ANÁLISE" = "S3: Registration to Analysis",
                           "ANÁLISE À VALIDAÇÃO" = "S4: Analysis to Validation"),
         indicator = "TAT",
         period = {period})

disa_vl <- bind_rows(df_vl_3, df_tat_1) %>%
glimpse()

# CREATE VLS DATASET ------------------------------------------------------

disa_vls <- disa_vl %>% 
  filter(result == "<1000") %>% 
  mutate(indicator = "VLS")

#---- UNION VL & VLS DATAFRAMES, PIVOT WIDER AND GROUP ----------------

disa <- bind_rows(disa_vl, disa_vls) %>% 
  mutate(row = row_number(),
         tat_step = na_if(tat_step, "temp")) %>% 
  pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>% 
  group_by(period, province, district, site, DISA_ID, sisma_id, age, group, sex, motive, tat_step) %>%
  summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T),
            TAT = sum(TAT, na.rm = T)) %>%
  ungroup() %>% 
  glimpse()

#---- JOIN DISA AJUDA MAP -------------------------------

disa_meta <- disa %>% 
  left_join(disa_datim_map) %>% 
  mutate(ajuda = replace_na(ajuda, 0)) %>% 
  rename(disa_id = DISA_ID) %>% 
  relocate(c(ajuda, datim_uid), .before = disa_id)

#---- FILTER OUT ROWS WITHOUT DATIM UID AND GROUP DATA -------------------------------

disa_final <- disa_meta %>% 
  drop_na(datim_uid) %>%
  group_by(period, datim_uid, age, group, sex, motive, tat_step) %>% 
  summarise(VL = sum(VL),
            VLS = sum(VLS),
            TAT = sum(TAT)) %>%
  left_join(datim_ou_map, by = c("datim_uid" = "orgunituid")) %>% 
  mutate(support_type = case_when(
    clinical_partner == "Sustainability Sites" ~ "MISAU",
    TRUE ~ as.character("AJUDA"))) %>% 
  select(period,
         datim_uid,
         snu1,
         psnu,
         sitename,
         support_type,
         partner = clinical_partner,
         agency = clinical_funding_agency,
         age,
         group,
         sex,
         motive,
         tat_step,
         VL,
         VLS,
         TAT) %>% 
  glimpse()

sum(disa_final$VL)

# PRINT OUTPUT TO DISK ------------------------------------------------------

readr::write_tsv(
  disa_final,
  {final_output},
  na ="")
