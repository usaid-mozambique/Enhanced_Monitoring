
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
period <- "FY21"

#---- DEFINE PATHS AND VALUES - COULD REQUIRE UPDATING EACH MONTH -------------------------------------------------------

final_output <- "Dataout/DISA_annual/disa_fy21.txt"

#---- LOAD DATASETS AND UNION -------------------------------------------------------

ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx") %>% 
  mutate(site_nid = as.character(site_nid))

disa_datim_map <- read_excel("Documents/disa_datim_map.xlsx")


xAge <- read_excel({file_monthly}, 
                   sheet = "Age & Sex", 
                   col_types = c("text", "text", "text", 
                                 "text", "text", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"), 
                   skip = 2) %>% 
  dplyr::mutate(group = "Age") %>%
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
  dplyr::mutate(group = "PW") %>% 
  dplyr::rename(US = HF,
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
  dplyr::mutate(group = "LW") %>%
  dplyr::rename(US = HF,
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

df_vl <- dplyr::bind_rows(xAge, xPW, xLW)

rm(xAge, xPW, xLW)

#---- PROCESS VL DATAFRAME -------------------------------------------------------

df_vl_1 <- df_vl %>% 
  dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  dplyr::rename(sisma_id = `SISMA ID`,
                province = PROVINCIA,
                district = DISTRITO,
                site = US,
                age = Age,
                sex = Sex) %>% 
  dplyr::relocate(c(group), .before = site) %>% 
  tidyr::pivot_longer(`Rotina (<1000)`:`Motivo de Teste não especificado (>1000)`, names_to = "indicator", values_to = "value") %>% 
  dplyr::mutate(motive = dplyr::case_when(grepl("Rotina", indicator) ~ "Routine",
                                          grepl("Fal", indicator) ~ "Theraputic Failure",
                                          grepl("Repetir", indicator) ~ "Post Breastfeeding",
                                          grepl("Motivo de Teste NS", indicator) ~ "Not Specified"),
                result = dplyr::case_when(grepl("<1000", indicator) ~ "<1000",
                                          grepl(">1000", indicator) ~ ">1000"),
                tat_step = "temp") %>% 
  dplyr::select(-c(indicator)) %>% 
  glimpse()

#---- RECODE VL AGE/SEX VALUES -----------------------------------------------

df_vl_2 <- df_vl_1 %>% 
  dplyr::mutate(age = dplyr::recode(age, "Idade não especificada" = "Unknown"),
                age = dplyr::recode(age, "No Age Specified" = "Unknown"),
                age = dplyr::recode(age, "Não especificada" = "Unknown"),
                age = tidyr::replace_na(age, "Unknown"),
                
                sex = dplyr::recode(sex, "UNKNOWN" = "Unknown"),
                sex = dplyr::recode(sex, "Not Specified" = "Unknown"),
                sex = dplyr::recode(sex, "Não especificado" = "Unknown"),
                sex = dplyr::recode(sex, "F" = "Female"),
                sex = dplyr::recode(sex, "M" = "Male"),
                sex = tidyr::replace_na(sex, "Unknown"))

#---- FILTER VL LINES ONLY >0 -----------------------------------------------

df_vl_3 <- df_vl_2 %>% 
  dplyr::filter(value > 0) %>% 
  dplyr::mutate(indicator = "VL",
                period = {period})

#---- PROCESS TAT DATAFRAME -----------------------------------------------

df_tat_1 <- df_tat %>% 
  dplyr::rename(sisma_id = `SISMA ID`,
                province = PROVINCIA,
                district = DISTRITO,
                site = US) %>% 
  tidyr::pivot_longer((`COLHEITA À RECEPÇÃO`:`ANÁLISE À VALIDAÇÃO`), names_to = "tat_step", values_to = "value") %>% 
  dplyr::mutate(tat_step = dplyr::recode(tat_step, 
                                         "COLHEITA À RECEPÇÃO" = "S1: Collection to Receipt",
                                         "RECEPÇÃO AO REGISTO" = "S2: Receipt to Registration",
                                         "REGISTO À ANÁLISE" = "S3: Registration to Analysis",
                                         "ANÁLISE À VALIDAÇÃO" = "S4: Analysis to Validation"),
                indicator = "TAT",
                period = {period})

disa_vl <- dplyr::bind_rows(df_vl_3, df_tat_1) %>% 
  glimpse()

# CREATE VLS DATASET ------------------------------------------------------

disa_vls <- disa_vl %>% 
  dplyr::filter(result == "<1000") %>% 
  dplyr::mutate(indicator = "VLS")

#---- UNION VL & VLS DATAFRAMES, PIVOT WIDER AND GROUP ----------------

disa <- dplyr::bind_rows(disa_vl, disa_vls) %>% 
  dplyr::mutate(row = row_number(),
                tat_step = na_if(tat_step, "temp")) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>% 
  dplyr::group_by(period, province, district, site, sisma_id, age, group, sex, motive, tat_step) %>%
  summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T),
            TAT = sum(TAT, na.rm = T)) %>%
  ungroup() %>% 
  glimpse()


test <- disa %>% 
  filter(group == "Age",
         motive == "Routine")

sum(test$VL)

#---- JOIN DISA AJUDA MAP -------------------------------

disa_final <- disa %>% 
  left_join(disa_datim_map) %>% 
  mutate(ajuda = replace_na(ajuda, 0)) %>% 
  rename(disa_id = DISA_ID) %>% 
  relocate(c(ajuda, datim_uid), .before = disa_id) %>% 
  glimpse()


# PRINT OUTPUT TO DISK ------------------------------------------------------

readr::write_tsv(
  disa_final,
  {final_output},
  na ="")
