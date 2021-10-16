#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES
library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)

rm(list = ls())

#-----------------------------------------------------------------------------------
# DEFINE HFR & CIRG PERIOD AND SET PATH

date_hfr <- "2021-09-01" # HFR PERIOD
date_open <- "2021-08-21"
date_close <- "2021-09-20"


date_cirg <- "FY21 Q3" # ONLY REQUIRED IF PRODUCING CIRG
quarter_open <- "2021-06-21"
quarter_close <- "2021-09-20"

#-----------------------------------------------------------------------------------
# IMPORT DATASET

Reporte_Diario_VMMC_USAID <- read_excel("Data/Jhpeigo/Reporte_Diario_VMMC_USAID.xlsx", .name_repair = "universal") # UTF ENCODING PRESERVES VARIABLE NAMES

#-----------------------------------------------------------------------------------
# GENERATE PRIMARY VMMC DATASET

em_vmmc <- Reporte_Diario_VMMC_USAID %>%
  dplyr::select(-c(FACILITY.OR.COMMUNITY.NAME...2,
                   CM9_Total.CMs.US,
                   P1_Total.CM.positivos....,
                   N1_.Total.CM.Negativos....,
                   NT__Total.Ñ.Testado,
                   Total.Rast_COVID,
                   Observação)
  ) %>%
  dplyr::mutate(T2_15.19.anos = P2_15.19.anos + N2_15.19.anos,
                T3_20.24.anos = P3_20.24.anos + N3_20.24.anos,
                T4_25.29.anos = P4_25.29.anos + N4_25.29.anos,
                T5_30.34.anos = P5_30.34.anos + N5_30.34.anos,
                T6_35.39.anos = P6_35.39.anos + N6_35.39.anos,
                T7_40.49.anos = P7_40.49.anos + N7_40.49.anos,
                T8_.50.anos = P8_.50.anos + N8_.50.anos
  ) %>%
  tidyr::gather(indicator, value, CM1_10.14.anos:T8_.50.anos ) %>%
  dplyr::mutate(
    otherdisaggregate = "",
    sex = "Male",
    operatingunit = "Mozambique",
    partner = "Jhpiego",
    agefine = dplyr::case_when(grepl("_10.14", indicator) ~ "10-14",
                               grepl("_15.19", indicator) ~ "15-19",
                               grepl("_20.24", indicator) ~ "20-24",
                               grepl("_25.29", indicator) ~ "25-29",
                               grepl("_30.34", indicator) ~ "30-34",
                               grepl("_35.39", indicator) ~ "35-39",
                               grepl("_40.49", indicator) ~ "40-49",
                               grepl("50.anos", indicator) ~ "50+"),
    agecoarse = dplyr::case_when(grepl("_10.14", indicator) ~ "<15",
                                 grepl("_15.19", indicator) ~ "15+",
                                 grepl("_20.24", indicator) ~ "15+",
                                 grepl("_25.29", indicator) ~ "15+",
                                 grepl("_30.34", indicator) ~ "15+",
                                 grepl("_35.39", indicator) ~ "15+",
                                 grepl("_40.49", indicator) ~ "15+",
                                 grepl("50.anos", indicator) ~ "15+"),
    ae_severity = dplyr::case_when(grepl("Ligeira", indicator) ~ "Mild",
                                   grepl("Moderada", indicator) ~ "Moderate",
                                   grepl("Severa", indicator) ~ "Severe"),
    indicator_final = dplyr::case_when(grepl("T2_15.19.anos", indicator) ~ "HTS_TST",
                                       grepl("T3_20.24.anos", indicator) ~ "HTS_TST",
                                       grepl("T4_25.29.anos", indicator) ~ "HTS_TST",
                                       grepl("T5_30.34.anos", indicator) ~ "HTS_TST",
                                       grepl("T6_35.39.anos", indicator) ~ "HTS_TST",
                                       grepl("T7_40.49.anos", indicator) ~ "HTS_TST",
                                       grepl("T8", indicator) ~ "HTS_TST",
                                       grepl("P2_15.19.anos", indicator) ~ "HTS_TST_POS",
                                       grepl("P3_20.24.anos", indicator) ~ "HTS_TST_POS",
                                       grepl("P4_25.29.anos", indicator) ~ "HTS_TST_POS",
                                       grepl("P5_30.34.anos", indicator) ~ "HTS_TST_POS",
                                       grepl("P6_35.39.anos", indicator) ~ "HTS_TST_POS",
                                       grepl("P7_40.49.anos", indicator) ~ "HTS_TST_POS",
                                       grepl("P8", indicator) ~ "HTS_TST_POS",
                                       grepl("CM1_10.14.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM2_15.19.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM3_20.24.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM4_25.29.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM5_30.34.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM6_35.39.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM7_40.49.anos", indicator) ~ "VMMC_CIRC",
                                       grepl("CM8", indicator) ~ "VMMC_CIRC",
                                       grepl("Complicação", indicator) ~ "VMMC_AE")
  ) %>%
  dplyr::rename(date = Data,
                mech_code = MECHANISM.ID,
                psnu = PSNU.Distrito.,
                snu = Provincia,
                orgunit = FACILITY.OR.COMMUNITY.NAME...6,
                orgunituid = FACILITY.OR.COMMUNITY.UID,
                unittype = Tipo.de.Unidade,
                indicator_all = indicator,
                indicator = indicator_final
  ) %>%
  dplyr::filter(value != 0) %>%
  drop_na(orgunituid) # CAREFUL WITH THIS PARTICULAR LINE.  IT COULD DROP DATA UNINTENTIONALLY

#----- GENERATE HFR VMMC DATASET -----------------------------------------------------

hfr_vmmc <- em_vmmc %>%
  drop_na(indicator) %>%
  dplyr::rename(val = value) %>%
  dplyr::filter(date >= as.Date(date_open) & date <= as.Date(date_close),
                indicator != "VMMC_AE") %>%
  dplyr::mutate(date = date_hfr) %>%
  dplyr::select(date, orgunit, orgunituid, mech_code, partner, operatingunit, psnu, indicator, sex, agecoarse, otherdisaggregate, val)

#----- GENERATE CIRG VMMC DATASET ----------------------------------------------------

cirg_vmmc <- em_vmmc %>%
  dplyr::filter(indicator == "VMMC_AE") %>% 
  dplyr::filter(date >= as.Date(quarter_open) & date <= as.Date(quarter_close)) %>% 
  dplyr::mutate(reportingperiod = {date_cirg},
                age = "Unknown",
                population = "",
                otherdisaggregate = ae_severity, # NEED TO ADD RECODE LINE TO MAKE SURE MILD AND BLANK ARE SET TO UNKNOWN (OR SOMETHING LIKE THAT)
                numdenom = "N") %>%
  dplyr::rename(val = value) %>%
  dplyr::select(reportingperiod, orgunit, orgunituid, mech_code, partner, operatingunit, psnu, indicator, sex, age, population, otherdisaggregate, numdenom, val)

#-----------------------------------------------------------------------------------
# PRINT EXCEL DOCUMENTS TO COMPUTER

readr::write_tsv(
  em_vmmc,
  "Dataout/em_vmmc.txt",
  na ="")

openxlsx::write.xlsx(hfr_vmmc, file = "Dataout/HFR/hfr_vmmc.xlsx", sheetName = "hfr_vmmc")
openxlsx::write.xlsx(cirg_vmmc, file = "Dataout/CIRG/cirg_vmmc.xlsx", sheetName = "cirg_vmmc")

# TEST SUMS CODE AND PLOT DATA

em_vmmc_plot <- em_vmmc %>% 
  filter(indicator == "VMMC_CIRC") %>% 
  select(-c(mech_code, indicator_all, ae_severity, operatingunit, otherdisaggregate)) %>% 
  drop_na(value) 

ggplot(data = em_vmmc_plot) +
  geom_col(mapping = aes(y = value, x = date))


