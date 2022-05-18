#  LOAD CORE TIDYVERSE & OTHER PACKAGES-----------------------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)


# DEFINE PERIODS AND SET PATH - NEEDS UPDATED EVERY MONTH AND QUARTER -----------------------------------------------------------------------------------


month <- "01/04/2022" # UPDATE
monthly_dataset <- "Dataout/PrEP/_CompileHistoric/PrEP_2022_04.txt" # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK
prep_submission <- "Data/Ajuda/PrEP/Monthly/PrEP_Monthly Report_APRIL_2022_V2_ECHO.xlsx"

#date_open <- "2021-07-01" # ONLY NEEDED FOR CIRG
#date_close <- "2021-09-01" # ONLY NEEDED FOR CIRG
#date_cirg <- "FY21 Q4" # ONLY NEEDED FOR CIRG
#input <- "C:/Users/josep/Documents/R/r_projects/Hfr"


# DEFINE PATHS AND OUTPUT NAMES - DOES NOT NEED UPDATING -----------------------------------------------------------------


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


prep_submission <- read_excel({prep_submission}, sheet = "PrEP_Monthly report", .name_repair = "universal") %>%
  glimpse()


historic_files_path <- "Dataout/PrEP/_CompileHistoric/"  # PATH USED TO CREATE A LIST OF ALL .CSV FILES PREVIOUSLY CREATED

historic_dataset <- "Dataout/em_prep.txt"


# PROCESS DATASET & CREATE ENHANCED MONITORING DATAFRAME -----------------------------------------------------------------------------------


em_prep_base <- prep_submission %>%
  tidyr::pivot_longer(cols= !c(Site, DATIM_code, Month), names_to = "indicator", values_to = "value") %>% 
  dplyr::mutate(
    Month = {month},
    sex = dplyr::case_when(stringr::str_detect(indicator, "_female") ~ "Female",
                           stringr::str_detect(indicator, "_male") ~ "Male"),
    age = dplyr::case_when(stringr::str_detect(indicator, "_15_19") ~ "15-19",
                           stringr::str_detect(indicator, "_20_24") ~ "20-24",
                           stringr::str_detect(indicator, "_25_49") ~ "25-49",
                           stringr::str_detect(indicator, "_50.") ~ "50+"),
    poptype = dplyr::case_when(stringr::str_detect(indicator, "PWID") ~ "PWID",
                               stringr::str_detect(indicator, "MSM") ~ "MSM",
                               stringr::str_detect(indicator, "TG") ~ "TG",
                               stringr::str_detect(indicator, "FSW") ~ "FSW",
                               stringr::str_detect(indicator, "Pregnant.breastfeeding.women") ~ "P/LW",
                               stringr::str_detect(indicator, "SDC") ~ "Serodiscordant Couples",
                               stringr::str_detect(indicator, "People.in.prison.and.other.closed.settings") ~ "Prisoners etc.",
                               stringr::str_detect(indicator, "AGYW") ~ "AGYW",
                               stringr::str_detect(indicator, "Custom") ~ "Other"),
    indicator_2 = dplyr::case_when(stringr::str_detect(indicator, "Number.of.clients.HIV.tested.for.PrEP.initiation") ~ "PrEP_SCREEN",
                                   stringr::str_detect(indicator, "Number.of.clients.screened.for.initiation.testing.negative") ~ "PrEP_ELIGIBLE",
                                   stringr::str_detect(indicator, "Number.of.clients.initiating.PrEP.for.the.first.time") ~ "PrEP_NEW_VERIFY",
                                   stringr::str_detect(indicator, "Number.of.clients.returning.for.1.month..initial") ~ "PrEP_1MONTH",
                                   stringr::str_detect(indicator, "Number.of.clients.returning.for.any.subsequent.follow.up.visits") ~ "PrEP_RETURN_ALL",
                                   stringr::str_detect(indicator, "Number.of.clients.restarting.PrEP") ~ "PrEP_RESTART",
                                   stringr::str_detect(indicator, "Number.of.seroconversions") ~ "PrEP_SEROCON"),
    agecoarse = dplyr::case_when(age == "<1" ~ "<15",
                                 age == "1-9" ~ "<15",
                                 age == "10-14" ~ "<15"),
    agecoarse = replace_na(agecoarse, "15+")
  ) %>%
  tidyr::drop_na(indicator_2) %>%
  dplyr::select(-c(indicator)) %>%
  dplyr::left_join(ajuda_site_map, c("DATIM_code" = "datim_uid")) %>%
  dplyr::select(c(period = Month, 
                  datim_uid = DATIM_code,
                  sisma_uid,
                  snu,
                  psnu, 
                  sitename,
                  site_nid,
                  indicator = indicator_2,
                  sex, 
                  age, 
                  age_coarse = agecoarse, 
                  pop_type = poptype, 
                  value)) %>% 
  glimpse()

em_prep <- em_prep_base %>% 
  dplyr::group_by(period, datim_uid, sisma_uid, snu, psnu, sitename, site_nid, indicator, sex, age, age_coarse, pop_type) %>% 
  summarize_at(vars(value), sum, na.rm = TRUE) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
  glimpse()


#---- WRITE MONTHLY PrEP CSV TO DISK -----------------------


readr::write_tsv(
  em_prep,
  {monthly_dataset})


#---- DEFINE PATH AND SURVEY MONTHLY PrEP DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE


#---- ROW BIND ALL HISTORIC FILES AND GENERATE OUTPUT (THIS OUTPUT IS INTENDED FOR USE WITH THE USAID MONTHLY AJUDA DASHBOARD) -----------------------

prep_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>%
  select(!c(sisma_uid, snu, psnu, sitename, site_nid)) %>% 
  left_join(ajuda_site_map) %>% 
  mutate(period = dmy(period)) %>% 
  relocate(sisma_uid:longitude, .after = datim_uid) %>% 
  glimpse()

#-----------------------------------------------------------------------------------
# PRINT DATAFRAME TO DISK


write_tsv(
  prep_tidy_history,
  {historic_dataset})


# GGPLOT VISUALS -----------------------------------------------------------------------------------


em_prep_new <- prep_tidy_history %>% 
  tidyr::drop_na(pop_type) %>% 
  dplyr::group_by(pop_type, period) %>% 
  summarize(PrEP_NEW_VERIFY = sum(PrEP_NEW_VERIFY))

ggplot(em_prep_new, aes(period, PrEP_NEW_VERIFY, fill = pop_type)) +
  geom_col() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 9),
        axis.text = element_text(size = 10),
        legend.position="right", legend.direction = "vertical") +
  labs(title = "PrEP_NEW Trend",
       caption = "Data source: ECHO Monthly PrEP Reporting")


# SUBSET DATAFRAME TO CREATE HFR SUBMISSION -----------------------------------------------------------------------------------


hfr_prep <- em_prep_base %>%
  dplyr::rename(val = value) %>%
  dplyr::mutate(operatingunit = "Mozambique",
                mech_code = "70212",
                partner = "ECHO",
                otherdisaggregate = "",
                indicator= dplyr::recode(indicator, "PrEP_NEW_VERIFY" = "PrEP_NEW")) %>%
  dplyr::filter(!is.na(age),
                indicator == "PrEP_NEW",
                date == as.Date(period)) %>% 
  dplyr::select(c(date, 
                  orgunit = site, 
                  orgunituid, 
                  mech_code, 
                  partner, 
                  operatingunit, 
                  psnu = district, 
                  indicator, 
                  sex, 
                  agecoarse, 
                  otherdisaggregate, 
                  val))

# SUBSET DATAFRAME TO CREATE CIRG SUBMISSION -----------------------------------------------------------------------------------

cirg_prep <- em_prep_base %>% 
  dplyr::filter(date >= as.Date(date_open) & date <= as.Date(date_close)) %>% 
  dplyr::filter(!indicator %in% "PrEP_RETURN_ALL") %>% 
  dplyr::rename(val = value,
                reportingperiod = date,
                orgunit = site,
                psnu = district,
                population = poptype) %>%
  dplyr::mutate(operatingunit = "Mozambique",
                otherdisaggregate = "",
                mech_code = "70212",
                partner = "ECHO",
                reportingperiod = date_cirg,
                numdenom = "N",
                population = dplyr::recode(population, 
                                           "Serodiscordant Couples" = "Non-KP (general population)", 
                                           "AGYW" = "Non-KP (seronegative persons in serodifferent partnerships)",
                                           "MSM" = "Men who have sex with men (MSM)",
                                           "FSW" = "Female sex workers (FSW)"),
                age = dplyr::recode(age, "25-49"= "Unknown"),
                age = if_else(indicator == "PrEP_1MONTH", "Unknown", age),
                sex = if_else(indicator == "PrEP_1MONTH", "Unknown", sex)
                #age = dplyr::case_when(indicator == "PrEP_1MONTH" ~ "Unknown"),
                #sex = dplyr::case_when(indicator == "PrEP_1MONTH" ~ "Unknown")
  ) %>% 
  dplyr::select(reportingperiod, 
                orgunit, 
                orgunituid, 
                mech_code, 
                partner, 
                operatingunit, 
                psnu, 
                indicator, 
                sex, 
                age, 
                population, 
                otherdisaggregate, 
                numdenom, 
                val) %>% 
  group_by(reportingperiod, orgunit, orgunituid, mech_code, partner, operatingunit, psnu, indicator, sex, age, population, otherdisaggregate, numdenom) %>% 
  summarize(val = sum(val)) %>% 
  ungroup()



openxlsx::write.xlsx(hfr_prep, file = "Dataout/HFR/hfr_prep.xlsx", sheetName = "hfr_prep")
openxlsx::write.xlsx(cirg_prep, file = "Dataout/CIRG/cirg_prep.xlsx", sheetName = "cirg_prep")
