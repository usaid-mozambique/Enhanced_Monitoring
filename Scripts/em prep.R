#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)

# DEFINE HFR PERIOD AND SET PATH -----------------------------------------------------------------------------------
# 

month <- "2021-06-01"
date_open <- "2021-04-01" # ONLY NEEDED FOR CIRG
date_close <- "2021-06-01" # ONLY NEEDED FOR CIRG
date_cirg <- "FY21 Q3" # ONLY NEEDED FOR CIRG
input <- "C:/Users/josep/Documents/R/r_projects/Hfr"

# IMPORT DATASET -----------------------------------------------------------------------------------
# 
# DATASET SOURCE: MONTHLY ECHO PREP SUBMISSION THAT ACCOMPANIES ER/DSD SUBMISSION

prep_compile <- read_excel("Data/Ajuda/PrEP/prep_compile.xlsx", .name_repair = "universal") %>%
  glimpse()

ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/AJUDA Site Map.xlsx") %>%
  dplyr::select(orgunituid, SNU, Psnu, Sitename) %>%
  dplyr::rename(psnu = Psnu,
                orgunit = Sitename,
                snu = SNU)

ajuda_site_map_2 <- read_excel("~/GitHub/AJUDA_Site_Map/AJUDA Site Map.xlsx") %>% 
  dplyr::select(-c(sisma_id, SNU, Psnu, Sitename)) %>% 
  dplyr::rename(partner = `IP FY20`)

# PROCESS DATASET & CREATE ENHANCED MONITORING DATAFRAME -----------------------------------------------------------------------------------
# 

em_prep_base <- prep_compile %>%
  tidyr::pivot_longer(cols= !c(Site, DATIM_Code, Month), names_to = "indicator", values_to = "value") %>% 
  dplyr::mutate(
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
  dplyr::filter(value != 0) %>%
  dplyr::select(-c(indicator)) %>%
  dplyr::left_join(ajuda_site_map, c("DATIM_Code" = "orgunituid")) %>%
  dplyr::select(c(date = Month, 
                  site = orgunit,
                  orgunituid = DATIM_Code,
                  district = psnu, 
                  province = snu,
                  indicator = indicator_2,
                  sex, 
                  age, 
                  agecoarse, 
                  poptype, 
                  value))

em_prep <- em_prep_base %>% 
  dplyr::group_by(date, site, orgunituid, district, province, indicator, sex, age, agecoarse, poptype) %>% 
  summarize_at(vars(value), sum, na.rm = TRUE) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
  dplyr::left_join(ajuda_site_map_2)

# SUBSET DATAFRAME TO CREATE HFR SUBMISSION -----------------------------------------------------------------------------------
# 

hfr_prep <- em_prep_base %>%
  dplyr::rename(val = value) %>%
  dplyr::mutate(operatingunit = "Mozambique",
                mech_code = "70212",
                partner = "ECHO",
                otherdisaggregate = "",
                indicator= dplyr::recode(indicator, "PrEP_NEW_VERIFY" = "PrEP_NEW")) %>%
  dplyr::filter(!is.na(age),
                indicator == "PrEP_NEW",
                date == as.Date(month)) %>% 
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

# SUBSET DATAFRAME TO CREATE CI SUBMISSION -----------------------------------------------------------------------------------

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


# PRINT EXCEL DOCUMENTS TO COMPUTER -----------------------------------------------------------------------------------
# 

readr::write_tsv(
  em_prep,
  "Dataout/em_prep.txt",
  na ="")

openxlsx::write.xlsx(em_prep, file = "Dataout/em_prep.xlsx", sheetName = "em_prep")
openxlsx::write.xlsx(hfr_prep, file = "~/R/r_projects/Hfr/output/hfr_prep.xlsx", sheetName = "hfr_prep")
openxlsx::write.xlsx(cirg_prep, file = "~/R/r_projects/Hfr/output/cirg_prep.xlsx", sheetName = "cirg_prep")

# GGPLOT VISUALS -----------------------------------------------------------------------------------
# 

em_prep_new <- em_prep_base %>% 
  dplyr::filter(indicator == "PrEP_NEW_VERIFY") %>% 
  tidyr::drop_na(poptype) %>% 
  dplyr::group_by(poptype, date) %>% 
  summarize(value = sum(value))

ggplot(em_prep_new, aes(date, value, fill = poptype)) +
  geom_col() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 9),
        axis.text = element_text(size = 10),
        legend.position="right", legend.direction = "vertical") +
  labs(title = "PrEP_NEW Trend",
       caption = "Data source: ECHO Monthly PrEP Reporting")

