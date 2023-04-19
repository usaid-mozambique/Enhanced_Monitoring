
rm(list = ls())

# PURPOSE:
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-09-11
# NOTES: 

# DEPENDENCIES & SETUP -----------------


library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(googlesheets4)
load_secrets()

# PATHS --------------------------------


path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets

path_novos_tarv <- "Data/MISAU/CT/novos_tarv.csv"


# SITE REMOVAL LIST -------------------------------------------------------

site_removal_list <- "abg5UReivZX"

# LOAD DATA ----------------------------

ajuda_site_map <- read_sheet(path_ajuda_site_map) %>%
  select(sisma_uid = sisma_id,
         datim_uid =  orgunituid,
         site_nid,
         partner = `IP FY20`,
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

novos_tarv <- read_csv({path_novos_tarv}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

# MUNGE --------------------------------

novos_tarv_1 <- novos_tarv %>% 
  select(-c(periodname,
            periodid,
            perioddescription,
            organisationunitcode,
            organisationunitdescription,
            orgunitlevel1,
            organisationunitname
  )) %>%
  gather(indicator, value, -c(periodcode,
                              orgunitlevel2,
                              orgunitlevel3,
                              orgunitlevel4,
                              organisationunitid), na.rm = TRUE) %>% 
  mutate(sex = case_when(str_detect(indicator, " - F ") ~ "Female",
                         str_detect(indicator, " - M ") ~ "Male",
                         TRUE ~ "Unknown"),
         age = case_when(str_detect(indicator, "0-4") ~ "0-4",
                         str_detect(indicator, "5 - 9") ~ "5-9",
                         str_detect(indicator, "10 - 14") ~ "10-14",
                         str_detect(indicator, "20+") ~ "20+"),
         indicator = "TARV_NOVOS",
         period = paste0(periodcode, "01"),
         period = ymd(period)) %>%
  rename(sisma_uid = organisationunitid,
         snu = orgunitlevel2,
         psnu = orgunitlevel3,
         sitename = orgunitlevel4) %>% 
  mutate(snu = str_to_title(snu),
         psnu = str_to_title(psnu)) %>%  
  select(sisma_uid, snu, psnu, sitename, period, indicator, sex, age, value) %>% 
  glimpse()


novos_tarv_2 <- novos_tarv_1 %>%
  left_join(ajuda_site_map, by = "sisma_uid") %>%
  select(sisma_uid,
         period,
         partner,
         snu,
         psnu,
         sitename,
         starts_with("support"),
         starts_with("his"),
         indicator,
         sex,
         age,
         value) %>% 
  mutate(partner = replace_na(partner, "MISAU")) %>% 
  glimpse()
         

sum(novos_tarv_2$value)
# ANALYTICS ----------------------------

# SAVE TO DISK -------------------------

readr::write_tsv(
  novos_tarv_2,
  "Dataout/em_novos_tarv.txt")
