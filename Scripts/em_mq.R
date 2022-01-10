
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

# DEFINE REPORTING MONTH AND FILE PATHS -------------------------------------------

month <- "2021-12-20" # UPDATE EVERY MONTH
monthly_dataset <- ("Data/Ajuda/ER_DSD_TPT_VL/VL/_CompileHistoric/VL_2021_12.csv") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA_Site_Map_20211206.xlsx"
echo <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/Monitoria Intensiva_ Template_FY22 12_20_2021_updated_ECHO.xlsx"
fgh <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/FGH_DEC_21_Monitoria Intensiva Template FY22_122021_Updated_ January 6_2022.xlsx"

# LOAD DATASETS -----------------------------------------------------------

ajuda_site_map <- read_excel(ajuda_path) %>% 
  select(orgunituid,
         SNU,
         Psnu, 
         Sitename,
         partner = `IP FY20`) %>% 
  select_all(str_to_lower)

# CREATE FUNCTION TO TIDY CV DATASETS -------------------------------------

cv_tidy <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "Monitoria Intensiva", 
                   skip = 9,
                   col_types = c("text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric")
                   ) %>%
  pivot_longer('dpi.colheu.pcr_d_total':'mds.cv.supressao_prop_mds', 
               names_to = c("indicator", "numdenom", "pop_type", "age"), 
               names_sep = "_", 
               values_to = "value") %>% 
    filter(!numdenom == "prop",
           !pop_type == "total") %>% 
    mutate(age = recode(age,
                        "menor2" = "<2",
                        "0.2" = "0-2",
                        "0.4" = "0-4",
                        "5.9" = "5-9",
                        "10.14" = "10-14",
                        "0.14" = "0-14",
                        "1.14" = "1-14",
                        "2.14" = "2-14"),
           numdenom = recode(numdenom,
                             "n" = "N",
                             "d" = "D"),
           pop_type = recode(pop_type,
                             "all" = "All",
                             "mg" = "MG",
                             "mds" = "MDS"),
           indicator = paste0(indicator,
                              if_else(numdenom %in% c("D"), "_D", "")),
           month ={month}) %>% 
    filter(Partner == ip)
  
}

# IMPORT & RESHAPE cv SUBMISSIONS -----------------------------------------------------------

echo <- cv_tidy(echo, "ECHO")
fgh <- cv_tidy(fgh, "FGH")

# COMPILE IP SUMBISSIONS --------------------------------------------------

cv_tidy <- dplyr::bind_rows(echo, fgh)

cv_tidy %>% # TABLE HF SUBMISSION LINES BY PARTNER.  CAUTION - BLANK SUBMISSION LINES ARE INCLUDED!
  group_by(Partner) %>% 
  distinct(DATIM_code) %>% 
  summarise(n())

rm(echo, fgh)

# WRITE MONTHLY TPT CSV TO DISK ------------------------------------

readr::write_csv(
  cv_tidy,
  {monthly_dataset})

# TEMPORARY WORKAROUND ----------------------------------------------------

cv_compile <- read_csv("Data/Ajuda/ER_DSD_TPT_VL/VL/_CompileHistoric/manual_compile/VL_2021_12_12.csv") %>% 
  select(-c(No,
            Type,
            Data,
            Partner,
            Province,
            District, 
            `Health Facility`)) %>% 
  left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  rename(orgunituid = DATIM_code,
         sisma_id = SISMA_code,
         province = snu,
         district = psnu,
         site = sitename) %>% 
  relocate(month:partner, .after = sisma_id) %>% 
  glimpse()

write_tsv(
  cv_compile,
  "Dataout/em_cv.txt")

