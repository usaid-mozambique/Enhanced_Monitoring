
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

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA_Site_Map_20211206.xlsx"
df_path <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/NON MER Indicators Template_FY22 12_20_2021a.xlsx"

# LOAD DATASETS -----------------------------------------------------------

df <- read_excel({df_path}, 
                   sheet = "Monitoria Intensiva de CV", 
                   skip = 9)

ajuda <- read_excel(ajuda_path) %>% 
  select(orgunituid,
         SNU,
         Psnu, 
         Sitename) %>% 
  select_all(str_to_lower)

# CREATE FUNCTION TO TIDY CV DATASETS -------------------------------------

cv_reshape <- function(filename, ip){
  
  df <- read_excel({df_path}, 
                   sheet = "Monitoria Intensiva de CV", 
                   skip = 9) %>%
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

# FUNCTION TEST -----------------------------------------------------------

df_test <- cv_reshape(df, "ARIEL")

# PRINT TO TO DISK ----------------------------------------------------------

readr::write_tsv(
  df_test,
  "Dataout/mi_cv_test.txt")

