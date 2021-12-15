#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)

rm(list = ls())

# LOAD DATASETS -----------------------------------------------------------

ajuda_meta_data <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/ajuda_site_map_fy22q1.xlsx") %>%
  select(sisma_id:`IP FY20`,
         clinical_ip = `IP FY20`) %>%
  select_all(str_to_lower) %>% 
  drop_na(sisma_id) %>% 
  glimpse()

tidy_cv <- read_csv("Data/MISAU/CV/SISMA_CV_T42020-T32021.csv") %>% 
  pivot_longer(cols = TesteCV_0.14:`SupressaoViral_15+`,
               values_to = "resultado",
               names_to = c("indicador", "idade"),
               names_sep = "_") %>% 
  drop_na(resultado) %>% 
  glimpse()


# JOIN DATASETS -----------------------------------------------------------

tidy_cv2 <- ajuda_meta_data %>% 
  left_join(tidy_cv, by = c("sisma_id" = "organisationunitid")) %>% 
  select(-c(periodname:organisationunitdescription)) %>% 
  mutate(idade = recode(idade, "0.14" = "0-14")) %>% 
  glimpse()

tidy_cv3 <- tidy_cv2 %>% 
  pivot_wider(names_from = indicador,
              values_from = resultado,
              values_fill = 0) %>% 
  select(-c(`NA`)) %>% 
  glimpse()


readr::write_csv(
  tidy_cv2,
  "Dataout/misau_cv.csv",
  na ="")

readr::write_csv(
  tidy_cv3,
  "Dataout/misau_cv.csv",
  na ="")
