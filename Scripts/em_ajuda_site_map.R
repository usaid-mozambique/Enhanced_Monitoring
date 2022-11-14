rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(glamr)
library(googlesheets4)
library(googledrive)
library(fs)
library(lubridate)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(gt)
load_secrets() 


# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets


# METADATA -----------------------------------------------------------


df_ajuda <- read_sheet(path_ajuda_site_map, sheet = "ajuda_map") %>% 
  left_join(read_sheet(path_ajuda_site_map, sheet = "ajuda_add_by_cop") %>% select(datim_uid, cop_entry), by = "datim_uid") %>% 
  left_join(read_sheet(path_ajuda_site_map, sheet = "parnter_alt") %>% select(datim_uid, partner_alt), by = "datim_uid") %>% 
  mutate(cop_entry = replace_na(cop_entry, "COP19")) %>% 
  relocate(partner_alt, .after = partner_pepfar) %>% 
  relocate(cop_entry, .before = partner_pepfar)

# SAVE TO DISK -------------------------

write.xlsx(df_ajuda, file = "Dataout/AJUDA_Site_Map/ajuda_site_map.xlsx", sheetName = "sheet1", append = TRUE)
