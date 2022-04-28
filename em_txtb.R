#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES


library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(gt)

rm(list = ls())


# DEFINE MONTH AND PATHS ---------------------------


month <- "20/03/2022" # UPDATE EACH MONTH
monthly_dataset <- ("Dataout/TXTB/_CompileHistoric/TXTB_2022_03.csv") # UPDATE EACH MONTH


ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/icap_test.xlsx"


historic_files_path <- "Dataout/TXTB/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH


# LOAD METADATA -----------------------------------------------------------


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


# CREATE FUNCTION TXTB RESHAPE ---------------------------------------------


txtb_reshape <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "TX_TB", col_types = c("numeric", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "text", "text", "text", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), skip = 7) %>% 
    filter(partner == ip) %>%  
    select(!c(contains(c("remove", "tot")))) %>%
    pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk', 
                 names_to = c("indicator", "disaggregate", "sex", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    mutate(period = as.Date(month, "%d/%m/%Y"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = recode(age, "Unk" = "Unknown")) %>% 
    pivot_wider(names_from =  indicator, values_from = value) %>%
    glimpse()
  
}


# IMPORT & RESHAPE TXTB SUBMISSIONS -------------------------------------------------


icap <- txtb_reshape(ICAP, "ICAP")








