#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES


library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

rm(list = ls())


# DEFINE MONTH AND PATHS ---------------------------


month <- "2022-02-20" # UPDATE EACH MONTH
monthly_dataset <- ("Dataout/TPT/_CompileHistoric/TPT_2022_02.csv") # UPDATE EACH MONTH

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/DOD__Fev_2022final 20022022 DOD Jhpiego Included Monitoria Intensiva de CV tab.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/ARIEL Monitoria Intensiva_ Template_FY22 12_20_2021_February.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/NON MER Indicators Template_FY22 02_20_2022 CCS.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/Monitoria Intensiva_ Template_February_2022_ECHO.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/EGPAF_Monitoria Intensiva_ Template_FY22 20_Fev_2022_updated.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/ICAP_Fevereiro2022_Monitoria Intensiva_ Template_FY22_11032022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_02/FGH_FEB_22_Monitoria Intensiva Template FY22.xlsx"

historic_files_path <- "Dataout/TPT/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH


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


# CREATE FUNCTION TPT RESHAPE ---------------------------------------------



tpt_reshape <- function(filename, ip){
  
  df <- read_excel(filename, sheet = "TB", 
                   col_types = c("numeric", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "numeric", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"),
                   skip = 7) %>%
    select(c(No,
             Partner,
             Province,
             District,
             `Health Facility`,
             DATIM_code,
             SISMA_code,
             Type,
             Period,
             TX_CURR,
             TX_CURR_TPT_Com,
             TX_CURR_TPT_Not_Comp,
             TX_CURR_TB_tto,
             TX_CURR_TPT_Not_Comp_POS_Screen,
             TX_CURR_Eleg_TPT_Comp,
             TX_CURR_W_TPT_last7Mo,
             TX_CURR_Eleg_TPT_Init)) %>%
    filter(Partner == ip)
  
}



# IMPORT & RESHAPE TPT SUBMISSIONS -------------------------------------------------


dod <- tpt_reshape(DOD, "JHPIEGO-DoD")
echo <- tpt_reshape(ECHO, "ECHO")
ariel <- tpt_reshape(ARIEL, "ARIEL")
ccs <- tpt_reshape(CCS, "CCS")
egpaf <- tpt_reshape(EGPAF, "EGPAF")
fgh <- tpt_reshape(FGH, "FGH")
icap <- tpt_reshape(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


tpt <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)

rm(dod, ariel, ccs, echo, egpaf, fgh, icap)


# CALCULATE NEW VARIABLES, PIVOT AND RENAME VARIABLES ---------------------


tpt_tidy <- tpt %>%
  mutate(TPT_candidates = TX_CURR - (TX_CURR_TPT_Com + TX_CURR_W_TPT_last7Mo) - (TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen),
         TPT_ineligible = TX_CURR_TB_tto + TX_CURR_TPT_Not_Comp_POS_Screen,
         TPT_active_complete = TX_CURR_W_TPT_last7Mo + TX_CURR_TPT_Com) %>%
  pivot_longer(TX_CURR:TPT_active_complete, names_to = "attribute", values_to = "value") %>%
  mutate(indicator = attribute) %>%
  mutate(indicator = dplyr::recode(indicator,
                                   "TX_CURR_W_TPT_last7Mo"= "Actively on TPT", # use to create new indicator
                                   "TX_CURR_TB_tto" = "Recent Active TB TX",
                                   "TX_CURR_TPT_Not_Comp_POS_Screen" = "Recent Pos TB Screen",
                                   "TX_CURR_TPT_Com" = "TPT Completed",  # use to create new indicator
                                   "TPT_candidates" = "TPT Candidates",
                                   "TPT_ineligible" = "TPT Ineligible",
                                   "TX_CURR_TPT_Not_Comp" = "TPT Not Comp",
                                   "TPT_active_complete" = "TPT Completed/Active"),
         Period = {month}
  ) %>%
  filter(!indicator %in% c("TX_CURR_Eleg_TPT_Init", "TX_CURR_Eleg_TPT_Comp")) %>%
  select(-c(No))


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


readr::write_csv(
  tpt_tidy,
  {monthly_dataset})

# 
# # TEMPORARY WORKAROUND ----------------------------------------------------
# 
# 
# temp <- read_csv("Dataout/TPT/_CompileHistoric/manual_compile/TPT_2021_2022_02.csv")
# 
# 
# #---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------
# 
# volumn_period <- temp %>% 
#   mutate(date = as.Date(Period, format =  "%y/%m/%d")) %>% 
#   select(DATIM_code, date, indicator, value) %>% 
#   filter(date == max(date),
#          indicator == "TX_CURR") %>% 
#   mutate(site_volume = case_when(
#     value < 1000 ~ "Low",
#     between(value, 1000, 5000) ~ "Medium",
#     value > 5000 ~ "High",
#     TRUE ~ "Not Reported")) %>% 
#   select(DATIM_code, site_volume) %>% 
#   glimpse()
# 
# 
# #---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------
# 
# tpt_tidy_history <- temp %>%
#   left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>% 
#   left_join(volumn_period) %>% 
#   select(datim_uid = DATIM_code,
#          sisma_uid,
#          site_nid,
#          period = Period,
#          partner,
#          snu,
#          psnu,
#          sitename,
#          site_volume,
#          ends_with("tude"),
#          starts_with("support"),
#          starts_with("his"),
#          indicator,
#          attribute,
#          value) %>% 
#   glimpse()
# 
# 
# 
# # WRITE MONTHLY OUTPUT FILE TO DISK ---------------------------------------
# 
# 
# readr::write_tsv(
#   tpt_tidy_history,
#   "Dataout/em_tpt.txt")


#---- SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.csv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

chr_files <- historic_files[historic_files %in% c("TPT_2021_03.csv", "TPT_2021_04.csv")]
non_chr_files <- historic_files[historic_files %ni% c("TPT_2021_03.csv", "TPT_2021_04.csv")]

temp_chr_files <- chr_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>% 
  mutate(Period = as.Date(Period, "%d/%m/%Y")) 

temp_non_chr_files <- non_chr_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind) 

tpt_tidy_history <- bind_rows(temp_chr_files, temp_non_chr_files)


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- tpt_tidy_history %>% 
  mutate(date = as.Date(Period, format =  "%y/%m/%d")) %>% 
  select(DATIM_code, date, indicator, value) %>% 
  filter(date == max(date),
         indicator == "TX_CURR") %>% 
  mutate(site_volume = case_when(
    value < 1000 ~ "Low",
    between(value, 1000, 5000) ~ "Medium",
    value > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(DATIM_code, site_volume) %>% 
  glimpse()


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


tpt_tidy_history_2 <- tpt_tidy_history %>%
  left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>% 
  left_join(volumn_period) %>% 
  select(datim_uid = DATIM_code,
         sisma_uid,
         site_nid,
         period = Period,
         partner,
         snu,
         psnu,
         sitename,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         indicator,
         attribute,
         value) %>% 
  glimpse()


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------

readr::write_tsv(
  tpt_tidy_history_2,
  "Dataout/em_tpt.txt")


