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
monthly_dataset <- ("Dataout/TXTB/_CompileHistoric/TXTB_2022_03.txt") # UPDATE EACH MONTH


DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/DOD__Mar_2022 final 20122021 DOD Jhpiego Included Monitoria Intensiva new Template.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ARIEL Monitoria Intensiva_ Template_FY22Q2 21.04.2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/CCS_Monitoria Intensiva_ Template_FY22Q2.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/Monitoria Intensiva_ Template_FY22Q2_ECHO.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/EGPAF_Monitoria Intensiva_ Template_FY22Q2 Marco_2022_versao 2.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ICAP_Marco2022_Monitoria Intensiva_ Template_FY22Q2_Update18042022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/Monitoria Intensiva_ Template_FY22Q2_FGH_Montlhy_data_March_22042022.xlsx"



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
           age = recode(age, "Unk" = "Unknown"),
           disaggregate = recode(disaggregate, 
                                 "newART" = "New on ART",
                                 "alreadyART" = "Already on ART")) %>% 
    pivot_wider(names_from =  indicator, values_from = value) %>%
    glimpse()
  
}


# IMPORT & RESHAPE TXTB SUBMISSIONS -------------------------------------------------


dod <- txtb_reshape(DOD, "JHPIEGO-DoD")
echo <- txtb_reshape(ECHO, "ECHO")
ariel <- txtb_reshape(ARIEL, "ARIEL")
ccs <- txtb_reshape(CCS, "CCS")
egpaf <- txtb_reshape(EGPAF, "EGPAF")
fgh <- txtb_reshape(FGH, "FGH")
icap <- txtb_reshape(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


txtb <- bind_rows(dod, ariel, ccs, egpaf, icap, echo, fgh)

rm(dod, ariel, ccs, echo, egpaf, fgh, icap)


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


readr::write_tsv(
  txtb,
  {monthly_dataset})


#---- SURVEY ALL MONTHLY TXTB DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.txt")

txtb_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind)


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- txtb_tidy_history %>% 
  select(datim_uid, period, TX_CURR) %>%
  filter(period == max(period)) %>% 
  group_by(datim_uid, .drop = TRUE) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(site_volume = case_when(
    TX_CURR < 1000 ~ "Low",
    between(TX_CURR, 1000, 5000) ~ "Medium",
    TX_CURR > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(datim_uid, site_volume) %>% 
  glimpse()


#---- JOIN AJUDA SITEMAP AND CLEAN DATAFRAME -----------------------


txtb_tidy_history_2 <- txtb_tidy_history %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>% 
  left_join(volumn_period) %>% 
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner.y,
         snu,
         psnu = psnu.y,
         sitename = sitename.y,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         sex,
         age,
         starts_with("TX_")) %>% 
  glimpse()


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  txtb_tidy_history_2,
  "Dataout/em_txtb.txt")




# GT TABLES ---------------------------------------------------------------


tbl <- txtb_tidy_history_2 %>%
  pivot_longer(cols = TX_CURR:TX_TB_CURR_N, names_to = "indicator", values_to = "value") %>% 
  select(indicator, period, value) %>% 
  arrange((period)) %>% 
  mutate(row_n = row_number(),
         period = as.character(period, format = "%b %y")) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  group_by(indicator) %>%
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  gt(rowname_col = "indicator") %>% 
  
  fmt_number(
    columns = 2:3, 
    rows = everything(),
    sep_mark = ",",
    decimals = 0) %>% 
  
  cols_width(
    indicator ~ px(200),
    everything() ~ px(100)) %>% 
  
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1),),
    locations = cells_body(
      columns = everything(),
      rows = everything())) %>% 
  
  tab_options(
    table.font.size = 18,
    table.font.names = "SourceSansPro-Regular",
    footnotes.font.size = 8) %>% 
  
  tab_header(title = "Mozambique TX_TB Enhanced Monitoring") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 


tbl

