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
monthly_dataset <- ("Dataout/TPT/_CompileHistoric/TPT_2022_03.csv") # UPDATE EACH MONTH

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/DOD__Mar_2022 final 20122021 DOD Jhpiego Included Monitoria Intensiva new Template.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ARIEL Monitoria Intensiva_ Template_FY22Q2 11.04.2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/CCS_Monitoria Intensiva_ Template_FY22Q2.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/Monitoria Intensiva_ Template_Marco_2022_ECHO_V2.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/EGPAF_Monitoria Intensiva_ Template_FY22Q2 Marco_2022.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ICAP_Marco2022_Monitoria Intensiva_ Template_FY22Q2_12042022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/FGH_MAR_22_Monitoria Intensiva Template FY22_122021_Updated_April 08_2022.xlsx"

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
  
  df <- read_excel(filename, sheet = "TPT Completion", 
                   # col_types = c("numeric", 
                   #               "text", "text", "text", "text", "text", 
                   #               "numeric", "numeric", "text", "numeric", 
                   #               "numeric", "numeric", "numeric", 
                   #               "numeric", "numeric", "numeric", 
                   #               "numeric"),
                   col_types = c("numeric", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "text", "numeric", "numeric", 
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


#---- OPTION 1: SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.csv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

chr_files <- historic_files[historic_files %in% c("TPT_2021_03.csv", "TPT_2021_04.csv")]
non_chr_files <- historic_files[historic_files %ni% c("TPT_2021_03.csv", "TPT_2021_04.csv")]

temp_chr_files <- chr_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>% 
  mutate(Period = as.Date(Period, "%d/%m/%Y")) 

temp_non_chr_files <- non_chr_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>% 
  mutate(Period = as.Date(Period, "%d/%m/%Y")) 

tpt_tidy_history <- bind_rows(temp_non_chr_files, temp_chr_files)



#---- OPTION 2: CREATE FUNCTION AND IMPORT ALL HISTORICAL FILES CREATING THEM WITH UNIFORM DATE CLASS ---------------------------------

# 
# df1 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_03.csv", col_types = cols(.default = "c", value = "d"))
# df2 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_04.csv", col_types = cols(.default = "c", value = "d"))
# df3 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_05.csv", col_types = cols(.default = "c", value = "d"))
# df4 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_06.csv", col_types = cols(.default = "c", value = "d"))
# df5 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_07.csv", col_types = cols(.default = "c", value = "d"))
# df6 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_08.csv", col_types = cols(.default = "c", value = "d"))
# df7 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_09.csv", col_types = cols(.default = "c", value = "d"))
# df8 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_10.csv", col_types = cols(.default = "c", value = "d"))
# df9 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_11.csv", col_types = cols(.default = "c", value = "d"))
# df10 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2021_12.csv", col_types = cols(.default = "c", value = "d"))
# df11 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2022_01.csv", col_types = cols(.default = "c", value = "d"))
# df12 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2022_02.csv", col_types = cols(.default = "c", value = "d"))
# df13 <- read_csv("Dataout/TPT/_CompileHistoric/TPT_2022_03.csv", col_types = cols(.default = "c", value = "d"))
# 
# tpt_tidy_history <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13) %>% 
#   mutate(Period = as.Date(Period, "%d/%m/%Y")) 


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


# GT TABLES ---------------------------------------------------------------


tbl <- tpt_tidy_history_2 %>%
  select(indicator, period, value) %>% 
  arrange((period)) %>% 
  mutate(row_n = row_number(),
         period = as.character(period, format = "%b %y")) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  group_by(indicator) %>%
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  gt(rowname_col = "indicator") %>% 
  
  fmt_number(
    columns = 2:14, 
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
  
  tab_header(title = "Mozambique TPT Enhanced Monitoring") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 
  

tbl
  
  