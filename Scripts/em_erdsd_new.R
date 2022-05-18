
# LOAD CORE TIDYVERSE & OTHER PACKAGES ------------------------------------


rm(list = ls())

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(scales)
library(lubridate)


# DEFINE MONTH AND PATHS ---------------------------


month <- "20/04/2022" # UPDATE EACH MONTH
monthly_dataset <- ("Dataout/ERDSD/_CompileHistoric/ERDSD_2022_04.txt") # UPDATE EACH MONTH

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/DOD__April_2022final 20042022 DOD Jhpiego New Template.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/ARIEL Monitoria Intensiva and DSD Template_FY22_April 2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/CCS_Monitoria Intensiva_ Template_FY22Abril.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/Monitoria Intensiva_ Template_FY22Q2_APR_ECHO_V2.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/EGPAF_Monitoria Intensiva_ Template_FY22Q2 Abril_2022.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/ICAP_Abril_2022_Monitoria Intensiva_ Template_FY22Q3_updated11052022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_04/Monitoria Intensiva_ Template_FY22Q2_FGH_Montlhy_data_April_05052022.xlsx"


historic_files_path <- "Dataout/ERDSD/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH


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


# CREATE FUNCTION TO RESHAPE SUBMISSIONS ---------------------------------------------


erdsd_var_mapping <- read_excel("Documents/erdsd_var_mapping.xlsx", sheet = "Sheet2")


erdsd_reshape <- function(filename, ip){
  
  df <- read_excel(filename, 
                   sheet = "ER_DSD", 
                   skip = 8,
                   col_types = "text") %>% 
    select(!c(No, SISMA_code, Period)) %>% 
    pivot_longer(TX_NEWTot:DCommTotNoEl10_14, 
                 names_to = "indicator", 
                 values_to = "value") %>% 
    inner_join(erdsd_var_mapping, by = "indicator") %>% 
    filter(!indicator_new == "remove") %>% 
    separate(indicator_new, 
             c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
             sep = "_") %>% 
    mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
           value = as.numeric(value),
           period = as.Date(month, "%d/%m/%Y"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           age = str_replace_all(age, "\\.", "-"),
           age = recode(age,
                        "unknown" = "Unknown"),
           sex = recode(sex,
                  "M" = "Male",
                  "F" = "Female")) %>% 
    filter(Partner == ip) %>% 
    select(partner = Partner,
           snu = Province,
           psnu = District,
           sitename = `Health Facility`,
           datim_uid = DATIM_code,
           period,
           indicator,
           sex,
           age,
           pop_type,
           dispensation,
           numdenom,
           er_status,
           dsd_eligibility,
           value) %>% 
    drop_na(value)
  
  tx_curr_prev <- df %>%
    filter(indicator == "TX_CURR") %>% 
    mutate(indicator = recode(indicator,
                              "TX_CURR" = "TX_CURR_Previous"),
           period = period + months(1))
  
  df <- bind_rows(df, tx_curr_prev)
  
  return(df)
  
}


# IMPORT & RESHAPE SUBMISSIONS -------------------------------------------------


ariel <- erdsd_reshape(ARIEL, "ARIEL")
echo <- erdsd_reshape(ECHO, "ECHO")
ccs <- erdsd_reshape(CCS, "CCS")
egpaf <- erdsd_reshape(EGPAF, "EGPAF")
fgh <- erdsd_reshape(FGH, "FGH")
icap <- erdsd_reshape(ICAP, "ICAP")
dod <- erdsd_reshape(DOD, "JHPIEGO-DoD")


# COMPILE SUMBISSIONS --------------------------------------------------


er_dsd <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)

rm(dod, ariel, ccs, echo, egpaf, fgh, icap)


# IDENTIFY ERRORS COMMON IN MONTHLY SUBMISSION --------------------------------------------------


check_missing_datim_uid <- er_dsd %>% 
  anti_join(ajuda_site_map, by = "datim_uid") %>% 
  distinct(partner, snu, psnu, sitename, datim_uid)


# WRITE MONTHLY CSV TO DISK ------------------------------------


readr::write_tsv(
  er_dsd,
  na = "NA",
  {monthly_dataset})


#---- SURVEY ALL MONTHLY DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.txt")

erdsd_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>% 
  subset(period < max(period))


#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- erdsd_tidy_history %>% 
  select(datim_uid, period, indicator, value) %>%
  filter(period == max(period),
         indicator == "TX_CURR") %>% 
  group_by(datim_uid, .drop = TRUE) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(site_volume = case_when(
    value < 1000 ~ "Low",
    between(value, 1000, 5000) ~ "Medium",
    value > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(datim_uid, site_volume) %>% 
  glimpse()


#---- JOIN AJUDA SITEMAP & CLEAN DATAFRAME -----------------------


erdsd_tidy_history_2 <- erdsd_tidy_history %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>% 
  left_join(volumn_period) %>% 
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner.y,
         snu = snu.y,
         psnu = psnu.y,
         sitename = sitename.y,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         sex,
         age,
         pop_type,
         dispensation,
         er_status,
         dsd_eligibility,
         numdenom,
         indicator, 
         value) %>% 
  glimpse()


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  erdsd_tidy_history_2,
  "Dataout/em_erdsd_new.txt")
