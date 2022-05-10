#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)

rm(list = ls())

# SET PATHS & VALUES -----------------------------------------------------------


period <- "2022-03-20" # UPDATE EACH QUARTER
quarterly_dataset <- ("Dataout/KP_MISAU/_CompileHistoric/misau_kp_fy22q2.txt") # UPDATE EACH QUARTER


ARIEL <- "Data/MISAU/KP/ARIEL FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2_21.04.22.xlsx"
FGH <- "Data/MISAU/KP/FGH FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2_22042022.xlsx"
ICAP <- "Data/MISAU/KP/ICAP_FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
CCS <- "Data/MISAU/KP/CCS FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
EGPAF <- "Data/MISAU/KP/EGPAF FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
ECHO <- "Data/MISAU/KP/ECHO FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"
DOD <- "Data/MISAU/KP/DOD FY22_Q2_TemplateReltrimestral_PopChave_MISAU_v2.2.xlsx"

historic_files_path <- "Dataout/KP_MISAU/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH


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



# EXTRACT FUNCTION RESHAPE ---------------------------------------------


kp_reshape <- function(filename, ip){

  df <- read_excel(filename, sheet = "POP CHAVES - Trimestre", skip = 9) %>% 
    select(-c(No, SISMA_code, Data, Column1)) %>% 
    pivot_longer(TX_New_KP_total:`TX_PVLS_Num_6meses_REC&MTS_25+`, 
                 names_to = "temp",
                 values_to = "value") %>% 
    filter(Partner == ip) %>% 
    mutate(indicator = case_when(str_detect(temp, "TX_CURR_6meses")  ~ "TX_CURR_6MO",
                                 str_detect(temp, "TX_CURR_meses")  ~ "TX_CURR_6MO",  # CORRECT IN REPORTING TEMPLATE?
                                 str_detect(temp, "TX_New_6meses")  ~ "TX_NEW_6MO",
                                 str_detect(temp, "TX_CURR_12meses")  ~ "TX_RET_N_12MO",
                                 str_detect(temp, "TX_New_12meses")  ~ "TX_RET_D_12MO",
                                 str_detect(temp, "TX_New")  ~ "TX_NEW",  # CORRECT IN REPORTING TEMPLATE?
                                 str_detect(temp, "TX_CURR")  ~ "TX_CURR",
                                 str_detect(temp, "TX_PVLS_Den_6meses")  ~ "TX_PVLS_D_6MO",
                                 str_detect(temp, "TX_PVLS_Den_meses")  ~ "TX_PVLS_D_6MO",   # CORRECT IN REPORTING TEMPLATE?
                                 str_detect(temp, "TX_PVLS_Num_6meses")  ~ "TX_PVLS_N_6MO",
                                 str_detect(temp, "TX_PVLS_Num")  ~ "TX_PVLS_N",
                                 str_detect(temp, "TX_PVLS_Den")  ~ "TX_PVLS_D"
                                 ),
    keypop = case_when(str_detect(temp, "otal")  ~ "All (KP & non-KP)",
                       str_detect(temp, "_PID_")  ~ "PID",
                       str_detect(temp, "_HSH_")  ~ "HSH",
                       str_detect(temp, "_MTS_")  ~ "MTS",
                       str_detect(temp, "_Rec_")  ~ "REC",  # CORRECT IN REPORTING TEMPLATE?
                       str_detect(temp, "_PID&HSH_")  ~ "PID/HSH",
                       str_detect(temp, "_PID&MST_")  ~ "PID/MTS",  # CORRECT IN REPORTING TEMPLATE?
                       str_detect(temp, "_PID&Rec_")  ~ "PID/REC", 
                       str_detect(temp, "_HSH&PID_")  ~ "HSH/PID",
                       str_detect(temp, "_HSH&Rec_")  ~ "HSH/REC",
                       str_detect(temp, "_MTS&PID_")  ~ "MTS/PID",
                       str_detect(temp, "_MTS&Rec_")  ~ "MTS/REC",
                       str_detect(temp, "_REC&PID_")  ~ "REC/PID",
                       str_detect(temp, "_REC&HSH_")  ~ "REC/HSH",
                       str_detect(temp, "_REC&MTS_")  ~ "REC/MTS",
                       str_detect(temp, "_MST")  ~ "MTS",  # CORRECT IN REPORTING TEMPLATE?
                       str_detect(temp, "_REC_")  ~ "REC"),
    age = case_when(str_detect(temp, "15_19")  ~ "15-19",
                    str_detect(temp, "20_24")  ~ "20-24",
                    str_detect(temp, "25+")  ~ "25+")
    ) %>% 
    pivot_wider(names_from = indicator, values_from = value)
  
}


# PROCESS IP SUBMISSIONS --------------------------------------------------


df_ariel <- kp_reshape(ARIEL, "ARIEL")
df_fgh <- kp_reshape(FGH, "FGH")
df_icap <- kp_reshape(ICAP, "ICAP")
df_ccs <- kp_reshape(CCS, "CCS")
df_egpaf <- kp_reshape(EGPAF, "EGPAF")
df_echo <- kp_reshape(ECHO, "ECHO")
df_dod <- kp_reshape(DOD, "JHPIEGO-DoD")


# JOIN METADATA -----------------------------------------------------------


df <- bind_rows(df_ariel, df_fgh, df_icap, df_ccs, df_egpaf, df_echo, df_dod) %>%  
  mutate(date = {period},
         pop_type = case_when(keypop == "All (KP & non-KP)" ~ "General",
                                TRUE ~ "Key Pop"),
         keypop = case_when(keypop == "All (KP & non-KP)" ~ "",
                            TRUE ~ keypop)) %>% 
  select(Province,
         District,
         `Health Facility`,
         DATIM_code,
         pop_type,
         keypop,
         age,
         period = date,
         starts_with("TX_")) %>% 
  glimpse()


# PRINT OUTPUT TO DISK ------------------------------------------------------


readr::write_tsv(
  df,
  {quarterly_dataset})


#---- SURVEY ALL MONTHLY TXTB DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({historic_files_path}, pattern = "*.txt")

misau_kp_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind)


#---- LOAD & ALIGN HISTORICAL DATASET PRIOR TO FY22 ---------------------------------

misau_kp_tidy_history_2 <- read_delim("Dataout/KP_MISAU/_CompileHistoric/old/misau_kp_fy20fy21.txt", 
                                delim = "\t", escape_double = FALSE, 
                                col_types = cols(age = col_character(),
                                                 TX_NEW_6MO = col_double(),
                                                 TX_CURR_6MO = col_double(),
                                                 TX_PVLS_D_6MO = col_double(),
                                                 TX_PVLS_N_6MO = col_double()), 
                                  
                                trim_ws = TRUE) %>% 
  glimpse()

misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="d3aFvsvQ7O2"]<-"Nv5WX50ktwr"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="GsYcZ9h9BUK"]<-"Z6ZZLw50mAM"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="GY2vnTIADh1"]<-"kKsL2QkNR4K"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="JCaOI2EWde3"]<-"p9u7nMWHEgF"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="rsZpZwTAPq1"]<-"DVF2BNdSzV9"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="S6gaiIWGmh9"]<-"fuVClko8HKS"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="Sqry0ikIChZ"]<-"aUMEQzUKI0K"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="U7iWzT5xo5X"]<-"NJKcpotvrAQ"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="VE6gqvIqNNU"]<-"kny7mdXnSEx"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="XOhxRL9w3Jt"]<-"wgqFTVU7Iky"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="xpFEJzy6IXt"]<-"VSHfWQyhzUu"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$DATIM_code=="Y9W059tBM50"]<-"yJ014QFEqru"
misau_kp_tidy_history_2$DATIM_code[misau_kp_tidy_history_2$`Health Facility`=="Chimonzo CS"]<-"R8hdPUSeX0B"

misau_kp_tidy_history_3 <- bind_rows(misau_kp_tidy_history, misau_kp_tidy_history_2)

#---- ROW BIND ALL IP SUBMISSION AND GENERATE OUTPUT -----------------------


volumn_period <- misau_kp_tidy_history_3 %>% 
  select(DATIM_code, period, TX_CURR) %>%
  filter(period == max(period)) %>% 
  group_by(DATIM_code, .drop = TRUE) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(site_volume = case_when(
    TX_CURR < 1000 ~ "Low",
    between(TX_CURR, 1000, 5000) ~ "Medium",
    TX_CURR > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(datim_uid = DATIM_code, 
         site_volume) %>% 
  glimpse()


#---- JOIN AJUDA SITEMAP AND CLEAN DATAFRAME -----------------------


misau_kp_tidy_history_4 <- misau_kp_tidy_history_3 %>%
  left_join(ajuda_site_map, by = c("DATIM_code" = "datim_uid")) %>% 
  left_join(volumn_period, by = c("DATIM_code" = "datim_uid")) %>% 
  select(datim_uid = DATIM_code,
         sisma_uid,
         site_nid,
         period,
         partner,
         snu,
         psnu,
         sitename,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         pop_type,
         keypop,
         age,
         starts_with("TX_")) %>% 
  glimpse()


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  misau_kp_tidy_history_4,
  "Dataout/em_kp_misau.txt")

