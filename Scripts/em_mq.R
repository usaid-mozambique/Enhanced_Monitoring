
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)


# DEFINE REPORTING MONTH AND FILE PATHS -------------------------------------------


month <- "2022-03-20" # UPDATE EVERY MONTH
monthly_dataset <- ("Dataout/MQ_CV/_CompileHistoric/CV_2022_03.txt") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK
final_compile <- "Dataout/em_mqcv.txt"

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx"

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/DOD__Mar_2022 final 20122021 DOD Jhpiego Included Monitoria Intensiva new Template.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ARIEL Monitoria Intensiva_ Template_FY22Q2 21.04.2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/CCS_Monitoria Intensiva_ Template_FY22Q2.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/Monitoria Intensiva_ Template_Marco_2022_ECHO_V2.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/EGPAF_Monitoria Intensiva_ Template_FY22Q2 Marco_2022_versao 2.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ICAP_Marco2022_Monitoria Intensiva_ Template_FY22Q2_Update18042022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/Monitoria Intensiva_ Template_FY22Q2_FGH_Montlhy_data_March_22042022.xlsx"

historic_files_path <- "Dataout/MQ_CV/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH



# LOAD DATASETS -----------------------------------------------------------


ajuda_site_map <- read_excel(ajuda_path) %>% 
  select(orgunituid,
         sisma_id,
         SNU,
         Psnu, 
         Sitename,
         partner = `IP FY20`) %>% 
  select_all(str_to_lower)


# CREATE FUNCTION TO TIDY CV DATASETS ---------------------------------------------------------

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
                                 "numeric")) %>% 
    rename(dpi.colheu.pcr_d__all = dpi.colheu.pcr_d_total,
           dpi.colheu.pcr_n__all = dpi.colheu.pcr_n_total,
           dpi.colheu.pcr_prop__all = dpi.colheu.pcr_prop_total,
           dpi.pcr.enviado_d__all = dpi.pcr.enviado_d_total,
           dpi.pcr.enviado_n__all = dpi.pcr.enviado_n_total,
           dpi.pcr.enviado_prop__all = dpi.pcr.enviado_prop_total,
           dpi.pcr.entregue_d__all = dpi.pcr.entregue_d_total,
           dpi.pcr.entregue_n__all = dpi.pcr.entregue_n_total,
           dpi.pcr.entregue_prop__all = dpi.pcr.entregue_prop_total,
           dpi.pcr.tarv_d__all = dpi.pcr.tarv_d_total,
           dpi.pcr.tarv_n__all = dpi.pcr.tarv_n_total,
           dpi.pcr.tarv_prop__all = dpi.pcr.tarv_prop_total) %>% 
    
    pivot_longer('dpi.colheu.pcr_d__all':'mds.cv.supressao_prop_mds', 
                 names_to = c("indicator", "numdenom", "pop_type", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    filter(!numdenom == "prop",
           !pop_type == "total") %>% 
    mutate(age = recode(age,
                        "menor2" = "<2 Months",
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
    filter(Partner == ip) %>% 
    select(-c(Data))
  
}

# IMPORT & RESHAPE cv SUBMISSIONS -----------------------------------------------------------

dod <- cv_tidy(DOD, "JHPIEGO-DoD")
echo <- cv_tidy(ECHO, "ECHO")
fgh <- cv_tidy(FGH, "FGH")
ariel <- cv_tidy(ARIEL, "ARIEL")
icap <- cv_tidy(ICAP, "ICAP")
ccs <- cv_tidy(CCS, "CCS")
egpaf <- cv_tidy(EGPAF, "EGPAF")

# COMPILE IP SUMBISSIONS --------------------------------------------------

cv_tidy <- dplyr::bind_rows(echo, fgh, ariel, icap, ccs, egpaf)

cv_tidy %>% # TABLE HF SUBMISSION LINES BY PARTNER.  CAUTION - BLANK SUBMISSION LINES ARE INCLUDED!
  group_by(Partner) %>% 
  distinct(DATIM_code) %>% 
  summarise(n())

rm(echo, fgh, ariel, icap, ccs, egpaf)

# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


readr::write_tsv(
  cv_tidy,
  na = "",
  {monthly_dataset})

#---- SURVEY ALL MONTHLY TPT DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.txt")


historic_import <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind) %>% 
  mutate(month = as.Date(month, "%Y/%m/%d")) 

#---- JOIN METADATA ---------------------------------

cv_tidy_historic <- historic_import %>% 
  select(-c(No,
            Partner,
            Province,
            District,
            `Health Facility`)) %>%
  left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>%
  rename(datim_uid = DATIM_code,
         sisma_uid = sisma_id,
         sisma_nid = SISMA_code,
         period = month) %>%
  relocate(sisma_uid, .after = datim_uid) %>%
  relocate(period:partner, .after = sisma_nid) %>%
  pivot_wider(names_from =  indicator, values_from = value) %>%
  glimpse()

# PRINT FINAL OUTPUT TO DISK ----------------------------------------------

write_tsv(
  cv_tidy_historic,
  na = "",
  {final_compile})
  
