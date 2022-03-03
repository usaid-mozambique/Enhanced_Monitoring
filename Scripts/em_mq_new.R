
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
monthly_dataset <- ("Dataout/MQ_CV/_CompileHistoric/CV_2021_12.txt") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK
monthly_compile <- "Dataout/MQ_CV/CV_compile.txt"
final_compile <- "Dataout/em_mqcv.txt"

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx"

# dod <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/ARIEL Monitoria Intensiva_ Template_FY22 12_20_2021.xlsx"
echo <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/Monitoria Intensiva_ Template_FY22 12_20_2021_updated_ECHO.xlsx"
fgh <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/FGH_DEC_21_Monitoria Intensiva Template FY22_122021_Updated_ January 6_2022.xlsx"
icap <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/ICAP_Dezembro_2021 Monitoria Intensiva_ Template_FY22 12_20_2021_updated10012022.xlsx"
ariel <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/ARIEL Monitoria Intensiva_ Template_FY22 12_20_2021.xlsx"
ccs <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/NON MER Indicators Template_FY22 12_20_2021 CCS.xlsx"
egpaf <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/EGPAF_Monitoria Intensiva_Dec_21_ Template_FY22 12_20_2021_updated (003).xlsx"


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

# dod <- cv_tidy(dod, "JHPIEGO-DoD")
echo <- cv_tidy(echo, "ECHO")
fgh <- cv_tidy(fgh, "FGH")
ariel <- cv_tidy(ariel, "ARIEL")
icap <- cv_tidy(icap, "ICAP")
ccs <- cv_tidy(ccs, "CCS")
egpaf <- cv_tidy(egpaf, "EGPAF")

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

# APPEND MONTHLY TO HISTORICAL FILE
readr::write_tsv(
  cv_tidy,
  na = "",
  append = TRUE,
  {monthly_compile})


# TEMPORARY WORKAROUND ----------------------------------------------------


cv_compile <- read_delim({monthly_compile}, 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE) %>%
  select(-c(No,
            Type,
            Partner,
            Province,
            District, 
            `Health Facility`)) %>% 
  left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  rename(datim_uid = DATIM_code,
         sisma_uid = sisma_id,
         sisma_nid = SISMA_code,
         province = snu,
         district = psnu,
         site = sitename) %>% 
  relocate(month:partner, .after = sisma_uid) %>% 
  relocate(sisma_uid, .after = datim_uid) %>% 
  pivot_wider(names_from =  indicator, values_from = value) %>% 
  glimpse()

write_tsv(
  cv_compile,
  na = "",
  {final_compile})
  
