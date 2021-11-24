#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

rm(list = ls())

#---- DEFINE MONTH AND LOAD DATASETS - NEEDS UPDATING EVERY MONTH! --------------------------

month <- "2021-10-20" # UPDATE
monthly_dataset <- ("Data/Ajuda/ER_DSD_TPT_VL/_CompileHistoric/mqvl_2021_10.csv") # PATH AND NAME OF MONTHLY DATASET BEING PROCESSED AND SAVED TO DISK

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/DOD__Oct_2021final 23102021 DOD Jhpiego Included Monitoria Intensiva de CV tab (1).xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Fundação ARIEL Oct_21 (Retention Template)_FY22.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Oct_21 (Retention Template)_FY22 (1).xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/Oct_21 (Retention Template)_FY22_ECHO.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/EGPAF_Oct_21 (Retention Template)_FY22 (003)_11 11 2021.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/ICAP_Oct_21 (Retention Template)_FY22_05112021.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2021_10/FGH_Oct_21 (Retention Template)_FY22_Updated_CS_Namagola_lugea_and CS_Palane.xlsx"

#---- DEFINE PATHS AND OUTPUT NAMES - DOES NOT NEED UPDATING --------------------------------


ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/ajuda_site_map_fy21q4.xlsx") %>%
  select(-c(sisma_id,
            `IP FY20`,
            ajuda,
            ajuda_phase,
            epts_date,
            idart_date)) %>%
  dplyr::mutate(conflict = replace_na(conflict, 0),
                corridor = replace_na(corridor, 0))

historic_files_path <- "Data/Ajuda/ER_DSD_TPT_VL/_CompileHistoric/"  # PATH USED TO CREATE A LIST OF ALL .CSV FILES PREVIOUSLY CREATED

historic_dataset <- ("Dataout/em_mqvl.txt")  # PATH AND NAME OF COMPILED USAID DATASET FOR USE IN TABLEAU DASHBOARD.  THE ONLY DIFFERENCE BETWEEN THIS AND ABOVE INTERAGENCY IS THE JOIN OF AJUDA SITE MAP

#---- IMPORT AND MERGE DOD DATA -------------------------------------------------------

dod <- read_excel("Data/Ajuda/ER_DSD_TPT_VL/2021_10/DOD__Oct_2021final 23102021 DOD Jhpiego Included Monitoria Intensiva de CV tab (1).xlsx", 
                  sheet = "Monitoria Intensiva de CV", 
                  skip = 10) %>%
  filter(Partner=="JHPIEGO-DoD")

#---- IMPORT AND MERGE ECHO DATA -------------------------------------------------------

echo <- read_excel({ECHO}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  filter(Partner=="ECHO")

#---- IMPORT AND MERGE ARIEL DATA -------------------------------------------------------

ariel <- read_excel({ARIEL}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  filter(Partner=="ARIEL")

#---- IMPORT AND MERGE CCS DATA -------------------------------------------------------

ccs <- read_excel({CCS}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  filter(Partner=="CCS")

#---- IMPORT AND MERGE EGPAF DATA -------------------------------------------------------

egpaf <- read_excel({EGPAF}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  filter(Partner=="EGPAF")

#---- IMPORT AND MERGE FGH DATA -------------------------------------------------------

fgh <- read_excel({FGH}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  filter(Partner=="FGH")

#---- IMPORT AND MERGE ICAP DATA -------------------------------------------------------

icap <- read_excel({ICAP}, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
  filter(Partner=="ICAP")

#---- COMPILE IP SUMBISSIONS --------------------------------------------

mqvl <- dplyr::bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)

rm(dod,ariel, ccs, echo, egpaf, fgh, icap)

#---- CALCULATE NEW VARIABLES, PIVOT AND RENAME VARIABLES -----------------------

mqvl_tdy <- mqvl%>%
 tidyr::pivot_longer ('13_consulta_1a_d_total':'pepfar_altocv12meses_2a_n_mg', names_to = "atributo", values_to = "value") %>%
  dplyr::mutate(
  idade = dplyr::case_when(stringr::str_detect(atributo, "_total") ~ "Total",
                           stringr::str_detect(atributo,"_0_4") ~ "0-4",
                           stringr::str_detect(atributo,"_5_9") ~ "5-9",
                           stringr::str_detect(atributo,"_10_14") ~ "10-14",
                           stringr::str_detect(atributo,"_15+") ~ ">=15"))
  #Num_vs_Den = dplyr::case_when(stringr::str_detect(atributo,"_d_") ~ "Den",
   #                             stringr::str_detect(atributo,"_n_") ~ "Num")
  
  #indicador = dplyr::case_when(stringr::str_detect(atributo,"13_consulta_1a") ~ "Pacientes na 1a Linha de TARV com Pedido de Carga Viral",
                               # stringr::str_detect(atributo,"13_pedido_1a_") ~ "Pacientes na 1a Linha de TARV com Pedido de Carga Viral",
                               # stringr::str_detect(atributo,"13_iniciaram_1a") ~ "Pacientes na 1a Linha de TARV com Resultado de Carga Viral entre o sexto e o nono mês após início do TARV",
                               # stringr::str_detect(atributo,"13_receberamcv_1a" ~ "Pacientes na 1a Linha de TARV com Resultado de Carga Viral entre o sexto e o nono mês após início do TARV"),
                               # stringr::str_detect(atributo,"13_cvalto_1a" ~ "Pacientes na 1a Linha de TARV que receberam 3 sessões de APSS/PP após Resultado Alto de Carga Viral com registo de pedido de CV"),
                               # stringr::str_detect(atributo,"13_apss_1a" ~ "Pacientes na 1a Linha de TARV que receberam 3 sessões de APSS/PP após Resultado Alto de Carga Viral com registo de pedido de CV"),
                               # stringr::str_detect(atributo,"13_consulta_2a" ~ "Pacientes na 2a Linha de TARV com Pedido de Carga Viral"),
                               # stringr::str_detect(atributo,"13_pedido_2a" ~ "Pacientes na 2a Linha de TARV com Pedido de Carga Viral"),
                               # stringr::str_detect(atributo,"13_iniciaram_2a" ~ "Pacientes na 2a Linha de TARV com Resultado de Carga Viral"),
                               # stringr::str_detect(atributo,"13_cvalto_2a" ~ "Pacientes na 2a Linha de TARV com Resultado de Carga Viral"),
                               # stringr::str_detect(atributo,"13.15_consulta_cpntarvstarted" ~ "Mulheres Grávidas que Iniciaram CPN em TARV com pedido de Carga Viral"),
                               # stringr::str_detect(atributo,"13.15_pedido_cpntarvstarted" ~ "Mulheres Grávidas que Iniciaram CPN em TARV com pedido de Carga Viral"),
                               # stringr::str_detect(atributo,"13.16_consulta_cpntarvalready" ~ "Mulheres Grávidas que entraram em TARV na CPN com pedido de Carga Viral"),
                               # stringr::str_detect(atributo,"13.16_pedido_cpntarvalready" ~ "Mulheres Grávidas que entraram em TARV na CPN com pedido de Carga Viral"),
                               # stringr::str_detect(atributo,"13.17_pedido_cpnmg" ~ "Mulheres Grávidas com Resultado de Carga Viral"),
                               # stringr::str_detect(atributo,"13.17_receberamcv_cpnmg" ~ "Mulheres Grávidas com Resultado de Carga Viral"),
                               # stringr::str_detect(atributo,"13.8_cvalto_mg" ~ "Mulheres Grávidas em 1a Linha de TARV com pedido de Carga Viral e que receberam 3 sessões de APSS/PP após resultado alto de Carga Viral"),
                               # stringr::str_detect(atributo,"13.8_cvaltopedido_mg" ~ "Mulheres Grávidas em 1a Linha de TARV com pedido de Carga Viral e que receberam 3 sessões de APSS/PP após resultado alto de Carga Viral"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedido_1a" ~ "Pacientes com segundo resultado de Carga Viral alto após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedidoaltocv_1a" ~ "Pacientes com segundo resultado de Carga Viral alto após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"duplicate1_d_" ~ "Pacientes com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"duplicate2_d_" ~ "Pacientes com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"duplicate3_d_" ~ "Pacientes com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"duplicate4_d_" ~ "Pacientes com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"duplicate5_d_" ~ "Pacientes com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedidosupcv_1a" ~ "Pacientes com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_altocv12meses" ~ "Pacientes que trocaram para segunda linha de TARV após segundo resultado alto de Carga Viral"),
                               # stringr::str_detect(atributo,"pepfar_altocv12meses_2a" ~ "Pacientes que trocaram para segunda linha de TARV após segundo resultado alto de Carga Viral"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedido_1a" ~ "Mulheres Grávidas com segundo resultado de Carga Viral alto após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedidoaltocv_1a_d_mg" ~ "Mulheres Grávidas com segundo resultado de Carga Viral alto após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedidoaltocv_1a_n_mg" ~ "Mulheres Grávidas com segundo resultado de Carga Viral alto após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"duplicate_mg" ~ "Mulheres Grávidas com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_2ndpedidosupcv_1a_n_mg" ~ "Mulheres Grávidas com segundo resultado de Carga Viral baixo após sessões de APSS/PP"),
                               # stringr::str_detect(atributo,"pepfar_altocv12meses_d_mg" ~ "Mulheres Grávidas que trocaram para segunda linha de TARV após segundo resultado alto de Carga Viral"),
                               # stringr::str_detect(atributo,"pepfar_altocv12meses_2a_n_mg" ~ "Mulheres Grávidas que trocaram para segunda linha de TARV após segundo resultado alto de Carga Viral"),)

#---- WRITE MONTHLY ALL IP TPT CSV TO DISK -----------------------

readr::write_csv(
  mqvl_tdy,
  {monthly_dataset})

#---- DEFINE PATH AND SURVEY ALL MONTHLY MQVL DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------

historic_files <- dir({historic_files_path}, pattern = "*.csv")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

#---- ROW BIND ALL IP SUBMISSION AND GENERATE INTER-AGENCY OUTPUT (THIS OUTPUT IS SHARED WITH CDC) -----------------------

mqvl_tidy_history <- historic_files %>%
  map(~ read_csv(file.path(historic_files_path, .))) %>%
  reduce(rbind)
  dplyr::left_join(ajuda_site_map, by = c("DATIM_code" = "orgunituid")) %>% 
  dplyr::rename(orgunituid = DATIM_code,
                Site = `Health Facility`)

#---- WRITE TPT CSV TO DISK -----------------------

readr::write_tsv(
  mqvl_tidy_history,
  {historic_dataset})
