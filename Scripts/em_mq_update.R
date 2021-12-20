
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

ajuda_path <- "~/GitHub/AJUDA_Site_Map/Dataout/AJUDA_Site_Map_20211206.xlsx"

test_path <- "Data/Ajuda/ER_DSD_TPT_VL/2021_12/NON MER Indicators Template_FY22 12_20_2021.xlsx"




test <- read_excel("Data/Ajuda/ER_DSD_TPT_VL/2021_12/NON MER Indicators Template_FY22 12_20_2021a.xlsx", 
                   sheet = "Monitoria Intensiva de CV", 
                   skip = 9) %>% 
  glimpse()



test2 <- test %>% 
  pivot_longer('dpi.colheu.pcr_d_total':'mds.cv.supressao_prop_mds', 
               names_to = c("indicator", "numdenom", "pop_type", "age"), 
               names_sep = "_", 
               values_to = "value") %>% 
  filter(!numdenom == "prop",
         !pop_type == "total") %>% 
  mutate(age = recode(age,
                      "menor2" = "<2",
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
                            if_else(numdenom %in% c("D"), "_D", ""))) %>% 
  glimpse()




# IMPORT AJUDA META DATA --------------------------------------------------

ajuda <- read_excel(ajuda_path) %>% 
  select(orgunituid,
         SNU,
         Psnu, 
         Sitename) %>% 
  select_all(str_to_lower)

# PROGRAM TIDY FUNCTION FOR MQ DATAFRAME ----------------------------------

mq_tidy <- function(filename, ip){
  
  df <- read_excel(filename, sheet = "Monitoria Intensiva de CV", skip = 10) %>%
    pivot_longer ('13_consulta_1a_d_total':'pepfar_altocv12meses_2a_n_mg', names_to = "atributo", values_to = "value") %>%
    mutate(idade = case_when(
      str_detect(atributo, "_total") ~ "Total",
      str_detect(atributo,"_0_4") ~ "0-4",
      str_detect(atributo,"_5_9") ~ "5-9",
      str_detect(atributo,"_2_14") ~ "2-14",
      str_detect(atributo,"_10_14") ~ "10-14",
      str_detect(atributo,"_15+") ~ "15+",
      
      str_detect(atributo,"duplicate1") ~ "Total", # THIS CHUNK OF CODE ADDRESSES THE LACK OF AGE IDENTIFIERS IN THE "DUPLICATE" VARIABLE NAMES
      str_detect(atributo,"duplicate1 2") ~ "0-4",
      str_detect(atributo,"duplicate1 3") ~ "5-9",
      str_detect(atributo,"duplicate1 4") ~ "10-14",
      str_detect(atributo,"duplicate1 5") ~ "15+"),
      
      num_denom = case_when(
        str_detect(atributo, "cvalto_2a") ~ "N", # TOP LINES ADDRESS ISSUES IN NAMING OF TEMPLATE VARIABLES
        str_detect(atributo, "13_iniciaram_2a_15+") ~ "D", 
        str_detect(atributo, "duplicate") ~ "D", 
        str_detect(atributo, "_n_") ~ "N",
        str_detect(atributo, "_d_") ~ "D"),
      
      indicador = case_when(
        str_detect(atributo, "13_consulta_1a") ~ "pedido_1a",
        str_detect(atributo, "13_pedido_1a") ~ "pedido_1a", 
        
        str_detect(atributo, "iniciaram_1a") ~ "receberamcv_1a",
        str_detect(atributo, "receberamcv_1a") ~ "receberamcv_1a",
        
        str_detect(atributo, "cvalto_1a") ~ "apss_1a",
        str_detect(atributo, "apss_1a") ~ "apss_1a",
        
        str_detect(atributo, "consulta_2a") ~ "pedido_2a",
        str_detect(atributo, "pedido_2a") ~ "pedido_2a",
        
        str_detect(atributo, "iniciaram_2a") ~ "cvalto_2a",
        str_detect(atributo, "cvalto_2a") ~ "cvalto_2a",
        
        str_detect(atributo, "consulta_cpntarvstarted") ~ "pedido_cpntarvstarted",
        str_detect(atributo, "pedido_cpntarvstarted") ~ "pedido_cpntarvstarted",
        
        str_detect(atributo, "consulta_cpntarvalready") ~ "pedido_cpntarvalready",
        str_detect(atributo, "pedido_cpntarvalready") ~ "pedido_cpntarvalready",
        
        str_detect(atributo, "pedido_cpnmg") ~ "receberamcv_cpnmg",
        str_detect(atributo, "receberamcv_cpnmg") ~ "receberamcv_cpnmg",
        
        str_detect(atributo, "cvalto_mg") ~ "cvaltopedido_mg",
        str_detect(atributo, "cvaltopedido_mg") ~ "cvaltopedido_mg",
        
        str_detect(atributo, "pepfar_2ndpedido_1a") ~ "2ndpedidoaltocv_1a", # ok
        str_detect(atributo, "2ndpedidoaltocv_1a") ~ "2ndpedidoaltocv_1a", # ok
        
        str_detect(atributo, "duplicate1") ~ "2ndpedidosupcv_1a",
        str_detect(atributo, "2ndpedidosupcv_1a") ~ "2ndpedidosupcv_1a",
        
        str_detect(atributo, "altocv12meses") ~ "altocv12meses_2a",
        str_detect(atributo, "altocv12meses_2a") ~ "altocv12meses_2a",
        
        str_detect(atributo, "duplicate") ~ "2ndpedidosupcv_1a", # ok
        str_detect(atributo, "2ndpedidosupcv_1a") ~ "2ndpedidosupcv_1a", # ok
        
        str_detect(atributo, "altocv12meses") ~ "altocv12meses_2a",
        str_detect(atributo, "altocv12meses_2a") ~ "altocv12meses_2a"),
      
      indicador = str_c(toupper(indicador), num_denom, sep = "_"),
      
      period = month) %>% 
    
    select(c(Partner,
             DATIMUID,
             atributo,
             value,
             idade,
             num_denom,
             indicador,
             period)) %>% 
  
  filter(Partner == ip) 
  
}

# RUN TIDY FUNCTION ACROSS IP SUBMISSIONS ---------------------------------

dod <- mq_tidy(dod_path, "JHPIEGO-DoD")
fgh <- mq_tidy(fgh_path, "FGH")
icap <- mq_tidy(icap_path, "ICAP")
ariel <- mq_tidy(ariel_path, "ARIEL")
ccs <- mq_tidy(ccs_path, "CCS")
egpaf <- mq_tidy(egpaf_path, "EGPAF")
echo <- mq_tidy(echo_path, "ECHO")

# COMPILE IP SUMBISSIONS --------------------------------------------------

mq <- dplyr::bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)

rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# JOIN AJUDA META, PIVOT WIDE AND CLEAN-UP FINAL OUTPUT ----------------------------------------------------

mq_final <- mq %>% 
  left_join(ajuda, by = c("DATIMUID" = "orgunituid")) %>% 
  select(period,
         orgunituid = DATIMUID,
         parceiro = Partner,
         provincia = snu,
         distrito = psnu,
         us = sitename,
         indicador,
         idade,
         value,
         ) %>% 
  mutate(row_n = row_number()) %>% 
  pivot_wider(names_from = indicador, values_from = value, values_fill = 0) %>% 
  select(-(row_n)) %>%
  group_by(period, parceiro, orgunituid, provincia, distrito, us, idade) %>% 
  summarize(across(PEDIDO_1A_D:ALTOCV12MESES_2A_N, sum, na.rm = TRUE)) %>% 
  ungroup()

# PRINT TO TO DISK ----------------------------------------------------------

write.xlsx(mq_final, file = "Dataout/mqvl_test.xlsx", sheetName = "sheet1", append = TRUE, overwrite = TRUE)
