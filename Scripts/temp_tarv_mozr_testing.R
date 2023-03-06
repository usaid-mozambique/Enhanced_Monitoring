
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(mozR)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(googlesheets4)
load_secrets()


# DEFINE PATHS ------------------------------------------------------------


year <- "2023"

tarv_path <- glue::glue("Data/MISAU/CT/tarv_{year}.csv")


# FUNCTIONAL -----------------------------------------------------------



tarv <- clean_sisma_csv(tarv_path)



df <- tarv %>% 
  
  dplyr::filter(!is.na(value)) %>% 
  
  dplyr::mutate(
    
    sex = dplyr::case_when(stringr::str_detect(indicator, "feminino")             ~ "Feminino",
                           stringr::str_detect(indicator, "masculino")            ~ "Masculino",
                           stringr::str_detect(indicator, "_0_4_anos|_5_9_anos")  ~ "Desconh."),
    
    age = dplyr::case_when(stringr::str_detect(indicator, "0_4")       ~ "<05",
                           stringr::str_detect(indicator, "5_9")     ~ "05-09",
                           stringr::str_detect(indicator, "10_14")   ~ "10-14",
                           stringr::str_detect(indicator, "15_19")   ~ "15-19",
                           stringr::str_detect(indicator, "20+")       ~ "20+",
                           stringr::str_detect(indicator, " 0_14")   ~ "<15",
                           stringr::str_detect(indicator, "15+")       ~ "15+",
                           TRUE ~ NA_character_),
    
    age_coarse = dplyr::case_when(age %in% c("<05", "05-09", "10-14", "<15") ~ "<15",
                                  age %in% c("15-19", "20+", "15+")          ~ "15+",
                                  TRUE ~ age),
    
    exit_type = dplyr::case_when(stringr::str_detect(indicator, "uspensos")         ~ "Suspenso",
                                 stringr::str_detect(indicator, "bitos")            ~ "Obito",
                                 stringr::str_detect(indicator, "bandonos")         ~ "Abandono",
                                 stringr::str_detect(indicator, "ransferidos_para") ~ "Transferidos Para"),
    
    indicator2 = dplyr::case_when(stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria durante") ~ "TX_NOVO",
                                  stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria ate") ~ "TX_NOVO_CUM"))
    

unique(df$sex)
unique(df$age)
unique(df$age_coarse)
unique(df$exit_type)
unique(df$sex)
unique(df$sex)
unique(df$sex)


    indicator2 = dplyr::case_when(stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria durante") ~ "TX_NOVO",
                                  stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria ate") ~ "TX_NOVO_CUM"))
    


unique(df$indicator2)

    source = "RM HIV/SIDA",
    
  )


df_sample <- df %>% 
  filter(sisma_uid == "XmuwpVFIIzP")

unique(df_sample$exit_type)



view(df)




x <- "MZ HIV SIDA - NÂº de pacientes que iniciou PrÃ©-TARV (cuidados HIV) nesta unidade sanitÃ¡ria durante o mÃªs - F 5 - 9 anos"

stringi::stri_trans_general(x, "latin-ascii")


