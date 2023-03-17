
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




path_ats_results <- "Data/MISAU/ATS/ats_results_2022.csv"

df <- clean_sisma_csv(file = path_ats_results)

df_parsed <- parse_sisma_ats_results(df)



clean_sisma_csv <- function(file, language) {
  
  df <- readr::read_csv(file) %>%
    
    dplyr::select(!c(periodname,
                     periodid,
                     perioddescription,
                     organisationunitcode,
                     organisationunitdescription,
                     orgunitlevel1,
                     organisationunitname)) %>%
    
    tidyr::pivot_longer(!c(periodcode,
                           orgunitlevel2,
                           orgunitlevel3,
                           orgunitlevel4,
                           organisationunitid),
                        names_to = "indicator",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%
    
    dplyr::filter(!is.na(value)) %>%
    
    dplyr::mutate(periodcode = paste0(periodcode, "01"),
                  periodcode = ymd(periodcode),
                  across(c(orgunitlevel2, orgunitlevel3), ~ str_to_title(.))) %>%
    
    if(language == "Portuguese"){
      
      dplyr::select(periodo = periodcode,
                    provincia = orgunitlevel2,
                    distrito = orgunitlevel3,
                    us = orgunitlevel4,
                    sisma_uid = organisationunitid,
                    indicador = indicator,
                    valor = value)
      
    } else if (language == "English"){
      
      dplyr::select(period = periodcode,
                    province = orgunitlevel2,
                    distrit = orgunitlevel3,
                    sitename = orgunitlevel4,
                    sisma_uid = organisationunitid,
                    indicator,
                    value)
    }
  
  return(df)
  
}



