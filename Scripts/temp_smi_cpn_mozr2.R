
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


year <- "2022"

tarv_path <- glue::glue("Data/MISAU/SMI/sisma_cpn_{year}.csv")


# FUNCTIONAL -----------------------------------------------------------





df_smi_cpn <- process_sisma_csv(tarv_path, type = "CPN")



parse_sisma_csv <- function(data, type){
  
  switch(type,
         "CPN" = parse_sisma_smi_cpn(data),
         "ATS Result" = parse_sisma_ats_results(data)
  )
  
}



process_sisma_csv <- function(file, type){
  
  df <- clean_sisma_csv(file) %>% 
    parse_sisma_csv(type)
  
  return(df)
  
}
