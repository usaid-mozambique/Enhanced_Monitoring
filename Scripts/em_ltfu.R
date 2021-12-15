
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)

# DEFINE REPORTING MONTH AND FILE PATHS -------------------------------------------

dev <- read_excel("Data/Devresults/LTFU_2016_2019Q4_Devresult.xlsx") 

dev_wide <- dev %>% 
  pivot_wider(names_from = `Indicator Title`, values_from = Result, values_fill = 0)

dev_wide2 <- dev_wide %>% 
  group_by(Activity, `Reporting Period`, Location, Province, District, AttributeValues) %>%
  summarise(RTT = sum(RTT),
            Identified = sum(Identified),
            Sought = sum(Sought),
            Found = sum(Found)) %>% 
  ungroup()

