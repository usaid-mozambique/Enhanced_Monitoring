
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


df <- read_csv("Data/Ajuda/ERDSD/old/AJUDA_transformed_Mar22.txt")

df_1 <- df %>% 
  select(DATIM_code,
         period = Months,
         numdenom = NumDen,
         age = AgeAsEntered,
         sex = Sex,
         dsd_eligibility = DSD_Eligibility, # NEED TO INCLUDE
         pop_type = PatientType,
         key_pop = KeyPop,
         er_status = ER_Status, # NEED TO INCLUDE
         dispensation = Dispensation,
         indicator = Indicator,
         value)


df_2 <- df_1 %>% 
  filter(!pop_type == "Total") %>%
  mutate(indicator = recode(indicator, 
                            "MMD" = "TX_MMD",
                            "previous_TX_CURR" = "TX_CURR_Previous",
                            "DSD" = "DSD_D",
                            "OneDSD" = "DSD_D",
                            "3MDD" = "DSD_3MD",
                            "6MDD" = "DSD_6MD",
                            "FluxoRa" = "DSD_FR",
                            "GAAC" = "DSD_GAAC",
                            "AFam" = "DSD_AF",
                            "ClubA" = "DSD_CA",
                            "DComm" = "DSD_DC",
                            "ER1Month" = "ER_1",
                            "ER4Month" = "ER_4"))




distinct(df_2, indicator)

  