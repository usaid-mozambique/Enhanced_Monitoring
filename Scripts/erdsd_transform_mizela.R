
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
                            "DSD" = "DSD_D",
                            "OneDSD" = "DSD_OneDSD",
                            "3MDD" = "DSD_3MD",
                            "6MDD" = "DSD_6MD",
                            "FluxoRa" = "DSD_FR",
                            "GAAC" = "DSD_GAAC",
                            "AFam" = "DSD_AF",
                            "ClubA" = "DSD_CA",
                            "DComm" = "DSD_DC",
                            "ER1Month" = "ER_1",
                            "ER4Month" = "ER_4"),
         indicator_new = case_when(indicator == "ER_1" & numdenom == "Denominator" ~ "ER_1_D",
                                   indicator == "ER_1" & numdenom == "Numerator" ~ "ER_1_N",
                                   indicator == "ER_4" & numdenom == "Denominator" ~ "ER_4_D",
                                   indicator == "ER_4" & numdenom == "Numerator" ~ "ER_4_N",
                                   indicator == "TX_CURR" & pop_type == "KeyPop" ~ "TX_CURR_KP",
                                   indicator == "TX_NEW" & pop_type == "KeyPop" ~ "TX_NEW_KP",
                                   indicator == "TX_NEW" & pop_type == "Breastfeeding" ~ "TX_NEW_BF",
                                   TRUE ~ as.character(indicator)))




distinct(df_2, indicator_new)


df_2 %>% 
  filter(indicator_new == "TX_NEW") %>% 
    distinct(indicator, pop_type, key_pop)
  
test_miz <- df_2 %>% 
  pivot_wider(names_from = indicator, values_from = value)
  distinct(indicator)
test_jl <- er_dsd %>% 
  pivot_wider(names_from = indicator, values_from = value)
  distinct(indicator)

setdiff(names(test_jl), names(test_miz))
setdiff(names(test_miz), names(test_jl))
