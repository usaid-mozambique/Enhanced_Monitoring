
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
# load_secrets()


# DEFINE PATHS ------------------------------------------------------------


year <- "2023"

path_df <- glue::glue("Data/MISAU/ATS/ats_autotest_{year}.csv")

map_indicator_ats_auto <- read_excel("Documents/indicator_mapper/sisma_ats_autoteste_indicator.xlsx") %>% filter(!is.na(indicator_new))

# FUNCTIONAL -----------------------------------------------------------


df <- clean_sisma_csv(path_df)

map_indicator_sisma <- df %>% distinct(indicator)

write_excel_csv(map_indicator_sisma, file = "map_indicator_autotest.csv")


df_1 <- df %>% 
  inner_join(map_indicator_ats_auto, by = "indicator")


df_1 %>% 
  filter(indicator_new == "ATS_TST_AUTO_HIST") %>% 
  ggplot(aes(disaggregate, value)) + 
  geom_col()

df_1 %>% 
  filter(indicator_new == "ATS_TST_AUTO_LOCAL") %>% 
  ggplot(aes(disaggregate, value)) + 
  geom_col()

df_1 %>% 
  filter(indicator_new == "ATS_TST_AUTO") %>% 
  ggplot(aes(age, value)) + 
  geom_col() + 
  coord_flip()
