
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


ats_results_2022_path <- "Data/MISAU/ATS/ats_results_2022.csv"

ats_hist_2022_path <- "Data/MISAU/ATS/ats_hist_2022.csv"

ats_ci_2022_path <- "Data/MISAU/ATS/ats_ci_lig_2022.csv"

ats_smi_2022_path <- "Data/MISAU/ATS/ats_smi_2022.csv"


# FUNCTIONAL -----------------------------------------------------------


ats_results <- clean_sisma_csv(ats_results_2022_path) %>% 
  parse_sisma_ats_results()

ats_history <- clean_sisma_csv(ats_hist_2022_path) %>% 
  parse_sisma_ats_history()

ats_index <- clean_sisma_csv(ats_ci_2022_path) %>% 
  parse_sisma_ats_index()

ats_smi <- clean_sisma_csv(ats_smi_2022_path) %>%
  parse_sisma_ats_mch()

ats_compile_2022 <- bind_rows(ats_results, ats_history, ats_index, ats_smi)


# WRITE ------------------------------------------------------------------

readr::write_tsv(
  ats_compile_2022,
  "Dataout/HTS/ats_compile_2022.txt",
  na = "")



ats_reg_2019 <- read_delim("Dataout/HTS/ats_2019.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

ats_reg_2020 <- read_delim("Dataout/HTS/ats_2020.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

ats_reg_2021 <- read_delim("Dataout/HTS/ats_2021.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)

ats_smi_2019_2021 <- read_delim("Dataout/HTS/ats_smi_2019_2021.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

compile <- bind_rows(ats_reg_2019, ats_reg_2020, ats_reg_2021, ats_smi_2019_2021)

compile_2019 <- compile %>% 
  filter(period < "2020-01-01")

compile_2020 <- compile %>% 
  filter(period > "2019-12-31" & period < "2021-01-01")

compile_2021 <- compile %>% 
  filter(period > "2020-12-31" & period < "2022-01-01")


readr::write_tsv(
  compile_2019,
  "Dataout/HTS/ats_compile_2019.txt",
  na = "")

readr::write_tsv(
  compile_2020,
  "Dataout/HTS/ats_compile_2020.txt",
  na = "")

readr::write_tsv(
  compile_2021,
  "Dataout/HTS/ats_compile_2021.txt",
  na = "")





