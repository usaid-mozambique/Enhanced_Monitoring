
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


year <- "2019"

ats_results_path <- glue::glue("Data/MISAU/ATS/ats_results_{year}.csv") # saved report "ats_resultados"     (sisma data search "ano")
ats_hist_path    <- glue::glue("Data/MISAU/ATS/ats_hist_{year}.csv")    # saved report "ats_hist_chave"     (sisma data search "historial", "chave")
ats_ci_path      <- glue::glue("Data/MISAU/ATS/ats_ci_lig_{year}.csv")  # saved report "ats_ci_lig"         (sisma data search "indice", "Diagno", "ligad", "diagno")
ats_smi_path     <- glue::glue("Data/MISAU/ATS/ats_smi_{year}.csv")     # saved report "ats_smi"
ats_ccsd_path    <- glue::glue("Data/MISAU/ATS/ats_ccs_ccd_{year}.csv") # saved report "ats_smi_ccs_ccd"
ats_saaj_cm_path <- glue::glue("Data/MISAU/ATS/ats_saaj_cm_{year}.csv") # saved report "ats_saaj_cm"

file_output <- glue::glue("Dataout/HTS/ats_all_{year}.txt")


# FUNCTIONAL -----------------------------------------------------------


ats_results <- clean_sisma_csv(ats_results_path) %>%
  parse_sisma_ats_results()

ats_history <- clean_sisma_csv(ats_hist_path) %>%
  parse_sisma_ats_history()

ats_index <- clean_sisma_csv(ats_ci_path) %>%
  parse_sisma_ats_index()

ats_smi <- clean_sisma_csv(ats_smi_path) %>%
  parse_sisma_ats_mch()

ats_ccsd <- clean_sisma_csv(ats_ccsd_path) %>% 
  parse_sisma_ats_ccsd()

ats_saaj_cm <- clean_sisma_csv(ats_saaj_cm_path) %>% 
  parse_sisma_ats_saaj_cm()


ats_compile <- bind_rows(ats_results, ats_history, ats_index, ats_smi, ats_ccsd, ats_saaj_cm)
# ats_compile <- bind_rows(ats_results, ats_history, ats_index, ats_smi) # use for 2019 as no saaj or ccsd data


# WRITE ------------------------------------------------------------------

readr::write_tsv(
  ats_compile,
  file_output,
  na = "")
