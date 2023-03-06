
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

ats_ci_path      <- glue::glue("Data/MISAU/ATS/ats_ci_lig_{year}.csv")  # saved report "ats_ci_lig"         (sisma data search "indice", "Diagno", "ligad", "diagno")



# FUNCTIONAL -----------------------------------------------------------



ats_index <- clean_sisma_csv(ats_ci_path) %>%
  parse_sisma_ats_index()


ats_index <- clean_sisma_csv(ats_ci_path)


# parsing actions

df_all <- ats_index %>%
  
  dplyr::filter(!is.na(value)) %>%
  
  dplyr::mutate(
    modality = dplyr::case_when(stringr::str_detect(indicator, "Banco de Socorros") ~ "ATS-BdS",
                                stringr::str_detect(indicator, "Consultas Externas") ~ "ATS-CE",
                                stringr::str_detect(indicator, "Enfermaria") ~ "ATS-Enf",
                                stringr::str_detect(indicator, "Outro ATIP") ~ "ATS-ATIP Outro",
                                stringr::str_detect(indicator, "SMI") ~ "ATS-SMI",
                                stringr::str_detect(indicator, "TB") ~ "ATS-TB",
                                stringr::str_detect(indicator, "Triagem") ~ "ATS-Triagem",
                                stringr::str_detect(indicator, "UATS") ~ "ATS-UATS",
                                stringr::str_detect(indicator, "ATS-C") ~ "ATS-C"),
    
    modality_sub = NA_character_,
    
    sub_group = dplyr::case_when(str_detect(indicator, "Filhos <10") ~ "Filhos <10",
                                 str_detect(indicator, "Parceiro") ~ "Parceiro",
                                 str_detect(indicator, " / Pai ") ~ "Mae/Pai"),
    
    result_status = dplyr::case_when(str_detect(indicator, "ositi") ~ "Positivo",
                                     str_detect(indicator, "egativ") ~ "Negativo"),
    
    indicator = dplyr::case_when(str_detect(indicator, "Teste de Subgrupo") ~ "ATS_CI_TST",
                                 str_detect(indicator, "Contactos de casos de indice") ~ "ATS_CI",
                                 str_detect(indicator, "Numero Diagnosticado") ~ "ATS_LIG_DEN",
                                 str_detect(indicator, "ligado aos") ~ "ATS_LIG_NUM"),
    
    
    age_coarse = dplyr::case_when(str_detect(indicator, "Filhos <10") ~ "<15",
                                  str_detect(indicator, "Parceiro") ~ "15+",
                                  str_detect(indicator, " / Pai ") ~ "15+"),
    
    source = "LdR ATS",
    
    age = NA_character_,
    
    sex = NA_character_)


df_pos <- df_all %>%
  dplyr::filter(result_status == "Positivo") %>%
  dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_CI_TST" ~ "ATS_CI_TST_POS"))


df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
  dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, modality_sub, sub_group, sex, age_coarse, age, result_status, value)




# WRITE ------------------------------------------------------------------

