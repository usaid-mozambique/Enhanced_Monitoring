
rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(fs)
library(googlesheets4)
library(googledrive)
library(glamr)
library(glitr)
library(grabr)
library(ggthemes)
library(janitor)
library(glue)
library(readxl)
library(openxlsx)
library(Wavelength)
load_secrets()


# GLOBAL VARIABLES --------------------------------------------------------

month <- "2023-01-20"
filename <- "Data/Disa_new/monthly/Relatorio Mensal de Carga Viral Janeiro 2023.xlsx"

dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("DISA_VL_{dt}")

# VALUES & PATHS ----------------------------------------------------------

# paths that require updating with each new monthly file




# paths that do not require monthly updating
path_historic_output_local <- "Dataout/DISA/monthly_processed/"
file_monthly_output_local <- path(path_historic_output_local, file, ext = "txt")
file_historic_output_local <- "Dataout/em_disa.txt"

path_disa_datim_map <- as_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12XN6RKaHNlmPoy3om0cbNd1Rn4SqHSva")
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")



# ingestion
df_age <- readxl::read_excel(filename, 
                             sheet = "Age & Sex", 
                             col_types = c("text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric"), 
                             skip = 4)


df_pw <- readxl::read_excel("C:/Users/jlara/Desktop/Relatorio Mensal de Carga Viral proposed template.xlsx", 
                            sheet = "S. Viral (M. Gravidas)", 
                            col_types = c("text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"), 
                            skip = 4)


df_lw <- readxl::read_excel("C:/Users/jlara/Desktop/Relatorio Mensal de Carga Viral proposed template.xlsx", 
                            sheet = "S. Viral (M. Lactantes)", 
                            col_types = c("text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"), 
                            skip = 4)


df_tat <- readxl::read_excel("C:/Users/jlara/Desktop/Relatorio Mensal de Carga Viral proposed template.xlsx", 
                             sheet = "TRL - AVG", 
                             col_types = c("text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"), 
                             skip = 4)


# tidy
df <- dplyr::bind_rows(df_age, df_pw, df_lw, df_tat) %>% 
  
  dplyr::select(!c(datim_uid, sitetype, contains("total"))) %>%
  
  tidyr::pivot_longer(cols = tidyselect::starts_with("vl"),
                      names_to = c("indicator", "group", "motive", "result", "tat_step"),
                      names_sep = "_",
                      values_to = "value") %>% 
  
  dplyr::filter(value > 0)


# feature engineering
df_1 <- df %>% 
  dplyr::mutate(period = as.Date(month, "%Y-%m-%d"),
                
                sex = dplyr::case_when(indicator == "vl" & sex == "F" ~ "Female",
                                       indicator == "vl" & sex == "M" ~ "Male",
                                       indicator == "vl" & sex %in% c("UNKNOWN", "Not Specified", "Não especificado") ~ "Unknown"),
                
                age = dplyr::case_when(age == "<1" ~ "<01",
                                       age == "Idade não especificada" ~ "Unknown Age",
                                       age == "No Age Specified" ~ "Unknown Age",
                                       age == "Não especificada" ~ "Unknown Age",
                                       age == "Não especificado" ~ "Unknown Age",
                                       age == "NS" ~ "Unknown Age",
                                       TRUE ~ age),
                
                group = dplyr::case_when(group == "age" ~ "Age",
                                         group == "pw" ~ "PW",
                                         group == "lw" ~ "LW"),
                
                motive = dplyr::case_when(motive == "routine" ~ "Routine",
                                          motive == "failure" ~ "Theraputic Failure",
                                          motive == "repeat" ~ "Post Breastfeeding",
                                          motive == "unspecified" ~ "Not Specified",
                                          motive == "" ~ NA_character_),
                
                tat_step = dplyr::case_when(str_detect(tat_step, "s1.") ~ "S1: Collection to Receipt",
                                            str_detect(tat_step, "s2.") ~ "S2: Receipt to Registration",
                                            str_detect(tat_step, "s3.") ~ "S3: Registration to Analysis",
                                            str_detect(tat_step, "s4.") ~ "S4: Analysis to Validation"),
                
                VL = dplyr::case_when(result == "suppress" | result == "unsuppress" & indicator == "vl" ~ value),
                
                VLS = dplyr::case_when(result == "suppress" & indicator == "vl" ~ value,
                                       is.na(result) ~ 0),
                
                TAT = dplyr::case_when(indicator == "vltat" ~ value)) %>% 
  
  dplyr::select(!c(result, indicator, value))
