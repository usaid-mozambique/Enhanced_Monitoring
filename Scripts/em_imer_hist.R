
# write_tsv NA values to ""
# check monthly submission headers
# need to update script to include new MI indicators (CD4 and Revelacao)

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(mozR)
library(glamr)
library(googlesheets4)
library(googledrive)
library(fs)
library(lubridate)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(gt)
load_secrets()


# FUNCTION ----------------------------------------------------------------

# CCS ---------------------------------------------------------------

ip <- "CCS"
month <- "2020-12-20"
period <- "IMER_DEC2020"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ccs <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)





# EGPAF -------------------------------------------------------------------


ip <- "EGPAF"
month <- "2020-12-20"
period <- "IMER_DEC2020"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_egpaf <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# ECHO --------------------------------------------------------------------

ip <- "ECHO"
month <- "2020-12-20"
period <- "IMER_DEC2020"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_echo <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# FGH ---------------------------------------------------------------------

ip <- "FGH"
month <- "2020-12-20"
period <- "IMER_DEC2020"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_fgh <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ARIEL -------------------------------------------------------------------

ip <- "ARIEL"
month <- "2020-12-20"
period <- "IMER_DEC2020"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ariel <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ICAP --------------------------------------------------------------------

ip <- "ICAP"
month <- "2020-12-20"
period <- "IMER_DEC2020"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_icap <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)






# 2020 COMPILE ------------------------------------------------------------

df_2020 <- bind_rows(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)
rm(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)




# CCS ---------------------------------------------------------------

ip <- "CCS"
month <- "2021-12-20"
period <- "IMER_DEC2021"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ccs <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)





# EGPAF -------------------------------------------------------------------


ip <- "EGPAF"
month <- "2021-12-20"
period <- "IMER_DEC2021"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_egpaf <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# ECHO --------------------------------------------------------------------

ip <- "ECHO"
month <- "2021-12-20"
period <- "IMER_DEC2021"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_echo <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# FGH ---------------------------------------------------------------------

ip <- "FGH"
month <- "2021-12-20"
period <- "IMER_DEC2021"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_fgh <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ARIEL -------------------------------------------------------------------

ip <- "ARIEL"
month <- "2021-12-20"
period <- "IMER_DEC2021"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ariel <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ICAP --------------------------------------------------------------------

ip <- "ICAP"
month <- "2021-12-20"
period <- "IMER_DEC2021"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_icap <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)






# 2021 COMPILE ------------------------------------------------------------

df_2021 <- bind_rows(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)
rm(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)






# CCS ---------------------------------------------------------------

ip <- "CCS"
month <- "2022-12-20"
period <- "IMER_DEC2022"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ccs <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)





# EGPAF -------------------------------------------------------------------


ip <- "EGPAF"
month <- "2022-12-20"
period <- "IMER_DEC2022"


df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_egpaf <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# ECHO --------------------------------------------------------------------

ip <- "ECHO"
month <- "2022-12-20"
period <- "IMER_DEC2022"


df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_echo <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# FGH ---------------------------------------------------------------------

ip <- "FGH"
month <- "2022-12-20"
period <- "IMER_DEC2022"


df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_fgh <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ARIEL -------------------------------------------------------------------

ip <- "ARIEL"
month <- "2022-12-20"
period <- "IMER_DEC2022"


df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ariel <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ICAP --------------------------------------------------------------------

ip <- "ICAP"
month <- "2022-12-20"
period <- "IMER_DEC2022"

df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/Historical/IM-ER Historical Request {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_icap <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)






# 2022 COMPILE ------------------------------------------------------------

df_2022 <- bind_rows(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)
rm(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)


# CCS ---------------------------------------------------------------

ip <- "CCS"
month <- "2023-12-20"
period <- "TX NEW, TX CURR AND IMER"



df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/MonthlyEnhancedMonitoringTemplates_FY24_Jan {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ccs <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)







# EGPAF -------------------------------------------------------------------

ip <- "EGPAF"
month <- "2023-12-20"
period <- "TX NEW, TX CURR AND IMER"



df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/MonthlyEnhancedMonitoringTemplates_FY24_Jan {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_egpaf <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)


# ECHO --------------------------------------------------------------------

ip <- "ECHO"
month <- "2023-12-20"
period <- "TX NEW, TX CURR AND IMER"



df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/MonthlyEnhancedMonitoringTemplates_FY24_Jan {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_echo <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# FGH ---------------------------------------------------------------------

ip <- "FGH"
month <- "2023-12-20"
period <- "TX NEW, TX CURR AND IMER"



df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/MonthlyEnhancedMonitoringTemplates_FY24_Jan {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_fgh <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# ARIEL -------------------------------------------------------------------

ip <- "ARIEL"
month <- "2023-12-20"
period <- "TX NEW, TX CURR AND IMER"



df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/MonthlyEnhancedMonitoringTemplates_FY24_Jan {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_ariel <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# ICAP --------------------------------------------------------------------

ip <- "ICAP"
month <- "2023-12-20"
period <- "TX NEW, TX CURR AND IMER"



df <- readxl::read_excel(glue::glue("Data/Ajuda/ER_DSD_TPT_VL/2023_12/MonthlyEnhancedMonitoringTemplates_FY24_Jan {ip}.xlsx"),
                         sheet = period,
                         skip = 8,
                         col_types = "text",
                         .name_repair = "unique_quiet") %>%
  
  tidyr::pivot_longer(TX_NEWTot:TX_C_MISAU_AD_F_20p,
                      names_to = "indicator",
                      values_to = "value") %>%
  
  dplyr::select(partner = Partner,
                snu = Province,
                psnu = District,
                sitename = `Health Facility`,
                datim_uid = DATIM_code,
                indicator,
                value) %>%
  
  dplyr::inner_join(data_em_imer_var_map, by = "indicator") %>%
  dplyr::filter(!indicator_new == "remove") %>%
  tidyr::separate(indicator_new,
                  c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                  sep = "\\.") %>%
  
  dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                value = as.numeric(value),
                period = as_date(month),
                indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                age = stringr::str_replace_all(age, "\\_", "-"),
                age = dplyr::recode(age,
                                    `<1`  = "<01",
                                    `1-4` = "01-04",
                                    `5-9` = "05-09",
                                    "unknown" = "Unknown Age"), # age coding correction
                sex = dplyr::recode(sex,
                                    "M" = "Male",
                                    "F" = "Female",
                                    "unknown" = "Unknown"),
                key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                           pop_type == "MSM" ~ "MSM",
                                           pop_type == "PWID" ~ "PWID",
                                           pop_type == "PPCS" ~ "PPCS"),
                pop_type = dplyr::recode(pop_type,
                                         "FSW" = "KP",
                                         "MSM" = "KP",
                                         "PWID" = "KP",
                                         "PPCS" = "KP"),
                pop_type = dplyr::case_when(age %in% c("<15", "<01", "01-04", "05-09", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown Age") ~ "By Age",
                                            TRUE ~ pop_type),
                numdenom = tidyr::replace_na(numdenom, "N"),
                er_status = dplyr::recode(er_status,
                                          "Initiated ART" = NA_character_)) %>%
  dplyr::filter(partner == ip) %>%
  dplyr::select(partner,
                snu,
                psnu,
                sitename,
                datim_uid,
                period,
                indicator,
                sex,
                age,
                pop_type,
                key_pop,
                dispensation,
                numdenom,
                er_status,
                dsd_eligibility,
                value)

df_prev <- df %>%
  dplyr::filter(indicator == "TX_CURR") %>%
  dplyr::mutate(indicator = dplyr::recode(indicator,
                                          "TX_CURR" = "TX_CURR_Previous"),
                period = period + months(1))

df_icap <- dplyr::bind_rows(df, df_prev)

rm(df, df_prev)

# 2023 COMPILE ------------------------------------------------------------

df_2023 <- bind_rows(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)
rm(df_ccs, df_egpaf, df_echo, df_fgh, df_ariel, df_icap)

# ALL COMPILE -------------------------------------------------------------

df <- bind_rows(df_2020, df_2021, df_2022, df_2023) %>% 
  filter(!indicator == "TX_CURR_Previous")

# LOAD SITE META DATA ---------------------------------------------------------------

ajuda_site_map <- pull_sitemap()


# WRITE TO DISK -----------------------------------------------------------

write_tsv(
  df,
  file = "Dataout/imer_historic.txt",
  na = ""
)
