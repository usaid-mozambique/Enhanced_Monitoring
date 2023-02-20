
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

# DEFINE HFR MONTH --------------------------------------------------------

hfr_month <- "2022-09-01"
hfr_month_var <- "01/09/2022"


## KEY SISMA SEARCH WORD(S) "ano"
## KEY SISMA SEARCH WORD(S) "indice", "Diagno", "ligad", "diagno"
## KEY SISMA SEARCH WORD(S) "historial", "chave"
## SAVED SISMA REPORT "ats_smi_misau__new2"

# DEFINE PATHS ------------------------------------------------------------


ats_results_2019_path <- "Data/MISAU/ATS/ats_results_2019.csv"
ats_results_2020_path <- "Data/MISAU/ATS/ats_results_2020.csv"
ats_results_2021_path <- "Data/MISAU/ATS/ats_results_2021.csv"
ats_results_2022_path <- "Data/MISAU/ATS/ats_results_2022.csv"

ats_hist_2019_path <- "Data/MISAU/ATS/ats_hist_2019.csv"
ats_hist_2020_path <- "Data/MISAU/ATS/ats_hist_2020.csv"
ats_hist_2021_path <- "Data/MISAU/ATS/ats_hist_2021.csv"
ats_hist_2022_path <- "Data/MISAU/ATS/ats_hist_2022.csv"

ats_ci_2019_path <- "Data/MISAU/ATS/ats_ci_lig_2019.csv"
ats_ci_2020_path <- "Data/MISAU/ATS/ats_ci_lig_2020.csv"
ats_ci_2021_path <- "Data/MISAU/ATS/ats_ci_lig_2021.csv"
ats_ci_2022_path <- "Data/MISAU/ATS/ats_ci_lig_2022.csv"

ats_smi_2019_path <- "Data/MISAU/ATS/ats_smi_2019.csv"
ats_smi_2020_path <- "Data/MISAU/ATS/ats_smi_2020.csv"
ats_smi_2021_path <- "Data/MISAU/ATS/ats_smi_2021.csv"
ats_smi_2022_path <- "Data/MISAU/ATS/ats_smi_2022.csv"

path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets


# SITE REMOVAL LIST -------------------------------------------------------

site_removal_list <- "abg5UReivZX"

# LOAD METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap() %>% 
  select(sisma_uid,
         datim_uid,
         site_nid,
         partner = partner_pepfar_clinical,
         his_epts,
         his_emr,
         his_idart,
         his_disa,
         program_ovc,
         program_ycm,
         latitude,
         longitude)


# LOAD DATASETS -----------------------------------------------------------


ats_results_2019 <- read_csv({ats_results_2019_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_results_2020 <- read_csv({ats_results_2020_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_results_2021 <- read_csv({ats_results_2021_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_results_2022 <- read_csv({ats_results_2022_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)



ats_hist_2019 <- read_csv({ats_hist_2019_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_hist_2020 <- read_csv({ats_hist_2020_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_hist_2021 <- read_csv({ats_hist_2021_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_hist_2022 <- read_csv({ats_hist_2022_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)



ats_ci_2019 <- read_csv({ats_ci_2019_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_ci_2020 <- read_csv({ats_ci_2020_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_ci_2021 <- read_csv({ats_ci_2021_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)

ats_ci_2022 <- read_csv({ats_ci_2022_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list)



ats_smi_2019 <- read_csv({ats_smi_2019_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list) %>% 
  clean_names() %>% 
  glimpse()

ats_smi_2020 <- read_csv({ats_smi_2020_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list) %>% 
  clean_names()

ats_smi_2021 <- read_csv({ats_smi_2021_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list) %>% 
  clean_names()

ats_smi_2022 <- read_csv({ats_smi_2022_path}) %>% 
  mutate(organisationunitcode = as.character(organisationunitcode)) %>% 
  filter(!organisationunitid == site_removal_list) %>% 
  clean_names()


# BIND YEARLY DATAFRAMES ---------------------------------------------


ats_results_all <- bind_rows(ats_results_2019, ats_results_2020, ats_results_2021, ats_results_2022)
ats_hist_all <- bind_rows(ats_hist_2019, ats_hist_2020, ats_hist_2021, ats_hist_2022)
ats_ci_all <- bind_rows(ats_ci_2019, ats_ci_2020, ats_ci_2021, ats_ci_2022)
ats_smi_all <- bind_rows(ats_smi_2019, ats_smi_2020, ats_smi_2021, ats_smi_2022)


# PROCESS ATS RESULTS DATAFRAME -------------------------------------------


ats_results_all_1 <- ats_results_all %>% 
  select(-c(periodname,
            periodid,
            perioddescription,
            organisationunitcode,
            organisationunitdescription,
            orgunitlevel1,
            organisationunitname
  )) %>%
  gather(indicator, value, -c(periodcode,
                              orgunitlevel2,
                              orgunitlevel3,
                              orgunitlevel4,
                              organisationunitid), na.rm = TRUE) %>%
  mutate(indicator = str_remove(indicator, "MZ ATS - Resultado por grupo etario - ")) %>%   ## removes the stem from Indicator
  separate(indicator, c("modality", "age_semi_fine", "sex"), sep = ", ") %>%   ## splits this out to columns
  mutate(
    age_semi_fine = str_remove_all(age_semi_fine, " |anos|ano"), ## clean up of `Age`
    sex = recode(sex, "FEMININO" = "Female", "MASCULINO" = "Male"),
    age_semi_fine = recode(age_semi_fine, "< 1 ano" = "<1", "1.9" = "1-9", "10.14" = "10-14", "15.19" = "15-19", "20.24" = "20-24", "25.49" = "25-49", "â\u2030¥50" = "50+"),
    result_status = str_extract(modality, "Negativo|Positivo"),
    result_status = recode(result_status, "Negativo" = "Negative", "Positivo" = "Positive"),
    modality = str_remove(modality, " (Negativo|Positivo)"),
    modality = case_when(modality == "ATS-C" ~ "Community",
                         modality == "Banco de Socorros" ~ "ER",
                         modality == "Consultas Externas" ~ "Outpatient",
                         modality == "Enfermaria" ~ "Inpatient",
                         modality == "Outro ATIP" ~ "Other PICT",
                         modality == "SMI" ~ "MCH",
                         modality == "TB" ~ "TB",
                         modality == "Triagem" ~ "Triage",
                         modality == "UATS" ~ "VCT"),
    age_coarse = case_when(age_semi_fine == "<1" ~ "<15",
                           age_semi_fine == "1-9" ~ "<15",
                           age_semi_fine == "10-14" ~ "<15"),
    age_coarse = replace_na(age_coarse, "15+"),
    sub_group = NA,
    indicator = "HTS_TST",
    period = paste0(periodcode, "01"),
    period = ymd(period)) %>%
  rename(sisma_uid = organisationunitid,
         snu = orgunitlevel2,
         psnu = orgunitlevel3,
         sitename = orgunitlevel4) %>% 
  mutate(snu = str_to_title(snu),
         psnu = str_to_title(psnu)) %>% 
  select(sisma_uid, snu, psnu, sitename, period, indicator, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)


#### CREATE ATS POSITIVE INDICATOR


ats_results_all_pos_1 <- ats_results_all_1 %>%
  filter(result_status == "Positive") %>% 
  mutate(indicator = recode(indicator, 
                            "HTS_TST" = "HTS_TST_POS")) %>% 
  glimpse()


#### BIND ATS POSITIVE INDICATOR DATAFRAME AND PIVOT WIDE


ats_results <- bind_rows(ats_results_all_1, ats_results_all_pos_1)

sum(ats_results$value, na.rm = TRUE) # 11685105 (AFTER REMOVAL) 11686094 WITH ALL (HTS 11028477, POS 657617)


# PROCESS ATS KEYPOP/HISTORY DATAFRAME -------------------------------------------


ats_hist_all_1 <- ats_hist_all %>% 
  select(-c(periodname,
            periodid,
            perioddescription,
            organisationunitcode,
            organisationunitdescription,
            orgunitlevel1,
            organisationunitname
  )) %>%
  gather(indicator_temp, value, -c(periodcode,
                                   orgunitlevel2,
                                   orgunitlevel3,
                                   orgunitlevel4,
                                   organisationunitid), na.rm = TRUE) %>%
  mutate(
    modality = case_when(grepl("Banco de Socorros", indicator_temp) ~ "ER",
                         grepl("Consultas Externas", indicator_temp) ~ "Outpatient",
                         grepl("Enfermaria", indicator_temp) ~ "Inpatient",
                         grepl("Outro ATIP", indicator_temp) ~ "Other PICT",
                         grepl("SMI", indicator_temp) ~ "MCH",
                         grepl("TB", indicator_temp) ~ "TB",
                         grepl("Triagem", indicator_temp) ~ "Triage",
                         grepl("UATS", indicator_temp) ~ "VCT",
                         grepl("ATS-C", indicator_temp) ~ "Community"),
    sub_group = case_when(grepl("- MTS", indicator_temp) ~ "FSW",
                          grepl("- PID", indicator_temp) ~ "IDU",
                          grepl("- HSH", indicator_temp) ~ "MSM",
                          grepl("- MIN", indicator_temp) ~ "Miners",
                          grepl("- REC", indicator_temp) ~ "Prisoners"),
    indicator = case_when(grepl("1 vez testado", indicator_temp) ~ "HTS_HIST_FIRST",
                          grepl("pos. ", indicator_temp) ~ "HTS_HIST_POS",
                          grepl("Subgrupo", indicator_temp) ~ "HTS_KP"),
    result_status = case_when(grepl("Positi", indicator_temp) ~ "Positive",
                              grepl("Negativ", indicator_temp) ~ "Negative"),
    age_coarse = NA,
    age_semi_fine = NA,
    sex = NA,
    period = paste0(periodcode, "01"),
    period = ymd(period)
  ) %>%
  rename(sisma_uid = organisationunitid,
         snu = orgunitlevel2,
         psnu = orgunitlevel3,
         sitename = orgunitlevel4) %>%
  mutate(snu = str_to_title(snu),
         psnu = str_to_title(psnu)) %>% 
  select(sisma_uid, snu, psnu, sitename, period, indicator, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)


#### CREATE ATS POSITIVE INDICATOR


ats_hist_all_pos_1 <- ats_hist_all_1 %>%
  filter(result_status == "Positive") %>% 
  mutate(indicator = recode(indicator, 
                            "HTS_KP" = "HTS_KP_POS")) %>% 
  glimpse()


#### BIND ATS POSITIVE INDICATOR DATAFRAME AND PIVOT WIDE


ats_hist <- bind_rows(ats_hist_all_1, ats_hist_all_pos_1)


# PROCESS ATS CASE INDEX DATAFRAME -------------------------------------------


ats_ci_all_1 <- ats_ci_all %>% 
  select(-c(periodname,
            periodid,
            perioddescription,
            organisationunitcode,
            organisationunitdescription,
            orgunitlevel1,
            organisationunitname
  )) %>%
  gather(indicator_temp, value, -c(periodcode,
                                   orgunitlevel2,
                                   orgunitlevel3,
                                   orgunitlevel4,
                                   organisationunitid), na.rm = TRUE) %>%
  dplyr::mutate(
    modality = dplyr::case_when(grepl("Banco de Socorros", indicator_temp) ~ "ER",
                                grepl("Consultas Externas", indicator_temp) ~ "Outpatient",
                                grepl("Enfermaria", indicator_temp) ~ "Inpatient",
                                grepl("Outro ATIP", indicator_temp) ~ "Other PICT",
                                grepl("SMI", indicator_temp) ~ "MCH",
                                grepl("TB", indicator_temp) ~ "TB",
                                grepl("Triagem", indicator_temp) ~ "Triage",
                                grepl("UATS", indicator_temp) ~ "VCT",
                                grepl("ATS-C", indicator_temp) ~ "Community"),
    sub_group = dplyr::case_when(grepl("Filhos <10", indicator_temp) ~ "Chilren <10",
                                 grepl("Parceiro", indicator_temp) ~ "Partner",
                                 grepl(" / Pai ", indicator_temp) ~ "Mother/Father"),
    indicator = dplyr::case_when(grepl("Teste de Subgrupo", indicator_temp) ~ "HTS_CI_TST",
                                 grepl("Contactos de casos de indice", indicator_temp) ~ "HTS_CI",
                                 grepl("Numero Diagnosticado", indicator_temp) ~ "HTS_LIG_DEN",
                                 grepl("ligado aos", indicator_temp) ~ "HTS_LIG_NUM"),
    result_status = dplyr::case_when(grepl("Positi", indicator_temp) ~ "Positive",
                                     grepl("Negativ", indicator_temp) ~ "Negative"),
    age_coarse = dplyr::case_when(grepl("Filhos <10", indicator_temp) ~ "<15",
                                  grepl("Parceiro", indicator_temp) ~ "15+",
                                  grepl(" / Pai ", indicator_temp) ~ "15+"),
    age_semi_fine = NA,
    sex = NA,
    period = paste0(periodcode, "01"),
    period = ymd(period)
  ) %>%
  rename(sisma_uid = organisationunitid,
         snu = orgunitlevel2,
         psnu = orgunitlevel3,
         sitename = orgunitlevel4) %>%
  mutate(snu = str_to_title(snu),
         psnu = str_to_title(psnu)) %>% 
  select(sisma_uid, snu, psnu, sitename, period, indicator, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)


#### CREATE CASE INDEX POSITIVE INDICATOR

ats_ci_all_1_pos <- ats_ci_all_1 %>%
  filter(result_status == "Positive") %>% 
  mutate(indicator = recode(indicator, 
                            "HTS_CI_TST" = "HTS_CI_TST_POS")) %>% 
  glimpse()


#### BIND CASE INDEX POSITIVE INDICATOR DATAFRAME AND PIVOT WIDE


ats_ci <- bind_rows(ats_ci_all_1, ats_ci_all_1_pos)


# PROCESS ATS MCH DATAFRAME -------------------------------------------

my_colnames2 <- as.tibble(names(ats_smi_all))

ats_smi_all_1 <- ats_smi_all %>% 
  select(-c(periodname,
            periodid,
            perioddescription,
            organisationunitcode,
            organisationunitdescription,
            orgunitlevel1,
            organisationunitname)) %>%
  replace_na(list(smi_cpp_pu_a_c_rperas_testadas_na_consulta_pa3s_parto = 0,
                  smi_cpn_mulheres_gr_a_vidas_testadas_hiv_positivas_durante_a_cpn = 0,
                  smi_mat_total_testadas_para_hiv_na_maternidade = 0,
                  smi_mat_hiv_identificadas_na_maternidade = 0,
                  smi_pf_hiv_utentes_testadas_na_consulta_de_sa_aode_reprodutiva = 0,
                  smi_pf_utentes_hiv_das_mulheres_testadas_na_csr_pf = 0, # PERHAPS NOT COUNTING IN POSITIVE IN THE CODE BELOW
                  smi_ug_total_testadas_para_hiv_nas_urg_aancias_de_ginecologia = 0,
                  smi_ug_hiv_identificadas_na_ginecologia = 0)
  ) %>%
  # CALCULATE NEGATIVE TEST VALUE BASED ON TOTAL TESTED AND TESTED POSITIVE
  mutate(smi_cpp_hiv_negative = smi_cpp_pu_a_c_rperas_testadas_na_consulta_pa3s_parto - smi_cpp_pu_a_c_rperas_hiv_das_pu_a_c_rperas_testadas_na_consulta_pa3s_parto,
         smi_mat_hiv_negative = smi_mat_total_testadas_para_hiv_na_maternidade - smi_mat_hiv_identificadas_na_maternidade,
         smi_pf_hiv_negative = smi_pf_hiv_utentes_testadas_na_consulta_de_sa_aode_reprodutiva - smi_pf_utentes_hiv_das_mulheres_testadas_na_csr_pf,
         smi_ug_hiv_negative = smi_ug_total_testadas_para_hiv_nas_urg_aancias_de_ginecologia - smi_ug_hiv_identificadas_na_ginecologia) %>%
  # REMOVE TOTAL TESTED INDICATORS TO AVOID DUPLICATING RESULTS
  select(-c(smi_cpp_pu_a_c_rperas_testadas_na_consulta_pa3s_parto,
            smi_mat_total_testadas_para_hiv_na_maternidade,
            smi_pf_hiv_utentes_testadas_na_consulta_de_sa_aode_reprodutiva,
            smi_ug_total_testadas_para_hiv_nas_urg_aancias_de_ginecologia)) %>%
  gather(indicator_temp, value, -c(periodcode,
                                   orgunitlevel2,
                                   orgunitlevel3,
                                   orgunitlevel4,
                                   organisationunitid), na.rm = TRUE) %>%
  mutate(modality = case_when(grepl("smi_cpn", indicator_temp) ~ "MCH-CPN",
                              grepl("smi_mat", indicator_temp) ~ "MCH-MAT",
                              grepl("smi_ccr", indicator_temp) ~ "MCH-CCR",
                              grepl("smi_cpp", indicator_temp) ~ "MCH-CPP",
                              grepl("smi_pf", indicator_temp) ~ "MCH-PF",
                              grepl("smi_ug", indicator_temp) ~ "MCH-UG"),
         result_status = case_when(grepl("negativ", indicator_temp) ~ "Negative",
                                  grepl("positi", indicator_temp) ~ "Positive",
                                  grepl("HIV+", indicator_temp) ~ "Positive",
                                  grepl("rperas_hiv_das", indicator_temp) ~ "Positive",
                                  grepl("utentes_hiv_das", indicator_temp) ~ "Positive",
                                  grepl("identificad", indicator_temp) ~ "Positive"),
         sub_group = case_when(grepl("parceir", indicator_temp) ~ "Partner",
                              grepl("expost", indicator_temp) ~ "Infant"),
         age_coarse = case_when(grepl("smi_ccr", modality) ~ "<15"),
         age_coarse = replace_na(age_coarse, "15+"),
         sex = case_when(grepl("parceir", indicator_temp) ~ "Male",
                         grepl("smi_ccr", modality) ~ "Unknown"),
         sex = replace_na(sex, "Female"),
         indicator = "HTS_TST",
         age_semi_fine = NA,
         period = paste0(periodcode, "01"),
         period = ymd(period)) %>%
  rename(sisma_uid = organisationunitid,
         snu = orgunitlevel2,
         psnu = orgunitlevel3,
         sitename = orgunitlevel4) %>% 
  mutate(snu = str_to_title(snu),
         psnu = str_to_title(psnu),
         source = "MCH Register") %>% 
  select(sisma_uid, snu, psnu, sitename, period, indicator, source, modality, sub_group, sex, age_coarse, age_semi_fine, result_status, value)


#### CREATE CASE INDEX POSITIVE INDICATOR

ats_smi_all_pos_1 <- ats_smi_all_1 %>%
  filter(result_status == "Positive") %>% 
  mutate(indicator = recode(indicator, 
                            "HTS_TST" = "HTS_TST_POS")) %>% 
  glimpse()


#### BIND CASE INDEX POSITIVE INDICATOR DATAFRAME AND PIVOT WIDE


ats_smi <- bind_rows(ats_smi_all_1, ats_smi_all_pos_1)


# BIND ALL DATAFRAMES -----------------------------------------------------


ats <- bind_rows(ats_results, ats_hist, ats_ci, ats_smi)

ats %>% 
  filter(period > "2021-12-30",
         indicator == "HTS_HIST_POS",
         value > 0) %>%
  count(snu, wt = value)

# JOIN METADATA -----------------------------------------------------------


ats_2 <- ats %>%
  left_join(ajuda_site_map, by = "sisma_uid") %>%
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner,
         snu,
         psnu,
         sitename,
         ends_with("tude"),
         starts_with("program"),
         starts_with("his"),
         indicator,
         modality,
         sub_group,
         sex,
         age_coarse,
         age_semi_fine,
         result_status,
         value) %>%
  mutate(
    across(c(program_ovc:his_disa), ~replace_na(.x, 0))
  ) %>%
  mutate(row_n = row_number()) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  select(-c(row_n)) %>% 
  glimpse()


test <- ats_2 %>% 
  filter(period > "2022-01-01") %>% 
  group_by(period) %>% 
  summarize(HTS_HIST_FIRST, na.rm = T) %>% 
  ungroup() %>% 
  count(period, HTS_HIST_FIRST, sort = T)


# PRINT DATAFRAME TO DISK -------------------------------------------------


readr::write_tsv(
  ats_2,
  "Dataout/em_hts.txt")


# HFR ------------------------------------------------------------------


hfr <- ats_2 %>% 
  filter(period == hfr_month,
         !modality %in% c("Community")) %>% 
  mutate(mech_code = "70212",
         operatingunit = "Mozambique",
         date = hfr_month_var) %>%
  right_join(ajuda_site_map) %>%
  filter(partner == "ECHO") %>% 
  select(date, 
         orgunit = sitename,
         orgunituid = datim_uid,
         mech_code, 
         partner,
         operatingunit,
         psnu,
         sex, 
         agecoarse = age_coarse,
         otherdisaggregate = result_status,
         HTS_TST,
         HTS_TST_POS
         )

hfr_2 <- hfr %>% 
  pivot_longer(HTS_TST:HTS_TST_POS, names_to = "indicator", values_to = "val") %>% 
  select(date, 
         orgunit,
         orgunituid,
         mech_code, 
         partner,
         operatingunit,
         psnu,
         indicator,
         sex, 
         agecoarse,
         otherdisaggregate,
         val)

readr::write_csv(
  hfr_2,
  "Dataout/HFR/hfr_hts_202209.csv",
  na = "0")

test <- hfr_2 %>% 
  filter(indicator == "HTS_TST")
sum(test$val, na.rm = T)




