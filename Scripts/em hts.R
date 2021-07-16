# NEED TO RERUN 2019, 2020 HISTORICAL NUMBERS BASED ON THE UPDATE TO MCH PORTION OF CODE.  ONLY KNOWN UNRESOLVED ISSUE RELATES TO CCR NUMBERS


#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)


#-----------------------------------------------------------------------------------
# HFR MONTH

month <- "2021-06-01"

# IMPORT TESTING DATASETS & AJUDA Site Map -----------------------------------------------------------------------------------
# 

hts_result <- read_excel("Data/MISAU/ats_result_2021.xlsx") ## KEY SISMA SEARCH WORD(S) "ano"
hts_ci_lig <- read_excel("Data/MISAU/ats_ci_lig_2021.xlsx") ## KEY SISMA SEARCH WORD(S) "indice", "diagno", "ligad"
hts_kp_hist <- read_excel("Data/MISAU/ats_hist_2021.xlsx")  ## KEY SISMA SEARCH WORD(S) "historial", "chave"
hts_smi <- read_excel("Data/MISAU/ats_smi_2021.xlsx", .name_repair = "universal") ## SAVED SISMA REPORT "ats_smi_misau__new2"

AJUDA_Site_Map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/ajuda_site_map_144.xlsx")

# IMPORT HISTORIC COMPILED DATASETS -----------------------------------------------------------------------------------
# 

output_em_hts_2019 <- read_csv("Data/MISAU/output_em_hts_2019.csv", 
                               col_types = cols(SubGroup = col_character()))


output_em_hts_2020 <- read_csv("Data/MISAU/output_em_hts_2020.csv", 
                               col_types = cols(SubGroup = col_character()))

# PROCESS TESTING DATAFRAME -----------------------------------------------------------------------------------
# 

hts_all <- hts_result %>% 
  dplyr::select(-c(periodname,
                   periodid,
                   perioddescription,
                   organisationunitcode,
                   organisationunitdescription,
                   orgunitlevel1,
                   organisationunitname
  )) %>%
  tidyr::gather(Indicator, Value, -c(periodcode,
                                     orgunitlevel2,
                                     orgunitlevel3,
                                     orgunitlevel4,
                                     organisationunitid), na.rm = TRUE) %>%
  dplyr::mutate(Indicator = stringr::str_remove(Indicator, "MZ ATS - Resultado por grupo etario - ")) %>%   ## removes the stem from Indicator
  tidyr::separate(Indicator, c("Modality", "AgeSemiFine", "Sex"), sep = ", ") %>%   ## splits this out to columns
  dplyr::mutate(
    AgeSemiFine = stringr::str_remove_all(AgeSemiFine, " |anos|ano"), ## clean up of `Age`
    Sex = dplyr::recode(Sex, "FEMININO" = "Female", "MASCULINO" = "Male"),
    AgeSemiFine = dplyr::recode(AgeSemiFine, "< 1 ano" = "<1", "1.9" = "1-9", "10.14" = "10-14", "15.19" = "15-19", "20.24" = "20-24", "25.49" = "25-49", "â\u2030¥50" = "50+"),
    ResultStatus = stringr::str_extract(Modality, "Negativo|Positivo"),
    ResultStatus = dplyr::recode(ResultStatus, "Negativo" = "Negative", "Positivo" = "Positive"),
    Modality = stringr::str_remove(Modality, " (Negativo|Positivo)"),
    Modality = dplyr::case_when(Modality == "ATS-C" ~ "Community",
                                Modality == "Banco de Socorros" ~ "ER",
                                Modality == "Consultas Externas" ~ "Outpatient",
                                Modality == "Enfermaria" ~ "Inpatient",
                                Modality == "Outro ATIP" ~ "Other PICT",
                                Modality == "SMI" ~ "MCH",
                                Modality == "TB" ~ "TB",
                                Modality == "Triagem" ~ "Triage",
                                Modality == "UATS" ~ "VCT"),
    AgeCoarse = dplyr::case_when(AgeSemiFine == "<1" ~ "<15",
                                 AgeSemiFine == "1-9" ~ "<15",
                                 AgeSemiFine == "10-14" ~ "<15"),
    AgeCoarse = replace_na(AgeCoarse, "15+"),
    SubGroup = NA,
    Indicator = "HTS_TST",
    Date = paste0(periodcode, "01"),
    Date = ymd(Date)) %>%
  dplyr::rename(Sisma_id = organisationunitid,
                Province = orgunitlevel2,
                District = orgunitlevel3,
                Site = orgunitlevel4) %>%
  dplyr::select(Date, Province, District, Site, Sisma_id, Indicator, Modality, SubGroup, Sex, AgeCoarse, AgeSemiFine, ResultStatus, Value)

# PROCESS CASE INDEX AND LINKAGE DATAFRAME -----------------------------------------------------------------------------------
# 

hts_ci_lig_all <- hts_ci_lig %>% 
  dplyr::select(-c(periodname,
                   periodid,
                   perioddescription,
                   organisationunitcode,
                   organisationunitdescription,
                   orgunitlevel1,
                   organisationunitname
  )) %>%
  tidyr::gather(Indicator_temp, Value, -c(periodcode,
                                          orgunitlevel2,
                                          orgunitlevel3,
                                          orgunitlevel4,
                                          organisationunitid), na.rm = TRUE) %>%
  dplyr::mutate(
    Modality = dplyr::case_when(grepl("Banco de Socorros", Indicator_temp) ~ "ER",
                                grepl("Consultas Externas", Indicator_temp) ~ "Outpatient",
                                grepl("Enfermaria", Indicator_temp) ~ "Inpatient",
                                grepl("Outro ATIP", Indicator_temp) ~ "Other PICT",
                                grepl("SMI", Indicator_temp) ~ "MCH",
                                grepl("TB", Indicator_temp) ~ "TB",
                                grepl("Triagem", Indicator_temp) ~ "Triage",
                                grepl("UATS", Indicator_temp) ~ "VCT",
                                grepl("ATS-C", Indicator_temp) ~ "Community"),
    SubGroup = dplyr::case_when(grepl("Filhos <10", Indicator_temp) ~ "Chilren <10",
                                grepl("Parceiro", Indicator_temp) ~ "Partner",
                                grepl(" / Pai ", Indicator_temp) ~ "Mother/Father"),
    Indicator = dplyr::case_when(grepl("Teste de Subgrupo", Indicator_temp) ~ "HTS_CI_TST",
                                 grepl("Contactos de casos de indice", Indicator_temp) ~ "HTS_CI",
                                 grepl("Numero Diagnosticado", Indicator_temp) ~ "HTS_LIG_DEN",
                                 grepl("ligado aos", Indicator_temp) ~ "HTS_LIG_NUM"),
    ResultStatus = dplyr::case_when(grepl("Positi", Indicator_temp) ~ "Positive",
                                    grepl("Negativ", Indicator_temp) ~ "Negative"),
    AgeCoarse = dplyr::case_when(grepl("Filhos <10", Indicator_temp) ~ "<15",
                                 grepl("Parceiro", Indicator_temp) ~ "15+",
                                 grepl(" / Pai ", Indicator_temp) ~ "15+"),
    AgeSemiFine = NA,
    Sex = NA,
    Date = paste0(periodcode, "01"),
    Date = ymd(Date)) %>%
  dplyr::rename(Sisma_id = organisationunitid,
                Province = orgunitlevel2,
                District = orgunitlevel3,
                Site = orgunitlevel4) %>%
  dplyr::select(Date, Province, District, Site, Sisma_id, Indicator, Modality, SubGroup, Sex, AgeCoarse, AgeSemiFine, ResultStatus, Value)

# PROCESS KP & TESTING HISTORY DATAFRAME -----------------------------------------------------------------------------------
# 

hts_kp_hist_all <- hts_kp_hist %>% 
  dplyr::select(-c(periodname,
                   periodid,
                   perioddescription,
                   organisationunitcode,
                   organisationunitdescription,
                   orgunitlevel1,
                   organisationunitname
  )) %>%
  tidyr::gather(Indicator_temp, Value, -c(periodcode,
                                          orgunitlevel2,
                                          orgunitlevel3,
                                          orgunitlevel4,
                                          organisationunitid), na.rm = TRUE) %>%
  dplyr::mutate(
    Modality = dplyr::case_when(grepl("Banco de Socorros", Indicator_temp) ~ "ER",
                                grepl("Consultas Externas", Indicator_temp) ~ "Outpatient",
                                grepl("Enfermaria", Indicator_temp) ~ "Inpatient",
                                grepl("Outro ATIP", Indicator_temp) ~ "Other PICT",
                                grepl("SMI", Indicator_temp) ~ "MCH",
                                grepl("TB", Indicator_temp) ~ "TB",
                                grepl("Triagem", Indicator_temp) ~ "Triage",
                                grepl("UATS", Indicator_temp) ~ "VCT",
                                grepl("ATS-C", Indicator_temp) ~ "Community"),
    SubGroup = dplyr::case_when(grepl("- MTS", Indicator_temp) ~ "FSW",
                                grepl("- PID", Indicator_temp) ~ "IDU",
                                grepl("- HSH", Indicator_temp) ~ "MSM",
                                grepl("- MIN", Indicator_temp) ~ "Miners",
                                grepl("- REC", Indicator_temp) ~ "Prisoners"),
    Indicator = dplyr::case_when(grepl("1 vez testado", Indicator_temp) ~ "HTS_HIST_FIRST",
                                 grepl("pos. ", Indicator_temp) ~ "HTS_HIST_POS",
                                 grepl("Subgrupo", Indicator_temp) ~ "HTS_KP"),
    ResultStatus = dplyr::case_when(grepl("Positi", Indicator_temp) ~ "Positive",
                                    grepl("Negativ", Indicator_temp) ~ "Negative"),
    AgeCoarse = NA,
    AgeSemiFine = NA,
    Sex = NA,
    Date = paste0(periodcode, "01"),
    Date = ymd(Date)
    ) %>%
  dplyr::rename(Sisma_id = organisationunitid,
                Province = orgunitlevel2,
                District = orgunitlevel3,
                Site = orgunitlevel4) %>%
  dplyr::select(Date, Province, District, Site, Sisma_id, Indicator, Modality, SubGroup, Sex, AgeCoarse, AgeSemiFine, ResultStatus, Value)

# PROCESS MCH DATAFRAME -----------------------------------------------------------------------------------
# 

hts_smi_all <- hts_smi %>% 
  dplyr::select(-c(periodname,
                   periodid,
                   perioddescription,
                   organisationunitcode,
                   organisationunitdescription,
                   orgunitlevel1,
                   organisationunitname)) %>%
  tidyr::replace_na(list(`SMI.CPP...PuÃ.rperas.testadas.na.Consulta.PÃ³s.Parto` = 0,
                         `SMI.CPP...PuÃ.rperas.HIV...das.PuÃ.rperas.Testadas.na.Consulta.PÃ³s.Parto.` = 0,
                         `SMI.MAT...Total.testadas.para.HIV.na.maternidade` = 0,
                         `SMI.MAT...HIV..identificadas.na.maternidade` = 0,
                         `SMI.PF...HIV.Utentes.testadas.na.Consulta.de.SaÃºde.Reprodutiva` = 0,
                         `SMI.PF...Utentes.HIV...das.Mulheres.Testadas.na.CSR.PF.` = 0,
                         `SMI.UG...Total.testadas.para.HIV.nas.UrgÃªncias.de.Ginecologia` = 0,
                         `SMI.UG...HIV..identificadas.na.ginecologia` = 0) 
  ) %>% 
  # CALCULATE NEGATIVE TEST VALUE BASED ON TOTAL TESTED AND TESTED POSITIVE
  dplyr::mutate(SMI_CPP_HIV_negative = `SMI.CPP...PuÃ.rperas.testadas.na.Consulta.PÃ³s.Parto` - `SMI.CPP...PuÃ.rperas.HIV...das.PuÃ.rperas.Testadas.na.Consulta.PÃ³s.Parto.`,
                SMI_MAT_HIV_negative = `SMI.MAT...Total.testadas.para.HIV.na.maternidade` - `SMI.MAT...HIV..identificadas.na.maternidade`,
                SMI_PF_HIV_negative = `SMI.PF...HIV.Utentes.testadas.na.Consulta.de.SaÃºde.Reprodutiva` - `SMI.PF...Utentes.HIV...das.Mulheres.Testadas.na.CSR.PF.`,
                SMI_UG_HIV_negative = `SMI.UG...Total.testadas.para.HIV.nas.UrgÃªncias.de.Ginecologia` - `SMI.UG...HIV..identificadas.na.ginecologia`) %>%
  # REMOVE TOTAL TESTED INDICATORS TO AVOID DUPLICATING RESULTS
  dplyr::select(-c(`SMI.CPP...PuÃ.rperas.testadas.na.Consulta.PÃ³s.Parto`,
                   `SMI.MAT...Total.testadas.para.HIV.na.maternidade`,
                   `SMI.PF...HIV.Utentes.testadas.na.Consulta.de.SaÃºde.Reprodutiva`,
                   `SMI.UG...Total.testadas.para.HIV.nas.UrgÃªncias.de.Ginecologia`)) %>%
  tidyr::gather(Indicator_temp, Value, -c(periodcode,
                                          orgunitlevel2,
                                          orgunitlevel3,
                                          orgunitlevel4,
                                          organisationunitid), na.rm = TRUE) %>%
  dplyr::mutate(Modality = dplyr::case_when(grepl("CPN", Indicator_temp) ~ "MCH-CPN",
                                            grepl("MAT", Indicator_temp) ~ "MCH-MAT",
                                            grepl("CCR", Indicator_temp) ~ "MCH-CCR",
                                            grepl("CPP", Indicator_temp) ~ "MCH-CPP",
                                            grepl("PF", Indicator_temp) ~ "MCH-PF",
                                            grepl("UG", Indicator_temp) ~ "MCH-UG"),
                ResultStatus = dplyr::case_when(grepl("negativ", Indicator_temp) ~ "Negative",
                                                grepl("positi", Indicator_temp) ~ "Positive",
                                                grepl("HIV...", Indicator_temp) ~ "Positive",
                                                grepl("identificad", Indicator_temp) ~ "Positive"),
                SubGroup = dplyr::case_when(grepl("Parceir", Indicator_temp) ~ "Partner",
                                            grepl("expost", Indicator_temp) ~ "Infant"),
                AgeCoarse = dplyr::case_when(grepl("MCH-CCR", Modality) ~ "<15"),
                AgeCoarse = tidyr::replace_na(AgeCoarse, "15+"),
                Sex = dplyr::case_when(grepl("Parceir", Indicator_temp) ~ "Male",
                                       grepl("MCH-CCR", Modality) ~ "Unknown"),
                Sex = tidyr::replace_na(Sex, "Female"),
                Indicator = "HTS_TST",
                AgeSemiFine = NA,
                Date = paste0(periodcode, "01"),
                Date = ymd(Date)) %>%
  dplyr::rename(Sisma_id = organisationunitid,
                Province = orgunitlevel2,
                District = orgunitlevel3,
                Site = orgunitlevel4) %>%
  dplyr::select(Date, Province, District, Site, Sisma_id, Indicator, Modality, SubGroup, Sex, AgeCoarse, AgeSemiFine, ResultStatus, Value)

# GENERATE SEPARATE DATAFRAME FOR HTS_TST_POS RESULTS (SUBSEQUENTLY APPENDED TO HTS_TST DATAFRAME) -----------------------------------------------------------------------------------
# 

hts_smi_pos <- hts_smi_all %>%
  dplyr::filter(ResultStatus == "Positive") %>%
  dplyr::mutate(Indicator = "HTS_TST_POS")

# UNION FROM HTS OBJECTS CREATED ABOVE -----------------------------------------------------------------------------------
# 

hts_compile_year <- dplyr::bind_rows(hts_all, hts_ci_lig_all, hts_kp_hist_all, hts_smi_all, hts_smi_pos)

# UNION YEARLY COMPILED DATASETS -----------------------------------------------------------------------------------
# 

hts_compile_history <- dplyr::bind_rows(output_em_hts_2019, output_em_hts_2020, hts_compile_year)

# GENERATE HFR SUBMISSION -----------------------------------------------------------------------------------
# 

hfr_hts_tst <- hts_compile_history %>%
  dplyr::mutate(mech_code = "70212",
                operatingunit = "Mozambique") %>%
  dplyr::rename(sisma_id = Sisma_id) %>%
  dplyr::right_join(AJUDA_Site_Map) %>%
  dplyr::filter(!Modality %in% c("Community"),
         Date == month,
         Indicator == "HTS_TST",
         epts == 1,
         `IP FY20` == "ECHO")

hfr_hts_pos <- hfr_hts_tst %>%
  dplyr::filter(ResultStatus == "Positive") %>%
  dplyr::mutate(Indicator = "HTS_TST_POS")

hfr_hts <- dplyr::bind_rows(hfr_hts_tst, hfr_hts_pos) %>%
  dplyr::select(date = Date,
         orgunit = Sitename,
         orgunituid,
         mech_code,
         partner = `IP FY20`,
         operatingunit,
         psnu = Psnu,
         indicator = Indicator,
         sex = Sex,
         agecoarse = AgeCoarse,
         otherdisaggregate = ResultStatus,
         val = Value)

# GENERATE ENHANCED MONITORING DATASET -----------------------------------------------------------------------------------
# 

em_hts_tidy <- hts_compile_history %>% 
  group_by(Date, Province, District, Site, Sisma_id, Indicator, Modality, SubGroup, Sex, AgeCoarse, AgeSemiFine, ResultStatus) %>%
  summarize_at(vars(Value), sum, na.rm = TRUE) %>% 
  pivot_wider(names_from = Indicator, values_from = Value)

# PRINT EM DATAFRAME TO DISK -----------------------------------------------------------------------------------
# 

readr::write_tsv(
  hts_compile_history,
  "Dataout/em_hts.txt")

# PRINT HFR DATAFRAME TO DISK -----------------------------------------------------------------------------------
# 

readr::write_csv(
  hfr_hts,
  "Dataout/HFR/hfr_hts.csv")

#-----------------------------------------------------------------------------------
# REMOVE TEMPORARY OBJECTS FROM ENVIRONMENT

remove(list=c("AJUDA_Site_Map", "hfr_hts_pos", "hfr_hts_tst", "hts_all", "hts_ci_lig", "hts_ci_lig_all", "hts_compile_year", "hts_kp_hist", "hts_kp_hist_all", "hts_result", "hts_smi", "hts_smi_all", "hts_smi_pos", "output_em_hts_2019", "output_em_hts_2020"))


# TEST SUMS CODE

em_hts_tst <- em_hts_tidy %>% 
  select(Date:ResultStatus, HTS_TST) %>% 
  drop_na(HTS_TST) 

ggplot(data = em_hts_tst) +
  geom_col(mapping = aes(y = HTS_TST, x = Date))


