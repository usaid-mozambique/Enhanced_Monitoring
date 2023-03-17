
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

# VALUES & PATHS ----------------------------------------------------------

# paths that require updating with each new monthly file
period <- "2023-02-20"
file <- "2023_02"
file_input <- "Data/Disa_new/monthly/Relatorio Mensal de Carga Viral Fevereiro 2023.xlsx"

# paths that do not require monthly updating
path_historic_output_local <- "Dataout/DISA/monthly_processed/"
file_monthly_output_local <- path(path_historic_output_local, file, ext = "txt")
file_historic_output_local <- "Dataout/em_disa.txt"

path_disa_datim_map <- as_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12XN6RKaHNlmPoy3om0cbNd1Rn4SqHSva")
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")


# DATA LOAD, MUNGE & COMPILE -------------------------------------------------------


disa_datim_map <- read_sheet(path_disa_datim_map, sheet = "map_disa") %>% 
  select(!c(note))

ajuda_site_map <- read_sheet(path_disa_datim_map, sheet = "list_ajuda") %>% 
  select(datim_uid,
         partner = partner_pepfar_clinical,
         starts_with("his_"))


ou_name <- name <- "Mozambique"
ou_uid <- get_ouuid(operatingunit = ou_name)
org_level <- get_ouorglevel(operatingunit = ou_name, org_type =  c("facility"))
orgsuids <- get_ouorgs(ouuid = ou_uid, level =  org_level)


cntry <- "Mozambique"
uid <- get_ouuid(cntry)
datim_orgsuids <- pull_hierarchy(uid, username = datim_user(), password = datim_pwd()) %>% 
  filter(!is.na(facility) & !is.na(psnu)) %>% 
  select(datim_uid = orgunituid,
         snu = snu1,
         psnu,
         sitename = facility) %>% 
  arrange(snu, psnu, sitename)


# disa by age
xAge <- readxl::read_excel({file_input}, 
                           sheet = "Age & Sex", 
                           col_types = c("text", "text", "text", "text", "text",
                                         "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric"), 
                           skip = 2) %>% 
  dplyr::mutate(group = "Age") %>%
  glimpse()


# disa pregnant women
xPW <- readxl::read_excel({file_input}, 
                          sheet = "S. Viral (M. Gravidas)",
                          col_types = c("text", "text", "text", "text",
                                        "text", "text", "text", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"), 
                          skip = 2) %>% 
  dplyr::mutate(group = "PW") %>% 
  glimpse


# disa lactating women
xLW <- readxl::read_excel({file_input}, 
                          sheet = "S. Viral (M. Lactantes)",
                          col_types = c("text", "text", "text", "text",
                                        "text", "text", "text", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"),
                          skip = 2) %>% 
  dplyr::mutate(group = "LW") %>%
  glimpse


# disa turn around time
df_tat <- readxl::read_excel({file_input}, 
                             sheet = "TRL - AVG", 
                             col_types = c("text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"), 
                             skip = 2) %>% 
  dplyr::select(-c(TOTAL))


df_vl <- dplyr::bind_rows(xAge, xPW, xLW)

rm(xAge, xPW, xLW)


# CLEAN VL DATAFRAME ---------------------------------------------------------


df_vl <- df_vl %>% 
  dplyr::select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  dplyr::rename(site_nid = `SISMA ID`,
                disa_uid = `DISA ID`,
                snu = PROVINCIA,
                psnu = DISTRITO,
                sitename = `U. SANITARIA`,
                age = Idade,
                sex = Sexo) %>% 
  dplyr::relocate(c(group, disa_uid), .before = sitename) %>% 
  tidyr::pivot_longer(`Rotina (<1000)`:`Motivo de Teste não especificado (>1000)`, names_to = "indicator", values_to = "value") %>% 
  dplyr::mutate(motive = dplyr::case_when(grepl("Rotina", indicator) ~ "Routine",
                                          grepl("Fal", indicator) ~ "Theraputic Failure",
                                          grepl("Repetir", indicator) ~ "Post Breastfeeding",
                                          grepl("Motivo de Teste NS", indicator) ~ "Not Specified"),
                result = dplyr::case_when(grepl("<1000", indicator) ~ "<1000",
                                          grepl(">1000", indicator) ~ ">1000"),
                tat_step = "temp") %>% 
  dplyr::select(-c(indicator)) %>% 
  dplyr::mutate(age = dplyr::recode(age, "Idade não especificada" = "Unknown Age"),
                age = dplyr::recode(age, "No Age Specified" = "Unknown Age"),
                age = dplyr::recode(age, "Não especificada" = "Unknown Age"),
                age = dplyr::recode(age, "Não especificado" = "Unknown Age"),
                age = dplyr::recode(age, "NS" = "Unknown Age"),
                age = dplyr::recode(age, "<1" = "<01"),
                age = tidyr::replace_na(age, "Unknown Age"),
                sex = dplyr::recode(sex, "UNKNOWN" = "Unknown"),
                sex = dplyr::recode(sex, "Not Specified" = "Unknown"),
                sex = dplyr::recode(sex, "Não especificado" = "Unknown"),
                sex = dplyr::recode(sex, "F" = "Female"),
                sex = dplyr::recode(sex, "M" = "Male"),
                sex = tidyr::replace_na(sex, "Unknown")
  ) %>% 
  dplyr::filter(value > 0) %>% 
  dplyr::mutate(indicator = "VL",
                period = {period})


# CLEAN TAT DATAFRAME -----------------------------------------------------


df_tat <- df_tat %>% 
  dplyr::rename(site_nid = `SISMA ID`,
         disa_uid = `DISA ID`,
         snu = PROVINCIA,
         psnu = DISTRITO,
         sitename = `U. SANITARIA`) %>% 
  tidyr::pivot_longer((`COLHEITA À RECEPÇÃO`:`ANÁLISE À VALIDAÇÃO`), names_to = "tat_step", values_to = "value") %>% 
  dplyr::mutate(tat_step = dplyr::recode(tat_step, 
                           "COLHEITA À RECEPÇÃO" = "S1: Collection to Receipt",
                           "RECEPÇÃO AO REGISTO" = "S2: Receipt to Registration",
                           "REGISTO À ANÁLISE" = "S3: Registration to Analysis",
                           "ANÁLISE À VALIDAÇÃO" = "S4: Analysis to Validation"),
         indicator = "TAT",
         period = {period})


disa_vl <- dplyr::bind_rows(df_vl, df_tat)


# SUBSET VLS DATAFRAME ------------------------------------------------------


disa_vls <- disa_vl %>% 
  dplyr::filter(result == "<1000") %>% 
  dplyr::mutate(indicator = "VLS")


# DATAFRAME COMPILE, GROUP, & PIVOT WIDE ----------------------------------


disa <- dplyr::bind_rows(disa_vl, disa_vls) %>% 
  dplyr::mutate(row = row_number(),
         tat_step = dplyr::na_if(tat_step, "temp")) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>% 
  dplyr::group_by(period, snu, psnu, sitename, disa_uid, site_nid, age, group, sex, motive, tat_step) %>%
  dplyr::summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T),
            TAT = sum(TAT, na.rm = T)) %>%
  dplyr::ungroup()

# tabulate sites that have viral load results reported
disa %>% 
  filter(!is.na(disa_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n()) %>% 
  arrange(`n()`)

# tabulate sites missing disa unique identifiers
disa %>% 
  filter(is.na(disa_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n()) %>% 
  arrange(`n()`)


# PRINT MONTHLY OUTPUT ----------------------------------------------------


readr::write_tsv(
  disa,
  {file_monthly_output_local},
  na ="")

drive_put(file_monthly_output_local,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt')
)


# SURVEY MONTHLY DATASETS AND COMPILE ----------------------------


historic_files <- dir({path_historic_output_local}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

disa_temp <- historic_files %>%
  map(~ read_tsv(file.path(path_historic_output_local, .))) %>%
  reduce(rbind)


# JOIN DISA MAPPING -------------------------------------------------------


disa_meta <- disa_temp %>% 
  select(!site_nid) %>% 
  left_join(disa_datim_map, by = c("disa_uid" = "disa_uid")) %>% 
  mutate(ajuda = replace_na(ajuda, 0)) %>% 
  relocate(c(ajuda, sisma_uid, datim_uid), .before = disa_uid)

# number of sites missing datim_uid coding
disa_meta %>% 
  filter(is.na(datim_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n())


# REMOVE ROWS WITHOUT DATIM UID & CLEAN -----------------------------------

disa_final <- disa_meta %>% 
  drop_na(datim_uid) %>%
  select(!c(snu, psnu, sitename)) %>% 
  left_join(datim_orgsuids, by = c("datim_uid" = "datim_uid")) %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>% 
  mutate(
    partner = replace_na(partner, "MISAU"),
    support_type = case_when(
      partner == "MISAU" ~ "Sustainability",
      TRUE ~ as.character("AJUDA")),
    agency = case_when(
      partner == "MISAU" ~ "MISAU",
      partner == "JHPIEGO-DoD" ~ "DOD",
      partner == "ECHO" ~ "USAID",
      TRUE ~ as.character("HHS/CDC")),
    snu = case_when(
      partner == "JHPIEGO-DoD" ~ "_Military",
      TRUE ~ snu),
    psnu = case_when(
      partner == "JHPIEGO-DoD" ~ "_Military",
      TRUE ~ psnu),
    sitename = case_when(
      partner == "JHPIEGO-DoD" ~ "_Military",
      TRUE ~ sitename)) %>% 
  select(period,
         sisma_uid,
         datim_uid,
         site_nid,
         starts_with("his_"),
         snu,
         psnu,
         sitename,
         support_type,
         partner,
         agency,
         age,
         group,
         sex,
         motive,
         tat_step,
         VL,
         VLS,
         TAT) %>% 
  glimpse()


# CHECK RESULTS LOST WHEN FILTERING ON DATIM_UID --------------------------


disa_missing <- disa_meta %>% 
  filter(is.na(datim_uid),
         group == "Age") %>% 
  group_by(period, snu, psnu, sitename, disa_uid) %>% 
  summarize(
    across(c(VL, VLS, TAT), .fns = sum), .groups = "drop")

sum(disa_final$VL, na.rm = T)
sum(disa_missing$VL, na.rm = T)


# PLOT HISTORIC DATA ----------------------------------------------

df_vl_plot <- disa_final %>% 
  filter(!is.na(group)) %>% 
  group_by(period, group, partner, snu) %>% 
  summarize(VL = sum(VL, na.rm = T)) %>% 
  ungroup()

df_vl_plot %>% 
  ggplot(aes(x = period, y = VL, fill = snu)) + 
  geom_col() + 
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text()) + 
  facet_wrap(~group)


# PRINT HISTORIC OUTPUTS ------------------------------------------------------


write.xlsx(disa_missing,
           {"Dataout/DISA/missing_sites_mfl.xlsx"},
           overwrite = TRUE)


readr::write_tsv(
  disa_final,
  {file_historic_output_local},
  na = "")


drive_put(file_historic_output_local,
          path = path_historic_output_gdrive)
