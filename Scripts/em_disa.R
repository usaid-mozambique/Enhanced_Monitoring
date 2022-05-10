
#------LOAD CORE TIDYVERSE & OTHER PACKAGES-------------------------------------------


library(tidyverse)
library(glamr)
library(glitr)
library(ggthemes)
library(janitor)
library(glue)
library(readxl)
library(openxlsx)

rm(list = ls())


#---- DEFINE PATHS AND VALUES - REQUIREs UPDATING WITH EACH NEW DATASET! -------------------------------------------------------


period <- "2022-03-20"
file_output <- "Dataout/DISA/monthly_processed/2022_03.txt"

file_input <- "Data/Disa_new/monthly/Relatorio de Carga Viral Março 2022.xlsx"
historic_files_path <- "Dataout/DISA/monthly_processed/"
file_output_historic <- "Dataout/em_disa.txt"


#---- LOAD DATASETS AND UNION -------------------------------------------------------

disa_datim_map <- read_excel("Documents/disa_datim_map_MAIO102022.xlsx") %>%
  select(DISA_ID, 
         sisma_uid = `SISMA DHIS2`, 
         datim_uid, 
         ajuda)

datim_ou_map <- read_excel("Documents/tx_site_reference.xlsx")

# DISA BY AGE
xAge <- read_excel({file_input}, 
                   sheet = "Age & Sex", 
                   col_types = c("text", "text", "text", "text",
                                 "text", "text", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"), 
                   skip = 2) %>% 
  mutate(group = "Age") %>%
  glimpse()

# DISA PREGNANT WOMEN
xPW <- read_excel({file_input}, 
                  sheet = "S. Viral (M. Gravidas)",
                  col_types = c("text", "text", "text",
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"), 
                  skip = 2) %>% 
  mutate(group = "PW") %>% 
  rename(US = US,
         PROVINCIA = PROVINCIA,
         DISTRITO = DISTRITO) %>% 
  glimpse

# DISA LACTATING WOMEN
xLW <- read_excel({file_input}, 
                  sheet = "S. Viral (M. Lactantes)",
                  col_types = c("text", "text", "text",
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric"),
                  skip = 2) %>% 
  mutate(group = "LW") %>%
  rename(US = US,
         PROVINCIA = PROVINCIA,
         DISTRITO = DISTRITO) %>% 
  glimpse

# DISA TURN AROUND TIME
df_tat <- read_excel({file_input}, 
                     sheet = "TRL", col_types = c("text", "text", "text",
                                                  "text", "text", "text", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), 
                     skip = 2) %>% 
  select(-c(TOTAL))

df_vl <- bind_rows(xAge, xPW, xLW)

rm(xAge, xPW, xLW)

#---- PROCESS VL DATAFRAME -------------------------------------------------------


df_vl <- df_vl %>% 
  select(-c(`CV < 1000`, `CV > 1000`, TOTAL)) %>%
  rename(site_nid = `SISMA ID`,
         disa_uid = `DISA ID`,
         snu = PROVINCIA,
         psnu = DISTRITO,
         sitename = US,
         age = Idade,
         sex = Sexo) %>% 
  relocate(c(group), .before = sitename) %>% 
  pivot_longer(`Rotina (<1000)`:`Motivo de Teste não especificado (>1000)`, names_to = "indicator", values_to = "value") %>% 
  mutate(motive = dplyr::case_when(grepl("Rotina", indicator) ~ "Routine",
                                   grepl("Fal", indicator) ~ "Theraputic Failure",
                                   grepl("Repetir", indicator) ~ "Post Breastfeeding",
                                   grepl("Motivo de Teste NS", indicator) ~ "Not Specified"),
         result = dplyr::case_when(grepl("<1000", indicator) ~ "<1000",
                                   grepl(">1000", indicator) ~ ">1000"),
         tat_step = "temp") %>% 
  select(-c(indicator)) %>% 

  
#---- RECODE VL AGE/SEX VALUES -----------------------------------------------


mutate(age = recode(age, "Idade não especificada" = "Unknown Age"),
       age = recode(age, "No Age Specified" = "Unknown Age"),
       age = recode(age, "Não especificada" = "Unknown Age"),
       age = recode(age, "NS" = "Unknown Age"),
       age = recode(age, "<1" = "<01"),
       age = replace_na(age, "Unknown Age"),
       
       
       sex = recode(sex, "UNKNOWN" = "Unknown"),
       sex = recode(sex, "Not Specified" = "Unknown"),
       sex = recode(sex, "Não especificado" = "Unknown"),
       sex = recode(sex, "F" = "Female"),
       sex = recode(sex, "M" = "Male"),
       sex = replace_na(sex, "Unknown")
) %>% 

  
#---- FILTER VL LINES ONLY >0 -----------------------------------------------


  filter(value > 0) %>% 
  mutate(indicator = "VL",
         period = {period})


#---- PROCESS TAT DATAFRAME -----------------------------------------------


df_tat <- df_tat %>% 
  rename(site_nid = `SISMA ID`,
         disa_uid = `DISA ID`,
         snu = PROVINCIA,
         psnu = DISTRITO,
         sitename = US) %>% 
  pivot_longer((`COLHEITA À RECEPÇÃO`:`ANÁLISE À VALIDAÇÃO`), names_to = "tat_step", values_to = "value") %>% 
  mutate(tat_step = recode(tat_step, 
                           "COLHEITA À RECEPÇÃO" = "S1: Collection to Receipt",
                           "RECEPÇÃO AO REGISTO" = "S2: Receipt to Registration",
                           "REGISTO À ANÁLISE" = "S3: Registration to Analysis",
                           "ANÁLISE À VALIDAÇÃO" = "S4: Analysis to Validation"),
         indicator = "TAT",
         period = {period})


disa_vl <- bind_rows(df_vl, df_tat)


# CREATE VLS DATASET ------------------------------------------------------


disa_vls <- disa_vl %>% 
  filter(result == "<1000") %>% 
  mutate(indicator = "VLS")


#---- UNION VL & VLS DATAFRAMES, PIVOT WIDER AND GROUP ----------------


disa <- bind_rows(disa_vl, disa_vls) %>% 
  mutate(row = row_number(),
         tat_step = na_if(tat_step, "temp")) %>% 
  pivot_wider(names_from = indicator, values_from = value, values_fill = NULL) %>% 
  group_by(period, snu, psnu, sitename, disa_uid, site_nid, age, group, sex, motive, tat_step) %>%
  summarise(VL = sum(VL, na.rm = T),
            VLS = sum(VLS, na.rm = T),
            TAT = sum(TAT, na.rm = T)) %>%
  ungroup() %>% 
  glimpse()


# PRINT MONTHLY OUTPUT ----------------------------------------------------


readr::write_tsv(
  disa,
  {file_output},
  na ="")


# SURVEY ALL MONTHLY DISA DATASETS AND COMPILE ----------------------------


historic_files <- dir({historic_files_path}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

disa_temp <- historic_files %>%
  map(~ read_tsv(file.path(historic_files_path, .))) %>%
  reduce(rbind)


# JOIN DISA / DATIM MAP TO COMPILED FILE ---------


disa_meta <- disa_temp %>% 
  left_join(disa_datim_map, by = c("disa_uid" = "DISA_ID")) %>% 
  mutate(ajuda = replace_na(ajuda, 0)) %>% 
  relocate(c(ajuda, sisma_uid, datim_uid), .before = disa_uid)


#---- FILTER OUT ROWS WITHOUT DATIM UID AND GROUP DATA -------------------------------


disa_final <- disa_meta %>% 
  drop_na(datim_uid) %>%
  group_by(period, sisma_uid, datim_uid, site_nid, age, group, sex, motive, tat_step) %>% 
  summarise(VL = sum(VL),
            VLS = sum(VLS),
            TAT = sum(TAT)) %>%
  left_join(datim_ou_map, by = c("datim_uid" = "orgunituid")) %>% 
  mutate(support_type = case_when(
    clinical_partner == "MISAU" ~ "Sustainability",
    TRUE ~ as.character("AJUDA"))) %>% 
  select(period,
         sisma_uid,
         datim_uid,
         site_nid,
         snu = snu1,
         psnu,
         sitename,
         support_type,
         partner = clinical_partner,
         agency = clinical_funding_agency,
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
    across(c(VL, VLS, TAT), .fns = sum), .groups = "drop"
  )

sum(disa_final$VL, na.rm = T)
sum(disa_missing$VL, na.rm = T)


# GRAPH HISTORIC DATA FOR QC ----------------------------------------------

df_vl_plot <- disa_final %>% 
  filter(group == "Age") %>% 
  group_by(period, partner, snu) %>% 
  summarize(VL = sum(VL, na.rm = T)) %>% 
  ungroup()

df_vl_plot %>% 
  ggplot(aes(x = period, y = VL, color = partner)) + 
  geom_col() + 
  labs(title = "TX_CURR Trend by Partner",
       subtitle = "Historical Trend of Patients on ART in Mozambique by PEPFAR Partner",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text())


# PRINT OUTPUTS TO DISK ------------------------------------------------------


write.xlsx(disa_missing,
           {"Dataout/DISA/missing_sites_mfl_mar22.xlsx"},
           overwrite = TRUE)


readr::write_tsv(
  disa_final,
  {file_output_historic},
  na = "")


