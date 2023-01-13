rm(list = ls())

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


# VALUES & PATHS ---------------------------

# update each month
month <- "2022-12-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2022_12/"

# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("IMER_{dt}")

month_lag6 <- as.Date(month) - months(5) # value for filtering gt table

# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates Dez 2022_FY23Q1_FGH.xlsx")


# do not update each month
path_monthly_output_repo <- "Dataout/IMER/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_imer.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap()

erdsd_var_mapping <- read_excel("Documents/erdsd_var_mapping.xlsx", sheet = "Sheet5")



# FUNCTIONS RUN -------------------------------------------------


dod <- reshape_em_imer(DOD, "JHPIEGO-DoD")
echo <- reshape_em_imer(ECHO, "ECHO")
ariel <- reshape_em_imer(ARIEL, "ARIEL")
ccs <- reshape_em_imer(CCS, "CCS")
egpaf <- reshape_em_imer(EGPAF, "EGPAF")
fgh <- reshape_em_imer(FGH, "FGH")
icap <- reshape_em_imer(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


imer <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
imer %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = c("datim_uid" = "datim_uid"))


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


# write to local
readr::write_tsv(
  imer,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# SURVEY ALL MONTHLY DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

imer_tidy_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) 


# CALCULATE INDICATORS ----------------------------------------------------

# imer_tidy_historic_2 <- imer_tidy_historic %>% 
#   mutate

clean_em_imer <- function(df){
  
  imer_tidy_historic_2 <- imer_tidy_historic %>% 
    dplyr::filter(period <= as.Date(month)) %>% 
    dplyr::select(-c(partner,
              snu,
              psnu,
              sitename)) %>%
    dplyr::left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>% 
    dplyr::select(datim_uid,
           sisma_uid,
           site_nid,
           period,
           partner = partner_pepfar_clinical,
           snu,
           psnu,
           sitename,
           grm_sernap,
           cop_entry,
           ends_with("tude"),
           starts_with("program_"),
           starts_with("his_"),
           indicator,
           numdenom,
           pop_type,
           key_pop,
           dispensation,
           er_status,
           dsd_eligibility,
           sex,
           age,
           value) %>% 
    dplyr::mutate(temp_indicator = indicator,
           temp_value = value) %>% 
    tidyr::pivot_wider(
      names_from = temp_indicator,
      values_from = temp_value
    )
  
}


imer_tidy_historic_2 <- clean_em_imer(imer_tidy_historic)

# METADATA JOIN ---------------------------------

imer_tidy_historic_2 <- imer_tidy_historic %>% 
  filter(period <= as.Date(month)) %>% 
  select(-c(partner,
            snu,
            psnu,
            sitename)) %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) 


# OUTPUT CLEAN -----------------------


imer_tidy_historic_3 <- imer_tidy_historic_2 %>%
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner_pepfar_clinical,
         snu,
         psnu,
         sitename,
         grm_sernap,
         cop_entry,
         ends_with("tude"),
         starts_with("program_"),
         starts_with("his_"),
         indicator,
         numdenom,
         pop_type,
         key_pop,
         dispensation,
         er_status,
         dsd_eligibility,
         sex,
         age,
         value) %>% 
  mutate(temp_indicator = indicator,
         temp_value = value) %>% 
  pivot_wider(
    names_from = temp_indicator,
    values_from = temp_value
  )




# PLOTS & TABLES ---------------------------------------------------------------


tbl <- imer_tidy_historic_2 %>%
  filter(period >= month_lag6) %>% 
  select(indicator, period, value) %>% 
  arrange((period)) %>% 
  mutate(row_n = row_number(),
         period = as.character(period, format = "%b %y")) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  group_by(indicator) %>%
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  gt(rowname_col = "indicator") %>% 
  
  fmt_number(
    columns = !c(indicator), 
    rows = everything(),
    sep_mark = ",",
    decimals = 0) %>% 
  
  cols_width(
    indicator ~ px(200),
    everything() ~ px(100)) %>% 
  
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1),),
    locations = cells_body(
      columns = everything(),
      rows = everything())) %>% 
  
  tab_options(
    table.font.size = 18,
    table.font.names = "SourceSansPro-Regular",
    footnotes.font.size = 8) %>% 
  
  tab_header(title = "Mozambique TX & ER Enhanced Monitoring - 6 Month Trend") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring") 


tbl




# OUTPUT WRITE ----------------------------------------------


readr::write_tsv(
  imer_tidy_historic_2,
  "Dataout/em_imer.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)
