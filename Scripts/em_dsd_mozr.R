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

file <- glue::glue("DSD_{dt}")

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
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/DSD/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/15x2biGIIYrY_eW-zrKQbrvMT47cI5yOE") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_dsd.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved


# METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap()


# FUNCTIONS ---------------------------------------------


dsd_reshape <- function(df, ip) {
  
  df <- readxl::read_excel(df, # function argument
                           sheet = "MDS", 
                           skip = 8) %>% 
    dplyr::select(!c(No, SISMA_code, Period)) %>% 
    tidyr::pivot_longer(remove.1:DSD.AHD__LW_15p, 
                        names_to = c("indicator", "dsd_eligibility", "pop_type", "age"),
                        names_sep = "_",
                        values_to = "value") %>% 
    dplyr::filter(Partner == ip, # function argument
                  !str_detect(indicator, "remove")) %>% 
    dplyr::mutate(period = as.Date(month, "%Y-%m-%d"),
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = stringr::str_replace_all(age, "\\.", "-"),
                  age = dplyr::case_when(age == "15p" ~ "15+",
                                         age == "2u" ~ "<2",
                                         TRUE ~ age),
                  dsd_eligibility = dplyr::recode(dsd_eligibility,
                                                  ELI = "Eligible",
                                                  NEL = "Non-Eligible",
                                                  TOTAL = NA_character_),
                  pop_type = dplyr::recode(pop_type, 
                                           ADULT = "Adult",
                                           PED = "Pediatric")) %>% 
    dplyr::select(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
                  period,
                  indicator,
                  dsd_eligibility,
                  pop_type,
                  age,
                  value)
  
}


# FUNCTIONS RUN -------------------------------------------------


dod <- reshape_em_dsd(DOD, "JHPIEGO-DoD")
echo <- reshape_em_dsd(ECHO, "ECHO")
ariel <- reshape_em_dsd(ARIEL, "ARIEL")
ccs <- reshape_em_dsd(CCS, "CCS")
egpaf <- reshape_em_dsd(EGPAF, "EGPAF")
fgh <- reshape_em_dsd(FGH, "FGH")
icap <- reshape_em_dsd(ICAP, "ICAP")


# COMPILE IP SUMBISSIONS --------------------------------------------------


dsd <- bind_rows(dod, ariel, ccs, echo, egpaf, fgh, icap)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
dsd %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = c("datim_uid" = "datim_uid"))


# WRITE MONTHLY TPT CSV TO DISK ------------------------------------


# write to local
readr::write_tsv(
  dsd,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# SURVEY ALL MONTHLY DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

dsd_tidy_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) 



# METADATA JOIN ---------------------------------


dsd_tidy_historic_2 <- dsd_tidy_historic %>% 
  filter(period <= as.Date(month)) %>% 
  select(-c(partner,
            snu,
            psnu,
            sitename)) %>%
  left_join(ajuda_site_map, by = c("datim_uid" = "datim_uid")) %>%
  glimpse()



# OUTPUT CLEAN -----------------------


dsd_tidy_historic_3 <- dsd_tidy_historic_2 %>%
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner_pepfar_clinical,
         snu,
         psnu,
         sitename,
         ends_with("tude"),
         starts_with("program_"),
         starts_with("his_"),
         indicator,
         pop_type,
         dsd_eligibility,
         age,
         value) %>% 
  mutate(temp_indicator = indicator,
         temp_value = value) %>% 
  pivot_wider(
    names_from = temp_indicator,
    values_from = temp_value)

  
# GT TABLES ---------------------------------------------------------------


tbl <- dsd_tidy_historic_3 %>%
  select(indicator, period, value) %>% 
  filter(period >= month_lag6) %>% 
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
    indicator ~ px(250),
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
  
  tab_header(title = "Mozambique DSD Enhanced Monitoring - 6 Month Trend") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring Reporting") 


tbl



# OUTPUT WRITE ----------------------------------------------


readr::write_tsv(
  dsd_tidy_historic_3,
  "Dataout/em_dsd.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

