rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
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

file <- glue::glue("TXTB_{dt}")


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
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1zKg8l6bmO_6uk9GoOmxYsAWP3msHtjB3") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_txtb.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved


# METADATA -----------------------------------------------------------

ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "list_ajuda")

ajuda_site_map %>% 
  count(program_phase_ahd)


# FUNCTIONS ---------------------------------------------


txtb_reshape <- function(filename, ip){
  
  df <- readxl::read_excel(filename, 
                           sheet = "TX_TB", 
                           col_types = c("text", "text", "text",
                                         "text", "text", "text", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"), skip = 7) %>% 
    dplyr::filter(partner == ip) %>% 
    dplyr::select(!c(contains(c("remove", "tot")))) %>%
    tidyr::pivot_longer('TX.CURR_newART_Male_<15':'TX.TB.CURR.N_alreadyART_Female_Unk', 
                        names_to = c("indicator", "disaggregate", "sex", "age"), 
                        names_sep = "_", 
                        values_to = "value") %>% 
    dplyr::mutate(period = as.Date(month, "%Y-%m-%d"),
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = dplyr::recode(age, "Unk" = "Unknown"),
                  disaggregate = dplyr::recode(disaggregate, 
                                               "newART" = "New on ART",
                                               "alreadyART" = "Already on ART")) %>% 
    tidyr::pivot_wider(names_from =  indicator, values_from = value)
  
}



# FUNCTIONS RUN -------------------------------------------------

dod <- txtb_reshape(DOD, "JHPIEGO-DoD")
echo <- txtb_reshape(ECHO, "ECHO")
ariel <- txtb_reshape(ARIEL, "ARIEL")
ccs <- txtb_reshape(CCS, "CCS")
egpaf <- txtb_reshape(EGPAF, "EGPAF")
fgh <- txtb_reshape(FGH, "FGH")
icap <- txtb_reshape(ICAP, "ICAP")


# COMPILE DATASETS --------------------------------------------------


txtb <- bind_rows(dod, ariel, ccs, egpaf, icap, echo, fgh)
rm(dod, ariel, ccs, echo, egpaf, fgh, icap)

# detect lines not coded with datim_uids
txtb %>% 
  distinct(datim_uid, snu1, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = c("datim_uid" = "datim_uid"))


# MONTHLY FILE WRITE ------------------------------------

# write to local
readr::write_tsv(
  txtb,
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# HISTORIC DATASET BUILD ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")

txtb_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind)


# HF VOLUME CALCULATE -----------------------


volumn_period <- txtb_tidy_history %>% 
  select(datim_uid, period, TX_CURR) %>%
  filter(period == max(period)) %>% 
  group_by(datim_uid, .drop = TRUE) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(site_volume = case_when(
    TX_CURR < 1000 ~ "Low",
    between(TX_CURR, 1000, 5000) ~ "Medium",
    TX_CURR > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(datim_uid, site_volume) %>% 
  glimpse()


# METADATA JOIN -----------------------


txtb_tidy_history_2 <- txtb_tidy_history %>%
  left_join(ajuda_site_map, by = "datim_uid") %>% 
  left_join(volumn_period) %>% 
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         partner = partner_pepfar_clinical,
         snu,
         psnu = psnu.y,
         sitename = sitename.y,
         grm_sernap,
         cop_entry,
         adv_disease_phase = program_phase_ahd,
         site_volume,
         ends_with("tude"),
         starts_with("support"),
         starts_with("his"),
         sex,
         age,
         disaggregate,
         starts_with("TX_")) %>% 
  glimpse()

# detect lines not coded with datim_uids
txtb_tidy_history_2 %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)


# OUTPUT WRITE ----------------------------------------------


readr::write_tsv(
  txtb_tidy_history_2,
  "Dataout/em_txtb.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)


# PLOTS & TABLES ---------------------------------------------------------------


tbl <- txtb_tidy_history_2 %>%
  pivot_longer(cols = TX_CURR:TX_TB_CURR_N, names_to = "indicator", values_to = "value") %>% 
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
  
  tab_header(title = "Mozambique TX_TB Enhanced Monitoring - 6 Month Trend") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring") 


tbl

