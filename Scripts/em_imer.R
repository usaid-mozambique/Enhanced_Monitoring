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
month <- "2022-11-20"
path_monthly_input_repo <- "Data/Ajuda/ER_DSD_TPT_VL/2022_11/"

# do not update each month
dt <- base::format(as.Date(month), 
                   "%Y_%m")

file <- glue::glue("IMER_{dt}")

month_lag6 <- as.Date(month) - months(5) # value for filtering gt table

# update each month
DOD <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_DOD.xlsx")
ARIEL <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov 2022_ARIEL.xlsx")
CCS <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_CCS.xlsx")
ECHO <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_ECHO.xlsx")
EGPAF <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_EGPAF.xlsx")
ICAP <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_ICAP.xlsx")
FGH <- glue::glue("{path_monthly_input_repo}MonthlyEnhancedMonitoringTemplates_FY22_Nov_2022_FGH.xlsx")


# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_monthly_output_repo <- "Dataout/IMER/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12bkLnrQNXbKpbyo-zwk9dmxS6NHDyLwU") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_imer.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------


ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "list_ajuda")

erdsd_var_mapping <- read_excel("Documents/erdsd_var_mapping.xlsx", sheet = "Sheet5")


# FUNCTIONS ---------------------------------------------


imer_reshape <- function(filename, ip){
  
  df <- readxl::read_excel(filename, 
                           sheet = "TX NEW, TX CURR AND IMER", 
                           skip = 8,
                           col_types = "text") %>% 
    dplyr::select(!c(No, SISMA_code, Period)) %>% 
    tidyr::pivot_longer(TX_NEWTot:I4_ER4_40_RetCalc, 
                        names_to = "indicator", 
                        values_to = "value") %>% 
    dplyr::inner_join(erdsd_var_mapping, by = "indicator") %>% 
    dplyr::filter(!indicator_new == "remove") %>% 
    tidyr::separate(indicator_new, 
                    c("indicator", "sex", "age", "pop_type", "dispensation", "numdenom", "er_status", "dsd_eligibility"),
                    sep = "\\.") %>% 
    dplyr::mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))),
                  value = as.numeric(value),
                  period = as.Date(month, "%Y-%m-%d"),
                  indicator = stringr::str_replace_all(indicator, "\\.", "_"),
                  age = stringr::str_replace_all(age, "\\_", "-"),
                  age = dplyr::recode(age,
                                      "unknown" = "Unknown"),
                  sex = dplyr::recode(sex,
                                      "M" = "Male",
                                      "F" = "Female"),
                  key_pop = dplyr::case_when(pop_type == "FSW" ~ "FSW",
                                             pop_type == "MSM" ~ "MSM",
                                             pop_type == "PWID" ~ "PWID",
                                             pop_type == "PPCS" ~ "PPCS"),
                  pop_type = dplyr::recode(pop_type,
                                           "FSW" = "KP",
                                           "MSM" = "KP",
                                           "PWID" = "KP",
                                           "PPCS" = "KP"),
                  pop_type = dplyr::case_when(age %in% c("<15", "<1", "1-4", "5-9", "10-14", "15+", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "50+", "65+", "Unknown") ~ "By Age",
                                              TRUE ~ pop_type),
                  numdenom = tidyr::replace_na(numdenom, "N"),
                  er_status = dplyr::recode(er_status,
                                            "Initiated ART" = NA_character_)) %>% 
    dplyr::filter(Partner == ip) %>% 
    dplyr::select(partner = Partner,
                  snu = Province,
                  psnu = District,
                  sitename = `Health Facility`,
                  datim_uid = DATIM_code,
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
  
  tx_curr_prev <- df %>%
    dplyr::filter(indicator == "TX_CURR") %>% 
    dplyr::mutate(indicator = dplyr::recode(indicator,
                              "TX_CURR" = "TX_CURR_Previous"),
           period = period + months(1))
  
  df <- bind_rows(df, tx_curr_prev)
  
  return(df)
  
}


# FUNCTIONS RUN -------------------------------------------------


dod <- imer_reshape(DOD, "JHPIEGO-DoD")
echo <- imer_reshape(ECHO, "ECHO")
ariel <- imer_reshape(ARIEL, "ARIEL")
ccs <- imer_reshape(CCS, "CCS")
egpaf <- imer_reshape(EGPAF, "EGPAF")
fgh <- imer_reshape(FGH, "FGH")
icap <- imer_reshape(ICAP, "ICAP")


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


tbl <- imer_tidy_historic_3 %>%
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
  imer_tidy_historic_3,
  "Dataout/em_imer.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)
