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

file <- glue::glue("PREP_{dt}")

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
path_monthly_output_repo <- "Dataout/PrEP/monthly_processed/" # folder path where monthly dataset archived
path_monthly_output_file <- path(path_monthly_output_repo, file, ext = "txt") # composite path/filename where monthly dataset saved
path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1BYq-xdMhxw8sOUHYwFiZH8_w2UsQmBun") # google drive folder where monthly dataset saved 
path_historic_output_file <- "Dataout/em_prep.txt" # folder path where monthly dataset archived
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------


ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "list_ajuda")


ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "Sheet1") %>%
  select(sisma_uid = sisma_id,
         datim_uid =  orgunituid,
         site_nid,
         partner = `IP FY20`,
         snu = SNU,
         psnu = Psnu,
         sitename = Sitename,
         his_epts = epts,
         his_emr = emr,
         his_idart = idart,
         his_disa = disa,
         support_ovc = ovc,
         support_ycm = ycm,
         ovc,
         ycm,
         latitude = Lat,
         longitude = Long)


# FUNCTIONS ---------------------------------------------

prep_reshape <- function(filename, ip){
  
  df <- read_excel(filename, # Function argument
                   sheet = "Resumo Mensal de PrEP", 
                   col_types = c("text", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "text", "text", "numeric", 
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
                                 "numeric", "numeric", "numeric"), 
                   skip = 7) %>% 
    filter(Partner == ip) %>%  # Function argument
    select(!c(Elegible.to.PrEP_All_Total_Total,
              Elegible.to.PrEP_Casos.Especiais_Male_Total,
              Elegible.to.PrEP_Casos.Especiais_Female_Total,
              Elegible.to.PrEP_All_PW_Total,
              Elegible.to.PrEP_All_LW_Total,
              PrEP.NEW_All_Total_Total,
              PrEP.NEW_Casos.Especiais_Male_Total,
              PrEP.NEW_Casos.Especiais_Female_Total,
              PrEP.NEW_All_PW_Total,
              PrEP.NEW_All_LW_Total,
              PrEP.New.Who.RTT_All_Total_Total,
              PrEP.New.Who.RTT_Casos.Especiais_Male_Total,
              PrEP.New.Who.RTT_Casos.Especiais_Female_Total,
              PrEP.New.Who.RTT_All_PW_Total,
              PrEP.New.Who.RTT_All_LW_Total,
              PrEP.CT_All_Total_Total,
              PrEP.CT_Casos.Especiais_Male_Total,
              PrEP.CT_Casos.Especiais_Female_Total,
              PrEP.CT_All_PW_Total,
              PrEP.CT_All_LW_Total,
              PrEP.CT.3months_All_Total_Total,
              PrEP.CT.3months_Casos.Especiais_Male_Total,
              PrEP.CT.3months_Casos.Especiais_Female_Total,
              PrEP.CT.3months_All_PW_Total,
              PrEP.CT.3months_All_LW_Total)) %>%
    pivot_longer('Elegible.to.PrEP_Casos.Especiais_Male_10.14':'PrEP.CT.3months_TP_People.who.Injected.Drugs_Total', 
                 names_to = c("indicator", "pop_type", "disaggregate", "age"), 
                 names_sep = "_", 
                 values_to = "value") %>%
    mutate(period = as.Date(month, "%Y-%m-%d"),
           indicator = str_replace_all(indicator, "\\.", "_"),
           indicator = str_replace_all(indicator, "Elegible_to_PrEP", "PrEP_Eligible"),
           indicator = str_replace_all(indicator, "PrEP_New_Who_RTT", "PrEP_NEW_RTT"),
           age = str_replace_all(age, "\\.", "-"),
           age = str_replace_all(age, "Total", "Unknown"),
           sex = case_when(
             disaggregate == "Female" ~ "Female",
             disaggregate == "Male" ~ "Male",
             TRUE ~ as.character("Unknown")),
           pop_type = recode(pop_type, 
                             "Casos.Especiais" = "Special Cases",
                             "TP.AtRisk" = "TP at Risk",
                             "All" = "PLW"),
           disaggregate = recode(disaggregate,
                        "Long.Distance.driver" = "Long Distance Drivers",
                        "military" = "Military",
                        "miner" = "Miners",
                        "People.who.Injected.Drugs" = "PWID",
                        "Sero.Discordante.Couples" = "Sero-Discordante Couples",
                        "Sex.workers" = "Sex Workers",
                        "TG" = "Transgender")) %>% 
    pivot_wider(names_from =  indicator, values_from = value)
  
}

# FUNCTIONS RUN -------------------------------------------------


dod <- prep_reshape(DOD, "JHPIEGO-DoD")
echo <- prep_reshape(ECHO, "ECHO")
ariel <- prep_reshape(ARIEL, "ARIEL")
ccs <- prep_reshape(CCS, "CCS")
egpaf <- prep_reshape(EGPAF, "EGPAF")
fgh <- prep_reshape(FGH, "FGH")
icap <- prep_reshape(ICAP, "ICAP")


# COMPILE DATASETS --------------------------------------------------


prep <- bind_rows(dod, echo, ariel, ccs, egpaf, fgh, icap)
rm(dod, echo, ariel, ccs, egpaf, fgh, icap)

# detect lines not coded with datim_uids
prep %>% 
  distinct(`Datim Code`, Province, District, `Health Facility`) %>% 
  anti_join(ajuda_site_map, by = c("Datim Code" = "datim_uid"))


# MONTHLY FILE WRITE ------------------------------------

# write to local
readr::write_tsv(
  prep,
  na = "",
  {path_monthly_output_file})

# write to google drive
drive_put(path_monthly_output_file,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt'))


# HISTORIC DATASET BUILD ---------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

historic_import <- historic_files %>%
  map(~ read_tsv(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind)


# METADATA JOIN ---------------------------------

prep_tidy_historic <- historic_import %>% 
  select(-c(No,
            Partner,
            Province,
            District,
            `Health Facility`,
            `SISMA Code`,
            Relatorio_period,
            Relatorio_Date)) %>%
  mutate(across(starts_with('PrEP_'), ~ replace_na(., 0))) %>% 
  left_join(ajuda_site_map, by = c("Datim Code" = "datim_uid")) %>%
  rename(datim_uid = `Datim Code`) %>%
  glimpse()


# OUTPUT CLEAN -----------------------


prep_tidy_historic_2 <- prep_tidy_historic %>%
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
         pop_type,
         disaggregate,
         sex,
         age,
         starts_with("PrEP_")) %>% 
  glimpse()


# OUTPUT WRITE ----------------------------------------------

# write to local
readr::write_tsv(
  prep_tidy_historic_2,
  "Dataout/em_prep.txt")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)


# PLOTS & TABLES ---------------------------------------------------------------

tbl <- prep_tidy_historic_2 %>%
  pivot_longer(cols = PrEP_Eligible:PrEP_CT_3months, names_to = "indicator", values_to = "value") %>% 
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
  
  tab_header(title = "Mozambique PrEP Enhanced Monitoring") %>% 
  tab_source_note("Source: AJUDA Enhanced Monitoring") 


tbl


