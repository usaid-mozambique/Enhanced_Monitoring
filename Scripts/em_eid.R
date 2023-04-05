
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
library(mozR)
load_secrets()


# PATHS & VALUES --------------------------------

month_input <- "2023-03-20"
file_input <- "Data/Disa_new/monthly/Relatorio Mensal de DPI Marco 2023.xlsx"

dt <- base::format(as.Date(month_input), 
                   "%Y_%m")

file <- glue::glue("DISA_DPI_{dt}")


# paths that do not require monthly updating
path_historic_output_local <- "Dataout/DISA/dpi_monthly_processed/"
file_monthly_output_local <- path(path_historic_output_local, file, ext = "txt")
file_historic_output_local <- "Dataout/em_dpi.txt"

path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/1dYjLGeGQVZEXlFcDInx5_1YTtnFPN0gM")
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")


# LOAD DATA ----------------------------

df <- reshape_disa_dpi(filename = file_input, month = month_input)


disa_datim_map <- pull_sitemap(sheet = "map_disa") %>% 
  select(!c(note))


ajuda_site_map <- pull_sitemap() %>% 
  select(datim_uid,
         partner = partner_pepfar_clinical,
         starts_with("his_"))

# SUBMISSION CHECKS -------------------------------------------------------


# tabulate unique sites that have viral load results reported
df %>% 
  filter(!is.na(disa_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n()) %>% 
  arrange(`n()`)

# tabulate sites missing disa unique identifiers
df %>% 
  filter(is.na(disa_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n()) %>% 
  arrange(`n()`)


# PRINT MONTHLY OUTPUT ----------------------------------------------------


readr::write_tsv(
  df,
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


# JOIN DISA MAPPING & CHECK -------------------------------------------------------


disa_meta <- disa_temp %>% 
  left_join(disa_datim_map, by = c("disa_uid" = "disa_uid"), multiple = "first") %>% # two additional lines of data being introduced with left join
  mutate(ajuda = replace_na(ajuda, 0)) %>% 
  relocate(c(ajuda, sisma_uid, datim_uid), .before = disa_uid)

# number of sites missing datim_uid coding
disa_meta %>% 
  filter(is.na(datim_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n())


# MUNGE --------------------------------

# ANALYTICS ----------------------------

# SAVE TO DISK -------------------------

readr::write_tsv(
  disa_meta,
  {file_historic_output_local},
  na = "")

