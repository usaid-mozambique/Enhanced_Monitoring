
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


TXTB_2023_03 <- read_delim("Dataout/TXTB/monthly_processed/TXTB_2023_03.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE) %>% 
  dplyr::select(!c(partner, snu, psnu, sitename)) %>% # strip meta data that will be replaced by sitemap
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
  dplyr::group_by(datim_uid, period, disaggregate, sex, age) %>% 
  summarise(across(starts_with("TX_"), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup()



# TXTB - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/TXTB/monthly_processed_wide/" # path to repo where corrected files will be saved


# batch load files to be processed
batch_list <- path_monthly_output_repo %>% 
  dir_ls() %>% 
  map(
    .f = function(path){
      read_tsv(
        path
      )
    }
  )


# set names, bind rows, and replace data_uids
batch_data_tbl <- batch_list %>% 
  set_names(dir_ls(path_monthly_output_repo)) %>% 
  bind_rows(.id = "file_path") %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
  dplyr::group_by(partner, snu, psnu, sitename, datim_uid, period, disaggregate, sex, age) %>% 
  summarise(across(starts_with("TX_"), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup()
  


# create new repo
dir_create(path_new_directory)


# batch save new files
batch_data_tbl %>% 
  mutate(file_path = period) %>% 
  group_by(file_path) %>% 
  group_split(.keep = FALSE) %>% 
  map(
    .f = function(data){
      filename <- unique(data$period) %>% # create a period-based value that will be used for naming the file
        format(., "%Y_%m") # reformat above value to only include year_month with underscore separation
      write_tsv(data, file = glue::glue("{path_new_directory}TXTB_{filename}.txt"))
    }
  )







df_view_orig <- read_delim("Dataout/TXTB/monthly_processed/TXTB_2023_03.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE) %>% 
  glimpse()




df_view <- read_delim("Dataout/TXTB/monthly_processed_wide/TXTB_2023_03.txt", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE) %>% 
  glimpse()
