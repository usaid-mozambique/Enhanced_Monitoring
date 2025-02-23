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

# TXTB SEPT 24 - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/TXTB/monthly_processed_historic_sept24_change/" # path to repo where corrected files will be saved


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
  mutate(disaggregate_sub = NA) |> 
  pivot_longer(cols = TX_CURR:TX_TB_CURR_N, names_to = "indicator", values_to = "value") |> 
  filter(value > 0) |> 
  relocate(disaggregate_sub, .after = disaggregate) |> 
  mutate(disaggregate = case_match(disaggregate, 
                                   "GX" ~ "mWRD",
                                   "LAM" ~ "TBLAM",
                                   .default = disaggregate)) |> 
  mutate(disaggregate_sub = case_when(
    disaggregate == "mWRD" ~ "mWRD",
    disaggregate == "TBLAM" ~ "TBLAM",
    disaggregate == "Other" ~ "Other",
    disaggregate == "Smear" ~ "Smear",
    TRUE ~ NA_character_)
  ) |> 
  mutate(disaggregate = case_match(disaggregate, 
                                   "Already on ART" ~ "Already on ART",
                                   "New on ART" ~ "New on ART",
                                   .default = NA_character_)
  )
  


batch_data_tbl |> 
  distinct(
    disaggregate,
    disaggregate_sub
  )
                                   


batch_data_tbl |> 
  group_by(indicator) %>%
  summarize(total_value = sum(value, na.rm = TRUE))

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


# TPT - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/TPT/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/TPT/monthly_processed_historic_lucio/" # path to repo where corrected files will be saved


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
  select(!c("SISMA_code")) %>% 
  rename(partner = Partner,
         snu = Province,
         psnu = District,
         sitename = `Health Facility`,
         period = Period,
         datim_uid = DATIM_code)

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
      write_tsv(data, file = glue::glue("{path_new_directory}TPT_{filename}.txt"))
    }
  )




# TXTB - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/TXTB/monthly_processed_historic/" # path to repo where corrected files will be saved


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
  select(!c("sisma_nid")) %>% 
  rename(snu = snu1) %>% 
  pivot_longer('TX_CURR':'TX_TB_CURR_N',
               names_to = "indicator",
               values_to = "value")

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



# PREP - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/PrEP/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/PrEP/monthly_processed_historic/" # path to repo where corrected files will be saved


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
  select(!c("No",
            "SISMA Code",
            "Relatorio_period",
            "Relatorio_Date")) %>%
  rename(partner = Partner,
         snu = Province,
         psnu = District,
         sitename = `Health Facility`,
         datim_uid = `Datim Code`) %>% 
  pivot_longer(cols = starts_with("PrEP_"), names_to = "indicator", values_to = "value")

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
      write_tsv(data, file = glue::glue("{path_new_directory}PREP_{filename}.txt"))
    }
  )



# MI - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/MI/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/MI/monthly_processed_historic/" # path to repo where corrected files will be saved


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
  select(!c("No",
            "SISMA_code")) %>%
  rename(partner = Partner,
         snu = Province,
         psnu = District,
         sitename = `Health Facility`,
         datim_uid = DATIM_code,
         period = month)

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
      write_tsv(data, file = glue::glue("{path_new_directory}MI_{filename}.txt"))
    }
  )

