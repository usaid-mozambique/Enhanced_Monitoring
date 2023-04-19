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


# TXTB - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/TXTB/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/TXTB/monthly_processed_age/" # path to repo where corrected files will be saved


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
  mutate(age = recode(age, Unknown = "Unknown Age"))


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
path_new_directory <- "Dataout/PrEP/monthly_processed_age/" # path to repo where corrected files will be saved


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
  mutate(age = recode(age, Unknown = "Unknown Age"))


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

# DSD - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/DSD/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/DSD/monthly_processed_age/" # path to repo where corrected files will be saved


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
  mutate(age = recode(age, 
                      Unknown = "Unknown Age",
                      `<2`    = "<02",
                      `2-4`   = "02-04",
                      `5-9`   = "05-09")
  )


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
      write_tsv(data, file = glue::glue("{path_new_directory}DSD_{filename}.txt"))
    }
  )

# MQ - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/MQ_CV/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/MQ_CV/monthly_processed_age/" # path to repo where corrected files will be saved


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
  mutate(age = recode(age, 
                      `<2 Months` = "<02 Months",
                      `0-2`    = "<02",
                      `0-4`   = "<04",
                      `1-4`   = "01-04",
                      `5-9`   = "05-09",
                      `2-14`   = "02-14",
                      `1-14`   = "01-14",
                      `0-14`   = "<15",
                      `<1`   = "<01")
  )


# create new repo
dir_create(path_new_directory)


# batch save new files
batch_data_tbl %>% 
  mutate(file_path = month) %>% 
  group_by(file_path) %>% 
  group_split(.keep = FALSE) %>% 
  map(
    .f = function(data){
      filename <- unique(data$month) %>% # create a period-based value that will be used for naming the file
        format(., "%Y_%m") # reformat above value to only include year_month with underscore separation
      write_tsv(data, file = glue::glue("{path_new_directory}MQ_CV_{filename}.txt"))
    }
  )

# DISA - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/DISA/monthly_processed/" # path to repo where original files are stored
path_new_directory <- "Dataout/DISA/monthly_processed_age/" # path to repo where corrected files will be saved


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
  mutate(age = recode(age, 
                      `Não especificado` = "Unknown Age")
  )


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
      write_tsv(data, file = glue::glue("{path_new_directory}DISA_{filename}.txt"))
    }
  )
