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
path_monthly_output_repo <- "Dataout/TXTB/outdated/" # path to repo where original files are stored
path_new_directory <- "Dataout/TXTB/monthly_processed/" # path to repo where corrected files will be saved


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
  mutate(datim_uid = recode(datim_uid, EjFYleP5G9K = "LqB6YZq9sG2"))


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



# MQ - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/MQ_CV/outdated/" # path to repo where original files are stored
path_new_directory <- "Dataout/MQ_CV/monthly_processed/" # path to repo where corrected files will be saved


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
  mutate(DATIM_code = recode(DATIM_code, EjFYleP5G9K = "LqB6YZq9sG2"))


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
      write_tsv(data, file = glue::glue("{path_new_directory}MQ_{filename}.txt"))
    }
  )


# TPT - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/TPT/outdated/" 
path_new_directory <- "Dataout/TPT/monthly_processed/"


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
  mutate(DATIM_code = recode(DATIM_code, EjFYleP5G9K = "LqB6YZq9sG2"))


# create new repo
dir_create(path_new_directory)


# batch save new files
batch_data_tbl %>% 
  mutate(file_path = Period) %>% 
  group_by(file_path) %>% 
  group_split(.keep = FALSE) %>% 
  map(
    .f = function(data){
      filename <- unique(data$Period) %>% # create a period-based value that will be used for naming the file
        format(., "%Y_%m") # reformat above value to only include year_month with underscore separation
      write_tsv(data, file = glue::glue("{path_new_directory}TPT_{filename}.txt"))
    }
  )


# PREP - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/PrEP/outdated/" 
path_new_directory <- "Dataout/PrEP/monthly_processed/"


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
  mutate(`Datim Code` = recode(`Datim Code`, EjFYleP5G9K = "LqB6YZq9sG2"))


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
      write_tsv(data, file = glue::glue("{path_new_directory}PrEP_{filename}.txt"))
    }
  )

