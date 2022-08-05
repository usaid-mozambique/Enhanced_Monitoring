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
path_monthly_output_repo <- "Dataout/TXTB/outdated/" 
path_new_directory <- "Dataout/TXTB/monthly_processed/"


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
  mutate(file_path = file_path %>% str_replace(path_monthly_output_repo, path_new_directory)) %>% 
  group_by(file_path) %>% 
  group_split() %>% 
  map(
    .f = function(data){
      write_tsv(data, file = unique(data$file_path))
    }
  )
 


# MQ - BATCH PROCESS HISTORIC FILES -------------------------------------

# paths & values
path_monthly_output_repo <- "Dataout/MQ_CV/outdated/" 
path_new_directory <- "Dataout/MQ_CV/monthly_processed/"


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
  mutate(file_path = file_path %>% str_replace(path_monthly_output_repo, path_new_directory)) %>% 
  group_by(file_path) %>% 
  group_split() %>% 
  map(
    .f = function(data){
      write_tsv(data, file = unique(data$file_path))
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
  mutate(file_path = file_path %>% str_replace(path_monthly_output_repo, path_new_directory)) %>% 
  group_by(file_path) %>% 
  group_split() %>% 
  map(
    .f = function(data){
      write_tsv(data, file = unique(data$file_path))
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
  mutate(file_path = file_path %>% str_replace(path_monthly_output_repo, path_new_directory)) %>% 
  group_by(file_path) %>% 
  group_split() %>% 
  map(
    .f = function(data){
      write_tsv(data, file = unique(data$file_path))
    }
  )

