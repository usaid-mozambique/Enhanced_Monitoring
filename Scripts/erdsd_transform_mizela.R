
# LOAD CORE TIDYVERSE & OTHER PACKAGES ------------------------------------


rm(list = ls())

library(tidyverse)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(scales)
library(lubridate)


df <- read_csv("Data/Ajuda/ERDSD/old/AJUDA_transformed_Mar22.txt")