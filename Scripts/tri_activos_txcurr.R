
# PURPOSE:
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-11-18
# NOTES: 

# DEPENDENCIES & SETUP -----------------

library(tidyverse)
library(openxlsx)
library(glamr)
library(glitr)
library(readxl)
library(janitor)
library(glue)
library(ggthemes)

# PATHS --------------------------------


path_activos_sisma <- "Data/MISAU/CT/Activos TARV 09.2022 V28102022.xlsx"
path_activos_epts <- "Dataout/em_dsd.txt"


# LOAD FUNCTIONS -----------------------

# LOAD DATA ----------------------------


df_activos_sisma <- read_excel(path_activos_sisma, 
                         sheet = "ACTIVOS POR US", 
                         skip = 4) %>% 
  clean_names() %>% 
  select(sisma_uid = sis_ma_id,
         TX_ACTIVOS_SISMA = x15)


df_activos_epts <- read_tsv(path_activos_epts) %>% 
  filter(indicator == "TX_ACTIVE",
         period == "2022-09-20") %>% 
  select(datim_uid, sisma_uid, site_nid, snu, psnu, sitename, value) %>% 
  group_by(datim_uid, sisma_uid, site_nid, snu, psnu, sitename, .drop = TRUE) %>% 
  summarize(TX_ACTIVOS_EPTS = sum(value, na.rm = TRUE))


sum(df_txcurr$TX_CURR, na.rm =T)

df_triangulation <- df_activos_epts %>% 
  left_join(df_activos_sisma, by = "sisma_uid") %>% 
  glimpse()


# MUNGE --------------------------------

# ANALYTICS ----------------------------

# SAVE TO DISK -------------------------

readr::write_tsv(
  df_triangulation,
  "Dataout/MISAU/tri_activos.txt")
