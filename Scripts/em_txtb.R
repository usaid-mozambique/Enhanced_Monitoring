
# PURPOSE:  Munge and Analysis of
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-04-23
# NOTES: 

## LOCALS & SETUP ------------------------
#

library(tidyverse)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(gt)

rm(list = ls())


## PATHS ------------------------
#

month <- "20/03/2022" # UPDATE EACH MONTH
monthly_dataset <- ("Dataout/TXTB/_CompileHistoric/TXTB_2022_03.csv") # UPDATE EACH MONTH

DOD <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/DOD__Mar_2022 final 20122021 DOD Jhpiego Included Monitoria Intensiva new Template.xlsx"
ARIEL <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ARIEL Monitoria Intensiva_ Template_FY22Q2 11.04.2022.xlsx"
CCS <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/CCS_Monitoria Intensiva_ Template_FY22Q2.xlsx"
ECHO <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/Monitoria Intensiva_ Template_Marco_2022_ECHO_V2.xlsx"
EGPAF <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/EGPAF_Monitoria Intensiva_ Template_FY22Q2 Marco_2022.xlsx"
ICAP <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/ICAP_Marco2022_Monitoria Intensiva_ Template_FY22Q2_12042022.xlsx"
FGH <- "Data/Ajuda/ER_DSD_TPT_VL/2022_03/FGH_MAR_22_Monitoria Intensiva Template FY22_122021_Updated_April 08_2022.xlsx"

historic_files_path <- "Dataout/TXTB/_CompileHistoric/" # DOES NOT REQUIRE UPDATING EACH MONTH


## LOAD DATA ----------------------------
#

ajuda_site_map <- read_excel("~/GitHub/AJUDA_Site_Map/Dataout/AJUDA Site Map.xlsx") %>%
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


## MUNGE ---------------------------------
#


df <- read_excel("Data/Ajuda/ER_DSD_TPT_VL/2022_03/ARIEL Monitoria Intensiva_ Template_FY22Q2 11.04.2022.xlsx", 
                 sheet = "TX_TB", skip = 7) %>% 
  select(!c(No,
            `TX_CURR_tot`,
            `TX_CURR_newonART`,
            `TX_CURR_newonART_Male_<15`,
            `TX_CURR_newonART_Female_<15`,
            `TX_CURR_prevonART`,
            `TX_CURR_prevonART_Male_<15`,
            `TX_CURR_prevonART_Female_<15`,
            `TX_CURR_CLC_tot`,
            `TX_CURR_CLC_newonART`,
            `TX_CURR_CLC_newonART_Male_<15`,
            `TX_CURR_CLC_newonART_Female_<15`,
            `TX_CURR_CLC_prevonART`,
            `TX_CURR_CLC_prevonART_Male_<15`,
            `TX_CURR_CLC_prevonART_Female_<15`,
            `TX_TB_D_tot`,
            `TX_TB_D_newonART`,
            `TX_TB_D_newonART_Male_<15`,
            `TX_TB_D_newonART_Female_<15`,
            `TX_TB_D_prevonART`,
            `TX_TB_D_prevonART_Male_<15`,
            `TX_TB_D_prevonART_Female_<15`,
            `TX_TB_CURR_D_tot`,
            `TX_TB_CURR_D_newonART`,
            `TX_TB_CURR_D_newonART_Male_<15`,
            `TX_TB_CURR_D_newonART_Female_<15`,
            `TX_TB_CURR_D_prevonART`,
            `TX_TB_CURR_D_prevonART_Male_<15`,
            `TX_TB_CURR_D_prevonART_Female_<15`,
            `TX_TB_POS_tot`,
            `TX_TB_POS_newonART`,
            `TX_TB_POS_newonART_Male_<15`,
            `TX_TB_POS_newonART_Female_<15`,
            `TX_TB_POS_prevonART`,
            `TX_TB_POS_prevonART_Male_<15`,
            `TX_TB_POS_prevonART_Female_<15`,
            `TX_TB_NEG_tot`,
            `TX_TB_NEG_newonART`,
            `TX_TB_NEG_newonART_Male_<15`,
            `TX_TB_NEG_newonART_Female_<15`,
            `TX_TB_NEG_prevonART`,
            `TX_TB_NEG_prevonART_Male_<15`,
            `TX_TB_NEG_prevonART_Female_<15`,
            `TX_TB_SpecimentSent`,
            `TX_TB_SpecimentSent_Smear`,
            `TX_TB_SpecimentReturn_POS_Smear`,
            `TX_TB_SpecimentReturn_NEG_Smear`,
            `TX_TB_SpecimentReturn_POS_TBtto_Smear`,
            `TX_TB_N_newonART`,
            `TX_TB_N_newonART_Male_<15`,
            `TX_TB_N_newonART_Female_<15`,
            `TX_TB_N_prevonART`,
            `TX_TB_N_prevonART_Male_<15`,
            `TX_TB_N_prevonART_Female_<15`,
            `TX_TB_CURR_N_tot`,
            `TX_TB_CURR_N_newonART`,
            `TX_TB_CURR_N_newonART_Male_<15`,
            `TX_TB_CURR_N_newonART_Female_<15`,
            `TX_TB_CURR_N_prevonART`,
            `TX_TB_CURR_N_prevonART_Male_<15`,
            `TX_TB_CURR_N_prevonART_Female_<15`,
            `Column1`)) %>%
  filter(Partner == "ARIEL") %>% 
  pivot_longer(cols = `TX_CURR_newonART_Male_15+`:TX_TB_CURR_N_prevonART_Female, names_to = "indicator", values_to = "value") %>% 
  glimpse()



## SAVE TO DISK ---------------------
#
