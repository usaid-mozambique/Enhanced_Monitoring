
# PURPOSE:  Munge and Analysis of Passos Monthly Data
# AUTHOR:  Joe Lara | USAID
# DATE: 2021-06-01
# NOTES: 

## LOCALS & SETUP ------------------------
#

library(tidyverse)
library(glamr)
library(glitr)
library(readxl)
library(janitor)
library(glue)
library(filenamer)

## LOAD DATA ----------------------------
#

passos <- read_excel("Data/Passos/PASSOS FY2021 Tracker Mensal_Abril 05052021.xlsx", 
                     sheet = "Dados e Metas PC", skip = 2)

## MUNGE ---------------------------------
#

passos_formatted <- passos  %>%  
  filter(`Data Type` == "Result")  %>%
  
  # RENAME VARIABLES
  
  dplyr::mutate(
    SNU = Province,
    PSNU = District, 
    FY = `Fiscal Year`, 
    TX_PVLS_VERIFY_D = `TX_PVLS_VERIFY (Denominator)`,
    TX_PVLS_VERIFY_N = `TX_PVLS_VERIFY (Numerator)`,
    TX_NEW_VERIFY = KP_TX_NEW_VERIFY, 
    TX_CURR_VERIFY = KP_TX_CURR_VERIFY,
    TX_RTT_VERIFY = KP_TX_RTT_VERIFY,	
    TX_PVLS_ELIGIBLE = KP_TX_VL_ELIGIBLE  
  ) %>%
  
  # PIVOT INDICATOR VALUES TO LONG
  
  tidyr::pivot_longer(c(TX_NEW_VERIFY, TX_CURR_VERIFY, COMM_SUPP_RET, TX_RTT_VERIFY,
                        TX_PVLS_ELIGIBLE, TX_PVLS_VERIFY_D,	TX_PVLS_VERIFY_N,
                        PrEP_SCREEN,	PrEP_ELIGIBLE,	PrEP_NEW_VERIFY,	PrEP_CURR_VERIFY, KP_PREV)) %>%
  dplyr::mutate(
    # RECODE PIVOT OUTPUT
    indicator = name,
    val = as.numeric(value),
    
    # RECODE KEY POPULATIONS FOR CIRG
    Population=recode(`Key Population`,
                      "FSW" = "Female sex workers (FSW)",
                      "MSM" = "Men who have sex with men (MSM)",
                      "PWID" = "People who inject drugs (PWID)",
                      "TG" = "Transgender people (TG)",
                      "Prisoners" = "People in prisons and other closed settings",
                      "Non-KP" = "Non-KP (general population)")) %>% 
  
  # SUBSET FOR ONLY REQUIRED FIELDS
  select (SNU, PSNU, FY, Quarter, Month, Population, indicator, val)  %>% 
  
  # RECODE DISTRICTS TO DATIM SPELLED NAMES
  mutate(PSNU = recode(PSNU,
                       "Chongoene" = "Chonguene",
                       "MuchunguÃ©" = "Chibabava" #these are not in Datim, must be recoded (see 2 steps below)
  ))  %>%
  
  # ADD MECHANISM, PARTNER AND OU INFO
  mutate(orgunit = PSNU,
         mech_code	= 18280,
         partner= "FHI360",
         operatingunit = "Mozambique",
         age = NA,
         sex = NA,
         otherdisaggregate = NA) %>%
  
  # CODE DISTRICTS TO KPIF
  mutate(fundingsource = recode(PSNU,
                                "Chimoio" = "KPIF",
                                "Kamavota" = "KPIF",
                                "Kampfumu" = "KPIF",
                                "Kamubukwana" = "KPIF",
                                "Matola" = "KPIF",
                                "Nacala" = "KPIF",
                                "Nampula" = "KPIF",
                                "Nlhamankulu" = "KPIF",
                                "Quelimane" = "KPIF",
                                .default = "COP"),
         
         # CREATE NUMERATOR / DENOMINATOR FIELD
         numdenom = recode(indicator,
                           "TX_PVLS_VERIFY_D" = "Denominator",
                           .default = "Numerator"
         ),
         
         # RENAME INDICATOR FIELDS AFTER N/D DISTINGUIS ELEMENTS
         indicator = recode(indicator,
                            "TX_PVLS_VERIFY_D" = "TX_PVLS_VERIFY",
                            "TX_PVLS_VERIFY_N" = "TX_PVLS_VERIFY",
         ),
         
         # CREATE FIELD TO SPECIFY AGGREGATION/REPORTING
         reporting_agg = recode(indicator,
                                "TX_CURR_VERIFY" = "Quarterly",
                                "TX_PVLS_VERIFY" = "Quarterly",
                                .default = "Monthly"),
         
         # GENERATE FISCAL YEAR FOR FOR FILTERING AND REPORT GENERATION
         period = recode(FY,
                         "2018" = "FY18",
                         "2019" = "FY19",
                         "2020" = "FY20",
                         "2021" = "FY21",
                         "2022" = "FY22",
                         "2023" = "FY23",
                         "2024" = "FY24",
                         "2025" = "FY25")) %>%
  
  filter(!is.na(val)) %>%
  
  # CREATE REPORTING FIELD, MONTH NAME
  mutate(reportingperiod = paste(period, Quarter, sep = " "))  %>% 
  
  
  `names<-`(tolower(names(.))) %>%   # CHANGE FIELD NAMES TO LOWER CASE
  
  mutate(month_n = match(month,month.name))  %>% glimpse()
## SAVE TO DISK ---------------------
#
