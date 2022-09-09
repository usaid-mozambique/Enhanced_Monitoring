#------- Choose Quarter and FY for Custom Indicator Export -----------------------

#SELECT QUARTER
Q_choose <- "Q4"

#SELECT FY
FY_choose <- 2022

#Select Reporting period
Current_Reporting_Month <- 07

#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(readxl)
library(openxlsx)
library(glue)
library(janitor)
library(filenamer)
library(lubridate)

#Joe's read files
#setwd("~/")

passos <- read_excel("Data/Passos/PASSOS_FY2022_Tracker_Mensal_Julho16082022.xlsx",
                     sheet = "Dados e Metas PC", skip = 2)
glimpse(passos) #review data structure

psnuuid_lookup <- read_excel("Documents/psnu_psnuuid.xlsx") %>%
  dplyr::rename(PSNU = Psnu)

#Bourke's read files
# Data input from PASSOS tracker ------------------------------------------
# setwd("~/KP Dashboard/Data Preparation/Custom Indicator Reporting/Mozambique")
# passos <- read_excel("Data/PASSOS_FY2021_Tracker_Mensal_Junho_08072021.xlsx",
#                      sheet = "Dados e Metas PC", skip = 2)
# glimpse(passos) #review data structure
# 
# psnuuid_lookup <- read_excel("Data/psnu_psnuuid.xlsx") %>%
#   dplyr::rename(PSNU = Psnu)


#------- PROCESS PASSOS TRACKER -----------------------

unique(passos$Month)

passos_format <- passos  %>%  
  dplyr::filter(`Data Type` == "Result") %>%
  dplyr::mutate(
    SNU = Province,
    PSNU = District, 
    FY = `Fiscal Year`, 
    TX_PVLS_VERIFY_D = `TX_PVLS_VERIFY (Denominator)`,
    TX_PVLS_VERIFY_N = `TX_PVLS_VERIFY (Numerator)`,
    TX_NEW_VERIFY = KP_TX_NEW_VERIFY, 
    TX_CURR_VERIFY = KP_TX_CURR_VERIFY,
    TX_RTT_VERIFY = KP_TX_RTT_VERIFY,	
    TX_PVLS_ELIGIBLE = KP_TX_VL_ELIGIBLE,
    HTS_TST = KP_TST,
    HTS_TST_POS = KP_POS,
    date = Month,
    month = months(Month)
    ) %>%
  tidyr::pivot_longer(c(KP_PREV, HTS_TST, HTS_TST_POS, TX_NEW_VERIFY, TX_CURR_VERIFY, COMM_SUPP_RET, TX_RTT_VERIFY,
                 TX_PVLS_ELIGIBLE, TX_PVLS_VERIFY_D,	TX_PVLS_VERIFY_N,
                 PrEP_SCREEN,	PrEP_ELIGIBLE,	PrEP_NEW_VERIFY,	PrEP_CURR_VERIFY)) %>%
  dplyr::mutate(indicator = name,
         val = as.numeric(value),
         Population = recode(`Key Population`,
                           "FSW" = "Female sex workers (FSW)",
                           "MSM" = "Men who have sex with men (MSM)",
                           "PWID" = "People who inject drugs (PWID)",
                           "TG" = "Transgender people (TG)",
                           "Prisoners" = "People in prisons and other closed settings",
                           "Non-KP" = "Non-KP (general population)"),
         Population = replace_na(Population, "Non-KP (general population)")) %>%
  dplyr::select (SNU, PSNU, FY, Quarter, month, date, Population, indicator, val) %>%
  dplyr::mutate(date = if_else(Quarter == "Q1" & FY == year(date), make_date(year(date)-1, month(date), day(date)), make_date(year(date), month(date), day(date)))) %>%
  arrange(desc(date)) %>%
  glimpse()

passos_check <- passos_format %>% 
  filter(FY == 2022 & Quarter == "Q1") %>% glimpse()
                
table(passos_format$date)


#------- CONTINUE PROCESSING PASSOS TRACKER -----------------------

passos_format_2 <- passos_format %>%  
  mutate(orgunit = PSNU,
         mech_code	= 18280,
         partner= "FHI360",
         operatingunit = "Mozambique",
         age = NA,
         sex = NA,
         otherdisaggregate = if_else(str_detect(indicator, "TX") & str_detect(indicator, "VERIFY"), "Site Support Type: PEPFAR supported", "")) %>%
  mutate(PSNU = recode(PSNU,
                        "Chongoene" = "Chonguene",
                        "Muchungué" = "Chibabava"), # RENAME THESE DISTRICTS WHICH ARE SPELLED DIFFERENTLY IN DATIM
         fundingsource = recode(PSNU,
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
         numdenom = recode(indicator,
                    "TX_PVLS_VERIFY_D" = "Denominator",
                    .default = "Numerator"),
         indicator = recode(indicator,
                   "TX_PVLS_VERIFY_D" = "TX_PVLS_VERIFY",
                   "TX_PVLS_VERIFY_N" = "TX_PVLS_VERIFY"),
         aggregation = recode(indicator,
                                "TX_CURR_VERIFY" = "Quarterly",
                                "TX_PVLS_VERIFY" = "Quarterly",
                                .default = "Monthly"),
         period = recode(FY,
                "2018" = "FY18",
                "2019" = "FY19",
                "2020" = "FY20",                
                "2021" = "FY21",
                "2022" = "FY22",
                "2023" = "FY23",
                "2024" = "FY24",
                "2025" = "FY25"),
         reportingperiod = paste(period, Quarter, sep = " ")) %>% 
  
  dplyr::left_join(psnuuid_lookup, by = "PSNU") %>% 
  dplyr::rename_with(tolower, .cols = everything()) %>% 
  glimpse()


# Clean up mismatched dates -----------------------------------------------
passos_format_3 <- passos_format_2 %>%
  mutate(date = if_else(fy==2021 & date=="2020-01-01" & psnu =="Chiure", date + 365, date)) %>% #corrections for mislabeled FY and years
  glimpse()

table(passos_format_3$date)



# Lag TX_CURR to get previous value ---------------------------------------
passos_format_4 <- passos_format_3 %>%
  filter(indicator=="TX_CURR_VERIFY",
         val!= "NA") %>%
  mutate(date= if_else(month(date)==12, make_date(year(date)+1,3,1), make_date(year(date),month(date)+3,1)),
         quarter=if_else(quarter=="Q4", "Q1", if_else(quarter=="Q1", "Q2", if_else(quarter=="Q2", "Q3", if_else(quarter=="Q3", "Q4", "NA")))),
         fy = if_else(month(date)>9,year(date)+1,year(date)),
         period = recode(fy,
                         "2018" = "FY18",
                         "2019" = "FY19",
                         "2020" = "FY20",                
                         "2021" = "FY21",
                         "2022" = "FY22",
                         "2023" = "FY23",
                         "2024" = "FY24",
                         "2025" = "FY25"),
         reportingperiod = paste(period, quarter, sep = " "),
         indicator= "Previous_TX_CURR") %>%  
  filter(date <= make_date(FY_choose,Current_Reporting_Month,01)) %>%
  glimpse()  

table(passos_format_4$reportingperiod)


# Append/rbind Previous TX CURR and the rest of the data ------------------
em_kp_appended <- rbind(passos_format_3, passos_format_4)



#------- CREATE monthly data set and pivot to wide format  -----------------------

em_kp_monthly <- em_kp_appended  %>%
  group_by(fundingsource, fy, reportingperiod, date, month, snu, orgunit,	mech_code,	partner,	operatingunit,	psnu, psnuuid, indicator, numdenom, population,	otherdisaggregate) %>%
  summarise(val = sum(val), .groups = 'drop') %>%
  ungroup() %>% 
  filter(val!= "NA") %>%
    dplyr::rename(FY = fy, Province = snu, District = psnu, Orgnunituid = psnuuid, NumDen = numdenom, Date = date, Month = month) %>%
    dplyr::mutate(Partner = "PASSOS",
                  PatientType = if_else(!str_detect(population, "Non-KP"), "Key Populations", population),
                  KeyPop = if_else(str_detect(population, "prison"), "Prisoners", if_else(PatientType == "Key Populations", str_extract(population, "(?<=\\().*(?=\\))"), population))) %>%
    dplyr::select(Partner, Province, District, Orgnunituid, fundingsource, NumDen, PatientType, KeyPop, FY, Date, Month, indicator, reportingperiod, KeyPop, val) %>%
######## --------------- Pivot to Wide format ----------------
       mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = indicator, values_from = val, values_fill = 0) %>%
  select(-row) %>%
  group_by(Partner, Province, District, Orgnunituid, fundingsource, NumDen, PatientType, KeyPop, FY, Date, Month, reportingperiod) %>%
  summarise(HTS_TST = sum(HTS_TST), HTS_TST_POS = sum(HTS_TST_POS), KP_PREV = sum(KP_PREV), TX_NEW_VERIFY = sum(TX_NEW_VERIFY), TX_CURR_VERIFY = sum(TX_CURR_VERIFY), Previous_TX_CURR_VERIFY = sum(Previous_TX_CURR),
            TX_PVLS_ELIGIBLE = sum(TX_PVLS_ELIGIBLE), TX_PVLS_VERIFY = sum(TX_PVLS_VERIFY), TX_RTT_VERIFY = sum(TX_RTT_VERIFY), 
            PrEP_NEW_VERIFY = sum(PrEP_NEW_VERIFY), PrEP_ELIGIBLE = sum(PrEP_ELIGIBLE), PrEP_SCREEN = sum(PrEP_SCREEN),
            .groups = 'drop') %>% 
  ungroup() %>%
  glimpse()



# em_kp_monthly_check <- em_kp_monthly %>%
#   filter(FY==2021 & Date=="2020-01-01")
  


# Name Custom INidcator File to be exported ------------------------------------------------
  filename <- "em_kp.txt"
  filename <- tag(filename)
  filename <- set_fpath(filename, "Dataout")
  
  filename
  
  #------- PRINT WIDE PROGRAM DATA TO DESK -----------------------
  
  readr::write_tsv(
    em_kp_monthly,
    filename,
    na ="")
  
  
  ################################################
  ################################################
  ################################################
  
  #------- Create list and function for filtering out MER data from CI submission -----------------------
  MER <- c('KP_PREV','HTS_TST','HTS_TST_POS') #establish string to match against
  `%notin%` <- Negate(`%in%`) #create function for inverse of %in%
  
  #------- FILTER PERIOD TO CIRG AND GROUP VALUES BY QUARTER -----------------------
  cirg_kp <- passos_format_3  %>%
    dplyr::mutate(IndicatorType = case_when(indicator %notin% MER ~ "Custom")) %>%
    dplyr::filter(val!= "NA", IndicatorType == "Custom", fy == FY_choose, quarter == Q_choose) %>%
    group_by(reportingperiod,	orgunit,	psnuuid,	mech_code,	partner,	operatingunit,	psnu, indicator,	sex,	age,	population,	otherdisaggregate,	numdenom) %>%
    summarise(val = sum(val), .groups = 'drop') %>% 
    ungroup() %>% 
    dplyr::select(reportingperiod, orgunit, orgnunituid = psnuuid, mech_code, partner, operatingunit,	psnu, indicator,	sex,	age,	population,	otherdisaggregate,	numdenom, val) %>%
    glimpse()
  
  #------- Name File -----------------------
  FY_chosen <- head(str_extract(cirg_kp$reportingperiod, "^.*(?= Q)"), 1)
  FY_chosen
  export_date <- format(Sys.Date(), "%Y%m%d")
  export_date
  
  filename_ci <- "CIRG.xlsx"
  filename_ci <- tag(filename_ci, FY_chosen)
  filename_ci <- tag(filename_ci, Q_choose)
  filename_ci <- tag(filename_ci, "Mozambique_PASSOS")
  filename_ci <- tag(filename_ci, export_date)
  filename_ci <- set_fpath(filename_ci, "Dataout/CIRG")
  
  filename_ci
  
  #------- PRINT CUSTOM INDICATOR SUBMISSION TO DESK -----------------------
  #export to spreadsheet for submission via google forms at https://docs.google.com/forms/d/e/1FAIpQLSd1XtCoRZ-22zp9cOi24P2OhEdq-SYS0QORZkUtQj8UMei7RQ/viewform?gxids=7628
  #Initial; Long; KP --> assign filename
  
  write.xlsx(cirg_kp, file = filename_ci, sheetName = "CIRG", startRow = 2, append=TRUE)
  
  # REVIEW OUTPUT
  names(passos_format_3)
  glimpse(passos_format_3)
  table(passos_format_3$snu)
  table(passos_format_3$reportingperiod)
  
  # TEST TO SEE IF ALL PSNU HAVE BEEN MATCHED TO PSNUUID
  psnuuid_tester <- passos_format_3  %>% 
    filter(psnuuid == "y")
  