#------- Choose Quarter and FY for Custom Indicator Export -----------------------

#SELECT QUARTER
Q_choose <- "Q3"

#SELECT FY
FY_choose <- 2021

#modify 3 filepaths in export steps

#-----------------------------------------------------------------------------------
##  LOAD CORE TIDYVERSE & OTHER PACKAGES

library(tidyverse)
library(glamr)
library(readxl)
library(openxlsx)
library(glue)
library(janitor)
library(filenamer)

# Data input from PASSOS tracker ------------------------------------------

#Joe's read files
passos <- read_excel("Data/Passos/PASSOS FY2021 Tracker Mensal_Maio09062021.xlsx", 
                     sheet = "Dados e Metas PC", skip = 2)
glimpse(passos) #review data structure                    

psnuuid_lookup <- read_excel("Documents/psnu_psnuuid.xlsx") %>% 
  dplyr::rename(PSNU = Psnu)

#Bourke's read files
# passos <- read_excel("~/KP Dashboard/Data Preparation/Custom Indicator Reporting/Mozambique/PASSOS FY2021 Tracker Mensal_Maio09062021.xlsx", 
#                      sheet = "Dados e Metas PC", skip = 2)
# glimpse(passos) #review data structure
# 
# psnuuid_lookup <- read_excel("~/KP Dashboard/Data Preparation/Custom Indicator Reporting/Mozambique/psnu_psnuuid.xlsx") %>% 
#   dplyr::rename(PSNU = Psnu)


#------- PROCESS PASSOS TRACKER -----------------------

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
                           "Non-KP" = "Non-KP (general population)")) %>%
  dplyr::select (SNU, PSNU, FY, Quarter, month, date, Population, indicator, val)
  
#------- CONTINUE PROCESSING PASSOS TRACKER -----------------------

passos_format_2 <- passos_format %>%  
  mutate(orgunit = PSNU,
         mech_code	= 18280,
         partner= "FHI360",
         operatingunit = "Mozambique",
         age = NA,
         sex = NA,
         otherdisaggregate = "Site Support Type: PEPFAR supported") %>%
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
  dplyr::rename_all(funs(tolower)) %>% 
  glimpse()

#------- Create list and function for filtering -----------------------

MER <- c('KP_PREV','HTS_TST','HTS_TST_POS') #establish string to match against
`%notin%` <- Negate(`%in%`) #create function for inverse of %in%

#------- FILTER PERIOD TO CIRG AND GROUP VALUES BY QUARTER -----------------------

cirg_kp <- passos_format_2  %>%
  dplyr::mutate(IndicatorType = case_when(indicator %notin% MER ~ "Custom")) %>%
  dplyr::filter(val!= "NA", IndicatorType == "Custom", fy == FY_choose, quarter == Q_choose) %>%
  group_by(IndicatorType, fundingsource, reportingperiod,	orgunit,	psnuuid,	mech_code,	partner,	operatingunit,	psnu, indicator,	sex,	age,	population,	otherdisaggregate,	numdenom) %>%
  summarise(val = sum(val), .groups = 'drop') %>% 
  ungroup() %>% 
  dplyr::select(IndicatorType, reportingperiod, orgunit, orgnunituid = psnuuid, mech_code, partner, operatingunit,	psnu, indicator,	sex,	age,	population,	otherdisaggregate,	numdenom, val) %>%
  glimpse()

#------- Name File -----------------------
FY_chosen <- head(str_extract(cirg_kp$reportingperiod, "^.*(?= Q)"), 1)
FY_chosen
export_date <- format(Sys.Date(), "%Y%m%d")
export_date

filename <- "CIRG.xlsx"
filename <- tag(filename, FY_chosen)
filename <- tag(filename, Q_choose)
filename <- tag(filename, "Mozambique_PASSOS")
filename <- tag(filename, export_date)
filename <- set_fpath(filename, "Dataout/CIRG")

filename

#------- PRINT CUSTOM INDICATOR SUBMISSION TO DESK -----------------------
#export to spreadsheet for submission via google forms at https://docs.google.com/forms/d/e/1FAIpQLSd1XtCoRZ-22zp9cOi24P2OhEdq-SYS0QORZkUtQj8UMei7RQ/viewform?gxids=7628
#Initial; Long; KP --> assign filename

write.xlsx(cirg_kp, file = filename, sheetName = "CIRG", startRow = 2, append = TRUE)

# REVIEW OUTPUT
names(passos_format_2)
glimpse(passos_format_2)
table(cirg_kp$IndicatorType)
table(passos_format_2$snu)
table(passos_format_2$reportingperiod)

# TEST TO SEE IF ALL PSNU HAVE BEEN MATCHED TO PSNUUID
psnuuid_tester <- passos_format_2  %>% 
  filter(psnuuid == "y")

#------- CREATE Long file, by Month for dashboard -----------------------
em_kp_monthly <- passos_format_2  %>%
  group_by(fundingsource, fy, date, month, snu, orgunit,	mech_code,	partner,	operatingunit,	psnu, psnuuid, indicator, numdenom, population,	otherdisaggregate) %>%
  summarise(val = sum(val), .groups = 'drop') %>%
  ungroup() %>% 
  filter(val!= "NA") %>%
    dplyr::rename(FY = fy, Province = snu, District = psnu, Orgnunituid = psnuuid, NumDen = numdenom, Date = date, Month = month) %>%
    dplyr::mutate(Partner = "PASSOS",
                  PatientType = case_when(!str_detect(population, "Non-KP") ~ "KeyPop"),
                  KeyPop = case_when(PatientType == "KeyPop" ~ str_extract(population, "(?<=\\().*(?=\\))"))) %>%
    dplyr::select(Partner, Province, District, Orgnunituid, fundingsource, NumDen, PatientType, KeyPop, Date, Month, indicator, KeyPop, val) %>%
   glimpse()


filename2 <- "em_kp_long.txt"
filename2 <- tag(filename2, export_date)
filename2 <- set_fpath(filename2, "Dataout")

filename2

#------- PRINT WIDE PROGRAM DATA TO DESK -----------------------

readr::write_tsv(
  em_kp_monthly,
  filename2,
  na ="")

#------- CREATE Wide file, by Month for dashboard -----------------------
em_kp_monthly_wide <- em_kp_monthly %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = indicator, values_from = val, values_fill = 0) %>%
  select(-row) %>%
  glimpse()

  filename3 <- "em_kp_wide.txt"
  filename3 <- tag(filename3, export_date)
  filename3 <- set_fpath(filename3, "Dataout")
  
  filename3
  
  #------- PRINT WIDE PROGRAM DATA TO DESK -----------------------
  
  readr::write_tsv(
    em_kp_monthly_wide,
    filename3,
    na ="")
  
