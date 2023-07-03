
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(mozR)


# DEFINE PATHS ------------------------------------------------------------

year <- "2022"

ats_results_path <- glue::glue("Data/MISAU/ATS/ats_results_{year}.csv")
ats_hist_path <- glue::glue("Data/MISAU/ATS/ats_hist_{year}.csv")
ats_ci_path <- glue::glue("Data/MISAU/ATS/ats_ci_lig_{year}.csv")
ats_ccsd_path <- glue::glue("Data/MISAU/ATS/ats_ccs_ccd_{year}.csv")
ats_saaj_path <- glue::glue("Data/MISAU/ATS/ats_saaj_cm_{year}.csv")
ats_smi_path <- glue::glue("Data/MISAU/ATS/ats_smi_{year}.csv")
ats_auto_path <- glue::glue("Data/MISAU/ATS/ats_autotest_{year}.csv")

# STATUS ------------------------------------------------------------------


df_1 <- process_sisma_csv(ats_results_path, type = "ATS Result") 
df_2 <- process_sisma_csv(ats_hist_path, type = "ATS History")  
df_3 <- process_sisma_csv(ats_ci_path, type = "ATS CI")          
df_4 <- process_sisma_csv(ats_ccsd_path, type = "ATS CCSD")     
df_5 <- process_sisma_csv(ats_saaj_path, type = "ATS SAAJ")      
df_6 <- process_sisma_csv(ats_smi_path, type = "ATS SMI")        


df_c <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6)


write_tsv(df_c,
          "Dataout/sisma_ats_2022.txt")
