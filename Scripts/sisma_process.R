
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(mozR)


# DEFINE PATHS ------------------------------------------------------------

year <- "2023"

cpn_path <- glue::glue("Data/MISAU/SMI/sisma_cpn_{year}.csv")
ats_results_path <- glue::glue("Data/MISAU/ATS/ats_results_{year}.csv")
ats_hist_path <- glue::glue("Data/MISAU/ATS/ats_hist_{year}.csv")
ats_ci_path <- glue::glue("Data/MISAU/ATS/ats_ci_lig_{year}.csv")
ats_ccsd_path <- glue::glue("Data/MISAU/ATS/ats_ccs_ccd_{year}.csv")
ats_saaj_path <- glue::glue("Data/MISAU/ATS/ats_saaj_cm_{year}.csv")
ats_smi_path <- glue::glue("Data/MISAU/ATS/ats_smi_{year}.csv")
ats_auto_path <- glue::glue("Data/MISAU/ATS/ats_autotest_{year}.csv")
hiv_tarv_path <- glue::glue("Data/MISAU/CT/tarv_{year}.csv")

# STATUS ------------------------------------------------------------------


df_ats <- bind_rows(process_sisma_csv(ats_results_path, type = "ATS Result"), # functional
                    process_sisma_csv(ats_hist_path, type = "ATS History"),   # functional
                    process_sisma_csv(ats_ci_path, type = "ATS CI"),          # functional
                    process_sisma_csv(ats_ccsd_path, type = "ATS CCSD"),      # functional
                    process_sisma_csv(ats_saaj_path, type = "ATS SAAJ"),      # functional prior to indicator map change
                    process_sisma_csv(ats_smi_path, type = "ATS SMI")         # functional prior to indicator map change
                    )

df_autoteste <- process_sisma_csv(ats_auto_path, type = "ATS Auto")      # functional

df_tarv <- process_sisma_csv(hiv_tarv_path, type = "HIV TARV")      # NOT TESTED functional

df_cpn <- process_sisma_csv(cpn_path, type = "CPN") 

