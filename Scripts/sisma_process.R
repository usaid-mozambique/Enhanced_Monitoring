
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


df_1 <- process_sisma_csv(ats_results_path, type = "ATS Result") # functional
df_2 <- process_sisma_csv(ats_hist_path, type = "ATS History")   # functional
df_3 <- process_sisma_csv(ats_ci_path, type = "ATS CI")          # functional
df_4 <- process_sisma_csv(ats_ccsd_path, type = "ATS CCSD")      # functional
df_5 <- process_sisma_csv(ats_saaj_path, type = "ATS SAAJ")      # functional prior to indicator map change
df_6 <- process_sisma_csv(ats_smi_path, type = "ATS SMI")        # functional prior to indicator map change
df_7 <- process_sisma_csv(ats_auto_path, type = "ATS Auto")      # functional
df_8 <- process_sisma_csv(hiv_tarv_path, type = "HIV TARV")      # NOT TESTED functional


df_c <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6)

# FUNCTIONAL -----------------------------------------------------------

df <- clean_sisma_csv(hiv_tarv_path)

df_function_parse <- df %>%
  parse_sisma_hiv_tarv()



data <- as.tibble(data_sisma_ats_results)


# EXPIREMENT FUNCTION -----------------------------------------------------

df_function_parse <- df %>%
  dplyr::left_join(data_sisma_hiv_tarv, by = "indicator") %>%
  tidyr::drop_na(tidyselect::any_of(c("indicator_new", "value"))) %>%
  dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator = indicator_new, age, sex, exit_type, value)
sum(df_function_parse$value, na.rm=T)



df_function_parse <- df %>%
  dplyr::left_join(data_sisma_hiv_tarv, by = "indicator") %>%
  filter_all(any_vars(is.na(.))) %>%
  dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator = indicator_new, age, sex, exit_type, value)
sum(df_function_parse$value, na.rm=T)


df_function_parse <- df %>%
  dplyr::left_join(data_sisma_hiv_tarv, by = "indicator") %>%
  dplyr::filter(!is.na(value)) %>% 
  dplyr::filter(!is.na(indicator_new)) %>% 
  dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator = indicator_new, age, sex, exit_type, value)

sum(df_function_parse$value, na.rm=T)
