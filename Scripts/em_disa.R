
rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(fs)
library(googlesheets4)
library(googledrive)
library(glamr)
library(glitr)
library(grabr)
library(ggthemes)
library(janitor)
library(glue)
library(readxl)
library(openxlsx)
library(Wavelength)
library(mozR)
load_secrets()


# VALUES & PATHS ----------------------------------------------------------


month_input <- "2023-10-20"
file_input <- "Data/Disa_new/monthly/Relatorio Mensal de Carga Viral Outubro 2023.xlsx"


dt <- base::format(as.Date(month_input), 
                   "%Y_%m")

file <- glue::glue("DISA_VL_{dt}")


# paths that do not require monthly updating
path_historic_output_local <- "Dataout/DISA/monthly_processed/"
file_monthly_output_local <- path(path_historic_output_local, file, ext = "txt")
file_historic_output_local <- "Dataout/em_disa.txt"

path_monthly_output_gdrive <- as_id("https://drive.google.com/drive/folders/12XN6RKaHNlmPoy3om0cbNd1Rn4SqHSva")
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1xBcPZNAeYGahYj_cXN5aG2-_WSDLi6rQ")



# DATA LOAD -------------------------------------------------------


df <- reshape_disa_vl(filepath = file_input, month = month_input)


disa_datim_map <- pull_sitemap(sheet = "map_disa") %>% 
  select(!c(note))


ajuda_site_map <- pull_sitemap() %>% 
  select(datim_uid,
         partner = partner_pepfar_clinical,
         starts_with("his_"))

psnuuid_map <- pull_sitemap(sheetname = "list_psnu") %>% 
  select(psnu,
         psnuuid)


cntry <- "Mozambique"
uid <- get_ouuid(cntry)
datim_orgsuids <- pull_hierarchy(uid, username = datim_user(), password = datim_pwd()) %>% 
  filter(!is.na(facility) & !is.na(psnu)) %>% 
  select(datim_uid = orgunituid,
         snu = snu1,
         psnu = community, # changed to community with update to datim
         sitename = facility) %>% 
  arrange(snu, psnu, sitename)


# datim_orgsuids_2 <- datim_orgsuids %>% 
#   filter(!is.na(facility) & !is.na(psnu)) %>% 
#   select(datim_uid = orgunituid,
#          snu = snu1,
#          psnuuid,
#          psnu = community, # changed to community with update to datim
#          sitename = facility)
#   
# write_csv(datim_orgsuids_2, 
#           "Documents/datim_uids.csv")

# SUBMISSION CHECKS -------------------------------------------------------


# tabulate unique sites that have viral load results reported
df %>% 
  filter(!is.na(disa_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n()) %>% 
  arrange(`n()`)

# tabulate sites missing disa unique identifiers
df %>% 
  filter(is.na(disa_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n()) %>% 
  arrange(`n()`)


# PRINT MONTHLY OUTPUT ----------------------------------------------------


readr::write_tsv(
  df,
  {file_monthly_output_local},
  na ="")


drive_put(file_monthly_output_local,
          path = path_monthly_output_gdrive,
          name = glue({file}, '.txt')
)


# SURVEY MONTHLY DATASETS AND COMPILE ----------------------------


historic_files <- dir({path_historic_output_local}, pattern = "*.txt")  # PATH FOR PURR TO FIND MONTHLY FILES TO COMPILE

disa_temp <- historic_files %>%
  map(~ read_tsv(file.path(path_historic_output_local, .))) %>%
  reduce(rbind)


# JOIN DISA MAPPING & CHECK -------------------------------------------------------


disa_meta <- disa_temp %>% 
  select(!site_nid) %>% 
  left_join(disa_datim_map, by = c("disa_uid" = "disa_uid")) %>% 
  mutate(ajuda = replace_na(ajuda, 0)) %>% 
  relocate(c(ajuda, sisma_uid, datim_uid), .before = disa_uid)

# number of sites missing datim_uid coding
disa_meta %>% 
  filter(is.na(datim_uid)) %>% 
  group_by(snu) %>% 
  distinct(sitename) %>% 
  summarise(n())


# REMOVE ROWS WITHOUT DATIM UID & CLEAN -----------------------------------


disa_final <- clean_disa_vl(disa_meta)


# CHECK UNIQUE AGE/SEX & ASSESS MISSING DATA --------------------------


disa_final %>% 
  distinct(age)

disa_final %>% 
  distinct(sex)


disa_missing <- disa_meta %>% 
  filter(is.na(datim_uid),
         group == "Age") %>% 
  group_by(period, snu, psnu, sitename, disa_uid) %>% 
  summarize(
    across(c(VL, VLS, TAT), .fns = sum), .groups = "drop")

check_total_vl <- sum(disa_final$VL, na.rm = T)
check_total_missing <- sum(disa_missing$VL, na.rm = T)

1 - check_total_missing / check_total_vl


disa_final %>% 
  filter(period > max(period) - months(6)) %>% 
  group_by(period, snu) %>% 
  distinct(period, sitename) %>% 
  summarise(n()) %>% 
  arrange(snu) %>% 
  print(n=100)

# PLOT HISTORIC DATA ----------------------------------------------


df_vl_plot <- disa_final %>% 
  filter(!is.na(group)) %>% 
  group_by(period, group, partner, snu) %>% 
  summarize(VL = sum(VL, na.rm = T)) %>% 
  ungroup()

df_vl_plot %>% 
  ggplot(aes(x = period, y = VL, fill = snu)) + 
  geom_col() + 
  labs(title = "Testing of Viral Load Specimens by Population",
       subtitle = "Historical trend of Viral Load specimens processed within the DISA laboratory network",
       color = "Partner") + 
  theme_solarized() + 
  theme(axis.title = element_text()) + 
  facet_wrap(~group)  


# PRINT HISTORIC OUTPUTS ------------------------------------------------------


write.xlsx(disa_missing,
           {"Dataout/DISA/missing_sites_mfl.xlsx"},
           overwrite = TRUE)


readr::write_tsv(
  disa_final,
  {file_historic_output_local},
  na = "")


drive_put(file_historic_output_local,
          path = path_historic_output_gdrive)
