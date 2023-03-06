
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(mozR)
library(lubridate)
library(glamr)
library(janitor)
library(readxl)
library(openxlsx)
library(glue)
library(ggthemes)
library(googlesheets4)
load_secrets()



# DEFINE PATHS ------------------------------------------------------------


year <- "2023"

tarv_path <- glue::glue("Data/MISAU/SMI/sisma_cpn_{year}.csv")


# FUNCTIONAL -----------------------------------------------------------



smi_cpn <- clean_sisma_csv(tarv_path)

indicator_list <- distinct(smi_cpn, indicator)


df <- smi_cpn %>% 

  dplyr::filter(!is.na(value)) %>% 
  
  dplyr::mutate(
    
    indicator_temp = dplyr::case_when(stringr::str_detect(indicator, "_anos") ~ "MG_1CON_MES",
                                      stringr::str_detect(indicator, "12_semanas") ~ "MG_1CON_12SEM_MES",
                                      stringr::str_detect(indicator, "total_da_coorte") ~ "MG_1CON_COORTE",
                                      stringr::str_detect(indicator, "_4_ou_mais_") ~ "MG_4CON_COORTE",
                                      stringr::str_detect(indicator, "parceiros_presentes") ~ "PARCEIRO_PRESENTE",
                                      stringr::str_detect(indicator, "parceiros_testado") ~ "PARCEIRO_TESTADO",
                                      stringr::str_detect(indicator, "iniciaram_ctz|ctz_a_entrada") ~ "MG_HIV_CTZ",
                                      stringr::str_detect(indicator, "positiva_a_entrada|gravidas_testadas_hiv") ~ "MG_ESTADO_HIV",
                                      stringr::str_detect(indicator, "tarv_a_entrada|iniciaram_tarv|monoprofilaxia|biprofilaxia") ~ "MG_HIV_POS_ARV",
                                      
                                      stringr::str_detect(indicator, "2_doses_de_tip") ~ "MG_MAL_TIP_2DOS",
                                      stringr::str_detect(indicator, "4_ou_mais_doses_de_tip") ~ "MG_MAL_TIP_4DOS",
                                      stringr::str_detect(indicator, "remtil") ~ "MG_MAL_REMTIL",
                                      stringr::str_detect(indicator, "diagnostico_laboratorial") ~ "MG_MAL_DIAG",
                                      stringr::str_detect(indicator, "tratamento_para_malaria") ~ "MG_MAL_DIAG_TX"),
    
    age = dplyr::case_when(stringr::str_detect(indicator, "10_14") ~ "10-14",
                           stringr::str_detect(indicator, "15_19") ~ "15-19",
                           stringr::str_detect(indicator, "20_24") ~ "20-24",
                           stringr::str_detect(indicator, "_25_") ~ "25+"),
    
    disaggregate = dplyr::case_when(stringr::str_detect(indicator, "hiv_positiva_a_entrada") ~ "HIV+ a entrada",
                                    stringr::str_detect(indicator, "testadas_hiv_positivas") ~ "HIV+ testado/a",
                                    stringr::str_detect(indicator, "testadas_hiv_negativas") ~ "HIV- testado/a",
                                    stringr::str_detect(indicator, "parceiros_testados_positivos") ~ "HIV+ testado/a",
                                    stringr::str_detect(indicator, "parceiros_testados_negativos") ~ "HIV- testado/a",
                                    stringr::str_detect(indicator, "iniciaram_ctz") ~ "Inicio CTZ",
                                    stringr::str_detect(indicator, "ctz_a_entrada") ~ "CTZ a entrada",
                                    stringr::str_detect(indicator, "tarv_a_entrada") ~ "TARV a entrada",
                                    stringr::str_detect(indicator, "iniciaram_tarv") ~ "Inicio TARV",
                                    stringr::str_detect(indicator, "monoprofilaxia") ~ "Monoprofilaxia",
                                    stringr::str_detect(indicator, "iniciaram_tarv") ~ "biprofilaxia"),
    
    period_cohort = dplyr::if_else(indicator_temp %in% c("MG_1CON_MES", "MG_1CON_12SEM_MES"), period, period - months(6)))
    



    
df %>% 
  filter(indicator_temp == "MG_1CON_MES") %>% 
  ggplot(aes(period, value, fill = age)) +
  geom_col(position="fill") + 
  labs(
    title = "PMTCT Trends",
    x = NULL,
    y = "No. 1st ANC") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  facet_grid(~ snu) 



  
  
  dplyr::relocate(period_cohort, .after = period) %>% 
  
  glimpse()




parceiros_testados_negativos
parceiros_testados_positivos


unique(df$indicator_temp)
    
    sex = dplyr::case_when(stringr::str_detect(indicator, "feminino")             ~ "Feminino",
                           stringr::str_detect(indicator, "masculino")            ~ "Masculino",
                           stringr::str_detect(indicator, "_0_4_anos|_5_9_anos")  ~ "Desconh."),
    
    age = dplyr::case_when(stringr::str_detect(indicator, "0_4")       ~ "<05",
                           stringr::str_detect(indicator, "5_9")     ~ "05-09",
                           stringr::str_detect(indicator, "10_14")   ~ "10-14",
                           stringr::str_detect(indicator, "15_19")   ~ "15-19",
                           stringr::str_detect(indicator, "20+")       ~ "20+",
                           stringr::str_detect(indicator, " 0_14")   ~ "<15",
                           stringr::str_detect(indicator, "15+")       ~ "15+",
                           TRUE ~ NA_character_),
    
    age_coarse = dplyr::case_when(age %in% c("<05", "05-09", "10-14", "<15") ~ "<15",
                                  age %in% c("15-19", "20+", "15+")          ~ "15+",
                                  TRUE ~ age),
    
    exit_type = dplyr::case_when(stringr::str_detect(indicator, "uspensos")         ~ "Suspenso",
                                 stringr::str_detect(indicator, "bitos")            ~ "Obito",
                                 stringr::str_detect(indicator, "bandonos")         ~ "Abandono",
                                 stringr::str_detect(indicator, "ransferidos_para") ~ "Transferidos Para"),
    
    indicator2 = dplyr::case_when(stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria durante") ~ "TX_NOVO",
                                  stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria ate") ~ "TX_NOVO_CUM"))
    

unique(df$sex)
unique(df$age)
unique(df$age_coarse)
unique(df$exit_type)
unique(df$sex)
unique(df$sex)
unique(df$sex)


    indicator2 = dplyr::case_when(stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria durante") ~ "TX_NOVO",
                                  stringr::str_detect(indicator, "iniciou TARV nesta unidade sanitaria ate") ~ "TX_NOVO_CUM"))
    


unique(df$indicator2)

    source = "RM HIV/SIDA",
    
  )


df_sample <- df %>% 
  filter(sisma_uid == "XmuwpVFIIzP")

unique(df_sample$exit_type)



view(df)




x <- "MZ HIV SIDA - NÂº de pacientes que iniciou PrÃ©-TARV (cuidados HIV) nesta unidade sanitÃ¡ria durante o mÃªs - F 5 - 9 anos"

stringi::stri_trans_general(x, "latin-ascii")


