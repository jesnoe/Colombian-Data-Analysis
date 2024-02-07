# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

municipios_capital <- municipios@data %>% mutate(municipio=str_to_upper(municipio, locale="en"))
municipios_capital$municipio <- stri_trans_general(municipios_capital$municipio, "Latin-ASCII")
municipios_capital$depto <-  stri_trans_general(municipios_capital$depto, "Latin-ASCII")

municipios_capital$depto <- gsub(" De ", " de ", municipios_capital$depto)
municipios_capital$depto <- gsub(" Del ", " del ", municipios_capital$depto)
municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
municipios_capital$depto <- gsub("Bogota, D. C.", "Bogota", municipios_capital$depto)
municipios_capital$municipio <- gsub(", D.C.", "", municipios_capital$municipio)
municipios_capital$municipio <- gsub("GUADALAJARA DE BUGA", "BUGA", municipios_capital$municipio)

# data from https://simplemaps.com/data/world-cities and https://geokeo.com/database/town/co
cities <- read.csv("Colombia Data/cities and towns.csv") %>% as_tibble
cities$city_town <- str_to_upper(stri_trans_general(cities$city_town, "Latin-ASCII"))

base_to_base  <- read_xlsx("Colombia Data/Anecdotal base to base.xlsx")
base_to_base$`source municipio` <- str_to_upper(base_to_base$`source municipio`)
base_to_base$`Destine municipio` <- str_to_upper(base_to_base$`Destine municipio`)

HCl_to_HCl  <- read_xlsx("Colombia Data/Anecdotal HCl to HCl.xlsx")
HCl_to_HCl$`source municipio` <- str_to_upper(HCl_to_HCl$`source municipio`)
HCl_to_HCl$`Destine municipio` <- str_to_upper(HCl_to_HCl$`Destine municipio`)

cities %>% filter(!(city_town %in% base_to_base$`source municipio`))
base_to_base %>% filter(!(`source municipio` %in% cities$city_town)) %>% pull(`source municipio`) %>% unique
base_to_base %>% filter(!(`Destine municipio` %in% cities$city_town)) %>% pull(`Destine municipio`) %>% unique
HCl_to_HCl %>% filter(!(`source municipio` %in% cities$city_town)) %>% pull(`source municipio`) %>% unique
HCl_to_HCl %>% filter(!(`Destine municipio` %in% cities$city_town)) %>% pull(`Destine municipio`) %>% unique

base_to_base %>% filter(!(`source Depto` %in% unique(municipios_capital$depto)))
base_to_base %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto)))
# Putomayo -> Putumayo
# Guajira -> La Guajira

base_to_base %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio)))
# TUMACO, Narino -> SAN ANDRES DE TUMACO
# RESINAS CARTAGENA -> CARTAGENA DE INDIAS
# VEGALARGA -> NEIVA
# CRUCE DE LAS DELICIAS (no information)
# INSPECCIÓN BALSILLAS -> VILLAVIEJA, Huila
# SAN VINCENTE DEL CAGUAN -> SAN VICENTE DEL CAGUAN
# BALSELLAS, Caqueta ->VALPARAISO, Caqueta
# SAN JOSE DE LA FRAGUA, Caqueta -> SAN JOSE DEL FRAGUA
base_to_base %>% filter(!(`Destine municipio` %in% unique(municipios_capital$municipio)))
# PURISIMA, Cordoba -> PURISIMA DE LA CONCEPCION
# SAHAGUAN, Cordoba -> SAHAGUN
# SAN BERNANDO DEL VIENTO, Cordoba -> SAN BERNARDO DEL VIENTO
# SANTA CRUZ DE LORICA, Cordoba -> LORICA
# TIERRASALTA, Cordoba -> TIERRALTA

base_to_base <- base_to_base %>% rename(municipio=`source municipio`, depto=`source Depto`)
base_to_base <- left_join(base_to_base, municipios_capital[,-2], by=c("municipio", "depto")) %>% 
  rename(source_id=id,
         source_municipio=municipio,
         source_depto=depto,
         municipio=`Destine municipio`,
         depto=`Destine Depto.`) %>% 
  left_join(municipios_capital[,-2], by=c("municipio", "depto")) %>% 
  rename(destination_id=id,
         destination_municipio=municipio,
         destination_depto=depto)
base_to_base %>% filter(destination_municipio == "BOGOTA")
# base_to_base %>% write.csv("Colombia Data/Anecdotal base to base with id.csv", row.names=F)


HCl_to_HCl %>% filter(!(`source Depto` %in% unique(municipios_capital$depto))) %>% pull(`source Depto`) %>% unique
HCl_to_HCl %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto))) %>% pull(`Destine Depto.`) %>% unique

HCl_to_HCl %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio))) %>% pull(`source municipio`) %>% unique
HCl_to_HCl %>% filter(!(`Destine municipio` %in% unique(municipios_capital$municipio))) %>% pull(`Destine municipio`) %>% unique