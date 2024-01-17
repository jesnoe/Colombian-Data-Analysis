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

base_to_base  <- read_xlsx("Colombia Data/Anecdotal base to base.xlsx")
base_to_base$`source municipio` <- str_to_upper(base_to_base$`source municipio`)
base_to_base$`Destine municipio` <- str_to_upper(base_to_base$`Destine municipio`)

base_to_base %>% filter(!(`source Depto` %in% unique(municipios_capital$depto)))
base_to_base %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto)))
# Putomayo -> Putumayo
# Guajira -> La Guajira

base_to_base %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio)))
# TUMACO -> SAN ANDRES DE TUMACO
# RESINAS CARTAGENA -> CARTAGENA DE INDIAS
# VEGALARGA -> NEIVA
# PUERTA DEL SOL -> CALI, Valle del Cauca
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
# base_to_base %>% write.csv("Anecdotal base to base with id.csv", row.names=F)

# HCl_to_HCl  <- read_xlsx("Colombia Data/Anecdotal HCl to HCl.xlsx")
# HCl_to_HCl$`source municipio` <- str_to_upper(HCl_to_HCl$`source municipio`)
# HCl_to_HCl$`Destine municipio` <- str_to_upper(HCl_to_HCl$`Destine municipio`)
# 
# HCl_to_HCl %>% filter(!(`source Depto` %in% unique(municipios_capital$depto))) %>% pull(`source Depto`) %>% unique
# HCl_to_HCl %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto))) %>% pull(`Destine Depto.`) %>% unique
# 
# HCl_to_HCl %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio)))
# HCl_to_HCl %>% filter(!(`Destine municipio` %in% unique(municipios_capital$municipio)))

price_2013_2015 <- read_xlsx("Colombia Data/Colombia Price Data 2013-2015 for R.xlsx")
price_2013_2015$city <- str_to_upper(price_2013_2015$city) %>% stri_trans_general("Latin-ASCII")
price_2013_2015$department <- str_to_title(price_2013_2015$department)

municipios_capital$depto %>% unique %>% sort
# BOGOTA's department is set to NA
# BOGOTA (CUNDINAMARCA) LIMITES CON BOYACA -> BOGOTA
# Guajira -> La Guajira
price_2013_2015 %>% filter(!(department %in% unique(municipios_capital$depto))) %>% pull(department) %>% unique
price_2013_2015$department <- gsub(" Del ", " del ", price_2013_2015$department)
price_2013_2015$department <- gsub(" De ", " de ", price_2013_2015$department)
price_2013_2015$department <- gsub("Cauqueta", "Caqueta", price_2013_2015$department)
price_2013_2015$department <- gsub("Bajo Cauca Antioqueño", "Antioquia", price_2013_2015$department)
# San Andres looks like San Andres and Providencia (two islands), so deleted
price_2013_2015 <- price_2013_2015 %>% filter(department != "San Andres")

municipios_capital$municipio %>% unique %>% sort
# AMAZONAS, Amazonas -> Leticia (caplital city)
# CUNDINAMARCA, Cundinamarca -> 
price_2013_2015 %>% filter(!(city %in% unique(municipios_capital$municipio))) %>% pull(city) %>% unique
price_2013_2015$city <- gsub("METROPOLITANA DE BOGOTA", "BOGOTA", price_2013_2015$city)

price_2016_2021 <- read_xlsx("Colombia Data/Colombia Price Data 2016-2021 for R.xlsx")
price_2016_2021$city <- str_to_upper(price_2016_2021$city) %>% stri_trans_general("Latin-ASCII")
price_2016_2021$department <- str_to_title(price_2016_2021$department) %>% stri_trans_general("Latin-ASCII")

municipios_capital$depto %>% unique %>% sort
price_2016_2021 %>% filter(!(department %in% unique(municipios_capital$depto))) %>% pull(department) %>% unique
# Guajira -> La Guajira
price_2016_2021$department <- gsub(" Del ", " del ", price_2016_2021$department)
price_2016_2021$department <- gsub(" De ", " de ", price_2016_2021$department)
price_2016_2021$department <- gsub("Cauqueta", "Caqueta", price_2016_2021$department)

municipios_capital$municipio %>% unique %>% sort
price_2016_2021 %>% filter(!(city %in% unique(municipios_capital$municipio))) %>% pull(city) %>% unique