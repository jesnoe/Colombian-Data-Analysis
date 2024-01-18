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

base_to_base  <- read_xlsx("Colombia Data/Anecdotal base to base.xlsx")
base_to_base$`source municipio` <- str_to_upper(base_to_base$`source municipio`)
base_to_base$`Destine municipio` <- str_to_upper(base_to_base$`Destine municipio`)

base_to_base %>% filter(!(`source Depto` %in% unique(municipios_capital$depto)))
base_to_base %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto)))
# Putomayo -> Putumayo
# Guajira -> La Guajira

base_to_base %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio)))
# TUMACO, Narino -> SAN ANDRES DE TUMACO
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
# base_to_base %>% write.csv("Colombia Data/Anecdotal base to base with id.csv", row.names=F)

HCl_to_HCl  <- read_xlsx("Colombia Data/Anecdotal HCl to HCl.xlsx")
HCl_to_HCl$`source municipio` <- str_to_upper(HCl_to_HCl$`source municipio`)
HCl_to_HCl$`Destine municipio` <- str_to_upper(HCl_to_HCl$`Destine municipio`)

HCl_to_HCl %>% filter(!(`source Depto` %in% unique(municipios_capital$depto))) %>% pull(`source Depto`) %>% unique
HCl_to_HCl %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto))) %>% pull(`Destine Depto.`) %>% unique

HCl_to_HCl %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio))) %>% pull(`source municipio`) %>% unique
HCl_to_HCl %>% filter(!(`Destine municipio` %in% unique(municipios_capital$municipio))) %>% pull(`Destine municipio`) %>% unique



# Price Data 2013-2016
price_2013_2016 <- read_xlsx("Colombia Data/Colombia Price Data 2013-2016 for R.xlsx")
price_2013_2016$city <- str_to_upper(price_2013_2016$city) %>% stri_trans_general("Latin-ASCII")
price_2013_2016$department <- str_to_title(price_2013_2016$department)

# BOGOTA's department is set to NA
# BOGOTA (CUNDINAMARCA) LIMITES CON BOYACA -> BOGOTA
# Guajira -> La Guajira
price_2013_2016 %>% filter(!(department %in% unique(municipios_capital$depto))) %>% pull(department) %>% unique
price_2013_2016$department <- gsub(" Del ", " del ", price_2013_2016$department)
price_2013_2016$department <- gsub(" De ", " de ", price_2013_2016$department)
price_2013_2016$department <- gsub("Cauqueta", "Caqueta", price_2013_2016$department)
price_2013_2016$department <- gsub("Cundinmarca", "Cundinamarca ", price_2013_2016$department)
price_2013_2016$department <- gsub("Bajo Cauca Antioqueño", "Antioquia", price_2013_2016$department)

# San Andres looks like San Andres and Providencia (two islands), so deleted
price_2013_2016 <- price_2013_2016 %>% filter(department != "San Andres")

municipios_capital$municipio %>% unique %>% sort
# AMAZONAS, Amazonas -> Leticia (caplital city)
# CUNDINAMARCA, Cundinamarca -> will assign the same depto_id
# VILLA GARZON -> VILLAGARZON
# PIEDRA, Narino -> TAMINANGO
# SANTA FE, Narino -> EL TABLON DE GOMEZ
# TUMACO, Narino -> SAN ANDRES DE TUMACO
# URABA, Antioquia: Urabá Antioquia is a subregion consisting of Apartadó, Arboletes, Carepa, Chigorodó, Murindó, Mutatá, Turbo, Necoclí,
# San Juan de Urabá, San Pedro de Urabá, and Vigía del Fuerte: only 3 observations
# PUERTO INIRIDA, Guainia -> INIRIDA
# CARTAGENA, Boliva -> CARTAGENA DE INDIAS
# SUR DE BOLIVAR corresponds to the southern end of Bolívar: only 3 observations
# CATATUMBO is a subregion in the northeast of Norte de Santander (https://es.wikipedia.org/wiki/Regi%C3%B3n_del_Catatumbo): only 4 observations
# BAJO CAUCA is a territorial subregion in the northeast of Antioquia: only 14 observations with 2 prices. The rest are arroba
# Cauca, Cauca only 1 observation
# SAN JOSE, Guaviare -> SAN JOSE DEL GUAVIARE
price_2013_2016 %>% filter(!(city %in% unique(municipios_capital$municipio))) %>% pull(city) %>% unique
price_2013_2016$city <- gsub("AMAZONAS", "LETICIA", price_2013_2016$city)
price_2013_2016$city <- gsub("METROPOLITANA DE BOGOTA", "BOGOTA", price_2013_2016$city)
price_2013_2016$city <- gsub("LACRUZ", "LA CRUZ", price_2013_2016$city)
price_2013_2016$city <- gsub("APARATDO", "APARTADO", price_2013_2016$city)
price_2013_2016$city <- gsub("LAESTRELLA", "LA ESTRELLA", price_2013_2016$city)
price_2013_2016$city <- gsub("LAPLATA", "LA PLATA", price_2013_2016$city)
price_2013_2016$city <- gsub("PUERTO INIRIDA", "INIRIDA", price_2013_2016$city)
price_2013_2016$city <- gsub("VILLAVINCENCIO", "VILLAVICENCIO", price_2013_2016$city)
price_2013_2016$city <- gsub("RIHOACHA", "RIOHACHA", price_2013_2016$city)
price_2013_2016$city <- gsub("NAYA", "BUENAVENTURA", price_2013_2016$city)
price_2013_2016$city <- gsub("TAMBO", "EL TAMBO", price_2013_2016$city)
price_2013_2016$city <- gsub("PTO. NARE", "PUERTO NARE", price_2013_2016$city)
price_2013_2016$city <- gsub("PTO. COLOMBIA", "PUERTO COLOMBIA", price_2013_2016$city)
price_2013_2016$city <- gsub("TOLU", "SANTIAGO DE TOLU", price_2013_2016$city)
price_2013_2016$city <- gsub("MARIQUITA", "SAN SEBASTIAN DE MARIQUITA", price_2013_2016$city)
price_2013_2016$city <- gsub("PUERTO MILAN", "MILAN", price_2013_2016$city)
price_2013_2016$city <- gsub("PTO. SANTANDER", "PUERTO SANTANDER", price_2013_2016$city)

price_2013_2016 <- price_2013_2016 %>% filter(!(city %in% c("SUR DE BOLIVAR", "CATATUMBO", "BAJO CAUCA", "MAGDALENA MEDIO", "CAUCA")))
price_2013_2016$city <- ifelse(price_2013_2016$city == "SAN JOSE" & price_2013_2016$department == "Guaviare", "SAN JOSE DEL GUAVIARE", price_2013_2016$city)

price_2013_2016 <- price_2013_2016 %>% 
  rename(municipio=city, depto=department) %>% 
  left_join(municipios_capital %>% select(-id_depto), by=c("municipio", "depto")) %>% 
  left_join(municipios_capital %>% select(depto, id_depto) %>% unique, by="depto")
price_2013_2016 %>% filter(is.na(id))
# write.csv(price_2013_2016, "Colombia Data/Colombia Price Data 2013-2016 edited.csv", row.names=F)



#### for later 2013-2021 data
price_2013_2015 <- read_xlsx("Colombia Data/Colombia Price Data 2013-2015 for R.xlsx")
price_2013_2015$city <- str_to_upper(price_2013_2015$city) %>% stri_trans_general("Latin-ASCII")
price_2013_2015$department <- str_to_title(price_2013_2015$department)

price_2016_2021 <- read_xlsx("Colombia Data/Colombia Price Data 2016-2021 for R.xlsx")
price_2016_2021$city <- str_to_upper(price_2016_2021$city) %>% stri_trans_general("Latin-ASCII")
price_2016_2021$department <- str_to_title(price_2016_2021$department) %>% stri_trans_general("Latin-ASCII")

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

price_2016_2021 %>% filter(!(department %in% unique(municipios_capital$depto))) %>% pull(department) %>% unique
price_2016_2021$department <- gsub(" Del ", " del ", price_2016_2021$department)
price_2016_2021$department <- gsub(" De ", " de ", price_2016_2021$department)
price_2016_2021$department <- gsub("Cauqueta", "Caqueta", price_2016_2021$department)
price_2016_2021$department <- gsub("Antiioquia", "Antioquia", price_2016_2021$department)
price_2016_2021$department <- gsub("Vale del Cauca", "Valle del Cauca", price_2016_2021$department)
# cities of Valle department are N/A, so deleted
# deleted department of "N/A", "24", "18", "9"
price_2016_2021 <- price_2016_2021 %>% filter(department %in% municipios_capital$depto)

municipios_capital$municipio %>% unique %>% sort
# AMAZONAS, Amazonas -> Leticia (caplital city)
# CUNDINAMARCA, Cundinamarca -> will assign the same depto_id
# VILLA GARZON -> VILLAGARZON
# PIEDRA, Narino -> TAMINANGO
# SANTA FE, Narino -> EL TABLON DE GOMEZ
# TUMACO, Narino -> SAN ANDRES DE TUMACO
# URABA, Antioquia: Urabá Antioquia is a subregion consisting of Apartadó, Arboletes, Carepa, Chigorodó, Murindó, Mutatá, Turbo, Necoclí,
# San Juan de Urabá, San Pedro de Urabá, and Vigía del Fuerte: only 3 observations
# PUERTO INIRIDA, Guainia -> INIRIDA
# CARTAGENA, Boliva -> CARTAGENA DE INDIAS
# SUR DE BOLIVAR corresponds to the southern end of Bolívar: only 3 observations
# CATATUMBO is a subregion in the northeast of Norte de Santander (https://es.wikipedia.org/wiki/Regi%C3%B3n_del_Catatumbo): only 4 observations
# BAJO CAUCA is a territorial subregion in the northeast of Antioquia: only 14 observations with 2 prices. The rest are arroba
# Cauca, Cauca only 1 observation
# SAN JOSE, Guaviare -> SAN JOSE DEL GUAVIARE
price_2013_2015 %>% filter(!(city %in% unique(municipios_capital$municipio))) %>% pull(city) %>% unique
price_2013_2015$city <- gsub("AMAZONAS", "LETICIA", price_2013_2015$city)
price_2013_2015$city <- gsub("METROPOLITANA DE BOGOTA", "BOGOTA", price_2013_2015$city)
price_2013_2015$city <- gsub("TUMACO", "SAN ANDRES DE TUMACO", price_2013_2015$city)
price_2013_2015$city <- gsub("APARATDO", "APARTADO", price_2013_2015$city)
price_2013_2015$city <- gsub("PUERTO INIRIDA", "INIRIDA", price_2013_2015$city)
price_2013_2015$city <- gsub("VILLAVINCENCIO", "VILLAVICENCIO", price_2013_2015$city)
price_2013_2015$city <- gsub("RIHOACHA", "RIOHACHA", price_2013_2015$city)
price_2013_2015$city <- gsub("RIOACHA", "RIOHACHA", price_2013_2015$city)
price_2013_2015$city <- gsub("NAYA", "BUENAVENTURA", price_2013_2015$city)
price_2013_2015$city <- gsub("TAMBO", "EL TAMBO", price_2013_2015$city)
price_2013_2015$city <- gsub("PTO. NARE", "PUERTO NARE", price_2013_2015$city)
price_2013_2015$city <- gsub("PTO. COLOMBIA", "PUERTO COLOMBIA", price_2013_2015$city)
price_2013_2015$city <- gsub("TOLU", "SANTIAGO DE TOLU", price_2013_2015$city)
price_2013_2015$city <- gsub("MARIQUITA", "SAN SEBASTIAN DE MARIQUITA", price_2013_2015$city)
price_2013_2015$city <- gsub("PUERTO MILAN", "MILAN", price_2013_2015$city)
price_2013_2015$city <- gsub("NUIQUI", "NUQUI", price_2013_2015$city)
price_2013_2015$city <- gsub("PTO. SANTANDER", "PUERTO SANTANDER", price_2013_2015$city)

price_2013_2015 <- price_2013_2015 %>% filter(!(city %in% c("URABA", "SUR DE BOLIVAR", "CATATUMBO", "BAJO CAUCA", "MAGDALENA MEDIO", "CAUCA")))
price_2013_2015$department <- ifelse(price_2013_2015$city == "BOGOTA", "Bogota", price_2013_2015$department)
price_2013_2015$city <- ifelse(price_2013_2015$city == "SAN JOSE" & price_2013_2015$department == "Guaviare", "SAN JOSE DEL GUAVIARE", price_2013_2015$city)

price_2013_2015 <- price_2013_2015 %>% 
  rename(municipio=city, depto=department) %>% 
  left_join(municipios_capital %>% select(-id_depto), by=c("municipio", "depto")) %>% 
  left_join(municipios_capital %>% select(depto, id_depto) %>% unique, by="depto")
price_2013_2015 %>% filter(is.na(id)) %>% view
# write.csv(price_2013_2015, "Colombia Data/Colombia Price Data 2013-2015 edited.csv", row.names=F)

municipios_capital %>% filter(grepl("CAUCASIA", municipio))
# CAQUETA, CAQUETA only 1 observation
municipios_capital$municipio %>% unique %>% sort
price_2016_2021 %>% filter(!(city %in% unique(municipios_capital$municipio))) %>% pull(city) %>% unique
price_2016_2021$city <- gsub("APARATDO", "APARTADO", price_2016_2021$city)
price_2016_2021$city <- gsub("PUERTO INIRIDA", "INIRIDA", price_2016_2021$city)
price_2016_2021$city <- gsub("VILLAVINCENCIO", "VILLAVICENCIO", price_2016_2021$city)
price_2016_2021$city <- gsub("TUMACO", "SAN ANDRES DE TUMACO", price_2016_2021$city)
price_2016_2021$city <- gsub("RIOACHA", "RIOHACHA", price_2016_2021$city)
price_2016_2021$city <- gsub("TOLU", "SANTIAGO DE TOLU", price_2016_2021$city)
price_2016_2021$city <- gsub("MARIQUITA", "SAN SEBASTIAN DE MARIQUITA", price_2016_2021$city)
price_2016_2021$city <- gsub("UBATE", "VILLA DE SAN DIEGO DE UBATE", price_2016_2021$city)
price_2016_2021$city <- gsub("QUINBAYA", "QUIMBAYA", price_2016_2021$city)
price_2016_2021$city <- gsub("PEREIRALES", "PEREIRA", price_2016_2021$city)
price_2016_2021$city <- gsub("DOVIO", "EL DOVIO", price_2016_2021$city)
price_2016_2021$city <- gsub("CAUCACIA", "CAUCASIA", price_2016_2021$city)
price_2016_2021$city <- gsub("ZONA URABA", "URABA", price_2016_2021$city)
price_2016_2021$city <- gsub("REGION URABA", "URABA", price_2016_2021$city)

price_2016_2021 <- price_2016_2021 %>% filter(!is.na(city) & !(city %in% c("CAQUETA", "X", "N/A")))
price_2016_2021$department <- ifelse(price_2016_2021$city == "BOGOTA", "Bogota", price_2016_2021$department)
price_2016_2021$city <- ifelse(price_2016_2021$city == "SAN JOSE" & price_2016_2021$department == "Guaviare", "SAN JOSE DEL GUAVIARE", price_2016_2021$city)

price_2016_2021 <- price_2016_2021 %>% 
  rename(municipio=city, depto=department) %>% 
  left_join(municipios_capital %>% select(-id_depto), by=c("municipio", "depto")) %>% 
  left_join(municipios_capital %>% select(depto, id_depto) %>% unique, by="depto") %>% 
  mutate(id=as.numeric(id), id_depto=as.numeric(id_depto))

# Some municipios has 2 rows of the same cocaine type. Cannot just sum for multiple rows
price_2016_2021 %>% filter(is.na(id)) %>% view
price_2016_2021 %>% group_by(month, year, id) %>% 
  summarise(seeds=sum(seeds),
            leaves=sum(leaves),
            paste_avg=sum(paste_avg),
            paste_wholesale=sum(paste_wholesale),
            paste_retail=sum(paste_retail),
            base_avg=sum(base_avg),
            base_wholesale=sum(base_wholesale),
            base_retail=sum(base_retail),
            hyd_avg=sum(hyd_avg),
            hyd_wholesale=sum(hyd_wholesale),
            hyd_retail=sum(hyd_retail))
# write.csv(price_2016_2021, "Colombia Data/Colombia Price Data 2013-2015 edited.csv", row.names=F)