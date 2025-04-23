# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)
library(caret)
library(randomForest)
library(pracma)
library(GWmodel)
library(pROC)
library(glmnet)
library(reshape2)
library(regclass)
########## used bandwidth range 0.5~3.0 due to the time limit
{
  municipios_capital <- municipios@data %>% mutate(municipio=str_to_upper(municipio, locale="en"))
  municipios_capital$id <- as.numeric(municipios_capital$id)
  municipios_capital$municipio <- stri_trans_general(municipios_capital$municipio, "Latin-ASCII")
  municipios_capital$depto <-  stri_trans_general(municipios_capital$depto, "Latin-ASCII")
  municipios_capital$depto <- gsub(" De ", " de ", municipios_capital$depto)
  municipios_capital$depto <- gsub(" Del ", " del ", municipios_capital$depto)
  municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
  municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
  municipios_capital$depto <- gsub("Bogota, D. C.", "Bogota", municipios_capital$depto)
  municipios_capital$municipio <- gsub(", D.C.", "", municipios_capital$municipio)
  municipios_capital$municipio <- gsub("GUADALAJARA DE BUGA", "BUGA", municipios_capital$municipio)
  municipios_capital <- municipios_capital %>% filter(!(id %in% c(88001, 88564))) %>% as_tibble
  
  map <- municipios
  map_df <- suppressMessages(fortify(map)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(!(id %in% c(88001, 88564)))
  map_df <- left_join(map_df, municipios_capital %>% unique, by="id")
  
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id, municipio, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  labs_hyd <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (COCAINE HYDROCHLORIDE).csv") %>% as_tibble
  labs_hyd <- labs_hyd %>% rename(id=CODMPIO) %>% mutate(id=as.numeric(id)) %>%
    select(-c(CODDEPTO, MUNICIPIO, DEPARTAMENTO)) %>% 
    left_join(municipios_capital %>% select(-id_depto), by="id") %>% 
    relocate(id, depto, municipio, X1997:X2022) %>% 
    pivot_longer(-(id:municipio), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer) %>% 
    group_by(depto, year) %>% 
    summarize(n_labs = sum(n_labs, na.rm = T))
  labs_PPI <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (PRIMARY PRODUCTION INFRASTRUCTURE).csv") %>% as_tibble
  labs_PPI <- labs_PPI %>% rename(id=CODMPIO) %>% mutate(id=as.numeric(id)) %>%
    select(-c(CODDEPTO, MUNICIPIO, DEPARTAMENTO)) %>% 
    left_join(municipios_capital %>% select(-id_depto), by="id") %>% 
    relocate(id, depto, municipio, X1997:X2022) %>% 
    pivot_longer(-(c(id:municipio,)), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer) %>% 
    group_by(depto, year) %>% 
    summarize(n_labs = sum(n_labs, na.rm = T))
  
  coca_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca leaves (kg).xlsx")
  base_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca paste and base (kg).xlsx")
  hyd_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Cocaine (kg).xlsx")
  coca_seizures <- coca_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  base_seizures <- base_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  hyd_seizures <- hyd_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  
  coca_seizures_long <- coca_seizures %>% 
    left_join(municipios_capital %>% select(-id_depto), by="id") %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="coca_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(coca_seizures)) %>% 
    group_by(depto, year) %>% 
    summarize(coca_seizures = sum(coca_seizures, na.rm = T))
  base_seizures_long <- base_seizures %>% 
    left_join(municipios_capital %>% select(-id_depto), by="id") %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="base_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(base_seizures)) %>% 
    group_by(depto, year) %>% 
    summarize(base_seizures = sum(base_seizures, na.rm = T))
  hyd_seizures_long <- hyd_seizures %>% 
    left_join(municipios_capital %>% select(-id_depto), by="id") %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="hyd_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(hyd_seizures)) %>% 
    group_by(depto, year) %>% 
    summarize(hyd_seizures = sum(hyd_seizures, na.rm = T))
  
  coca_seizures_avg_med <- coca_seizures_long %>% 
    group_by(year) %>% 
    summarize(national_avg = mean(coca_seizures, na.rm=T),
              national_med = median(coca_seizures, na.rm=T)) %>% 
    pivot_longer(-year, names_to = "depto", values_to = "coca_seizures")
  coca_seizures_long <- bind_rows(coca_seizures_long, coca_seizures_avg_med)
  base_seizures_avg_med <- base_seizures_long %>% 
    group_by(year) %>% 
    summarize(national_avg = mean(base_seizures, na.rm=T),
              national_med = median(base_seizures, na.rm=T)) %>% 
    pivot_longer(-year, names_to = "depto", values_to = "base_seizures")
  base_seizures_long <- bind_rows(base_seizures_long, base_seizures_avg_med)
  hyd_seizures_avg_med <- hyd_seizures_long %>% 
    group_by(year) %>% 
    summarize(national_avg = mean(hyd_seizures, na.rm=T),
              national_med = median(hyd_seizures, na.rm=T)) %>% 
    pivot_longer(-year, names_to = "depto", values_to = "hyd_seizures")
  hyd_seizures_long <- bind_rows(hyd_seizures_long, hyd_seizures_avg_med)
  labs_hyd_avg_med <- labs_hyd %>% 
    group_by(year) %>% 
    summarize(national_avg = mean(n_labs, na.rm=T),
              national_med = median(n_labs, na.rm=T)) %>% 
    pivot_longer(-year, names_to = "depto", values_to = "n_labs")
  labs_hyd <- bind_rows(labs_hyd, labs_hyd_avg_med)
  labs_PPI_avg_med <- labs_PPI %>% 
    group_by(year) %>% 
    summarize(national_avg = mean(n_labs, na.rm=T),
              national_med = median(n_labs, na.rm=T)) %>% 
    pivot_longer(-year, names_to = "depto", values_to = "n_labs")
  labs_PPI <- bind_rows(labs_PPI, labs_PPI_avg_med)
  
  price_2013_2015 <- read.csv("Colombia Data/Colombia Price Data 2013-2015 edited.csv") %>% as_tibble
  price_2016_2021 <- read.csv("Colombia Data/Colombia Price Data 2016-2021 edited.csv") %>% as_tibble
  price <- rbind(price_2013_2015, price_2016_2021 %>% select(-seeds, -leaves))
  
  price_annual <- price %>% 
    group_by(id, year) %>% 
    summarize(base_avg=mean(base_avg, na.rm=T),
              hyd_avg=mean(hyd_avg, na.rm=T)
    ) %>% 
    mutate(base_avg=ifelse(is.nan(base_avg), NA, base_avg),
           hyd_avg=ifelse(is.nan(hyd_avg), NA, hyd_avg)
    ) %>% 
    filter(!is.na(id)) %>% 
    left_join(municipios_capital %>% select(id, depto), by="id") %>% 
    ungroup(id) %>% select(-id) %>% 
    pivot_longer(c("base_avg", "hyd_avg"), names_to = "drug", values_to = "price")
  price_annual_avg_med <- price_annual %>% 
    group_by(drug, year) %>% 
    summarize(national_avg = mean(price, na.rm=T),
              national_med = median(price, na.rm=T)) %>% 
    pivot_longer(c("national_avg", "national_med"), names_to = "depto", values_to = "price")
  price_annual <- bind_rows(price_annual, price_annual_avg_med)
}

Ecuador_border <- c("Narino", "Putumayo", "national_avg", "national_med")
East_coast <- c("Antioquia", "Atlantico", "Bolivar", "Cauca", "Choco", "Cordoba", "La Guajira", "Magdalena", "Sucre", "Valle del Cauca", "national_avg", "national_med")
Venezuela_border <- c("Arauca", "Boyaca", "Cesar", "Guainia", "La Guajira", "Norte de Santander", "Vichada", "national_avg", "national_med")
coca_seizures_long %>% ggplot() +
  geom_line(aes(x=year, y=coca_seizures, group=depto, color=depto))
coca_seizures_long %>%
  filter(depto %in% Ecuador_border) %>% ggplot() +
  geom_line(aes(x=year, y=coca_seizures, group=depto, color=depto))

base_seizures_long %>% ggplot() +
  geom_line(aes(x=year, y=base_seizures, group=depto, color=depto))
base_seizures_long %>%
  filter(depto %in% Ecuador_border) %>% ggplot() +
  geom_line(aes(x=year, y=base_seizures, group=depto, color=depto))

hyd_seizures_long %>% ggplot() +
  geom_line(aes(x=year, y=hyd_seizures, group=depto, color=depto))
hyd_seizures_long %>%
  filter(depto %in% Ecuador_border) %>% ggplot() +
  geom_line(aes(x=year, y=hyd_seizures, group=depto, color=depto))

labs_PPI %>% ggplot() + ggtitle("PPI Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))
labs_PPI %>%
  filter(depto %in% Ecuador_border) %>% ggplot() + ggtitle("PPI Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))

labs_hyd %>% ggplot() + ggtitle("hyd Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))
labs_hyd %>%
  filter(depto %in% Ecuador_border) %>% ggplot() + ggtitle("hyd Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))


# East Coast
coca_seizures_long %>%
  filter(depto %in% East_coast) %>% ggplot() +
  geom_line(aes(x=year, y=coca_seizures, group=depto, color=depto))

base_seizures_long %>%
  filter(depto %in% East_coast) %>% ggplot() +
  geom_line(aes(x=year, y=base_seizures, group=depto, color=depto))

hyd_seizures_long %>%
  filter(depto %in% East_coast) %>% ggplot() +
  geom_line(aes(x=year, y=hyd_seizures, group=depto, color=depto))

labs_PPI %>%
  filter(depto %in% East_coast) %>% ggplot() + ggtitle("PPI Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))

labs_hyd %>%
  filter(depto %in% East_coast) %>% ggplot() + ggtitle("hyd Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))


# Venezuela Border
coca_seizures_long %>%
  filter(depto %in% Venezuela_border) %>% ggplot() +
  geom_line(aes(x=year, y=coca_seizures, group=depto, color=depto))

base_seizures_long %>%
  filter(depto %in% Venezuela_border) %>% ggplot() +
  geom_line(aes(x=year, y=base_seizures, group=depto, color=depto))

hyd_seizures_long %>%
  filter(depto %in% Venezuela_border) %>% ggplot() +
  geom_line(aes(x=year, y=hyd_seizures, group=depto, color=depto))

labs_PPI %>%
  filter(depto %in% Venezuela_border) %>% ggplot() + ggtitle("PPI Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))

labs_hyd %>%
  filter(depto %in% Venezuela_border) %>% ggplot() + ggtitle("hyd Labs") +
  geom_line(aes(x=year, y=n_labs, group=depto, color=depto))


# price data
price_annual %>% filter(drug == "base_avg" & depto %in% Ecuador_border) %>%
  ggplot() + ggtitle("base prices") +
  geom_line(aes(x=year, y=price, group=depto, color=depto))

price_annual %>% filter(drug == "hyd_avg" & depto %in% Ecuador_border) %>%
  ggplot() + ggtitle("hyd prices") +
  geom_line(aes(x=year, y=price, group=depto, color=depto))

price_annual %>% filter(drug == "base_avg" & depto %in% East_coast) %>%
  ggplot() + ggtitle("base prices") +
  geom_line(aes(x=year, y=price, group=depto, color=depto))

price_annual %>% filter(drug == "hyd_avg" & depto %in% East_coast) %>%
  ggplot() + ggtitle("hyd prices") +
  geom_line(aes(x=year, y=price, group=depto, color=depto))

price_annual %>% filter(drug == "base_avg" & depto %in% Venezuela_border) %>%
  ggplot() + ggtitle("base prices") +
  geom_line(aes(x=year, y=price, group=depto, color=depto))

price_annual %>% filter(drug == "hyd_avg" & depto %in% Venezuela_border) %>%
  ggplot() + ggtitle("hyd prices") +
  geom_line(aes(x=year, y=price, group=depto, color=depto))


