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
}

Ecuador_border <- c("Narino", "Putumayo")
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
