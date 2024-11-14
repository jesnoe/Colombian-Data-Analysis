# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(tidygeocoder)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)

{
municipios_capital <- municipios@data %>%
    mutate(municipio=str_to_upper(municipio, locale="en"),
           depto=str_to_upper(depto, locale="en"))
municipios_capital$id <- as.numeric(municipios_capital$id)
municipios_capital$municipio <- stri_trans_general(municipios_capital$municipio, "Latin-ASCII")
municipios_capital$depto <-  stri_trans_general(municipios_capital$depto, "Latin-ASCII")
# municipios_capital$depto <- gsub(" De ", " de ", municipios_capital$depto)
# municipios_capital$depto <- gsub(" Del ", " del ", municipios_capital$depto)
# municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
# municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
municipios_capital$depto <- gsub("BOGOTA, D. C.", "BOGOTA", municipios_capital$depto)
municipios_capital$municipio <- gsub(", D.C.", "", municipios_capital$municipio)
municipios_capital$municipio <- gsub("GUADALAJARA DE BUGA", "BUGA", municipios_capital$municipio)
anecdote <- read_xlsx("Colombia Data/Anecdotal data (06-29-2024).xlsx")
anecdote <- anecdote %>% 
  mutate(across(source:destination_departamento, function(x) stri_trans_general(x, "Latin-ASCII") %>% str_to_upper))
anecdote_sources <- read.csv("Colombia Data/anecdotal source location names (11-02-2024).csv") %>% as_tibble
anecdote_destinations <- read.csv("Colombia Data/anecdotal destination location names (11-02-2024).csv") %>% as_tibble
anecdote_sources_unique <- read.csv("Colombia Data/anecdotal source location names unique (11-02-2024).csv") %>% as_tibble
anecdote_destinations_unique <- read.csv("Colombia Data/anecdotal destination location names unique (11-02-2024).csv") %>% as_tibble
}
depto <- unique(municipios_capital$depto) %>% sort
municipio <- unique(municipios_capital$municipio) %>% sort
typo_depto <- anecdote %>% filter(!(source_departamento %in% depto)) %>% pull(source_departamento) %>% unique
typo_depto <- c(typo_depto,
                anecdote %>% filter(!(destination_departamento %in% depto)) %>% pull(destination_departamento) %>% unique) %>% unique %>% sort
typo_depto
anecdote %>% filter(destination_departamento == "VENEZUELA")
anecdote %>% filter(source_departamento == "CIERRA DEL GRAMAL")
anecdote %>% filter(destination_departamento == "CIERRA DEL GRAMAL")


anecdote %>% filter(!(source %in% municipio)) %>% pull(source) %>% unique %>% sort
anecdote %>% filter(!(destination %in% municipio)) %>% pull(destination) %>% unique %>% sort

source_not_match_index <- which(!(anecdote$source %in% municipio))
destination_not_match_index <- which(!(anecdote$destination %in% municipio))

anecdote %>% filter(source == "YARUMO")

ex3 <-  geo(address="YARUMO, Colombia", method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

anecdote_sources <- tibble()
anecdote_destinations <- tibble()
start.t <- Sys.time()
for (i in 1:length(source_not_match_index)) {
  index_i <- source_not_match_index[i]
  source_i <- paste(anecdote$source[index_i], anecdote$source_departamento[index_i], "Colombia", sep=", ")
  
  source_i_coords <- geo(address=source_i, method = "arcgis", unique_only = T)
  source_i_rev_geocode <- reverse_geocode(source_i_coords, lat = lat, long = long, address = addr,method = "arcgis", full_results = T)
  anecdote_sources <- rbind(anecdote_sources, source_i_rev_geocode)
  
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
for (i in 1:length(destination_not_match_index)) {
  index_i <- destination_not_match_index[i]
  destination_i <- paste(anecdote$destination[index_i], anecdote$destination_departamento[index_i], "Colombia", sep=", ")
  
  destination_i_coords <- geo(address=destination_i, method = "arcgis", unique_only = T)
  destination_i_rev_geocode <- reverse_geocode(destination_i_coords, lat = lat, long = long, address = addr,method = "arcgis", full_results = T)
  anecdote_destinations <- rbind(anecdote_destinations, destination_i_rev_geocode)
  
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 
# anecdote_sources %>% select(address, District, City, Subregion, Region) %>% unique %>% 
#   write.csv("Colombia Data/anecdotal source location names unique (11-02-2024).csv", row.names=F)
# anecdote_destinations %>% select(address, District, City, Subregion, Region) %>% unique %>% 
#   write.csv("Colombia Data/anecdotal destination location names unique (11-02-2024).csv", row.names=F)
# write.csv(anecdote_sources, "Colombia Data/anecdotal source location names (11-02-2024).csv", row.names=F)
# write.csv(anecdote_destinations, "Colombia Data/anecdotal destination location names (11-02-2024).csv", row.names=F)
anecdote_sources_unique
anecdote_destinations_unique

anecdote_sources_upper <- anecdote_sources %>% 
  select(address, District, City, Subregion, Region) %>% 
  mutate(across(address:Region, function(x) stri_trans_general(x, "Latin-ASCII") %>% str_to_upper))
anecdote_destinations_upper <- anecdote_destinations %>% 
  select(address, District, City, Subregion, Region) %>% 
  mutate(across(address:Region, function(x) stri_trans_general(x, "Latin-ASCII") %>% str_to_upper))
  
anecdote_sources_upper %>% filter(!(Subregion %in% municipio)) %>% pull(Subregion) %>% unique %>% sort
anecdote_destinations_upper %>% filter(!(Subregion %in% municipio)) %>% pull(Subregion) %>% unique %>% sort

