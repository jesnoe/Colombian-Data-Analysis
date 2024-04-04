# setwd("C:/Users/gkfrj/Documents/R")
# devtools::install_github("https://github.com/cran/rgdal.git")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)
library(rgdal)

# ACLED data
article <- read.csv("Colombia Data/LatinAmerica_2018-2024.csv") %>% as_tibble
article$event_type %>% unique %>% sort
article$actor1 %>% unique %>% sort
article %>% filter(country == "Colombia") %>% pull(actor1) %>% unique %>% sort

# NA_SA <- st_read("Colombia Data/NA_SA_NetwordData.gdb")
# NA_SA
# st_coordinates(NA_SA$Shape[1])
# NA_SA[,-ncol(NA_SA)] %>% summary

fgdb <- "Colombia Data/NA_SA_NetwordData.gdb"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

global_shipping <- readOGR(dsn=fgdb, layer="GlobalShipping_DARPA_NorthSouthAmerica")
multimodal_networks <- readOGR(dsn=fgdb, layer="MultimodalNetworks_DARPA_NorthSouthAmerica")
grip_roads <- readOGR(dsn=fgdb, layer="GRIPRoads_DARPA_NorthSouthAmerica") # GRIP = Global Roads Inventory Project

# Determine the FC extent, projection, and attribute information
summary(NA_SA)

# View the feature class
plot(global_shipping)
plot(multimodal_networks)
plot(grip_roads)
