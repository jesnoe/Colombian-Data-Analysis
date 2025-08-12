# setwd("/Users/R")
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
library(logistf)
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
  
  depto_map <- suppressMessages(fortify(departamentos)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(id != 88) %>% 
    left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
  
  empty_map <- ggplot(depto_map, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group),
                 color = "black",
                 fill="white",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map$long, y = depto_map$lat) + 
    coord_quickmap() +
    labs(fill="", x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
}

hyd_to_hyd <- read.csv("Colombia Data/Anecdotal annual with municipality.csv") %>% as_tibble %>% filter(PROCESS == "COCAINE" & YEAR %in% c(2013, 2014, 2016))

hyd_to_hyd_map <- empty_map +
  geom_point(data=hyd_to_hyd,
             aes(x=source_long, 
                 y=source_lat,
                 color=destination_depto),
             size=0.1) +
  geom_segment(data=hyd_to_hyd,
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destination_long,
                   yend=destination_lat,
                   color=destination_depto),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.2, "cm"),
                           type="closed")
  ) +
  labs(title="Hyd to Hyd", color="Destination Dep.") +
  theme(legend.position="right")
# ggsave("Colombia Data/Figs/hyd to hyd map (2013-2016).png", hyd_to_hyd_map, scale=1)

hyd_destination_map <- empty_map +
  geom_point(data=hyd_to_hyd,
             aes(x=destination_long, 
                 y=destination_lat),
             size=0.1)
# ggsave("Colombia Data/Figs/hyd destination map (2013-2016).png", hyd_destination_map, scale=1)
