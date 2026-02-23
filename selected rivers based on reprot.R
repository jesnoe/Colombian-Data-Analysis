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
  map <- municipios
  map_df <- suppressMessages(fortify(map)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(!(id %in% c(88001, 88564)))
  map_df <- left_join(map_df, municipios_capital %>% unique, by="id")
  
  depto_map <- departamentos
  depto_map_df <- suppressMessages(fortify(depto_map)) %>% 
    mutate(id_depto=as.numeric(id)) %>% 
    filter(id_depto != 88) # excludes islands in Caribbean
  
  transport <- st_read("Colombia Data/Shape Data/gis_osm_transport_free_1.shp")
  ferry <- transport %>% filter(fclass == "ferry_terminal")
  waterways <- st_read("Colombia Data/Shape Data/gis_osm_waterways_free_1.shp")
  waterways$fclass %>% unique
  selected_rivers <- c(
    "Río Catatumbo",
    "Río San Jorge",
    "Río Magdalena",
    "Río Arauca",
    "Río Cauca",
    "Río Atrato",
    "Río Casanare",
    "Río San Juan",
    "Río Cusiana",
    "Río Meta",
    "Río Tomo",
    "Río Orinoco",
    "Río Vichada",
    "Río Uva",
    "Río Naya",
    "Río Patía",
    "Río Mira",
    "Río Mataje",
    "Río Guaviare",
    "Río Vaupés",
    "Río Apaporis",
    "Río Caquetá",
    "Río Putumayo",
    "Río Amazonas", # not in the OSM data
    "Río Negro",
    "Río Inírida"
  )
  rivers <- waterways %>% filter(fclass == "river" & name %in% selected_rivers)
  
}

for (river in selected_rivers) {
  print(waterways %>% filter(fclass == "river" & name == river) %>% nrow)
}


## n_airports
municipio_min_max <- map_df %>%
  group_by(id, municipio, depto) %>% 
  summarise(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))


## Labeling big/small rivers
ferry_points <- st_coordinates(ferry$geometry) %>% as.data.frame
river_coords <- st_coordinates(rivers$geometry) %>% as.data.frame
rivers <- cbind(rivers,
                river_coords %>% 
                  group_by(L1) %>% 
                  summarise(min_long=min(X),
                            max_long=max(X),
                            min_lat=min(Y),
                            max_lat=max(Y)) %>% 
                  select(-L1))
rivers$size <- "small"
for (i in 1:nrow(ferry_points)) {
  long_i <- ferry_points$X[i]
  lat_i <- ferry_points$Y[i]
  
  candidates <- rivers %>%
    filter(min_long <= long_i & max_long >= long_i & min_lat <= lat_i & max_lat >= lat_i)
  if (nrow(candidates) < 1) next
  
  for (j in 1:nrow(candidates)) {
    river_coords_i <- st_coordinates(candidates$geometry[j]) %>% as_tibble %>% select(X,Y)
    min_distance <- sqrt((long_i - river_coords_i$X)^2 + (lat_i - river_coords_i$Y)^2) %>% min
    if (min_distance < 0.05) {
      rivers$size[which(rivers$osm_id == candidates$osm_id[j])] <- "big"
    }
  }
}
rivers$size <- as.factor(rivers$size)
rivers$size %>% table

## river length per municipio
river_length_muni <- map_df %>% 
  group_by(id, municipio, depto) %>% 
  summarize(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))

get_river_length <- function(coords) {
  long <- coords$X
  lat <- coords$Y
  result <- 0
  
  if (nrow(coords) < 2) {
    return(result)
  }
  
  for (i in 1:(nrow(coords)-1)) {
    result <- result + sqrt((long[i] - long[i+1])^2 + (lat[i] - lat[i+1])^2)
  }
  
  return(result)
}

river_length_muni$river_length <- 0
river_length_muni$n_rivers <- 0
river_length_muni$n_big_rivers <- 0
for (i in 1:nrow(river_length_muni)) {
  min_long_i <- river_length_muni$min_long[i]
  max_long_i <- river_length_muni$max_long[i]
  min_lat_i <- river_length_muni$min_lat[i]
  max_lat_i <- river_length_muni$max_lat[i]
  
  candidates <- rivers %>%
    filter(min_long <= max_long_i & max_long >= min_long_i & min_lat <= max_lat_i & max_lat >= min_lat_i)
  
  municipio_id_i <- river_length_muni$id[i]
  municipio_coords_i <- map_df %>% filter(id == municipio_id_i) %>% select(long, lat)
  
  if (nrow(candidates) < 1) next
  
  in_rivers_index <- c()
  river_length_i <- 0
  for (j in 1:nrow(candidates)) {
    river_coords_i <- st_coordinates(candidates$geometry[j]) %>% as_tibble
    in_points_index <- point.in.polygon(river_coords_i$X, river_coords_i$Y, municipio_coords_i$long, municipio_coords_i$lat)
    
    if (sum(in_points_index) > 0) {
      in_rivers_index <- c(in_rivers_index, j)
      river_coords_ij <- river_coords_i[which(in_points_index == 1),-3]
      river_length_i <- river_length_i + get_river_length(river_coords_ij)
    }
  }
  n_rivers <- length(in_rivers_index)
  n_big_rivers <- sum(candidates$size[in_rivers_index] == "big")
  
  river_length_muni$river_length[i] <- river_length_i
  river_length_muni$n_rivers[i] <- n_rivers
  river_length_muni$n_big_rivers[i] <- n_big_rivers
}

river_length_muni %>% filter(n_rivers > 0) %>% select(-(min_long:max_lat))

# write.csv(river_length_muni %>% select(-(min_long:max_lat)), "Colombia Data/rivers (selected).csv", row.names=F)

river_length_muni <- read.csv("Colombia Data/rivers (selected).csv") %>% as_tibble

river_map <- rivers %>% ggplot() + 
  expand_limits(x = depto_map_df$long, y = depto_map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=depto_map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = depto_map_df$long, y = depto_map_df$lat) + 
  labs(fill="", x="", y="", title="All Rivers") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )
# ggsave("Colombia Data/Figs/rivers (selected).png", river_map, scale=1)