# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
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
  
}

waterways <- st_read("Colombia Data/Shape Data/gis_osm_waterways_free_1.shp")
waterways$fclass %>% unique
rivers <- waterways %>% filter(fclass == "river")

transport <- st_read("Colombia Data/Shape Data/gis_osm_transport_free_1.shp")
ferry <- transport %>% filter(fclass == "ferry_terminal")
airport <- transport %>% filter(fclass == "airport")

roads <- st_read("Colombia Data/Shape Data/gis_osm_roads_free_1.shp")
trunk_roads <- roads %>% filter(fclass == "trunk")
primary_roads <- roads %>% filter(fclass == "primary")
secondary_roads <- roads %>% filter(fclass == "secondary")
tertiary_roads <- roads %>% filter(fclass == "tertiary")
railways <- st_read("Colombia Data/Shape Data/gis_osm_railways_free_1.shp")

base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual municipality only.csv") %>% as_tibble

population <- read.csv("Colombia Data/Census population by municipios (2018).csv") %>% as_tibble
population$log_population <- log(population$population)


## n_airports
airport_points <- st_coordinates(airport$geometry) %>% as.data.frame
municipio_min_max <- map_df %>%
  group_by(id, municipio, depto) %>% 
  summarise(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))

municipio_min_max$n_airports <- 0
for (i in 1:nrow(municipio_min_max)) {
  muni_coord <- map_df %>% filter(id == municipio_min_max$id[i])
  min_long_i <- municipio_min_max$min_long[i]
  max_long_i <- municipio_min_max$max_long[i]
  min_lat_i <- municipio_min_max$min_lat[i]
  max_lat_i <- municipio_min_max$max_lat[i]
  
  candidates <- airport_df %>%
    filter(X >= min_long_i & X <= max_long_i & Y >= min_lat_i & Y <= max_lat_i)
  
  if (nrow(candidates) < 1) next
  
  in_muni_airports <- point.in.polygon(candidates$X, candidates$Y, muni_coord$long, muni_coord$lat)
  municipio_min_max$n_airports[i] <- length(in_muni_airports)
  
}
municipio_min_max
# write.csv((municipio_min_max %>% select(id, n_airports))[,-1], "Colombia Data/airports.csv", row.names=F)


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

# write.csv(river_length_muni %>% select(-(min_long:max_lat)), "Colombia Data/rivers.csv", row.names=F)

river_length_muni <- read.csv("Colombia Data/rivers.csv") %>% as_tibble

## rivers histogram
  # river_length
par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% base_to_base$source_id) %>% pull(river_length) %>% 
  hist(breaks=50, main="Base to Base Sources", xlab="River Length", ylab="", xlim=c(0, 44))
river_length_muni %>% filter(id %in% base_to_base$destination_id) %>% pull(river_length) %>% 
  hist(breaks=50, main="Destinations", xlab="River Length", ylab="", xlim=c(0, 44))
river_length_muni %>% filter(!(id %in% c(base_to_base$source_id, base_to_base$destination_id))) %>% pull(river_length) %>% 
  hist(breaks=50, main="Unlabeled", xlab="River Length", ylab="", xlim=c(0, 44))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% HCl_to_HCl$source_id) %>% pull(river_length) %>% 
  hist(breaks=50, main="HCl to HCl Sources", xlab="River Length", ylab="", xlim=c(0, 44))
river_length_muni %>% filter(id %in% HCl_to_HCl$destination_id) %>% pull(river_length) %>% 
  hist(breaks=50, main="Destinations", xlab="River Length", ylab="", xlim=c(0, 44))
river_length_muni %>% filter(!(id %in% c(HCl_to_HCl$source_id, HCl_to_HCl$destination_id))) %>% pull(river_length) %>% 
  hist(breaks=50, main="Unlabeled", xlab="River Length", ylab="", xlim=c(0, 44))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% general$source_id) %>% pull(river_length) %>% 
  hist(breaks=50, main="General Sources", xlab="River Length", ylab="", xlim=c(0, 44))
river_length_muni %>% filter(id %in% general$destination_id) %>% pull(river_length) %>% 
  hist(breaks=50, main="Destinations", xlab="River Length", ylab="", xlim=c(0, 44))
river_length_muni %>% filter(!(id %in% c(general$source_id, general$destination_id))) %>% pull(river_length) %>% 
  hist(breaks=50, main="Unlabeled", xlab="River Length", ylab="", xlim=c(0, 44))
par(mfrow=c(1,1))

  # n_rivers
par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% base_to_base$source_id) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Base to Base Sources", xlab="# of Rivers", ylab="", xlim=c(0, 250))
river_length_muni %>% filter(id %in% base_to_base$destination_id) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Destinations", xlab="# of Rivers", ylab="", xlim=c(0, 250))
river_length_muni %>% filter(!(id %in% c(base_to_base$source_id, base_to_base$destination_id))) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Unlabeled", xlab="# of Rivers", ylab="", xlim=c(0, 250))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% base_to_base$source_id) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Base to Base Sources", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
river_length_muni %>% filter(id %in% base_to_base$destination_id) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Destinations", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
river_length_muni %>% filter(!(id %in% c(base_to_base$source_id, base_to_base$destination_id))) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Unlabeled", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
par(mfrow=c(1,1))


par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% HCl_to_HCl$source_id) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="HCl to HCl Sources", xlab="# of Rivers", ylab="", xlim=c(0, 250))
river_length_muni %>% filter(id %in% HCl_to_HCl$destination_id) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Destinations", xlab="# of Rivers", ylab="", xlim=c(0, 250))
river_length_muni %>% filter(!(id %in% c(HCl_to_HCl$source_id, HCl_to_HCl$destination_id))) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Unlabeled", xlab="# of Rivers", ylab="", xlim=c(0, 250))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% HCl_to_HCl$source_id) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="HCl to HCl Sources", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
river_length_muni %>% filter(id %in% HCl_to_HCl$destination_id) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Destinations", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
river_length_muni %>% filter(!(id %in% c(HCl_to_HCl$source_id, HCl_to_HCl$destination_id))) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Unlabeled", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
par(mfrow=c(1,1))


par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% general$source_id) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="General Sources", xlab="# of Rivers", ylab="", xlim=c(0, 250))
river_length_muni %>% filter(id %in% general$destination_id) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Destinations", xlab="# of Rivers", ylab="", xlim=c(0, 250))
river_length_muni %>% filter(!(id %in% c(general$source_id, general$destination_id))) %>% pull(n_rivers) %>% 
  hist(breaks=50, main="Unlabeled", xlab="# of Rivers", ylab="", xlim=c(0, 250))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
river_length_muni %>% filter(id %in% general$source_id) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="General Sources", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
river_length_muni %>% filter(id %in% general$destination_id) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Destinations", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
river_length_muni %>% filter(!(id %in% c(general$source_id, general$destination_id))) %>% pull(n_big_rivers) %>% 
  hist(breaks=50, main="Unlabeled", xlab="# of Big Rivers", ylab="", xlim=c(0, 4))
par(mfrow=c(1,1))

river_length_muni %>% arrange(desc(n_rivers))

  # n_rivers > 0
river_length_muni %>% filter(id %in% base_to_base$source_id) %>% pull(n_rivers) %>% table
river_length_muni %>% filter(id %in% base_to_base$destination_id) %>% pull(n_rivers) %>% table
river_length_muni %>% filter(!(id %in% c(base_to_base$source_id, base_to_base$destination_id))) %>% pull(n_rivers) %>% table

river_length_muni %>% filter(id %in% HCl_to_HCl$source_id) %>% pull(n_rivers) %>% table
river_length_muni %>% filter(id %in% HCl_to_HCl$destination_id) %>% pull(n_rivers) %>% table
river_length_muni %>% filter(!(id %in% c(HCl_to_HCl$source_id, HCl_to_HCl$destination_id))) %>% pull(n_rivers) %>% table

river_length_muni %>% filter(id %in% general$source_id) %>% pull(n_rivers) %>% table
river_length_muni %>% filter(id %in% general$destination_id) %>% pull(n_rivers) %>% table
river_length_muni %>% filter(!(id %in% c(general$source_id, general$destination_id))) %>% pull(n_rivers) %>% table

## population
  # population histogram
par(mfrow=c(1,3))
population %>% filter(id %in% base_to_base$source_id) %>% pull(log_population) %>% 
  hist(breaks=20, main="Base to Base Sources", xlab="Log Population", ylab="", xlim=c(5, 16))
population %>% filter(id %in% base_to_base$destination_id) %>% pull(log_population) %>% 
  hist(breaks=20, main="Destinations", xlab="Log Population", ylab="", xlim=c(5, 16))
population %>% filter(!(id %in% c(base_to_base$source_id, base_to_base$destination_id))) %>% pull(log_population) %>% 
  hist(breaks=20, main="Unlabeled", xlab="Log Population", ylab="", xlim=c(5, 16))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
population %>% filter(id %in% HCl_to_HCl$source_id) %>% pull(log_population) %>% 
  hist(breaks=20, main="HCl to HCl Sources", xlab="Log Population", ylab="", xlim=c(5, 16))
population %>% filter(id %in% HCl_to_HCl$destination_id) %>% pull(log_population) %>% 
  hist(breaks=20, main="Destinations", xlab="Log Population", ylab="", xlim=c(5, 16))
population %>% filter(!(id %in% c(HCl_to_HCl$source_id, HCl_to_HCl$destination_id))) %>% pull(log_population) %>% 
  hist(breaks=20, main="Unlabeled", xlab="Log Population", ylab="", xlim=c(5, 16))
par(mfrow=c(1,1))

par(mfrow=c(1,3))
population %>% filter(id %in% general$source_id) %>% pull(log_population) %>% 
  hist(breaks=20, main="General Sources", xlab="Log Population", ylab="", xlim=c(5, 16))
population %>% filter(id %in% general$destination_id) %>% pull(log_population) %>% 
  hist(breaks=20, main="Destinations", xlab="Log Population", ylab="", xlim=c(5, 16))
population %>% filter(!(id %in% c(general$source_id, general$destination_id))) %>% pull(log_population) %>% 
  hist(breaks=20, main="Unlabeled", xlab="Log Population", ylab="", xlim=c(5, 16))
par(mfrow=c(1,1))

population$source_id <- population$id
population$destination_id <- population$id

base_to_base_pop_diff <- base_to_base %>%
  select(source_id, destination_id) %>% 
  left_join(population %>% select(source_id, log_population), by="source_id") %>% 
  rename(source_pop=log_population)
base_to_base_pop_diff <- base_to_base_pop_diff %>%
  left_join(population %>% select(destination_id, log_population), by="destination_id") %>% 
  rename(destination_pop=log_population)
base_to_base_pop_diff$pop_diff <- base_to_base_pop_diff$destination_pop - base_to_base_pop_diff$source_pop

HCl_to_HCl_pop_diff <- HCl_to_HCl %>%
  select(source_id, destination_id) %>% 
  left_join(population %>% select(source_id, log_population), by="source_id") %>% 
  rename(source_pop=log_population)
HCl_to_HCl_pop_diff <- HCl_to_HCl_pop_diff %>%
  left_join(population %>% select(destination_id, log_population), by="destination_id") %>% 
  rename(destination_pop=log_population)
HCl_to_HCl_pop_diff$pop_diff <- HCl_to_HCl_pop_diff$destination_pop - HCl_to_HCl_pop_diff$source_pop
  
general_pop_diff <- general %>%
  select(source_id, destination_id) %>% 
  left_join(population %>% select(source_id, log_population), by="source_id") %>% 
  rename(source_pop=log_population)
general_pop_diff <- general_pop_diff %>%
  left_join(population %>% select(destination_id, log_population), by="destination_id") %>% 
  rename(destination_pop=log_population)
general_pop_diff$pop_diff <- general_pop_diff$destination_pop - general_pop_diff$source_pop

par(mfrow=c(1,3))
hist(base_to_base_pop_diff$pop_diff, breaks=20, main="Base to Base", xlab="Log Population Diff.", ylab="", xlim=c(-5, 7))
hist(HCl_to_HCl_pop_diff$pop_diff, breaks=20, main="HCl to HCl", xlab="Log Population Diff.", ylab="", xlim=c(-5, 7))
hist(general_pop_diff$pop_diff, breaks=20, main="General", xlab="Log Population Diff.", ylab="", xlim=c(-5, 7))
par(mfrow=c(1,1))

  # population map
population_coord <- left_join(map_df, population %>% select(id, population), by="id") %>% 
  mutate(log_pop = log(population))

palette <- colorRampPalette(c("grey60", "#b30000"))
population_map <- ggplot(population_coord) + 
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group,
                   fill = population),
               color = "black",
               linewidth = 0.1) +
  scale_fill_viridis_c() +
  labs(fill = "Population", x="", y="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())
# ggsave("Colombia Data/Figs/population.png", population_map, scale=1)

population_map <- ggplot(population_coord) + 
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group,
                   fill = log_pop),
           color = "black",
           linewidth = 0.1) +
  scale_fill_viridis_c() +
  labs(fill = "Log Population", x="", y="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())
# ggsave("Colombia Data/Figs/log population.png", population_map, scale=1)

base_to_base_population_map <- population_map +
  labs(fill = "Log Population", x="", y="", title="Log Population with base to base") +
  geom_segment(data=base_to_base %>% filter(source.Depto != "?" & Destine.Depto. != "?"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destine_long,
                   yend=destine_lat,
                   color=source.Depto),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
              )
# ggsave("Colombia Data/Figs/base to base with population.png", base_to_base_population_map, scale=2)

base_to_base_2017_population_map <- population_map +
  labs(fill = "Log Population", x="", y="", title="Log Population with base to base (2017)") +
  geom_segment(data=anecdotal_annual %>% filter(YEAR == 2017 & PROCESS == "BASE"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destination_long,
                   yend=destination_lat,
                   color=SOURCE.DEPARTAMENTO),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed"))
# ggsave("Colombia Data/Figs/base to base with population (2017).png", base_to_base_2017_population_map, scale=2)

HCl_to_HCl_population_map <- population_map +
  labs(fill = "Log Population", x="", y="", title="Log Population with HCl to HCl") +
  geom_segment(data=HCl_to_HCl %>% filter(source.Depto != "?" & source.Depto != "??"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destination_long,
                   yend=destination_lat,
                   color=source.Depto),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
  )
# ggsave("Colombia Data/Figs/HCl to HCl with population.png", HCl_to_HCl_population_map, scale=2)

HCl_to_HCl_2017_population_map <- population_map +
  labs(fill = "Log Population", x="", y="", title="Log Population with HCl to HCl (2017)") +
  geom_segment(data=anecdotal_annual %>% filter(YEAR == 2017 & PROCESS == "COCAINE"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destination_long,
                   yend=destination_lat,
                   color=SOURCE.DEPARTAMENTO),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed"))
# ggsave("Colombia Data/Figs/HCl to HCl with population (2017).png", HCl_to_HCl_2017_population_map, scale=2)

## road length per municipio
road_length_muni <- map_df %>% 
  group_by(id, municipio, depto) %>% 
  summarize(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))

major_roads <- roads %>% filter(fclass %in% c("trunk", "primary", "secondary"))
major_roads_coords <- st_coordinates(major_roads$geometry) %>% as.data.frame
major_roads <- cbind(major_roads,
                     major_roads_coords %>% 
                       group_by(L1) %>% 
                       summarise(min_long=min(X),
                                 max_long=max(X),
                                 min_lat=min(Y),
                                 max_lat=max(Y)) %>% 
                       select(-L1))

get_road_length <- function(coords) {
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

road_length_muni$road_length <- 0
road_length_muni$n_roads <- 0
for (i in 1:nrow(road_length_muni)) {
  min_long_i <- road_length_muni$min_long[i]
  max_long_i <- road_length_muni$max_long[i]
  min_lat_i <- road_length_muni$min_lat[i]
  max_lat_i <- road_length_muni$max_lat[i]
  
  candidates <- major_roads %>%
    filter(min_long <= max_long_i & max_long >= min_long_i & min_lat <= max_lat_i & max_lat >= min_lat_i)
  
  municipio_id_i <- road_length_muni$id[i]
  municipio_coords_i <- map_df %>% filter(id == municipio_id_i) %>% select(long, lat)
  
  if (nrow(candidates) < 1) next
  
  in_roads_index <- c()
  road_length_i <- 0
  for (j in 1:nrow(candidates)) {
    road_coords_i <- st_coordinates(candidates$geometry[j]) %>% as_tibble
    in_points_index <- point.in.polygon(road_coords_i$X, road_coords_i$Y, municipio_coords_i$long, municipio_coords_i$lat)
    
    if (sum(in_points_index) > 0) {
      in_roads_index <- c(in_roads_index, j)
      road_coords_ij <- road_coords_i[which(in_points_index == 1),-3]
      road_length_i <- road_length_i + get_road_length(road_coords_ij)
    }
  }
  n_roads <- length(in_roads_index)
  road_length_muni$road_length[i] <- road_length_i
  road_length_muni$n_roads[i] <- n_roads
}

road_length_muni %>% filter(n_roads > 0) %>% select(-(min_long:max_lat))

# write.csv(road_length_muni %>% select(-(min_long:max_lat)), "Colombia Data/major roads without tertiary.csv", row.names=F)

road_length_muni <- read.csv("Colombia Data/major roads without tertiary.csv") %>% as_tibble
