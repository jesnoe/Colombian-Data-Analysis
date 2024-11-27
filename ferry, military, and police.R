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
    mutate(id=as.numeric(id))
  map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")
  
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id, municipio, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  
  regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
    mutate(base_avg=scale(base_avg)[,1],
           paste_avg=scale(paste_avg)[,1],
           hyd_avg=scale(hyd_avg)[,1])
}

## n_ferry_terminal
transport <- st_read("Colombia Data/Shape Data/gis_osm_transport_free_1.shp")
ferry <- transport %>% filter(fclass == "ferry_terminal")
ferry_df <- st_coordinates(ferry) %>% as.data.frame
municipio_min_max <- map_df %>%
  group_by(id, municipio, depto) %>% 
  summarise(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))

municipio_min_max$n_ferry <- 0
for (i in 1:nrow(municipio_min_max)) {
  muni_coord <- map_df %>% filter(id == municipio_min_max$id[i])
  min_long_i <- municipio_min_max$min_long[i]
  max_long_i <- municipio_min_max$max_long[i]
  min_lat_i <- municipio_min_max$min_lat[i]
  max_lat_i <- municipio_min_max$max_lat[i]
  
  candidates <- ferry_df %>%
    filter(X >= min_long_i & X <= max_long_i & Y >= min_lat_i & Y <= max_lat_i)
  
  if (nrow(candidates) < 1) next
  
  in_muni_ferry <- point.in.polygon(candidates$X, candidates$Y, muni_coord$long, muni_coord$lat)
  municipio_min_max$n_ferry[i] <- length(in_muni_ferry)
  
}
municipio_min_max
# write.csv((municipio_min_max %>% select(id, n_ferry))[,-1], "Colombia Data/ferry terminals.csv", row.names=F)


# n_police and military
police <- st_read("Colombia Data/Shape Data/gis_osm_pois_free_1.shp") %>% filter(fclass == "police")
military <- st_read("Colombia Data/Shape Data/gis_osm_landuse_a_free_1.shp") %>% filter(fclass == "military")

police_df <- st_coordinates(police1) %>% as.data.frame
municipio_min_max <- map_df %>%
  group_by(id, municipio, depto) %>% 
  summarise(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))

municipio_min_max$n_polices <- 0
for (i in 1:nrow(municipio_min_max)) {
  muni_coord <- map_df %>% filter(id == municipio_min_max$id[i])
  min_long_i <- municipio_min_max$min_long[i]
  max_long_i <- municipio_min_max$max_long[i]
  min_lat_i <- municipio_min_max$min_lat[i]
  max_lat_i <- municipio_min_max$max_lat[i]
  
  candidates <- police_df %>%
    filter(X >= min_long_i & X <= max_long_i & Y >= min_lat_i & Y <= max_lat_i)
  
  if (nrow(candidates) < 1) next
  
  in_muni_polices <- point.in.polygon(candidates$X, candidates$Y, muni_coord$long, muni_coord$lat)
  municipio_min_max$n_polices[i] <- length(in_muni_polices)
  
}
municipio_min_max
# write.csv((municipio_min_max %>% select(id, n_polices))[,-1], "Colombia Data/polices.csv", row.names=F)

military_df <- military %>% st_centroid %>% st_coordinates %>% as.data.frame
municipio_min_max <- map_df %>%
  group_by(id, municipio, depto) %>% 
  summarise(min_long=min(long),
            max_long=max(long),
            min_lat=min(lat),
            max_lat=max(lat))

municipio_min_max$n_military <- 0
for (i in 1:nrow(municipio_min_max)) {
  muni_coord <- map_df %>% filter(id == municipio_min_max$id[i])
  min_long_i <- municipio_min_max$min_long[i]
  max_long_i <- municipio_min_max$max_long[i]
  min_lat_i <- municipio_min_max$min_lat[i]
  max_lat_i <- municipio_min_max$max_lat[i]
  
  candidates <- military_df %>%
    filter(X >= min_long_i & X <= max_long_i & Y >= min_lat_i & Y <= max_lat_i)
  
  if (nrow(candidates) < 1) next
  
  in_muni_military <- point.in.polygon(candidates$X, candidates$Y, muni_coord$long, muni_coord$lat)
  municipio_min_max$n_military[i] <- length(in_muni_military)
  
}
municipio_min_max
# write.csv((municipio_min_max %>% select(id, n_military))[,-1], "Colombia Data/military.csv", row.names=F)

# maps
depto_map <- departamentos
depto_map_df <- suppressMessages(fortify(depto_map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean
depto_map_df$id <- as.numeric(depto_map_df$id)

empty_map <- ggplot(depto_map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group),
               color = "black",
               fill = "white",
               linewidth = 0.3) + 
  expand_limits(x = depto_map_df$long, y = depto_map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )

ferry <- read.csv("Colombia Data/ferry terminals.csv") %>% as_tibble
police <- read.csv("Colombia Data/polices.csv") %>% as_tibble
military <- read.csv("Colombia Data/military.csv") %>% as_tibble
ferry$n_polices <- police$n_polices
ferry$n_military <- military$n_military
ferry <- ferry %>% 
  mutate(ferry = ifelse(n_ferry > 0, 1, 0) %>% as.factor,
         military = ifelse(n_military > 0, 1, 0) %>% as.factor,
         police = ifelse(n_polices > 0, 1, 0) %>% as.factor)

ferry_map <- empty_map + 
  geom_point(data=ferry_df %>% filter(X >= min(map_df$long) & X <= max(map_df$long)),
             aes(x=X, y=Y), color="blue", size=0.5) + 
  ggtitle("Ferry Terminals")
#ggsave("Colombia Data/Figs/Public data map/ferry terminals.png", ferry_map, scale=1)
map_df_ferry <- map_df %>% left_join(ferry %>% select(id, ferry:police), by="id")

ferry_map <-  ggplot(map_df_ferry,
                     aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=ferry),
               color = "black",
               linewidth = 0.3) + 
  expand_limits(x = map_df_ferry$long, y = map_df_ferry$lat) + 
  scale_fill_manual(values = c("1" = "blue", "0" = "white")) +
  coord_quickmap() +
  labs(fill="ferry", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )
# ggsave("Colombia Data/Figs/Public data map/ferry terminals (municipio).png", ferry_map, scale=1)

military_map <- empty_map + 
  geom_point(data=military_df %>% filter(X >= min(map_df$long) & X <= max(map_df$long)),
             aes(x=X, y=Y), color="red", size=0.5) + 
  ggtitle("Military Landuses (Centroid)")
# ggsave("Colombia Data/Figs/Public data map/military.png", military_map, scale=1)
military_map <-  ggplot(map_df_ferry,
                     aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=military),
               color = "black",
               linewidth = 0.3) + 
  expand_limits(x = map_df_ferry$long, y = map_df_ferry$lat) + 
  scale_fill_manual(values = c("1" = "blue", "0" = "white")) +
  coord_quickmap() +
  labs(fill="military", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )
# ggsave("Colombia Data/Figs/Public data map/military (municipio).png", military_map, scale=1)

police_map <- empty_map + 
  geom_point(data=police_df %>% filter(X >= min(map_df$long) & X <= max(map_df$long)),
             aes(x=X, y=Y), color="red", size=0.5) + 
  ggtitle("Police Posts/Stations")
# ggsave("Colombia Data/Figs/Public data map/police.png", police_map, scale=1)
police_map <-  ggplot(map_df_ferry,
                        aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=police),
               color = "black",
               linewidth = 0.3) + 
  expand_limits(x = map_df_ferry$long, y = map_df_ferry$lat) + 
  scale_fill_manual(values = c("1" = "blue", "0" = "white")) +
  coord_quickmap() +
  labs(fill="police", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )
# ggsave("Colombia Data/Figs/Public data map/police (municipio).png", police_map, scale=1)


ever_anecdotal_hyd_destination <- regression_data_years %>%
  group_by(id) %>% 
  summarize(hyd_destination = ifelse(sum(hyd_destination) > 0, 1, 0)) %>% 
  filter(hyd_destination == 1)
hyd_destination_map <- empty_map + 
  geom_point(data=municipio_centroid %>% filter(id %in% ever_anecdotal_hyd_destination$id),
             aes(x=long, y=lat), color="red", size=0.5) + 
  ggtitle("hyd. Destinations")
# ggsave("Colombia Data/Figs/Public data map/hyd destinations.png", hyd_destination_map, scale=1)