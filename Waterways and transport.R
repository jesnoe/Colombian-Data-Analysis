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
}

waterways <- st_read("Colombia Data/Shape Data/gis_osm_waterways_free_1.shp")
waterways$fclass %>% unique
rivers <- waterways %>% filter(fclass == "river")

transport <- st_read("Colombia Data/Shape Data/gis_osm_transport_free_1.shp")
ferry <- transport %>% filter(fclass == "ferry_terminal")

roads <- st_read("Colombia Data/Shape Data/gis_osm_roads_free_1.shp")
trunk_roads <- roads %>% filter(fclass == "trunk")
primary_roads <- roads %>% filter(fclass == "primary")
railways <- st_read("Colombia Data/Shape Data/gis_osm_railways_free_1.shp")

base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
anecdotal_annual  <- read.csv("Colombia Data/Anecdotal annual municipality only.csv") %>% as_tibble

depto_map <- departamentos
depto_map_df <- suppressMessages(fortify(depto_map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean

empty_map <- ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group),
               color = "black",
               fill = "white",
               linewidth = 0.3) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
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

table(waterways$fclass)

waterway_map <- waterways %>% ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="All Waterways") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )
# ggsave("Colombia Data/Figs/waterways.png", waterway_map, scale=1)

river_map <- rivers %>% ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="All Rivers") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )
# ggsave("Colombia Data/Figs/rivers.png", river_map, scale=1)

broad_river_map <- rivers %>%
  filter(width > 0) %>% 
  ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1,
               alpha=1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="Rivers (Width > 0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )
# ggsave("Colombia Data/Figs/broad rivers.png", broad_river_map, scale=1)

canals <- waterways %>% filter(fclass == "canal")
canal_map <- canals %>% ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="Canals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )
# ggsave("Colombia Data/Figs/canals.png", canal_map, scale=1)

river_ferry_map <- rivers %>% ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="All Rivers") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  ) +
  geom_point(data=st_coordinates(ferry$geometry) %>% as.data.frame,
             aes(x=X, y=Y),
             color="red",
             size=0.2)
# ggsave("Colombia Data/Figs/rivers and ports.png", river_ferry_map, scale=1)

ferry_df <- st_coordinates(ferry) %>% as.data.frame %>% mutate(fclass="ferry_terminal")
airport <- transport %>% filter(fclass == "airport")
airport_df <- st_coordinates(airport) %>% as.data.frame %>% mutate(fclass="airport")
ferry_airport_coord <- rbind(ferry_df, airport_df) %>% mutate(fclass=as.factor(fclass))
  
river_ferry_airport_map <- rivers %>% ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="All Rivers") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) +
  geom_point(data=ferry_airport_coord,
             aes(x=X, y=Y, color=fclass),
             size=0.2) +
  scale_color_manual(labels=c("airport", "ferry_terminal"),
                     values=c("violet", "red"))
  
# ggsave("Colombia Data/Figs/rivers and airports.png", river_ferry_airport_map, scale=2)

broad_river_ferry_map <- rivers %>%
  filter(width > 0) %>% 
  ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1,
               alpha=1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="Rivers (Width > 0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  ) +
  geom_point(data=st_coordinates(ferry$geometry) %>% as.data.frame,
             aes(x=X, y=Y),
             color="red",
             size=0.2)
# ggsave("Colombia Data/Figs/broad rivers and ports.png", broad_river_ferry_map, scale=1)

trunk_primary <- roads %>% filter(fclass %in% c("primary", "trunk"))
roads_map <- trunk_primary %>% ggplot() + 
  geom_sf(aes(color=ifelse(fclass=="primary", "primary", "trunk")), linewidth = 0.3) +
  geom_polygon(data=depto_map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = depto_map_df$long, y = depto_map_df$lat) + 
  labs(color="Road type", x="", y="", title="Primary/Trunk Roads") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )
# ggsave("Colombia Data/Figs/roads.png", roads_map, scale=1)

railways_map <- railways %>% ggplot() + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  geom_sf(color="blue", linewidth = 0.2) +
  geom_polygon(data=map_df,
               aes(x=long,
                   y=lat,
                   group=group),
               color = "black",
               fill = NA,
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  labs(fill="", x="", y="", title="Railways") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  )
# ggsave("Colombia Data/Figs/railways.png", railways_map, scale=1)

### rivers and anecdotal flows
ferry <- transport %>% filter(fclass == "ferry_terminal")
river_ferry_base_map <- river_ferry_map + 
  labs(fill="", x="", y="", title="Base to Base") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  ) +
  geom_segment(data=base_to_base %>% filter(source.Depto != "?" & Destine.Depto. != "?"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destine_long,
                   yend=destine_lat),
               color="green",
               linewidth = 0.3,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
  )
# ggsave("Colombia Data/Figs/base to base with rivers and ports.png", river_ferry_base_map, scale=2)

river_ferry_base_map_2017 <- river_ferry_map +
  labs(fill="", x="", y="", title="Base to Base (2017)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
  ) +
  geom_segment(data=anecdotal_annual %>% filter(YEAR == 2017 & PROCESS == "BASE"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destination_long,
                   yend=destination_lat),
               color="green",
               linewidth = 0.3,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
  )
# ggsave("Colombia Data/Figs/base to base with rivers and ports (2017).png", river_ferry_base_map_2017, scale=2)

anecdotal_on_object <- function(object_map, anecdotal_data, map_title) {
  result_map <- object_map + 
    labs(fill="", x="", y="", title=map_title) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank(),
          legend.position="none"
    ) +
    geom_segment(data=anecdotal_data,
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destine_long,
                     yend=destine_lat,
                     color=source.Depto),
                 linewidth = 0.2,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    )
  return(result_map)
}

anecdotal_on_object_year <- function(object_map, anecdotal_data, type, year, map_title) {
  result_map <- object_map + 
    labs(fill="", x="", y="", title=paste(map_title, year)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank(),
          legend.position="none"
    ) +
    geom_segment(data=anecdotal_annual %>% filter(YEAR == year & PROCESS == type),
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destination_long,
                     yend=destination_lat,
                     color=SOURCE.DEPARTAMENTO),
                 linewidth = 0.3,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    )
  return(result_map)
}

river_ferry_HCl_map <- anecdotal_on_object(river_ferry_map, HCl_to_HCl, "HCl to HCl")
# ggsave("Colombia Data/Figs/HCl to HCl with rivers and ports.png", river_ferry_HCl_map, scale=2)

river_ferry_HCl_map_2017 <- anecdotal_on_object_year(river_ferry_map, anecdotal_annual, "COCAINE", 2017, "HCl to HCl")
# ggsave("Colombia Data/Figs/HCl to HCl with rivers and ports (2017).png", river_ferry_HCl_map_2017, scale=2)

river_ferry_general_map <- anecdotal_on_object(river_ferry_map, general, "General")
# ggsave("Colombia Data/Figs/general with rivers and ports.png", river_ferry_general_map, scale=2)

river_ferry_general_map_2017 <- anecdotal_on_object_year(river_ferry_map, anecdotal_annual, "GENERAL", 2017, "General")
# ggsave("Colombia Data/Figs/general with rivers and ports (2017).png", river_ferry_general_map_2017, scale=2)
