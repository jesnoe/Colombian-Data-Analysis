# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

waterways <- st_read("Colombia Data/gis_osm_waterways_free_1.shp")
waterways$fclass %>% unique
waterways$geometry[1]

map <- departamentos
map_df <- suppressMessages(fortify(map)) %>% 
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
ggsave("Colombia Data/Figs/waterways.png", waterway_map, scale=1)


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
ggsave("Colombia Data/Figs/rivers.png", river_map, scale=1)

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
ggsave("Colombia Data/Figs/broad rivers.png", broad_river_map, scale=1)

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
ggsave("Colombia Data/Figs/canals.png", canal_map, scale=1)
