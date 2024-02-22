# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

municipios_capital <- municipios@data %>% mutate(municipio=str_to_upper(municipio, locale="en"))
municipios_capital$municipio <- stri_trans_general(municipios_capital$municipio, "Latin-ASCII")
municipios_capital$depto <-  stri_trans_general(municipios_capital$depto, "Latin-ASCII")

municipios_capital$depto <- gsub(" De ", " de ", municipios_capital$depto)
municipios_capital$depto <- gsub(" Del ", " del ", municipios_capital$depto)
municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
municipios_capital$depto <- gsub(" Y ", " y ", municipios_capital$depto)
municipios_capital$depto <- gsub("Bogota, D. C.", "Bogota", municipios_capital$depto)
municipios_capital$municipio <- gsub(", D.C.", "", municipios_capital$municipio)
municipios_capital$municipio <- gsub("GUADALAJARA DE BUGA", "BUGA", municipios_capital$municipio)

# data from https://simplemaps.com/data/world-cities and https://geokeo.com/database/town/co
# cities <- read.csv("Colombia Data/cities and towns.csv") %>% as_tibble

# cities <- read.csv("Colombia Data/world cities.csv") %>% as_tibble %>%
#   filter(country == "Colombia") %>% 
#   select
# cities$city <- str_to_upper(stri_trans_general(cities$city, "Latin-ASCII"))
# cities <- cities %>% filter(department != "")
# cities <- cities %>% filter(department != "San AndrA(C)s y Providencia")
# cities$department <- stri_trans_general(cities$department, "Latin-ASCII")
# cities$department <- gsub("BogotA!", "Bogota", cities$department)
# cities$department <- gsub("AtlA!ntico", "Atlantico", cities$department)
# cities$department <- gsub("BolA-var", "Bolivar", cities$department)
# cities$department <- gsub("CA³rdoba", "Cordoba", cities$department)
# cities$department[which(cities$department == "NariA+/-o")] <- "Narino"
# cities$department <- gsub("QuindA-o", "Quindio", cities$department)
# cities$department <- gsub("CaquetA!", "Caqueta", cities$department)
# cities$department <- gsub("BoyacA!", "Boyaca", cities$department)
# cities$department <- gsub("ChocA³", "Choco", cities$department)
# cities$department[which(cities$department == "VaupA(C)s")] <- "Vaupes"
# cities$department <- gsub("GuainA-a", "Guainia", cities$department)
# write.csv(cities, "Colombia Data/Colombia cities.csv", row.names = F)

cities <- read.csv("Colombia Data/Colombia cities.csv") %>% as_tibble
base_to_base  <- read_xlsx("Colombia Data/Anecdotal base to base.xlsx")
base_to_base$`source municipio` <- str_to_upper(base_to_base$`source municipio`)
base_to_base$`Destine municipio` <- str_to_upper(base_to_base$`Destine municipio`)

HCl_to_HCl  <- read_xlsx("Colombia Data/Anecdotal HCl to HCl.xlsx")
HCl_to_HCl$`source municipio` <- str_to_upper(HCl_to_HCl$`source municipio`)
HCl_to_HCl$`Destine municipio` <- str_to_upper(HCl_to_HCl$`Destine municipio`)

same_town_names <- which(table(cities$city) > 1) %>% names
cities %>% filter(!(city %in% base_to_base$`source municipio`))
base_to_base %>% filter(!(`source municipio` %in% cities$city))# %>% pull(`source municipio`) %>% unique
base_to_base %>% filter(!(`Destine municipio` %in% cities$city))# %>% pull(`Destine municipio`) %>% unique
HCl_to_HCl %>% filter(!(`source municipio` %in% cities$city))# %>% pull(`source municipio`) %>% unique
HCl_to_HCl %>% filter(!(`Destine municipio` %in% cities$city))# %>% pull(`Destine municipio`) %>% unique

base_to_base %>% filter(!(`source Depto` %in% unique(municipios_capital$depto)))
base_to_base %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto)))
# Putomayo -> Putumayo
# Guajira -> La Guajira

base_to_base %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio)))
# TUMACO, Narino -> SAN ANDRES DE TUMACO
# RESINAS CARTAGENA -> CARTAGENA DE INDIAS
# VEGALARGA -> NEIVA
# CRUCE DE LAS DELICIAS (no information, city in Cuba)
# INSPECCIÓN BALSILLAS -> VILLAVIEJA, Huila
# SAN VINCENTE DEL CAGUAN -> SAN VICENTE DEL CAGUAN
# BALSELLAS, Caqueta -> VALPARAISO, Caqueta
# SAN JOSE DE LA FRAGUA, Caqueta -> SAN JOSE DEL FRAGUA
base_to_base %>% filter(!(`Destine municipio` %in% unique(municipios_capital$municipio)))
# PURISIMA, Cordoba -> PURISIMA DE LA CONCEPCION
# SAHAGUAN, Cordoba -> SAHAGUN
# SAN BERNANDO DEL VIENTO, Cordoba -> SAN BERNARDO DEL VIENTO
# SANTA CRUZ DE LORICA, Cordoba -> LORICA
# TIERRASALTA, Cordoba -> TIERRALTA

base_to_base <- base_to_base %>% rename(municipio=`source municipio`, depto=`source Depto`)
base_to_base <- left_join(base_to_base, municipios_capital[,-2], by=c("municipio", "depto")) %>% 
  rename(source_id=id,
         source_municipio=municipio,
         source_depto=depto,
         municipio=`Destine municipio`,
         depto=`Destine Depto.`) %>% 
  left_join(municipios_capital[,-2], by=c("municipio", "depto")) %>% 
  rename(destination_id=id,
         destination_municipio=municipio,
         destination_depto=depto)
base_to_base %>% filter(destination_municipio == "BOGOTA")
# base_to_base %>% write.csv("Colombia Data/Anecdotal base to base with id.csv", row.names=F)


HCl_to_HCl %>% filter(!(`source Depto` %in% unique(municipios_capital$depto))) %>% pull(`source Depto`) %>% unique
HCl_to_HCl %>% filter(!(`Destine Depto.` %in% unique(municipios_capital$depto))) %>% pull(`Destine Depto.`) %>% unique

HCl_to_HCl %>% filter(!(`source municipio` %in% unique(municipios_capital$municipio))) %>% pull(`source municipio`) %>% unique
HCl_to_HCl %>% filter(!(`Destine municipio` %in% unique(municipios_capital$municipio))) %>% pull(`Destine municipio`) %>% unique

# base-to-base network map
towns <- read.csv("Colombia Data/cities and towns.csv") %>% as_tibble
base_to_base_match <- base_to_base[,1:2] %>%
  rename(city=`source municipio`,
         department=`source Depto`)
base_to_base_match <- left_join(base_to_base_match, towns, by=c("city", "department"))
base_to_base_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# ALBAN, Narino -> ALBAN, Cundinamarca
# OLAYA HERRERA, Narino is entire municipio
base_to_base$source_lat <- base_to_base_match$lat
base_to_base$source_long <- base_to_base_match$long

base_to_base_match <- base_to_base[,3:4] %>%
  rename(city=`Destine municipio`,
         department=`Destine Depto.`)
base_to_base_match <- left_join(base_to_base_match, towns, by=c("city", "department"))
base_to_base_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# LORICA, Cordoba -> SANTA CRUZ DE LORICA
base_to_base$destine_lat <- base_to_base_match$lat
base_to_base$destine_long <- base_to_base_match$long
# write.csv(base_to_base, "Colombia Data/Anecdotal base to base with coordinates.csv", row.names=F)

map <- departamentos
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean
map_df <- left_join(map_df, municipios_capital %>% mutate(id=id_depto) %>% select(id, depto) %>% unique, by="id")

# for (i in 1:nrow(towns)) {
#   long_i <- towns$long[i]
#   lat_i <- towns$lat[i]
#   dep_index <- which(abs(long_i - map_df$long) < 0.01)
#   candidate_dep <- map_df[dep_index,]
#   candidate_dep_minmax <- candidate_dep %>% 
#     group_by(id) %>% 
#     summarise(min_lat = min(lat), max_lat=max(lat), depto=depto[1])
#   for (j in 1:nrow(candidate_dep_minmax)) {
#     depto_j <- candidate_dep_minmax[j,]
#     if (lat_i >= depto_j$min_lat & lat_i <= depto_j$max_lat) {
#       towns$department[i] <- depto_j$depto
#     }
#   }
#   
# }
# write.csv(towns, "Colombia Data/cities and towns.csv", row.names=F)

empty_map <- ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill = ""),
             color = "black",
           linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  # scale_fill_gradientn(colors = palette(1)) +
  scale_fill_manual(values="white",na.value = "white") +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank(),
        legend.position="none"
        )


base_to_base_map <- empty_map
base_to_base_map <- base_to_base_map +
  geom_point(data=base_to_base %>% filter(`source Depto` != "?" & `Destine Depto.` != "?"),
             aes(x=source_long, 
                 y=source_lat,
                 color=`source Depto`),
             size=0.1) +
  geom_segment(data=base_to_base %>% filter(`source Depto` != "?" & `Destine Depto.` != "?"),
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destine_long,
                   yend=destine_lat,
                   color=`source Depto`),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
               ) +
  labs(title="Base to Base", color="Source Dep.") + 
  theme(legend.position="right")
# ggsave("Colombia Data/Figs/base to base map.png", base_to_base_map, scale=1)

base_paste_seizure <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca paste and base (kg).xlsx")[-1,] %>% 
  rename(id_depto=CodDepto, id=CodMpio) %>% 
  mutate(id=as.numeric(id))

map <- municipios
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(!(id %in% c(88001, 88564))) # excludes islands in Caribbean
palette <- colorRampPalette(c("grey60", "#b30000"))

map_df
log(base_paste_seizure[,-(1:4)]) %>% summary
for (year in 2007:2022) {
  year_index <- which(names(base_paste_seizure) == year)
  map_df_y <- left_join(map_df, base_paste_seizure[,c(3, year_index)], by="id")
  map_df_y[,ncol(map_df_y)] <- log(map_df_y[,ncol(map_df_y)])
  names(map_df_y)[ncol(map_df_y)] <- "seizure"
  
  base_paste_seizrue_map_y <- ggplot(map_df_y, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill = seizure),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_gradientn(colors = palette(10), na.value = "white", limits=c(-8,10)) +
    labs(x="", y="", title=year) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  ggsave(paste0("Colombia Data/Figs/log base-paste seizure/log base-paste seizure ", year, ".png"), base_paste_seizrue_map_y, scale=1)
}


# HCl map
towns <- read.csv("Colombia Data/cities and towns.csv") %>% as_tibble
HCl_to_HCl_match <- HCl_to_HCl[,1:2] %>%
  rename(city=`source municipio`,
         department=`source Depto`)
HCl_to_HCl_match <- left_join(HCl_to_HCl_match, towns, by=c("city", "department"))
HCl_to_HCl_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# Norte de Santader -> Norte de Santander
# AQUAZUL, Casanare -> AGUAZUL
# CANAS GORDAS, Antioquia -> CANASGORDAS
# PENON, Boliva -> EL PENON
# SAMONDOCO, Boyaca -> SOMONDOCO
# SAN LUIS DE DACENO, Boyaca -> SAN LUIS DE GACENO
# SAN PABLE DE BORBUR, Boyaca -> SAN PABLO DE BORBUR
# SANTO DOMINGO DE SILOS, Norte de Santander -> SILOS
# SOGAMOZO, Boyaca -> SOGAMOSO
# TIMBIQUUI, Cauca -> TIMBIQUI
# SOTARA, Cauca and Calima, Valle are entire municipios
HCl_to_HCl$source_lat <- HCl_to_HCl_match$lat
HCl_to_HCl$source_long <- HCl_to_HCl_match$long

HCl_to_HCl_match <- HCl_to_HCl[,3:4] %>%
  rename(city=`Destine municipio`,
         department=`Destine Depto.`)
HCl_to_HCl_match <- left_join(HCl_to_HCl_match, towns, by=c("city", "department"))
HCl_to_HCl_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# ARIGUANI and ZONA BANANERA, Magdalena are entire municipios
# MANU, Casanare -> MANI
# PAZ DE ARIPOSO, Casanare -> PAZ DE ARIPORO
# PROVINDENCIA, Narino -> PROVIDENCIA
# Paraguachón MAICAO, La Guajira -> Paraguachón
HCl_to_HCl$destine_lat <- HCl_to_HCl_match$lat
HCl_to_HCl$destine_long <- HCl_to_HCl_match$long
# write.csv(HCl_to_HCl, "Colombia Data/Anecdotal HCl to HCl with coordinates.csv", row.names=F)

HCl_to_HCl <- HCl_to_HCl %>% filter(!grepl("\\?", `source Depto`) & !grepl("\\?", `Destine Depto.`))
HCl_to_HCl_map <- empty_map
HCl_to_HCl_map <- HCl_to_HCl_map +
  geom_point(data=HCl_to_HCl,
             aes(x=source_long, 
                 y=source_lat,
                 color=`source Depto`),
             size=0.1) +
  geom_segment(data=HCl_to_HCl,
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destine_long,
                   yend=destine_lat,
                   color=`source Depto`),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
  ) +
  labs(title="HCl to HCl", color="Source Dep.") +
  theme(legend.position="right")
# ggsave("Colombia Data/Figs/HCl to HCl map.png", HCl_to_HCl_map, scale=1)

for (depto in unique(HCl_to_HCl$`source Depto`)) {
  HCl_to_HCl_depto <- HCl_to_HCl %>% filter(`source Depto` == depto)
  HCl_to_HCl_sub <- empty_map +
    geom_point(data=HCl_to_HCl_depto,
               aes(x=source_long, 
                   y=source_lat,
                   color=`source Depto`),
               size=0.1) +
    geom_segment(data=HCl_to_HCl_depto,
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destine_long,
                     yend=destine_lat,
                     color=`source Depto`),
                 linewidth = 0.1,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    ) +
    labs(title=paste0("HCl to HCl ", "(", depto, ")"), color="Source Dep.") +
    theme(legend.position="none")
  ggsave(paste0("Colombia Data/Figs/Anecdotal flows map/HCl to HCl map ", "(", depto, ").png"), HCl_to_HCl_sub, scale=1)
}

# General map
general <- read_xlsx("Colombia Data/Anecdotal general.xlsx")
general$`source municipio` <- str_to_upper(general$`source municipio`)
general$`Destine municipio` <- str_to_upper(general$`Destine municipio`)

general_match <- general[,1:2] %>%
  rename(city=`source municipio`,
         department=`source Depto`)
general_match <- left_join(general_match, towns, by=c("city", "department"))
general_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
general$source_lat <- general_match$lat
general$source_long <- general_match$long

general_match <- general[,3:4] %>%
  rename(city=`Destine municipio`,
         department=`Destine Depto.`)
general_match <- left_join(general_match, towns, by=c("city", "department"))
general_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# BETUALIA, Antioquia -> BETULIA
# COCORDIA, Antioquia -> CONCORDIA
# DONMATIAS, Antioquia -> DON MATIAS
# LIORINA, Antioquia -> LIBORINA
# PALONUEVO, Atlantico -> POLONUEVO
# RIOACHA, La Guajira -> RIOHACHA
# SAHAGUAN, Coroba -> SAHAGUN
# SAN BERNANDO DEL VIENTO, Cordoba -> SAN BERNARDO DEL VIENTO
# SANTA CRUZ DE MONPOX, Bolivar -> SANTA CRUZ DE MOMPOX
# TIERRASALTA, Cordoba -> TIERRALTA
# VAGACHI, Antioquia -> VEGACHI
general$destine_lat <- general_match$lat
general$destine_long <- general_match$long

general_map <- empty_map
general_map <- general_map +
  geom_point(data=general,
             aes(x=source_long, 
                 y=source_lat,
                 color=`source Depto`),
             size=0.1) +
  geom_segment(data=general,
               aes(x=source_long, 
                   y=source_lat, 
                   xend=destine_long,
                   yend=destine_lat,
                   color=`source Depto`),
               linewidth = 0.1,
               arrow=arrow(angle=10,
                           length=unit(0.1, "cm"),
                           type="closed")
  ) +
  labs(title="General", color="Source Dep.") +
  theme(legend.position="right")
# ggsave("Colombia Data/Figs/Anecdotal flows map/General map.png", general_map, scale=1)

for (depto in unique(general$`Destine Depto.`)) {
  general_depto <- general %>% filter(`Destine Depto.` == depto)
  general_sub <- empty_map +
    geom_point(data=general_depto,
               aes(x=source_long, 
                   y=source_lat,
                   color=`Destine Depto.`),
               size=0.1) +
    geom_segment(data=general_depto,
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destine_long,
                     yend=destine_lat,
                     color=`Destine Depto.`),
                 linewidth = 0.1,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    ) +
    labs(title=paste0("General ", "(", depto, ")"), color="Destine Dep.") +
    theme(legend.position="none")
  ggsave(paste0("Colombia Data/Figs/Anecdotal flows map/General map ", "(", depto, ").png"), general_sub, scale=1)
}

## Annual map
# towns <- read.csv("Colombia Data/cities and towns.csv") %>% as_tibble
# 
# for (i in 1:nrow(towns)) {
#   long_i <- towns$long[i]
#   lat_i <- towns$lat[i]
#   dep_index <- which(abs(long_i - map_df$long) < 0.005)
#   candidate_dep <- map_df[dep_index,]
#   candidate_dep_minmax <- candidate_dep %>%
#     group_by(id) %>%
#     summarise(min_lat = min(lat), max_lat=max(lat), depto=depto[1])
#   for (j in 1:nrow(candidate_dep_minmax)) {
#     depto_j <- candidate_dep_minmax[j,]
#     if (lat_i >= depto_j$min_lat & lat_i <= depto_j$max_lat) {
#       towns$department[i] <- depto_j$depto
#     }
#   }
# }
# towns <- towns %>% 
#   group_by(city, department) %>% 
#   summarise(lat=lat[1], long=long[1])
# write.csv(towns, "Colombia Data/cities and towns 2.csv", row.names=F)

# remove_space <- function(string) {
#   n_letters <- str_length(string)
#   result <- ifelse(substr(string, n_letters, n_letters) == " ", substr(string, 1, n_letters-1), string)
#   return(result)
# }
# {
# anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual.csv") %>% as_tibble
# anecdotal_annual <- anecdotal_annual %>% 
#   mutate(SOURCE.MUNICIPIO.CITY=stri_trans_general(SOURCE.MUNICIPIO.CITY, "Latin-ASCII"),
#          SOURCE.DEPARTAMENTO=stri_trans_general(SOURCE.DEPARTAMENTO, "Latin-ASCII") %>% str_to_title,
#          DESTINATION..CITY.=stri_trans_general(DESTINATION..CITY., "Latin-ASCII"),
#          DESTINATION.DEPARTAMENTO=stri_trans_general(DESTINATION.DEPARTAMENTO, "Latin-ASCII") %>% str_to_title)
# anecdotal_annual$SOURCE.DEPARTAMENTO <- gsub(" De ", " de ", anecdotal_annual$SOURCE.DEPARTAMENTO)
# anecdotal_annual$SOURCE.DEPARTAMENTO <- gsub(" Del ", " del ", anecdotal_annual$SOURCE.DEPARTAMENTO)
# anecdotal_annual$SOURCE.DEPARTAMENTO[which(anecdotal_annual$SOURCE.MUNICIPIO.CITY == "BOGOTA")] <- "Bogota"
# 
# anecdotal_annual$DESTINATION.DEPARTAMENTO <- gsub(" De ", " de ", anecdotal_annual$DESTINATION.DEPARTAMENTO)
# anecdotal_annual$DESTINATION.DEPARTAMENTO <- gsub(" Del ", " del ", anecdotal_annual$DESTINATION.DEPARTAMENTO)
# anecdotal_annual$DESTINATION.DEPARTAMENTO[which(anecdotal_annual$DESTINATION..CITY. == "BOGOTA")] <- "Bogota"
# 
# anecdotal_annual$SOURCE.MUNICIPIO.CITY <- str_to_upper(anecdotal_annual$SOURCE.MUNICIPIO.CITY)
# anecdotal_annual$DESTINATION..CITY. <- str_to_upper(anecdotal_annual$DESTINATION..CITY.)
# }
# 
# towns <- read.csv("Colombia Data/cities and towns 2.csv") %>% as_tibble
# annual_data_match <- anecdotal_annual[,3:4] %>%
#   rename(city=SOURCE.MUNICIPIO.CITY,
#          department=SOURCE.DEPARTAMENTO)
# annual_data_match <- left_join(annual_data_match, towns, by=c("city", "department"))
# annual_data_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# ALBAN, Narino is entire municipio
# ALBANIA, Tolima -> LA ALBANIA
# BELEN, Caqueta -> BELEN DE LOS ANDAQUIES

# anecdotal_annual$source_lat <- annual_data_match$lat
# anecdotal_annual$source_long <- annual_data_match$long
# 
# annual_data_match <- anecdotal_annual[,5:6] %>%
#   rename(city=DESTINATION..CITY.,
#          department=DESTINATION.DEPARTAMENTO)
# annual_data_match <- left_join(annual_data_match, towns, by=c("city", "department"))
# annual_data_match %>% filter(is.na(lat)) %>% unique %>% arrange(city, department)
# 
# anecdotal_annual$destination_lat <- annual_data_match$lat
# anecdotal_annual$destination_long <- annual_data_match$long
# write.csv(anecdotal_annual, "Colombia Data/Anecdotal annual with coordinates.csv", row.names=F)

anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual with coordinates.csv") %>% as_tibble
map <- departamentos
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean
map_df <- left_join(map_df, municipios_capital %>% mutate(id=id_depto) %>% select(id, depto) %>% unique, by="id")

for (year in unique(anecdotal_annual$YEAR) %>% sort) {
  annual_annecdotal_map_year <- empty_map +
    geom_point(data=anecdotal_annual %>% filter(PROCESS == "BASE" & YEAR == year),
               aes(x=source_long, 
                   y=source_lat,
                   color=SOURCE.DEPARTAMENTO),
               size=0.1) +
    geom_segment(data=anecdotal_annual %>% filter(PROCESS == "BASE" & YEAR == year),
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destination_long,
                     yend=destination_lat,
                     color=SOURCE.DEPARTAMENTO),
                 linewidth = 0.1,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    ) +
    labs(title=paste0("Base to Base (", year, ")"), color="Source Dep.") +
    theme(legend.position="right")
  ggsave(paste0("Colombia Data/Figs/Annual base to base map (", year, ").png"), annual_annecdotal_map_year, scale=1)
}

for (year in unique(anecdotal_annual$YEAR) %>% sort) {
  annual_annecdotal_map_year <- empty_map +
    geom_point(data=anecdotal_annual %>% filter(PROCESS == "COCAINE" & YEAR == year),
               aes(x=source_long, 
                   y=source_lat,
                   color=SOURCE.DEPARTAMENTO),
               size=0.1) +
    geom_segment(data=anecdotal_annual %>% filter(PROCESS == "COCAINE" & YEAR == year),
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destination_long,
                     yend=destination_lat,
                     color=SOURCE.DEPARTAMENTO),
                 linewidth = 0.1,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    ) +
    labs(title=paste0("HCL to HCL (", year, ")"), color="Source Dep.") +
    theme(legend.position="right")
  fig_scale <- ifelse(year > 2015, 1.3, 1)
  ggsave(paste0("Colombia Data/Figs/Annual HCL to HCL map (", year, ").png"), annual_annecdotal_map_year, scale=fig_scale)
}

for (year in unique(anecdotal_annual$YEAR) %>% sort) {
  annual_annecdotal_map_year <- empty_map +
    geom_point(data=anecdotal_annual %>% filter(PROCESS == "GENERAL" & YEAR == year),
               aes(x=source_long, 
                   y=source_lat,
                   color=SOURCE.DEPARTAMENTO),
               size=0.1) +
    geom_segment(data=anecdotal_annual %>% filter(PROCESS == "GENERAL" & YEAR == year),
                 aes(x=source_long, 
                     y=source_lat, 
                     xend=destination_long,
                     yend=destination_lat,
                     color=SOURCE.DEPARTAMENTO),
                 linewidth = 0.1,
                 arrow=arrow(angle=10,
                             length=unit(0.1, "cm"),
                             type="closed")
    ) +
    labs(title=paste0("GENERAL (", year, ")"), color="Source Dep.") +
    theme(legend.position="right")
  ggsave(paste0("Colombia Data/Figs/Annual GENERAL map (", year, ").png"), annual_annecdotal_map_year, scale=1)
}
