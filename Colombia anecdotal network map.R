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
base_to_base_match <- base_to_base[,1:2] %>%
  rename(city=`source municipio`,
         department=`source Depto`)
base_to_base_match <- left_join(base_to_base_match, cities, by=c("city", "department"))
towns <- read.csv("Colombia Data/cities and towns.csv") %>% as_tibble %>% filter(department == "") %>% rename(city=city_town)
towns$city <- str_to_upper(stri_trans_general(towns$city, "Latin-ASCII"))
towns$city[which(towns$city == "SANTA BARBARA")] <- "X"
towns$city[which(towns$city == "SANTA BARBARA - ISCUANDE")] <- "SANTA BARBARA"
towns$city[which(towns$city == "BELEN")[1]] <- "X"
towns$city[which(towns$city == "NARINO")[1:2]] <- "X"
towns$city[which(towns$city == "BARAYA")[1]] <- "X"
towns$city[which(towns$city == "SAN ANDRES")[1]] <- "X"
towns$city[which(towns$city == "VALPARAISO")[2]] <- "X" # for VALPARAISO, Antioquia lat: 5.615384099, long: -75.6247028
towns$city[which(towns$city == "ALBANIA")[1:2]] <- "X"
base_to_base_match_na <- base_to_base_match %>% filter(is.na(lat)) %>% select(city, department)
base_to_base_match_na <- left_join(base_to_base_match_na, towns[,-4], by="city")
base_to_base_match_na$lat[41] <- 5.615384099
base_to_base_match_na$long[41] <- -75.6247028
base_to_base_match$lat[is.na(base_to_base_match$lat)] <- base_to_base_match_na$lat
base_to_base_match$long[is.na(base_to_base_match$long)] <- base_to_base_match_na$long
base_to_base$source_lat <- base_to_base_match$lat
base_to_base$source_long <- base_to_base_match$long

base_to_base_match <- base_to_base[,3:4] %>%
  rename(city=`Destine municipio`,
         department=`Destine Depto.`)
base_to_base_match <- left_join(base_to_base_match, cities, by=c("city", "department"))
cities$city[which(cities$city == "BUENAVISTA" & cities$department == "Cordoba")[2]] <- "X"
base_to_base_match_na <- base_to_base_match %>% filter(is.na(lat)) %>% select(city, department)
base_to_base_match_na <- left_join(base_to_base_match_na, towns[,-4], by="city")
base_to_base_match$lat[is.na(base_to_base_match$lat)] <- base_to_base_match_na$lat
base_to_base_match$long[is.na(base_to_base_match$long)] <- base_to_base_match_na$long
base_to_base$destine_lat <- base_to_base_match$lat
base_to_base$destine_long <- base_to_base_match$long


map <- departamentos
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean

empty_map <- ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill = "1"),
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
        line = element_blank()
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
  labs(title="Base to Base", color="Source Dep.")
ggsave("Colombia Data/Figs/base to base map.png", base_to_base_map, scale=1)

base_paste_seizure <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca paste and base (kg).xlsx")[-1,] %>% 
  rename(id_depto=CodDepto, id=CodMpio) %>% 
  mutate(id=as.numeric(id))

map <- municipios
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(!(id %in% c(88001, 88564))) %>% # excludes islands in Caribbean
  as_tibble
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
