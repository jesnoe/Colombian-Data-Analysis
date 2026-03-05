# setwd("/Users/R")
# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(tidygeocoder)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)
# library(caret)
# library(randomForest)

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
  map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")
  
  # https://ucdp.uu.se/downloads/#section-disaggregated
  conflict <- readRDS("Colombia Data/GEDEvent_v25_1.rds") %>% filter(country == "Colombia" & year %in% 2013:2017) %>% filter(year != 2015) %>% select(id, year, dyad_name, adm_1, adm_2, latitude, longitude) %>% 
    rename(lat = latitude, long = longitude)
  conflict$adm_1 <- gsub(" district", "", conflict$adm_1)
  conflict$adm_1 <- gsub(" department", "", conflict$adm_1) %>% stri_trans_general("Latin-ASCII")
  conflict$adm_2 <- gsub(" municipality", "", conflict$adm_2) %>% stri_trans_general("Latin-ASCII")
  
  violence <- readRDS("Colombia Data/Violent events.rds") %>% as_tibble %>% 
    select(ViolenceType, NumVictims, MonthYear, FARC, AUC, ELN, LocationFinal, gMapLat, gMapLon) %>% 
    rename(lat = gMapLat, long = gMapLon) %>% 
    mutate(MonthYear = as.Date(paste(1, str_to_title(MonthYear)), "%d %B %Y"),
           month = month(MonthYear),
           year = year(MonthYear)) %>% 
    filter(year %in% 2013:2017 & year != 2015) %>% 
    arrange(year, month)
  
  depto_map <- departamentos
  depto_map_df <- suppressMessages(fortify(depto_map)) %>% 
    mutate(id_depto=as.numeric(id)) %>% 
    filter(id_depto != 88) # excludes islands in Caribbean
  depto_map_df <- left_join(depto_map_df, municipios_capital %>% mutate(id=id_depto) %>% select(id, depto) %>% unique, by="id")
  empty_map <- ggplot(depto_map_df, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group),
                 color = "black",
                 fill="white",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map_df$long, y = depto_map_df$lat) + 
    coord_quickmap() +
    labs(fill="", x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
}
conflict
violence

conflict_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(conflict)) {
  latitude_i <- conflict$lat[i]
  longitude_i <- conflict$long[i]
  if (is.na(latitude_i) | is.na(longitude_i)) {
    conflict_locations <- bind_rows(conflict_locations, conflict_locations[i-1,])
    conflict_locations[i,] <- NA
    next
  }
  conflict_i_coords <- tibble(lat = latitude_i, long = longitude_i)
  conflict_i_rev_geocode <- reverse_geocode(conflict_i_coords, lat = lat, long = long, method = "arcgis", full_results = T)
  conflict_locations <- bind_rows(conflict_locations, conflict_i_rev_geocode)
  
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 1.918966 mins

violence_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(violence)) {
  latitude_i <- violence$lat[i]
  longitude_i <- violence$long[i]
  if (is.na(latitude_i) | is.na(longitude_i)) {
    violence_locations <- bind_rows(violence_locations, violence_locations[i-1,])
    violence_locations[i,] <- NA
    next
  }
  violence_i_coords <- tibble(lat = latitude_i, long = longitude_i)
  violence_i_rev_geocode <- reverse_geocode(violence_i_coords, lat = lat, long = long, method = "arcgis", full_results = T)
  violence_locations <- bind_rows(violence_locations, violence_i_rev_geocode)
  
  if (i %% 1000 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 30.24251 mins

# conflict$municipio <- conflict_locations$Subregion
# conflict$depto <- conflict_locations$Region
conflict$municipio <- str_to_upper(conflict$municipio)
conflict$adm_2 <- str_to_upper(conflict$adm_2)
conflict$adm_2 <- gsub("TUMACO", "SAN ANDRES DE TUMACO", conflict$adm_2)
conflict$adm_2 <- gsub("VISTA HERMOSA", "VISTAHERMOSA", conflict$adm_2)
conflict$adm_2 <- gsub("MONTANITA", "LA MONTANITA", conflict$adm_2)
conflict$adm_2 <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", conflict$adm_2)
conflict$municipio <- gsub("TUMACO", "SAN ANDRES DE TUMACO", conflict$municipio)
conflict$municipio <- gsub("VISTA HERMOSA", "VISTAHERMOSA", conflict$municipio)
conflict$municipio <- gsub("MONTANITA", "LA MONTANITA", conflict$municipio)
conflict$municipio <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", conflict$municipio)
conflict$depto <- gsub("Distrito Capital", "Bogota", conflict$depto)
# conflict$municipio <- stri_trans_general(conflict_locations$Subregion, "Latin-ASCII") %>% str_to_upper
# conflict$depto <- stri_trans_general(conflict_locations$Region, "Latin-ASCII")
conflict <- conflict %>% filter(municipio != "")

conflict %>% filter(adm_1 != depto)
conflict$municipio[which(conflict$adm_1 != conflict$depto & !is.na(conflict$adm_2))] <- conflict$adm_2[which(conflict$adm_1 != conflict$depto & !is.na(conflict$adm_2))]
conflict$depto[which(conflict$adm_1 != conflict$depto & !is.na(conflict$adm_2))] <- conflict$adm_1[which(conflict$adm_1 != conflict$depto & !is.na(conflict$adm_2))]
conflict %>% filter(adm_2 != municipio) %>% print(n=58) # the coordinates seems to be closer to the reverse_geocode results, so the original locations are substituted
# conflict$depto[which(conflict$adm_2 != conflict$municipio)] <- conflict$adm_1[which(conflict$adm_2 != conflict$municipio)]
# conflict$municipio[which(conflict$adm_2 != conflict$municipio)] <- conflict$adm_2[which(conflict$adm_2 != conflict$municipio)]

conflict <- left_join(conflict %>% select(-id), municipios_capital %>% select(id, municipio, depto), by=c("municipio", "depto"))
conflict %>% filter(is.na(id))
# write.csv(conflict, "Colombia Data/Conflict events.csv", row.names = F)

conflict <- read.csv("Colombia Data/Conflict events.csv") %>% as_tibble
conflict$FARC <- grepl("FARC", conflict$dyad_name) %>% as.numeric
conflict$ELN <- grepl("ELN", conflict$dyad_name) %>% as.numeric
conflict$EGC <- grepl("EGC", conflict$dyad_name) %>% as.numeric

for (year_ in c(2013, 2014, 2016, 2017)) {
  conflict_year <- conflict %>% filter(year == year_)
  conflict_year_map <- left_join(map_df, conflict_year %>% select(id:EGC), by="id")
  
  FARC_map_year <- conflict_year_map %>% 
    ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(FARC)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("0"="white", "1"="red"), na.value = "white") +
    labs(fill="Conflicts - FARC", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  ELN_map_year <- conflict_year_map %>% 
    ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(ELN)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("0"="white", "1"="red"), na.value = "white") +
    labs(fill="Conflicts - ELN", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  EGC_map_year <- conflict_year_map %>% 
    ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(EGC)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("0"="white", "1"="red"), na.value = "white") +
    labs(fill="Conflicts - EGC", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/conflict maps/Conflicts involved with FARC %i.png", year_),
         FARC_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/conflict maps/Conflicts involved with ELN %i.png", year_),
         ELN_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/conflict maps/Conflicts involved with EGC %i.png", year_),
         EGC_map_year, scale=1)
}

# violence <- read.csv("Colombia Data/Violent events.csv") %>% as_tibble
# violence$municipio <- violence_locations$Subregion
# violence$depto <- violence_locations$Region
violence$municipio <- str_to_upper(violence$municipio)
violence$municipio <- gsub("TUMACO", "SAN ANDRES DE TUMACO", violence$municipio)
violence$municipio <- gsub("VISTA HERMOSA", "VISTAHERMOSA", violence$municipio)
violence$municipio <- gsub("MONTANITA", "LA MONTANITA", violence$municipio)
violence$municipio <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", violence$municipio)
violence$depto <- gsub("Distrito Capital", "Bogota", violence$depto)

violence <- left_join(violence, municipios_capital %>% select(id, municipio, depto), by=c("municipio", "depto"))
violence %>% filter(is.na(id))
violence %>% filter(is.na(lat))
violence %>% filter(is.na(id) & !is.na(lat))
violence_i_coords <- tibble(lat = 23.1, long = -82.4)
reverse_geocode(violence_i_coords, lat = lat, long = long, method = "arcgis", full_results = T)
# write.csv(violence, "Colombia Data/Violent events.csv", row.names = F)

for (year_ in c(2013, 2014, 2016, 2017)) {
  violence_year <- violence %>% filter(year == year_)
  
  FARC_map_year <- empty_map +
    geom_point(data=violence_year %>% filter(FARC == "yes" & lat < 13),
               aes(x=long, y=lat),
               color = "red", size=0.7) +
    labs(title=sprintf("Violence - FARC (%i)", year_), x="", y="")
  
  ELN_map_year <- empty_map +
    geom_point(data=violence_year %>% filter(ELN == "yes" & lat < 13),
               aes(x=long, y=lat),
               color = "red", size=0.7) +
    labs(title=sprintf("Violence - ELN (%i)", year_), x="", y="")
  
  AUC_map_year <- empty_map +
    geom_point(data=violence_year %>% filter(AUC == "yes" & lat < 13),
               aes(x=long, y=lat),
               color = "red", size=0.7) +
    labs(title=sprintf("Violence - AUC (%i)", year_), x="", y="")
  
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with FARC %i.png", year_),
         FARC_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with ELN %i.png", year_),
         ELN_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with AUC %i.png", year_),
         AUC_map_year, scale=1)
}
