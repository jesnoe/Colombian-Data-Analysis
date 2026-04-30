# setwd("/Users/R")
# setwd("C:/Users/User/Documents/R")
library(readxl)
library(writexl)
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
  # conflict_RDS <- readRDS("Colombia Data/GEDEvent_v25_1.rds") %>% filter(country == "Colombia" & year %in% 2013:2017) %>% filter(year != 2015) %>% select(id, year, dyad_name, adm_1, adm_2, latitude, longitude) %>% 
  #   rename(lat = latitude, long = longitude)
  # conflict_RDS$adm_1 <- gsub(" district", "", conflict_RDS$adm_1)
  # conflict_RDS$adm_1 <- gsub(" department", "", conflict_RDS$adm_1) %>% stri_trans_general("Latin-ASCII")
  # conflict_RDS$adm_2 <- gsub(" municipality", "", conflict_RDS$adm_2) %>% stri_trans_general("Latin-ASCII")
  
  violence_RDS <- readRDS("Colombia Data/Violent events.rds") %>% as_tibble %>% 
    rename(lat = gMapLat, long = gMapLon) %>% 
    mutate(MonthYear = as.Date(paste(1, str_to_title(MonthYear)), "%d %B %Y"),
           month = month(MonthYear),
           year = year(MonthYear),
           Front = stri_unescape_unicode(Front),
           Bloque = stri_unescape_unicode(Bloque),
           articleText = stri_encode(
             stri_conv(articleText, from = "UTF-8", to = "LATIN1"),
             from = "CP1252",
             to = "UTF-8"
           )) %>% 
    filter(year %in% 2013:2017) %>% 
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

violence <- read.csv("Colombia Data/Violent events.csv") %>% as_tibble

violence$municipio_new <- gsub("TUMACO", "SAN ANDRES DE TUMACO", violence$municipio_new)
violence$municipio_new <- gsub("VISTA HERMOSA", "VISTAHERMOSA", violence$municipio_new)
violence$municipio_new <- gsub("MONTANITA", "LA MONTANITA", violence$municipio_new)
violence$municipio_new <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", violence$municipio_new)
violence$municipio_new <- gsub("ARMERO", "ARMERO GUAYABAL", violence$municipio_new)
violence$municipio_new <- gsub("CARMEN DE VIBORAL", "EL CARMEN DE VIBORAL", violence$municipio_new)
violence$municipio_new <- gsub("DON MATIAS", "DONMATIAS", violence$municipio_new)
violence$municipio_new <- gsub(", D.C.", "", violence$municipio_new)
violence$municipio_new <- gsub(" D.C.", "", violence$municipio_new)
violence$depto_new <- gsub("Distrito Capital", "Bogota", violence$depto_new)
violence$municipio_new[which(violence$municipio_new == "CARTAGENA" & violence$depto_new == "Bolivar")] <- "CARTAGENA DE INDIAS"
violence$municipio_new[which(violence$municipio_new == "EL CARMEN" & violence$depto_new == "Santander")] <- "EL CARMEN DE CHUCURI"
violence$municipio_new[which(violence$municipio_new == "EL TABLON" & violence$depto_new == "Narino")] <- "EL TABLON DE GOMEZ"
violence$municipio_new[which(violence$municipio_new == "GUICAN" & violence$depto_new == "Boyaca")] <- "GUICAN DE LA SIERRA"
violence$municipio_new[which(violence$municipio_new == "HATO NUEVO" & violence$depto_new == "La Guajira")] <- "HATONUEVO"
violence$municipio_new[which(violence$municipio_new == "LOPEZ" & violence$depto_new == "Cauca")] <- "LOPEZ DE MICAY"
violence$municipio_new[which(violence$municipio_new == "MARIQUITA" & violence$depto_new == "Tolima")] <- "SAN SEBASTIAN DE MARIQUITA"
violence$municipio_new[which(violence$municipio_new == "PACOA" & violence$depto_new == "Amazonas")] <- "LA VICTORIA"
violence$municipio_new[which(violence$municipio_new == "PURISIMA" & violence$depto_new == "Cordoba")] <- "PURISIMA DE LA CONCEPCION"
violence$municipio_new[which(violence$municipio_new == "SAN ANDRES" & violence$depto_new == "Antioquia")] <- "SAN ANDRES DE CUERQUIA"
violence$municipio_new[which(violence$municipio_new == "SAN VICENTE" & violence$depto_new == "Antioquia")] <- "SAN VICENTE FERRER"
violence$municipio_new[which(violence$municipio_new == "SANTA CRUZ" & violence$depto_new == "Narino")] <- "SANTACRUZ"
violence$municipio_new[which(violence$municipio_new == "SANTANDER" & violence$depto_new == "Amazonas")] <- "PUERTO SANTANDER"
violence$municipio_new[which(violence$municipio_new == "SANTUARIO" & violence$depto_new == "Antioquia")] <- "EL SANTUARIO"
violence$municipio_new[which(violence$municipio_new == "SINCE" & violence$depto_new == "Sucre")] <- "SAN LUIS DE SINCE"
violence$municipio_new[which(violence$municipio_new == "TOLU" & violence$depto_new == "Sucre")] <- "TOLU VIEJO"

# violence %>% filter(!(municipio_new %in% municipios_capital$municipio)) %>% select(LocationFinal, municipio_new, depto_new, lat, long)

violence_with_id <- violence %>%
  select(year, Guerrilla:ELN, Front, Bloque, LocationFinal, municipio_new, depto_new) %>% 

  rename(municipio = municipio_new, depto = depto_new) %>%
  mutate(municipio = str_to_upper(municipio)) %>% 
  left_join(municipios_capital %>% select(id, municipio, depto), by=c("municipio", "depto"))
violence_with_id %>% filter(is.na(id)) %>% select(LocationFinal, municipio, depto) %>% unique %>% arrange(municipio) %>% filter(municipio != "")
# write.csv(violence_with_id, "Colombia Data/violence with id (all).csv", row.names = F)

violence_all <- read.csv("Colombia Data/violence with id (all).csv") %>% as_tibble %>% filter(!is.na(id))
violence_combined <- violence_all %>%
  mutate(Guerrilla = ifelse(Guerrilla == "yes", 1 , 0),
         FARC = ifelse(FARC == "yes", 1 , 0),
         ELN = ifelse(ELN == "yes", 1 , 0),
         AUC = ifelse(AUC == "yes", 1 , 0),
         Front = ifelse(Front != -1, 1 , 0),
         Bloque = ifelse(Bloque != -1, 1 , 0)) %>% 
  group_by(id, year) %>% # removed year under the assumption that paramilitary and guerrilla groups do not relocate that much
  summarize(Guerrilla = ifelse(any(Guerrilla == 1), 1, 0),
            FARC = ifelse(any(FARC == 1), 1, 0),
            ELN = ifelse(any(ELN == 1), 1, 0),
            AUC = ifelse(any(AUC == 1), 1, 0),
            Front = ifelse(any(Front == 1), 1, 0),
            Bloque = ifelse(any(Bloque == 1), 1, 0)) %>% ungroup %>% 
  mutate(left_wing = if_any(c(Guerrilla:ELN, Front), ~ . == 1) %>% as.numeric,
         right_wing = if_any(c(AUC, Bloque), ~ . == 1) %>%  as.numeric)

for (year_ in 2013:2017) { # area maps
  violence_year <- violence_combined %>% filter(year == year_) %>% 
    mutate(both = ifelse(left_wing == 1, "left-wing",
                         ifelse(right_wing == 1, "right-wing", "X"))) %>% 
    mutate(both = ifelse(left_wing == 1 & right_wing == 1, "both", both))
  violence_year_map <- left_join(map_df, violence_year %>% select(id, left_wing, right_wing, both), by="id")
  
  left_wing_map_year <- violence_year_map %>% ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(left_wing)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("0"="white", "1"="red"), na.value = "white") +
    labs(fill="Violence: left-wing", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  right_wing_map_year <- violence_year_map %>% ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(right_wing)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("0"="white", "1"="red"), na.value = "white") +
    labs(fill="Violence: right-wing", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  both_map_year <- violence_year_map %>% ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(both)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("X"="white", "left-wing"="red", "right-wing"="blue", "both"="violet"), na.value = "white") +
    labs(fill="Violence: left- and right-wing", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/left right/violence involved with left-wing %i.png", year_),
         left_wing_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/left right/violence involved with right-wing %i.png", year_),
         right_wing_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/left right/violence involved with both %i.png", year_),
         both_map_year, scale=1)
}

violence_all_years <- violence_combined %>% group_by(id) %>%
  summarize(left_wing = ifelse(sum(left_wing), "yes", "no"), right_wing = ifelse(sum(right_wing), "yes", "no")) %>% ungroup %>% 
  mutate(armed_group = ifelse(left_wing == "yes", "left-wing",
                       ifelse(right_wing == "yes", "right-wing", "X"))) %>% 
  mutate(armed_group = ifelse(left_wing == "yes" & right_wing == "yes", "both", armed_group))

both_map_all_year <- left_join(map_df, violence_all_years %>% select(id, armed_group), by="id") %>% ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(armed_group)),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = map_df$long, y = map_df$lat) +
  coord_quickmap() +
  scale_fill_manual(values = c("X"="white", "left-wing"="red", "right-wing"="blue", "both"="violet"), na.value = "white") +
  labs(fill="Violence: left, right", x="", y="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())

ggsave("Colombia Data/local GWR PML result predicted prices/violence maps/left right/violence involved with both 2013-2017.png", both_map_all_year, scale=1)

violence_RDS$Front %>% unique
violence_RDS$Bloque %>% unique


violence_RDS %>% select(ViolenceType, NumVictims, MonthYear, Guerrilla:ELN, Front, Bloque, LocationFinal, lat, long, articleText)

violence_RDS %>% filter(Front != -1 | Bloque != -1)
violence_RDS %>% filter(Front == -1 & Bloque == -1 & if_any(FARC:ELN, ~ . == "yes")) %>% select(ViolenceType, NumVictims, MonthYear, FARC, AUC, ELN, Front, Bloque, LocationFinal, lat, long)
violence_RDS %>% filter(Front == -1 & Bloque == -1 & if_any(FARC:ELN, ~ . == "yes")) %>% select(WordsSpanish, Front, Bloque, articleText)

# violence_RDS %>% filter(Front == -1 & if_any(c(FARC, ELN), ~ . == "yes")) %>% select(ViolenceType, NumVictims, MonthYear, FARC, ELN, Front, LocationFinal, lat, long, articleText) %>% 
#   write_xlsx("Colombia Data/violence left wing neg Front.xlsx")
# violence_RDS %>% filter(Bloque == -1 & (AUC == "yes")) %>% select(ViolenceType, NumVictims, MonthYear, AUC, Bloque, LocationFinal, lat, long, articleText) %>% 
#   write_xlsx("Colombia Data/violence right wing neg Bloque.xlsx")
# 
# violence_RDS %>% filter(Guerrilla == "yes") %>% select(ViolenceType, NumVictims, MonthYear, Guerrilla, LocationFinal, lat, long, articleText) %>% head(10) %>% 
#   write_xlsx("Colombia Data/violence Guerrilla.xlsx")

violence_RDS %>% filter(FARC == "yes" | ELN == "yes") # 3305
violence_RDS %>% filter(Guerrilla == "yes"| FARC == "yes" | ELN == "yes") # 3851
violence_RDS %>% filter(Front != -1) # 1454
violence_RDS %>% filter(Guerrilla == "yes"| FARC == "yes" | ELN == "yes" | Front != -1) # 3925 (+ 74 from FARC and ELN)

violence_RDS %>% filter(AUC == "yes") # 251
violence_RDS %>% filter(Bloque != -1) # 1035
violence_RDS %>% filter(AUC == "yes" | Bloque != -1) # 1130  (+ 879 from Guerrilla and AUC)

violence_RDS %>% filter(Guerrilla == "yes" | AUC == "yes") # 3872
violence_RDS %>% filter(Guerrilla == "yes" | AUC == "yes" | Bloque != -1) # 4345  (+ 473 from Guerrilla and AUC)

# new coordinates on LocationFinal
lat_min <- min(map_df$lat)
lat_max <- max(map_df$lat)
long_min <- min(map_df$long)
long_max <- max(map_df$long)
violence_coords <- tibble()
violence_locations <- tibble()
violence <- violence_RDS
start.t <- Sys.time()
for (i in 1:nrow(violence)) {
  address_i <- violence$LocationFinal[i]
  lat_i <- violence$lat[i]
  long_i <- violence$long[i]
  if (is.na(address_i)) {
    violence_locations <- bind_rows(violence_locations, violence_i_rev_geocode)
    violence_locations[i,] <- NA
    next
  }
  
  if (!is.na(lat_i) & !is.na(long_i)) in_Colombia <- ifelse(lat_i >= lat_min & lat_i <= lat_max & long_i >= long_min & long_i <= long_max, T, F)
  
  if (any(is.na(c(lat_i, long_i))) | !in_Colombia) {
    violence_i_coords <- geo(address=address_i, method = "arcgis", unique_only = T, quiet = TRUE)
    
    lat_new <- violence_i_coords$lat
    long_new <- violence_i_coords$long
    if (is.na(lat_new) | is.na(long_new)) {
      violence_locations <- bind_rows(violence_locations, violence_i_rev_geocode)
      violence_locations[i,] <- NA
      next
    }
    if (!(lat_new >= lat_min & lat_new <= lat_max & long_new >= long_min & long_new <= long_max)) {
      violence_locations <- bind_rows(violence_locations, violence_i_rev_geocode)
      violence_locations[i,] <- NA
      next
    }
  }else{
    violence_i_coords <- tibble(address=address_i, lat = lat_i, long = long_i)
  }
  
  violence_i_rev_geocode <- reverse_geocode(violence_i_coords, lat = lat, long = long, address = "address_new", method = "arcgis", full_results = T, quiet = T)
  violence_locations <- bind_rows(violence_locations, violence_i_rev_geocode)

  if (i %% 1000 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 1.222069 hours
violence$municipio_new <- stri_trans_general(violence_locations$Subregion, "Latin-ASCII") %>% str_to_upper
violence$depto_new <- stri_trans_general(violence_locations$Region, "Latin-ASCII")

no_Colombia <- which(violence_locations$CntryName != "Colombia")
violence_locations[no_Colombia,] %>% select(Subregion, Region, CntryName)
#### check strange locations
violence_locations[which(!(violence$municipio_new %in% municipios_capital$municipio)),] %>% select(address, Subregion, Region, CntryName)

# write.csv(violence[-no_Colombia,], "Colombia Data/Violent events.csv", row.names = F)