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
# violence_types <- tibble(ViolenceType = violence$ViolenceType %>% unique %>% sort)
# violence_types$type_id <- 1:nrow(violence_types)
# write.csv(violence_types, "Colombia Data/Violence types.csv", row.names = F)
# violence <- left_join(violence, violence_types, by = "ViolenceType")

violence$municipio_new <- gsub("TUMACO", "SAN ANDRES DE TUMACO", violence$municipio_new)
violence$municipio_new <- gsub("VISTA HERMOSA", "VISTAHERMOSA", violence$municipio_new)
violence$municipio_new <- gsub("MONTANITA", "LA MONTANITA", violence$municipio_new)
violence$municipio_new <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", violence$municipio_new)
violence$municipio_new <- gsub(", D.C.", "", violence$municipio_new)
violence$municipio_new <- gsub(" D.C.", "", violence$municipio_new)
violence$depto_new <- gsub("Distrito Capital", "Bogota", violence$depto_new)
violence$municipio_new <- gsub(" D.C.", "", violence$municipio_new)
violence$depto_new <- gsub("Distrito Capital", "Bogota", violence$depto_new)

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
    labs(fill="Violence - left-wing", x="", y="") +
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
    labs(fill="Violence - right-wing", x="", y="") +
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
    labs(fill="Violence - right-wing", x="", y="") +
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

violence_RDS$Front %>% unique
violence_RDS$Bloque %>% unique


violence_RDS %>% select(ViolenceType, NumVictims, MonthYear, Guerrilla:ELN, Front, Bloque, LocationFinal, lat, long, articleText)

violence_RDS %>% filter(Front != -1 | Bloque != -1)
violence_RDS %>% filter(Front == -1 & Bloque == -1 & if_any(FARC:ELN, ~ . == "yes")) %>% select(ViolenceType, NumVictims, MonthYear, FARC, AUC, ELN, Front, Bloque, LocationFinal, lat, long)
violence_RDS %>% filter(Front == -1 & Bloque == -1 & if_any(FARC:ELN, ~ . == "yes")) %>% select(WordsSpanish, Front, Bloque, articleText)

violence_RDS %>% filter(Front == -1 & if_any(c(FARC, ELN), ~ . == "yes")) %>% select(ViolenceType, NumVictims, MonthYear, FARC, ELN, Front, LocationFinal, lat, long, articleText) %>% 
  write_xlsx("Colombia Data/violence left wing neg Front.xlsx")
violence_RDS %>% filter(Bloque == -1 & (AUC == "yes")) %>% select(ViolenceType, NumVictims, MonthYear, AUC, Bloque, LocationFinal, lat, long, articleText) %>% 
  write_xlsx("Colombia Data/violence right wing neg Bloque.xlsx")

violence_RDS %>% filter(Guerrilla == "yes") %>% select(ViolenceType, NumVictims, MonthYear, Guerrilla, LocationFinal, lat, long, articleText) %>% head(10) %>% 
  write_xlsx("Colombia Data/violence Guerrilla.xlsx")

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
# violence_coords <- tibble()
# violence <- violence_RDS
# start.t <- Sys.time()
# for (i in 1:nrow(violence)) {
#   address_i <- violence$LocationFinal[i]
#   if (is.na(address_i)) {
#     violence_coords <- bind_rows(violence_coords, violence_coords[i-1,])
#     violence_coords[i,] <- NA
#     next
#   }
#   violence_i_coords <- geo(address=address_i, method = "arcgis", unique_only = T, quiet = TRUE)
#   # violence_i_rev_geocode <- reverse_geocode(violence_i_coords, latviolence_i_coords = lat, long = long, method = "arcgis", full_results = T)
#   violence_coords <- bind_rows(violence_coords, violence_i_coords %>% rename(lat_new = lat, long_new = long))
# 
#   if (i %% 1000 == 0) print(paste0(i, "th row complete"))
# }
# end.t <- Sys.time()
# end.t - start.t # 43.63414 mins
# violence_coords[,-1]
# violence <- bind_cols(violence, violence_coords[,-1])
# write.csv(violence, "Colombia Data/Violent events.csv", row.names = F)

# new muni, depto using lat_new and long_new
violence_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(violence)) {
  latitude_i <- violence$lat_new[i]
  longitude_i <- violence$long_new[i]
  if (is.na(latitude_i) | is.na(longitude_i)) {
    violence_locations <- bind_rows(violence_locations, violence_locations[i-1,])
    violence_locations[i,] <- NA
    next
  }
  violence_i_coords <- tibble(lat = latitude_i, long = longitude_i)
  violence_i_rev_geocode <- reverse_geocode(violence_i_coords, lat = lat, long = long, method = "arcgis", full_results = T, quiet = T)
  violence_locations <- bind_rows(violence_locations, violence_i_rev_geocode)
  
  if (i %% 1000 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 54.41113 mins mins

violence$municipio_new <- stri_trans_general(violence_locations$Subregion, "Latin-ASCII")
violence$depto_new <- stri_trans_general(violence_locations$Region, "Latin-ASCII")
# write.csv(violence, "Colombia Data/Violent events.csv", row.names = F)