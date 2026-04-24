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
  # conflict_RDS <- readRDS("Colombia Data/GEDEvent_v25_1.rds") %>% filter(country == "Colombia" & year %in% 2013:2017) %>% filter(year != 2015) %>% select(id, year, dyad_name, adm_1, adm_2, latitude, longitude) %>% 
  #   rename(lat = latitude, long = longitude)
  # conflict_RDS$adm_1 <- gsub(" district", "", conflict_RDS$adm_1)
  # conflict_RDS$adm_1 <- gsub(" department", "", conflict_RDS$adm_1) %>% stri_trans_general("Latin-ASCII")
  # conflict_RDS$adm_2 <- gsub(" municipality", "", conflict_RDS$adm_2) %>% stri_trans_general("Latin-ASCII")
  
  violence_RDS <- readRDS("Colombia Data/Violent events.rds") %>% as_tibble %>% 
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
  
  conflict <- read.csv("Colombia Data/Conflict events.csv") %>% as_tibble
}
# conflict_RDS
violence_RDS

violence <- read.csv("Colombia Data/Violent events.csv") %>% as_tibble
# violence_types <- tibble(ViolenceType = violence$ViolenceType %>% unique %>% sort)
# violence_types$type_id <- 1:nrow(violence_types)
# write.csv(violence_types, "Colombia Data/Violence types.csv", row.names = F)
# violence <- left_join(violence, violence_types, by = "ViolenceType")
violence %>% filter(grepl("mine", ViolenceType)) %>% pull(ViolenceType) %>% unique %>% sort

violence$municipio_new <- gsub("TUMACO", "SAN ANDRES DE TUMACO", violence$municipio_new)
violence$municipio_new <- gsub("VISTA HERMOSA", "VISTAHERMOSA", violence$municipio_new)
violence$municipio_new <- gsub("MONTANITA", "LA MONTANITA", violence$municipio_new)
violence$municipio_new <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", violence$municipio_new)
violence$municipio_new <- gsub(", D.C.", "", violence$municipio_new)
violence$municipio_new <- gsub(" D.C.", "", violence$municipio_new)
violence$depto_new <- gsub("Distrito Capital", "Bogota", violence$depto_new)

violence_with_id <- violence %>%
  select(year, Guerrilla:ELN, Front, Bloque, LocationFinal, municipio_new:etc.) %>% 
  rename(municipio = municipio_new, depto = depto_new) %>%
  mutate(municipio = str_to_upper(municipio)) %>% 
  left_join(municipios_capital %>% select(id, municipio, depto), by=c("municipio", "depto"))
violence_with_id %>% filter(is.na(id)) %>% select(LocationFinal, municipio, depto) %>% unique %>% arrange(municipio) %>% filter(municipio != "")
violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes"))
violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes") & FARC == "yes")
violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes") & ELN == "yes")
# write.csv(violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes")), "Colombia Data/violence with id (AAMM).csv", row.names = F)


violence_collapsed <- violence_collapsed %>% mutate(assassination = ifelse(grepl("murder", ViolenceType), "yes", assassination),)
violence_collapsed <- violence_collapsed %>% mutate(assassination = ifelse(grepl("asesinato", ViolenceType), "yes", assassination),)
violence_collapsed <- violence_collapsed %>% mutate(assassination = ifelse(grepl("homicid", ViolenceType), "yes", assassination),)
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("shooting", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("ambush", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("emboscada", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("combat", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("armed", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("arson", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("assault", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("atentado", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("bomb", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("explosi", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("fire", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("gunfire", ViolenceType), "yes", attack))
violence_collapsed <- violence_collapsed %>% mutate(attack = ifelse(grepl("terror", ViolenceType), "yes", attack))

violence_collapsed <- violence_collapsed %>% mutate(etc. = ifelse(massacre == "yes" | assassination == "yes" | attack == "yes" | mine == "yes" | kidnap == "yes", "no", etc.))
# ceasefire?
violence_collapsed %>% filter(if_all(massacre:kidnap, function(x) x == "no")) %>% pull(ViolenceType) %>% unique %>% sort

# violence_collapsed$municipio_new <- gsub("TUMACO", "SAN ANDRES DE TUMACO", violence_collapsed$municipio_new)
# violence_collapsed$municipio_new <- gsub("VISTA HERMOSA", "VISTAHERMOSA", violence_collapsed$municipio_new)
# violence_collapsed$municipio_new <- gsub("MONTANITA", "LA MONTANITA", violence_collapsed$municipio_new)
# violence_collapsed$municipio_new <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", violence_collapsed$municipio_new)
# violence_collapsed$municipio_new <- gsub(", D.C.", "", violence_collapsed$municipio_new)
# violence_collapsed$municipio_new <- gsub(" D.C.", "", violence_collapsed$municipio_new)
# violence_collapsed$depto_new <- gsub("Distrito Capital", "Bogota", violence_collapsed$depto_new)

violence_with_id <- violence_collapsed %>%
  select(year, FARC:ELN, LocationFinal, municipio_new:etc.) %>% 
  rename(municipio = municipio_new, depto = depto_new) %>%
  mutate(municipio = str_to_upper(municipio)) %>% 
  left_join(municipios_capital %>% select(id, municipio, depto), by=c("municipio", "depto"))
violence_with_id %>% filter(is.na(id)) %>% select(LocationFinal, municipio, depto) %>% unique %>% arrange(municipio) %>% filter(municipio != "")
violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes"))
violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes") & FARC == "yes")
violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes") & ELN == "yes")
# write.csv(violence_with_id %>% filter(if_any(massacre:mine, ~ .x == "yes")), "Colombia Data/violence with id (AAMM).csv", row.names = F)
# write.csv(violence_with_id %>% filter(etc. == "yes"), "Colombia Data/violence with id (etc).csv", row.names = F)
# write.csv(violence_with_id, "Colombia Data/FARC violence with id (all).csv", row.names = F)


regression_data_CF_2013 <- read.csv("Colombia Data/regression data all municipios CF 2013.csv") %>% as_tibble
regression_data_CF_2014 <- read.csv("Colombia Data/regression data all municipios CF 2014.csv") %>% as_tibble
regression_data_CF_2016 <- read.csv("Colombia Data/regression data all municipios CF 2016.csv") %>% as_tibble
regression_data_CF_2017 <- read.csv("Colombia Data/regression data all municipios CF 2017.csv") %>% as_tibble

violence_map_collapsed <- function(violence_tbl, violence_type, reg_data) {
  violence_tbl <- violence_tbl[violence_tbl[[violence_type]] == "yes",]
  violence_annual <- violence_tbl %>% filter(!is.na(id)) %>% 
    group_by(id, year) %>% 
    summarize(FARC = ifelse("yes" %in% FARC, "yes", "no"),
              AUC = ifelse("yes" %in% AUC, "yes", "no"),
              ELN = ifelse("yes" %in% ELN, "yes", "no"))
  
  FARC_tbl <- tibble()
  ELN_tbl <- tibble()
  y_tbl <- tibble()
  for (year_ in c(2013, 2014, 2016, 2017)) { # area maps
    violence_year <- violence_annual %>% filter(year == year_) %>% select(-year)
    reg_data_year <- left_join(reg_data[[paste0("y", year_)]], violence_year, by = "id") %>% 
      mutate(FARC = ifelse(is.na(FARC), "no", FARC),
             AUC = ifelse(is.na(AUC), "no", AUC),
             ELN = ifelse(is.na(ELN), "no", ELN))
    
    y_tbl_year <- reg_data_year %>% select(base_source:hyd_destination)%>% apply(2, function(x) sum(x))
    FARC_tbl_year <- tibble(n_FARC = sum(reg_data_year$FARC == "yes"),
                            FARC_base_source = nrow(reg_data_year %>% filter(base_source == 1 & FARC == "yes")),
                            FARC_base_destination = nrow(reg_data_year %>% filter(base_destination == 1 & FARC == "yes")),
                            FARC_hyd_source = nrow(reg_data_year %>% filter(hyd_source == 1 & FARC == "yes")),
                            FARC_hyd_destination = nrow(reg_data_year %>% filter(hyd_destination == 1 & FARC == "yes")))
    ELN_tbl_year <- tibble(n_ELN = sum(reg_data_year$ELN == "yes"),
                           ELN_base_source = nrow(reg_data_year %>% filter(base_source == 1 & ELN == "yes")),
                           ELN_base_destination = nrow(reg_data_year %>% filter(base_destination == 1 & ELN == "yes")),
                           ELN_hyd_source = nrow(reg_data_year %>% filter(hyd_source == 1 & ELN == "yes")),
                           ELN_hyd_destination = nrow(reg_data_year %>% filter(hyd_destination == 1 & ELN == "yes")))
    
    y_tbl <- bind_rows(y_tbl, y_tbl_year)
    FARC_tbl <- bind_rows(FARC_tbl, FARC_tbl_year)
    ELN_tbl <- bind_rows(ELN_tbl, ELN_tbl_year) 
  }
  y_tbl$year <- c(2013, 2014, 2016, 2017)
  FARC_tbl$year <- c(2013, 2014, 2016, 2017)
  ELN_tbl$year <- c(2013, 2014, 2016, 2017)
  return(list(y = y_tbl, FARC = FARC_tbl, ELN = ELN_tbl))
}

reg_data_list <- list(y2013 = regression_data_CF_2013,
                      y2014 = regression_data_CF_2014,
                      y2016 = regression_data_CF_2016,
                      y2017 = regression_data_CF_2017)

violence_map_collapsed(violence_with_id, "massacre", reg_data_list)
violence_map_collapsed(violence_with_id, "assassination", reg_data_list)
violence_map_collapsed(violence_with_id, "attack", reg_data_list)
violence_map_collapsed(violence_with_id, "mine", reg_data_list)
violence_map_collapsed(violence_with_id, "kidnap", reg_data_list)
violence_map_collapsed(violence_with_id, "etc.", reg_data_list)

violence_map_collapsed <- function(violence_tbl, violence_type, violence_color) {
  violence_tbl <- violence_tbl[violence_tbl[[violence_type]] == "yes",]
  violence_annual <- violence_tbl %>% filter(!is.na(id)) %>% 
    group_by(id, year) %>% 
    summarize(FARC = ifelse("yes" %in% FARC, "yes", "no"),
              AUC = ifelse("yes" %in% AUC, "yes", "no"),
              ELN = ifelse("yes" %in% ELN, "yes", "no"))
  
  for (year_ in c(2013, 2014, 2016, 2017)) { # area maps
    violence_year <- violence_annual %>% filter(year == year_)
    
    violence_year_map <- left_join(map_df, violence_year %>% select(id, FARC:ELN), by="id") %>% 
      mutate(across(FARC:ELN, function(x) ifelse(is.na(x), "no", x)))
    
    FARC_map_year <- violence_year_map %>% ggplot() +
      geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(FARC)),
                   color = "black",
                   linewidth = 0.1) +
      expand_limits(x = map_df$long, y = map_df$lat) +
      coord_quickmap() +
      scale_fill_manual(values = c("no"="white", "yes"=violence_color), na.value = "white") +
      labs(fill=sprintf("%s - FARC", violence_type), x="", y="", title = year_) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank())
    
    ELN_map_year <- violence_year_map %>% ggplot() +
      geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(ELN)),
                   color = "black",
                   linewidth = 0.1) +
      expand_limits(x = map_df$long, y = map_df$lat) +
      coord_quickmap() +
      scale_fill_manual(values = c("no"="white", "yes"=violence_color), na.value = "white") +
      labs(fill=sprintf("%s - ELN", violence_type), x="", y="", title = year_) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank())
    
    AUC_map_year <- violence_year_map %>% ggplot() +
      geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(AUC)),
                   color = "black",
                   linewidth = 0.1) +
      expand_limits(x = map_df$long, y = map_df$lat) +
      coord_quickmap() +
      scale_fill_manual(values = c("no"="white", "yes"=violence_color), na.value = "white") +
      labs(fill=sprintf("%s - AUC", violence_type), x="", y="", title = year_) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank())
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps %s/violence involved with FARC %i.png", violence_type, year_),
           FARC_map_year, scale=1)
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps %s/violence involved with ELN %i.png", violence_type, year_),
           ELN_map_year, scale=1)
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps %s/violence involved with AUC %i.png", violence_type, year_),
           AUC_map_year, scale=1)
  }
}

violence_map_collapsed(violence_with_id, "massacre", "#fc8d59")
violence_map_collapsed(violence_with_id, "assassination", "#5e4fa2")
violence_map_collapsed(violence_with_id, "attack", "#9e0142")
violence_map_collapsed(violence_with_id, "mine", "#006837")
violence_map_collapsed(violence_with_id, "kidnap", "#3288bd")
violence_map_collapsed(violence_with_id, "etc.", "#543005")

### create conflict.csv and violence.csv
conflict_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(conflict_RDS)) {
  latitude_i <- conflict_RDS$lat[i]
  longitude_i <- conflict_RDS$long[i]
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
for (i in 1:nrow(violence_RDS)) {
  latitude_i <- violence_RDS$lat[i]
  longitude_i <- violence_RDS$long[i]
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

violence <- read.csv("Colombia Data/Violent events.csv") %>% as_tibble

# for (year_ in c(2013, 2014, 2016, 2017)) { # point maps
#   violence_year <- violence %>% filter(year == year_)
#   
#   FARC_map_year <- empty_map +
#     geom_point(data=violence_year %>% filter(FARC == "yes" & lat < 13),
#                aes(x=long, y=lat),
#                color = "red", size=0.7) +
#     labs(title=sprintf("Violence - FARC (%i)", year_), x="", y="")
#   
#   ELN_map_year <- empty_map +
#     geom_point(data=violence_year %>% filter(ELN == "yes" & lat < 13),
#                aes(x=long, y=lat),
#                color = "red", size=0.7) +
#     labs(title=sprintf("Violence - ELN (%i)", year_), x="", y="")
#   
#   AUC_map_year <- empty_map +
#     geom_point(data=violence_year %>% filter(AUC == "yes" & lat < 13),
#                aes(x=long, y=lat),
#                color = "red", size=0.7) +
#     labs(title=sprintf("Violence - AUC (%i)", year_), x="", y="")
#   
#   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with FARC %i.png", year_),
#          FARC_map_year, scale=1)
#   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with ELN %i.png", year_),
#          ELN_map_year, scale=1)
#   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with AUC %i.png", year_),
#          AUC_map_year, scale=1)
# }

grep("HABANA", municipios_capital$municipio %>% unique)
sum(!grepl(", colombia", violence$LocationFinal))

violence %>% filter(!grepl(", colombia", LocationFinal)) %>% pull(LocationFinal) %>% unique # blank ("") LocationFinal for 102 locations, everything else ends with ", colombia
violence$municipio_new <- gsub("TUMACO", "SAN ANDRES DE TUMACO", violence$municipio_new)
violence$municipio_new <- gsub("VISTA HERMOSA", "VISTAHERMOSA", violence$municipio_new)
violence$municipio_new <- gsub("MONTANITA", "LA MONTANITA", violence$municipio_new)
violence$municipio_new <- gsub("MIRITI-PARANA", "PUERTO SANTANDER", violence$municipio_new)
violence$municipio_new <- gsub(", D.C.", "", violence$municipio_new)
violence$municipio_new <- gsub("D.C.", "", violence$municipio_new)
violence$depto_new <- gsub("Distrito Capital", "Bogota", violence$depto_new)

violence_with_coords <- violence %>% filter(!is.na(lat)) %>% 
  mutate(location = gsub(", colombia", "", stri_trans_general(LocationFinal, "Latin-ASCII")) %>% str_to_upper,
         municipio_new = str_to_upper(municipio_new))
map_df %>% select(long, lat) %>% summary
violence_with_coords %>% filter(municipio != location)
(violence_name_diff_with_coords <- violence_with_coords %>% filter(municipio_new != location) %>% select(LocationFinal, municipio_new, depto_new) %>% unique %>% arrange(LocationFinal) %>% unique)
violence_with_coords %>% filter(lat > 13) %>% select(LocationFinal, lat, long, municipio_new, depto_new, lat_new, long_new) %>% unique %>% arrange(LocationFinal)
violence_with_coords %>% filter(lat_new > 13) %>% select(LocationFinal, lat, long, lat_new, long_new, municipio_new, depto_new) %>% unique %>% arrange(LocationFinal)
violence_with_coords %>% filter(round(lat, 2) != round(lat_new, 2))
violence_with_coords %>% filter(abs(lat - lat_new) > 1)

violence_without_coords <- violence %>% filter(is.na(lat)) %>% 
  mutate(location = gsub(", colombia", "", stri_trans_general(LocationFinal, "Latin-ASCII")) %>% str_to_upper,
         municipio_new = str_to_upper(municipio_new))
violence_without_coords
violence_without_coords %>% filter(municipio_new != location) %>% select(-lat, -long)
violence_without_coords %>% filter(municipio_new != location) %>% select(LocationFinal, municipio_new, depto_new, location) %>% unique %>% arrange(LocationFinal) %>% unique
(violence_name_diff_without_coords <- violence_without_coords %>% filter(municipio_new != location) %>% select(LocationFinal, municipio_new, depto_new) %>% unique %>% arrange(LocationFinal))
violence_without_coords$LocationFinal %>% unique %>% sort
violence_without_coords$location %>% unique %>% sort

violence_name_diff <- bind_rows(violence_name_diff_with_coords, violence_name_diff_without_coords) %>% unique %>% arrange(LocationFinal)
# write_excel_csv(violence_name_diff[sample(1:nrow(violence_name_diff), nrow(violence_name_diff)),], "Colombia Data/violence_name_diff.csv")
violence %>% filter(!(LocationFinal %in% violence_name_diff$LocationFinal)) # 4,958 rows (decreased from 7,621 rows)

violence_with_id <- violence %>%
  select(year, FARC:ELN, LocationFinal, municipio_new, depto_new) %>% 
  rename(municipio = municipio_new, depto = depto_new) %>%
  mutate(municipio = str_to_upper(municipio)) %>% 
  left_join(municipios_capital %>% select(id, municipio, depto), by=c("municipio", "depto"))
violence_with_id %>% filter(is.na(id)) %>% select(LocationFinal, municipio, depto) %>% unique %>% arrange(municipio) %>% filter(municipio != "")
violence_annual <- violence_with_id %>% filter(!is.na(id)) %>% 
  group_by(id, year) %>% 
  summarize(FARC = ifelse("yes" %in% FARC, "yes", "no"),
            AUC = ifelse("yes" %in% AUC, "yes", "no"),
            ELN = ifelse("yes" %in% ELN, "yes", "no"))


for (year_ in c(2013, 2014, 2016, 2017)) { # area maps
  violence_year <- violence_annual %>% filter(year == year_)
  violence_year_map <- left_join(map_df, violence_year %>% select(id, FARC:ELN), by="id") %>% 
    mutate(across(FARC:ELN, function(x) ifelse(is.na(x), "no", x)))
  
  FARC_map_year <- violence_year_map %>% ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(FARC)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("no"="white", "yes"="red"), na.value = "white") +
    labs(fill="Violence - FARC", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())

  ELN_map_year <- violence_year_map %>% ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(ELN)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("no"="white", "yes"="red"), na.value = "white") +
    labs(fill="Violence - ELN", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())

  AUC_map_year <- violence_year_map %>% ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(AUC)),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    coord_quickmap() +
    scale_fill_manual(values = c("no"="white", "yes"="red"), na.value = "white") +
    labs(fill="Violence - AUC", x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())

  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with FARC %i.png", year_),
         FARC_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with ELN %i.png", year_),
         ELN_map_year, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/violence maps/violence involved with AUC %i.png", year_),
         AUC_map_year, scale=1)
}

# new coordinates on LocationFinal
# violence_coords <- tibble()
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
# end.t - start.t # 30.24251 mins
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
end.t - start.t # 46.96939 mins

violence$municipio_new <- stri_trans_general(violence_locations$Subregion, "Latin-ASCII")
violence$depto_new <- stri_trans_general(violence_locations$Region, "Latin-ASCII")
# write.csv(violence, "Colombia Data/Violent events.csv", row.names = F)