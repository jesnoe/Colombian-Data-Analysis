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
library(caret)
library(randomForest)

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
  
  base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
  hyd_to_hyd <- read.csv("Colombia Data/Anecdotal hyd to hyd municipality only.csv") %>% as_tibble
  general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
  anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual with municipality.csv") %>% as_tibble
  
  population <- read.csv("Colombia Data/Census population by municipios (2018).csv") %>% as_tibble
  population$log_population <- log(population$population)
  
  armed_groups <- list()
  years <- (2008:2020)[-c(2,8,12)]
  for (year in years) {
    armed_groups_year <- read_xlsx(paste0("Colombia Data/Colombia-Armed groups-Paramilitar ", year ,".xlsx")) %>% 
      filter(!grepl("Archipiélago", DEPARTAMENTO)) %>% 
      rename(municipio=MUNICIPIO, depto=DEPARTAMENTO) %>% 
      mutate(municipio=str_to_upper(stri_trans_general(municipio, "Latin-ASCII"))) %>% 
      filter(municipio != "RIO ORO") %>% 
      select(-Pais, -REGION)
    
    armed_groups_year$municipio <- gsub("BELEN DE BAJIRA", "RIOSUCIO", armed_groups_year$municipio)
    RIOSUCIO_index <- which(armed_groups_year$municipio == "RIOSUCIO" & armed_groups_year$depto == "Choco")
    colsum_row <- c(as.vector(armed_groups_year[RIOSUCIO_index[2], 1:2]) %>% unlist, colSums(armed_groups_year[RIOSUCIO_index, -(1:2)], na.rm=T))
    for (i in 3:ncol(armed_groups_year)) {
      if (colsum_row[i] != "0") {
        armed_groups_year[RIOSUCIO_index[2], i] <- as.numeric(colsum_row[i])
      }
    }
    armed_groups_year <- armed_groups_year[-RIOSUCIO_index[1],]
    
    armed_groups_year$n_armed_groups <- apply(armed_groups_year, 1, function(x) sum(!is.na(x[3:ncol(armed_groups_year)])))
    armed_groups_year <- armed_groups_year %>% 
      left_join(municipios_capital %>% select(-id_depto), by=c("municipio", "depto")) %>% 
      relocate(id, municipio, depto, n_armed_groups)
    names(armed_groups_year) <- gsub("\r", " ", names(armed_groups_year), fixed=T)
    names(armed_groups_year) <- gsub("\n", "", names(armed_groups_year), fixed=T)
    names(armed_groups_year) <- gsub("/", "", names(armed_groups_year))
    
    armed_groups_year[is.na(armed_groups_year)] <- 0
    
    if ("Las Autodefensas Gaitanistas de Colombia (AGC)" %in% names(armed_groups_year) & "Clan del Golfo (Formerly Los Urabeños)" %in% names(armed_groups_year)) {
      AGC_index <- which(names(armed_groups_year) == "Las Autodefensas Gaitanistas de Colombia (AGC)")
      Golfo_index <- which(names(armed_groups_year) == "Clan del Golfo (Formerly Los Urabeños)")
      armed_groups_year <- armed_groups_year %>% 
        mutate(`Clan del Golfo (Formerly Los Urabeños)`=`Clan del Golfo (Formerly Los Urabeños)` + `Las Autodefensas Gaitanistas de Colombia (AGC)`) %>% 
        mutate(`Clan del Golfo (Formerly Los Urabeños)`=ifelse(`Clan del Golfo (Formerly Los Urabeños)` > 0, 1, 0)) %>% 
        select(-`Las Autodefensas Gaitanistas de Colombia (AGC)`)
    }
    
    if ("La Oficina" %in% names(armed_groups_year)) {
      sum_to_index <- which(names(armed_groups_year) == "La Oficina de Envigado DEL VALLE DE ABURRÁ U OVA")
      sum_from_index <- which(names(armed_groups_year) == "La Oficina")
      armed_groups_year <- armed_groups_year %>% 
        mutate(`La Oficina de Envigado DEL VALLE DE ABURRÁ U OVA`=`La Oficina de Envigado DEL VALLE DE ABURRÁ U OVA` + `La Oficina`) %>% 
        mutate(`La Oficina de Envigado DEL VALLE DE ABURRÁ U OVA`=ifelse(`La Oficina de Envigado DEL VALLE DE ABURRÁ U OVA` > 0, 1, 0)) %>% 
        select(-`La Oficina`)
    }
    
    if ("Autodefensas" %in% substr(names(armed_groups_year), 1, 12)) {
      sum_from_index <- which(substr(names(armed_groups_year), 1, 12) == "Autodefensas")
      armed_groups_year$`Autodefensas Unidas de Colombia (AUC)` <- armed_groups_year[,sum_from_index] %>% apply(1, sum)
      armed_groups_year <- armed_groups_year[, -sum_from_index] %>% 
        mutate(`Autodefensas Unidas de Colombia (AUC)`=ifelse(`Autodefensas Unidas de Colombia (AUC)` > 0, 1, 0))
    }
    
    armed_groups[[paste0("y", year)]] <- armed_groups_year
  }
  armed_groups$y2008
  
  n_armed_groups <- armed_groups$y2008 %>% select(id, n_armed_groups) %>% rename(X2008=n_armed_groups)
  for (year in years[-1]) {
    n_armed_groups <- full_join(n_armed_groups, armed_groups[[paste0("y", year)]] %>% select(id, n_armed_groups), by="id")
    names(n_armed_groups)[ncol(n_armed_groups)] <- paste0("X", year)
  }
  n_armed_groups_long <- n_armed_groups %>% 
    pivot_longer(-id, names_to="year", values_to="n_armed_groups") %>% 
    mutate(year=substr(year,2,5) %>% as.integer)
  
  cultivation <- read.csv("Colombia Data/Colombia Coca Cultivation 1999-2016 renamed (Ha).csv") %>% as_tibble
  cultivation <- cultivation %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  labs_hyd <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (COCAINE HYDROCHLORIDE).csv") %>% as_tibble
  labs_hyd <- labs_hyd %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  labs_PPI <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (PRIMARY PRODUCTION INFRASTRUCTURE).csv") %>% as_tibble
  labs_PPI <- labs_PPI %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  
  eradication_aerial <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Aerial (Ha).xlsx")
  eradication_manual <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Manual (Ha).xlsx")
  eradication_aerial <- eradication_aerial %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  eradication_manual <- eradication_manual %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  
  eradication_aerial_long <- eradication_aerial %>% 
    pivot_longer(`1994`:`2015`, names_to="year", values_to="erad_aerial") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(erad_aerial))
  
  eradication_manual_long <- eradication_manual %>% 
    pivot_longer(`1998`:`2022`, names_to="year", values_to="erad_manual") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(erad_manual))
  
  coca_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca leaves (kg).xlsx")
  base_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca paste and base (kg).xlsx")
  hyd_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Cocaine (kg).xlsx")
  coca_seizures <- coca_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  base_seizures <- base_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  hyd_seizures <- hyd_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  
  coca_seizures_long <- coca_seizures %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="coca_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(coca_seizures))
  base_seizures_long <- base_seizures %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="base_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(base_seizures))
  hyd_seizures_long <- hyd_seizures %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="hyd_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(hyd_seizures))
  
  price_2013_2015 <- read.csv("Colombia Data/Colombia Price Data 2013-2015 edited.csv") %>% as_tibble
  price_2016_2021 <- read.csv("Colombia Data/Colombia Price Data 2016-2021 edited.csv") %>% as_tibble
  # eradication_aerial <- left_join(municipios_id, eradication_aerial, by="id")
  # eradication_manual <- left_join(municipios_id, eradication_manual, by="id")
  # coca_seizures <- left_join(municipios_id, coca_seizures, by="id")
  
  river_length_muni <- read.csv("Colombia Data/rivers.csv") %>% as_tibble
  road_length_muni <- read.csv("Colombia Data/major roads without tertiary.csv") %>% as_tibble
  
  population <- read_xlsx("Colombia Data/Census population by municipios (2018).xlsx") %>% 
    filter(!is.na(municipio)) %>% 
    select(-type)
  population <- population %>% 
    mutate(id=substr(municipio, 1, 5) %>% as.numeric) %>%
    select(id, population) %>% 
    right_join(municipios_capital %>% mutate(id=as.numeric(id)), by="id") %>% 
    select(id, id_depto, municipio, depto, population)
  # write.csv(population, "Colombia Data/Census population by municipios (2018).csv")
  airports <- read.csv("Colombia Data/airports.csv") %>% as_tibble
  
  price_2016_2021 %>% 
    group_by(id) %>% 
    summarise(n_seeds=sum(!is.na(seeds)),
              n_leaves=sum(!is.na(leaves))) %>% 
    arrange(desc(n_seeds), desc(n_leaves))
  
  price <- rbind(price_2013_2015, price_2016_2021 %>% select(-seeds, -leaves))
  
  price %>% select(month, year) %>% unique %>% nrow # 86 months (except for 2013 with "Jan to Apr", "May to Aug", "Sep to Dec")
}

## price correlation
price_annual <- price %>% 
  group_by(id, year) %>% 
  summarise(paste_avg=mean(paste_avg, na.rm=T),
            paste_wholesale=mean(paste_wholesale, na.rm=T),
            paste_retail=mean(paste_retail, na.rm=T),
            base_avg=mean(base_avg, na.rm=T),
            base_wholesale=mean(base_wholesale, na.rm=T),
            base_retail=mean(base_retail, na.rm=T),
            hyd_avg=mean(hyd_avg, na.rm=T),
            hyd_wholesale=mean(hyd_wholesale, na.rm=T),
            hyd_retail=mean(hyd_retail, na.rm=T)
  ) %>% 
  mutate(paste_avg=ifelse(is.nan(paste_avg), NA, paste_avg),
         paste_wholesale=ifelse(is.nan(paste_wholesale), NA, paste_wholesale),
         paste_retail=ifelse(is.nan(paste_retail), NA, paste_retail),
         base_avg=ifelse(is.nan(base_avg), NA, base_avg),
         base_wholesale=ifelse(is.nan(base_wholesale), NA, base_wholesale),
         base_retail=ifelse(is.nan(base_retail), NA, base_retail),
         hyd_avg=ifelse(is.nan(hyd_avg), NA, hyd_avg),
         hyd_wholesale=ifelse(is.nan(hyd_wholesale), NA, hyd_wholesale),
         hyd_retail=ifelse(is.nan(hyd_retail), NA, hyd_retail),
  ) %>% 
  filter(!is.na(id))
price_annual

price_med <- price %>% 
  group_by(id, year) %>% 
  summarise(paste_med=median(paste_avg, na.rm=T),
            base_med=median(base_avg, na.rm=T),
            hyd_med=median(hyd_avg, na.rm=T),
  ) %>% 
  mutate(paste_med=ifelse(is.nan(paste_med), NA, paste_med),
         base_med=ifelse(is.nan(base_med), NA, base_med),
         hyd_med=ifelse(is.nan(hyd_med), NA, hyd_med),
  ) %>% 
  filter(!is.na(id))

## regression data
{
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id, municipio, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  # all municipio data
  municipios_capital_arrange <- municipios_capital %>% arrange(id)
  municipios_capital_reg <- tibble()
  for (i in 1:nrow(municipios_capital_arrange)) {
    municipios_capital_reg_i <- municipios_capital_arrange[i,]
    municipios_capital_reg <- rbind(municipios_capital_reg,
                                    tibble(id=rep(municipios_capital_reg_i$id, 26),
                                           MUNICIPIO=rep(municipios_capital_reg_i$municipio, 26),
                                           DEPARTAMENTO=rep(municipios_capital_reg_i$depto, 26),
                                           year=1997:2022))
  }
    
  cultivation_coords <- cultivation %>% 
    select(-CODDEPTO, -MUNICIPIO, -DEPARTAMENTO) %>% 
    right_join(municipio_centroid %>%
                 rename(MUNICIPIO=municipio, DEPARTAMENTO=depto),
               by="id") %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO)
  
  cultivation_reg_data <- cultivation_coords %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO) %>% 
    pivot_longer(X1999:X2016, names_to="year", values_to="coca_area") %>% 
    mutate(year=substr(year,2,5) %>% as.integer)
    
  
  labs_PPI_reg_data <- labs_PPI %>% 
    select(-CODDEPTO) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
    pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer) %>% 
    select(-MUNICIPIO, -DEPARTAMENTO) %>% 
    right_join(municipios_capital_reg, by=c("id", "year")) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO)
  
  labs_hyd_reg_data <- labs_hyd %>% 
    select(-CODDEPTO) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
    pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer) %>% 
    select(-MUNICIPIO, -DEPARTAMENTO) %>% 
    right_join(municipios_capital_reg, by=c("id", "year")) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO)
  
  
  labs_PPI_reg_data <- labs_PPI_reg_data %>% 
    left_join(cultivation_reg_data %>% select(id, year, coca_area), by=c("id", "year"))
  labs_PPI_reg_data$coca_distance <- 0
  no_coca_index <- which(is.na(labs_PPI_reg_data$coca_area))
  
  for (i in no_coca_index) { # the closest distance to a coca cultivated municipio for municipios and years without coca cultivation
    id_i <- labs_PPI_reg_data$id[i]
    year_i <- labs_PPI_reg_data$year[i]
    
    if (!(year_i %in% 1999:2016)) next
    
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    year_index <- grep(year_i, names(cultivation_coords))
    cultivation_index <- which(!is.na(cultivation_coords[,year_index]))
    distance_i <- sqrt((long_i - cultivation_coords$long[cultivation_index])^2 + (lat_i - cultivation_coords$lat[cultivation_index])^2) %>% min
    labs_PPI_reg_data$coca_distance[i] <- distance_i
  }
  
  labs_PPI_reg_data <- labs_PPI_reg_data %>% 
    left_join(price_annual %>% select(id, year, paste_avg, base_avg, hyd_avg), by=c("id", "year"))
  labs_PPI_reg_data$paste_price_distance <- 0
  no_paste_index <- which(is.na(labs_PPI_reg_data$paste_avg))
  no_paste_index <- no_paste_index[which(labs_PPI_reg_data$year[no_paste_index]>2012 & labs_PPI_reg_data$year[no_paste_index]<2022)]
  
  for (i in no_paste_index) { # the closest distance to a municipio with paste price observation from municipios and years without price
    id_i <- labs_PPI_reg_data$id[i]
    year_i <- labs_PPI_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    neighbors_id_price <- price_annual %>% filter(year == year_i & !is.na(paste_avg)) %>% select(id, paste_avg)
    neighbors_id_price <- neighbors_id_price %>% left_join(municipio_centroid, by="id")
    neighbors_distances <- sqrt((long_i - neighbors_id_price$long)^2 + (lat_i - neighbors_id_price$lat)^2)
    distance_i <- min(neighbors_distances)
    
    labs_PPI_reg_data$paste_price_distance[i] <- distance_i
    labs_PPI_reg_data$paste_avg[i] <- neighbors_id_price$paste_avg[which.min(neighbors_distances)]
  }
  
  labs_PPI_reg_data$base_price_distance <- 0
  no_base_index <- which(is.na(labs_PPI_reg_data$base_avg))
  no_base_index <- no_base_index[which(labs_PPI_reg_data$year[no_base_index]>2012 & labs_PPI_reg_data$year[no_base_index]<2022)]
  
  for (i in no_base_index) { # the closest distance to a municipio with base price observation from municipios and years without price
    id_i <- labs_PPI_reg_data$id[i]
    year_i <- labs_PPI_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    neighbors_id_price <- price_annual %>% filter(year == year_i & !is.na(base_avg)) %>% select(id, base_avg)
    neighbors_id_price <- neighbors_id_price %>% left_join(municipio_centroid, by="id")
    neighbors_distances <- sqrt((long_i - neighbors_id_price$long)^2 + (lat_i - neighbors_id_price$lat)^2)
    distance_i <- min(neighbors_distances)
    
    labs_PPI_reg_data$base_price_distance[i] <- distance_i
    labs_PPI_reg_data$base_avg[i] <- neighbors_id_price$base_avg[which.min(neighbors_distances)]
  }
  
  labs_PPI_reg_data$hyd_price_distance <- 0
  no_hyd_index <- which(is.na(labs_PPI_reg_data$hyd_avg))
  no_hyd_index <- no_hyd_index[which(labs_PPI_reg_data$year[no_hyd_index]>2012 & labs_PPI_reg_data$year[no_hyd_index]<2022)]
  
  for (i in no_hyd_index) { # the closest distance to a municipio with hyd price observation from municipios and years without price
    id_i <- labs_PPI_reg_data$id[i]
    year_i <- labs_PPI_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    neighbors_id_price <- price_annual %>% filter(year == year_i & !is.na(hyd_avg)) %>% select(id, hyd_avg)
    neighbors_id_price <- neighbors_id_price %>% left_join(municipio_centroid, by="id")
    neighbors_distances <- sqrt((long_i - neighbors_id_price$long)^2 + (lat_i - neighbors_id_price$lat)^2)
    distance_i <- min(neighbors_distances)
    
    labs_PPI_reg_data$hyd_price_distance[i] <- distance_i
    labs_PPI_reg_data$hyd_avg[i] <- neighbors_id_price$hyd_avg[which.min(neighbors_distances)]
  }
  
  labs_PPI_reg_data <- labs_PPI_reg_data %>% 
    left_join(river_length_muni %>% select(-municipio, -depto), by="id") %>%
    left_join(road_length_muni %>% select(-municipio, -depto), by="id") %>%
    left_join(n_armed_groups_long, by=c("id", "year")) %>% 
    left_join(eradication_aerial_long %>% select(id, year, erad_aerial), by=c("id", "year")) %>% 
    left_join(eradication_manual_long %>% select(id, year, erad_manual), by=c("id", "year")) %>% 
    left_join(coca_seizures_long %>% select(id, year, coca_seizures), by=c("id", "year")) %>%
    left_join(base_seizures_long %>% select(id, year, base_seizures), by=c("id", "year")) %>% 
    left_join(hyd_seizures_long %>% select(id, year, hyd_seizures), by=c("id", "year"))
  
  labs_hyd_reg_data <- labs_hyd_reg_data %>% 
    left_join(cultivation_reg_data %>% select(id, year, coca_area), by=c("id", "year"))
  labs_hyd_reg_data$coca_distance <- 0
  no_coca_index <- which(is.na(labs_hyd_reg_data$coca_area))
  
  for (i in no_coca_index) { # the closest distance to a coca cultivated municipio for municipios and years without coca cultivation
    id_i <- labs_hyd_reg_data$id[i]
    year_i <- labs_hyd_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    year_index <- grep(year_i, names(cultivation_coords))
    cultivation_index <- which(!is.na(cultivation_coords[,year_index]))
    distance_i <- sqrt((long_i - cultivation_coords$long[cultivation_index])^2 + (lat_i - cultivation_coords$lat[cultivation_index])^2) %>% min
    labs_hyd_reg_data$coca_distance[i] <- distance_i
  }
  
  labs_hyd_reg_data <- labs_hyd_reg_data %>% 
    left_join(river_length_muni %>% select(-municipio, -depto), by="id") %>%
    left_join(road_length_muni %>% select(-municipio, -depto), by="id") %>%
    left_join(n_armed_groups_long, by=c("id", "year")) %>% 
    left_join(eradication_aerial_long %>% select(id, year, erad_aerial), by=c("id", "year")) %>% 
    left_join(eradication_manual_long %>% select(id, year, erad_manual), by=c("id", "year")) %>% 
    left_join(coca_seizures_long %>% select(id, year, coca_seizures), by=c("id", "year")) %>%
    left_join(base_seizures_long %>% select(id, year, base_seizures), by=c("id", "year")) %>% 
    left_join(hyd_seizures_long %>% select(id, year, hyd_seizures), by=c("id", "year"))
  
}

labs_reg_data <- left_join(labs_PPI_reg_data,
                           labs_hyd_reg_data %>%
                             rename(n_hyd_labs=n_labs) %>% 
                             select(id, year, n_hyd_labs), by=c("id", "year")) %>% 
  rename(n_PPI_labs=n_labs)
# write.csv(labs_reg_data, "Colombia Data/labs_reg_data all municipio.csv", row.names = F)
labs_reg_data <- read.csv("Colombia Data/labs_reg_data all municipio.csv") %>% as_tibble

labs_2steps_years <- tibble()
{
ex_year <- 2013
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_2steps_year <- labs_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads) %>% 
  mutate(n_PPI_labs=ifelse(is.na(n_PPI_labs), 0, n_PPI_labs),
         n_hyd_labs=ifelse(is.na(n_hyd_labs), 0, n_hyd_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         hyd_seizures=ifelse(is.na(hyd_seizures), 0, hyd_seizures)) %>% 
  relocate(id, year, n_PPI_labs, n_hyd_labs) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:depto)), by="id")
labs_2steps_year[is.na(labs_2steps_year)] <- 0


data_year <- labs_2steps_year %>% left_join(population %>% select(id, population), by="id")
data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$hyd_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$hyd_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor

data_year <- data_year %>% 
  mutate(hyd_group = `Los Urabeños` + `Los Pachenga O AUTODEFENSAS CONQUISTADORAS DE LA SIERRA NEVADA (ACSN)` + `La Constru` + `Los Pelusos`)

data_year$base_group <- data_year$`Los Pelusos`

labs_2steps_years <- rbind(labs_2steps_years,
                               data_year %>%
                                 select(id:n_armed_groups, hyd_group, base_group, population, base_source_all:general_destination))
}
# no base source in anecdotal_annual (2014), insignificant association between Colombia data and base_source_all

{
ex_year <- 2014
  anecdotal_year <- anecdotal_annual %>%
    filter(YEAR == ex_year)
  labs_2steps_year <- labs_reg_data %>% 
    filter(year == ex_year) %>% 
    select(-MUNICIPIO, -DEPARTAMENTO, -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads) %>% 
    mutate(n_PPI_labs=ifelse(is.na(n_PPI_labs), 0, n_PPI_labs),
           n_hyd_labs=ifelse(is.na(n_hyd_labs), 0, n_hyd_labs),
           coca_area=ifelse(is.na(coca_area), 0, coca_area),
           erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
           erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
           coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
           base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
           hyd_seizures=ifelse(is.na(hyd_seizures), 0, hyd_seizures)) %>% 
    relocate(id, year, n_PPI_labs, n_hyd_labs) %>% 
    left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:depto)), by="id")
  labs_2steps_year[is.na(labs_2steps_year)] <- 0
  
  data_year <- labs_2steps_year %>% left_join(population %>% select(id, population), by="id")
  data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
  data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
  data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
  data_year$hyd_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
  data_year$hyd_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
  data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
  data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor
  
data_year <- data_year %>% 
  mutate(hyd_group = `Los Urabeños` + `Los Pachenga O AUTODEFENSAS CONQUISTADORAS DE LA SIERRA NEVADA (ACSN)` + `La Constru` + `Los Pelusos`)
data_year$base_group <- data_year$`Los Pelusos`

labs_2steps_years <- rbind(labs_2steps_years,
                           data_year %>%
                             select(id:n_armed_groups, hyd_group, base_group, population, base_source_all:general_destination))
}

{
ex_year <- 2016
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_2steps_year <- labs_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads) %>% 
  mutate(n_PPI_labs=ifelse(is.na(n_PPI_labs), 0, n_PPI_labs),
         n_hyd_labs=ifelse(is.na(n_hyd_labs), 0, n_hyd_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         hyd_seizures=ifelse(is.na(hyd_seizures), 0, hyd_seizures)) %>% 
  relocate(id, year, n_PPI_labs, n_hyd_labs) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:depto)), by="id")
labs_2steps_year[is.na(labs_2steps_year)] <- 0


data_year <- labs_2steps_year %>% left_join(population %>% select(id, population), by="id")
data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$hyd_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$hyd_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor

data_year <- data_year %>% 
  mutate(hyd_group = `Clan del Golfo (Formerly Los Urabeños)` + `Los Pachenga O AUTODEFENSAS CONQUISTADORAS DE LA SIERRA NEVADA (ACSN)` +
           `La Constru` + `Los Pelusos`)

data_year$base_group <- data_year$`Los Pelusos`

labs_2steps_years <- rbind(labs_2steps_years,
                           data_year %>%
                             select(id:n_armed_groups, hyd_group, base_group, population, base_source_all:general_destination))

}

regression_data_years <- labs_2steps_years

## lab ever existed
ever_lap <- regression_data_years %>% 
  group_by(id) %>% 
  summarise(n_PPI_labs=ifelse(sum(n_PPI_labs)>0, 1, 0),
            n_hyd_labs=ifelse(sum(n_hyd_labs)>0, 1, 0))


ever_lap_data_years <- regression_data_years %>% 
  select(-n_PPI_labs, -n_hyd_labs) %>% 
  left_join(ever_lap, by="id") %>% relocate(id, year, n_PPI_labs, n_hyd_labs)

## add probability that PPI/hyd labs exist
PPI_lab_glm_years <- glm(PPI_lab~., family="binomial",
                          data=ever_lap_data_years %>%
                            mutate(PPI_lab=ifelse(n_PPI_labs > 0, 1, 0) %>% as.factor,
                                   year=as.factor(year)) %>%
                            select(year, coca_area:population, PPI_lab, -hyd_avg, -hyd_price_distance, -hyd_seizures))
summary(PPI_lab_glm_years)

hyd_lab_glm_years <- glm(hyd_lab~., family="binomial",
                         data=ever_lap_data_years %>%
                           mutate(hyd_lab=ifelse(n_hyd_labs > 0, 1, 0) %>% as.factor,
                                  year=as.factor(year)) %>%
                           select(year, coca_area:population, hyd_lab, -base_avg, -base_price_distance,
                                  -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(hyd_lab_glm_years)

airports <- airports %>% 
  mutate(airport=ifelse(n_airports > 0, 1, 0)) %>% 
  select(-n_airports)
  
regression_data_years <- regression_data_years %>% 
  mutate(PPI_lab_prob=PPI_lab_glm_years$fitted.values,
         hyd_lab_prob=hyd_lab_glm_years$fitted.values) %>% 
  left_join(airports, by="id")
# write.csv(regression_data_years %>% relocate(id, year, n_PPI_labs, PPI_lab_prob, n_hyd_labs, hyd_lab_prob),
#           "Colombia Data/regression data all municipios ever lab (02-05-2025).csv", row.names = F)

regression_data_years <- read.csv("Colombia Data/regression data all municipios ever lab (02-05-2025).csv") %>% as_tibble
regression_data_years_old <- read.csv("Colombia Data/regression data (05-15-2024).csv") %>% as_tibble

lab_probs <- regression_data_years %>% 
  select(id, year, PPI_lab_prob, hyd_lab_prob) %>% arrange(id)
lab_probs_old <- regression_data_years_old %>% 
  select(id, year, PPI_lab_prob, hyd_lab_prob) %>% arrange(id)

lab_probs_comparison <- left_join(lab_probs_old, lab_probs, by=c("id", "year")) %>%
  relocate(id, year, PPI_lab_prob.x, PPI_lab_prob.y, hyd_lab_prob.x, hyd_lab_prob.y) %>% 
  rename(PPI_lab_prob_516=PPI_lab_prob.x, 
         PPI_lab_prob_1120=PPI_lab_prob.y, 
         hyd_lab_prob_516=hyd_lab_prob.x,
         hyd_lab_prob_1120=hyd_lab_prob.y)
# write.csv(lab_probs_comparison, "Colombia Data/lab probabilities comparison (07-05-2024).csv", row.names=F)

summary(lab_probs_comparison$PPI_lab_prob.x - lab_probs_comparison$PPI_lab_prob.y)
sum(abs(lab_probs_comparison$PPI_lab_prob.x - lab_probs_comparison$PPI_lab_prob.y) > 0.1) # 53 out of 1623
sum(abs(lab_probs_comparison$hyd_lab_prob.x - lab_probs_comparison$hyd_lab_prob.y) > 0.1) # 23 out of 1623
