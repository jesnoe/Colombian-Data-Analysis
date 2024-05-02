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
  HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
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
  labs_HCl <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (COCAINE HYDROCHLORIDE).csv") %>% as_tibble
  labs_HCl <- labs_HCl %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
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
  HCl_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Cocaine (kg).xlsx")
  coca_seizures <- coca_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  base_seizures <- base_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  HCl_seizures <- HCl_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  
  coca_seizures_long <- coca_seizures %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="coca_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(coca_seizures))
  base_seizures_long <- base_seizures %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="base_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(base_seizures))
  HCl_seizures_long <- HCl_seizures %>% 
    pivot_longer(`1999`:`2022`, names_to="year", values_to="HCl_seizures") %>% 
    mutate(year=as.integer(year)) %>% 
    filter(!is.na(HCl_seizures))
  
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
price_med
## regression
{
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id, municipio, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  
  cultivation <- cultivation %>% left_join(municipio_centroid %>% select(id, long, lat), by="id")
  
  cultivation_reg_data <- cultivation %>% 
    select(-CODDEPTO) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO) %>% 
    pivot_longer(X1999:X2016, names_to="year", values_to="coca_area") %>% 
    mutate(year=substr(year,2,5) %>% as.integer)
  
  labs_PPI %>% apply(1, function(x) return(sum(as.numeric(x[5:30]), na.rm=T))) %>% table
  labs_PPI_reg_data <- labs_PPI %>% 
    select(-CODDEPTO) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
    pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer)
  
  labs_HCl %>% apply(1, function(x) return(sum(as.numeric(x[5:30]), na.rm=T))) %>% table
  labs_HCl_reg_data <- labs_HCl %>% 
    select(-CODDEPTO) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
    pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer) 
  
  
  labs_PPI_reg_data <- labs_PPI_reg_data %>% 
    left_join(cultivation_reg_data %>% select(id, year, coca_area), by=c("id", "year"))
  labs_PPI_reg_data$coca_distance <- 0
  no_coca_index <- which(is.na(labs_PPI_reg_data$coca_area))
  
  for (i in no_coca_index) { # the closest distance to a coca cultivated municipio for municipios and years without coca cultivation
    id_i <- labs_PPI_reg_data$id[i]
    year_i <- labs_PPI_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    year_index <- grep(year_i, names(cultivation))
    cultivation_index <- which(!is.na(cultivation[,year_index]))
    distance_i <- sqrt((long_i - cultivation$long[cultivation_index])^2 + (lat_i - cultivation$lat[cultivation_index])^2) %>% min
    labs_PPI_reg_data$coca_distance[i] <- distance_i
  }
  labs_PPI_reg_data$coca_distance <- ifelse(is.infinite(labs_PPI_reg_data$coca_distance), NA, labs_PPI_reg_data$coca_distance)
  labs_PPI_reg_data <- labs_PPI_reg_data %>% filter(!is.na(year))
  
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
    full_join(river_length_muni %>% select(-municipio, -depto), by="id") %>%
    full_join(road_length_muni %>% select(-municipio, -depto), by="id") %>%
    left_join(n_armed_groups_long, by=c("id", "year")) %>% 
    left_join(eradication_aerial_long %>% select(id, year, erad_aerial), by=c("id", "year")) %>% 
    left_join(eradication_manual_long %>% select(id, year, erad_manual), by=c("id", "year")) %>% 
    left_join(coca_seizures_long %>% select(id, year, coca_seizures), by=c("id", "year")) %>%
    left_join(base_seizures_long %>% select(id, year, base_seizures), by=c("id", "year")) %>% 
    left_join(HCl_seizures_long %>% select(id, year, HCl_seizures), by=c("id", "year"))
  
  labs_HCl_reg_data <- labs_HCl_reg_data %>% 
    left_join(cultivation_reg_data %>% select(id, year, coca_area), by=c("id", "year"))
  labs_HCl_reg_data$coca_distance <- 0
  no_coca_index <- which(is.na(labs_HCl_reg_data$coca_area))
  
  for (i in no_coca_index) { # the closest distance to a coca cultivated municipio for municipios and years without coca cultivation
    id_i <- labs_HCl_reg_data$id[i]
    year_i <- labs_HCl_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    year_index <- grep(year_i, names(cultivation))
    cultivation_index <- which(!is.na(cultivation[,year_index]))
    distance_i <- sqrt((long_i - cultivation$long[cultivation_index])^2 + (lat_i - cultivation$lat[cultivation_index])^2) %>% min
    labs_HCl_reg_data$coca_distance[i] <- distance_i
  }
  
  labs_HCl_reg_data <- labs_HCl_reg_data %>% 
    full_join(river_length_muni %>% select(-municipio, -depto), by="id") %>%
    full_join(road_length_muni %>% select(-municipio, -depto), by="id") %>%
    left_join(n_armed_groups_long, by=c("id", "year")) %>% 
    left_join(eradication_aerial_long %>% select(id, year, erad_aerial), by=c("id", "year")) %>% 
    left_join(eradication_manual_long %>% select(id, year, erad_manual), by=c("id", "year")) %>% 
    left_join(coca_seizures_long %>% select(id, year, coca_seizures), by=c("id", "year")) %>%
    left_join(base_seizures_long %>% select(id, year, base_seizures), by=c("id", "year")) %>% 
    left_join(HCl_seizures_long %>% select(id, year, HCl_seizures), by=c("id", "year"))
  
  labs_HCl_reg_data$coca_distance <- ifelse(is.infinite(labs_HCl_reg_data$coca_distance), NA, labs_HCl_reg_data$coca_distance)
  labs_HCl_reg_data <- labs_HCl_reg_data %>% filter(!is.na(year))
  
  labs_HCl_reg_data <- labs_HCl_reg_data %>% 
    left_join(price_annual %>% select(id, year, paste_avg, base_avg, hyd_avg), by=c("id", "year"))
  labs_HCl_reg_data$paste_price_distance <- 0
  no_paste_index <- which(is.na(labs_HCl_reg_data$paste_avg))
  no_paste_index <- no_paste_index[which(labs_HCl_reg_data$year[no_paste_index]>2012 & labs_HCl_reg_data$year[no_paste_index]<2022)]
  
  for (i in no_paste_index) { # the closest distance to a municipio with paste price observation from municipios and years without price
    id_i <- labs_HCl_reg_data$id[i]
    year_i <- labs_HCl_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    neighbors_id_price <- price_annual %>% filter(year == year_i & !is.na(paste_avg)) %>% select(id, paste_avg)
    neighbors_id_price <- neighbors_id_price %>% left_join(municipio_centroid, by="id")
    neighbors_distances <- sqrt((long_i - neighbors_id_price$long)^2 + (lat_i - neighbors_id_price$lat)^2)
    distance_i <- min(neighbors_distances)
    
    labs_HCl_reg_data$paste_price_distance[i] <- distance_i
    labs_HCl_reg_data$paste_avg[i] <- neighbors_id_price$paste_avg[which.min(neighbors_distances)]
  }
  
  labs_HCl_reg_data$base_price_distance <- 0
  no_base_index <- which(is.na(labs_HCl_reg_data$base_avg))
  no_base_index <- no_base_index[which(labs_HCl_reg_data$year[no_base_index]>2012 & labs_HCl_reg_data$year[no_base_index]<2022)]
  
  for (i in no_base_index) { # the closest distance to a municipio with base price observation from municipios and years without price
    id_i <- labs_HCl_reg_data$id[i]
    year_i <- labs_HCl_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    neighbors_id_price <- price_annual %>% filter(year == year_i & !is.na(base_avg)) %>% select(id, base_avg)
    neighbors_id_price <- neighbors_id_price %>% left_join(municipio_centroid, by="id")
    neighbors_distances <- sqrt((long_i - neighbors_id_price$long)^2 + (lat_i - neighbors_id_price$lat)^2)
    distance_i <- min(neighbors_distances)
    
    labs_HCl_reg_data$base_price_distance[i] <- distance_i
    labs_HCl_reg_data$base_avg[i] <- neighbors_id_price$base_avg[which.min(neighbors_distances)]
  }
  
  labs_HCl_reg_data$hyd_price_distance <- 0
  no_hyd_index <- which(is.na(labs_HCl_reg_data$hyd_avg))
  no_hyd_index <- no_hyd_index[which(labs_HCl_reg_data$year[no_hyd_index]>2012 & labs_HCl_reg_data$year[no_hyd_index]<2022)]
  
  for (i in no_hyd_index) { # the closest distance to a municipio with hyd price observation from municipios and years without price
    id_i <- labs_HCl_reg_data$id[i]
    year_i <- labs_HCl_reg_data$year[i]
    municipio_index <- which(municipio_centroid$id == id_i)
    long_i <- municipio_centroid$long[municipio_index]
    lat_i <- municipio_centroid$lat[municipio_index]
    neighbors_id_price <- price_annual %>% filter(year == year_i & !is.na(hyd_avg)) %>% select(id, hyd_avg)
    neighbors_id_price <- neighbors_id_price %>% left_join(municipio_centroid, by="id")
    neighbors_distances <- sqrt((long_i - neighbors_id_price$long)^2 + (lat_i - neighbors_id_price$lat)^2)
    distance_i <- min(neighbors_distances)
    
    labs_HCl_reg_data$hyd_price_distance[i] <- distance_i
    labs_HCl_reg_data$hyd_avg[i] <- neighbors_id_price$hyd_avg[which.min(neighbors_distances)]
  }
}
# write.csv(labs_PPI_reg_data, "Colombia Data/labs_PPI_reg_data.csv", row.names = F)
# write.csv(labs_HCl_reg_data, "Colombia Data/labs_HCl_reg_data.csv", row.names = F)

ex_year <- 2016
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_PPI_2steps_year <- labs_PPI_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -paste_avg, -hyd_avg, -paste_price_distance, -hyd_price_distance,
         -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads, -HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures)) %>% 
  left_join(labs_HCl_reg_data %>%
              filter(year == ex_year) %>%
              mutate(n_HCl_labs=n_labs) %>%
              select(id, n_HCl_labs), by="id") %>% 
  mutate(n_HCl_labs=ifelse(is.na(n_HCl_labs), 0, n_HCl_labs)) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:n_armed_groups)), by="id")
labs_PPI_2steps_year[is.na(labs_PPI_2steps_year)] <- 0

anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id) %>% unique %>% length # 64
sum(!(anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id) %>% unique %in% labs_PPI_2steps_year$id)) # 14 municipals in anecdotal data lack enough attributes (excluded)

{
  data_year <- labs_PPI_2steps_year %>% left_join(population %>% select(id, population), by="id")
  
  # names(data_year) <- gsub(" ", "_", names(data_year)) %>% stri_trans_general("Latin-ASCII") 
  # names(data_year)[15:37] <- c("Los_Rastrojos", "Clan_del_Golfo", "Las_Aguilas_Negras", "Los_Paisas",
  #                              "Ejercito_Revolucionario_Popular_Antisubversivo_de_Colombia", "Los_Pachenga_O_AUTODEFENSAS_CONQUISTADORAS_DE_LA_SIERRA_NEVADA",
  #                              "Los_Caparros_FRENTE_VIRGILIO_PERALTA_ARENAS", "Los_Pachelly", "Los_Puntilleros", "La_Constru", "Los_Contadores",
  #                              "La_Oficina_de_Envigado_DEL_VALLE_DE_ABURRA_U_OVA", "Los_Pelusos", "Libertadores_del_Nordeste_NUEVO_RENACER_O_GUEROS", "La_Cordillera",
  #                              "La_Empresa", "La_Local", "Los_Caquetenos", "Los_Costenos", "Nuevo_Orden", "HEROES_DEL_CENTRAL_BOLIVAR_BAJO_CAUCA",
  #                              "Grupos_no_identificados", "Autodefensas_Unidas_de_Colombia")
  
  data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
  data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
  data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
  data_year$HCl_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
  data_year$HCl_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
  data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
  data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor
  
  base_source_positive_index <- which(data_year$base_source == 1)
  base_source_unlabeled_index <- which(data_year$base_source == 0)
  base_destination_positive_index <- which(data_year$base_destination == 1)
  base_destination_unlabeled_index <- which(data_year$base_destination == 0)
  HCl_source_positive_index <- which(data_year$HCl_source == 1)
  HCl_source_unlabeled_index <- which(data_year$HCl_source == 0)
  HCl_destination_positive_index <- which(data_year$HCl_destination == 1)
  HCl_destination_unlabeled_index <- which(data_year$HCl_destination == 0)
  general_source_positive_index <- which(data_year$general_source == 1)
  general_source_unlabeled_index <- which(data_year$general_source == 0)
  general_destination_positive_index <- which(data_year$general_destination == 1)
  general_destination_unlabeled_index <- which(data_year$general_destination == 0)
}
sample_positive_unlabeled <- function(positive, unlabeled) {
  positive_samples <- positive[sample(1:nrow(positive), 50),]
  unlabeled_samples <- unlabeled[sample(1:nrow(unlabeled), 50),]
  result <- rbind(positive_samples, unlabeled_samples)
  return(result)
}

shuffle <- function(positive, unlabeled, N) { # N: number of positive and unlabeled samples
  result <- tibble(positive=sample(positive, N),
                   unlabeled=sample(unlabeled, N))
  return(result)
}

{
  set.seed(100)
  data_year_base_source1_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
  data_year_base_source1 <- data_year[c(data_year_base_source1_index$positive, data_year_base_source1_index$unlabeled), ]
  data_year_base_source2_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
  data_year_base_source2 <- data_year[c(data_year_base_source1_index$positive, data_year_base_source1_index$unlabeled), ]
  data_year_base_source3_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
  data_year_base_source3 <- data_year[c(data_year_base_source1_index$positive, data_year_base_source1_index$unlabeled), ]
  data_year_base_destination1_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
  data_year_base_destination1 <- data_year[c(data_year_base_destination1_index$positive, data_year_base_destination1_index$unlabeled), ]
  data_year_base_destination2_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
  data_year_base_destination2 <- data_year[c(data_year_base_destination1_index$positive, data_year_base_destination1_index$unlabeled), ]
  data_year_base_destination3_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
  data_year_base_destination3 <- data_year[c(data_year_base_destination1_index$positive, data_year_base_destination1_index$unlabeled), ]
  
  data_year_HCl_source1_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
  data_year_HCl_source1 <- data_year[c(data_year_HCl_source1_index$positive, data_year_HCl_source1_index$unlabeled), ]
  data_year_HCl_source2_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
  data_year_HCl_source2 <- data_year[c(data_year_HCl_source1_index$positive, data_year_HCl_source1_index$unlabeled), ]
  data_year_HCl_source3_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
  data_year_HCl_source3 <- data_year[c(data_year_HCl_source1_index$positive, data_year_HCl_source1_index$unlabeled), ]
  data_year_HCl_destination1_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
  data_year_HCl_destination1 <- data_year[c(data_year_HCl_destination1_index$positive, data_year_HCl_destination1_index$unlabeled), ]
  data_year_HCl_destination2_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
  data_year_HCl_destination2 <- data_year[c(data_year_HCl_destination1_index$positive, data_year_HCl_destination1_index$unlabeled), ]
  data_year_HCl_destination3_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
  data_year_HCl_destination3 <- data_year[c(data_year_HCl_destination1_index$positive, data_year_HCl_destination1_index$unlabeled), ]
  
  data_year_general_destination1_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
  data_year_general_destination1 <- data_year[c(data_year_general_destination1_index$positive, data_year_general_destination1_index$unlabeled), ]
  data_year_general_destination2_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
  data_year_general_destination2 <- data_year[c(data_year_general_destination1_index$positive, data_year_general_destination1_index$unlabeled), ]
  data_year_general_destination3_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
  data_year_general_destination3 <- data_year[c(data_year_general_destination1_index$positive, data_year_general_destination1_index$unlabeled), ]
}

labs_PPI_2steps_years <- tibble()
{
ex_year <- 2013
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_PPI_2steps_year <- labs_PPI_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)) %>% 
  left_join(labs_HCl_reg_data %>%
              filter(year == ex_year) %>%
              mutate(n_HCl_labs=n_labs) %>%
              select(id, n_HCl_labs), by="id") %>% 
  mutate(n_HCl_labs=ifelse(is.na(n_HCl_labs), 0, n_HCl_labs)) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:depto)), by="id")
labs_PPI_2steps_year[is.na(labs_PPI_2steps_year)] <- 0

anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id) %>% unique %>% length # 158
sum(!(anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id) %>% unique %in% labs_PPI_2steps_year$id)) # 83 municipals in anecdotal data lack enough attributes (excluded)

data_year <- labs_PPI_2steps_year %>% left_join(population %>% select(id, population), by="id")
data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$HCl_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$HCl_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor

HCl_destination_glm_year <- glm(HCl_destination~., family="binomial",
                           data=data_year %>%
                             # mutate(n_labs=ifelse(n_labs > 0, 1, 0)) %>%
                             select(n_labs:population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_destination_glm_year)
data_year$coca_group <- data_year$`Los Urabeños`
# labs_PPI_2steps_years <- rbind(labs_PPI_2steps_years,
#                                data_year %>%
#                                  select(id:n_armed_groups, coca_group, population, base_source_all:general_destination))

fit_test <- data_year %>%
  select(id, n_labs:n_HCl_labs, coca_group, population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_destination_glm_year$fitted.values)

sum(fit_test$HCl_destination == 1)
threshold <- .1
fit_test %>% filter(posterior_glm < threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table  # 0: 219, 1: 10
fit_test %>% filter(posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 247, 1: 65
}
# no base source in anecdotal_annual (2014), insignificant association between Colombia data and base_source_all

{
ex_year <- 2014
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_PPI_2steps_year <- labs_PPI_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)) %>% 
  left_join(labs_HCl_reg_data %>%
              filter(year == ex_year) %>%
              mutate(n_HCl_labs=n_labs) %>%
              select(id, n_HCl_labs), by="id") %>% 
  mutate(n_HCl_labs=ifelse(is.na(n_HCl_labs), 0, n_HCl_labs)) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:depto)), by="id")
labs_PPI_2steps_year[is.na(labs_PPI_2steps_year)] <- 0

anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id) %>% unique %>% length # 279
sum(!(anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id) %>% unique %in% labs_PPI_2steps_year$id)) # 103 municipals in anecdotal data lack enough attributes (excluded)

data_year <- labs_PPI_2steps_year %>% left_join(population %>% select(id, population), by="id")
data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$HCl_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$HCl_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor

HCl_destination_glm_year <- glm(HCl_destination~., family="binomial",
                           data=data_year %>%
                             # mutate(n_labs=ifelse(n_labs > 0, 1, 0)) %>%
                             select(n_labs:population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_destination_glm_year)
data_year$coca_group <- data_year$`Los Urabeños`
# labs_PPI_2steps_years <- rbind(labs_PPI_2steps_years, 
#                                data_year %>% 
#                                  select(id:n_armed_groups, coca_group, population, base_source_all:general_destination))

fit_test <- data_year %>%
  select(id, n_labs:n_HCl_labs, coca_group, population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_destination_glm_year$fitted.values)

sum(fit_test$HCl_destination == 1)
threshold <- .1
fit_test %>% filter(posterior_glm < threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table  # 0: 119, 1: 5
fit_test %>% filter(posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 246, 1: 171
}

{
ex_year <- 2016
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_PPI_2steps_year <- labs_PPI_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)) %>% 
  left_join(labs_HCl_reg_data %>%
              filter(year == ex_year) %>%
              mutate(n_HCl_labs=n_labs) %>%
              select(id, n_HCl_labs), by="id") %>% 
  mutate(n_HCl_labs=ifelse(is.na(n_HCl_labs), 0, n_HCl_labs)) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:depto)), by="id")
labs_PPI_2steps_year[is.na(labs_PPI_2steps_year)] <- 0

anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id) %>% unique %>% length # 124
sum(!(anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id) %>% unique %in% labs_PPI_2steps_year$id)) # 24 municipals in anecdotal data lack enough attributes (excluded)

data_year <- labs_PPI_2steps_year %>% left_join(population %>% select(id, population), by="id")
data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$HCl_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$HCl_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor

HCl_destination_glm_year <- glm(HCl_destination~., family="binomial",
                           data=data_year %>%
                             # mutate(n_labs=ifelse(n_labs > 0, 1, 0)) %>%
                             select(n_labs:population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_destination_glm_year)
data_year <- data_year %>% 
  mutate(coca_group = `Clan del Golfo (Formerly Los Urabeños)` + `Los Pachenga O AUTODEFENSAS CONQUISTADORAS DE LA SIERRA NEVADA (ACSN)` +
           `La Constru` + `Los Pelusos`)
# labs_PPI_2steps_years <- rbind(labs_PPI_2steps_years, 
#                                data_year %>% 
#                                  select(id:n_armed_groups, coca_group, population, base_source_all:general_destination))

fit_test <- data_year %>%
  select(id, n_labs:n_HCl_labs, coca_group, population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_destination_glm_year$fitted.values)

sum(fit_test$HCl_destination == 1)
threshold <- .1
fit_test %>% filter(posterior_glm < threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table  # 0: 140, 1: 14
fit_test %>% filter(posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 140, 1: 64
}
# write.csv(labs_PPI_2steps_years, "Colombia Data/regression data (5-01-2024).csv", row.names = F)

labs_PPI_2steps_years <- read.csv("Colombia Data/regression data (04-24-2024).csv") %>% as_tibble
HCl_destination_glm_years <- glm(HCl_destination~., family="binomial",
                                 data=labs_PPI_2steps_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   select(year, n_labs:population, HCl_destination, -base_avg, -base_price_distance,
                                          -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_destination_glm_years)

fit_test_years <- labs_PPI_2steps_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_labs:population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_destination_glm_years$fitted.values)

sum(fit_test_years$HCl_destination == 1) # 329
threshold <- .1
fit_test_years %>% filter(posterior_glm < threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table  # 0: 383, 1: 31
fit_test_years %>% filter(posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 911, 1: 298
298/329 # 0.9057751

sum(fit_test_years %>% filter(year == 2013) %>% pull(HCl_destination) == 1) # 75
sum(fit_test_years %>% filter(year == 2014) %>% pull(HCl_destination) == 1) # 176
sum(fit_test_years %>% filter(year == 2016) %>% pull(HCl_destination) == 1) # 78
65/75 # 0.8666667
171/176 # 0.9715909
64/78 # 0.8205128

threshold <- .2
fit_test_years %>% filter(year == 2013, posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 304, 1: 53
fit_test_years %>% filter(year == 2014, posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 344, 1: 176
fit_test_years %>% filter(year == 2016, posterior_glm >= threshold) %>% select(HCl_destination, posterior_glm) %>% pull(HCl_destination) %>% table # 0: 263, 1: 69

