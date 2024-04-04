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
    mutate(id=as.numeric(id))
  map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")
  
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

labs_PPI_pois_2010 <- glm(n_labs~., data=labs_PPI_reg_data %>%
                            filter(year == 2010) %>%
                            select(n_labs, coca_distance, river_length, road_length) %>% 
                            mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs)),
                          family="poisson")
summary(labs_PPI_pois_2010)

labs_PPI_logi_2010 <- glm(n_labs~.,
                          data=labs_PPI_reg_data %>%
                            filter(year == 2010) %>% 
                            select(n_labs, coca_distance, river_length, road_length) %>% 
                            mutate(n_labs=ifelse(is.na(n_labs), 0, 1)),
                          family="binomial")
summary(labs_PPI_logi_2010)

  ## Poisson regression
glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, erad_aerial) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, erad_manual) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance:erad_manual) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, coca_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
             base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
             coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
             base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             coca_seizures=ifelse(is.na(coca_seizures), 0, log(1+coca_seizures)),
             base_seizures=ifelse(is.na(base_seizures), 0, log(1+base_seizures)),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, log(1+HCl_seizures))),
    family="poisson") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
             coca_seizures=ifelse(is.na(coca_seizures), 0, log(1+coca_seizures)),
             base_seizures=ifelse(is.na(base_seizures), 0, log(1+base_seizures)),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, log(1+HCl_seizures))),
    family="poisson") %>% 
  summary

labs_PPI_reg_data1 <- labs_PPI_reg_data %>%
  filter(year == 2010) %>%
  select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
         coca_seizures=ifelse(is.na(coca_seizures), 0, log(1+coca_seizures)),
         base_seizures=ifelse(is.na(base_seizures), 0, log(1+base_seizures)),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, log(1+HCl_seizures)))
PPI_poisson <- glm(n_labs~., data=labs_PPI_reg_data1,
                   family="poisson")
PPI_poisson$count_residuals <- PPI_poisson$y - PPI_poisson$fitted.values

PPI_poisson_res <- data.frame(fitted_values=PPI_poisson$fitted.values, residuals=PPI_poisson$count_residuals, y=PPI_poisson$y)
PPI_poisson_res %>% 
  ggplot() +
  geom_point(aes(x=fitted_values, y=residuals))

exp(as.matrix(labs_PPI_reg_data1[which(PPI_poisson$residuals > 20), -1]) %*% as.matrix(PPI_poisson$coefficients[-1], ncol=1) + PPI_poisson$coefficients[1])
PPI_poisson
PPI_poisson_res[which(PPI_poisson$count_residuals > 200),]
labs_PPI_reg_data1[which(PPI_poisson$count_residuals > 200),] %>% mutate(n_labs=log(n_labs))


  ## logistic regression
glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1)),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, erad_aerial) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1)),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, erad_manual) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1)),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance:erad_manual) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1)),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
             base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
             coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
             base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             coca_seizures=ifelse(is.na(coca_seizures), 0, log(1+coca_seizures)),
             base_seizures=ifelse(is.na(base_seizures), 0, log(1+base_seizures)),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, log(1+HCl_seizures))),
    family="binomial") %>% 
  summary

glm(n_labs~., data=labs_PPI_reg_data %>%
      filter(year == 2010) %>%
      select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
             coca_seizures=ifelse(is.na(coca_seizures), 0, log(1+coca_seizures)),
             base_seizures=ifelse(is.na(base_seizures), 0, log(1+base_seizures)),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, log(1+HCl_seizures))),
    family="binomial") %>% 
  summary

labs_PPI_reg_data2 <- labs_PPI_reg_data %>%
  filter(year == 2010) %>%
  select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
         coca_seizures=ifelse(is.na(coca_seizures), 0, log(1+coca_seizures)),
         base_seizures=ifelse(is.na(base_seizures), 0, log(1+base_seizures)),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, log(1+HCl_seizures)))
PPI_logistic <- glm(n_labs~., data=labs_PPI_reg_data2,
                    family="binomial")
PPI_logistic$y_hat <- ifelse(PPI_logistic$fitted.values >= 0.5, 1, 0) %>% as.factor

confusionMatrix(PPI_logistic$y_hat, as.factor(PPI_logistic$y))

PPI_logistic_res <- data.frame(fitted_values = PPI_logistic$fitted.values,
                               residuals = PPI_logistic$y - PPI_logistic$fitted.values,
                               y = PPI_logistic$y,
                               pred_label = ifelse(PPI_logistic$y == 1 & PPI_logistic$y_hat == 1, "TP",
                                                   ifelse(PPI_logistic$y == 1 & PPI_logistic$y_hat == 0, "FN",
                                                          ifelse(PPI_logistic$y == 0 & PPI_logistic$y_hat == 0, "TN", "FP") )))
PPI_logistic_res %>% 
  ggplot() +
  geom_point(aes(x=fitted_values, y=residuals, color=pred_label))

exp(as.matrix(labs_PPI_reg_data1[which(PPI_logistic$residuals > 20), -1]) %*% as.matrix(PPI_logistic$coefficients[-1], ncol=1) + PPI_logistic$coefficients[1])
PPI_logistic
PPI_logistic_res[which(PPI_logistic$count_residuals > 200),]
labs_PPI_reg_data1[which(PPI_logistic$residuals > 0 & PPI_logistic$fitted.values < 0.25),] %>% as.data.frame
labs_PPI_reg_data1[which(PPI_logistic$residuals > 0 & PPI_logistic$fitted.values > 0.75),] %>% as.data.frame
labs_PPI_reg_data1[which(PPI_logistic$residuals < 0 & PPI_logistic$fitted.values < 0.25),] %>% as.data.frame
labs_PPI_reg_data1[which(PPI_logistic$residuals < 0 & PPI_logistic$fitted.values > 0.75),] %>% as.data.frame


labs_PPI_reg_data %>%
  filter(year == 2010) %>%
  select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual)) %>%
  apply(1, function(x) sum(is.na(x))) %>% 
  table # only 73 samples out of 835 have full observation in 2010

labs_HCl_reg_data %>%
  filter(year == 2010) %>%
  select(n_labs, coca_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual)) %>%
  apply(1, function(x) sum(is.na(x))) %>% 
  table # only 69 samples out of 818 have full observation in 2010

## correlation between n_labs and seizures
  # PPI
labs_PPI_cor_seizures <- tibble(year=1997:2022)
cor_column <- c()
for(year_i in 1997:2022) {
  cor_column <- c(cor_column,
                  cor(labs_PPI_reg_data %>%
                        filter(year == year_i) %>% 
                        select(n_labs, coca_seizures) %>% 
                        filter(!is.na(n_labs) & !is.na(coca_seizures)))[1,2])
}
labs_PPI_cor_seizures$coca <- cor_column

cor_column <- c()
for(year_i in 1997:2022) {
  cor_column <- c(cor_column,
                  cor(labs_PPI_reg_data %>%
                        filter(year == year_i) %>% 
                        select(n_labs, base_seizures) %>% 
                        filter(!is.na(n_labs) & !is.na(base_seizures)))[1,2])
}
labs_PPI_cor_seizures$base <- cor_column

cor_column <- c()
for(year_i in 1997:2022) {
  cor_column <- c(cor_column,
                  cor(labs_PPI_reg_data %>%
                        filter(year == year_i) %>% 
                        select(n_labs, HCl_seizures) %>% 
                        filter(!is.na(n_labs) & !is.na(HCl_seizures)))[1,2])
}
labs_PPI_cor_seizures$HCl <- cor_column

labs_PPI_cor_seizures[-(1:2),] %>%
  pivot_longer(-year,
               names_to = "cocaine_type",
               values_to = "correlation") %>% 
  ggplot() + ggtitle("Correlation Between # of PPI Labs and Seizures") + ylim(-0.3, 1) +
  geom_point(aes(x=year, y=correlation, color=cocaine_type)) +
  geom_line(aes(x=year, y=correlation, color=cocaine_type))
  
  # PPI price
pois_data <- labs_PPI_reg_data %>%
  filter(year == 2014) %>%
  select(n_labs, coca_distance:hyd_price_distance, base_price_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures))

glm(n_labs~., data=pois_data,
    family="poisson") %>% 
  summary

glm(n_labs~.+base_avg*base_price_distance+paste_avg*paste_price_distance+hyd_avg*hyd_price_distance,
    data=pois_data,
    family="poisson") %>% 
  summary

glm(n_labs~.+base_avg*base_price_distance+paste_avg*paste_price_distance+hyd_avg*hyd_price_distance,
    data=pois_data %>% 
      mutate(n_labs=log(1+n_labs),
             erad_aerial=log(1+erad_aerial),
             erad_manual=log(1+erad_manual),
             n_armed_groups=log(1+n_armed_groups),
             coca_seizures=log(1+coca_seizures),
             base_seizures=log(1+base_seizures),
             HCl_seizures=log(1+HCl_seizures)),
    family="poisson") %>% 
  summary # no Cobb-Douglas relationship


glm(n_labs~.+, data=labs_PPI_reg_data %>%
      filter(year == 2014) %>%
      select(n_labs, coca_distance:hyd_price_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
             coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
             base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)),
    family="binomial") %>% 
  summary

glm(n_labs~.+base_avg*base_price_distance+paste_avg*paste_price_distance+hyd_avg*hyd_price_distance,
    data=labs_PPI_reg_data %>%
      filter(year == 2014) %>%
      select(n_labs, coca_distance:hyd_price_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
      mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
             erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
             erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
             n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
             coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
             base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
             HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures)),
    family="binomial") %>% 
  summary


  # HCl price
labs_HCl_cor_seizures <- tibble(year=1997:2022)
cor_column <- c()
for(year_i in 1997:2022) {
  cor_column <- c(cor_column,
                  cor(labs_HCl_reg_data %>%
                        filter(year == year_i) %>% 
                        select(n_labs, coca_seizures) %>% 
                        filter(!is.na(n_labs) & !is.na(coca_seizures)))[1,2])
}
labs_HCl_cor_seizures$coca <- cor_column

cor_column <- c()
for(year_i in 1997:2022) {
  cor_column <- c(cor_column,
                  cor(labs_HCl_reg_data %>%
                        filter(year == year_i) %>% 
                        select(n_labs, base_seizures) %>% 
                        filter(!is.na(n_labs) & !is.na(base_seizures)))[1,2])
}
labs_HCl_cor_seizures$base <- cor_column

cor_column <- c()
for(year_i in 1997:2022) {
  cor_column <- c(cor_column,
                  cor(labs_HCl_reg_data %>%
                        filter(year == year_i) %>% 
                        select(n_labs, HCl_seizures) %>% 
                        filter(!is.na(n_labs) & !is.na(HCl_seizures)))[1,2])
}
labs_HCl_cor_seizures$HCl <- cor_column

labs_HCl_cor_seizures[-(1:2),] %>%
  pivot_longer(-year,
               names_to = "cocaine_type",
               values_to = "correlation") %>% 
  ggplot() + ggtitle("Correlation Between # of HCl Labs and Seizures") + ylim(-0.3, 1) +
  geom_point(aes(x=year, y=correlation, color=cocaine_type)) +
  geom_line(aes(x=year, y=correlation, color=cocaine_type))


pois_data <- labs_PPI_reg_data %>%
  filter(year == 2014) %>%
  select(n_labs, coca_distance:hyd_price_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures))
pois_data %>% summary


# armed groups experiments
pois_data_armed <- labs_PPI_reg_data %>%
  filter(year == 2016) %>%
  select(id, n_labs, coca_distance, base_avg, base_price_distance, river_length, road_length, n_armed_groups:coca_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures)) %>% 
  left_join(armed_groups$y2016[,-(2:4)], by="id")

logi_data <- labs_PPI_reg_data %>%
  filter(year == 2014) %>%
  select(n_labs, coca_distance, base_avg, base_price_distance, river_length, road_length, n_armed_groups:HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures),
         HCl_seizures=ifelse(is.na(HCl_seizures), 0, HCl_seizures))

glm(n_labs~., data=pois_data_armed[,which(apply(pois_data_armed, 2, function(x) sum(x, na.rm=T)) > 0)[-1]], family="poisson") %>% 
  summary

n_same_rows <- c()
for (i in 12:23) {
  n_same_rows <- c(n_same_rows, sum(data[,i] == data$`Autodefensas Unidas de Colombia (AUC)`, na.rm=T))
}
pois_data_armed[, (12:33)[which(n_same_rows ==534)]] %>% summary

for (y in years) {
  pois_data_year <- labs_PPI_reg_data %>%
    filter(year == y) %>%
    select(id, n_labs, coca_distance, base_avg, base_price_distance, river_length, road_length, n_armed_groups:coca_seizures) %>% 
    mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
           erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
           erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
           n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
           coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures)) %>% 
    left_join(armed_groups[[paste0("y", y)]][,-(2:4)], by="id") %>% select(-id)
  
  pois_reg_year <- glm(n_labs~., data=pois_data_year[,which(apply(pois_data_year, 2, function(x) sum(x, na.rm=T)) > 0)], family="poisson") %>% summary
  significant_groups <- names(pois_data_year)[-(1:10)][which(pois_reg_year$coefficients[-(1:10),4] <= 0.05)]
  cat("\n", y, "\n")
  print(significant_groups)
  print(paste("n_armed_groups:", ifelse(pois_reg_year$coefficients[,4][which(row.names(pois_reg_year$coefficients) == "n_armed_groups")] <= 0.05, "*", "-")))
}

for (y in years) {
  logi_data_year <- labs_PPI_reg_data %>%
    filter(year == y) %>%
    select(id, n_labs, coca_distance, base_avg, base_price_distance, river_length, road_length, n_armed_groups:coca_seizures) %>% 
    mutate(n_labs=ifelse(is.na(n_labs), 0, 1),
           erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
           erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
           n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
           coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures)) %>% 
    left_join(armed_groups[[paste0("y", y)]][,-(2:4)], by="id") %>% select(-id)
  
  logi_reg_year <- glm(n_labs~., data=logi_data_year[,which(apply(logi_data_year, 2, function(x) sum(x, na.rm=T)) > 0)], family="binomial") %>% summary
  significant_groups <- names(logi_data_year)[-(1:10)][which(logi_reg_year$coefficients[-(1:10),4] <= 0.05)]
  cat("\n", y, "\n")
  print(significant_groups)
  print(paste("n_armed_groups:", ifelse(logi_reg_year$coefficients[,4][which(row.names(logi_reg_year$coefficients) == "n_armed_groups")] <= 0.05, "*", "-")))
}


for (y in years) {
  pois_data_year <- labs_HCl_reg_data %>%
    filter(year == y) %>%
    select(id, n_labs, coca_distance, base_avg, base_price_distance, river_length, road_length, n_armed_groups:coca_seizures) %>% 
    mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
           erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
           erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
           n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
           coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures)) %>% 
    left_join(armed_groups[[paste0("y", y)]][,-(2:4)], by="id") %>% select(-id)
  
  pois_reg_year <- glm(n_labs~., data=pois_data_year[,which(apply(pois_data_year, 2, function(x) sum(x, na.rm=T)) > 0)], family="poisson") %>% summary
  significant_groups <- names(pois_data_year)[-(1:10)][which(pois_reg_year$coefficients[-(1:10),4] <= 0.05)]
  cat("\n", y, "\n")
  print(significant_groups)
  print(paste("n_armed_groups:", ifelse(pois_reg_year$coefficients[,4][which(row.names(pois_reg_year$coefficients) == "n_armed_groups")] <= 0.05, "*", "-")))
}

for (y in years) {
  logi_data_year <- labs_HCl_reg_data %>%
    filter(year == y) %>%
    select(id, n_labs, coca_distance, base_avg, base_price_distance, river_length, road_length, n_armed_groups:coca_seizures) %>% 
    mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
           erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
           erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
           n_armed_groups=ifelse(is.na(n_armed_groups), 0, n_armed_groups),
           coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures)) %>% 
    left_join(armed_groups[[paste0("y", y)]][,-(2:4)], by="id") %>% select(-id)
  
  logi_reg_year <- glm(n_labs~., data=logi_data_year[,which(apply(logi_data_year, 2, function(x) sum(x, na.rm=T)) > 0)], family="binomial") %>% summary
  significant_groups <- names(logi_data_year)[-(1:10)][which(logi_reg_year$coefficients[-(1:10),4] <= 0.05)]
  cat("\n", y, "\n")
  print(significant_groups)
  print(paste("n_armed_groups:", ifelse(logi_reg_year$coefficients[,4][which(row.names(logi_reg_year$coefficients) == "n_armed_groups")] <= 0.05, "*", "-")))
}

ggplot(labs_PPI_reg_data) +
  geom_point(aes(x=n_armed_groups, y=n_labs))

plot_list <- list()
for (y in years) {
  labs_PPI_reg_data %>% 
    filter(year == y) %>% 
    ggplot() +
    geom_point(aes(x=n_armed_groups, y=n_labs)) +
    ggtitle(y) -> plot_list[[paste0("y", y)]]
}
grid.arrange(plot_list$y2008, plot_list$y2010, plot_list$y2011, plot_list$y2012, plot_list$y2013,
             plot_list$y2014, plot_list$y2016, plot_list$y2017, plot_list$y2018, plot_list$y2020,
             ncol=5)
