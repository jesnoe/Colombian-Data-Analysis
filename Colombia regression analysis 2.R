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
  mutate(id=as.numeric(id))
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
  
names(data_year) <- gsub(" ", "_", names(data_year)) %>% stri_trans_general("Latin-ASCII") 
names(data_year)[15:37] <- c("Los_Rastrojos", "Clan_del_Golfo", "Las_Aguilas_Negras", "Los_Paisas",
                             "Ejercito_Revolucionario_Popular_Antisubversivo_de_Colombia", "Los_Pachenga_O_AUTODEFENSAS_CONQUISTADORAS_DE_LA_SIERRA_NEVADA",
                             "Los_Caparros_FRENTE_VIRGILIO_PERALTA_ARENAS", "Los_Pachelly", "Los_Puntilleros", "La_Constru", "Los_Contadores",
                             "La_Oficina_de_Envigado_DEL_VALLE_DE_ABURRA_U_OVA", "Los_Pelusos", "Libertadores_del_Nordeste_NUEVO_RENACER_O_GUEROS", "La_Cordillera",
                             "La_Empresa", "La_Local", "Los_Caquetenos", "Los_Costenos", "Nuevo_Orden", "HEROES_DEL_CENTRAL_BOLIVAR_BAJO_CAUCA",
                             "Grupos_no_identificados", "Autodefensas_Unidas_de_Colombia")

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

## regression with data_year (population and rivers)
  # base source/destination
base_source_glm <- glm(base_source~., family="binomial",
                       data=data_year %>%
                         # mutate(n_labs=ifelse(n_labs > 0, 1, 0)) %>% 
                         select(n_labs:population, base_source))
summary(base_source_glm)

base_source_glm2 <- glm(base_source~., family="binomial",
                        data=data_year %>%
                          mutate(coca_group=(Clan_del_Golfo + Los_Pelusos)) %>%
                          select(n_labs:base_seizures, -erad_aerial, coca_group, population, base_source))
summary(base_source_glm2)

base_source_glm_reduced <- glm(base_source~., family="binomial",
                        data=data_year %>%
                          mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                          select(n_labs:coca_seizures, -base_price_distance, -erad_aerial, -erad_manual, coca_group, population, base_source))
summary(base_source_glm_reduced)

base_source_year <- data_year %>%
  select(id, n_labs:population, base_source) %>% 
  mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
  mutate(posterior_glm=base_source_glm$fitted.values) %>% 
  mutate(posterior_glm2=base_source_glm2$fitted.values) %>% 
  mutate(posterior_glm_r=base_source_glm_reduced$fitted.values)

threshold <- .1
base_source_year %>% filter(posterior_glm < threshold) %>% select(base_source, posterior_glm) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_glm >= threshold) %>% select(base_source, posterior_glm) %>% pull(base_source) %>% table

base_source_year %>% filter(posterior_glm2 < threshold) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_glm2 >= threshold) %>% pull(base_source) %>% table # lower true 1

base_source_year %>% filter(posterior_glm_r < threshold) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_glm_r >= threshold) %>% pull(base_source) %>% table # lower true 1

base_source_year %>% 
  filter(posterior_glm < threshold & base_source == 1) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm2)

base_source_year %>% 
  filter(posterior_glm2 < threshold & base_source == 1) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm2)

base_source_year %>% 
  filter(posterior_glm > threshold) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm)

base_source_year %>% 
  filter(posterior_glm2 > threshold) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm2)

base_source_year %>% filter(posterior_glm < threshold & base_source == 1) %>% select(base_source, posterior_glm) %>% arrange(posterior_glm)

set.seed(291038)
base_source_rf <- randomForest(base_source~.,
                               data=data_year %>%
                                 select(n_labs:population, base_source),
                               type="classification", ntree=50, mtry=20)

base_source_rf2 <- randomForest(base_source~.,
                                data=data_year %>%
                                  mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                                  select(n_labs:base_seizures, coca_group, population, base_source),
                                type="classification", ntree=50, mtry=10)
base_source_year$posterior_rf <- base_source_rf$votes[,2]
base_source_year$posterior_rf2 <- base_source_rf2$votes[,2]

threshold <- .1
base_source_year %>% filter(posterior_rf < threshold) %>% select(base_source, posterior_rf) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_rf >= threshold) %>% select(base_source, posterior_rf) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_rf < threshold & base_source == 1) %>% select(base_source, posterior_rf) %>% arrange(posterior_rf)

base_source_year %>% filter(posterior_rf2 < threshold) %>% select(base_source, posterior_rf2) %>% pull(base_source) %>% table # worse than individual groups
base_source_year %>% filter(posterior_rf2 >= threshold) %>% select(base_source, posterior_rf2) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_rf2 < threshold & base_source == 1) %>% select(base_source, posterior_rf2) %>% arrange(posterior_rf2)

base_source_year <- base_source_year %>% 
  mutate(y_glm=ifelse(posterior_glm < threshold, 0, 1),
         y_glm2=ifelse(posterior_glm2 < threshold, 0, 1),
         y_rf=ifelse(posterior_rf < threshold, 0, 1),
         y_rf2=ifelse(posterior_rf2 < threshold, 0, 1))

base_source_year %>%
  filter(y_glm != y_rf2) %>% 
  select(base_source, y_glm, y_rf2, posterior_glm, posterior_rf2) %>% 
  print(n=20)

base_source_year %>%
  filter(y_glm2 != y_rf2) %>% 
  select(base_source, posterior_glm2, posterior_rf2)

sum(base_source_year$base_source == 1) # 50

data_year_base_source1_glm <- glm(base_source~., family="binomial",  data=data_year_base_source1 %>% select(n_labs:population, base_source))
summary(data_year_base_source1_glm)
data_year_base_source2_glm <- glm(base_source~., family="binomial",  data=data_year_base_source2 %>% select(n_labs:population, base_source))
summary(data_year_base_source2_glm)
data_year_base_source3_glm <- glm(base_source~., family="binomial",  data=data_year_base_source3 %>% select(n_labs:population, base_source))
summary(data_year_base_source3_glm)

data_year_base_destination_glm <- glm(base_destination~., family="binomial",  data=data_year %>% select(n_labs:population, base_destination))
summary(data_year_base_destination_glm)
data_year_base_destination1_glm <- glm(base_destination~., family="binomial",  data=data_year_base_destination1 %>% select(n_labs:population, base_destination))
summary(data_year_base_destination1_glm)
data_year_base_destination2_glm <- glm(base_destination~., family="binomial",  data=data_year_base_destination2 %>% select(n_labs:population, base_destination))
summary(data_year_base_destination2_glm)
data_year_base_destination3_glm <- glm(base_destination~., family="binomial",  data=data_year_base_destination3 %>% select(n_labs:population, base_destination))
summary(data_year_base_destination3_glm)


## kmeans clustering
set.seed(50)
base_source_kmeans <- kmeans(data_year %>%
                               mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                               select(n_labs:base_seizures, -erad_aerial, coca_group, population) %>% 
                               scale,
                             4)

for (i in 1:length(base_source_kmeans$size)) {
  print(paste("cluster", i, ":", sum(base_source_year$base_source == 1 & as.numeric(base_source_kmeans$cluster == i) == 1)))
}
base_source_kmeans$size

base_source_year$cluster <- as.numeric(base_source_kmeans$cluster == 3)
table(base_source_year$base_source)
table(base_source_year$cluster)

base_source_year %>% 
  filter(base_source == 1 & cluster == 0) %>% 
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm)

F1_PU <- function(glm_model, test_data, positive) {
  n_data <- nrow(test_data)
  positive <- as.numeric(positive)
  n_positive <- sum(positive, na.rm=T)
  y_hat <- ifelse(predict(glm_model, test_data) > threshold, 1, 0)
  recall <- sum(positive*y_hat, na.rm=T)/n_positive
  result <- recall^2/(sum(y_hat/n_data, na.rm=T))
  return(result)
}
test_data_source1 <- data_year[c(base_source_positive_index[!(base_source_positive_index %in% data_year_base_source1_index$positive)],
                     sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data_year_base_source1_index$unlabeled)], 50)), ]
F1_PU(data_year_base_source1_glm, test_data_source1, test_data_source1$base_source)
test_data_source2 <- data_year[c(base_source_positive_index[!(base_source_positive_index %in% data_year_base_source2_index$positive)],
                     sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data_year_base_source2_index$unlabeled)], 50)), ]
F1_PU(data_year_base_source2_glm, test_data_source2, test_data_source2$base_source)
test_data_source3 <- data_year[c(base_source_positive_index[!(base_source_positive_index %in% data_year_base_source3_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data_year_base_source3_index$unlabeled)], 50)), ]
F1_PU(data_year_base_source3_glm, test_data_source3, test_data_source3$base_source)

test_data_destination1 <- data_year[c(base_destination_positive_index[!(base_destination_positive_index %in% data_year_base_destination1_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data_year_base_destination1_index$unlabeled)], 50)), ]
F1_PU(data_year_base_destination1_glm, test_data_destination1, test_data_destination1$base_destination)
test_data_destination2 <- data_year[c(base_destination_positive_index[!(base_destination_positive_index %in% data_year_base_destination2_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data_year_base_destination2_index$unlabeled)], 50)), ]
F1_PU(data_year_base_destination2_glm, test_data_destination2, test_data_destination2$base_destination)
test_data_destination3 <- data_year[c(base_destination_positive_index[!(base_destination_positive_index %in% data_year_base_destination3_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data_year_base_destination3_index$unlabeled)], 50)), ]
F1_PU(data_year_base_destination3_glm, test_data_destination3, test_data_destination3$base_destination)

  # HCl source/destination
data_year_HCl_source_glm <- glm(HCl_source~., family="binomial",  data=data_year %>% select(n_labs:population, HCl_source))
summary(data_year_HCl_source_glm)
data_year_HCl_source1_glm <- glm(HCl_source~., family="binomial",  data=data_year_HCl_source1 %>% select(n_labs:population, HCl_source))
summary(data_year_HCl_source1_glm)
data_year_HCl_source2_glm <- glm(HCl_source~., family="binomial",  data=data_year_HCl_source2 %>% select(n_labs:population, HCl_source))
summary(data_year_HCl_source2_glm)
data_year_HCl_source3_glm <- glm(HCl_source~., family="binomial",  data=data_year_HCl_source3 %>% select(n_labs:population, HCl_source))
summary(data_year_HCl_source3_glm)

data_year_HCl_destination_glm <- glm(HCl_destination~., family="binomial",  data=data_year %>% select(n_labs:population, HCl_destination))
summary(data_year_HCl_destination_glm)
data_year_HCl_destination1_glm <- glm(HCl_destination~., family="binomial",  data=data_year_HCl_destination1 %>% select(n_labs:population, HCl_destination))
summary(data_year_HCl_destination1_glm)
data_year_HCl_destination2_glm <- glm(HCl_destination~., family="binomial",  data=data_year_HCl_destination2 %>% select(n_labs:population, HCl_destination))
summary(data_year_HCl_destination2_glm)
data_year_HCl_destination3_glm <- glm(HCl_destination~., family="binomial",  data=data_year_HCl_destination3 %>% select(n_labs:population, HCl_destination))
summary(data_year_HCl_destination3_glm)

test_data_source1 <- data_year[c(HCl_source_positive_index[!(HCl_source_positive_index %in% data_year_HCl_source1_index$positive)],
                             sample(HCl_source_unlabeled_index[!(HCl_source_unlabeled_index %in% data_year_HCl_source1_index$unlabeled)], 50)), ]
F1_PU(data_year_HCl_source1_glm, test_data_source1, test_data_source1$HCl_source)
test_data_source2 <- data_year[c(HCl_source_positive_index[!(HCl_source_positive_index %in% data_year_HCl_source2_index$positive)],
                             sample(HCl_source_unlabeled_index[!(HCl_source_unlabeled_index %in% data_year_HCl_source2_index$unlabeled)], 50)), ]
F1_PU(data_year_HCl_source2_glm, test_data_source2, test_data_source2$HCl_source)
test_data_source3 <- data_year[c(HCl_source_positive_index[!(HCl_source_positive_index %in% data_year_HCl_source3_index$positive)],
                             sample(HCl_source_unlabeled_index[!(HCl_source_unlabeled_index %in% data_year_HCl_source3_index$unlabeled)], 50)), ]
F1_PU(data_year_HCl_source3_glm, test_data_source3, test_data_source3$HCl_source)

test_data_destination1 <- data_year[c(HCl_destination_positive_index[!(HCl_destination_positive_index %in% data_year_HCl_destination1_index$positive)],
                                  sample(HCl_destination_unlabeled_index[!(HCl_destination_unlabeled_index %in% data_year_HCl_destination1_index$unlabeled)], 50)), ]
F1_PU(data_year_HCl_destination1_glm, test_data_destination1, test_data_destination1$HCl_destination)
test_data_destination2 <- data_year[c(HCl_destination_positive_index[!(HCl_destination_positive_index %in% data_year_HCl_destination2_index$positive)],
                                  sample(HCl_destination_unlabeled_index[!(HCl_destination_unlabeled_index %in% data_year_HCl_destination2_index$unlabeled)], 50)), ]
F1_PU(data_year_HCl_destination2_glm, test_data_destination2, test_data_destination2$HCl_destination)
test_data_destination3 <- data_year[c(HCl_destination_positive_index[!(HCl_destination_positive_index %in% data_year_HCl_destination3_index$positive)],
                                  sample(HCl_destination_unlabeled_index[!(HCl_destination_unlabeled_index %in% data_year_HCl_destination3_index$unlabeled)], 50)), ]
F1_PU(data_year_HCl_destination3_glm, test_data_destination3, test_data_destination3$HCl_destination)

# general source/destination
data_year_general_source_glm <- glm(general_source~., family="binomial",  data=data_year %>% select(n_labs:population, general_source))
summary(data_year_general_source_glm)

data_year_general_destination_glm <- glm(general_destination~., family="binomial",  data=data_year %>% select(n_labs:population, general_destination))
summary(data_year_general_destination_glm)
data_year_general_destination1_glm <- glm(general_destination~., family="binomial",  data=data_year_general_destination1 %>% select(n_labs:population, general_destination))
summary(data_year_general_destination1_glm)
data_year_general_destination2_glm <- glm(general_destination~., family="binomial",  data=data_year_general_destination2 %>% select(n_labs:population, general_destination))
summary(data_year_general_destination2_glm)
data_year_general_destination3_glm <- glm(general_destination~., family="binomial",  data=data_year_general_destination3 %>% select(n_labs:population, general_destination))
summary(data_year_general_destination3_glm)

test_data_destination1 <- data_year[c(general_destination_positive_index[!(general_destination_positive_index %in% data_year_general_destination1_index$positive)],
                                  sample(general_destination_unlabeled_index[!(general_destination_unlabeled_index %in% data_year_general_destination1_index$unlabeled)], 50)), ]
F1_PU(data_year_general_destination1_glm, test_data_destination1, test_data_destination1$general_destination)
test_data_destination2 <- data_year[c(general_destination_positive_index[!(general_destination_positive_index %in% data_year_general_destination2_index$positive)],
                                  sample(general_destination_unlabeled_index[!(general_destination_unlabeled_index %in% data_year_general_destination2_index$unlabeled)], 50)), ]
F1_PU(data_year_general_destination2_glm, test_data_destination2, test_data_destination2$general_destination)
test_data_destination3 <- data_year[c(general_destination_positive_index[!(general_destination_positive_index %in% data_year_general_destination3_index$positive)],
                                  sample(general_destination_unlabeled_index[!(general_destination_unlabeled_index %in% data_year_general_destination3_index$unlabeled)], 50)), ]
F1_PU(data_year_general_destination3_glm, test_data_destination3, test_data_destination3$general_destination)


## regression with data2 (data_year + cultivation, 1999~2016)
labs_PPI$lab_PPI_exist <- apply(labs_PPI, 1, function(x) return(sum(!is.na(x[5:30]))>0))
labs_HCl$lab_HCl_exist <- apply(labs_HCl, 1, function(x) return(sum(!is.na(x[5:30]))>0))
coca_seizures$coca_seizures <- apply(coca_seizures, 1, function(x) return(sum(!is.na(x[5:28]))>0))
data2 <- left_join(data_year, labs_PPI %>% select(id, lab_PPI_exist), by="id") %>% 
  left_join(labs_HCl %>% select(id, lab_HCl_exist), by="id") %>% 
  left_join(coca_seizures %>% select(id, coca_seizures), by="id")
data2$lab_PPI_exist <- ifelse(is.na(data2$lab_PPI_exist), F, T)
data2$lab_HCl_exist <- ifelse(is.na(data2$lab_HCl_exist), F, T)
data2$coca_seizures <- ifelse(is.na(data2$coca_seizures), F, T)

{
set.seed(5478)
data2_base_source1_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
data2_base_source1 <- data2[c(data2_base_source1_index$positive, data2_base_source1_index$unlabeled), ]
data2_base_source2_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
data2_base_source2 <- data2[c(data2_base_source1_index$positive, data2_base_source1_index$unlabeled), ]
data2_base_source3_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
data2_base_source3 <- data2[c(data2_base_source1_index$positive, data2_base_source1_index$unlabeled), ]
data2_base_destination1_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
data2_base_destination1 <- data2[c(data2_base_destination1_index$positive, data2_base_destination1_index$unlabeled), ]
data2_base_destination2_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
data2_base_destination2 <- data2[c(data2_base_destination1_index$positive, data2_base_destination1_index$unlabeled), ]
data2_base_destination3_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
data2_base_destination3 <- data2[c(data2_base_destination1_index$positive, data2_base_destination1_index$unlabeled), ]

data2_HCl_source1_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
data2_HCl_source1 <- data2[c(data2_HCl_source1_index$positive, data2_HCl_source1_index$unlabeled), ]
data2_HCl_source2_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
data2_HCl_source2 <- data2[c(data2_HCl_source1_index$positive, data2_HCl_source1_index$unlabeled), ]
data2_HCl_source3_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
data2_HCl_source3 <- data2[c(data2_HCl_source1_index$positive, data2_HCl_source1_index$unlabeled), ]
data2_HCl_destination1_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
data2_HCl_destination1 <- data2[c(data2_HCl_destination1_index$positive, data2_HCl_destination1_index$unlabeled), ]
data2_HCl_destination2_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
data2_HCl_destination2 <- data2[c(data2_HCl_destination1_index$positive, data2_HCl_destination1_index$unlabeled), ]
data2_HCl_destination3_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
data2_HCl_destination3 <- data2[c(data2_HCl_destination1_index$positive, data2_HCl_destination1_index$unlabeled), ]

data2_general_destination1_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
data2_general_destination1 <- data2[c(data2_general_destination1_index$positive, data2_general_destination1_index$unlabeled), ]
data2_general_destination2_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
data2_general_destination2 <- data2[c(data2_general_destination1_index$positive, data2_general_destination1_index$unlabeled), ]
data2_general_destination3_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
data2_general_destination3 <- data2[c(data2_general_destination1_index$positive, data2_general_destination1_index$unlabeled), ]
}


  # base source
data2_base_source_glm <- glm(base_source~., family="binomial",  data=data2 %>%
                               select(n_labs:population, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source_glm)
data2_base_source1_glm <- glm(base_source~., family="binomial",  data=data2_base_source1 %>% 
                                select(n_labs:population, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source1_glm)
data2_base_source2_glm <- glm(base_source~., family="binomial",  data=data2_base_source2 %>%
                                select(n_labs:population, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source2_glm)
data2_base_source3_glm <- glm(base_source~., family="binomial",  data=data2_base_source3 %>% 
                                select(n_labs:population, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source3_glm)

data2_base_destination_glm <- glm(base_destination~., family="binomial",  data=data2 %>%
                                    select(n_labs:population, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination_glm)
data2_base_destination1_glm <- glm(base_destination~., family="binomial",  data=data2_base_destination1 %>%
                                     select(n_labs:population, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination1_glm)
data2_base_destination2_glm <- glm(base_destination~., family="binomial",  data=data2_base_destination2 %>% 
                                     select(n_labs:population, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination2_glm)
data2_base_destination3_glm <- glm(base_destination~., family="binomial",  data=data2_base_destination3 %>% 
                                     select(n_labs:population, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination3_glm)

test_data_source1 <- data2[c(base_source_positive_index[!(base_source_positive_index %in% data2_base_source1_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data2_base_source1_index$unlabeled)], 50)), ]
F1_PU(data2_base_source1_glm, test_data_source1, test_data_source1$base_source)
test_data_source2 <- data2[c(base_source_positive_index[!(base_source_positive_index %in% data2_base_source2_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data2_base_source2_index$unlabeled)], 50)), ]
F1_PU(data2_base_source2_glm, test_data_source2, test_data_source2$base_source)
test_data_source3 <- data2[c(base_source_positive_index[!(base_source_positive_index %in% data2_base_source3_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data2_base_source3_index$unlabeled)], 50)), ]
F1_PU(data2_base_source3_glm, test_data_source3, test_data_source3$base_source)

test_data_destination1 <- data2[c(base_destination_positive_index[!(base_destination_positive_index %in% data2_base_destination1_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data2_base_destination1_index$unlabeled)], 50)), ]
F1_PU(data2_base_destination1_glm, test_data_destination1, test_data_destination1$base_destination)
test_data_destination2 <- data2[c(base_destination_positive_index[!(base_destination_positive_index %in% data2_base_destination2_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data2_base_destination2_index$unlabeled)], 50)), ]
F1_PU(data2_base_destination2_glm, test_data_destination2, test_data_destination2$base_destination)
test_data_destination3 <- data2[c(base_destination_positive_index[!(base_destination_positive_index %in% data2_base_destination3_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data2_base_destination3_index$unlabeled)], 50)), ]
F1_PU(data2_base_destination3_glm, test_data_destination3, test_data_destination3$base_destination)

  # HCl source
data2_HCl_source_glm <- glm(HCl_source~., family="binomial",  data=data2 %>% select(n_labs:population, HCl_source, X1999:X2016))
summary(data2_HCl_source_glm)
data2_HCl_source1_glm <- glm(HCl_source~., family="binomial",  data=data2_HCl_source1 %>% select(n_labs:population, HCl_source, X1999:X2016))
summary(data2_HCl_source1_glm)
data2_HCl_source2_glm <- glm(HCl_source~., family="binomial",  data=data2_HCl_source2 %>% select(n_labs:population, HCl_source, X1999:X2016))
summary(data2_HCl_source2_glm)
data2_HCl_source3_glm <- glm(HCl_source~., family="binomial",  data=data2_HCl_source3 %>% select(n_labs:population, HCl_source, X1999:X2016))
summary(data2_HCl_source3_glm)

data2_HCl_destination_glm <- glm(HCl_destination~., family="binomial",  data=data2 %>% select(n_labs:population, HCl_destination, X1999:X2016))
summary(data2_HCl_destination_glm)
data2_HCl_destination1_glm <- glm(HCl_destination~., family="binomial",  data=data2_HCl_destination1 %>% select(n_labs:population, HCl_destination, X1999:X2016))
summary(data2_HCl_destination1_glm)
data2_HCl_destination2_glm <- glm(HCl_destination~., family="binomial",  data=data2_HCl_destination2 %>% select(n_labs:population, HCl_destination, X1999:X2016))
summary(data2_HCl_destination2_glm)
data2_HCl_destination3_glm <- glm(HCl_destination~., family="binomial",  data=data2_HCl_destination3 %>% select(n_labs:population, HCl_destination, X1999:X2016))
summary(data2_HCl_destination3_glm)

  # general source
data2_general_source_glm <- glm(general_source~., family="binomial",  data=data2 %>% select(n_labs:population, general_source, X1999:X2016))
summary(data2_general_source_glm)
data2_general_source1_glm <- glm(general_source~., family="binomial",  data=data2_general_source1 %>% select(n_labs:population, general_source, X1999:X2016))
summary(data2_general_source1_glm)
data2_general_source2_glm <- glm(general_source~., family="binomial",  data=data2_general_source2 %>% select(n_labs:population, general_source, X1999:X2016))
summary(data2_general_source2_glm)
data2_general_source3_glm <- glm(general_source~., family="binomial",  data=data2_general_source3 %>% select(n_labs:population, general_source, X1999:X2016))
summary(data2_general_source3_glm)

data2_general_destination_glm <- glm(general_destination~., family="binomial",  data=data2 %>% select(n_labs:population, general_destination, X1999:X2016))
summary(data2_general_destination_glm)
data2_general_destination1_glm <- glm(general_destination~., family="binomial",  data=data2_general_destination1 %>% select(n_labs:population, general_destination, X1999:X2016))
summary(data2_general_destination1_glm)
data2_general_destination2_glm <- glm(general_destination~., family="binomial",  data=data2_general_destination2 %>% select(n_labs:population, general_destination, X1999:X2016))
summary(data2_general_destination2_glm)
data2_general_destination3_glm <- glm(general_destination~., family="binomial",  data=data2_general_destination3 %>% select(n_labs:population, general_destination, X1999:X2016))
summary(data2_general_destination3_glm)