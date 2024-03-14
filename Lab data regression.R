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
  map <- municipios
  map_df <- suppressMessages(fortify(map)) %>% 
    mutate(id=as.numeric(id))
  map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")
  
  population <- read.csv("Colombia Data/Census population by municipios (2018).csv") %>% as_tibble
  population$log_population <- log(population$population)
  
  armed_groups_combined <- list()
  years <- (2008:2016)[-c(2,8)]
  for (year in years) {
    armed_groups_combined_year <- read.csv(paste("Colombia Data/Armed Groups (Combined)/Colombia-Armed groups-Paramilitar", year ,"(combined).csv")) %>%
      as_tibble
    armed_groups_combined[[paste0("y", year)]] <- armed_groups_combined_year
  }
  armed_groups_combined$y2008
  
  n_armed_groups <- armed_groups_combined$y2008 %>% select(id, n_armed_groups) %>% rename(X2008=n_armed_groups)
  for (year in years[-1]) {
    n_armed_groups <- full_join(n_armed_groups, armed_groups_combined[[paste0("y", year)]] %>% select(id, n_armed_groups), by="id")
    names(n_armed_groups)[ncol(n_armed_groups)] <- paste0("X", year)
  }
  
  
  cultivation <- read.csv("Colombia Data/Colombia Coca Cultivation 1999-2016 renamed (Ha).csv") %>% as_tibble
  cultivation <- cultivation %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  labs_HCl <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (COCAINE HYDROCHLORIDE).csv") %>% as_tibble
  labs_HCl <- labs_HCl %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  labs_PPI <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (PRIMARY PRODUCTION INFRASTRUCTURE).csv") %>% as_tibble
  labs_PPI <- labs_PPI %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  
  eradication_aerial <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Aerial (Ha).xlsx")
  eradication_manual <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Manual (Ha).xlsx")
  coca_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca leaves (kg).xlsx")
  base_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca paste and base (kg).xlsx")
  HCl_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Cocaine (kg).xlsx")
  eradication_aerial <- eradication_aerial %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  eradication_manual <- eradication_manual %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
  coca_seizures <- coca_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  base_seizures <- base_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  HCl_seizures <- HCl_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
  
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

## regression
municipio_centroid <- map_df %>% 
  filter(!(id %in% c(88001, 88564))) %>% 
  group_by(id, municipio, depto) %>% 
  summarize(long=mean(long),
            lat=mean(lat))

cultivation <- cultivation %>% left_join(municipio_centroid %>% select(id, long, lat), by="id")

cultivation_reg_data <- cultivation %>% 
  select(-CODDEPTO) %>% 
  relocate(id, MUNICIPIO, DEPARTAMENTO) %>% 
  pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="coca_area") %>% 
  mutate(year=substr(year,2,5) %>% as.integer)

labs_PPI %>% apply(1, function(x) return(sum(as.numeric(x[5:30]), na.rm=T))) %>% table
labs_PPI_reg_data <- labs_PPI %>% 
  select(-CODDEPTO) %>% 
  relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
  pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
  mutate(year=substr(year,2,5) %>% as.integer) %>% 
  filter(year > 1998 & year <2017)

labs_HCl %>% apply(1, function(x) return(sum(as.numeric(x[5:30]), na.rm=T))) %>% table
labs_HCl_reg_data <- labs_HCl %>% 
  select(-CODDEPTO) %>% 
  relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
  pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
  mutate(year=substr(year,2,5) %>% as.integer) %>% 
  filter(year > 1998 & year < 2017)


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

labs_PPI_reg_data <- labs_PPI_reg_data %>% 
  left_join(river_length_muni %>% select(-municipio, -depto), by="id") %>% 
  left_join(road_length_muni %>% select(-municipio, -depto), by="id")

labs_PPI_glm_2010 <- glm(n_labs~., data=labs_PPI_reg_data %>% filter(year == 2010) %>% select(n_labs:n_roads), family="poisson")
summary(labs_PPI_glm_2010)
