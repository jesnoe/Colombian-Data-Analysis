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
    mutate(id=as.numeric(id)) %>% 
    filter(!(id %in% c(88001, 88564)))
  map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")
  
  base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
  HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
  general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
  anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual municipality only.csv") %>% as_tibble
  
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
  
  labs_PPI_reg_data <- labs_PPI %>% 
    select(-CODDEPTO) %>% 
    relocate(id, MUNICIPIO, DEPARTAMENTO, X1997:X2022) %>% 
    pivot_longer(-(id:DEPARTAMENTO), names_to="year", values_to="n_labs") %>% 
    mutate(year=substr(year,2,5) %>% as.integer)
  
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
}

# labs_PPI_reg_data %>% write.csv("Colombia Data/labs PPI reg data.csv", row.names=F)
# labs_HCl_reg_data %>% write.csv("Colombia Data/labs HCl reg data.csv", row.names=F)

labs_PPI_reg_data # 14,647 rows
labs_PPI_reg_data %>% filter(is.na(n_labs) & is.na(coca_seizures)) # 10,258
labs_PPI_reg_data %>% filter(is.na(n_labs) & is.na(base_seizures)) # 6,647
labs_PPI_reg_data %>% filter(is.na(n_labs) & is.na(HCl_seizures))  # 6,872
labs_PPI_reg_data %>% filter(is.na(n_labs) & is.na(coca_seizures) & is.na(base_seizures) & is.na(HCl_seizures)) # 5,834
labs_PPI_reg_data %>% filter(!is.na(n_labs) & !is.na(coca_seizures)) %>% select(id:n_labs, coca_seizures:HCl_seizures) # 2,700

labs_HCl_reg_data # 12,948 rows
labs_HCl_reg_data %>% filter(is.na(n_labs) & is.na(coca_seizures)) # 9,028
labs_HCl_reg_data %>% filter(is.na(n_labs) & is.na(base_seizures)) # 5,539
labs_HCl_reg_data %>% filter(is.na(n_labs) & is.na(HCl_seizures))  # 6,203
labs_HCl_reg_data %>% filter(is.na(n_labs) & is.na(coca_seizures) & is.na(base_seizures) & is.na(HCl_seizures)) # 4,659
labs_HCl_reg_data %>% filter(!is.na(n_labs) & !is.na(coca_seizures)) %>% select(id:n_labs, coca_seizures:HCl_seizures) # 1,023

labs_PPI_reg_data %>% filter(year == 2010) # 541 rows
labs_PPI_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(coca_seizures)) # 299
labs_PPI_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(base_seizures)) # 164
labs_PPI_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(HCl_seizures))  # 170
labs_PPI_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(coca_seizures) & is.na(base_seizures) & is.na(HCl_seizures)) # 117
labs_PPI_reg_data %>% filter(year == 2010 & !is.na(n_labs) & !is.na(coca_seizures)) %>% select(id:n_labs, coca_seizures:HCl_seizures) # 156

labs_HCl_reg_data %>% filter(year == 2010) # 498 rows
labs_HCl_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(coca_seizures)) # 311
labs_HCl_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(base_seizures)) # 150
labs_HCl_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(HCl_seizures))  # 207
labs_HCl_reg_data %>% filter(year == 2010 & is.na(n_labs) & is.na(coca_seizures) & is.na(base_seizures) & is.na(HCl_seizures)) # 109
labs_HCl_reg_data %>% filter(year == 2010 & !is.na(n_labs) & !is.na(coca_seizures)) %>% select(id:n_labs, coca_seizures:HCl_seizures) # 44

n_armed_groups2 <- n_armed_groups %>%
  left_join(municipios_capital[,-2], by="id") %>%
  relocate(id, municipio, depto) %>%
  mutate(n_non_zero=apply(n_armed_groups[,-1], 1, function(x) sum(x > 0))) %>%
  arrange(desc(n_non_zero))
# write.csv(n_armed_groups2, "Colombia Data/number of armed groups.csv", row.names=F)
table(n_armed_groups2$n_non_zero)
table(n_armed_groups2$n_non_zero)/nrow(n_armed_groups2) %>% round(-3)

for (year in years) {
  n_armed_groups_coord <- left_join(map_df, n_armed_groups[,c(1, grep(year, names(n_armed_groups)))], by="id")
  names(n_armed_groups_coord)[ncol(n_armed_groups_coord)] <- "n_armed_groups"
  n_armed_groups_map <- ggplot(n_armed_groups_coord) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = n_armed_groups),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) + 
    scale_fill_viridis_c(limits=c(0,6)) +
    labs(fill = "n_armed_groups", x="", y="", title=year) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Colombia Data/Figs/Armed groups maps/n_armed_groups (", year, ")", ".png"), n_armed_groups_map, scale=1)
}
# 
# armed_groups$y2008[,-(1:4)] %>% apply(2, function(x) sum(x, na.rm=T))
# 
# armed_groups_names <- c()
# for (year in years) {
#   armed_groups_year <- armed_groups[[paste0("y", year)]]
#   n_municipio_group <- armed_groups_year[,-(1:4)] %>% apply(2, function(x) sum(x, na.rm=T))
#   armed_groups_names <- c(armed_groups_names, names(armed_groups_year)[-(1:4)][which(n_municipio_group > 0)])
# }
# armed_groups_names <- unique(armed_groups_names) %>% sort
# 
# n_each_armed_groups <- matrix(0, length(years), length(armed_groups_names)) %>%
#   as_tibble %>%
#   mutate(year=years) %>%
#   relocate(year)
# names(n_each_armed_groups)[-1] <- armed_groups_names
# for (year in years) {
#   armed_groups_year <- armed_groups[[paste0("y", year)]]
#   n_municipio_group <- armed_groups_year[,-(1:4)] %>% apply(2, function(x) sum(x, na.rm=T))
#   non_zero_index <- which(n_municipio_group > 0)
# 
#   for (i in non_zero_index) {
#     n_each_armed_groups[which(years == year), grep(paste0("^", names(n_municipio_group)[i], "$"), names(n_each_armed_groups))] <- n_municipio_group[i]
#   }
# }
# t(n_each_armed_groups) %>% write.csv("Colombia Data/number of municipals for each armed group.csv")

active_groups_1016 <- c("Las Águilas Negras",
                        "Los Paisas",
                        "Los Rastrojos",
                        "Los Urabeños")
    # For 2016
# Los Pelusos
# Los Puntilleros

for (year in 2010:2014) {
  armed_groups_year <- armed_groups[[paste0("y", year)]]
  
  for (group_name in active_groups_1016) {
    if (sum(armed_groups_year[, grep(group_name, names(armed_groups_year))], na.rm=T) == 0) next
    group_coord <- armed_groups_year[, c(1, grep(group_name, names(armed_groups_year)))] %>% 
      right_join(map_df, by="id")
    names(group_coord)[2] <- "presence"
    group_coord$presence <- ifelse(is.na(group_coord$presence), group_coord$presence, 1)
    group_map <- ggplot(group_coord) +
      geom_polygon(aes(x = long,
                       y = lat,
                       group = group,
                       fill = presence),
                   color = "black",
                   linewidth = 0.1) +
      labs(fill = "", x="", y="", title=paste(group_name, year)) +
      theme_bw() +
      theme(legend.position ="none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank())
    ggsave(paste0("Colombia Data/Figs/Armed groups maps/",
                  year,
                  "/",
                  stri_trans_general(group_name, "Latin-ASCII"),
                  " (",
                  year,
                  ")",
                  ".png"),
           group_map, width=10, unit="cm")
  }
}

for (group_name in c("Las Águilas Negras", "Los Pelusos", "Los Puntilleros", "Los Rastrojos")) {
  if (sum(armed_groups$y2016[, grep(group_name, names(armed_groups$y2016))], na.rm=T) == 0) next
  group_coord <- armed_groups$y2016[, c(1, grep(group_name, names(armed_groups$y2016)))] %>% 
    right_join(map_df, by="id")
  names(group_coord)[2] <- "presence"
  group_coord$presence <- ifelse(is.na(group_coord$presence), group_coord$presence, 1)
  group_map <- ggplot(group_coord) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = presence),
                 color = "black",
                 linewidth = 0.1) +
    labs(fill = "", x="", y="", title=paste(group_name, 2016)) +
    theme_bw() +
    theme(legend.position ="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Colombia Data/Figs/Armed groups maps/2016/",
                stri_trans_general(group_name, "Latin-ASCII"),
                " (2016)",
                ".png"),
         group_map, width=10, unit="cm")
}


for (year_ in years) {
  labs_PPI_coord <- left_join(map_df,
                                    labs_PPI_reg_data %>%
                                      filter(year == year_) %>% 
                                      select(id, n_labs),
                                    by="id")
  labs_PPI_map <- ggplot(labs_PPI_coord) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = n_labs),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) + 
    scale_fill_viridis_c(na.value="white") +
    labs(fill = "# of PPI Labs", x="", y="", title=year_) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Colombia Data/Figs/Lab maps/Number of PPI Labs (", year_, ")", ".png"), labs_PPI_map, scale=1)
}

for (year_ in years) {
  labs_HCl_coord <- left_join(map_df,
                              labs_HCl_reg_data %>%
                                filter(year == year_) %>% 
                                select(id, n_labs),
                              by="id")
  labs_HCl_map <- ggplot(labs_HCl_coord) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = n_labs),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = map_df$long, y = map_df$lat) + 
    scale_fill_viridis_c(na.value="white") +
    labs(fill = "# of HCl Labs", x="", y="", title=year_) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Colombia Data/Figs/Lab maps/Number of HCl Labs (", year_, ")", ".png"), labs_HCl_map, scale=1)
}
