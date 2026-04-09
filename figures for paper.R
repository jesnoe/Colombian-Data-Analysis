indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual", "FARC", "ELN")
max_bwd <- 4
# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)
library(caret)
# library(randomForest)
library(pracma)
library(GWmodel)
library(pROC)
library(glmnet)
library(reshape2)
library(regclass)
library(logistf)

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
  map_df <- left_join(map_df, municipios_capital %>% unique, by="id")
  
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id, municipio, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  
  airports <- read.csv("Colombia Data/airports.csv") %>% as_tibble
  ferry <- read.csv("Colombia Data/ferry terminals.csv") %>% as_tibble
  police <- read.csv("Colombia Data/polices.csv") %>% as_tibble
  military <- read.csv("Colombia Data/military.csv") %>% as_tibble
  ferry$n_police <- police$n_polices
  ferry$n_military <- military$n_military
  
  n_drop_ <- 10
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  depto_map <- suppressMessages(fortify(departamentos)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(id != 88) %>% 
    left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
  
  
  violence_AAMM <- read.csv("Colombia Data/violence with id (AAMM).csv") %>% as_tibble
  conflict <- read.csv("Colombia Data/Conflict events.csv") %>% as_tibble
  violence_combined <- bind_rows(violence_AAMM %>% select(id, year, FARC, ELN, AUC) %>%
                                   mutate(FARC = ifelse(FARC == "yes", 1 , 0),
                                          ELN = ifelse(ELN == "yes", 1 , 0),
                                          AUC = ifelse(AUC == "yes", 1 , 0)),
                                 conflict %>% mutate(FARC = ifelse(grepl("FARC", dyad_name), 1, 0)) %>% select(id, year, FARC)) %>% 
    group_by(id, year) %>%
    summarize(FARC = ifelse(any(FARC == 1), 1, 0),
              ELN = ifelse(any(ELN == 1), 1, 0),
              AUC = ifelse(any(AUC == 1), 1, 0)) %>% ungroup
  
  regression_data_CF_2013 <- read.csv("Colombia Data/regression data all municipios CF 2013.csv") %>% as_tibble
  regression_data_CF_2014 <- read.csv("Colombia Data/regression data all municipios CF 2014.csv") %>% as_tibble
  regression_data_CF_2016 <- read.csv("Colombia Data/regression data all municipios CF 2016.csv") %>% as_tibble
  regression_data_CF_2017 <- read.csv("Colombia Data/regression data all municipios CF 2017.csv") %>% as_tibble
  
  regression_data_CF_2013 <- regression_data_CF_2013 %>% left_join(violence_combined %>% filter(year == 2013) %>% select(-year), by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  regression_data_CF_2014 <- regression_data_CF_2014 %>% left_join(violence_combined %>% filter(year == 2014) %>% select(-year), by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  regression_data_CF_2016 <- regression_data_CF_2016 %>% left_join(violence_combined %>% filter(year == 2016) %>% select(-year), by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  regression_data_CF_2017 <- regression_data_CF_2017 %>% left_join(violence_combined %>% filter(year == 2017) %>% select(-year), by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  
  coord_unique <- left_join(regression_data_CF_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  # PML_gwr_coefs_AUC_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_AUC all var drop 10 2016 data CF (03-24-2026).csv") %>% as_tibble
}

ROC_pred <- function(GWR_pred) {
  result <- roc(GWR_pred$y, GWR_pred$pi_hat, positive = "1", quiet = T)
  return(result)
}

neighbor_id <- function(id_i, bw_i, scale_11_, gwr_data_) {
  if (scale_11_) gwr_data_id <- gwr_data_$scale_11
  else gwr_data_id <- gwr_data_$norm
  
  coord_unique_ <- gwr_data_$coord
  local_gwr_dist_ <- gwr_data_$dist %>% as.matrix
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)])
  return(result)
}

regression_data_CF_2013$hyd_seizures <- round(regression_data_CF_2013$hyd_seizures, 2)
regression_data_CF_2014$hyd_seizures <- round(regression_data_CF_2014$hyd_seizures, 2)
regression_data_CF_2016$hyd_seizures <- round(regression_data_CF_2016$hyd_seizures, 2)
regression_data_CF_2017$hyd_seizures <- round(regression_data_CF_2017$hyd_seizures, 2)

PML_gwr_coefs_AUC_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2016 data no price CF (03-24-2026).csv") %>% as_tibble
PML_gwr_coefs_AUC_CF_2017 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2017 data no price CF (03-24-2026).csv") %>% as_tibble

major_cities <- regression_data_CF_2016 %>% arrange(desc(population)) %>% select(id, population) %>% left_join(municipio_centroid, by='id') %>% head(5)

dep_var <- "hyd_destination"
coef_table <- PML_gwr_coefs_AUC_CF_2016
for (i in 4:length(coef_table)) {
  var_name <- names(coef_table)[i]
  
  gwr_coefs_i <- data.frame(id=coef_table$id,
                            coef=coef_table[[var_name]],
                            rounded_coef=coef_table[[var_name]] %>% round(3))
  min_coef <- min(gwr_coefs_i$coef, na.rm=T)
  max_coef <- max(gwr_coefs_i$coef, na.rm=T)
  
  coef_map_coords <- map_df %>%
    left_join(gwr_coefs_i, by="id")
  
  gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) +
    geom_polygon(aes(group=group, fill=coef),
                 color = "grey60",
                 linewidth = 0.1) +
    geom_text(data=major_cities, aes(x=long, y=lat, label=municipio), color="black", size=5) +
    expand_limits(x = depto_map$long, y = depto_map$lat) +
    coord_quickmap() +
    scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                         values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                         na.value = "white") +
    labs(fill=var_name, x="", y="", title=dep_var) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  
  # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%i) - etc/local GWR PML coef by AUC violence_etc %s %s all var drop n_drop=%i %i data %s CF (%s).png",
  #                dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price, date_),
  #        gwr_coef_map, scale=1)
}

