# setwd("/Users/R")
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
library(randomForest)
library(pracma)
library(GWmodel)
library(pROC)
library(glmnet)
library(reshape2)
library(regclass)
library(logistf)
########## used bandwidth range 0.5~3.0 due to the time limit
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
  
  nonzero_seizure <- read.csv("Colombia Data/nonzero_seizure.csv") %>% as_tibble
  nonzero_coca_area <- read.csv("Colombia Data/nonzero_coca_area.csv") %>% as_tibble
  n_price <- read.csv("Colombia Data/n_price.csv") %>% as_tibble
  n_river_length <- read.csv("Colombia Data/n_river_length.csv") %>% as_tibble
  n_road_length <- read.csv("Colombia Data/n_road_length.csv") %>% as_tibble
  n_lab_prob <- read.csv("Colombia Data/n_lab_prob.csv") %>% as_tibble
  n_airport <- read.csv("Colombia Data/n_airport.csv") %>% as_tibble
  n_armed_group <- read.csv("Colombia Data/n_armed_group.csv") %>% as_tibble
  n_ferry <- read.csv("Colombia Data/n_ferry.csv") %>% as_tibble
  n_police <- read.csv("Colombia Data/n_police.csv") %>% as_tibble
  n_military <- read.csv("Colombia Data/n_military.csv") %>% as_tibble
}

minmax_scale <- function(vec) {
  return(vec/(max(vec) - min(vec)))
}

ever_regression_data_years_price_pred <- function(dep_var) { 
  # coca area set to zero for unobserved locations. Therefore, coca_distance is not needed
  # prices are now predicted. price_distance is not needed
  regression_data_years_price_pred_dep_var <- regression_data_years_price_pred
  names(regression_data_years_price_pred_dep_var)[which(names(regression_data_years_price_pred) == dep_var)] <- "y"
  if (grepl("base", dep_var)) {
    regression_data_years_price_pred_dep_var <- regression_data_years_price_pred_dep_var %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_PPI_labs = ifelse(sum(n_PPI_labs) > 0, 1, 0),
                price_avg = median(base_avg),
                # price_distance = min(base_price_distance),
                coca_area = max(coca_area),
                # coca_distance = min(coca_distance),
                seizures = sum(base_seizures),
                n_armed_groups = sum(n_armed_groups),
                river_length=river_length[1],
                road_length=road_length[1],
                population = population[1],
                airport = airport[1]) %>% 
      mutate(price_avg=scale(price_avg)[,1],
             population=scale(population)[,1],
             seizures = scale(seizures)[,1],
             armed_group = ifelse(n_armed_groups > 0, 1, 0))
    regression_data_years_price_pred_dep_var$lab_prob <- glm(n_PPI_labs~., data = regression_data_years_price_pred_dep_var %>% select(-id, -y), family=binomial)$fitted
    gwr_data_coord <- left_join(regression_data_years_price_pred_dep_var %>%
                                  select(-n_armed_groups, -n_PPI_labs),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }else if (grepl("hyd", dep_var)) {
    regression_data_years_price_pred_dep_var <- regression_data_years_price_pred_dep_var %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_hyd_labs = ifelse(sum(n_hyd_labs) > 0, 1, 0),
                price_avg = median(hyd_avg),
                # price_distance = min(hyd_price_distance),
                coca_area = max(coca_area),
                # coca_distance = min(coca_distance),
                seizures = sum(hyd_seizures),
                n_armed_groups = sum(n_armed_groups),
                river_length=river_length[1],
                road_length=road_length[1],
                population = population[1],
                airport = airport[1]) %>% 
      mutate(price_avg=scale(price_avg)[,1],
             population=scale(population)[,1],
             seizures = scale(seizures)[,1],
             armed_group = ifelse(n_armed_groups > 0, 1, 0))
    regression_data_years_price_pred_dep_var$lab_prob <- glm(n_hyd_labs~., data = regression_data_years_price_pred_dep_var %>% select(-id, -y), family=binomial)$fitted
    gwr_data_coord <- left_join(regression_data_years_price_pred_dep_var %>%
                                  select(-n_armed_groups, -n_hyd_labs),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }
  
  
  ### use regression_data_years_price_pred to make hyd_destination is 1 if a municipio was destination in at least 2 years
  
  coord_unique <- gwr_data_coord %>% select(id, long, lat) %>% unique
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  gwr_data_dep_var <- gwr_data_coord %>%
    select(-long, -lat) %>% 
    mutate(y = as.factor(y))
  
  municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
  municipios_sf$area_km2 <- st_area(municipios_sf) %>% units::set_units("km^2") %>% as.numeric
  gwr_data_dep_var_by_area <- gwr_data_dep_var %>% 
    left_join(municipios_sf %>% as_tibble %>% select(id, area_km2), by="id") %>% 
    mutate(coca_area = scale(coca_area / area_km2)[,1],
           river_length = scale(river_length / area_km2)[,1],
           road_length = scale(road_length / area_km2)[,1]) %>% 
    select(-area_km2)
  gwr_data_dep_var_by_area_11_scale <- gwr_data_dep_var_by_area %>% 
    mutate(across(price_avg:population, ~ scales::rescale(.x, c(-1,1))))
  
  ferry <- ferry %>%
    mutate(ferry=ifelse(n_ferry > 0, 1, 0),
           police=ifelse(n_police > 0, 1, 0),
           military=ifelse(n_military > 0, 1, 0))
  gwr_data_dep_var_by_area <- left_join(gwr_data_dep_var_by_area, ferry %>% select(id, ferry:military), by="id")
  
  gwr_data_dep_var_by_area_11_scale <- left_join(gwr_data_dep_var_by_area_11_scale, ferry %>% select(id, ferry:military), by="id")
  return(list(norm=gwr_data_dep_var_by_area, 
              scale_11=gwr_data_dep_var_by_area_11_scale,
              dist=gwr_data_dist,
              coord=coord_unique))
}

regression_data_each_year_price_pred <- function(dep_var, year_) {
  # coca area set to zero for unobserved locations. Therefore, coca_distance is not needed
  # prices are now predicted. price_distance is not needed
  regression_data_years_price_pred_dep_var <- regression_data_years_price_pred %>% filter(year == year_)
  names(regression_data_years_price_pred_dep_var)[which(names(regression_data_years_price_pred) == dep_var)] <- "y"
  if (grepl("base", dep_var)) {
    regression_data_years_price_pred_dep_var <- regression_data_years_price_pred_dep_var %>% 
      rename(lab_prob = PPI_lab_prob,
             price_avg = base_avg,
             seizures = base_seizures) %>% 
      select(id, lab_prob, coca_area, price_avg, river_length, road_length, seizures, n_armed_groups, population, y, airport) %>% 
      mutate(price_avg=scale(price_avg)[,1],
             population=scale(population)[,1],
             seizures = scale(seizures)[,1],
             armed_group = ifelse(n_armed_groups > 0, 1, 0))
    gwr_data_coord <- left_join(regression_data_years_price_pred_dep_var %>%
                                  select(-n_armed_groups),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }else if (grepl("hyd", dep_var)) {
    regression_data_years_price_pred_dep_var <- regression_data_years_price_pred_dep_var %>% 
      rename(lab_prob = hyd_lab_prob,
             price_avg = hyd_avg,
             seizures = hyd_seizures) %>% 
      select(id, lab_prob, coca_area, price_avg, river_length, road_length, seizures, n_armed_groups, population, y, airport) %>% 
      mutate(price_avg=scale(price_avg)[,1],
             population=scale(population)[,1],
             seizures = scale(seizures)[,1],
             armed_group = ifelse(n_armed_groups > 0, 1, 0))
    gwr_data_coord <- left_join(regression_data_years_price_pred_dep_var %>%
                                  select(-n_armed_groups),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }
  
  
  ### use regression_data_years_price_pred to make hyd_destination is 1 if a municipio was destination in at least 2 years
  
  coord_unique <- gwr_data_coord %>% select(id, long, lat) %>% unique
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  gwr_data_dep_var <- gwr_data_coord %>%
    select(-long, -lat) %>% 
    mutate(y = as.factor(y))
  
  municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
  municipios_sf$area_km2 <- st_area(municipios_sf) %>% units::set_units("km^2") %>% as.numeric
  gwr_data_dep_var_by_area <- gwr_data_dep_var %>% 
    left_join(municipios_sf %>% as_tibble %>% select(id, area_km2), by="id") %>% 
    mutate(coca_area = scale(coca_area / area_km2)[,1],
           river_length = scale(river_length / area_km2)[,1],
           road_length = scale(road_length / area_km2)[,1]) %>% 
    select(-area_km2)
  gwr_data_dep_var_by_area_11_scale <- gwr_data_dep_var_by_area %>% 
    mutate(across(price_avg:population, ~ scales::rescale(.x, c(-1,1))))
  
  ferry <- ferry %>%
    mutate(ferry=ifelse(n_ferry > 0, 1, 0),
           police=ifelse(n_police > 0, 1, 0),
           military=ifelse(n_military > 0, 1, 0))
  gwr_data_dep_var_by_area <- left_join(gwr_data_dep_var_by_area, ferry %>% select(id, ferry:military), by="id")
  
  gwr_data_dep_var_by_area_11_scale <- left_join(gwr_data_dep_var_by_area_11_scale, ferry %>% select(id, ferry:military), by="id")
  return(list(norm=gwr_data_dep_var_by_area, 
              scale_11=gwr_data_dep_var_by_area_11_scale,
              dist=gwr_data_dist,
              coord=coord_unique))
}


neighbor_id <- function(id_i, bw_i, scale_11_, coord_unique_, local_gwr_dist_, gwr_data_=gwr_data) {
  if (scale_11_) gwr_data_id <- gwr_data_$scale_11
  else gwr_data_id <- gwr_data_$norm
  
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}

cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_dest cv min dev (03-07-2025).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv") %>% as_tibble
local_gwr_lasso_coefs_hyd_destination <- read.csv("Colombia Data/local GWR lasso result/local GWR lasso coefs hyd_destination (03-11-2025).csv") %>% as_tibble %>% arrange(id)
# load("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price (03-28-2025).RData")

regression_data_years <- read.csv("Colombia Data/regression data all municipios ever lab (02-05-2025).csv") %>% as_tibble %>% 
  mutate(coca_area = ifelse(coca_distance > 0, 0, coca_area))
{
  reg_data <- regression_data_years %>% 
    filter(hyd_avg > 100000) %>%
    select(id, hyd_avg, hyd_lab_prob, coca_area, hyd_price_distance, hyd_seizures, n_armed_groups, river_length, road_length, population, airport)
  reg_data <- left_join(reg_data, ferry %>%
                          rename(ferry=n_ferry,
                                 police=n_police,
                                 military=n_military) %>% 
                          mutate(ferry=ifelse(ferry > 0, 1, 0),
                                 police=ifelse(police > 0, 1, 0),
                                 military=ifelse(military > 0, 1, 0)),
                        by="id")
  hyd_avg_lm <- lm(hyd_avg~., reg_data %>% filter(hyd_price_distance == 0) %>% select(-id, -hyd_price_distance))
  hyd_avg_pred <- predict(hyd_avg_lm, reg_data %>% select(-hyd_price_distance))
  regression_data_years_price_pred <- regression_data_years
  regression_data_years_price_pred$hyd_avg <- ifelse(regression_data_years$hyd_price_distance == 0, regression_data_years$hyd_avg, hyd_avg_pred)
  
  reg_data <- regression_data_years %>% 
    filter(base_avg > 100000) %>%
    select(id, base_avg, PPI_lab_prob, coca_area, base_price_distance, base_seizures, n_armed_groups, river_length, road_length, population, airport)
  reg_data <- left_join(reg_data, ferry %>%
                          rename(ferry=n_ferry,
                                 police=n_police,
                                 military=n_military) %>% 
                          mutate(ferry=ifelse(ferry > 0, 1, 0),
                                 police=ifelse(police > 0, 1, 0),
                                 military=ifelse(military > 0, 1, 0)),
                        by="id")
  base_avg_lm <- lm(base_avg~., reg_data %>% filter(base_price_distance == 0) %>% select(-id, -base_price_distance))
  base_avg_pred <- predict(base_avg_lm, reg_data %>% select(-base_price_distance))
  regression_data_years_price_pred <- regression_data_years
  regression_data_years_price_pred$base_avg <- ifelse(regression_data_years$base_price_distance == 0, regression_data_years$base_avg, base_avg_pred)
  
  reg_data <- regression_data_years %>% 
    filter(paste_avg > 100000) %>%
    select(id, paste_avg, PPI_lab_prob, coca_area, paste_price_distance, coca_seizures, n_armed_groups, river_length, road_length, population, airport)
  reg_data <- left_join(reg_data, ferry %>%
                          rename(ferry=n_ferry,
                                 police=n_police,
                                 military=n_military) %>% 
                          mutate(ferry=ifelse(ferry > 0, 1, 0),
                                 police=ifelse(police > 0, 1, 0),
                                 military=ifelse(military > 0, 1, 0)),
                        by="id")
  paste_avg_lm <- lm(paste_avg~., reg_data %>% filter(paste_price_distance == 0) %>% select(-id, -paste_price_distance))
  paste_avg_pred <- predict(paste_avg_lm, reg_data %>% select(-paste_price_distance))
  regression_data_years_price_pred <- regression_data_years
  regression_data_years_price_pred$paste_avg <- ifelse(regression_data_years$paste_price_distance == 0, regression_data_years$paste_avg, paste_avg_pred)
  
  regression_data_aggr <- regression_data_years %>% 
    group_by(id) %>% 
    summarize(seizures = mean(hyd_seizures, na.rm=T),
              coca_area = max(coca_area, na.rm=T))
  regression_data_aggr$seizures_log_scale <- scale(log(regression_data_aggr$seizures+1))[,1]
  regression_data_aggr$coca_area_log_scale <- scale(log(regression_data_aggr$coca_area+1))[,1]
  }

gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
bwd_range <- seq(0.5, 3, by=0.1)

min_seizure_scaled <- min(gwr_data$norm$seizures) %>% round(3)
min_coca_area_scaled <- min(gwr_data$norm$coca_area) %>% round(3)
coord_unique <- gwr_data$coord
local_gwr_dist <- gwr_data$dist %>% as.matrix
# gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
# gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
# gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]

# nonzero_seizure <- cv_aic_min_mat_
# nonzero_coca_area <- cv_aic_min_mat_
# n_price <- cv_aic_min_mat_
# n_river_length <- cv_aic_min_mat_
# n_road_length <- cv_aic_min_mat_
# n_lab_prob <- cv_aic_min_mat_
# n_airport <- cv_aic_min_mat_
# n_armed_group <- cv_aic_min_mat_
# n_ferry <- cv_aic_min_mat_
# n_police <- cv_aic_min_mat_
# n_military <- cv_aic_min_mat_
# for (i in 1:nrow(cv_aic_min_mat_)) {
#   id_i <- cv_aic_min_mat_$id[i]
#   for (bw_ij in bwd_range) {
#     col_name_ij <- paste0("bw_", bw_ij)
#     neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_=F, coord_unique, local_gwr_dist)
#     nonzero_seizure[[col_name_ij]][i] <- unique(neighbor_ij$seizures) %>% length
#     nonzero_coca_area[[col_name_ij]][i] <- sum(round(neighbor_ij$coca_area, 3) > min_coca_area_scaled)
    # nonzero_seizure[[col_name_ij]][i] <- sum(round(neighbor_ij$seizures, 3) > min_seizure_scaled)
    # nonzero_coca_area[[col_name_ij]][i] <- sum(round(neighbor_ij$coca_area, 3) > min_coca_area_scaled)
#     n_price[[col_name_ij]][i] <- table(neighbor_ij$price_avg) %>% length
#     n_river_length[[col_name_ij]][i] <- table(neighbor_ij$river_length) %>% length
#     n_road_length[[col_name_ij]][i] <- table(neighbor_ij$road_length) %>% length
#     n_lab_prob[[col_name_ij]][i] <- table(neighbor_ij$lab_prob) %>% length
#     
#     n_airport_ij <- table(neighbor_ij$airport)
#     n_armed_group_ij <- table(neighbor_ij$armed_group)
#     n_ferry_ij <- table(neighbor_ij$ferry)
#     n_police_ij <- table(neighbor_ij$police)
#     n_military_ij <- table(neighbor_ij$military)
#     
#     n_airport[[col_name_ij]][i] <- ifelse(n_airport_ij %>% length == 2, n_airport_ij %>% min, 0)
#     n_armed_group[[col_name_ij]][i] <- ifelse(n_armed_group_ij %>% length == 2, n_armed_group_ij %>% min, 0)
#     n_ferry[[col_name_ij]][i] <- ifelse(n_ferry_ij %>% length == 2, n_ferry_ij %>% min, 0)
#     n_police[[col_name_ij]][i] <- ifelse(n_police_ij %>% length == 2, n_police_ij %>% min, 0)
#     n_military[[col_name_ij]][i] <- ifelse(n_military_ij %>% length == 2, n_military_ij %>% min, 0)
#   }
# }
# nonzero_seizure %>% write.csv("Colombia Data/nonzero_seizure.csv", row.names = F)
# nonzero_coca_area %>% write.csv("Colombia Data/nonzero_coca_area.csv", row.names = F)
# n_price %>% write.csv("Colombia Data/n_price.csv", row.names = F)
# n_river_length %>% write.csv("Colombia Data/n_river_length.csv", row.names = F)
# n_road_length %>% write.csv("Colombia Data/n_road_length.csv", row.names = F)
# n_lab_prob %>% write.csv("Colombia Data/n_lab_prob.csv", row.names = F)
# n_airport %>% write.csv("Colombia Data/n_airport.csv", row.names = F)
# n_armed_group %>% write.csv("Colombia Data/n_armed_group.csv", row.names = F)
# n_ferry %>% write.csv("Colombia Data/n_ferry.csv", row.names = F)
# n_police %>% write.csv("Colombia Data/n_police.csv", row.names = F)
# n_military %>% write.csv("Colombia Data/n_military.csv", row.names = F)

bwd_range <- seq(0.5, 3, by=0.1)
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

local_gwr_lasso_coefs_hyd_destination <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso coefs hyd_destination (03-28-2025).csv") %>%
  as_tibble %>% arrange(id)
indep_vars_ <- names(local_gwr_lasso_coefs_hyd_destination)[-(1:3)]


# load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop (05-21-2025).RData") # local_GWR_coefs_PML_hyd_dest_model_drop
# local_GWR_coefs_list<-local_GWR_coefs_PML_hyd_dest_model_drop; cv_aic_min_mat<-cv_aic_min_mat_; indep_vars<-indep_vars_; dep_var="hyd_destination"; weight_=NULL; alpha<-0.1

# load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price all var drop (06-17-2025).RData") # local_GWR_coefs_PML_hyd_dest_var_drop

# local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled
# load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure scaled (05-21-2025).RData")
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure coca scaled (06-20-2025).RData")

# local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled
# load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price var drop log seizure scaled (06-05-2025).RData")
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price all var drop log seizure coca scaled (06-20-2025).RData")

## Penalized MLE comparisons
PML_gwr_coefs_F1_model_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_seizure_bw_F1 all var model drop 5 (06-17-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_model_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_log_seizure_coca_bw_F1 all var model drop 5 (06-20-2025).csv") %>% as_tibble

PML_gwr_coefs_F1_var_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_seizure_bw_F1 all var drop 5 (06-17-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_log_seizure_coca_bw_F1 all var drop 5 (06-20-2025).csv") %>% as_tibble

PML_F1_score_hyd_dest_model_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 model drop (05-21-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_var_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop (06-17-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_model_drop_log_seizure <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var model drop log seizure coca scaled (06-20-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_var_drop_log_seizure <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop log seizure scaled (06-20-2025).csv") %>% as_tibble

# armed group check
  # model drop
model_drop_n_neighbors <- PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(!is.na(bw)) %>%
  apply(1, function(x) local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled[[paste0("id_",x[1])]][[paste0("bw_",x[2])]]$model %>% nrow)
var_drop_n_neighbors <- PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(!is.na(bw)) %>%
  apply(1, function(x) local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_",x[1])]][[paste0("bw_",x[2])]]$model %>% nrow)
model_drop_n_neighbors %>% sum
var_drop_n_neighbors %>% sum

PML_gwr_coefs_F1_model_drop_log_seizure_coca$armed_group %>% abs %>% hist(main="coef. of armed_group")
PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(armed_group) > 25)
high_armed_group_id <- PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(armed_group) > 25) %>% pull(id)
PML_F1_score_hyd_dest_model_drop_log_seizure %>% filter(id %in% high_armed_group_id)
PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(armed_group) < 25) %>% print(n=15)

PML_gwr_coefs_F1_model_drop_log_seizure_coca$lab_prob %>% abs %>% hist(main="coef. of lab_prob")
PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(lab_prob) > 50)
high_lab_prob_id <- PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(lab_prob) > 50) %>% pull(id)
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_5002$bw_0.6$model

PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(armed_group) > 25)
n_airport %>% filter(id %in% high_armed_group_id)
n_armed_group %>% filter(id %in% high_armed_group_id)
n_ferry %>% filter(id %in% high_armed_group_id)
n_police %>% filter(id %in% high_armed_group_id)
n_military %>% filter(id %in% high_armed_group_id)

gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob_scaled <- scale(gwr_data$norm$lab_prob)[,1]
gwr_data$norm$lab_prob_log_scaled <- scale(log(1+gwr_data$norm$lab_prob))[,1]
id_i <- 81591; bw_i <- 1.6
id_i <- 81794; bw_i <- 1.3
id_i <- 15212; bw_i <- 0.7
id_i <- 17662; bw_i <- 0.5
id_i <- 25572; bw_i <- 0.6
id_i <- 17444; bw_i <- 0.6
id_i <- 5002; bw_i <- 0.6
id_i <- 5031; bw_i <- 1.2
data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
data_id %>% select(airport:military, -lab_prob) %>% apply(2, function(x) min(c(sum(x), length(x) - sum(x))))
data_id %>% select(y, armed_group) %>% arrange(armed_group, y) %>% print(n=nrow(data_id))
  # perfect prediction?
data.frame(id=data_id$id,
           y_=data_id$y,
           y_pred=ifelse(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled[[paste0("id_",id_i)]][[paste0("bw_",bw_i)]]$predict < 0.5, 0, 1),
           armed_group=data_id$armed_group)
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled[[paste0("id_",id_i)]][[paste0("bw_",bw_i)]] %>% summary
logistf(y~.-id-lab_prob_scaled-lab_prob_log_scaled, data_id) %>% summary
logistf(y~.-id-lab_prob-lab_prob_log_scaled, data_id) %>% summary
logistf(y~.-id-lab_prob_scaled-lab_prob, data_id) %>% summary # id=17662: -130 even for lab_prob_log_scaled, too small number of obs.?
# logistf(y~.-id, data_id %>% mutate(across(c(airport:armed_group, ferry:military), ~as.factor(.x))) %>% summary# same result
logistf(y~.-id, data_id[-5,]) %>% summary

data_id_coefs_leave_one_out <- PML_gwr_coefs_F1_model_drop_log_seizure_coca[1,-2] %>% rename(id_out=id)
for (j in 1:nrow(data_id)) {
  try_logistf <- tryCatch(
    {
      logistf_j <- logistf(y~.-id, data_id[-j,])
      coef_j <- matrix(c(data_id$id[j], coef(logistf_j)), nrow=1) %>% as_tibble
    },
    error = function(e) {
      coef_j <- matrix(c(data_id$id[j], rep(NA, 13)), nrow=1) %>% as_tibble
      return(coef_j)
    }
  )
  if (inherits(try_logistf, "error")) coef_j <- matrix(c(data_id$id[j], rep(NA, 13)), nrow=1) %>% as_tibble
  names(coef_j) <- names(data_id_coefs_leave_one_out)
  data_id_coefs_leave_one_out <- bind_rows(data_id_coefs_leave_one_out, coef_j)
}
data_id_coefs_leave_one_out <- data_id_coefs_leave_one_out[-1,]
data_id_coefs_leave_one_out %>% print(n=nrow(data_id_coefs_leave_one_out))
PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(id == id_i)
sd(data_id_coefs_leave_one_out$armed_group)

data_id_coefs_leave_one_out %>% filter(abs(armed_group) < 20)
data_id[which(abs(data_id_coefs_leave_one_out$armed_group) < 20),]
data_id[which(abs(data_id_coefs_leave_one_out$armed_group) >= 20),] %>% print(n=nrow(data_id))
### armed_group is related to lab_prob? outliers need high coefs for either armed_group or lab_prob

  # leave-one-out coef standard deviation
  # stable coef ids are same for armed_group and lab_prob with model drop
stable_armed_group_index <- which(!is.na(PML_gwr_coefs_F1_model_drop_log_seizure_coca$bw) & !(PML_gwr_coefs_F1_model_drop_log_seizure_coca$id %in% high_armed_group_id))
set.seed(100)
sample_index <- c(sample(stable_armed_group_index, 20, replace = F), which(PML_gwr_coefs_F1_model_drop_log_seizure_coca$id %in% high_armed_group_id))
model_drop_leave_one_out_armed_group_sd <- c()
model_drop_leave_one_out_lab_prob_sd <- c()
for (i in sample_index) {
  id_i <- PML_gwr_coefs_F1_model_drop_log_seizure_coca$id[i]
  bw_i <- PML_gwr_coefs_F1_model_drop_log_seizure_coca$bw[i]
  
  if (is.na(bw_i)) {
    model_drop_leave_one_out_armed_group_sd <- c(model_drop_leave_one_out_armed_group_sd, NA)
    model_drop_leave_one_out_lab_prob_sd <- c(model_drop_leave_one_out_lab_prob_sd, NA)
    next
  }
  
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  
  data_id_coefs_leave_one_out_i <- PML_gwr_coefs_F1_model_drop_log_seizure_coca[1,-2] %>% rename(id_out=id)
  for (j in 1:nrow(data_id)) {
    try_logistf <- tryCatch(
      {
        logistf_j <- logistf(y~.-id, data_id[-j,])
        coef_j <- matrix(c(data_id$id[j], coef(logistf_j)), nrow=1) %>% as_tibble
      },
      error = function(e) {
        return(e)
      }
    )
    if (inherits(try_logistf, "error")) coef_j <- matrix(c(data_id$id[j], rep(NA, 13)), nrow=1) %>% as_tibble
    
    names(coef_j) <- names(data_id_coefs_leave_one_out_i)
    data_id_coefs_leave_one_out_i <- bind_rows(data_id_coefs_leave_one_out_i, coef_j)
  }
  data_id_coefs_leave_one_out_i <- data_id_coefs_leave_one_out_i[-1,]
  
  model_drop_leave_one_out_armed_group_sd <- c(model_drop_leave_one_out_armed_group_sd, sd(data_id_coefs_leave_one_out_i$armed_group, na.rm = T))
  model_drop_leave_one_out_lab_prob_sd <- c(model_drop_leave_one_out_lab_prob_sd, sd(data_id_coefs_leave_one_out_i$lab_prob, na.rm = T))
}
model_drop_leave_one_out_coef_sd <- tibble(id = PML_gwr_coefs_F1_model_drop_log_seizure_coca$id[sample_index],
                                           extreme = rep(0, length(sample_index)))
model_drop_leave_one_out_coef_sd$extreme[rev(1:length(sample_index))[1:length(high_armed_group_id)]] <- 1
model_drop_leave_one_out_coef_sd$armed_group_sd <- model_drop_leave_one_out_armed_group_sd
model_drop_leave_one_out_coef_sd$lab_prob_sd <- model_drop_leave_one_out_lab_prob_sd
model_drop_leave_one_out_coef_sd %>% print(n=nrow(model_drop_leave_one_out_coef_sd))
# write.csv(model_drop_leave_one_out_coef_sd, "Colombia Data/local GWR PML result predicted prices/model_drop_leave_one_out_coef_sd.csv", row.names = F)

  # var drop
PML_gwr_coefs_F1_var_drop_log_seizure_coca$armed_group %>% abs %>% hist
PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(abs(armed_group) > 40) %>% print(n=23)
high_armed_group_id_var <- PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(abs(armed_group) > 40) %>% pull(id)
PML_gwr_coefs_F1_var_drop_log_seizure_coca$lab_prob %>% abs %>% hist
PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(abs(lab_prob) > 60) %>% print(n=31)
high_lab_prob_id_var <- PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(abs(lab_prob) > 60) %>% pull(id)

data_id %>% filter(y == 1 & ferry == 0) %>% print(n=nrow(data_id))
logistf(y~.-id, data_id[which(!(data_id$id %in% c(81220))),]) %>% summary

stable_index_var <- with(PML_gwr_coefs_F1_var_drop_log_seizure_coca,
                         which(!is.na(bw) & !is.na(armed_group) & !is.na(lab_prob) & !(id %in% high_armed_group_id_var) & !(id %in% high_lab_prob_id_var)))

set.seed(100)
sample_index <- c(sample(stable_index_var, 20, replace = F),
                  with(PML_gwr_coefs_F1_var_drop_log_seizure_coca, which((id %in% high_armed_group_id_var) | (id %in% high_lab_prob_id_var))))
var_drop_leave_one_out_coef_sd <- tibble(id = PML_gwr_coefs_F1_var_drop_log_seizure_coca$id[sample_index],
                                         extreme_armed_group = ifelse(id %in% high_armed_group_id_var, 1, 0),
                                         extreme_lab_group = ifelse(id %in% high_lab_prob_id_var, 1, 0))
coef_tbl <- tibble(var_name=names(PML_gwr_coefs_F1_var_drop_log_seizure_coca)[-(1:3)])
var_drop_leave_one_out_armed_group_sd <- c()
var_drop_leave_one_out_lab_prob_sd <- c(); k <- 1
for (i in sample_index) {
  id_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca$id[i]
  bw_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca$bw[i]
  
  if (is.na(bw_i)) {
    var_drop_leave_one_out_armed_group_sd <- c(var_drop_leave_one_out_armed_group_sd, NA)
    var_drop_leave_one_out_lab_prob_sd <- c(var_drop_leave_one_out_lab_prob_sd, NA)
    next
  }
  
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  if (any(PML_gwr_coefs_F1_var_drop_log_seizure_coca[i,-(1:3)] %>% is.na)) {
    drop_index <- which(PML_gwr_coefs_F1_var_drop_log_seizure_coca[i,-(1:3)] %>% is.na)
    reg_data_id <- data_id[-j, -(drop_index + 2)]
  }else{
    reg_data_id <- data_id
  }
  
  data_id_coefs_leave_one_out_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca[1,-(2:3)] %>% rename(id_out=id)
  for (j in 1:nrow(data_id)) {
    try_logistf <- tryCatch(
      {
        logistf_j <- logistf(y~.-id, reg_data_id[-j,])
        coef_j <- coef(logistf_j)[-1]
        coef_tbl_j <- tibble(var_name=names(coef_j), coef=coef_j)
        coef_tbl_j <- left_join(coef_tbl, coef_tbl_j, by="var_name")
        coef_j <- matrix(c(data_id$id[j], coef_tbl_j$coef), nrow=1) %>% as_tibble
      },
      error = function(e) {
        return(e)
      }
    )
    if (inherits(try_logistf, "error")) coef_j <- matrix(c(data_id$id[j], rep(NA, 12)), nrow=1) %>% as_tibble
    
    names(coef_j) <- names(data_id_coefs_leave_one_out_i)
    data_id_coefs_leave_one_out_i <- bind_rows(data_id_coefs_leave_one_out_i, coef_j)
  }
  data_id_coefs_leave_one_out_i <- data_id_coefs_leave_one_out_i[-1,]
  
  var_drop_leave_one_out_armed_group_sd <- c(var_drop_leave_one_out_armed_group_sd, sd(data_id_coefs_leave_one_out_i$armed_group, na.rm = T))
  var_drop_leave_one_out_lab_prob_sd <- c(var_drop_leave_one_out_lab_prob_sd, sd(data_id_coefs_leave_one_out_i$lab_prob, na.rm = T))
  
  print(paste(k, "completed"))
  k <- k + 1
}

var_drop_leave_one_out_coef_sd$armed_group_sd <- var_drop_leave_one_out_armed_group_sd
var_drop_leave_one_out_coef_sd$lab_prob_sd <- var_drop_leave_one_out_lab_prob_sd
var_drop_leave_one_out_coef_sd <- read.csv("Colombia Data/local GWR PML result predicted prices/var_drop_leave_one_out_coef_sd.csv") %>% as_tibble
var_drop_leave_one_out_coef_sd %>% print(n=nrow(var_drop_leave_one_out_coef_sd))
# write.csv(var_drop_leave_one_out_coef_sd, "Colombia Data/local GWR PML result predicted prices/var_drop_leave_one_out_coef_sd.csv", row.names = F)

PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(id %in% c(68162, 73347, 68524, 73030))
id_i <- 68162; bw_i <- 0.7
id_i <- 73347; bw_i <- 0.5
id_i <- 68524; bw_i <- 0.6
id_i <- 73030; bw_i <- 0.6
data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
if (any(PML_gwr_coefs_F1_var_drop_log_seizure_coca[i,-(1:3)] %>% is.na)) {
  drop_index <- which(PML_gwr_coefs_F1_var_drop_log_seizure_coca[i,-(1:3)] %>% is.na)
  reg_data_id <- data_id[-j, -(drop_index + 2)]
}else{
  reg_data_id <- data_id
}

coef_tbl <- tibble(var_name=names(PML_gwr_coefs_F1_var_drop_log_seizure_coca)[-(1:3)])
data_id_coefs_leave_one_out <- PML_gwr_coefs_F1_var_drop_log_seizure_coca[1,-(2:3)] %>% rename(id_out=id)
for (j in 1:nrow(data_id)) {
  try_logistf <- tryCatch(
    {
      logistf_j <- logistf(y~.-id, reg_data_id[-j,])
      coef_j <- coef(logistf_j)[-1]
      coef_tbl_j <- tibble(var_name=names(coef_j), coef=coef_j)
      coef_tbl_j <- left_join(coef_tbl, coef_tbl_j, by="var_name")
      coef_j <- matrix(c(data_id$id[j], coef_tbl_j$coef), nrow=1) %>% as_tibble
    },
    error = function(e) {
      return(e)
    }
  )
  if (inherits(try_logistf, "error")) coef_j <- matrix(c(data_id$id[j], rep(NA, 12)), nrow=1) %>% as_tibble
  names(coef_j) <- names(data_id_coefs_leave_one_out)
  data_id_coefs_leave_one_out <- bind_rows(data_id_coefs_leave_one_out, coef_j)
}
data_id_coefs_leave_one_out <- data_id_coefs_leave_one_out[-1,]
data_id_coefs_leave_one_out %>% print(n=nrow(data_id_coefs_leave_one_out))
data_id %>% print(n=nrow(data_id))

  # check neighbors' best bandwidth data
id_4_neighbors <- coord_unique$id[which(rank(local_gwr_dist[coord_unique$id == id_i,]) < 6)]
data_4_neighbors_list <- list()
for (j in 1:5) {
  id_j <- id_4_neighbors[j]
  if (id_j == id_i) next
  data_4_neighbors_list[[paste0("id_", id_j)]] <- neighbor_id(id_j, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
}
data_4_neighbors_list
logistf(y~.-id, data_4_neighbors_list[[1]]) %>% summary
logistf(y~.-id, data_4_neighbors_list[[2]]) %>% summary
logistf(y~.-id, data_4_neighbors_list[[3]]) %>% summary
logistf(y~.-id, data_4_neighbors_list[[4]]) %>% summary

data_4_neighbors_list[[3]] %>% filter(id %in% data_id$id) %>% as.data.frame
data_4_neighbors_list[[3]] %>% filter(!(id %in% data_id$id)) %>% as.data.frame

logistf(y~.-id, data_4_neighbors_list[[3]] %>% filter(id %in% data_id$id)) %>% summary
logistf(y~.-id, bind_rows(data_4_neighbors_list[[3]] %>% filter(id %in% data_id$id), (data_4_neighbors_list[[3]] %>% filter(!(id %in% data_id$id)))[1,])) %>% summary

n_binary_id <- (data_id %>% select(id, airport:military) %>% mutate(n_obs=0) %>% relocate(id, n_obs, lab_prob))[1,]
for (id_k in high_armed_group_id) { # minimum number of pos and neg observations and number of unique lab_prob values
  bw_k <- PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(id == id_k) %>% pull(bw)
  data_k <- local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled[[paste0("id_",id_k)]][[paste0("bw_",bw_k)]]$model %>% select(airport:military)
  n_binary_k <- data_k %>% select(-lab_prob) %>% apply(2, function(x) table(x) %>% min)
  n_binary_k <- c(id_k, nrow(data_k), data_k$lab_prob %>% table %>% length, n_binary_k)
  n_binary_k <- matrix(n_binary_k, nrow=1) %>% as_tibble; names(n_binary_k) <- names(n_binary_id)
  n_binary_id <- bind_rows(n_binary_id, n_binary_k)
}
n_binary_id <- n_binary_id[-1,]
n_binary_id


# lab_prob check
PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(lab_prob) > 50)
high_lab_prob_id <- PML_gwr_coefs_F1_model_drop_log_seizure_coca %>% filter(abs(lab_prob) > 50) %>% pull(id)
PML_F1_score_hyd_dest_model_drop_log_seizure %>% filter(id %in% high_lab_prob_id)

PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(abs(lab_prob) > 50) %>% print(n=35)
high_lab_prob_id_var <- PML_gwr_coefs_F1_var_drop_log_seizure_coca %>% filter(abs(lab_prob) > 50) %>% pull(id)
PML_F1_score_hyd_dest_var_drop_log_seizure %>% filter(id %in% high_lab_prob_id_var)

gwr_data$norm <- left_join(gwr_data$norm, regression_data_aggr %>% select(-seizures, -coca_area), by="id")
data_id <- neighbor_id(17662, 0.5, scale_11_=F, coord_unique, local_gwr_dist)
data_id %>% select(y, armed_group) %>% arrange(armed_group, y) %>% print(n=23)
logistf(y~., data_id %>% select(-id, -seizures, -coca_area))

# all cases are the first non_NA bandwidths
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_81591$bw_1.6$model$armed_group %>% sum # 5
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_81794$bw_1.3$model$armed_group %>% sum # 6
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_15212$bw_0.7$model$armed_group %>% sum # 4
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_17662$bw_0.5$model$armed_group %>% sum # 4
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_25572$bw_0.6$model$armed_group %>% sum # 8
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_17444$bw_0.6$model$armed_group %>% sum # 7

local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_81591$bw_1.7
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_81794$bw_1.4
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_15212$bw_0.8
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_17662$bw_0.6
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_25572$bw_0.7
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_17444$bw_0.7
