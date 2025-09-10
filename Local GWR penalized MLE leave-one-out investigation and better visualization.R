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
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
bwd_range <- seq(0.5, 3, by=0.1)

min_seizure_scaled <- min(gwr_data$norm$seizures) %>% round(3)
min_coca_area_scaled <- min(gwr_data$norm$coca_area) %>% round(3)
coord_unique <- gwr_data$coord
local_gwr_dist <- gwr_data$dist %>% as.matrix

# local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out all var drop log seizure coca scaled n_drop=10 (08-20-2025).RData")
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled_loo <- local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled
rm(local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled)
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price all var drop log seizure coca scaled n_drop=10 (07-07-2025).RData")

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_log_seizure_coca_bw_F1 all var drop 10 (07-07-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (08-20-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_var_drop_log_seizure_10 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop log seizure scaled n_drop=10 (07-07-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out F1 all var drop log seizure scaled n_drop=10 (08-20-2025).csv") %>% as_tibble
sum(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10 != PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo, na.rm = T)

PML_GWR_pred_loo <- function(var_drop_coef, var_drop_F1, var_drop_GWR) {
  PML_best_F1 <- var_drop_coef %>% select(id, bw) %>% rename(bw_var_drop = bw)
  PML_best_F1$var_drop_F1 <- var_drop_F1[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, max(x, na.rm=T)))) %>% unlist
  
  PML_gwr_pi_hat_var_drop <- c()
  for (i in 1:nrow(var_drop_coef)) {
    id_i <- var_drop_coef$id[i]
    bw_i <- var_drop_coef$bw[i]
    neighbor_i <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
    if (is.na(bw_i)) {PML_gwr_pi_hat_var_drop <- c(PML_gwr_pi_hat_var_drop, 0)}
    else {
      model_i <- var_drop_GWR[[i]][[paste0("bw_", bw_i)]]
      model_vars_i <- (model_i$coefficients %>% names)[-1]
      var_names_i <- names(neighbor_i)[names(neighbor_i) %in% model_vars_i]
      data_pred_i <- neighbor_i %>% filter(id == id_i) %>% select(all_of(var_names_i)) %>% relocate(model_vars_i)
      pi_hat_i <- predict(model_i, data_pred_i, type="response")
      PML_gwr_pi_hat_var_drop <- c(PML_gwr_pi_hat_var_drop, pi_hat_i)
    }
  }
  
  gwr_PML_pi_hat <- tibble(id = var_drop_coef$id,
                           PML_gwr_pi_hat_var_drop_loo = PML_gwr_pi_hat_var_drop)
  
  gwr_PML_data_norm <- PML_best_F1 %>% 
    left_join(gwr_data$norm %>% select(id, y), by = "id") %>% 
    left_join(gwr_PML_pi_hat, by = "id") %>% 
    mutate(y_PML_var_drop_loo = ifelse(PML_gwr_pi_hat_var_drop_loo < 0.5, 0, 1)) %>% 
    relocate(id, y, y_PML_var_drop_loo)
  
  return(gwr_PML_data_norm)
}

# PML_GWR_pred_10_loo <-  PML_GWR_pred_loo(var_drop_coef=PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo,
#                                          var_drop_F1=PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo,
#                                          var_drop_GWR=local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled_loo)
# write.csv(PML_GWR_pred_10_loo, "Colombia Data/local GWR PML result predicted prices/GWR PML predictions leave-one-out n_drop=10.csv", row.names=F)
PML_GWR_pred_10 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML predictions n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML predictions leave-one-out n_drop=10.csv") %>% as_tibble

PML_GWR_pred_10 <- left_join(PML_GWR_pred_10, PML_GWR_pred_10_loo %>% select(id, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop_loo), by="id")
PML_GWR_pred_10 %>% filter(y_PML_var_drop != y_PML_var_drop_loo) %>% select(id, y, y_PML_var_drop, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop, PML_gwr_pi_hat_var_drop_loo)
sum(PML_GWR_pred_10$PML_gwr_pi_hat_var_drop != PML_GWR_pred_10_loo$PML_gwr_pi_hat_var_drop_loo)


CM_var_drop_10 <- confusionMatrix(PML_GWR_pred_10$y_PML_var_drop %>% as.factor, PML_GWR_pred_10$y %>% as.factor, positive = "1")
CM_var_drop_10
CM_var_drop_10$byClass

CM_var_drop_10_loo <- confusionMatrix(PML_GWR_pred_10_loo$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo$y %>% as.factor, positive = "1")
CM_var_drop_10_loo
CM_var_drop_10_loo$byClass

global_PML <- logistf(y~.-id-municipio, gwr_data$norm)
PML_GWR_pred_10$y_global_PML <- ifelse(global_PML$predict < 0.5, 0, 1) %>% as.factor
CM_global_PML <- confusionMatrix(ifelse(global_PML$predict < 0.5, 0, 1) %>% as.factor, global_PML$model$y, positive = "1")
CM_global_PML
CM_global_PML$byClass

sum(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10 != PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo, na.rm = T)

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10 %>% select(-(id:Intercept)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + ylim(-60, 100) +
  geom_boxplot(aes(x=variable, y=coefs)) -> PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_coefs_box
ggsave("Colombia Data/local GWR PML result predicted prices/coef maps/local GWR PML hyd_dest all var drop log seizure coca scaled n_drop=10 (08-20-2025).png",
       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_coefs_box, units="cm", width = 20, height = 40)

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo %>% select(-(id:Intercept)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + ylim(-60, 100) +
  geom_boxplot(aes(x=variable, y=coefs)) -> PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box
ggsave("Colombia Data/local GWR PML result predicted prices/coef maps/local GWR PML hyd_dest leave-one-out all var drop log seizure coca scaled n_drop=10 (08-20-2025).png",
       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box, units="cm", width = 20, height = 40)


id_i <- 5002; bw_i <- 0.6
data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
model_i <- local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled_loo[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
model_vars_i <- (model_i$coefficients %>% names)[-1]
var_names_i <- names(data_id)[names(data_id) %in% model_vars_i]
logistf(y~., data_id %>% select(y, var_names_i))
logistf(y~., data_id %>% filter(id != id_i) %>% select(y, var_names_i))
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo %>% filter(id == id_i)

data_pred_i <- data_id %>% filter(id == id_i) %>% select(var_names_i) %>% relocate(model_vars_i)
predict(model_i, data_pred_i, type="response")
predict(local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]], data_pred_i, type="response")
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]$predict[which(data_id$id == id_i)]


PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_diff <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10 %>% select(id)
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_diff$abs_coef_diff <- 
  apply(abs((PML_gwr_coefs_F1_var_drop_log_seizure_coca_10 %>% select(-id, -bw)) - (PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo %>% select(-id, -bw))), 1, function(x) sum(x, na.rm = T))
PML_GWR_pred_10 <- left_join(PML_GWR_pred_10, PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_diff, by="id")
PML_GWR_pred_10_y_coef_diff <- PML_GWR_pred_10 %>% select(id, y, y_PML_var_drop, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop, PML_gwr_pi_hat_var_drop_loo, abs_coef_diff)
PML_GWR_pred_10_y_coef_diff$pi_hat_dist_from_0.5 <- abs(PML_GWR_pred_10_y_coef_diff$PML_gwr_pi_hat_var_drop - 0.5)
PML_GWR_pred_10_y_coef_diff$pi_hat_dist_from_0.5_loo <- abs(PML_GWR_pred_10_y_coef_diff$PML_gwr_pi_hat_var_drop_loo - 0.5)

PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop == y_PML_var_drop_loo) %>% pull(abs_coef_diff) %>% hist
PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop != y_PML_var_drop_loo) %>% pull(abs_coef_diff) %>% hist

PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop == y_PML_var_drop_loo) %>% pull(pi_hat_dist_from_0.5) %>% hist
PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop != y_PML_var_drop_loo) %>% pull(pi_hat_dist_from_0.5) %>% hist
## most opposite predictions were made with pi_hat closer to 0.5

PML_GWR_pred_10_y_coef_diff %>% filter(abs_coef_diff > 60)
PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop != y_PML_var_drop_loo) %>% arrange(abs_coef_diff)

PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop == y_PML_var_drop_loo)
PML_GWR_pred_10_y_coef_diff %>% filter(y_PML_var_drop != y_PML_var_drop_loo)
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo


PML_GWR_pred_10_loo_y_diff <- PML_GWR_pred_10 %>% filter(y_PML_var_drop != y_PML_var_drop_loo) %>% select(id, bw_var_drop, y, y_PML_var_drop, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop, PML_gwr_pi_hat_var_drop_loo)

PML_GWR_pred_10_loo_FP <- PML_GWR_pred_10 %>% filter(y == 0, y_PML_var_drop_loo == 1) %>% select(id, y, y_PML_var_drop, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop, PML_gwr_pi_hat_var_drop_loo) %>% 
  left_join(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_diff, by = "id")
PML_GWR_pred_10_loo_FP$pi_hat_dist_from_0.5_loo <- PML_GWR_pred_10_loo_FP$PML_gwr_pi_hat_var_drop_loo - 0.5
high_potential_id <- PML_GWR_pred_10_loo_FP %>% filter(PML_gwr_pi_hat_var_drop_loo > 0.8) %>% pull(id)
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo %>% filter(id %in% high_potential_id) %>% print(n=length(high_potential_id))
gwr_data$norm %>% filter(id %in% high_potential_id) %>% print(n=length(high_potential_id))


## False Positive Investigation with focal point data results
PML_GWR_pred_10_FP <- PML_GWR_pred_10 %>% filter(y == 0 & y_PML_var_drop == 1) %>% select(id, y, y_PML_var_drop, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop, PML_gwr_pi_hat_var_drop_loo) %>% 
  left_join(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_diff, by = "id")
PML_GWR_pred_10_FP$pi_hat_dist_from_0.5 <- PML_GWR_pred_10_FP$PML_gwr_pi_hat_var_drop - 0.5

PML_GWR_pred_10_y_coef_diff %>% filter(y == 0 & y_PML_var_drop == 1) %>% pull(pi_hat_dist_from_0.5) %>% hist(main="False Positive Cases", xlab="|pi_hat - 0.5|")
PML_GWR_pred_10_y_coef_diff %>% filter(y == 1 & y_PML_var_drop == 1) %>% pull(pi_hat_dist_from_0.5) %>% hist(main="True Positive Cases", xlab="|pi_hat - 0.5|")
PML_GWR_pred_10_y_coef_diff %>% filter(y == 1 & y_PML_var_drop == 0) %>% pull(pi_hat_dist_from_0.5) %>% hist(main="False Negative Cases", xlab="|pi_hat - 0.5|")
PML_GWR_pred_10_y_coef_diff %>% filter(y == 0 & y_PML_var_drop == 0) %>% pull(pi_hat_dist_from_0.5) %>% hist(main="True Negative Cases", xlab="|pi_hat - 0.5|")
PML_GWR_pred_10_y_coef_diff %>% filter(!(y == 0 & y_PML_var_drop == 1)) %>% pull(pi_hat_dist_from_0.5) %>% hist(main="", xlab="|pi_hat - 0.5|")
