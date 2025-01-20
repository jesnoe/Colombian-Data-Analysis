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
}

cv.auc_mat <- read.csv("Colombia Data/local GWR lasso rescaled cv AUC (12-03-2024).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv") %>% as_tibble
local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
load("Colombia Data/local GWR lasso rescaled result (12-03-2024).RData") # load local_GWR_coefs_lasso_result

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
gwr_hyd_destination_coord <- left_join(regression_data_years %>%
                                         mutate(armed_group = ifelse(n_armed_groups > 0, 1, 0)) %>% 
                                         select(-n_armed_groups, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance,
                                                -coca_seizures, -base_seizures, -base_group, -hyd_group, -erad_aerial, -(base_source_all:hyd_source),
                                                -general_source, -general_destination),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
gwr_hyd_destination_coord$hyd_destination %>% table # 0: 2802, 1: 558 -> different by years

coord_unique <- gwr_hyd_destination_coord %>% select(id, long, lat) %>% unique
gwr_hyd_destination_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T)
dim(gwr_hyd_destination_dist)
gwr_data_hyd_destination <- gwr_hyd_destination_coord %>%
  select(-n_PPI_labs, -n_hyd_labs, -erad_manual, -long, -lat) %>% 
  mutate(hyd_destination = as.factor(hyd_destination))

municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
municipios_sf$area_km2 <- st_area(municipios_sf) %>% units::set_units("km^2") %>% as.numeric
gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination %>% 
  left_join(municipios_sf %>% as_tibble %>% select(id, area_km2), by="id") %>% 
  mutate(coca_area = coca_area / area_km2,
         river_length = river_length / area_km2,
         road_length = road_length / area_km2,
         hyd_destination = hyd_destination %>% as.character %>% as.integer) %>% 
  select(-area_km2)

minmax_scale <- function(vec) {
  return(vec/(max(vec) - min(vec)))
}
gwr_data_hyd_destination_by_area_11_scale <- gwr_data_hyd_destination_by_area %>% 
  mutate(across(PPI_lab_prob:population, ~ scales::rescale(.x, c(-1,1))))

gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination_by_area %>% 
  mutate(across(PPI_lab_prob:population, ~ scale(.x)[,1]))


bwd_range <- seq(0.5, 3, by=0.1)
local_gwr_data_id <- gwr_data_hyd_destination_by_area %>% select(id)
local_gwr_data <- gwr_data_hyd_destination_by_area %>% select(-id, -municipio, -year)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)


neighbor_id <- function(id_i, bw_i) {
  id_i_index <- which(coord_unique$id == id_i)
  i <- id_i_index 
  result <- list()
  result <- gwr_data_hyd_destination_by_area %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-(municipio:year))
  return(result)
}
neighbor_id_11 <- function(id_i, bw_i) { # LASSO for [-1, 1] scaled data
  id_i_index <- which(coord_unique$id == id_i)
  i <- id_i_index 
  result <- gwr_data_hyd_destination_by_area_11_scale %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-(municipio:year))
  return(result)
}

lasso_beta_check <- function(neighbor_id, measure, nfolds.=10, w=NULL, lambda=F, interact=F) {
  if (interact) {
    if ("coca_area" %in% names(neighbor_id)) {
      x_mat <- model.matrix(as.formula(hyd_destination~.+coca_area*coca_distance+hyd_avg*hyd_price_distance), neighbor_id)[, -1]
    }else{
      x_mat <- model.matrix(as.formula(hyd_destination~.+hyd_avg*hyd_price_distance), neighbor_id)[, -1]
    }
    
  }else{
    x_mat <- neighbor_id %>% select(-hyd_destination) %>% as.matrix
  }
  y_vec <- neighbor_id$hyd_destination
  id_cv.glmnet <- cv.glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            nfolds = nfolds.,
                            weights = w,
                            type.measure = measure)
  lambda_lasso <- ifelse(lambda, lambda, id_cv.glmnet$lambda.min)
  lasso_result_id <- glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            alpha = 1,
                            weights = w,
                            lambda = lambda_lasso)
  return(list(cv=id_cv.glmnet, lasso=lasso_result_id, x_mat=x_mat))
}

predicted_y <- function(lasso_result_, pred_x_mat) {
  lasso_result_pred <- predict(lasso_result_$lasso, pred_x_mat, lasso_result_$cv$lambda.min, type="response")[,1]
  return(ifelse(lasso_result_pred < 0.5, 0, 1))
}

prediction_result <- function(lasso_result_) {
  lasso_result_fitted <- predict(lasso_result_$lasso, lasso_result_$x_mat, lasso_result_$cv$lambda.min, type="response")[,1]
  return(confusionMatrix(ifelse(lasso_result_fitted < 0.5, 0, 1) %>% as.factor, y_vec[id_index] %>% as.factor, positive="1"))
}

lasso_exp <- function(type.measure_, interact_, scale_11, weight_=NULL) {
  y_pred_vec <- c()
  y_vec <- c()
  id_vec <- c()
  for (i in 1:nrow(local_GWR_coefs_bw_lasso)) {
    id_i <- local_GWR_coefs_bw_lasso$id[i]
    bw_id_i <- local_GWR_coefs_bw_lasso$bw[i]
    if (is.na(bw_id_i)) next
    
    if (scale_11) {
      neighbor_11_i <- neighbor_id_11(id_i, bw_id_i)
      data_id_i <- neighbor_11_i %>% filter(id == id_i)
      neighbor_11_i <- neighbor_11_i %>% filter(id != id_i)
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_11_i$hyd_destination, 0.9, 0.1)
      }else{
        weight_i <- NULL
      }
      lasso_result_ <- lasso_beta_check(neighbor_11_i %>% select(-id), type.measure_, interact=interact_, w=weight_i)
    }else{
      neighbor_i <- neighbor_id(id_i, bw_id_i)
      data_id_i <- neighbor_i %>% filter(id == id_i)
      neighbor_i <- neighbor_i %>% filter(id != id_i)
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_i$hyd_destination, 0.9, 0.1)
      }else{
        weight_i <- NULL
      }
      lasso_result_ <- lasso_beta_check(neighbor_i %>% select(-id), type.measure_, interact=interact_, w=weight_i)
    }
    y_pred_i <- predicted_y(lasso_result_, data_id_i %>% select(-id, -hyd_destination) %>% as.matrix)
    y_pred_vec <- c(y_pred_vec, y_pred_i)
    y_vec <- c(y_vec, data_id_i$hyd_destination)
    id_vec <- c(id_vec, data_id_i$id)
  }
  return(tibble(id=id_vec, hyd_destination=y_vec, prediction=y_pred_vec))
}

pred_tb
pred_tb_default <- list()
for (j in 1:10) {
  pred_tb_default[[paste0("result", j)]] <- lasso_exp("default", F, F)
}
save("pred_tb_default", file = "Colombia Data/local GWR lasso default pred table (01-20-2025).RData")

lapply(pred_tb_default, function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)), x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))

pred_tb_weight <- list()
for (j in 1:10) {
  pred_tb_weight[[paste0("result", j)]] <- lasso_exp("default", F, F, weight_=T)
}
save("pred_tb_weight", file = "Colombia Data/local GWR lasso default-weight pred table (01-20-2025).RData")

lapply(pred_tb_weight, function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)), x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))
