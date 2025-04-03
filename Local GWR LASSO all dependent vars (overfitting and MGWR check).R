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
  ferry <- read.csv("Colombia Data/ferry terminals.csv") %>% as_tibble
  police <- read.csv("Colombia Data/polices.csv") %>% as_tibble
  military <- read.csv("Colombia Data/military.csv") %>% as_tibble
  ferry$n_police <- police$n_polices
  ferry$n_military <- military$n_military
}

minmax_scale <- function(vec) {
  return(vec/(max(vec) - min(vec)))
}

ever_regression_data_years <- function(dep_var) {
  names(regression_data_years)[which(names(regression_data_years) == dep_var)] <- "y"
  if (grepl("base", dep_var)) {
    regression_data_years_dep_var <- regression_data_years %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_PPI_labs = ifelse(sum(n_PPI_labs) > 0, 1, 0),
                price_avg = median(base_avg),
                price_distance = min(base_price_distance),
                coca_area = max(coca_area),
                coca_distance = min(coca_distance),
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
    regression_data_years_dep_var$lab_prob <- glm(n_PPI_labs~., data = regression_data_years_dep_var %>% select(-id, -y), family=binomial)$fitted
    gwr_data_coord <- left_join(regression_data_years_dep_var %>%
                                  select(-n_armed_groups, -n_PPI_labs),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }else if (grepl("hyd", dep_var)) {
    regression_data_years_dep_var <- regression_data_years %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_hyd_labs = ifelse(sum(n_hyd_labs) > 0, 1, 0),
                price_avg = median(hyd_avg),
                price_distance = min(hyd_price_distance),
                coca_area = max(coca_area),
                coca_distance = min(coca_distance),
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
    regression_data_years_dep_var$lab_prob <- glm(n_hyd_labs~., data = regression_data_years_dep_var %>% select(-id, -y), family=binomial)$fitted
    gwr_data_coord <- left_join(regression_data_years_dep_var %>%
                                  select(-n_armed_groups, -n_hyd_labs),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }
  
  
  ### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
  
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

ever_regression_data_years_price_pred <- function(dep_var) {
  names(regression_data_years_price_pred)[which(names(regression_data_years_price_pred) == dep_var)] <- "y"
  if (grepl("base", dep_var)) {
    regression_data_years_price_pred_dep_var <- regression_data_years_price_pred %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_PPI_labs = ifelse(sum(n_PPI_labs) > 0, 1, 0),
                price_avg = median(base_avg),
                price_distance = min(base_price_distance),
                coca_area = max(coca_area),
                coca_distance = min(coca_distance),
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
    regression_data_years_price_pred_dep_var <- regression_data_years_price_pred %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_hyd_labs = ifelse(sum(n_hyd_labs) > 0, 1, 0),
                price_avg = median(hyd_avg),
                price_distance = min(hyd_price_distance),
                coca_area = max(coca_area),
                coca_distance = min(coca_distance),
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
forward_selection <- function(id_i, data_id, local_gwr_lasso_coefs, sig_level=0.05) {
  i <- which(local_gwr_lasso_coefs$id == id_i)
  bw_i <- local_gwr_lasso_coefs$bw[i]
  
  prev_data <- data_id %>% select(y)
  remaining_data <- data_id %>% select(-y)
  non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
  remaining_data <- remaining_data[,non_sigular_col_index]
  
  result <- list()
  reg_models <- list()
  p_values <- list()
  significance <- 1
  k <- 1
  while (significance) {
    p_values_i <- tibble()
    for (j in 1:ncol(remaining_data)) {
      new_var_j <- remaining_data[,j]
      reg_data_j <- bind_cols(prev_data, new_var_j)
      reg_model_j <- glm(y~.,
                         data = reg_data_j,
                         family = binomial)
      var_name_j <- names(new_var_j)
      reg_model_coefs_j <- summary(reg_model_j)$coefficients
      p_value_j <- reg_model_coefs_j[which(rownames(reg_model_coefs_j) == var_name_j), 4]
      p_value_j <- ifelse(p_value_j == 0, 1, p_value_j)
      p_values_i <- bind_rows(p_values_i, tibble(var_name=var_name_j, p_value=p_value_j))
    }
    p_values[[paste0("p_values_", k)]] <- p_values_i
    if (sum(p_values_i$p_value <= sig_level) > 0) {
      best_col_index <- which.min(p_values_i$p_value)
      prev_data <- bind_cols(prev_data, remaining_data[, best_col_index])
      remaining_data <- remaining_data[, -best_col_index]
      reg_models[[paste0("model_", k)]] <- glm(y~., data = prev_data, family = binomial)
      k <- k + 1
      next
    }else{
      significance <- 0
    }
  }
  
  # result <- list(reg_model = reg_models, forward_p_value=p_values)
  # return(result)
  # reg_model_i <- glm(y~.,
  #                    data = prev_data,
  #                    family = binomial)
  # local_gwr_forward_models[[paste0("id_", id_i)]] <- reg_model_i
  # roc_i <- roc(prev_data$y, reg_model_i$fitted.values)
  # performance_i <- c(id_i, ncol(prev_data)-1, auc(roc_i), (coords(roc_i, "best"))[1,] %>% unlist)
  # names(performance_i) <- c("id", "n_var", "AUC", "threshold", "specificity", "sensitivity")
  # local_gwr_performance <- bind_rows(local_gwr_performance, performance_i)
  # local_gwr_lasso_coefs$bw[i] <- bw_i
  # if (i %% 100 == 0) print(paste(i, "complete"))
}


cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_dest cv min dev (03-07-2025).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv") %>% as_tibble
local_gwr_lasso_coefs_hyd_destination <- read.csv("Colombia Data/local GWR lasso result/local GWR lasso coefs hyd_destination (03-11-2025).csv") %>% as_tibble %>% arrange(id)
load("Colombia Data/local GWR lasso hyd_dest (03-07-2025).RData") # local_GWR_coefs_lasso_hyd_dest

regression_data_years <- read.csv("Colombia Data/regression data all municipios ever lab (02-05-2025).csv") %>% as_tibble
regression_data_years %>% filter(hyd_avg < 100000)

reg_data <- regression_data_years %>% 
  filter(hyd_avg > 100000) %>%
  select(id, hyd_avg, hyd_lab_prob, coca_area, coca_distance, hyd_price_distance, hyd_seizures, n_armed_groups, river_length, road_length, population, airport)
reg_data <- left_join(reg_data, ferry %>%
                        rename(ferry=n_ferry,
                               police=n_police,
                               military=n_military) %>% 
                        mutate(ferry=ifelse(ferry > 0, 1, 0),
                               police=ifelse(police > 0, 1, 0),
                               military=ifelse(military > 0, 1, 0)),
                      by="id")
hyd_avg_lm <- lm(hyd_avg~., reg_data %>% filter(hyd_price_distance == 0) %>% select(-id, -hyd_price_distance))
hyd_avg_lm %>% summary
hyd_avg_lm$fitted.values

hyd_avg_pred <- predict(hyd_avg_lm, reg_data %>% select(-hyd_price_distance))

data.frame(price=ifelse(reg_data$hyd_price_distance == 0, reg_data$hyd_avg, NA), price_pred=hyd_avg_pred) %>% 
  ggplot() +
  geom_point(aes(x=price, y=price_pred)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("hyd. price vs. fitted price")

regression_data_years_price_pred <- regression_data_years
regression_data_years_price_pred$hyd_avg <- ifelse(regression_data_years$hyd_price_distance == 0, regression_data_years$hyd_avg, hyd_avg_pred)

## overfitting check
  # hyd_destination
local_gwr_lasso_coefs_hyd_destination %>% filter(id %in% c(17777, 73770)) %>% select(id:price_avg)

gwr_lasso_data <- ever_regression_data_years("hyd_destination")
gwr_lasso_data <- ever_regression_data_years_price_pred("hyd_destination")

id_index <- which(gwr_lasso_data$coord$id == 17777)
data_id_ <- gwr_lasso_data$norm %>% 
  filter(id %in% gwr_lasso_data$coord$id[which(gwr_lasso_data$dist[id_index,] <= local_gwr_lasso_coefs_hyd_destination$bw[id_index])])
forward_result_hyd_destination_id17777 <- forward_selection(17777, data_id_ %>% select(-(id:municipio)), local_gwr_lasso_coefs_hyd_destination)
forward_result_hyd_destination_id17777$reg_model %>% lapply(summary)
forward_result_hyd_destination_id17777$forward_p_value

data_id_ <- left_join(data_id_,
                      regression_data_years %>% group_by(id) %>% summarize(n_armed_groups=sum(n_armed_groups)),
                      by= "id") %>% select(-armed_group)
coef(local_GWR_coefs_lasso_hyd_dest$id_17777$bw_0.5)
p <- 0.7
glmnet(x = data_id_ %>% select(-(id:y)) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       # weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_17777$bw_0.5$lambda) %>% coef
glmnet(x = data_id_ %>% select(-(id:y), -price_distance, -coca_area, -coca_distance) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       # weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_17777$bw_0.5$lambda) %>% coef
glm_id <- glm(y~., data_id_ %>% select(-(id:municipio), -price_distance, -coca_area, -coca_distance), family=binomial)
glm_id %>% summary
data_id_$seizures %>% table
confusionMatrix(data_id_$y, ifelse(glm_id$fitted.values < 0.5, 0, 1) %>% as.factor)
data_id_ %>% ggplot() + geom_point(aes(x=1:nrow(data_id_), y=seizures)) + ggtitle("id=17777 seizures")

local_GWR_coefs_lasso_hyd_dest_id <- local_GWR_coefs_lasso_hyd_dest$id_17777$bw_0.5
local_GWR_coefs_lasso_hyd_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_lasso_hyd_dest_id,
                   data_id_ %>% select(-(id:y)) %>% as.matrix,
                   s=local_GWR_coefs_lasso_hyd_dest_id$lambda,
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = forward_result_hyd_destination_id17777$reg_model$model_2$fitted.values)) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. forward id=17777")
ggplot(data.frame(y = data_id_$y, pred = pred_id[,1])) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. GWR id=17777")


gwr_lasso_data <- ever_regression_data_years("hyd_destination")
gwr_lasso_data <- ever_regression_data_years_price_pred("hyd_destination")

id_index <- which(gwr_lasso_data$coord$id == 73770)
data_id_ <- gwr_lasso_data$norm %>% 
  filter(id %in% gwr_lasso_data$coord$id[which(gwr_lasso_data$dist[id_index,] <= local_gwr_lasso_coefs_hyd_destination$bw[id_index])])
forward_result_hyd_destination_id73770 <- forward_selection(73770, data_id_ %>% select(-(id:municipio)), local_gwr_lasso_coefs_hyd_destination)
forward_result_hyd_destination_id73770$reg_model %>% lapply(summary)
forward_result_hyd_destination_id73770$forward_p_value

coef(local_GWR_coefs_lasso_hyd_dest$id_73770$bw_0.7)
p <- 0.7
glmnet(x = data_id_ %>% select(-(id:y)) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       # weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_73770$bw_0.7$lambda) %>% coef
glmnet(x = data_id_ %>% select(-(id:y), -price_distance, -coca_area, -coca_distance) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       # weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_73770$bw_0.7$lambda) %>% coef
glm_id <- glm(y~., data_id_ %>% select(-(id:municipio), -price_distance, -coca_area, -coca_distance), family=binomial)
glm_id %>% summary
data_id_$seizures %>% table
confusionMatrix(data_id_$y, ifelse(glm_id$fitted.values < 0.5, 0, 1) %>% as.factor)
data_id_ %>% ggplot() + geom_point(aes(x=1:nrow(data_id_), y=seizures)) + ggtitle("id=73770 seizures")

local_GWR_coefs_lasso_hyd_dest_id <- local_GWR_coefs_lasso_hyd_dest$id_73770$bw_0.7
local_GWR_coefs_lasso_hyd_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_lasso_hyd_dest_id,
                   data_id_ %>% select(-(id:y)) %>% as.matrix,
                   s=local_GWR_coefs_lasso_hyd_dest_id$lambda,
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = forward_result_hyd_destination_id73770$reg_model$model_4$fitted.values)) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. forward id=73770")
ggplot(data.frame(y = data_id_$y, pred = pred_id[,1])) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. GWR id=73770")


gwr_lasso_data <- ever_regression_data_years("hyd_destination")
gwr_lasso_data <- ever_regression_data_years_price_pred("hyd_destination")

local_gwr_lasso_coefs_hyd_destination %>% filter(id == 5390)
id_index <- which(gwr_lasso_data$coord$id == 5390)
data_id_ <- gwr_lasso_data$norm %>% 
  filter(id %in% gwr_lasso_data$coord$id[which(gwr_lasso_data$dist[id_index,] <= local_gwr_lasso_coefs_hyd_destination$bw[id_index])])
forward_result_hyd_destination_id5390 <- forward_selection(5390, data_id_ %>% select(-(id:municipio)), local_gwr_lasso_coefs_hyd_destination)
forward_result_hyd_destination_id5390$reg_model %>% lapply(summary)
forward_result_hyd_destination_id5390$forward_p_value
confusionMatrix(data_id_$y, ifelse(forward_result_hyd_destination_id5390$reg_model$model_2$fitted.values < 0.5, 0, 1) %>% as.factor)

coef(local_GWR_coefs_lasso_hyd_dest$id_5390$bw_0.5)
glmnet(x = data_id_ %>% select(-(id:y)) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       # weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_5390$bw_0.5$lambda) %>% coef
glmnet(x = data_id_ %>% select(-(id:y), -price_distance, -coca_area, -coca_distance) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       # weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_5390$bw_0.5$lambda) %>% coef
glm_id <- glm(y~., data_id_ %>% select(-(id:municipio), -price_distance, -coca_area, -coca_distance), family=binomial)
glm_id %>% summary
data_id_$seizures %>% table
confusionMatrix(data_id_$y, ifelse(glm_id$fitted.values < 0.5, 0, 1) %>% as.factor)
data_id_ %>% ggplot() + geom_point(aes(x=1:nrow(data_id_), y=seizures)) + ggtitle("id=5390 seizures")

glm_id <- glm(y~., data_id_ %>% select(-(id:municipio), -price_distance, -coca_area, -coca_distance, -seizures), family=binomial)
glm_id %>% summary
confusionMatrix(data_id_$y, ifelse(glm_id$fitted.values < 0.5, 0, 1) %>% as.factor)
data_id_$price_avg

local_GWR_coefs_lasso_hyd_dest_id <- local_GWR_coefs_lasso_hyd_dest$id_5390$bw_0.5
local_GWR_coefs_lasso_hyd_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_lasso_hyd_dest_id,
                   data_id_ %>% select(-(id:y)) %>% as.matrix,
                   s=local_GWR_coefs_lasso_hyd_dest_id$lambda,
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = forward_result_hyd_destination_id5390$reg_model$model_2$fitted.values)) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. forward id=5390")
ggplot(data.frame(y = data_id_$y, pred = pred_id[,1])) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. GWR id=5390")

local_gwr_lasso_coefs_hyd_destination %>% filter(id == 5145)
id_index <- which(gwr_lasso_data$coord$id == 5145)
data_id_ <- gwr_lasso_data$norm %>% 
  filter(id %in% gwr_lasso_data$coord$id[which(gwr_lasso_data$dist[id_index,] <= local_gwr_lasso_coefs_hyd_destination$bw[id_index])])
forward_result_hyd_destination_id5145 <- forward_selection(5145, data_id_ %>% select(-(id:municipio)), local_gwr_lasso_coefs_hyd_destination)
forward_result_hyd_destination_id5145$reg_model %>% lapply(summary)
forward_result_hyd_destination_id5145$forward_p_value

coef(local_GWR_coefs_lasso_hyd_dest$id_5145$bw_0.6)
glmnet(x = data_id_ %>% select(-(id:y)) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       weights = ifelse(data_id_$y == 1, 0.9, 0.1),
       lambda = local_GWR_coefs_lasso_hyd_dest$id_5145$bw_0.6$lambda) %>% coef
local_GWR_coefs_lasso_hyd_dest_id <- local_GWR_coefs_lasso_hyd_dest$id_5145$bw_0.6
local_GWR_coefs_lasso_hyd_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_lasso_hyd_dest_id,
                   data_id_ %>% select(-(id:y)) %>% as.matrix,
                   s=local_GWR_coefs_lasso_hyd_dest_id$lambda,
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = pred_id[,1])) +
  geom_point(aes(x=pred, y=y))

  # base_destination
load("Colombia Data/local GWR lasso base_dest (03-07-2025).RData") # local_GWR_coefs_lasso_base_dest
local_gwr_lasso_coefs_base_destination <- read.csv("Colombia Data/local GWR lasso result/local GWR lasso coefs base_destination (03-11-2025).csv") %>% as_tibble# %>% arrange(id)
cv_dev_min_mat_base_dest <- read.csv("Colombia Data/local GWR lasso base_dest cv min dev (03-07-2025).csv") %>% as_tibble

local_gwr_lasso_coefs_base_destination %>% filter(id %in% c(15296, 85400)) %>% select(id, bw, airport, armed_group)
gwr_lasso_data <- ever_regression_data_years("base_destination")

id_index <- which(gwr_lasso_data$coord$id == 15296)
data_id_ <- gwr_lasso_data$norm %>% 
  filter(id %in% gwr_lasso_data$coord$id[which(gwr_lasso_data$dist[id_index,] <= local_gwr_lasso_coefs_base_destination$bw[id_index])])
forward_result_base_destination_id15296 <- forward_selection(15296, data_id_ %>% select(-(id:municipio)), local_gwr_lasso_coefs_base_destination)
forward_result_base_destination_id15296$reg_model %>% lapply(summary)
forward_result_base_destination_id15296$forward_p_value

tibble(y=data_id_$y,
       pred=forward_result_base_destination_id15296$reg_model$model_4$fitted.values) %>% 
  ggplot() +
  geom_point(aes(x=pred, y=y))

coef(local_GWR_coefs_lasso_base_dest$id_15296$bw_1.4)
p <- 0.7
glmnet(x = data_id_ %>% select(-(id:y)) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       weights = ifelse(data_id_$y == 1, p, 1-p),
       lambda = local_GWR_coefs_lasso_base_dest$id_15296$bw_1.4$lambda) %>% coef
local_GWR_coefs_lasso_base_dest_id <- local_GWR_coefs_lasso_base_dest$id_15296$bw_1.4
local_GWR_coefs_lasso_base_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_lasso_base_dest_id,
                   data_id_ %>% select(-(id:y)) %>% as.matrix,
                   s=local_GWR_coefs_lasso_base_dest_id$lambda,
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = pred_id[,1])) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. GWR id=15296")
ggplot(data.frame(y = data_id_$y, pred = forward_result_base_destination_id15296$reg_model$model_4$fitted.values)) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. forward id=15296")

id_index <- which(gwr_lasso_data$coord$id == 85400)
data_id_ <- gwr_lasso_data$norm %>% 
  filter(id %in% gwr_lasso_data$coord$id[which(gwr_lasso_data$dist[id_index,] <= local_gwr_lasso_coefs_base_destination$bw[id_index])])
forward_result_base_destination_id85400 <- forward_selection(85400, data_id_ %>% select(-(id:municipio)), local_gwr_lasso_coefs_base_destination)
forward_result_base_destination_id85400$reg_model %>% lapply(summary)
forward_result_base_destination_id85400$forward_p_value

coef(local_GWR_coefs_lasso_base_dest$id_85400$bw_2)
glmnet(x = data_id_ %>% select(-(id:y)) %>% as.matrix,
       y = data_id_$y,
       family = binomial,
       alpha = 1,
       weights = ifelse(data_id_$y == 1, 0.9, 0.1),
       lambda = local_GWR_coefs_lasso_base_dest$id_85400$bw_2$lambda) %>% coef
local_GWR_coefs_lasso_base_dest_id <- local_GWR_coefs_lasso_base_dest$id_85400$bw_2
local_GWR_coefs_lasso_base_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_lasso_base_dest_id,
                   data_id_ %>% select(-(id:y)) %>% as.matrix,
                   s=local_GWR_coefs_lasso_base_dest_id$lambda,
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = pred_id[,1])) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. id=85400")



## MGWR test