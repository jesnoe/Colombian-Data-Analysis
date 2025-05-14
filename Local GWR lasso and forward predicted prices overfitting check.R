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
  }

neighbor_id <- function(id_i, bw_i, scale_11_, coord_unique_, local_gwr_dist_) {
  if (scale_11_) gwr_data_id <- gwr_forward_data$scale_11
  else gwr_data_id <- gwr_forward_data$norm
  
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}

bwd_range <- seq(0.5, 3, by=0.1)
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
gwr_forward_data <- ever_regression_data_years_price_pred("hyd_destination")

local_gwr_lasso_coefs_hyd_destination <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso coefs hyd_destination (03-28-2025).csv") %>%
  as_tibble %>% arrange(id)
indep_vars_ <- names(local_gwr_lasso_coefs_hyd_destination)[-(1:3)]
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble

min_seizure_scaled <- min(gwr_forward_data$norm$seizures) %>% round(3)
min_coca_area_scaled <- min(gwr_forward_data$norm$coca_area) %>% round(3)
coord_unique <- gwr_forward_data$coord
local_gwr_dist <- gwr_forward_data$dist %>% as.matrix
nonzero_seizure <- cv_dev_min_mat_
nonzero_coca_area <- cv_dev_min_mat_ 
for (i in 1:nrow(cv_dev_min_mat_)) {
  id_i <- cv_dev_min_mat_$id[i]
  for (bw_ij in bwd_range) {
    col_name_ij <- paste0("bw_", bw_ij)
    neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_=F, coord_unique, local_gwr_dist)
    nonzero_seizure[[col_name_ij]][i] <- sum(round(neighbor_ij$seizures, 3) > min_seizure_scaled)
    nonzero_coca_area[[col_name_ij]][i] <- sum(round(neighbor_ij$coca_area, 3) > min_coca_area_scaled)
  }
}
nonzero_seizure
nonzero_coca_area

cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price cv min dev (03-28-2025).csv") %>% as_tibble
load("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price (03-28-2025).RData") # local_GWR_coefs_lasso_hyd_dest
local_gwr_lasso_coef_map_limited(local_GWR_coefs_lasso_hyd_dest, cv_dev_min_mat_, "hyd_destination"); rm(local_GWR_coefs_lasso_hyd_dest)

## alpha = 0.1 model drop
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev alpha = 0.1 (04-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price alpha = 0.1 (04-09-2025).RData") # local_GWR_coefs_forward_hyd_dest_alpha_0.1
local_gwr_forward_coef_map_limited(local_GWR_coefs_forward_hyd_dest_alpha_0.1, cv_dev_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_forward_hyd_dest_alpha_0.1)
# local_GWR_coefs_forward_list<-local_GWR_coefs_forward_hyd_dest_alpha_0.1; cv_dev_min_mat<-cv_dev_min_mat_; indep_vars<-indep_vars_; dep_var="hyd_destination"; weight_=NULL; alpha<-0.1;

## alpha = 0.1 variable drop
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev alpha = 0.1 drop (04-21-2025).csv") %>% as_tibble
load("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price alpha = 0.1 drop (04-21-2025).RData") # local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop
local_gwr_forward_coef_map_limited(local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop, cv_dev_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop)
# local_GWR_coefs_forward_list<-local_GWR_coefs_forward_hyd_dest_alpha_0.1; cv_dev_min_mat<-cv_dev_min_mat_; indep_vars<-indep_vars_; dep_var="hyd_destination"; weight_=NULL; alpha<-0.1;

forward_gwr_coefs_model_drop <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward coefs limited alpha=0.1 hyd_destination (04-15-2025).csv") %>% as_tibble
forward_gwr_coefs_var_drop <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward coefs limited drop alpha=0.1 hyd_destination (04-21-2025).csv") %>% as_tibble
step_gwr_coefs_model_drop <- read.csv("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise coefs limited alpha=0.1 hyd_destination model drop (05-12-2025).csv") %>% as_tibble
step_gwr_coefs_var_drop <- read.csv("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise coefs limited alpha=0.1 hyd_destination var drop (05-12-2025).csv") %>% as_tibble

step_gwr_coefs_var_drop %>% filter(lab_prob > 10000)

data_id <- neighbor_id(25805, 0.5, scale_11_=F, coord_unique, local_gwr_dist)
data_id %>% select(y, airport, armed_group, ferry:military, seizures, coca_area) %>% arrange(y) %>% print(n=49)
# variable drop method does not change bandwidth (the number of neighbors), and thus the same quasi-complete separation
# On the other hand, model drop method increases bandwidth, which result in the less or no quasi-complete separation

forward_gwr_coefs_model_drop %>% filter(abs(seizures) > 1000)
forward_gwr_coefs_var_drop %>% filter(abs(seizures) > 1000)

# local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop high lab_prob coef data check
forward_overfit_model_drop <- forward_gwr_coefs_model_drop %>% filter(abs(seizures) > 1000)
forward_overfit_var_drop <- forward_gwr_coefs_var_drop %>% filter(abs(seizures) > 1000)
step_overfit_var_drop <- step_gwr_coefs_var_drop %>% filter(abs(lab_prob) > 1000)
forward_overfit_model_drop$id[!(forward_overfit_model_drop$id %in% forward_overfit_var_drop$id)]

id_overfit <- forward_overfit_model_drop$id
bw_overfit <- forward_overfit_model_drop$bw

i <- 1
for (i in 1:nrow(forward_overfit_model_drop)) {
  id_i <- id_overfit[i]
  bw_i <- bw_overfit[i]
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  nonzero_seizure %>% filter(id == id_i) %>% bind_rows(nonzero_coca_area %>% filter(id == id_i))
  data_id %>% ggplot() +
    geom_point(aes(x=1:nrow(data_id), y = seizures))
  ggsave(paste0("Colombia Data/overfitting check/forward model drop/id=", id_i, " seizures.png"), scale = 1)
}

stepwise <- function(data_id, w=NULL, sig_level=0.05, iter_limit) {
  prev_data <- data_id %>% select(y)
  remaining_data <- data_id %>% select(-y)
  non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
  remaining_data <- remaining_data[,non_sigular_col_index]
  reg_model <- NULL
  
  result <- list()
  reg_models <- list()
  p_values <- list()
  significance <- 1
  backward <- 0
  k <- 1
  while (significance) {
    p_values_i <- tibble()
    for (j in 1:ncol(remaining_data)) {
      new_var_j <- remaining_data[,j]
      reg_data_j <- bind_cols(prev_data, new_var_j)
      reg_model_j <- glm(y~.,
                         data = reg_data_j,
                         weights = w,
                         family = binomial)
      var_name_j <- names(new_var_j)
      reg_model_coefs_j <- summary(reg_model_j)$coefficients
      p_value_j <- reg_model_coefs_j[which(rownames(reg_model_coefs_j) == var_name_j), 4]
      p_value_j <- ifelse(p_value_j == 0, 1, p_value_j)
      p_values_i <- bind_rows(p_values_i, tibble(var_name=var_name_j, p_value=p_value_j))
    }
    
    if (sum(p_values_i$p_value <= sig_level) == 0) {
      break
    }
    
    p_values[[paste0("p_values_", k)]] <- p_values_i
    
    while (sum(p_values_i$p_value <= sig_level) > 0) {
      best_col_index <- which.min(p_values_i$p_value)
      prev_data <- bind_cols(prev_data, remaining_data[, best_col_index])
      remaining_data <- remaining_data[, -best_col_index]
      reg_model <- glm(y~., data = prev_data, weights = w, family = binomial)
      reg_models[[paste0("model_", k)]] <- reg_model
      k <- k + 1
      
      if (ncol(remaining_data) < 1) break
      
      p_values_i <- tibble()
      for (j in 1:ncol(remaining_data)) {
        new_var_j <- remaining_data[,j]
        reg_data_j <- bind_cols(prev_data, new_var_j)
        reg_model_j <- glm(y~.,
                           data = reg_data_j,
                           weights = w,
                           family = binomial)
        var_name_j <- names(new_var_j)
        reg_model_coefs_j <- summary(reg_model_j)$coefficients
        p_value_j <- reg_model_coefs_j[which(rownames(reg_model_coefs_j) == var_name_j), 4]
        p_value_j <- ifelse(p_value_j == 0, 1, p_value_j)
        p_values_i <- bind_rows(p_values_i, tibble(var_name=var_name_j, p_value=p_value_j))
      }
      p_values[[paste0("p_values_", k)]] <- p_values_i
    }
    
    
    p_values_i <- tibble(var_name = names(prev_data)[-1], p_value = summary(reg_model)$coefficients[,4][-1])
    p_values[[paste0("p_values_", k+1)]] <- p_values_i
    while (any(p_values_i$p_value > sig_level)) {
      if (backward) {
        significance <- 0
        break
      }
      worst_col_index <- which.max(p_values_i$p_value) + 1
      remaining_data <- bind_cols(remaining_data, prev_data[, worst_col_index])
      prev_data <- prev_data[, -worst_col_index]
      reg_model <- glm(y~., data = prev_data, weights = w, family = binomial)
      reg_models[[paste0("model_", k)]] <- reg_model
      k <- k + 1
      
      p_values_i <- tibble(var_name = names(prev_data)[-1], p_value = summary(reg_model)$coefficients[,4][-1])
      p_values[[paste0("p_values_", k)]] <- p_values_i
      if(ncol(prev_data) < 2) break
    }
    backward <- 1
    
    if (k > iter_limit) break
  }
  
  result <- list(reg_model = reg_models, p_values=p_values)
  return(result)
}

regression_data_aggr <- regression_data_years %>% 
  group_by(id) %>% 
  summarize(seizures = mean(hyd_seizures, na.rm=T))

forward_overfit_model_drop_raw_seizure <- list()
for (i in 1:nrow(forward_overfit_model_drop)) {
  id_i <- forward_overfit_model_drop$id[i]
  bw_i <- forward_overfit_model_drop$bw[i]
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  step_raw_seizure_id <- stepwise(data_id %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% select(-id), sig_level = 0.1, iter_limit = 100)
  # step_raw_seizure_id <- stepwise(data_id[,-1], sig_level = 0.1)
  if (length(step_raw_seizure_id) == 0) {
    forward_overfit_model_drop_raw_seizure[[i]] <- NA
    next
  }
  forward_overfit_model_drop_raw_seizure[[i]] <- step_raw_seizure_id$reg_model[[length(step_raw_seizure_id$reg_model)]]
}
lapply(forward_overfit_model_drop_raw_seizure, coef)

forward_overfit_var_drop_raw_seizure <- list()
for (i in 1:nrow(forward_overfit_var_drop)) {
  id_i <- forward_overfit_var_drop$id[i]
  bw_i <- forward_overfit_var_drop$bw[i]
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  step_raw_seizure_id <- stepwise(data_id %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% select(-id), sig_level = 0.1, iter_limit = 100)
  # step_raw_seizure_id <- stepwise(data_id[,-1], sig_level = 0.1)
  forward_overfit_var_drop_raw_seizure[[i]] <- step_raw_seizure_id$reg_model[[length(step_raw_seizure_id$reg_model)]]
}
lapply(forward_overfit_var_drop_raw_seizure, coef)

glm(y~., gwr_forward_data$norm %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% select(-id, -municipio), family = binomial) %>% summary
gwr_forward_data$norm$seizures %>% summary
regression_data_aggr$seizures %>% summary


### quasi separation check
forward_overfit_model_drop
QS_check_for_0_model_drop <- matrix(0, nrow(forward_overfit_model_drop), 7)
QS_check_for_1_model_drop <- matrix(0, nrow(forward_overfit_model_drop), 7)
QS_cases_reg_models_norm_seizures <- list()
QS_cases_reg_models_raw_seizures <- list()
for (i in 1:nrow(forward_overfit_model_drop)) {
  id_i <- forward_overfit_model_drop$id[i]
  bw_i <- forward_overfit_model_drop$bw[i]
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  data_id_0 <- data_id %>% filter(y == 0)
  data_id_1 <- data_id %>% filter(y == 1)
  n_data_0 <- nrow(data_id_0)
  n_data_1 <- nrow(data_id_1)
  QS_cherck_for_0_i <- c(id_i, n_data_0,
                         sum(data_id_0$airport == 1) / n_data_0,
                         sum(data_id_0$armed_group == 1) / n_data_0,
                         sum(data_id_0$ferry == 1) / n_data_0,
                         sum(data_id_0$police == 1) / n_data_0,
                         sum(data_id_0$military == 1) / n_data_0)
  QS_cherck_for_1_i <- c(id_i, n_data_1,
                         sum(data_id_1$airport == 1) / n_data_1,
                         sum(data_id_1$armed_group == 1) / n_data_1,
                         sum(data_id_1$ferry == 1) / n_data_1,
                         sum(data_id_1$police == 1) / n_data_1,
                         sum(data_id_1$military == 1) / n_data_1)
  QS_check_for_0_model_drop[i,] <- QS_cherck_for_0_i
  QS_check_for_1_model_drop[i,] <- QS_cherck_for_1_i
  
  QS_cases_reg_models_norm_seizures[[i]] <- glm(y~., data = data_id %>% select(-id), family = binomial)
  QS_cases_reg_models_raw_seizures[[i]] <- glm(y~., data = data_id %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% select(-id), family = binomial)
}
QS_check_for_0_model_drop <- as_tibble(QS_check_for_0_model_drop)
QS_check_for_1_model_drop <- as_tibble(QS_check_for_1_model_drop)
names(QS_check_for_0_model_drop) <- c("id", "n_obs", "y_airport", "y_armed_group", "y_ferry", "y_police", "y_military")
names(QS_check_for_1_model_drop) <- c("id", "n_obs", "y_airport", "y_armed_group", "y_ferry", "y_police", "y_military")
QS_check_for_0_model_drop %>% left_join(gwr_forward_data$norm %>% select(id, coca_area, seizures), by="id")
QS_check_for_1_model_drop %>% left_join(gwr_forward_data$norm %>% select(id, coca_area, seizures), by="id")
QS_cases_reg_models_norm_seizures
QS_cases_reg_models_raw_seizures


QS_check_for_0_var_drop <- matrix(0, nrow(forward_overfit_var_drop), 7)
QS_check_for_1_var_drop <- matrix(0, nrow(forward_overfit_var_drop), 7)
QS_cases_reg_models_var_drop_norm_seizures <- list()
QS_cases_reg_models_var_drop_raw_seizures <- list()
for (i in 1:nrow(forward_overfit_var_drop)) {
  id_i <- forward_overfit_var_drop$id[i]
  bw_i <- forward_overfit_var_drop$bw[i]
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  data_id_0 <- data_id %>% filter(y == 0)
  data_id_1 <- data_id %>% filter(y == 1)
  n_data_0 <- nrow(data_id_0)
  n_data_1 <- nrow(data_id_1)
  QS_cherck_for_0_i <- c(id_i, n_data_0,
                         sum(data_id_0$airport == 1) / n_data_0,
                         sum(data_id_0$armed_group == 1) / n_data_0,
                         sum(data_id_0$ferry == 1) / n_data_0,
                         sum(data_id_0$police == 1) / n_data_0,
                         sum(data_id_0$military == 1) / n_data_0)
  QS_cherck_for_1_i <- c(id_i, n_data_1,
                         sum(data_id_1$airport == 1) / n_data_1,
                         sum(data_id_1$armed_group == 1) / n_data_1,
                         sum(data_id_1$ferry == 1) / n_data_1,
                         sum(data_id_1$police == 1) / n_data_1,
                         sum(data_id_1$military == 1) / n_data_1)
  QS_check_for_0_var_drop[i,] <- QS_cherck_for_0_i
  QS_check_for_1_var_drop[i,] <- QS_cherck_for_1_i
}
QS_check_for_0_var_drop <- as_tibble(QS_check_for_0_var_drop)
QS_check_for_1_var_drop <- as_tibble(QS_check_for_1_var_drop)
names(QS_check_for_0_var_drop) <- c("id", "n_obs", "y_airport", "y_armed_group", "y_ferry", "y_police", "y_military")
names(QS_check_for_1_var_drop) <- c("id", "n_obs", "y_airport", "y_armed_group", "y_ferry", "y_police", "y_military")
QS_check_for_0_var_drop %>% print(n=21)
QS_check_for_1_var_drop %>% print(n=21)


step_overfit_var_drop
step_QS_check_for_0_var_drop <- matrix(0, nrow(step_overfit_var_drop), 7)
step_QS_check_for_1_var_drop <- matrix(0, nrow(step_overfit_var_drop), 7)
for (i in 1:nrow(step_overfit_var_drop)) {
  id_i <- step_overfit_var_drop$id[i]
  bw_i <- step_overfit_var_drop$bw[i]
  data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  data_id_0 <- data_id %>% filter(y == 0)
  data_id_1 <- data_id %>% filter(y == 1)
  n_data_0 <- nrow(data_id_0)
  n_data_1 <- nrow(data_id_1)
  QS_cherck_for_0_i <- c(id_i, n_data_0,
                         sum(data_id_0$airport == 1) / n_data_0,
                         sum(data_id_0$armed_group == 1) / n_data_0,
                         sum(data_id_0$ferry == 1) / n_data_0,
                         sum(data_id_0$police == 1) / n_data_0,
                         sum(data_id_0$military == 1) / n_data_0)
  QS_cherck_for_1_i <- c(id_i, n_data_1,
                         sum(data_id_1$airport == 1) / n_data_1,
                         sum(data_id_1$armed_group == 1) / n_data_1,
                         sum(data_id_1$ferry == 1) / n_data_1,
                         sum(data_id_1$police == 1) / n_data_1,
                         sum(data_id_1$military == 1) / n_data_1)
  step_QS_check_for_0_var_drop[i,] <- QS_cherck_for_0_i
  step_QS_check_for_1_var_drop[i,] <- QS_cherck_for_1_i
}
step_QS_check_for_0_var_drop <- as_tibble(step_QS_check_for_0_var_drop)
step_QS_check_for_1_var_drop <- as_tibble(step_QS_check_for_1_var_drop)
names(step_QS_check_for_0_var_drop) <- c("id", "n_obs", "y_airport", "y_armed_group", "y_ferry", "y_police", "y_military")
names(step_QS_check_for_1_var_drop) <- c("id", "n_obs", "y_airport", "y_armed_group", "y_ferry", "y_police", "y_military")
step_QS_check_for_0_var_drop %>% print(n=21)
step_QS_check_for_1_var_drop %>% print(n=21)

##
data_id$lab_prob %>% boxplot(ylim=c(0,1), main="lab_prob bw=0.6") # mostly lower than 0.2. An outlier over 0.4
data_id_bw_1.4$lab_prob %>% boxplot(ylim=c(0,1), main="lab_prob bw=1.4") # max=0.9604825
data_id %>% filter(y == 1) %>% pull(lab_prob) %>% boxplot
data_id %>% filter(y == 0) %>% pull(lab_prob) %>% boxplot

gwr_forward_data$norm$lab_prob %>% hist
gwr_forward_data$norm$lab_prob %>% summary
scale(gwr_forward_data$norm$lab_prob) %>% summary

local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop$id_25307$bw_0.6 %>% summary
glm(y~., data_id %>% mutate(airport = as.factor(airport)) %>% select(y, airport, lab_prob, armed_group, price_avg), family=binomial) %>% summary
glm(y~., 
    data_id %>% 
      mutate(airport = as.factor(airport),
             lab_prob = ifelse(lab_prob < 0.5, 0, 1) %>% as.factor) %>% # no positive obs
      select(y, airport, lab_prob, armed_group, price_avg),
    family=binomial) %>% summary
local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop$id_25307$bw_1.4 %>% summary


## confusion matrix and summary for model drop and variable drop results
gwr_forward_data$norm %>% filter(id %in% (forward_gwr_coefs_model_drop %>% filter(is.na(bw)) %>% pull(id))) %>% print(n=30) # only 1 positive 44847, URIBIA, La Guajira
gwr_forward_data$norm %>% filter(id %in% (forward_gwr_coefs_var_drop %>% filter(is.na(bw)) %>% pull(id))) %>% print(n=29) # all negative

lasso_gwr_coefs_model_drop <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso coefs limited hyd_destination (04-15-2025).csv") %>% as_tibble
lasso_gwr_coefs_var_drop <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso coefs drop hyd_destination (04-21-2025).csv") %>% as_tibble
gwr_forward_data$norm %>% filter(id %in% (lasso_gwr_coefs_model_drop %>% filter(is.na(bw)) %>% pull(id))) %>% print(n=30) # only 1 positive 44847, URIBIA, La Guajira
gwr_forward_data$norm %>% filter(id %in% (lasso_gwr_coefs_var_drop %>% filter(is.na(bw)) %>% pull(id))) %>% print(n=29) # all negative

load("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price model drop (04-15-2025).RData") # local_GWR_coefs_lasso_hyd_dest_model_drop
load("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price drop (04-21-2025).RData") # local_GWR_coefs_lasso_hyd_dest

gwr_forward_lasso_data_norm <- read.csv("Colombia Data/local GWR forward result predicted prices/GWR forward lasso predictions.csv") %>% as_tibble
confusionMatrix(gwr_forward_lasso_data_norm$y_forward_model_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")
confusionMatrix(gwr_forward_lasso_data_norm$y_forward_var_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")
confusionMatrix(gwr_forward_lasso_data_norm$y_lasso_model_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")
confusionMatrix(gwr_forward_lasso_data_norm$y_lasso_var_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")


forward_diff_id <- gwr_forward_lasso_data_norm %>% filter(y_forward_model_drop != y_forward_var_drop) %>% pull(id)
lasso_diff_id <- gwr_forward_lasso_data_norm %>% filter(y_lasso_model_drop != y_lasso_var_drop) %>% pull(id)
forward_diff_id %>% length # 67
lasso_diff_id %>% length # 105


gwr_forward_lasso_data_norm %>% filter(id %in% forward_diff_id)
forward_gwr_coefs_model_drop %>% filter(id %in% forward_diff_id)


forward_model_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_forward_model_drop != y) %>% pull(id)
forward_var_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_forward_var_drop != y) %>% pull(id)
lasso_model_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_lasso_model_drop != y) %>% pull(id)
lasso_var_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_lasso_var_drop != y) %>% pull(id)


