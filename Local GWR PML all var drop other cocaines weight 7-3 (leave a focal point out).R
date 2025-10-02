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
  
  n_drop_ <- 10
  bwd_range <- seq(0.5, 3, by=0.1)
  depto_map <- suppressMessages(fortify(departamentos)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(id != 88) %>% 
    left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
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


neighbor_id <- function(id_i, bw_i, scale_11_, coord_unique_, local_gwr_dist_, gwr_data_) {
  if (scale_11_) gwr_data_id <- gwr_data_$scale_11
  else gwr_data_id <- gwr_data_$norm
  
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}

local_GWR_PML <- function(type.measure_="default", cv_aic_min_mat, sig_level_=0.05, gwr_PML_data_, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var) {
  bwd_range <- seq(0.5, 3, by=0.1)
  coord_unique <- gwr_PML_data_$coord
  local_gwr_dist <- gwr_PML_data_$dist %>% as.matrix
  
  local_GWR_coefs_PML_result <- list()
  for (i in 1:nrow(cv_aic_min_mat)) {
    id_i <- cv_aic_min_mat$id[i]
    local_GWR_coefs_PML_result[[paste0("id_", id_i)]] <- list()
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      bw_name <- paste0("bw_", bw_ij)
      local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[bw_name]] <- NA
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, coord_unique, local_gwr_dist, gwr_data_=gwr_PML_data_) %>% 
        filter(id != id_i) #### leave a focal point out
      n_0_1 <- neighbor_ij$y %>% table
      
      # restrict too unbalanced responses
      if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        cv_aic_min_mat[[bw_name]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      n_unique_vals <- neighbor_ij %>% select(-id) %>% apply(2, function(x) length(table(x)))
      if (method_ == "model drop") {
        if (any(n_unique_vals < 2) | any(n_unique_vals[3:4] < n_drop)) {
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
          next
        }
      }
      
      
      if (method_ == "var drop") {
        if (n_unique_vals[["coca_area"]] < n_drop) {
          neighbor_ij$coca_area <- NULL
          n_unique_vals[["coca_area"]] <- n_drop
        }
        if (n_unique_vals[["seizures"]] < n_drop) {
          neighbor_ij$seizures <- NULL
          n_unique_vals[["seizures"]] <- n_drop
        }
        if (n_unique_vals[["price_avg"]] < n_drop) {
          neighbor_ij$price_avg <- NULL
          n_unique_vals[["price_avg"]] <- n_drop
        }
        if (n_unique_vals[["river_length"]] < n_drop) {
          neighbor_ij$river_length <- NULL
          n_unique_vals[["river_length"]] <- n_drop
        }
        if (n_unique_vals[["road_length"]] < n_drop) {
          neighbor_ij$road_length <- NULL
          n_unique_vals[["road_length"]] <- n_drop
        }
        if (n_unique_vals[["lab_prob"]] < n_drop) {
          neighbor_ij$lab_prob <- NULL
          n_unique_vals[["lab_prob"]] <- n_drop
        }
        
        if (neighbor_ij$airport %>% table %>% min < n_drop | n_unique_vals[["airport"]] < 2) neighbor_ij$airport <- NULL
        if (neighbor_ij$armed_group %>% table %>% min < n_drop | n_unique_vals[["armed_group"]] < 2) neighbor_ij$armed_group <- NULL
        if (neighbor_ij$ferry %>% table %>% min < n_drop | n_unique_vals[["ferry"]] < 2) neighbor_ij$ferry <- NULL
        if (neighbor_ij$police %>% table %>% min < n_drop | n_unique_vals[["police"]] < 2) neighbor_ij$police <- NULL
        if (neighbor_ij$military %>% table %>% min < n_drop | n_unique_vals[["military"]] < 2) neighbor_ij$military <- NULL
      }
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      result_i <- tryCatch(
        {
          PML_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)
          AIC_i <- extractAIC(PML_result_ij)[[2]]
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- AIC_i
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- PML_result_ij
        },
        error = function(e) {
          return(e)
        }
      )
      
      if (inherits(result_i, "error")) {
        cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      }
      
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(cv_aic_min_mat=cv_aic_min_mat,
              PML=local_GWR_coefs_PML_result))
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

local_GWR_PML_y <- function(dep_var_, seed_model, weight_in=NULL) {
  gwr_data <- ever_regression_data_years_price_pred(dep_var_)
  gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
  gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
  gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
  cv_aic_min_mat_[,-1] <- NA
  cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
  bwd_range <- seq(0.5, 3, by=0.1)
  
  min_seizure_scaled <- min(gwr_data$norm$seizures) %>% round(3)
  min_coca_area_scaled <- min(gwr_data$norm$coca_area) %>% round(3)
  coord_unique <- gwr_data$coord
  local_gwr_dist <- gwr_data$dist %>% as.matrix
  
  set.seed(seed_model)
  local_GWR_coefs_PML_list <- local_GWR_PML(dep_var = dep_var_, cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="var drop", sig_level_ = 0.1, n_drop = 10, weight_ = weight_in)
  
  local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1 <- local_GWR_coefs_PML_list$PML
  weight_in_1 <- weight_in[1]*10
  weight_in_0 <- weight_in[2]*10
  write.csv(local_GWR_coefs_PML_list$cv_aic_min_mat,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight %i-%i (09-08-2025).csv",
                    dep_var_, weight_in_1, weight_in_0), row.names = F)
  save("local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1",
       file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight %i-%i (09-08-2025).RData",
                      dep_var_, weight_in_1, weight_in_0))
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1); rm(local_GWR_coefs_PML_list)
}

gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
# write.csv(gwr_data$norm, "Colombia Data/hyd gwr data.csv", row.names = F)
gwr_data <- ever_regression_data_years_price_pred("base_source")
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
# write.csv(gwr_data$norm, "Colombia Data/base gwr data.csv", row.names = F)

# set.seed(1000)
# seed_i <- sample(1:1000000, 1)
# local_GWR_PML_y("base_source", 700630) # 700630
# seed_i <- sample(1:1000000, 1)
# local_GWR_PML_y("base_destination", 49056) # 49056
# seed_i <- sample(1:1000000, 1)
# local_GWR_PML_y("hyd_source", 424271) # 424271

weights <- c(0.7 , 0.3) # 0.7 for postive outcomes
weights <- c(0.9 , 0.1) # 0.9 for postive outcomes
# local_GWR_PML_y("base_source", 700630, weight_in = weights) # 700630
# local_GWR_PML_y("base_destination", 49056, weight_in = weights) # 49056
# local_GWR_PML_y("hyd_source", 424271, weight_in = weights) # 424271
# local_GWR_PML_y("hyd_destination", 100, weight_in = weights)


# coef map by F1 scores
local_gwr_PML_coef_map_by_F1 <- function(local_GWR_coefs_list, PML_best_bw_tbl_, criteria, dep_var, indep_vars, alpha=0.1, n_drop, date_) {
  indep_vars <- c("Intercept", indep_vars)
  coef_table <- tibble(id = PML_best_bw_tbl_$id, bw=PML_best_bw_tbl_[[criteria]])
  pval_table <- coef_table
  coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars))
  pval_mat <- coef_mat
  
  indep_vars_df <- data.frame(var_name=indep_vars)
  for (i in 1:nrow(coef_table)) {
    bw_i <- coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_model_i <- local_GWR_coefs_list[[i]][[paste0("bw_", bw_i)]]
    coef_i <- coef(local_GWR_model_i)
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i, p_value=local_GWR_model_i$prob)
    coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
    coef_mat[i,] <- coef_i_df$coef
    pval_mat[i,] <- coef_i_df$p_value
  }
  
  coef_table <- bind_cols(coef_table, coef_mat)
  pval_table <- bind_cols(pval_table, pval_mat)
  names(coef_table)[-(1:2)] <- indep_vars
  names(pval_table)[-(1:2)] <- indep_vars
  write.csv(coef_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out %s all var drop %i weight 9-1 (%s).csv", dep_var, criteria, n_drop, date_),
            row.names = F)
  
  
  # for (i in c(2, 4:length(coef_table))) {
  #   var_name <- names(coef_table)[i]
  #   gwr_coefs_i <- data.frame(id=coef_table$id,
  #                             coef=coef_table[[var_name]],
  #                             rounded_coef=coef_table[[var_name]] %>% round(3),
  #                             p_value=pval_table[[var_name]])
  #   min_coef <- min(gwr_coefs_i$coef, na.rm=T)
  #   max_coef <- max(gwr_coefs_i$coef, na.rm=T)
  #   coef_map_coords_bw <- map_df %>% 
  #     left_join(gwr_coefs_i, by="id")
  #   # gwr_coefs_i$coef <- ifelse(gwr_coefs_i$p_value > alpha, NA, gwr_coefs_i$coef)
  #   coef_map_coords <- map_df %>% 
  #     left_join(gwr_coefs_i, by="id")
  #   
  #   if (i == 2) {
  #     gwr_coef_map <- ggplot(coef_map_coords_bw, aes(x=long, y=lat)) + 
  #       geom_polygon(aes(group=group, fill=coef),
  #                    color = "black",
  #                    linewidth = 0.1) + 
  #       expand_limits(x = depto_map$long, y = depto_map$lat) + 
  #       coord_quickmap() +
  #       scale_fill_viridis_c(na.value = "white") +
  #       labs(fill=var_name, x="", y="", title=dep_var) +
  #       theme_bw() +
  #       theme(panel.grid.major = element_blank(),
  #             panel.grid.minor = element_blank(),
  #             panel.border = element_blank(),
  #             axis.text = element_blank(),
  #             line = element_blank()
  #       )
  #   }else{
  #     gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
  #       geom_polygon(aes(group=group, fill=coef),
  #                    color = "black",
  #                    linewidth = 0.1) + 
  #       expand_limits(x = depto_map$long, y = depto_map$lat) + 
  #       coord_quickmap() +
  #       scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
  #                            values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
  #                            na.value = "white") +
  #       labs(fill=var_name, x="", y="", title=dep_var) +
  #       theme_bw() +
  #       theme(panel.grid.major = element_blank(),
  #             panel.grid.minor = element_blank(),
  #             panel.border = element_blank(),
  #             axis.text = element_blank(),
  #             line = element_blank()
  #       )
  #   }
  #   
  #   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s/local GWR PML coef by drop %s %s %s all var drop %i weight 7-3 (%s).png", dep_var, var_name, dep_var, criteria, n_drop, date_),
  #          gwr_coef_map, scale=1)
  # }
}

PML_F1_score <- function(PML_model_list, cv_aic_min_mat) {
  F1_mat <- cv_aic_min_mat
  for (i in 1:nrow(cv_aic_min_mat)) {
    id_i <- cv_aic_min_mat$id[i]
    PML_model_list_id <- PML_model_list[[i]]
    for (j in 1:length(PML_model_list_id)) {
      if (is.na(cv_aic_min_mat[i, j+1])) {
        F1_mat[i,j+1] <- NA
        next
      }
      
      PML_model_list_id_bw <- PML_model_list_id[[j]]
      PML_model_list_id_bw_y <- PML_model_list_id_bw$model$y
      PML_model_list_id_bw_pred <- ifelse(PML_model_list_id_bw$predict < 0.5, 0, 1) %>% factor(levels = c("0", "1"))
      PML_model_list_id_bw_CM <- confusionMatrix(PML_model_list_id_bw_pred, PML_model_list_id_bw_y, positive = "1")
      F1_mat[i,j+1] <- PML_model_list_id_bw_CM$byClass[7] # F1 score
      
    }
  }
  return(F1_mat)
}

PML_F1_score_y <- function(dep_var_) {
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight 9-1 (09-08-2025).RData", dep_var_))
  PML_gwr_aic_var_drop_log_seizure_coca <- 
    read_csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight 9-1 (09-08-2025).csv", dep_var_)) %>% as_tibble
  
  PML_F1_score_var_drop_log_seizure <- PML_F1_score(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1, PML_gwr_aic_var_drop_log_seizure_coca)
  write.csv(PML_F1_score_var_drop_log_seizure,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 9-1 (09-08-2025).csv", dep_var_),
            row.names = F)
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
}

# PML_F1_score_y("hyd_source")
# PML_F1_score_y("hyd_destination")
# PML_F1_score_y("base_source")
# PML_F1_score_y("base_destination")

read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_source leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML base_source leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML base_destination leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble

## coef map by F1 var drop 
local_gwr_PML_coef_map_by_F1_y <- function(dep_var_) {
  gwr_data <- ever_regression_data_years_price_pred(dep_var_)
  gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
  gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
  gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
  indep_vars_ <- names(gwr_data$norm)[-(1:3)]
  PML_F1_score_var_drop_log_seizure_10_loo <- 
    read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 9-1 (09-08-2025).csv", dep_var_)) %>% as_tibble
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight 9-1 (09-08-2025).RData", dep_var_))
  
  PML_best_bw_tbl_var_drop  <- tibble(id = PML_F1_score_var_drop_log_seizure_10_loo$id,
                                      PML_log_seizure_coca_bw_F1 = PML_F1_score_var_drop_log_seizure_10_loo[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
  
  local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1, PML_best_bw_tbl_var_drop, criteria="PML_log_seizure_coca_bw_F1", dep_var = dep_var_,
                               indep_vars = indep_vars_, n_drop=10, date_="09-08-2025")
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_9_1)
}

local_gwr_PML_coef_map_by_F1_y("hyd_source")
local_gwr_PML_coef_map_by_F1_y("hyd_destination")
local_gwr_PML_coef_map_by_F1_y("base_source")
local_gwr_PML_coef_map_by_F1_y("base_destination")


# read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv", "hyd_source")) %>% as_tibble
# read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv", "base_source")) %>% as_tibble
# read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv", "base_destination")) %>% as_tibble

# read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv", "hyd_source")) %>% as_tibble
# read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv", "base_source")) %>% as_tibble
# read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv", "base_destination")) %>% as_tibble

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- 
  read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv", "hyd_source")) %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- 
  read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv", "hyd_destination")) %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source <- 
  read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv", "base_source")) %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest <- 
  read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv", "base_destination")) %>% as_tibble

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source %>% select(-(id:Intercept)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + ylim(-40, 60) +
  geom_boxplot(aes(x=variable, y=coefs)) -> PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box
ggsave("Colombia Data/local GWR PML result predicted prices/coef maps/local GWR PML hyd_source leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).png",
       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box, units="cm", width = 20, height = 40)

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% select(-(id:Intercept)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + ylim(-40, 60) +
  geom_boxplot(aes(x=variable, y=coefs)) -> PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box
ggsave("Colombia Data/local GWR PML result predicted prices/coef maps/local GWR PML hyd_destination leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).png",
       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box, units="cm", width = 20, height = 40)

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source %>% select(-(id:Intercept)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + ylim(-40, 60) +
  geom_boxplot(aes(x=variable, y=coefs)) -> PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box
ggsave("Colombia Data/local GWR PML result predicted prices/coef maps/local GWR PML base_source leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).png",
       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box, units="cm", width = 20, height = 40)

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest %>% select(-(id:Intercept)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + ylim(-40, 60) +
  geom_boxplot(aes(x=variable, y=coefs)) -> PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box
ggsave("Colombia Data/local GWR PML result predicted prices/coef maps/local GWR PML base_destination leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).png",
       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_coefs_box, units="cm", width = 20, height = 40)


### prediction comparison

PML_GWR_pred_loo <- function(var_drop_coef, var_drop_F1, var_drop_GWR, PML_gwr_data) {
  PML_best_F1 <- var_drop_coef %>% select(id, bw) %>% rename(bw_var_drop = bw)
  PML_best_F1$var_drop_F1 <- var_drop_F1[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, max(x, na.rm=T)))) %>% unlist
  coord_unique <- PML_gwr_data$coord %>% select(id, long, lat) %>% unique
  local_gwr_dist <- PML_gwr_data$dist
    
  PML_gwr_pi_hat_var_drop <- c()
  for (i in 1:nrow(var_drop_coef)) {
    id_i <- var_drop_coef$id[i]
    bw_i <- var_drop_coef$bw[i]
    neighbor_i <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist, PML_gwr_data)
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

PML_GWR_pred_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML hyd_source predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_destination predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_hyd_source_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML hyd_source predictions leave-one-out n_drop=10 weight 7-3.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_source_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10 weight 7-3.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_dest_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_destination predictions leave-one-out n_drop=10 weight 7-3.csv") %>% as_tibble
PML_GWR_pred_10_loo_hyd_source_9_1 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML hyd_source predictions leave-one-out n_drop=10 weight 9-1.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_source_9_1 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10 weight 9-1.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_dest_9_1 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_destination predictions leave-one-out n_drop=10 weight 9-1.csv") %>% as_tibble

PML_GWR_pred_10_loo_base_source_9_1 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10 weight 9-1.csv") %>% as_tibble
confusionMatrix(PML_GWR_pred_10_loo_base_source_9_1$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_source_9_1$y %>% as.factor, positive = "1")

CM_var_drop_10_loo_hyd_dest <- confusionMatrix(PML_GWR_pred_10_loo_hyd_dest$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_dest$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_hyd_source <- confusionMatrix(PML_GWR_pred_10_loo_hyd_source$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_source$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_dest <- confusionMatrix(PML_GWR_pred_10_loo_base_dest$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_dest$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_source <- confusionMatrix(PML_GWR_pred_10_loo_base_source$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_source$y %>% as.factor, positive = "1")

CM_var_drop_10_loo_hyd_source_7_3 <- confusionMatrix(PML_GWR_pred_10_loo_hyd_source_7_3$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_source_7_3$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_source_7_3 <- confusionMatrix(PML_GWR_pred_10_loo_base_source_7_3$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_source_7_3$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_dest_7_3 <- confusionMatrix(PML_GWR_pred_10_loo_base_dest_7_3$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_dest_7_3$y %>% as.factor, positive = "1")

CM_var_drop_10_loo_hyd_source_9_1 <- confusionMatrix(PML_GWR_pred_10_loo_hyd_source_9_1$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_source_9_1$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_source_9_1 <- confusionMatrix(PML_GWR_pred_10_loo_base_source_9_1$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_source_9_1$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_dest_9_1 <- confusionMatrix(PML_GWR_pred_10_loo_base_dest_9_1$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_dest_9_1$y %>% as.factor, positive = "1")

CM_var_drop_10_loo_hyd_dest
CM_var_drop_10_loo_hyd_source
CM_var_drop_10_loo_base_dest
CM_var_drop_10_loo_base_source

CM_var_drop_10_loo_hyd_dest
CM_var_drop_10_loo_hyd_source_7_3
CM_var_drop_10_loo_base_dest_7_3
CM_var_drop_10_loo_base_source_7_3

CM_var_drop_10_loo_hyd_source_9_1
CM_var_drop_10_loo_base_dest_9_1
CM_var_drop_10_loo_base_source_9_1

CM_var_drop_10_loo_hyd_dest$byClass
CM_var_drop_10_loo_hyd_source$byClass
CM_var_drop_10_loo_base_dest$byClass
CM_var_drop_10_loo_base_source$byClass

gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest

check_most_influential_var <- function(check_id) {
  coef_id <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == check_id)
  data_id <- gwr_data$norm %>% filter(id == check_id)
  print(t(coef_id[,-(1:3)]) * t(data_id[,-(1:3)]))
  print(municipio_centroid %>% filter(id==check_id))
}

PML_GWR_pred_10_loo_hyd_dest %>% filter(y == 0 & y_PML_var_drop_loo == 1) %>% arrange(desc(PML_gwr_pi_hat_var_drop_loo))
check_most_influential_var(27001) # population
check_most_influential_var(50287) # airport
check_most_influential_var(23419) # seizure, police

map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=id == 50287),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = map_df$long, y = map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=id == 23419),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = map_df$long, y = map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

# local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3
# {
#   load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_source leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).RData")
#   PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   PML_F1_score_hyd_source_var_drop_log_seizure_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_source leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   gwr_data <- ever_regression_data_years_price_pred("hyd_source")
#   gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
#   gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
#   gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
# 
#   PML_GWR_pred_10_loo_hyd_source <-  PML_GWR_pred_loo(var_drop_coef=PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source,
#                                                       var_drop_F1=PML_F1_score_hyd_source_var_drop_log_seizure_10_loo,
#                                                       var_drop_GWR=local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3,
#                                                       PML_gwr_data=gwr_data)
#   rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
#   write.csv(PML_GWR_pred_10_loo_hyd_source, "Colombia Data/local GWR PML result predicted prices/GWR PML hyd_source predictions leave-one-out n_drop=10 weight 7-3.csv", row.names=F)
# }
# {
#   load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).RData")
#   PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
#   gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
#   gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
#   gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
#   
#   PML_GWR_pred_10_loo_hyd_dest <-  PML_GWR_pred_loo(var_drop_coef=PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest,
#                                                     var_drop_F1=PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo,
#                                                     var_drop_GWR=local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3,
#                                                     PML_gwr_data=gwr_data)
#   rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
#   write.csv(PML_GWR_pred_10_loo_hyd_dest, "Colombia Data/local GWR PML result predicted prices/GWR PML hyd_destination predictions leave-one-out n_drop=10 weight 7-3.csv", row.names=F)
# }
# {
#   load("Colombia Data/local GWR PML result predicted prices/local GWR PML base_source leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).RData")
#   PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   PML_F1_score_base_source_var_drop_log_seizure_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML base_source leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   gwr_data <- ever_regression_data_years_price_pred("base_source")
#   gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
#   gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
#   gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
# 
#   PML_GWR_pred_10_loo_base_source <-  PML_GWR_pred_loo(var_drop_coef=PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source,
#                                                       var_drop_F1=PML_F1_score_base_source_var_drop_log_seizure_10_loo,
#                                                       var_drop_GWR=local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3,
#                                                       PML_gwr_data=gwr_data)
#   rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
#   write.csv(PML_GWR_pred_10_loo_base_source, "Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10 weight 7-3.csv", row.names=F)
# }
# {
#   load("Colombia Data/local GWR PML result predicted prices/local GWR PML base_destination leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).RData")
#   PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   PML_F1_score_base_dest_var_drop_log_seizure_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML base_destination leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 (09-08-2025).csv") %>% as_tibble
#   gwr_data <- ever_regression_data_years_price_pred("base_destination")
#   gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
#   gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
#   gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
# 
#   PML_GWR_pred_10_loo_base_dest <-  PML_GWR_pred_loo(var_drop_coef=PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest,
#                                                       var_drop_F1=PML_F1_score_base_dest_var_drop_log_seizure_10_loo,
#                                                       var_drop_GWR=local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3,
#                                                       PML_gwr_data=gwr_data)
#   rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
#   write.csv(PML_GWR_pred_10_loo_base_dest, "Colombia Data/local GWR PML result predicted prices/GWR PML base_destination predictions leave-one-out n_drop=10 weight 7-3.csv", row.names=F)
# }

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (08-20-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble 
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble 
  
## coef map with the same color range
local_gwr_PML_coef_map_by_F1_same_col <- function(coef_table, dep_var, indep_vars, n_drop, date_, all_coefs_stat, weight=F) {
  indep_vars <- c("Intercept", indep_vars)
  for (i in c(2,4:length(coef_table))) {
    var_name_ <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              coef=coef_table[[var_name_]],
                              rounded_coef=coef_table[[var_name_]] %>% round(3))
    coef_map_coords <- map_df %>% 
      left_join(gwr_coefs_i, by="id")
    
    if (i == 2) {
      gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
        geom_polygon(aes(group=group, fill=coef),
                     color = "black",
                     linewidth = 0.1) + 
        expand_limits(x = depto_map$long, y = depto_map$lat) + 
        coord_quickmap() +
        scale_fill_viridis_c(na.value = "white") +
        labs(fill=var_name_, x="", y="", title=dep_var) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
    }else{
    lower_bound <- all_coefs_stat %>% filter(var_name == var_name_) %>% pull(lower_bound)
    min_coef <- all_coefs_stat %>% filter(var_name == var_name_) %>% pull(min_coef)
    if (min_coef > lower_bound) {
      temp <- min_coef
      min_coef <- lower_bound
      lower_bound <- temp
    }
    
    upper_bound <- all_coefs_stat %>% filter(var_name == var_name_) %>% pull(upper_bound)
    max_coef <- all_coefs_stat %>% filter(var_name == var_name_) %>% pull(max_coef)
    if (max_coef < upper_bound) {
      temp <- max_coef
      max_coef <- upper_bound
      upper_bound <- temp
    }
    
    
    gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
      geom_polygon(aes(group=group, fill=coef),
                   color = "black",
                   linewidth = 0.1) + 
      expand_limits(x = depto_map$long, y = depto_map$lat) + 
      coord_quickmap() +
      scale_fill_gradientn(colors = c("blue","deepskyblue3","skyblue","grey30","yellow","red","purple"),
                           values = scales::rescale(c(min_coef, lower_bound, -0.1, 0 , 0.1, upper_bound, max_coef), from = c(min_coef, max_coef)),
                           # values = scales::rescale(c(-1, -abs(lower_bound/min_coef), -.Machine$double.eps, 0 , .Machine$double.eps, upper_bound/max_coef, 1)),
                           limits = c(min_coef, max_coef),
                           na.value = "white") +
      labs(fill=var_name_, x="", y="", title=dep_var) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank()
        )
    }
    weight_ <- ifelse(weight, " weight 7-3", "")
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s/local GWR PML coef by drop %s %s all var drop %i%s (%s).png", dep_var, var_name_, dep_var, n_drop, weight_, date_),
           gwr_coef_map, scale=1)
  }
}

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (08-20-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble

all_coefs <- bind_rows(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source_7_3,
                       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest,
                       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source_7_3,
                       PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest_7_3)
all_coefs %>% select(-(id:Intercept)) %>% apply(2, function(x) summary(x))
all_coefs %>% select(-(id:Intercept)) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "coefs") %>% 
  ggplot() + 
  geom_boxplot(aes(x=variable, y=coefs))

all_coefs_stat_tbl <- tibble(var_name = names(all_coefs)[-(1:3)],
                             min_coef = all_coefs %>% select(-(id:Intercept)) %>% apply(2, function(x) min(x, na.rm = T)),
                             max_coef = all_coefs %>% select(-(id:Intercept)) %>% apply(2, function(x) max(x, na.rm = T)),
                             lower_bound = c(-10, -5, -22, -4, -15, -25, -10, -25, -35, -6, -8, -6),
                             upper_bound = c( 10,  5,  22,  4,  15,  40,  10,  25,  35,  6,  8,  5))
indep_vars_ <- names(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source)[-(1:3)]

local_gwr_PML_coef_map_by_F1_same_col(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, dep_var="hyd_destination",
                                      indep_vars=indep_vars_, n_drop=10, date_="09-16=2025", all_coefs_stat=all_coefs_stat_tbl, weight=F)

local_gwr_PML_coef_map_by_F1_same_col(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source_7_3, dep_var="hyd_source",
                                      indep_vars=indep_vars_, n_drop=10, date_="09-16=2025", all_coefs_stat=all_coefs_stat_tbl, weight=T)

local_gwr_PML_coef_map_by_F1_same_col(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest_7_3, dep_var="base_destination",
                                      indep_vars=indep_vars_, n_drop=10, date_="09-16=2025", all_coefs_stat=all_coefs_stat_tbl, weight=T)

local_gwr_PML_coef_map_by_F1_same_col(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source_7_3, dep_var="base_source",
                                      indep_vars=indep_vars_, n_drop=10, date_="09-16=2025", all_coefs_stat=all_coefs_stat_tbl, weight=T)


coef_table <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source_7_3
dep_var="base_source"; indep_vars=indep_vars_; n_drop=10; date_="09-16=2025"; all_coefs_stat=all_coefs_stat_tbl; weight=T



GWR_y_tbl_centroid_hyd_dest <- left_join(municipio_centroid, PML_GWR_pred_10_loo_hyd_dest %>% select(id, y, y_PML_var_drop_loo), by="id")
GWR_y_tbl_centroid_hyd_source_7_3 <- left_join(municipio_centroid, PML_GWR_pred_10_loo_hyd_source_7_3 %>% select(id, y, y_PML_var_drop_loo), by="id")
GWR_y_tbl_centroid_base_source_7_3 <- left_join(municipio_centroid, PML_GWR_pred_10_loo_base_source_7_3 %>% select(id, y, y_PML_var_drop_loo), by="id")
GWR_y_tbl_centroid_base_dest_7_3 <- left_join(municipio_centroid, PML_GWR_pred_10_loo_base_dest_7_3 %>% select(id, y, y_PML_var_drop_loo), by="id")

data_map <- function(PML_GWR_pred, dep_var, n_drop) {
  
  GWR_y_tbl_map_df_FP <- left_join(map_df,
                                   PML_GWR_pred %>%
                                     mutate(pred = ifelse(y == 1, "y=1",
                                                          ifelse(y == 0 & y_PML_var_drop_loo == 1, "FP", " ")) %>% 
                                              as.factor) %>% 
                                     select(id, pred),
                                   by="id")
  
  GWR_y_tbl_map_df_TP <- left_join(map_df,
                                   PML_GWR_pred %>%
                                     mutate(pred = ifelse(y == 1 & y_PML_var_drop_loo == 0, "y=1",
                                                          ifelse(y == 1 & y_PML_var_drop_loo == 1, "TP", " ")) %>% 
                                              as.factor) %>% 
                                     select(id, pred),
                                   by="id")
  
  GWR_y_tbl_map_df_FP %>% ggplot(aes(x=long, y=lat)) +
    geom_polygon(aes(group=group, fill=pred),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = GWR_y_tbl_map_df_FP$long, y = GWR_y_tbl_map_df_FP$lat) +
    coord_quickmap() +
    labs(fill="", x="", y="", title="") +
    scale_fill_manual(
      name = dep_var,
      values = c("y=1"="deepskyblue3", "FP"="red", " "="white"),
    ) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    ) -> FP_map
  
  GWR_y_tbl_map_df_TP %>% ggplot(aes(x=long, y=lat)) +
    geom_polygon(aes(group=group, fill=pred),
                 color = "black",
                 linewidth = 0.1) +
    expand_limits(x = GWR_y_tbl_map_df_TP$long, y = GWR_y_tbl_map_df_TP$lat) +
    coord_quickmap() +
    labs(fill="", x="", y="", title="") +
    scale_fill_manual(
      name = dep_var,
      values = c("y=1"="deepskyblue3", "TP"="red", " "="white"),
    ) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    ) -> TP_map
  
  
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/prediction maps/local GWR PML FP map %s leave-one-out n_drop %i weight 7-3.png", dep_var, n_drop), FP_map, scale=1)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/prediction maps/local GWR PML TP map %s leave-one-out n_drop %i weight 7-3.png", dep_var, n_drop), TP_map, scale=1)
  
  # gwr_data <- ever_regression_data_years_price_pred(dep_var)
  # gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
  # gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
  # gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
  # 
  # for (i in c(4, 6, 12)) {
  #   var_name_ <- names(gwr_data$norm)[i]
  #   gwr_data_i <- data.frame(id=gwr_data$norm$id,
  #                            obs=gwr_data$norm[[var_name_]])
  #   data_map_coords <- map_df %>% 
  #     left_join(gwr_data_i, by="id")
  #   ggplot(data_map_coords, aes(x=long, y=lat)) + 
  #     geom_polygon(aes(group=group, fill=obs),
  #                  color = "black",
  #                  linewidth = 0.1) + 
  #     expand_limits(x = depto_map$long, y = depto_map$lat) + 
  #     coord_quickmap() +
  #     scale_fill_viridis_c(na.value = "white") +
  #     labs(fill=var_name_, x="", y="", title=dep_var) +
  #     theme_bw() +
  #     theme(panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           axis.text = element_blank(),
  #           line = element_blank()
  #     ) -> data_map_i
  #   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/data maps/%s local GWR data %s n_drop %i.png", dep_var, var_name_, n_drop),
  #          data_map_i, scale=1)
  # }
}

data_map(PML_GWR_pred_10_loo_hyd_dest, "hyd_destination", 10)
data_map(PML_GWR_pred_10_loo_hyd_source_7_3, "hyd_source", 10)
data_map(PML_GWR_pred_10_loo_base_source_7_3, "base_source", 10)
data_map(PML_GWR_pred_10_loo_base_dest_7_3, "base_destination", 10)

GWR_y_tbl_centroid <- GWR_y_tbl_centroid_hyd_dest
GWR_y_tbl_map_df <- left_join(map_df,
                              PML_GWR_pred_10_loo_hyd_dest %>%
                                mutate(y = ifelse(y == 1, "y=1",
                                                                 ifelse(y == 0 & y_PML_var_drop_loo == 1, "FP", " ")) %>% 
                                         as.factor) %>% 
                                select(id, y),
                              by="id")

GWR_y_tbl_map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=y),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = GWR_y_tbl_map_df$long, y = GWR_y_tbl_map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  scale_fill_manual(
    name = "",
    values = c("y=1"="deepskyblue3", "FP"="red", " "="white"),
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )
ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/prediction maps/local GWR PML FP map hyd_destination n_drop %i weight 7-3.png", 10), scale=1)

gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]

var_name_ <- "police"
gwr_data_i <- data.frame(id=gwr_data$norm$id,
                         obs=gwr_data$norm[[var_name_]] %>% as.factor)
data_map_coords <- map_df %>%
  left_join(gwr_data_i, by="id")
ggplot(data_map_coords, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=obs),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = depto_map$long, y = depto_map$lat) +
  coord_quickmap() +
  scale_fill_viridis_d(na.value = "white") +
  labs(fill=var_name_, x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> data_map_i
ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/data maps/local GWR data %s n_drop %i.png", var_name_, 10),
       data_map_i, scale=1)

# empty_map +
#   geom_point(data = GWR_y_tbl_centroid %>% filter(y == 1),
#              aes(x=long, y=lat, color = "true y"), size=0.2) +
#   geom_point(data = GWR_y_tbl_centroid %>% filter(y == 0 & y_PML_var_drop_loo == 1),
#              aes(x=long, y=lat, color = "FP with PML GWR"), size=0.2) +
#   scale_color_manual(
#     name = "hyd destinations",
#     values = c("true y"="blue", "FP with PML GWR"="red"),
#   ) -> FP_map
# 
# empty_map +
#   geom_point(data = GWR_y_tbl_centroid %>% filter(y == 1),
#              aes(x=long, y=lat, color = "true y"), size=0.2) +
#   geom_point(data = GWR_y_tbl_centroid %>% filter(y == 1 & y_PML_var_drop_loo == 1),
#              aes(x=long, y=lat, color = "TP with PML GWR"), size=0.2) +
#   scale_color_manual(
#     name = "hyd destinations",
#     values = c("true y"="blue", "TP with PML GWR"="red"),
#   ) -> TP_map
# 
# ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/prediction maps/local GWR PML FP map %s leave-one-out.png", dep_var), FP_map, scale=1)
# ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/prediction maps/local GWR PML TP map %s leave-one-out.png", dep_var), TP_map, scale=1)
# 
# gwr_data <- ever_regression_data_years_price_pred(dep_var_)
# gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
# gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
# gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
# 
# for (i in 4:ncol(gwr_data$norm)) {
#   var_name_ <- names(gwr_data$norm)[i]
#   gwr_data_i <- data.frame(id=gwr_data$norm$id,
#                            obs=gwr_data$norm[[var_name_]])
#   data_map_coords <- map_df %>% 
#     left_join(gwr_data_i, by="id")
#   ggplot(data_map_coords, aes(x=long, y=lat)) + 
#     geom_polygon(aes(group=group, fill=obs),
#                  color = "black",
#                  linewidth = 0.1) + 
#     expand_limits(x = depto_map$long, y = depto_map$lat) + 
#     coord_quickmap() +
#     scale_fill_viridis_c(na.value = "white") +
#     labs(fill=var_name_, x="", y="", title=dep_var) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           axis.text = element_blank(),
#           line = element_blank()
#     ) -> data_map_i
#   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/data maps/%s local GWR data %s n_drop %i.png", dep_var, var_name_, n_drop),
#          data_map_i, scale=1)
# }