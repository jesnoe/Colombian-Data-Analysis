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
library(distances)
########## used bandwidth range 0.5~3.0 due to the time limit
{
  binary_vars <- c("y", "airport", "armed_group", "ferry", "police", "military")
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

local_GWR_PML <- function(type.measure_="default", cv_aic_min_mat, sig_level_=0.05, gwr_PML_data_, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var, n_data_neighbors) {
  bwd_range <- seq(0.5, 3, by=0.1)
  coord_unique <- gwr_PML_data_$coord
  local_gwr_dist <- gwr_PML_data_$dist %>% as.matrix
  model_vars <- names(gwr_PML_data_$norm)[-(1:3)]
  model_vars_continous <- model_vars[-which(model_vars %in% binary_vars)]
  data_neighbors_check_mat <- cv_aic_min_mat
  data_neighbors_check_mat[,-1] <- NA
  
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
      
      gwr_data_ij <- gwr_PML_data_$norm %>% filter(id == id_i)
      gwr_data_ij_min <- neighbor_ij %>% select(all_of(model_vars_continous)) %>% apply(2, function(x) min(x))
      gwr_data_ij_max <- neighbor_ij %>% select(all_of(model_vars_continous)) %>% apply(2, function(x) max(x))
      gwr_data_ij_range <- gwr_data_ij_max - gwr_data_ij_min
      gwr_data_ij_continous <- gwr_data_ij %>% select(all_of(model_vars_continous))
      gwr_data_ij_vec <- gwr_data_ij_continous %>% t %>% as.vector
      data_distance_weights <- bind_rows(gwr_data_ij_continous, gwr_data_ij_min, gwr_data_ij_max, gwr_data_ij_range) %>% 
        apply(2, function(x) ifelse(x[1] < x[2], 1+(x[2] - x[1])/x[4],
                                    ifelse(x[1] > x[3], 1+(x[1] - x[3])/x[4], 1))) # within_range_weight = 1
      if (mean(data_distance_weights) > 1) { # add data neighbors only when a focal point data is away from geographical neighbors
        data_neighbors_check_mat[[bw_name]][i] <- 1
        gwr_data_dist <- distances(gwr_PML_data_$norm %>% select(all_of(model_vars_continous)) %>% as.data.frame, weights = data_distance_weights) %>% as.matrix
        
        data_neighbor_ij_id <- gwr_PML_data_$norm$id[order(gwr_data_dist[j,] %>% t)[2:(n_data_neighbors+1)]]
        data_neighbor_ij <- gwr_PML_data_$norm %>% filter(id %in% data_neighbor_ij_id)
        neighbor_ij <- bind_rows(neighbor_ij, data_neighbor_ij) %>% unique
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
  
  return(list(cv_aic_min_mat = cv_aic_min_mat,
              PML = local_GWR_coefs_PML_result,
              data_neighbors = data_neighbors_check_mat))
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
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
cv_aic_min_mat_[,-1] <- NA
bwd_range <- seq(0.5, 3, by=0.1)

start.time <- Sys.time()
set.seed(100)
local_GWR_coefs_PML_hyd_dest_list <- local_GWR_PML(dep_var = "hyd_destination", cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="var drop", sig_level_ = 0.1,
                                                   n_drop = 10, weight_ = NULL, n_data_neighbors = 10)
end.time <- Sys.time()
end.time - start.time # 48.21265 mins for hyd_destination
local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors <- local_GWR_coefs_PML_hyd_dest_list$PML
write.csv(local_GWR_coefs_PML_hyd_dest_list$cv_aic_min_mat,
          "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out aic all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).csv", row.names = F)
write.csv(local_GWR_coefs_PML_hyd_dest_list$data_neighbors,
          "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out data neighbors all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).csv", row.names = F)
save("local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors",
     file = "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).RData")
# rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors)
rm(local_GWR_coefs_PML_hyd_dest_list)

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

# local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).RData")
PML_gwr_aic_var_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out aic all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).csv") %>% as_tibble

PML_F1_score_var_drop_log_seizure <- PML_F1_score(local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors, PML_gwr_aic_var_drop_log_seizure_coca)
dep_var_ <- "hyd_destination"
# write.csv(PML_F1_score_var_drop_log_seizure,
#           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).csv", dep_var_),
#           row.names = F)

###### PML coefs
PML_F1_score_var_drop_log_seizure <- read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).csv", dep_var_)) %>% as_tibble
PML_data_neighbors_var_drop_log_seizure <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out data neighbors all var drop log seizure coca scaled n_drop=10 n_data_neighbors=10 (11-15-2025).csv") %>% as_tibble
PML_F1_score_var_drop_log_seizure

bwd_range
indep_vars <- names(gwr_data$norm)[-(1:3)]
best_bw <- c()
data_neighbor_used <- c()
coef_table <- tibble(id = PML_F1_score_var_drop_log_seizure$id)
indep_vars_df <- data.frame(var_name=c("Intercept", indep_vars))
coef_mat <- matrix(NA, nrow(PML_F1_score_var_drop_log_seizure), length(indep_vars)+1)
for (i in 1:nrow(PML_F1_score_var_drop_log_seizure)) {
  id_i <- PML_F1_score_var_drop_log_seizure$id[i]
  if (sum(!is.na(PML_F1_score_var_drop_log_seizure[i,-1])) == 0) {
    best_bw <- c(best_bw, NA)
    data_neighbor_used <- c(data_neighbor_used, NA)
    next
  }
  
  bw_i <- bwd_range[which.max(PML_F1_score_var_drop_log_seizure[i,-1])]
  best_bw <- c(best_bw, bw_i)
  data_neighbor_used <- c(data_neighbor_used, (PML_data_neighbors_var_drop_log_seizure %>% filter(id == id_i))[[paste0("bw_", bw_i)]])
  local_GWR_model_i <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  coef_i <- coef(local_GWR_model_i)
  coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
  coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
  coef_mat[i,] <- coef_i_df$coef
  
}
coef_table <- bind_cols(coef_table, coef_mat)
names(coef_table)[-1] <- c("Intercept", indep_vars)
coef_table <- coef_table %>% mutate(bw = best_bw, data_neighbor_used = data_neighbor_used) %>% relocate(id, bw, data_neighbor_used)
coef_table


# local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out all var drop log seizure coca scaled n_drop=10 (08-20-2025).RData")

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (08-20-2025).csv") %>% as_tibble
coef_table_forced_DN <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest
coef_mat_foced_DN <- coef_table_forced_DN %>% select(-id, -bw) %>% as.matrix
data_neighbor_used_2 <- numeric(nrow(coef_table_forced_DN))
indep_vars <- names(gwr_data$norm)[-(1:3)]
indep_vars_df <- data.frame(var_name=c("Intercept", indep_vars))
hyd_gwr_data <- gwr_data$norm
gwr_data_dist <- gwr_data$dist
n_neighbors <- 5
for (i in 1:nrow(coef_table_forced_DN)) {
  id_i <- coef_table_forced_DN$id[i]
  j <- which(hyd_gwr_data$id == id_i)
  bw_i <- coef_table_forced_DN$bw[i]
  if (is.na(bw_i)) next
  
  neighbors <- hyd_gwr_data[which(gwr_data_dist[j,] <= bw_i),] %>% filter(id != id_i)
  hyd_gwr_data_i <- hyd_gwr_data %>% filter(id == id_i)
  
  local_GWR_i <- local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  model_vars_i <- (local_GWR_i$coefficients %>% names)[-1]
  if (sum(model_vars_i %in% binary_vars) > 0) {
    model_vars_i_continous <- model_vars_i[-which(model_vars_i %in% binary_vars)]
  }else{
    model_vars_i_continous <- model_vars_i
  }
  hyd_gwr_data_i_min <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) min(x))
  hyd_gwr_data_i_max <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) max(x))
  hyd_gwr_data_i_range <- hyd_gwr_data_i_max - hyd_gwr_data_i_min
  hyd_gwr_data_i_continous <- hyd_gwr_data_i %>% select(all_of(model_vars_i_continous))
  hyd_gwr_data_i_vec <- hyd_gwr_data_i_continous %>% t %>% as.vector
  data_distance_weights <- bind_rows(hyd_gwr_data_i_continous, hyd_gwr_data_i_min, hyd_gwr_data_i_max, hyd_gwr_data_i_range) %>% 
    apply(2, function(x) ifelse(x[1] < x[2], 1+(x[2] - x[1])/x[4],
                                ifelse(x[1] > x[3], 1+(x[1] - x[3])/x[4], 1)))
  
  if (mean(data_distance_weights) > 1) {
    hyd_gwr_data_dist <- distances(hyd_gwr_data %>% select(all_of(model_vars_i_continous)) %>% as.data.frame, weights = data_distance_weights) %>% as.matrix
    
    data_neighbors_id <- hyd_gwr_data$id[order(hyd_gwr_data_dist[j,] %>% t)[2:(n_neighbors+1)]]
    data_neighbors <- hyd_gwr_data %>% filter(id %in% data_neighbors_id)
    neighbors_i <- bind_rows(neighbors, data_neighbors)
    
    local_GWR_DN_model_i <- logistf(y~., neighbors_i %>% select(y, all_of(model_vars_i)))
    local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]] <- local_GWR_DN_model_i
    coef_i <- coef(local_GWR_DN_model_i)
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
    coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
    coef_mat_foced_DN[i,] <- coef_i_df$coef
    data_neighbor_used_2[i] <- 1
  }
}
coef_table_forced_DN <- bind_cols(coef_table_forced_DN %>% select(id, bw), coef_mat_foced_DN) %>% mutate(data_neighbor_used = data_neighbor_used_2) %>% relocate(id, bw, data_neighbor_used)
coef_table_forced_DN$data_neighbor_used %>% table


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

{
  gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
  gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
  gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
  gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
  
  PML_GWR_pred_10_loo_hyd_dest_DN <-  PML_GWR_pred_loo(var_drop_coef=coef_table,
                                                       var_drop_F1=PML_F1_score_var_drop_log_seizure,
                                                       var_drop_GWR=local_GWR_coefs_PML_var_drop_log_seizure_scaled_data_neighbors,
                                                       PML_gwr_data=gwr_data)
  # write.csv(PML_GWR_pred_10_loo_hyd_dest_DN, "Colombia Data/local GWR PML result predicted prices/GWR PML hyd_destination predictions leave-one-out n_drop=10 n_data_neighbors=10.csv", row.names=F)
}

{
  gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
  gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
  gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
  gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
  
  PML_GWR_pred_10_loo_hyd_dest_forced_DN <-  PML_GWR_pred_loo(var_drop_coef=coef_table_forced_DN,
                                                              var_drop_F1=PML_F1_score_var_drop_log_seizure,
                                                              var_drop_GWR=local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled,
                                                              PML_gwr_data=gwr_data)
  # write.csv(PML_GWR_pred_10_loo_hyd_dest_DN, "Colombia Data/local GWR PML result predicted prices/GWR PML hyd_destination predictions leave-one-out n_drop=10 n_data_neighbors=10.csv", row.names=F)
}

PML_GWR_pred_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_hyd_dest
PML_GWR_pred_10_loo_hyd_dest_DN
sum(PML_GWR_pred_10_loo_hyd_dest$bw_var_drop != PML_GWR_pred_10_loo_hyd_dest_DN$bw_var_drop, na.rm=T) # 257
sum(PML_GWR_pred_10_loo_hyd_dest$bw_var_drop > PML_GWR_pred_10_loo_hyd_dest_DN$bw_var_drop, na.rm=T) # 0
sum(PML_GWR_pred_10_loo_hyd_dest$bw_var_drop < PML_GWR_pred_10_loo_hyd_dest_DN$bw_var_drop, na.rm=T) # 257

CM_var_drop_10_loo_hyd_dest <- confusionMatrix(PML_GWR_pred_10_loo_hyd_dest$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_dest$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_hyd_dest_DN <- confusionMatrix(PML_GWR_pred_10_loo_hyd_dest_DN$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_dest_DN$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_hyd_dest_forced_DN <- confusionMatrix(PML_GWR_pred_10_loo_hyd_dest_forced_DN$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_dest_forced_DN$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_hyd_dest
CM_var_drop_10_loo_hyd_dest_DN
CM_var_drop_10_loo_hyd_dest_forced_DN

id_data_neighbors_index <- which(coef_table_forced_DN$data_neighbor_used == 1)
confusionMatrix(PML_GWR_pred_10_loo_hyd_dest$y_PML_var_drop_loo[id_data_neighbors_index] %>% as.factor, PML_GWR_pred_10_loo_hyd_dest$y[id_data_neighbors_index] %>% as.factor, positive = "1")
confusionMatrix(PML_GWR_pred_10_loo_hyd_dest_forced_DN$y_PML_var_drop_loo[id_data_neighbors_index] %>% as.factor, PML_GWR_pred_10_loo_hyd_dest_forced_DN$y[id_data_neighbors_index] %>% as.factor, positive = "1")

PML_GWR_pred_10_loo_hyd_dest_forced_DN_DN_samples <- PML_GWR_pred_10_loo_hyd_dest_forced_DN[id_data_neighbors_index,] %>% select(id, y, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop_loo)
names(PML_GWR_pred_10_loo_hyd_dest_forced_DN_DN_samples)[-1] <- paste0(names(PML_GWR_pred_10_loo_hyd_dest_forced_DN_DN_samples)[-1], "_DN")
PML_GWR_pred_10_loo_hyd_dest_DN_samples <- left_join(PML_GWR_pred_10_loo_hyd_dest[id_data_neighbors_index,] %>% select(id, y, y_PML_var_drop_loo, PML_gwr_pi_hat_var_drop_loo), 
                                                     PML_GWR_pred_10_loo_hyd_dest_forced_DN_DN_samples, by="id")

PML_GWR_pred_10_loo_hyd_dest_DN_samples %>% filter(y_PML_var_drop_loo != y_PML_var_drop_loo_DN) %>% select(-y_DN) %>%
  relocate(id, y, y_PML_var_drop_loo, y_PML_var_drop_loo_DN) %>% arrange(y, y_PML_var_drop_loo)
gwr_data$norm %>% filter(id %in% (PML_GWR_pred_10_loo_hyd_dest_DN_samples %>% filter(y_PML_var_drop_loo != y_PML_var_drop_loo_DN) %>% pull(id))) %>% print(n=17)

gwr_data$norm$population %>% summary
