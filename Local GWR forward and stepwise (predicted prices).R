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

forward_selection <- function(data_id, w=NULL, sig_level=0.05) {
  prev_data <- data_id %>% select(y)
  remaining_data <- data_id %>% select(-y)
  non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
  remaining_data <- remaining_data[,non_sigular_col_index]
  reg_model <- NULL
  
  # result <- list()
  # reg_models <- list()
  # p_values <- list()
  significance <- 1
  # k <- 1
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
    # p_values[[paste0("p_values_", k)]] <- p_values_i
    if (sum(p_values_i$p_value <= sig_level) > 0) {
      best_col_index <- which.min(p_values_i$p_value)
      prev_data <- bind_cols(prev_data, remaining_data[, best_col_index])
      remaining_data <- remaining_data[, -best_col_index]
      reg_model_test <- glm(y~., data = prev_data, weights = w, family = binomial)
      if (any(summary(reg_model_test)$coefficients[,4][-1] > sig_level)) break
      reg_model <- reg_model_test
      # reg_models[[paste0("model_", k)]] <- glm(y~., data = prev_data, family = binomial)
      # k <- k + 1
      next
    }else{
      significance <- 0
    }
    
  }
  
  # result <- list(reg_model = reg_models, forward_p_value=p_values)
  return(reg_model)
}

local_GWR_forward <- function(type.measure_="default", sig_level_=0.05, interact_=F, scale_11_=F, weight_=NULL, dep_var, gwr_forward_data_, method_) {
  bwd_range <- seq(0.5, 3, by=0.1)
  coord_unique <- gwr_forward_data_$coord
  local_gwr_dist <- gwr_forward_data_$dist %>% as.matrix
  
  cv_dev_min_mat <- cv_dev_min_mat_ 
  local_GWR_coefs_forward_result <- list()
  for (i in 1:nrow(cv_dev_min_mat)) {
    id_i <- cv_dev_min_mat$id[i]
    local_GWR_coefs_forward_result[[paste0("id_", id_i)]] <- list()
    
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      bw_name <- paste0("bw_", bw_ij)
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, coord_unique, local_gwr_dist)
      n_0_1 <- neighbor_ij$y %>% table
      
      # restrict too unbalanced responses
      if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        cv_dev_min_mat[[bw_name]][i] <- NA
        local_GWR_coefs_forward_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      if (method_ == "model drop") {
        if (nonzero_seizure[[bw_name]][i] < 5 | nonzero_coca_area[[bw_name]][i] < 5) {
          cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
          local_GWR_coefs_forward_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
          next
        }
      }
      
      if (method_ == "var drop") {
        if (nonzero_seizure[[bw_name]][i] < 5) neighbor_ij$seizures <- NULL
        if (nonzero_coca_area[[bw_name]][i] < 5) neighbor_ij$coca_area <- NULL
      }
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      forward_result_ij <- forward_selection(neighbor_ij %>% select(-id), w=weight_i, sig_level=sig_level_)
      # k <- 0
      # while (k < 1) {
      #   tryCatch(
      #     {
      #       forward_result_ij <- forward_selection(neighbor_ij %>% select(-id), w=weight_i)
      #       k <- k + 1
      #     },
      #     error = function(e) {print(e)}
      #   )
      # }
      
      # n_nonzero <- sum(forward_result_ij$forward$beta != 0)
      # if (n_nonzero == 0) {
      #   cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
      #   local_GWR_coefs_forward_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      #   next
      # }
      
      deviance_i <- ifelse(is.null(forward_result_ij$deviance), NA, forward_result_ij$deviance)
      cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- deviance_i
      forward_result_ij$model <- NULL
      forward_result_ij$data <- NULL
      local_GWR_coefs_forward_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- forward_result_ij
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(cv_dev_min_mat=cv_dev_min_mat,
              forward=local_GWR_coefs_forward_result))
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

local_GWR_stepwise <- function(type.measure_="default", sig_level_=0.05, interact_=F, scale_11_=F, weight_=NULL, dep_var, gwr_stepwise_data_, iter_limit_, method_) {
  bwd_range <- seq(0.5, 3, by=0.1)
  coord_unique <- gwr_stepwise_data_$coord
  local_gwr_dist <- gwr_stepwise_data_$dist %>% as.matrix
  
  cv_dev_min_mat <- cv_dev_min_mat_ 
  local_GWR_coefs_stepwise_result <- list()
  for (i in 1:nrow(cv_dev_min_mat)) {
    id_i <- cv_dev_min_mat$id[i]
    local_GWR_coefs_stepwise_result[[paste0("id_", id_i)]] <- list()
    
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      bw_name <- paste0("bw_", bw_ij)
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, coord_unique, local_gwr_dist)
      n_0_1 <- neighbor_ij$y %>% table
      
      # restrict too unbalanced responses
      if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        cv_dev_min_mat[[bw_name]][i] <- NA
        local_GWR_coefs_stepwise_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      if (method_ == "model drop") {
        if (nonzero_seizure[[bw_name]][i] < 5 | nonzero_coca_area[[bw_name]][i] < 5) {
          cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
          local_GWR_coefs_stepwise_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
          next
        }
      }
      
      if (method_ == "var drop") {
        if (nonzero_seizure[[bw_name]][i] < 5) neighbor_ij$seizures <- NULL
        if (nonzero_coca_area[[bw_name]][i] < 5) neighbor_ij$coca_area <- NULL
      }
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      stepwise_result_ij <- stepwise(neighbor_ij %>% select(-id), w=weight_i, sig_level=sig_level_, iter_limit=iter_limit_)
      if (is.empty.list(stepwise_result_ij$reg_model)) {
        cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_stepwise_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      }else{
        stepwise_result_ij <- stepwise_result_ij$reg_model[[length(stepwise_result_ij$reg_model)]]
        deviance_i <- ifelse(is.null(stepwise_result_ij$deviance), NA, stepwise_result_ij$deviance)
        cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- deviance_i
        stepwise_result_ij$model <- NULL
        stepwise_result_ij$data <- NULL
        local_GWR_coefs_stepwise_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- stepwise_result_ij
      }
      
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(cv_dev_min_mat=cv_dev_min_mat,
              stepwise=local_GWR_coefs_stepwise_result))
}

cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_dest cv min dev (03-07-2025).csv") %>% as_tibble
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
  }

gwr_forward_data <- ever_regression_data_years_price_pred("hyd_destination")
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

# stepwise
gwr_forward_data$norm$seizures <- gwr_forward_data$norm %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% select(-id) %>% pull(seizures)

set.seed(100)
start.time <- Sys.time()
local_GWR_coefs_step_hyd_dest_list <- local_GWR_stepwise(dep_var = "hyd_destination", gwr_stepwise_data_ = gwr_forward_data,
                                                         method_="model drop", iter_limit_ = 40, sig_level_ = 0.1)
end.time <- Sys.time()
end.time - start.time # 1.154734 hours for hyd_destination model drop

local_GWR_coefs_step_hyd_dest_model_drop <- local_GWR_coefs_step_hyd_dest_list$stepwise
write.csv(local_GWR_coefs_step_hyd_dest_list$cv_dev_min_mat, "Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price cv min dev model drop (05-09-2025).csv", row.names = F)
save("local_GWR_coefs_step_hyd_dest_model_drop", file = "Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price model drop (05-09-2025).RData")
rm(local_GWR_coefs_step_hyd_dest_model_drop); rm(local_GWR_coefs_step_hyd_dest_list)

set.seed(5640)
start.time <- Sys.time()
local_GWR_coefs_step_hyd_dest_list <- local_GWR_stepwise(dep_var = "hyd_destination", gwr_stepwise_data_ = gwr_forward_data,
                                                         method_="var drop", iter_limit_ = 40, sig_level_ = 0.1)
end.time <- Sys.time()
end.time - start.time # 54.86374 mins for hyd_destination var drop

local_GWR_coefs_step_hyd_dest_var_drop <- local_GWR_coefs_step_hyd_dest_list$stepwise
write.csv(local_GWR_coefs_step_hyd_dest_list$cv_dev_min_mat, "Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price cv min dev var drop (05-09-2025).csv", row.names = F)
save("local_GWR_coefs_step_hyd_dest_var_drop", file = "Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price var drop (05-09-2025).RData")
rm(local_GWR_coefs_step_hyd_dest_var_drop); rm(local_GWR_coefs_step_hyd_dest_list)

## forward selection
set.seed(100)
start.time <- Sys.time()
local_GWR_coefs_forward_hyd_dest_list <- local_GWR_forward(dep_var = "hyd_destination", gwr_forward_data_ = gwr_forward_data, sig_level_=0.1)
end.time <- Sys.time()
end.time - start.time # 1.043645 hours for hyd_destination (alpha = 0.05)
end.time - start.time # 56.69292 mins for hyd_destination (alpha = 0.1)
end.time - start.time # 1.062456 hours for hyd_destination drop (alpha = 0.1)

local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop <- local_GWR_coefs_forward_hyd_dest_list$forward
write.csv(local_GWR_coefs_forward_hyd_dest_list$cv_dev_min_mat, "Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev alpha = 0.1 drop (04-21-2025).csv", row.names = F)
save("local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop", file = "Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price alpha = 0.1 drop (04-21-2025).RData")
rm(local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop); rm(local_GWR_coefs_forward_hyd_dest_list)

    # alpha = 0.1
local_GWR_coefs_forward_hyd_dest_alpha_0.1 <- local_GWR_coefs_forward_hyd_dest_list$forward
write.csv(local_GWR_coefs_forward_hyd_dest_list$cv_dev_min_mat, "Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev alpha = 0.1 (04-09-2025).csv", row.names = F)
save("local_GWR_coefs_forward_hyd_dest_alpha_0.1", file = "Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price alpha = 0.1 (04-09-2025).RData")
rm(local_GWR_coefs_forward_hyd_dest); rm(local_GWR_coefs_forward_hyd_dest_alpha_0.1); rm(local_GWR_coefs_forward_hyd_dest_list)


# coef map
local_gwr_forward_coef_map <- function(local_GWR_coefs_forward_list, cv_dev_min_mat, dep_var, indep_vars, alpha=0.05, weight_=NULL) {
  optimal_bw <- gsub("bw_", "",
                     cv_dev_min_mat[,-1] %>% apply(1, function(x) ifelse(sum(!is.na(x)) == 0, NA, bwd_range[which.min(x)]))
  ) %>% as.numeric
  
  indep_vars <- c("Intercept", indep_vars)
  forward_coef_table <- tibble(id = cv_dev_min_mat$id, bw=optimal_bw)
  forward_coef_mat <- matrix(NA, nrow(forward_coef_table), length(indep_vars))
  
  indep_vars_df <- data.frame(var_name=indep_vars)
  for (i in 1:nrow(forward_coef_table)) {
    bw_i <- forward_coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_forward_i <- local_GWR_coefs_forward_list[[i]][[paste0("bw_", bw_i)]]
    coef_i <- coef(local_GWR_forward_i)
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
    forward_coef_mat[i,] <- left_join(indep_vars_df, coef_i_df, by="var_name")$coef
  }
  
  forward_coef_table <- bind_cols(forward_coef_table, forward_coef_mat)
  names(forward_coef_table)[-(1:2)] <- indep_vars
  # if (!is.null(weight_)) interact_ <- " weight"
  write.csv(forward_coef_table,
            paste0("Colombia Data/local GWR forward result predicted prices/local GWR forward coefs drop alpha=", alpha, " ", dep_var, weight_, " (04-21-2025).csv"),
            row.names = F)
  
  
  for (i in c(2, 4:length(forward_coef_table))) {
    var_name <- names(forward_coef_table)[i]
    gwr_coefs_i <- data.frame(id=forward_coef_table$id,
                              coef=forward_coef_table[[var_name]],
                              rounded_coef=forward_coef_table[[var_name]] %>% round(3))
    min_coef <- min(gwr_coefs_i$coef, na.rm=T)
    max_coef <- max(gwr_coefs_i$coef, na.rm=T)
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
        labs(fill=var_name, x="", y="", title=dep_var) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
    }else{
      gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
        geom_polygon(aes(group=group, fill=coef),
                     color = "black",
                     linewidth = 0.1) + 
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
    }
    
    ggsave(paste0("Colombia Data/local GWR forward result predicted prices/coef maps/",
                  dep_var, "/local GWR forward coef map drop ", var_name, " alpha=", alpha, " ", dep_var, weight_, " (04-21-2025).png"),
           gwr_coef_map, scale=1)
  }
}


bwd_range <- seq(0.5, 3, by=0.1)
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

local_gwr_lasso_coefs_hyd_destination <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward coefs limited alpha=0.1 hyd_destination (04-15-2025).csv") %>%
  as_tibble %>% arrange(id)
indep_vars_ <- names(local_gwr_lasso_coefs_hyd_destination)[-(1:3)]

cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price (04-09-2025).RData") # local_GWR_coefs_forward_hyd_dest
local_gwr_forward_coef_map(local_GWR_coefs_forward_hyd_dest, cv_dev_min_mat_, "hyd_destination", indep_vars_);# rm(local_GWR_coefs_forward_hyd_dest)
# local_GWR_coefs_forward_list<-local_GWR_coefs_forward_hyd_dest; cv_dev_min_mat<-cv_dev_min_mat_; dep_var="hyd_destination"; weight_=NULL

  ## alpha = 0.1
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev alpha = 0.1 (04-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price alpha = 0.1 (04-09-2025).RData") # local_GWR_coefs_forward_hyd_dest_alpha_0.1
local_gwr_forward_coef_map(local_GWR_coefs_forward_hyd_dest_alpha_0.1, cv_dev_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_forward_hyd_dest_alpha_0.1)
# local_GWR_coefs_forward_list<-local_GWR_coefs_forward_hyd_dest_alpha_0.1; cv_dev_min_mat<-cv_dev_min_mat_; indep_vars<-indep_vars_; dep_var="hyd_destination"; weight_=NULL; alpha<-0.1

  ## alpha = 0.1 variable drop
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev alpha = 0.1 drop (04-21-2025).csv") %>% as_tibble
load("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price alpha = 0.1 drop (04-21-2025).RData") # local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop
local_gwr_forward_coef_map(local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop, cv_dev_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop)

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
   if (is.na(cv_dev_min_mat_[[col_name_ij]][i])) next
   neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_=F, coord_unique, local_gwr_dist)
   nonzero_seizure[[col_name_ij]][i] <- sum(round(neighbor_ij$seizures, 3) > min_seizure_scaled)
   nonzero_coca_area[[col_name_ij]][i] <- sum(round(neighbor_ij$coca_area, 3) > min_coca_area_scaled)
 }
}
nonzero_seizure
sum(nonzero_seizure < 5, na.rm = T)

nonzero_seizure %>% filter(id == 85136)
neighbor_85136 <- neighbor_id(85136, 0.7, scale_11_=F, coord_unique, local_gwr_dist)
neighbor_85136$seizures %>% round(3)
neighbor_85136$coca_area %>% round(3)
regression_data_years_price_pred %>% filter(id %in% neighbor_85136$id) %>% pull(hyd_seizures)
regression_data_years_price_pred %>% filter(id %in% neighbor_85136$id) %>% pull(coca_area)
gwr_forward_data$norm %>% filter(id %in% neighbor_85136$id) %>% pull(seizures)

neighbor_85136_seizure <- regression_data_years_price_pred %>% filter(id %in% neighbor_85136$id) %>% group_by(id) %>%
  mutate(seizure = sum(hyd_seizures),
         coca_area = max(coca_area)) %>%
  select(id, seizure, coca_area) %>% 
  left_join(neighbor_85136 %>% select(-seizures, -coca_area), by="id")

glm(y~seizure+coca_area, neighbor_85136_seizure, family=binomial) %>% summary

## overfitting check
local_gwr_forward_coefs_hyd_destination <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward coefs hyd_destination (04-03-2025).csv") %>%
  as_tibble %>% arrange(id)

local_gwr_forward_coefs_hyd_destination %>% select(-(id:Intercept)) %>% apply(2, function(x) sum(!is.na(x))) # number of municipios where each indep. variable is in model

local_gwr_forward_coefs_hyd_destination %>% filter(coca_area < -10000 | lab_prob > 4000)
local_gwr_forward_coefs_hyd_destination %>% filter(lab_prob > 1000)
id_high_lab_prob <- local_gwr_forward_coefs_hyd_destination %>% filter(lab_prob > 1000)

for (i in 1:nrow(id_high_lab_prob)) {
  print(local_GWR_coefs_forward_hyd_dest[[paste0("id_", id_high_lab_prob$id[i])]][[paste0("bw_", id_high_lab_prob$bw[i]+0.1)]])
}


local_GWR_coefs_forward_hyd_dest$id_85136
cv_dev_min_mat_ %>% filter(id == 85136)

id_index <- which(gwr_forward_data$coord$id == 85136)
data_id_ <- gwr_forward_data$norm %>% 
  filter(id %in% gwr_forward_data$coord$id[which(gwr_forward_data$dist[id_index,] <= local_gwr_forward_coefs_hyd_destination$bw[id_index])])

glm_id <- local_GWR_coefs_forward_hyd_dest$id_85136$bw_0.7
glm_id %>% summary
p <- 0.7
# glm(y~., data_id_ %>% select(-id, -municipio), family = binomial, weights = ifelse(data_id_$y == 1, p, 1-p)) %>% summary
data_id_$seizures %>% table
confusionMatrix(data_id_$y, ifelse(glm_id$fitted.values < 0.5, 0, 1) %>% as.factor)
data_id_ %>% ggplot() + geom_point(aes(x=1:nrow(data_id_), y=seizures)) + ggtitle("id=85136 seizures")
cv_dev_min_mat_ %>% filter(id == 85136)
local_GWR_coefs_forward_hyd_dest$id_85136$bw_0.8 %>% summary

local_GWR_coefs_forward_hyd_dest_id <- local_GWR_coefs_forward_hyd_dest$id_85136$bw_0.7
local_GWR_coefs_forward_hyd_dest_id %>% summary
pred_id <- predict(local_GWR_coefs_forward_hyd_dest_id,
                   data_id_ %>% select(-(id:y)),
                   type="response")
ggplot(data.frame(y = data_id_$y, pred = pred_id)) +
  geom_point(aes(x=pred, y=y)) + ggtitle("predicted prob. GWR id=73770")

id_high_lab_prob
cv_dev_min_mat_ %>% filter(id == 25488)

id_index <- which(gwr_forward_data$coord$id == 25488)
data_id_ <- gwr_forward_data$norm %>% 
  filter(id %in% gwr_forward_data$coord$id[which(gwr_forward_data$dist[id_index,] <= local_gwr_forward_coefs_hyd_destination$bw[id_index])])

glm_id <- local_GWR_coefs_forward_hyd_dest$id_25488$bw_0.5
glm_id %>% summary
local_GWR_coefs_forward_hyd_dest$id_25488$bw_0.6 %>% summary
p <- 0.7
# glm(y~., data_id_ %>% select(-id, -municipio), family = binomial, weights = ifelse(data_id_$y == 1, p, 1-p)) %>% summary
data_id_$seizures %>% table
confusionMatrix(data_id_$y, ifelse(glm_id$fitted.values < 0.5, 0, 1) %>% as.factor)
data_id_ %>% ggplot() + geom_point(aes(x=1:nrow(data_id_), y=seizures)) + ggtitle("id=85136 seizures")

forward_selection(data_id_[,-(1:2)]) %>% summary
glm(y~., data_id_ %>% select(y, airport, lab_prob), family = binomial) %>% summary
glm(y~., data_id_ %>% select(y, airport, lab_prob, armed_group), family = binomial) %>% summary
