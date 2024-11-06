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
library(randomForest)
library(pracma)
library(GWmodel)
library(pROC)
# library(ROCR)
library(glmnet)

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

n_reg_data_mat <- read.csv("Colombia Data/local GWR number of neighbors (08-13-2024).csv") %>% as_tibble
aic_score_mat <- read.csv("Colombia Data/local GWR AIC (08-13-2024).csv") %>% as_tibble
local_GWR_coefs_bw <- read.csv("Colombia Data/local GWR best coefs (08-13-2024).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs (10-16-2024).csv") %>% as_tibble

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

### armed_group = 1 if there is at lease 1 armed group, and 0 otherwise 
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

bwd_range <- seq(0.5, 4, by=0.1)
local_gwr_data_id <- gwr_data_hyd_destination %>% select(id)
local_gwr_data <- gwr_data_hyd_destination %>% select(-id, -municipio, -year)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)

local_gwr_forward_coefs <- local_GWR_coefs_bw %>% 
  select(-n_neighbors, -n_NA, -n_PPI_labs, -n_hyd_labs, -erad_manual, -hyd_group) %>%
  rename(armed_group=n_armed_groups)
local_gwr_forward_models <- list()
local_gwr_performance <- tibble()
sig_level <- 0.1
for (i in 1:nrow(local_gwr_forward_coefs)) {
  id_i <- local_gwr_forward_coefs$id[i]
  bw_i <- local_gwr_forward_coefs$bw[i]
  id_i_index <- which(coord_unique$id == id_i)
  neighbors_i <- gwr_data_hyd_destination %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-(id:year))
  
  if (table(neighbors_i$hyd_destination)[2] == 0) {
    local_gwr_forward_models[[paste0("id_", id_i)]] <- NA
    local_gwr_forward_coefs[i,] <- c(id_i, bw_i, rep(NA, 13)) %>% matrix(nrow=1) %>% as_tibble
    performance_i <- c(id_i, rep(NA, 5))
    names(performance_i) <- c("id", "n_var", "AUC", "threshold", "specificity", "sensitivity")
    local_gwr_performance <- bind_rows(local_gwr_performance, performance_i)
    next
  }
  
  prev_data <- neighbors_i %>% select(hyd_destination)
  remaining_data <- neighbors_i %>% select(-hyd_destination)
  non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
  remaining_data <- remaining_data[,non_sigular_col_index]
  
  significance <- 1
  while (significance) {
    p_values_i <- tibble()
    for (j in 1:ncol(remaining_data)) {
      new_var_j <- remaining_data[,j]
      reg_data_j <- bind_cols(prev_data, new_var_j)
      reg_model_j <- glm(hyd_destination~.,
                         data = reg_data_j,
                         family = binomial)
      var_name_j <- names(new_var_j)
      reg_model_coefs_j <- summary(reg_model_j)$coefficients
      p_value_j <- reg_model_coefs_j[which(rownames(reg_model_coefs_j) == var_name_j), 4]
      p_value_j <- ifelse(p_value_j == 0, 1, p_value_j)
      p_values_i <- bind_rows(p_values_i, tibble(var_name=var_name_j, p_value=p_value_j))
    }
    if (sum(p_values_i$p_value <= sig_level) > 0) {
      best_col_index <- which.min(p_values_i$p_value)
      prev_data <- bind_cols(prev_data, remaining_data[, best_col_index])
      remaining_data <- remaining_data[, -best_col_index]
      next
    }else{
      significance <- 0
    }
  }
  reg_model_i <- glm(hyd_destination~.,
                     data = prev_data,
                     family = binomial)
  local_gwr_forward_models[[paste0("id_", id_i)]] <- reg_model_i
  roc_i <- roc(prev_data$hyd_destination, reg_model_i$fitted.values)
  performance_i <- c(id_i, ncol(prev_data)-1, auc(roc_i), (coords(roc_i, "best"))[1,] %>% unlist)
  names(performance_i) <- c("id", "n_var", "AUC", "threshold", "specificity", "sensitivity")
  local_gwr_performance <- bind_rows(local_gwr_performance, performance_i)
  local_gwr_forward_coefs$bw[i] <- bw_i
  if (i %% 100 == 0) print(paste(i, "complete"))
}

local_gwr_forward_pvals <- local_gwr_forward_coefs
var_table <- tibble(var_name=names(local_gwr_forward_coefs)[-(1:2)])
for (i in 1:nrow(local_gwr_forward_coefs)) {
  id_i <- local_gwr_forward_coefs$id[i]
  bw_i <- local_gwr_forward_coefs$bw[i]
  reg_model_i <- local_gwr_forward_models[[paste0("id_", id_i)]]
  
  if (is.na(reg_model_i[1])) next
  
  coefs_i <- tibble(var_name = names(reg_model_i$coefficients),
                    coef = reg_model_i$coefficients,
                    p_val = summary(reg_model_i)$coefficients[,4])
  coefs_i[1,1] <- "Intercept"
  coefs_i <- left_join(var_table, coefs_i, by="var_name")
  local_gwr_forward_coefs[i,] <- c(id_i, bw_i, coefs_i$coef) %>% matrix(ncol=ncol(local_gwr_forward_coefs)) %>% as_tibble
  local_gwr_forward_pvals[i,] <- c(id_i, bw_i, coefs_i$p_val) %>% matrix(ncol=ncol(local_gwr_forward_pvals)) %>% as_tibble
}

local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
local_gwr_forward_pvals <- read.csv("Colombia Data/local GWR best p-values forward selection (10-29-2024).csv") %>% as_tibble
local_gwr_performance <- read.csv("Colombia Data/local GWR performance forward selection (10-29-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result forward selection (10-29-2024).RData")

local_gwr_performance
local_gwr_forward_coefs
local_gwr_forward_pvals
# save("local_gwr_forward_models", file = "Colombia Data/local GWR result forward selection (10-29-2024).RData")
# local_gwr_performance %>% write.csv("Colombia Data/local GWR performance forward selection (10-29-2024).csv", row.names=F)
# local_gwr_forward_coefs %>% write.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv", row.names=F)
# local_gwr_forward_pvals %>% write.csv("Colombia Data/local GWR best p-values forward selection (10-29-2024).csv", row.names=F)