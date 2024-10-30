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
aic_score_mat <- read.csv("Colombia Data/local GWR AIC without lab counts (10-28-2024).csv") %>% as_tibble
local_GWR_coefs_bw <- read.csv("Colombia Data/local GWR best coefs without lab counts (10-28-2024).csv") %>% as_tibble
local_GWR_pvals_bw <- read.csv("Colombia Data/local GWR best p-values without lab counts (10-28-2024).csv") %>% as_tibble
# load("Colombia Data/local GWR result (08-13-2024).RData")

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

ever_anecdotal <- regression_data_years %>% 
  group_by(id) %>% 
  summarise(base_source=ifelse(sum(base_source)>0, 1, 0),
            base_destination=ifelse(sum(base_destination)>0, 1, 0),
            hyd_source=ifelse(sum(hyd_source)>0, 1, 0),
            hyd_destination=ifelse(sum(hyd_destination)>0, 1, 0))

ever_anecdotal_data_years <- regression_data_years %>% 
  select(-(base_source_all:general_destination)) %>% 
  left_join(ever_anecdotal, by="id")

gwr_hyd_destination_coord <- left_join(ever_anecdotal_data_years %>%
                                         select(id, year, n_PPI_labs:population, airport, hyd_destination, -base_avg, -base_price_distance,
                                                -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group, -erad_aerial),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)

coord_unique <- gwr_hyd_destination_coord %>% select(id, long, lat) %>% unique
gwr_hyd_destination_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T)
dim(gwr_hyd_destination_dist)
gwr_data_hyd_destination <- gwr_hyd_destination_coord %>% select(-long, -lat, -erad_manual)

bwd_range <- seq(0.5, 4, by=0.1)
local_gwr_data_id <- gwr_hyd_destination_coord %>% select(id)
local_gwr_data <- gwr_hyd_destination_coord %>% select(-id, -municipio, -n_PPI_labs, -n_hyd_labs, -year, -long, -lat)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)

# Finding the optimal GWR regression without overfitting
bwd_range <- seq(0.5, 4, by=0.1)
response_var <- "hyd_destination"
min_n_reg_data <- 30
model_coef_list <- list()
model_pval_list <- list()
for (i in 1:nrow(coord_unique)) {
  id_i <- coord_unique$id[i]
  model_coef_mat_i <- matrix(nrow=1+length(local_gwr_data), ncol=length(bwd_range), data = 0) %>% as_tibble() %>%
    mutate(var_name=c("Intercept", names(local_gwr_data))) %>%
    filter(var_name != response_var) %>%
    relocate(var_name)
  names(model_coef_mat_i)[-1] <- paste0("bw_", bwd_range)
  
  model_pval_mat_i <- matrix(nrow=1+length(local_gwr_data), ncol=length(bwd_range), data = 0) %>% as_tibble() %>%
    mutate(var_name=c("Intercept", names(local_gwr_data))) %>%
    filter(var_name != response_var) %>%
    relocate(var_name)
  names(model_pval_mat_i)[-1] <- paste0("bw_", bwd_range)
  
  for(j in 1:length(bwd_range)) {
    bw_j <- bwd_range[j]
    n_reg_data_i_bw <- (n_reg_data_mat %>% filter(id == id_i))[[paste0("bw_", bw_j)]]
    if (n_reg_data_i_bw < min_n_reg_data) {
      aic_score_mat[i, j+1] <- NA
      next
    }
    
    neighbor_dist_index <- which(local_gwr_dist[i,] <= bw_j)
    neighbor_id_i <- coord_unique$id[neighbor_dist_index]
    neighbor_dist <- tibble(id=neighbor_id_i, dist=local_gwr_dist[i,neighbor_dist_index])
    neighbor_index <- which(local_gwr_data_id$id %in% coord_unique$id[neighbor_dist_index])
    dist_weights_df <- left_join(local_gwr_data_id, neighbor_dist, by="id")
    
    local_gwr_data_bw_i <- local_gwr_data[neighbor_index,]
    
    gwr_result_bw_i <- glm(hyd_destination~.,
                           data=local_gwr_data_bw_i,
                           # weight=1/(1+local_gwr_data_bw_i[i,neighbor_index]),
                           family="binomial")
    coef_i <- gwr_result_bw_i$coefficients
    coef_i_df <- data.frame(var_name=names(coef_i), coef=coef_i)
    p_values_i <- summary(gwr_result_bw_i)$coefficients[,4]
    p_value_df <- data.frame(var_name=names(p_values_i), p_value=p_values_i)
    coef_p_value_df <- left_join(coef_i_df, p_value_df, by="var_name")
    model_coef_mat_i[,j+1] <- coef_p_value_df$coef
    model_pval_mat_i[,j+1] <- coef_p_value_df$p_value
    aic_score_mat[i, j+1] <- gwr_result_bw_i$aic
  }
  
  model_coef_list[[paste0("id_", id_i)]] <- model_coef_mat_i
  model_pval_list[[paste0("id_", id_i)]] <- model_pval_mat_i
}

# write.csv(aic_score_mat, "Colombia Data/local GWR AIC without lab counts (10-28-2024).csv", row.names=F)
aic_score_mat <- read.csv("Colombia Data/local GWR AIC without lab counts (10-28-2024).csv") %>% as_tibble
local_GWR_coefs_bw <- read.csv("Colombia Data/local GWR best coefs without lab counts (10-28-2024).csv") %>% as_tibble
# load("Colombia Data/local GWR result without lab counts (10-28-2024).RData")

# n_reg_data_mat
local_GWR_coefs_bw <- tibble(id=aic_score_mat$id,
                             bw=bwd_range[aic_score_mat[,-1] %>% apply(1, function(x) which.min(ifelse(x<30, Inf, x)))])
local_GWR_coefs_bw$n_neighbors <- local_GWR_coefs_bw %>% apply(1, function(x) n_reg_data_mat[[paste0("bw_", x[2])]][which(x[1] == local_GWR_coefs_bw$id)])
local_GWR_pvals_bw <- local_GWR_coefs_bw
local_GWR_coefs_mat <- matrix(0, nrow(local_GWR_coefs_bw), ncol(local_gwr_data))
local_GWR_pvals_mat <- matrix(0, nrow(local_GWR_coefs_bw), ncol(local_gwr_data))
for(i in 1:nrow(local_GWR_coefs_bw)) {
  id_i <- local_GWR_coefs_bw$id[i]
  bw_i <- local_GWR_coefs_bw$bw[i]
  local_GWR_coefs_mat[i,] <- model_coef_list[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  local_GWR_pvals_mat[i,] <- model_pval_list[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
}
local_GWR_coefs_mat <- as_tibble(local_GWR_coefs_mat)
names(local_GWR_coefs_mat) <- model_coef_list$id_5002$var_name
local_GWR_coefs_mat$n_NA <- local_GWR_coefs_mat %>% apply(1, function(x) sum(is.na(x)))
local_GWR_coefs_mat <- local_GWR_coefs_mat %>% relocate(n_NA)
local_GWR_coefs_bw <- cbind(local_GWR_coefs_bw, local_GWR_coefs_mat) %>% as_tibble

local_GWR_pvals_mat <- as_tibble(local_GWR_pvals_mat)
names(local_GWR_pvals_mat) <- model_pval_list$id_5002$var_name
local_GWR_pvals_mat$n_NA <- local_GWR_pvals_mat %>% apply(1, function(x) sum(is.na(x)))
local_GWR_pvals_mat <- local_GWR_pvals_mat %>% relocate(n_NA)
local_GWR_pvals_bw <- cbind(local_GWR_pvals_bw, local_GWR_pvals_mat) %>% as_tibble

# local_GWR_coefs_bw %>% write.csv("Colombia Data/local GWR best coefs without lab counts (10-28-2024).csv", row.names=F)
# local_GWR_pvals_bw %>% write.csv("Colombia Data/local GWR best p-values without lab counts (10-28-2024).csv", row.names=F)

# id=25377 overfitting due to too many neighbors? Test bw < 0.5