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

local_GWR_PML <- function(type.measure_="default", cv_aic_min_mat, sig_level_=0.05, gwr_PML_data_, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var) {
  bwd_range <- seq(0.5, 3, by=0.1)
  n_drop <- n_drop + 1
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
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, coord_unique, local_gwr_dist)
      n_0_1 <- neighbor_ij$y %>% table
      
      # restrict too unbalanced responses
      if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        cv_aic_min_mat[[bw_name]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      n_unique_vals <- neighbor_ij %>% select(-id) %>% apply(2, function(x) length(table(x)))
      if (method_ == "model drop") {
        if (any(n_unique_vals < 2) | any(n_unique_vals[3:4] < n_drop)) { # change to 6?
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
          next
        }
      }
      
      
      if (method_ == "var drop") {
        if (any(n_unique_vals[["coca_area"]] < n_drop)) {
          neighbor_ij$coca_area <- NULL
          n_unique_vals[["coca_area"]] <- n_drop
        }
        if (any(n_unique_vals[["seizures"]] < n_drop)) {
          neighbor_ij$seizures <- NULL
          n_unique_vals[["seizures"]] <- n_drop
        }
        if (any(n_unique_vals < 2)) neighbor_ij[, 1+which(n_unique_vals<2)] <- NULL
      }
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      tryCatch(
        {
          PML_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)
          AIC_i <- extractAIC(PML_result_ij)[[2]]
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- AIC_i
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- PML_result_ij
        },
        error = function(e) {
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        }
      )
      
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
    summarize(seizures = mean(hyd_seizures, na.rm=T))
  regression_data_aggr$seizures_log_scale <- scale(log(regression_data_aggr$seizures+1))[,1]
  }

gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
bwd_range <- seq(0.5, 3, by=0.1)

min_seizure_scaled <- min(gwr_data$norm$seizures) %>% round(3)
min_coca_area_scaled <- min(gwr_data$norm$coca_area) %>% round(3)
coord_unique <- gwr_data$coord
local_gwr_dist <- gwr_data$dist %>% as.matrix
nonzero_seizure <- cv_aic_min_mat_
nonzero_coca_area <- cv_aic_min_mat_ 
for (i in 1:nrow(cv_aic_min_mat_)) {
  id_i <- cv_aic_min_mat_$id[i]
  for (bw_ij in bwd_range) {
    col_name_ij <- paste0("bw_", bw_ij)
    neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_=F, coord_unique, local_gwr_dist)
    nonzero_seizure[[col_name_ij]][i] <- sum(round(neighbor_ij$seizures, 3) > min_seizure_scaled)
    nonzero_coca_area[[col_name_ij]][i] <- sum(round(neighbor_ij$coca_area, 3) > min_coca_area_scaled)
  }
}
nonzero_seizure
nonzero_coca_area

# penalized MLE
# type.measure_="default"; cv_aic_min_mat=cv_aic_min_mat_; sig_level_=0.1; interact_=F; scale_11_=F; weight_=NULL; dep_var="hyd_destination"; gwr_PML_data_=gwr_data; method_="model drop"; n_drop <- 5
cv_aic_min_mat_[,-1] <- NA
set.seed(100)
start.time <- Sys.time()
local_GWR_coefs_PML_hyd_dest_list <- local_GWR_PML(dep_var = "hyd_destination", cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="model drop", sig_level_ = 0.1)
end.time <- Sys.time()
end.time - start.time # 54.72302 mins for hyd_destination model drop

local_GWR_coefs_PML_hyd_dest_model_drop <- local_GWR_coefs_PML_hyd_dest_list$PML
write.csv(local_GWR_coefs_PML_hyd_dest_list$cv_aic_min_mat, "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic model drop (05-21-2025).csv", row.names = F)
save("local_GWR_coefs_PML_hyd_dest_model_drop", file = "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop (05-21-2025).RData")
rm(local_GWR_coefs_PML_hyd_dest_model_drop); rm(local_GWR_coefs_PML_hyd_dest_list)

set.seed(5640)
start.time <- Sys.time()
local_GWR_coefs_PML_hyd_dest_list <- local_GWR_PML(dep_var = "hyd_destination", cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="var drop", sig_level_ = 0.1, n_drop=5)
end.time <- Sys.time()
end.time - start.time # 39.89051 mins for hyd_destination var drop

local_GWR_coefs_PML_hyd_dest_var_drop <- local_GWR_coefs_PML_hyd_dest_list$PML
write.csv(local_GWR_coefs_PML_hyd_dest_list$cv_aic_min_mat, "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic var drop (06-05-2025).csv", row.names = F)
save("local_GWR_coefs_PML_hyd_dest_var_drop", file = "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price var drop (06-05-2025).RData")
rm(local_GWR_coefs_PML_hyd_dest_var_drop); rm(local_GWR_coefs_PML_hyd_dest_list)



# Local GWR PML with log(seizures) scaled data
gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
cv_aic_min_mat_[,-1] <- NA
set.seed(100)
start.time <- Sys.time()
local_GWR_coefs_PML_hyd_dest_list <- local_GWR_PML(dep_var = "hyd_destination", cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="model drop", sig_level_ = 0.1)
end.time <- Sys.time()
end.time - start.time # 45.24913 mins for hyd_destination model drop

local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled <- local_GWR_coefs_PML_hyd_dest_list$PML
write.csv(local_GWR_coefs_PML_hyd_dest_list$cv_aic_min_mat, "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic model drop log seizure scaled (05-21-2025).csv", row.names = F)
save("local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled", file = "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure scaled (05-21-2025).RData")
rm(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled); rm(local_GWR_coefs_PML_hyd_dest_list)

set.seed(5640)
start.time <- Sys.time()
local_GWR_coefs_PML_hyd_dest_list <- local_GWR_PML(dep_var = "hyd_destination", cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="var drop", sig_level_ = 0.1, n_drop = 5)
end.time <- Sys.time()
end.time - start.time # 1.050017 hours for hyd_destination var drop

local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled <- local_GWR_coefs_PML_hyd_dest_list$PML
write.csv(local_GWR_coefs_PML_hyd_dest_list$cv_aic_min_mat, "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic var drop log seizure scaled (06-05-2025).csv", row.names = F)
save("local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled", file = "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price var drop log seizure scaled (06-05-2025).RData")
rm(local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled); rm(local_GWR_coefs_PML_hyd_dest_list)


cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic model drop log seizure scaled (05-21-2025).csv") %>% as_tibble
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure scaled (05-21-2025).RData") # local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled
local_gwr_coef_map(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled, cv_aic_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled)

cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic var drop (05-21-2025).csv") %>% as_tibble
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price var drop (05-21-2025).RData") # local_GWR_coefs_PML_hyd_dest_var_drop
local_gwr_coef_map(local_GWR_coefs_PML_hyd_dest_var_drop, cv_aic_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_PML_hyd_dest_var_drop)
optimal_bw <- gsub("bw_", "",
                   cv_aic_min_mat[,-1] %>% apply(1, function(x) ifelse(sum(!is.na(x)) == 0, NA, bwd_range[which.min(x)]))
) %>% as.numeric

# best bw coefs
indep_vars <- c("Intercept", indep_vars)
coef_table <- tibble(id = cv_aic_min_mat$id, bw=optimal_bw)
coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars))

indep_vars_df <- data.frame(var_name=indep_vars)
for (i in 1:nrow(coef_table)) {
  bw_i <- coef_table$bw[i]
  if (is.na(bw_i)) next
  local_GWR_model_i <- local_GWR_coefs_list[[i]][[paste0("bw_", bw_i)]]
  coef_i <- coef(local_GWR_model_i)
  coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
  coef_mat[i,] <- left_join(indep_vars_df, coef_i_df, by="var_name")$coef
}

coef_table <- bind_cols(coef_table, coef_mat)
names(coef_table)[-(1:2)] <- indep_vars
# if (!is.null(weight_)) interact_ <- " weight"
write.csv(coef_table,
          paste0("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs drop ", dep_var, "model drop log seizure scaled (05-21-2025).csv"),
          row.names = F)