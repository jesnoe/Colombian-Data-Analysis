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
  n_binary <- read.csv("Colombia Data/n_binary.csv") %>% as_tibble
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
cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
bwd_range <- seq(0.5, 3, by=0.1)

min_seizure_scaled <- min(gwr_data$norm$seizures) %>% round(3)
min_coca_area_scaled <- min(gwr_data$norm$coca_area) %>% round(3)
coord_unique <- gwr_data$coord
local_gwr_dist <- gwr_data$dist %>% as.matrix

# nonzero_seizure <- cv_aic_min_mat_
# nonzero_coca_area <- cv_aic_min_mat_ 
# n_price <- cv_aic_min_mat_
# n_river_length <- cv_aic_min_mat_
# n_road_length <- cv_aic_min_mat_
# n_lab_prob <- cv_aic_min_mat_
# n_binary <- cv_aic_min_mat_
# for (i in 1:nrow(cv_aic_min_mat_)) {
#   id_i <- cv_aic_min_mat_$id[i]
#   for (bw_ij in bwd_range) {
#     col_name_ij <- paste0("bw_", bw_ij)
#     neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_=F, coord_unique, local_gwr_dist)
#     nonzero_seizure[[col_name_ij]][i] <- sum(round(neighbor_ij$seizures, 3) > min_seizure_scaled)
#     nonzero_coca_area[[col_name_ij]][i] <- sum(round(neighbor_ij$coca_area, 3) > min_coca_area_scaled)
#     n_price[[col_name_ij]][i] <- table(neighbor_ij$price_avg) %>% length
#     n_river_length[[col_name_ij]][i] <- table(neighbor_ij$river_length) %>% length
#     n_road_length[[col_name_ij]][i] <- table(neighbor_ij$road_length) %>% length
#     n_lab_prob[[col_name_ij]][i] <- table(neighbor_ij$lab_prob) %>% length
#     n_binary[[col_name_ij]][i] <- neighbor_ij %>% select(airport, armed_group, ferry, police, military) %>% apply(2, function(x) table(x) %>% length %>% min)
#   }
# }
# nonzero_seizure %>% write.csv("Colombia Data/nonzero_seizure.csv", row.names = F)
# nonzero_coca_area %>% write.csv("Colombia Data/nonzero_coca_area.csv", row.names = F)
# n_price %>% write.csv("Colombia Data/n_price.csv", row.names = F)
# n_river_length %>% write.csv("Colombia Data/n_river_length.csv", row.names = F)
# n_road_length %>% write.csv("Colombia Data/n_road_length.csv", row.names = F)
# n_lab_prob %>% write.csv("Colombia Data/n_lab_prob.csv", row.names = F)
# n_binary %>% write.csv("Colombia Data/n_binary.csv", row.names = F)

# coef map
local_gwr_coef_map <- function(local_GWR_coefs_list, cv_aic_min_mat, dep_var, indep_vars, alpha=0.05, weight_=NULL) {
  optimal_bw <- gsub("bw_", "",
                     cv_aic_min_mat[,-1] %>% apply(1, function(x) ifelse(sum(!is.na(x)) == 0, NA, bwd_range[which.min(x)]))
  ) %>% as.numeric
  
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
            paste0("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs drop alpha=", alpha, " ", dep_var, weight_, " model drop (05-21-2025).csv"),
            row.names = F)
  
  
  for (i in c(2, 4:length(coef_table))) {
    var_name <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              coef=coef_table[[var_name]],
                              rounded_coef=coef_table[[var_name]] %>% round(3))
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
    
    ggsave(paste0("Colombia Data/local GWR PML result predicted prices/coef maps/",
                  dep_var, "/local GWR PML coef map drop ", var_name, " alpha=", alpha, " ", dep_var, weight_, " (05-21-2025).png"),
           gwr_coef_map, scale=1)
  }
}

# coef map by F1 scores
local_gwr_PML_coef_map_by_F1 <- function(local_GWR_coefs_list, PML_best_bw_tbl_, criteria, dep_var, indep_vars, alpha=0.1, n_drop) {
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
  # if (!is.null(weight_)) interact_ <- " weight"
  write.csv(coef_table,
            paste0("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs ", dep_var, " ", criteria, " all var model drop ", n_drop, " (06-20-2025).csv"),
            row.names = F)
  # write.csv(pval_table,
  #           paste0("Colombia Data/local GWR PML result predicted prices/local GWR PML p-values ", dep_var, " ", criteria, " all var model drop ", n_drop, " (06-17-2025).csv"),
  #           # paste0("Colombia Data/local GWR PML result predicted prices/local GWR PML p-values alpha=", alpha, " ", dep_var, " ", criteria, " var drop (06-05-2025).csv"),
  #           row.names = F)
  
  
  for (i in c(2, 4:length(coef_table))) {
    var_name <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              coef=coef_table[[var_name]],
                              rounded_coef=coef_table[[var_name]] %>% round(3),
                              p_value=pval_table[[var_name]])
    min_coef <- min(gwr_coefs_i$coef, na.rm=T)
    max_coef <- max(gwr_coefs_i$coef, na.rm=T)
    coef_map_coords_bw <- map_df %>% 
      left_join(gwr_coefs_i, by="id")
    # gwr_coefs_i$coef <- ifelse(gwr_coefs_i$p_value > alpha, NA, gwr_coefs_i$coef)
    coef_map_coords <- map_df %>% 
      left_join(gwr_coefs_i, by="id")
    
    if (i == 2) {
      gwr_coef_map <- ggplot(coef_map_coords_bw, aes(x=long, y=lat)) + 
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
    
    ggsave(paste0("Colombia Data/local GWR PML result predicted prices/coef maps/",
                  dep_var, "/local GWR PML coef by drop ", var_name,
                  # " alpha=", alpha,
                  " ", dep_var, " ", criteria, " all var model drop ", n_drop, " (06-20-2025).png"),
           gwr_coef_map, scale=1)
  }
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
      PML_model_list_id_bw_CM <- confusionMatrix(PML_model_list_id_bw_pred, PML_model_list_id_bw_y)
      F1_mat[i,j+1] <- PML_model_list_id_bw_CM$byClass[7] # F1 score
      
    }
  }
  return(F1_mat)
}

bwd_range <- seq(0.5, 3, by=0.1)
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

local_gwr_lasso_coefs_hyd_destination <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso coefs hyd_destination (03-28-2025).csv") %>%
  as_tibble %>% arrange(id)
indep_vars_ <- names(local_gwr_lasso_coefs_hyd_destination)[-(1:3)]


cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic model drop (05-21-2025).csv") %>% as_tibble
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop (05-21-2025).RData") # local_GWR_coefs_PML_hyd_dest_model_drop
local_gwr_coef_map(local_GWR_coefs_PML_hyd_dest_model_drop, cv_aic_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_PML_hyd_dest_model_drop)
# local_GWR_coefs_list<-local_GWR_coefs_PML_hyd_dest_model_drop; cv_aic_min_mat<-cv_aic_min_mat_; indep_vars<-indep_vars_; dep_var="hyd_destination"; weight_=NULL; alpha<-0.1

cv_aic_min_mat_ <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic all var drop (06-17-2025).csv") %>% as_tibble
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price all var drop (06-17-2025).RData") # local_GWR_coefs_PML_hyd_dest_var_drop
local_gwr_coef_map(local_GWR_coefs_PML_hyd_dest_var_drop, cv_aic_min_mat_, "hyd_destination", indep_vars_, alpha=0.1);# rm(local_GWR_coefs_PML_hyd_dest_var_drop)

# local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled
# load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure scaled (05-21-2025).RData")
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure coca scaled (06-20-2025).RData")

# local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled
# load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price var drop log seizure scaled (06-05-2025).RData")
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price all var drop log seizure coca scaled (06-20-2025).RData")

## Penalized MLE comparisons
PML_gwr_coefs_model_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination model drop (05-21-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_model_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_seizure_bw_F1 all var model drop 5 (06-17-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_model_drop_alpha_ex <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs alpha=0.1 hyd_destination PML_seizure_bw_F1 model drop (05-21-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_model_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_log_seizure_coca_bw_F1 all var model drop 5 (06-20-2025).csv") %>% as_tibble

PML_gwr_coefs_F1_var_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_seizure_bw_F1 all var drop 5 (06-17-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination PML_log_seizure_coca_bw_F1 all var drop 5 (06-20-2025).csv") %>% as_tibble

PML_F1_score_hyd_dest_model_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 model drop (05-21-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_var_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop (06-17-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_model_drop_log_seizure <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 model drop log seizure coca scaled (06-20-2025).csv") %>% as_tibble
PML_F1_score_hyd_dest_var_drop_log_seizure <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop log seizure scaled (06-20-2025).csv") %>% as_tibble


PML_gwr_aic_model_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic model drop (05-21-2025).csv") %>% as_tibble
PML_gwr_aic_var_drop <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price aic all var drop (06-17-2025).csv") %>% as_tibble
PML_gwr_aic_model_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price model drop log seizure coca scaled (06-20-2025).csv") %>% as_tibble
PML_gwr_aic_var_drop_log_seizure_coca <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price all var drop log seizure coca scaled (06-20-2025).csv") %>% as_tibble

PML_gwr_coefs_F1_model_drop %>% filter(abs(coca_area) > 100)
PML_gwr_coefs_F1_model_drop %>% filter(coca_area > 100)
PML_gwr_coefs_F1_model_drop %>% filter(coca_area < -100)
PML_gwr_coefs_F1_model_drop %>% filter(lab_prob > 100)

data_id <- neighbor_id(73270, 1.3, scale_11_=F, coord_unique, local_gwr_dist)
local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3 %>% summary
res_73270 <- tibble(residual = as.numeric(as.character(local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$y)) - local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$predict,
                    coca_area = local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$coca_area,
                    lab_prob = local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$lab_prob,
                    seizure = local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$seizures,
                    river_length = local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$river_length,
                    road_length = local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$road_length,
                    population = local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model$population)
nrow(local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model) # 270
local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% select(airport, armed_group, ferry, police, military) %>% apply(2, sum)
res_73270 %>% ggplot() + geom_point(aes(x=coca_area, y=residual))
res_73270 %>% ggplot() + geom_point(aes(x=lab_prob, y=residual))
res_73270 %>% ggplot() + geom_point(aes(x=seizure, y=residual))
res_73270 %>% ggplot() + geom_point(aes(x=river_length, y=residual))
res_73270 %>% ggplot() + geom_point(aes(x=road_length, y=residual))
res_73270 %>% ggplot() + geom_point(aes(x=population, y=residual))

local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(coca_area > -0.01)
local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(lab_prob > 0.9)
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(coca_area < -0.01), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(lab_prob<0.9), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(coca_area < -0.01 & lab_prob<0.9), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% select(-ferry), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% select(-population), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% select(-coca_area), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(seizures<2), alpha=0.1) %>% summary

which(res_73270$coca_area > -0.18 & res_73270$residual < 0)
local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(coca_area > -0.18)
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model[-c(55,218),], alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(coca_area < -0.18 | y == 1), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model %>% filter(coca_area < -0.18 | y == 0), alpha=0.1) %>% summary

logistf(y~., left_join(data_id, regression_data_aggr %>% select(id, coca_area_log_scale), by="id") %>% select(-coca_area), alpha=0.1) %>% summary


local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_73270$bw_1.3 %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_73270$bw_1.3$model %>% filter(coca_area < -0.18 | y == 1), alpha=0.1) %>% summary
logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_73270$bw_1.3$model %>% filter(coca_area < -0.18 | y == 0), alpha=0.1) %>% summary
local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_73270$bw_1.3$model$coca_area

coefs <- c()
for (i in 1:nrow(local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model)) {
  logistf_i <- logistf(y~., local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model[-i,], alpha=0.1)
  coefs <- c(coefs, coef(logistf_i)[["coca_area"]])
}
leave_one_out_logistf_73270 <- tibble(i=1:270, coca_area=coefs)
leave_one_out_logistf_73270 %>% ggplot() + geom_point(aes(x=i, y=coca_area))
leave_one_out_logistf_73270 %>% filter(coca_area < 100)
leave_one_out_logistf_73270 %>% filter(coca_area > 110)

local_GWR_coefs_PML_hyd_dest_model_drop$id_73270$bw_1.3$model[c(4,27,57,84),]

data_id <- neighbor_id(81001, 2.3, scale_11_=F, coord_unique, local_gwr_dist)
local_GWR_coefs_PML_hyd_dest_model_drop$id_81001$bw_2.3 %>% summary

logistf(y~., left_join(data_id, regression_data_aggr %>% select(id, coca_area_log_scale), by="id") %>% select(-coca_area), alpha=0.1) %>% summary

data_id <- neighbor_id(81065, 1.7, scale_11_=F, coord_unique, local_gwr_dist)
local_GWR_coefs_PML_hyd_dest_model_drop$id_81065$bw_1.7 %>% summary

PML_gwr_coefs_F1_var_drop %>% filter(lab_prob > 100)
data_id <- neighbor_id(81065, 1.7, scale_11_=F, coord_unique, local_gwr_dist)
local_GWR_coefs_PML_hyd_dest_var_drop$id_81065$bw_1.7 %>% summary

PML_gwr_coefs_model_drop %>% filter(seizures > 100)
PML_gwr_coefs_model_drop %>% filter(abs(seizures) > 100 & id %in% (municipio_centroid %>% filter(depto == "Meta") %>% pull(id)))
PML_gwr_coefs_model_drop %>% filter(abs(lab_prob) > 100 & id %in% (municipio_centroid %>% filter(depto == "Meta") %>% pull(id)))


id_i <- 50350; bw_i <- 1.5
id_i <- 50370; bw_i <- 0.9

id_i <- 41615; bw_i <- 1.2
id_i <- 5031 ; bw_i <- 1.2

id_i <- 5002 ; bw_i <- 0.9
id_i <- 5002 ; bw_i <- 1.3
data_id <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
data_id %>% select(y, airport, armed_group, ferry:military, seizures, coca_area) %>% arrange(y) %>% print(n=49)
data_id %>% print(n=nrow(data_id))


glm(y~., data=data_id %>% select(-id), family=binomial) %>% summary
penal_logist <- logistf(y~., data=data_id %>% select(-id))
penal_logist %>% summary
penal_logist <- logistf(y~.,
                        data = data_id %>% select(-seizures) %>% 
                          left_join(regression_data_aggr %>% select(-seizures) %>% rename(seizures = seizures_log_scale), by="id") %>% select(-id)) %>% summary
extractAIC(penal_logist)
penal_logist$pl.conv

penal_logist %>% summary
var_name <- "seizures"
penal_logist_loglik_list[[var_name]] %>% 
  ggplot() + geom_point(aes(x=beta, y=loglik)) + geom_line(aes(x=beta, y=loglik)) + labs(x = paste0("beta - ", var_name), y = "log likelihood")

local_GWR_coefs_PML_hyd_dest_model_drop$id_5002$bw_0.9

profile(penal_logist, variable="price_avg") %>% plot
profile(penal_logist, variable="seizures") %>% plot
profile(penal_logist, variable="river_length") %>% plot
profile(penal_logist, variable="road_length") %>% plot
profile(penal_logist, variable="population") %>% plot

penal_logist <- local_GWR_coefs_PML_hyd_dest_model_drop$id_5079$bw_1.2

data_id %>% select(y, airport:military, -lab_prob) %>% arrange(y) %>% print(n=49)
glm(y~., data_id %>% select(-id), family = binomial) %>% summary
glm(y~., data_id %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% select(-id), family = binomial) %>% summary
glm(y~., data_id %>% select(-seizures) %>% left_join(regression_data_aggr, by="id") %>% mutate(seizures=scale(log(1+seizures))[,1]) %>% select(-id), family = binomial) %>% summary

data(sex2)
fit<-logistf(case ~ age+oc+vic+vicl+vis+dia, data=sex2)
profile(fit, variable="age") %>% plot


PML_gwr_coefs_model_drop %>% filter(abs(seizures) > 100)
high_seizure_muni <- PML_gwr_coefs_model_drop %>% filter(abs(seizures) > 100)

for (j in 1:nrow(high_seizure_muni)) {
  id_j <- high_seizure_muni$id[j]
  bw_j <- high_seizure_muni$bw[j]
  reg_model_j <- local_GWR_coefs_PML_hyd_dest_model_drop[[paste0("id_", id_j)]][[paste0("bw_", bw_j)]]
  reg_model_j$model %>% as_tibble %>% print(n=nrow(reg_model_j$model))
}

cv_aic_min_mat_[which(cv_aic_min_mat_$id == 5002),]
n_neighbors_id <- local_GWR_coefs_PML_hyd_dest_model_drop$id_5002 %>% lapply(function(x) ifelse(is.na(x), return(NA), return(nrow(x$model)))) %>% unlist

### F1 score mat
# PML_F1_score_hyd_dest_model_drop <- PML_F1_score(local_GWR_coefs_PML_hyd_dest_model_drop, PML_gwr_aic_model_drop)
# write.csv(PML_F1_score_hyd_dest_model_drop,
#           "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 model drop (05-21-2025).csv", row.names = F)
PML_F1_score_hyd_dest_model_drop_log_seizure <- PML_F1_score(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled, PML_gwr_aic_model_drop_log_seizure_coca)
write.csv(PML_F1_score_hyd_dest_model_drop_log_seizure,
          "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 model drop log seizure coca scaled (06-20-2025).csv", row.names = F)
# PML_F1_score_hyd_dest_var_drop <- PML_F1_score(local_GWR_coefs_PML_hyd_dest_var_drop, PML_gwr_aic_var_drop)
# write.csv(PML_F1_score_hyd_dest_var_drop,
#           "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop (06-17-2025).csv", row.names = F)
PML_F1_score_hyd_dest_var_drop_log_seizure <- PML_F1_score(local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled, PML_gwr_aic_var_drop_log_seizure_coca)
write.csv(PML_F1_score_hyd_dest_var_drop_log_seizure,
          "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var drop log seizure scaled (06-20-2025).csv", row.names = F)

PML_best_bw_tbl <- tibble(id = PML_gwr_aic_model_drop$id,
                          # PML_seizure_bw_AIC = PML_gwr_aic_model_drop[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                          # PML_seizure_bw_F1 = PML_F1_score_hyd_dest_model_drop[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                          PML_log_seizure_coca_bw_AIC = PML_gwr_aic_model_drop_log_seizure_coca[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                          PML_log_seizure_coca_bw_F1 = PML_F1_score_hyd_dest_model_drop_log_seizure[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
PML_best_bw_tbl %>% ggplot + geom_point(aes(x=PML_seizure_bw_AIC, y=PML_log_seizure_bw_AIC)) + ggtitle("best bandwidths - PML seizures AIC vs. PML log(seizures) AIC")
PML_best_bw_tbl %>% ggplot + geom_point(aes(x=PML_seizure_bw_AIC, y=PML_seizure_bw_F1)) + ggtitle("best bandwidths - PML seizures AIC vs. PML seizures F1")
PML_best_bw_tbl %>% ggplot + geom_point(aes(x=PML_log_seizure_bw_AIC, y=PML_log_seizure_bw_F1)) + ggtitle("best bandwidths - PML log(seizures) AIC vs. PML log(seizures) F1")
PML_best_bw_tbl %>% ggplot + geom_point(aes(x=PML_seizure_bw_F1, y=PML_log_seizure_bw_F1)) + ggtitle("best bandwidths - PML seizures F1 vs. PML log(seizures) F1")

# coef map by F1 model drop (for all vars)
# PML_F1_score_hyd_dest_model_drop_all <- PML_F1_score_hyd_dest_model_drop
PML_F1_score_hyd_dest_model_drop_all <- PML_F1_score_hyd_dest_model_drop_log_seizure
PML_F1_score_hyd_dest_model_drop_all[, -1][nonzero_coca_area[,-1] < 5] <- NA
PML_F1_score_hyd_dest_model_drop_all[, -1][nonzero_seizure[,-1] < 5] <- NA
PML_F1_score_hyd_dest_model_drop_all[, -1][n_price[,-1] < 5] <- NA
PML_F1_score_hyd_dest_model_drop_all[, -1][n_river_length[,-1] < 5]
PML_F1_score_hyd_dest_model_drop_all[, -1][n_road_length[,-1] < 5]
PML_F1_score_hyd_dest_model_drop_all[, -1][n_lab_prob[,-1] < 5]
PML_F1_score_hyd_dest_model_drop_all[, -1][n_binary[,-1] < 2] %>% summary

# PML_best_bw_tbl <- tibble(id = PML_F1_score_hyd_dest_model_drop_all$id,
#                           PML_seizure_bw_F1 = PML_F1_score_hyd_dest_model_drop_all[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
PML_best_bw_tbl <- tibble(id = PML_F1_score_hyd_dest_model_drop_all$id,
                          PML_seizure_bw_F1 = PML_F1_score_hyd_dest_model_drop_all[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist,
                          PML_log_seizure_coca_bw_F1 = PML_F1_score_hyd_dest_model_drop_all[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)

local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_hyd_dest_model_drop, PML_best_bw_tbl, criteria="PML_seizure_bw_F1", dep_var = "hyd_destination",
                             indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop$id_5040$bw_1.4$model)[-1], alpha = 0.1, n_drop = 5)
local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled, PML_best_bw_tbl, criteria="PML_log_seizure_coca_bw_F1", dep_var = "hyd_destination",
                             indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_5040$bw_1.4$model)[-1], alpha = 0.1, n_drop = 5)

# coef map by F1 var drop 
PML_best_bw_tbl_var_drop  <- tibble(id = PML_gwr_aic_var_drop$id,
                                    # PML_seizure_bw_AIC = PML_gwr_aic_var_drop[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                                    PML_seizure_bw_F1 = PML_F1_score_hyd_dest_var_drop[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist,
                                    # PML_log_seizure_coca_bw_AIC = PML_gwr_aic_var_drop_log_seizure_coca[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                                    PML_log_seizure_coca_bw_F1 = PML_F1_score_hyd_dest_var_drop_log_seizure[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)

local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_hyd_dest_var_drop, PML_best_bw_tbl_var_drop, criteria="PML_seizure_bw_F1", dep_var = "hyd_destination",
                             indep_vars = names(local_GWR_coefs_PML_hyd_dest_var_drop$id_5040$bw_1.4$model)[-1], n_drop=5)

local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled, PML_best_bw_tbl_var_drop, criteria="PML_log_seizure_coca_bw_F1", dep_var = "hyd_destination",
                             indep_vars = names(local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_5040$bw_1.4$model)[-1], n_drop=5)



#### model drop coef map by n_drop
local_gwr_PML_coef_map_by_F1_n_drop <- function(local_GWR_coefs_list, dep_var, indep_vars, alpha=0.1, n_drop, var_name) {
  indep_vars <- c("Intercept", indep_vars)
  
  PML_F1_score_hyd_dest_model_drop_all <- PML_F1_score_hyd_dest_model_drop
  PML_F1_score_hyd_dest_model_drop_all[, -1][nonzero_coca_area[,-1] < n_drop] <- NA
  PML_F1_score_hyd_dest_model_drop_all[, -1][nonzero_seizure[,-1] < n_drop] <- NA
  PML_F1_score_hyd_dest_model_drop_all[, -1][n_price[,-1] < n_drop] <- NA
  PML_F1_score_hyd_dest_model_drop_all[, -1][n_river_length[,-1] < n_drop] <- NA
  PML_F1_score_hyd_dest_model_drop_all[, -1][n_road_length[,-1] < n_drop] <- NA
  PML_F1_score_hyd_dest_model_drop_all[, -1][n_lab_prob[,-1] < n_drop] <- NA
  PML_F1_score_hyd_dest_model_drop_all[, -1][n_binary[,-1] < 2] <- NA
  # write.csv(PML_F1_score_hyd_dest_model_drop_all, "Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest predicted price F1 all var model drop log seizure coca scaled (06-20-2025).csv",
  # row.names = F)
  
  PML_best_bw_tbl_ <- tibble(id = PML_F1_score_hyd_dest_model_drop_all$id,
                             PML_seizure_bw_F1 = PML_F1_score_hyd_dest_model_drop_all[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist)
  
  coef_table <- tibble(id = PML_best_bw_tbl_$id, bw=PML_best_bw_tbl_$PML_seizure_bw_F1)
  coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars))
  
  indep_vars_df <- data.frame(var_name=indep_vars)
  for (i in 1:nrow(coef_table)) {
    bw_i <- coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_model_i <- local_GWR_coefs_list[[i]][[paste0("bw_", bw_i)]]
    coef_i <- coef(local_GWR_model_i)
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i, p_value=local_GWR_model_i$prob)
    coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
    coef_mat[i,] <- coef_i_df$coef
    # pval_mat[i,] <- coef_i_df$p_value
  }
  
  coef_table <- bind_cols(coef_table, coef_mat)
  # pval_table <- bind_cols(pval_table, pval_mat)
  names(coef_table)[-(1:2)] <- indep_vars
  # names(pval_table)[-(1:2)] <- indep_vars
  
  gwr_coefs_i <- data.frame(id=coef_table$id,
                            coef=coef_table[[var_name]],
                            rounded_coef=coef_table[[var_name]] %>% round(3))
  min_coef <- min(gwr_coefs_i$coef, na.rm=T)
  max_coef <- max(gwr_coefs_i$coef, na.rm=T)
  coef_map_coords_bw <- map_df %>% 
    left_join(gwr_coefs_i, by="id")
  # gwr_coefs_i$coef <- ifelse(gwr_coefs_i$p_value > alpha, NA, gwr_coefs_i$coef)
  coef_map_coords <- map_df %>% 
    left_join(gwr_coefs_i, by="id")
  
  ggplot(coef_map_coords, aes(x=long, y=lat)) + 
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

local_gwr_PML_coef_map_by_F1_n_drop(local_GWR_coefs_PML_hyd_dest_model_drop, dep_var = "hyd_destination",
                                    indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop$id_5040$bw_1.4$model)[-1], alpha = 0.1, n_drop = 5, var_name="price_avg")
local_gwr_PML_coef_map_by_F1_n_drop(local_GWR_coefs_PML_hyd_dest_model_drop, dep_var = "hyd_destination",
                                    indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop$id_5040$bw_1.4$model)[-1], alpha = 0.1, n_drop = 6, var_name="price_avg")
local_gwr_PML_coef_map_by_F1_n_drop(local_GWR_coefs_PML_hyd_dest_model_drop, dep_var = "hyd_destination",
                                    indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop$id_5040$bw_1.4$model)[-1], alpha = 0.1, n_drop = 10, var_name="price_avg")


# exclude AIC or F1 in the matrix if a focal point is false negative
PML_gwr_aic_model_drop_ex <- PML_gwr_aic_model_drop
PML_gwr_aic_model_drop_log_seizure_ex <- PML_gwr_aic_model_drop_log_seizure
PML_F1_score_hyd_dest_model_drop_ex <- PML_F1_score_hyd_dest_model_drop
PML_F1_score_hyd_dest_model_drop_log_seizure_ex <- PML_F1_score_hyd_dest_model_drop_log_seizure

# PML_model_list <- local_GWR_coefs_PML_hyd_dest_model_drop; text_aic_mat <- PML_gwr_aic_model_drop
PML_model_list <- local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled; text_aic_mat <- PML_gwr_aic_model_drop_log_seizure
for (i in 1:nrow(text_aic_mat)) {
  id_i <- text_aic_mat$id[i]
  PML_model_list_id <- PML_model_list[[i]]
  
  for (j in 1:length(PML_model_list_id)) {
    if (is.na(text_aic_mat[i, j+1])) {
      next
    }
    
    PML_model_list_id_j_id <- neighbor_id(id_i, bwd_range[j], scale_11_=F, coord_unique, local_gwr_dist)$id
    focal_index <- which(PML_model_list_id_j_id == id_i)
    PML_model_list_id_bw <- PML_model_list_id[[j]]
    PML_model_list_id_bw_y <- PML_model_list_id_bw$model$y
    PML_model_list_id_bw_pred <- ifelse(PML_model_list_id_bw$predict < 0.5, 0, 1) %>% factor(levels = c("0", "1"))
    
    if (PML_model_list_id_bw_y[focal_index] == "1" & PML_model_list_id_bw_pred[focal_index] == "0") {
      PML_gwr_aic_model_drop_ex[i, j+1] <- NA
      PML_gwr_aic_model_drop_log_seizure_ex[i, j+1] <- NA
      PML_F1_score_hyd_dest_model_drop_ex[i, j+1] <- NA
      PML_F1_score_hyd_dest_model_drop_log_seizure_ex[i, j+1] <- NA
    }
    
    
  }
}

PML_best_bw_tbl_ex <- tibble(id = PML_gwr_aic_model_drop_ex$id,
                             PML_seizure_bw_AIC = PML_gwr_aic_model_drop_ex[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                             PML_seizure_bw_F1 = PML_F1_score_hyd_dest_model_drop_ex[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                             PML_log_seizure_bw_AIC = PML_gwr_aic_model_drop_log_seizure_ex[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist,
                             PML_log_seizure_bw_F1 = PML_F1_score_hyd_dest_model_drop_log_seizure_ex[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.min(x)]))) %>% unlist)
PML_best_bw_tbl_ex %>% ggplot + geom_point(aes(x=PML_seizure_bw_AIC, y=PML_log_seizure_bw_AIC)) + ggtitle("best bandwidths - PML seizures AIC vs. PML log(seizures) AIC")
PML_best_bw_tbl_ex %>% ggplot + geom_point(aes(x=PML_seizure_bw_AIC, y=PML_seizure_bw_F1)) + ggtitle("best bandwidths - PML seizures AIC vs. PML seizures F1")
PML_best_bw_tbl_ex %>% ggplot + geom_point(aes(x=PML_log_seizure_bw_AIC, y=PML_log_seizure_bw_F1)) + ggtitle("best bandwidths - PML log(seizures) AIC vs. PML log(seizures) F1")
PML_best_bw_tbl_ex %>% ggplot + geom_point(aes(x=PML_seizure_bw_F1, y=PML_log_seizure_bw_F1)) + ggtitle("best bandwidths - PML seizures F1 vs. PML log(seizures) F1")

local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_hyd_dest_model_drop, PML_best_bw_tbl_ex, criteria="PML_seizure_bw_F1", dep_var = "hyd_destination",
                             indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop$id_5040$bw_1.4$model)[-1])

local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled, PML_best_bw_tbl_ex, criteria="PML_log_seizure_bw_F1", dep_var = "hyd_destination",
                             indep_vars = names(local_GWR_coefs_PML_hyd_dest_model_drop_log_seizure_scaled$id_5040$bw_1.4$model)[-1])

PML_reg_loglik <- function(penal_logist, n_betas) {
  penal_logist_y <- penal_logist$model$y %>% as.character %>% as.numeric
  penal_logist_X <- penal_logist$model[,-1]
  penal_logist_coef <- coef(penal_logist)
  penal_logist_pi <- penal_logist$predict
  penal_logist_ci_lower <- penal_logist$ci.lower
  penal_logist_ci_upper <- penal_logist$ci.upper
  coef_names <- names(penal_logist_coef)[-1]
  
  penal_logist_loglik_list <- list()
  for (k in 1:ncol(penal_logist_X)) {
    upper_betas_k <- seq(penal_logist_coef[[k+1]], penal_logist_ci_upper[[k+1]], length.out=n_betas)
    lower_betas_k <- rev(seq(penal_logist_ci_lower[[k+1]], penal_logist_coef[[k+1]], length.out=n_betas))
    upper_loglik_tbl <- tibble(beta=upper_betas_k)
    lower_loglik_tbl <- tibble(beta=lower_betas_k)
    loglik_k_right <- penal_logist$loglik[[1]]
    loglik_k_left <- penal_logist$loglik[[1]]
    for (j in 1:(length(upper_betas_k)-1)) {
      upper_coef_kj <- penal_logist_coef[-1]
      lower_coef_kj <- upper_coef_kj
      upper_coef_kj[k] <- upper_betas_k[j]
      lower_coef_kj[k] <- lower_betas_k[j]
      
      upper_pi_k <- 1 / (1 + exp( -( penal_logist_coef[[1]] + as.matrix(penal_logist_X) %*% matrix(upper_coef_kj, ncol=1)) )) %>% as.vector
      lower_pi_k <- 1 / (1 + exp( -( penal_logist_coef[[1]] + as.matrix(penal_logist_X) %*% matrix(lower_coef_kj, ncol=1)) )) %>% as.vector
      upper_partial_loglik_beta_kj <- sum((penal_logist_y - upper_pi_k + penal_logist$hat.diag*(0.5-upper_pi_k) )*penal_logist_X[,k])
      lower_partial_loglik_beta_kj <- sum((penal_logist_y - lower_pi_k + penal_logist$hat.diag*(0.5-lower_pi_k) )*penal_logist_X[,k])
      
      loglik_k_right <- c(loglik_k_right, rev(loglik_k_right)[1] + upper_partial_loglik_beta_kj * (upper_betas_k[j+1] - upper_betas_k[j]))
      loglik_k_left <- c(loglik_k_left, rev(loglik_k_left)[1] - lower_partial_loglik_beta_kj * (lower_betas_k[j] - lower_betas_k[j+1]))
    }
    upper_loglik_tbl$loglik <- loglik_k_right
    lower_loglik_tbl$loglik <- loglik_k_left
    penal_logist_loglik_k <- bind_rows(lower_loglik_tbl[n_betas:2,], upper_loglik_tbl)
    penal_logist_loglik_list[[coef_names[k]]] <- penal_logist_loglik_k
  }
  
  return(penal_logist_loglik_list)
}

PML_gwr_coefs_F1_model_drop %>% filter(abs(coca_area) > 100)
PML_gwr_coefs_F1_model_drop_log_seizure %>% filter(abs(coca_area) > 100)

PML_gwr_coefs_F1_model_drop %>% filter(abs(coca_area) < 100)

var_name <- "coca_area"
PML_reg_loglik(local_GWR_coefs_PML_hyd_dest_model_drop$id_81001$bw_2.3, 100)[[var_name]] %>% 
  ggplot() + geom_point(aes(x=beta, y=loglik)) + geom_line(aes(x=beta, y=loglik)) + labs(x = paste0("beta - ", var_name), y = "log likelihood")
PML_reg_loglik(local_GWR_coefs_PML_hyd_dest_model_drop$id_81001$bw_2.3, 100)[[var_name]] %>% mutate(likelihood = exp(loglik)) %>% 
  ggplot() + geom_point(aes(x=beta, y=likelihood)) + geom_line(aes(x=beta, y=likelihood)) + labs(x = paste0("beta - ", var_name), y = "likelihood") + xlim(-150, -100)

PML_reg_loglik(local_GWR_coefs_PML_hyd_dest_model_drop$id_81220$bw_2.6, 100)[[var_name]] %>% 
  ggplot() + geom_point(aes(x=beta, y=loglik)) + geom_line(aes(x=beta, y=loglik)) + labs(x = paste0("beta - ", var_name), y = "log likelihood")
PML_reg_loglik(local_GWR_coefs_PML_hyd_dest_model_drop$id_5002$bw_0.9, 100)[[var_name]] %>% 
  ggplot() + geom_point(aes(x=beta, y=loglik)) + geom_line(aes(x=beta, y=loglik)) + labs(x = paste0("beta - ", var_name), y = "log likelihood")

local_GWR_coefs_PML_hyd_dest_model_drop$id_73347$bw_0.8 %>% summary
local_GWR_coefs_PML_hyd_dest_model_drop$id_73347$bw_0.8$model
logistf(y~., data = local_GWR_coefs_PML_hyd_dest_model_drop$id_73347$bw_0.8$model %>% select(-coca_area)) %>% summary
logistf(y~., data = local_GWR_coefs_PML_hyd_dest_model_drop$id_73347$bw_0.8$model %>% filter(coca_area < -0.18) %>% select(-coca_area)) %>% summary


