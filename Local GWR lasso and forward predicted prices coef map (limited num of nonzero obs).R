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

# forward coef map
local_gwr_forward_coef_map_limited <- function(local_GWR_coefs_forward_list, cv_dev_min_mat, dep_var, indep_vars, alpha=0.05, weight_=NULL) {
  # only considers bandwidth more than 4 nonzero observations in seizures and coca_area
  # cv_dev_min_mat[,-1][nonzero_seizure[,-1] < 5] <- NA
  # cv_dev_min_mat[,-1][nonzero_coca_area[,-1] < 5] <- NA
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
            paste0("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise coefs limited alpha=", alpha, " ", dep_var, weight_, " model drop (05-12-2025).csv"),
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
    
    ggsave(paste0("Colombia Data/local GWR stepwise result predicted prices/coef maps/",
                  dep_var, "/local GWR stepwise limited coef map ", var_name, " alpha=", alpha, " ", dep_var, weight_, " model drop (05-12-2025).png"),
           gwr_coef_map, scale=1)
  }
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

local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward coefs alpha=0.1 hyd_destination (04-09-2025).csv") %>% as_tibble
overfitting_id <- c(25488, 85136, 73200)
local_gwr_forward_coefs %>% filter(id %in% overfitting_id)
nonzero_seizure %>% filter(id %in% overfitting_id)
nonzero_coca_area %>% filter(id %in% overfitting_id)
cv_dev_min_mat_ %>% filter(id %in% overfitting_id)
local_GWR_coefs_forward_hyd_dest_alpha_0.1$id_85136$bw_0.6
local_GWR_coefs_forward_hyd_dest_alpha_0.1$id_73200$bw_1.4
local_GWR_coefs_forward_hyd_dest_alpha_0.1$id_25488$bw_0.6

cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price (04-09-2025).RData") # local_GWR_coefs_forward_hyd_dest
local_gwr_forward_coef_map_limited(local_GWR_coefs_forward_hyd_dest, cv_dev_min_mat_, "hyd_destination", indep_vars_);# rm(local_GWR_coefs_forward_hyd_dest)
# local_GWR_coefs_forward_list<-local_GWR_coefs_forward_hyd_dest; cv_dev_min_mat<-cv_dev_min_mat_; dep_var="hyd_destination"; weight_=NULL


## stepwise map (raw seizures)
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price cv min dev model drop (05-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price model drop (05-09-2025).RData") # local_GWR_coefs_step_hyd_dest_model_drop
local_gwr_forward_coef_map_limited(local_GWR_coefs_step_hyd_dest_model_drop, cv_dev_min_mat_, "hyd_destination", indep_vars_, alpha = 0.1); rm(local_GWR_coefs_step_hyd_dest_model_drop)

cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price cv min dev var drop (05-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR stepwise result predicted prices/local GWR stepwise hyd_dest predicted price var drop (05-09-2025).RData") # local_GWR_coefs_step_hyd_dest_var_drop
local_gwr_forward_coef_map_limited(local_GWR_coefs_step_hyd_dest_var_drop, cv_dev_min_mat_, "hyd_destination", indep_vars_, alpha = 0.1); rm(local_GWR_coefs_step_hyd_dest_var_drop)

# lasso coef map
local_gwr_lasso_coef_map_limited <- function(local_GWR_coefs_lasso_list, cv_dev_min_mat, dep_var, weight_=NULL) {
  # only considers bandwidth more than 4 nonzero observations in seizures and coca_area
  # cv_dev_min_mat[,-1][nonzero_seizure[,-1] < 5] <- NA
  # cv_dev_min_mat[,-1][nonzero_coca_area[,-1] < 5] <- NA
  optimal_bw <- gsub("bw_", "",
                     cv_dev_min_mat[,-1] %>% apply(1, function(x) ifelse(sum(!is.na(x)) == 0, NA, bwd_range[which.min(x)]))
  ) %>% as.numeric
  
  indep_vars <- c("Intercept", rownames(coef(local_GWR_coefs_lasso_list$id_5001$bw_2.5))[-1])
  lasso_coef_table <- tibble(id = cv_dev_min_mat$id, bw=optimal_bw)
  lasso_coef_mat <- matrix(NA, nrow(lasso_coef_table), length(indep_vars))
  var_name_tbl <- tibble(var_name = indep_vars)
  
  for (i in 1:nrow(lasso_coef_table)) {
    bw_i <- lasso_coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_lasso_i <- local_GWR_coefs_lasso_list[[i]][[paste0("bw_", bw_i)]]
    local_GWR_lasso_coef_i <- coef(local_GWR_lasso_i)[,1]
    names(local_GWR_lasso_coef_i)[1] <- "Intercept"
    lasso_coef_mat[i,] <- left_join(var_name_tbl, tibble(var_name=names(local_GWR_lasso_coef_i), coef=local_GWR_lasso_coef_i), by="var_name")$coef
  }
  
  lasso_coef_table <- bind_cols(lasso_coef_table, lasso_coef_mat)
  names(lasso_coef_table)[-(1:2)] <- indep_vars
  # if (!is.null(weight_)) interact_ <- " weight"
  write.csv(lasso_coef_table,
            paste0("Colombia Data/local GWR lasso result predicted prices/local GWR lasso coefs limited", dep_var, " ", weight_, " raw seizures model drop (05-12-2025).csv"),
            row.names = F)
  
  
  for (i in c(2, 4:length(lasso_coef_table))) {
    var_name <- names(lasso_coef_table)[i]
    gwr_coefs_i <- data.frame(id=lasso_coef_table$id,
                              coef=lasso_coef_table[[var_name]],
                              rounded_coef=lasso_coef_table[[var_name]] %>% round(3))
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
    
    ggsave(paste0("Colombia Data/local GWR lasso result predicted prices/coef maps/",
                  dep_var, "/local GWR lasso coef map ", var_name, " ", dep_var, weight_, " raw seizures model drop (05-12-2025).png"),
           gwr_coef_map, scale=1)
  }
}


cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price cv min dev raw seizures model drop (05-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price raw seizures model drop (05-09-2025).RData") # local_GWR_coefs_lasso_hyd_dest_model_drop
local_gwr_lasso_coef_map_limited(local_GWR_coefs_lasso_hyd_dest_model_drop, cv_dev_min_mat_, "hyd_destination"); rm(local_GWR_coefs_lasso_hyd_dest_model_drop)

cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price cv min dev raw seizures var drop (05-09-2025).csv") %>% as_tibble
load("Colombia Data/local GWR lasso result predicted prices/local GWR lasso hyd_dest predicted price raw seizures var drop (05-09-2025).RData") # local_GWR_coefs_lasso_hyd_dest_var_drop
local_gwr_lasso_coef_map_limited(local_GWR_coefs_lasso_hyd_dest_var_drop, cv_dev_min_mat_, "hyd_destination"); rm(local_GWR_coefs_lasso_hyd_dest_var_drop)

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

forward_gwr_coefs_var_drop %>% filter(lab_prob > 1000)
forward_gwr_coefs_model_drop %>% filter(id %in% (forward_gwr_coefs_var_drop %>% filter(lab_prob > 1000) %>% pull(id)))

forward_gwr_coefs_model_drop[,-(1:3)] %>% apply(2, function(x) sum(is.na(x)))
forward_gwr_coefs_var_drop[,-(1:3)] %>% apply(2, function(x) sum(is.na(x)))


# local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop high lab_prob coef data check
id_i <- 25307
bw_ij <- 0.6

data_id <- neighbor_id(id_i, bw_ij, scale_11_=F, coord_unique, local_gwr_dist)
data_id_bw_1.4 <- neighbor_id(id_i, 1.4, scale_11_=F, coord_unique, local_gwr_dist)
nonzero_seizure %>% filter(id == id_i) %>% bind_rows(nonzero_coca_area %>% filter(id == id_i))


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

forward_pi_hat_model_drop <- c()
forward_pi_hat_var_drop <- c()
lasso_pi_hat_model_drop <- c()
lasso_pi_hat_var_drop <- c()
for (i in 1:nrow(forward_gwr_coefs_model_drop)) {
  id_i <- forward_gwr_coefs_model_drop$id[i]
  bw_i <- forward_gwr_coefs_model_drop$bw[i]
  neighbor_i <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  if (is.na(bw_i)) {forward_pi_hat_model_drop <- c(forward_pi_hat_model_drop, 0)}
  else {
    model_i <- local_GWR_coefs_forward_hyd_dest_alpha_0.1[[i]][[paste0("bw_", bw_i)]]
    forward_pi_hat_model_drop <- c(forward_pi_hat_model_drop, model_i$fitted.values[neighbor_i$id == id_i])
  }
  
  bw_i <- forward_gwr_coefs_var_drop$bw[i]
  neighbor_i <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  if (is.na(bw_i)) {forward_pi_hat_var_drop <- c(forward_pi_hat_var_drop, 0)}
  else {
    model_i <- local_GWR_coefs_forward_hyd_dest_alpha_0.1_drop[[i]][[paste0("bw_", bw_i)]]
    forward_pi_hat_var_drop <- c(forward_pi_hat_var_drop, model_i$fitted.values[neighbor_i$id == id_i])
  }
  
  bw_i <- lasso_gwr_coefs_model_drop$bw[i]
  neighbor_i <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  if (is.na(bw_i)) {lasso_pi_hat_model_drop <- c(lasso_pi_hat_model_drop, 0)}
  else {
    neighbor_i_data <- neighbor_i %>% filter(id == id_i) %>% select(-id, -y) %>% t %>% as.data.frame %>% rename(data=V1)
    neighbor_i_data$var_name <- row.names(neighbor_i_data)
    model_i <- local_GWR_coefs_lasso_hyd_dest[[i]][[paste0("bw_", bw_i)]]
    if (is.na(model_i) %>% length == 1)  {lasso_pi_hat_model_drop <- c(lasso_pi_hat_model_drop, 0)}
    else {
      intercept_i <- coef(model_i)[1]
      neighbor_i_data <- left_join(neighbor_i_data, data.frame(var_name = coef(model_i)[-1,] %>% names,
                                                               coef = coef(model_i)[-1,]),
                                   by = "var_name")
      bX <- intercept_i + sum(neighbor_i_data$data * neighbor_i_data$coef, na.rm=T)
      lasso_pi_hat_model_drop <- c(lasso_pi_hat_model_drop, 1/(1+exp(-bX)))
    }
  }
  
  bw_i <- lasso_gwr_coefs_var_drop$bw[i]
  neighbor_i <- neighbor_id(id_i, bw_i, scale_11_=F, coord_unique, local_gwr_dist)
  if (is.na(bw_i)) {lasso_pi_hat_var_drop <- c(lasso_pi_hat_var_drop, 0)}
  else {
    neighbor_i_data <- neighbor_i %>% filter(id == id_i) %>% select(-id, -y) %>% t %>% as.data.frame %>% rename(data=V1)
    neighbor_i_data$var_name <- row.names(neighbor_i_data)
    model_i <- local_GWR_coefs_lasso_hyd_dest_model_drop[[i]][[paste0("bw_", bw_i)]]
    if (is.na(model_i) %>% length == 1)  {lasso_pi_hat_var_drop <- c(lasso_pi_hat_var_drop, 0)}
    else {
      intercept_i <- coef(model_i)[1]
      neighbor_i_data <- left_join(neighbor_i_data, data.frame(var_name = coef(model_i)[-1,] %>% names,
                                                               coef = coef(model_i)[-1,]),
                                   by = "var_name")
      bX <- intercept_i + sum(neighbor_i_data$data * neighbor_i_data$coef, na.rm=T)
      lasso_pi_hat_var_drop <- c(lasso_pi_hat_var_drop, 1/(1+exp(-bX)))
    }
  }
}
forward_pi_hat_model_drop
forward_pi_hat_var_drop
gwr_forward_pi_hat <- tibble(id = forward_gwr_coefs_model_drop$id,
                             forward_pi_hat_model_drop = forward_pi_hat_model_drop,
                             forward_pi_hat_var_drop = forward_pi_hat_var_drop,
                             lasso_pi_hat_model_drop = lasso_pi_hat_model_drop,
                             lasso_pi_hat_var_drop = lasso_pi_hat_var_drop)

gwr_forward_lasso_data_norm <- gwr_forward_data$norm %>% 
  left_join(gwr_forward_pi_hat, by = "id") %>% 
  mutate(y_forward_model_drop = ifelse(forward_pi_hat_model_drop < 0.5, 0, 1),
         y_forward_var_drop = ifelse(forward_pi_hat_var_drop < 0.5, 0, 1),
         y_lasso_model_drop = ifelse(lasso_pi_hat_model_drop < 0.5, 0, 1),
         y_lasso_var_drop = ifelse(lasso_pi_hat_var_drop < 0.5, 0, 1)) %>% 
  relocate(id:y, y_forward_model_drop, y_forward_var_drop, y_lasso_model_drop, y_lasso_var_drop)
gwr_forward_lasso_data_norm
# write.csv(gwr_forward_lasso_data_norm, "Colombia Data/local GWR forward result predicted prices/GWR forward lasso predictions.csv", row.names=F)

gwr_forward_lasso_data_norm <- read.csv("Colombia Data/local GWR forward result predicted prices/GWR forward lasso predictions.csv") %>% as_tibble
confusionMatrix(gwr_forward_lasso_data_norm$y_forward_model_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")
confusionMatrix(gwr_forward_lasso_data_norm$y_forward_var_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")
confusionMatrix(gwr_forward_lasso_data_norm$y_lasso_model_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")
confusionMatrix(gwr_forward_lasso_data_norm$y_lasso_var_drop %>% as.factor, gwr_forward_lasso_data_norm$y %>% as.factor, positive = "1")

## pred diff coef maps
local_gwr_coef_diff_map <- function(local_GWR_coefs_, id_vec, dep_var, model_name) {
  forward_or_lasso <- ifelse(grepl("forward", model_name), "forward", "lasso")
  indep_vars <- names(local_GWR_coefs_)[-1]
  
  for (i in c(1, 3:length(indep_vars))) {
    var_name <- indep_vars[i]
    gwr_coefs_i <- data.frame(id=local_GWR_coefs_$id,
                              coef=local_GWR_coefs_[[var_name]],
                              rounded_coef=local_GWR_coefs_[[var_name]] %>% round(3))
      
    min_coef <- min(gwr_coefs_i$coef, na.rm=T)
    max_coef <- max(gwr_coefs_i$coef, na.rm=T)
    coef_map_coords <- map_df %>% 
      left_join(gwr_coefs_i %>% filter(id %in% id_vec), by="id")
    
    if (i == 1) {
      gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
        geom_polygon(aes(group=group, fill=coef),
                     color = "black",
                     linewidth = 0.1) + 
        expand_limits(x = depto_map$long, y = depto_map$lat) + 
        coord_quickmap() +
        scale_fill_viridis_c(na.value = "white", limits = c(0.5, 3.0)) +
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
        geom_polygon(aes(group=group, fill=coef, color = ifelse(coef_map_coords$id %in% id_vec, "red", "black")),
                     linewidth = 0.1) + 
        expand_limits(x = depto_map$long, y = depto_map$lat) + 
        coord_quickmap() +
        scale_color_manual(values = c("black", "red")) +
        scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                             values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                             limits = c(min_coef, max_coef),
                             na.value = "white") +
        labs(fill=var_name, x="", y="", title=dep_var) +
        guides(color = "none") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
    }
    
    ggsave(paste0("Colombia Data/local GWR ", forward_or_lasso, " result predicted prices/coef maps diff/", model_name,
                  "/local GWR ", model_name, " coef map diff ", var_name, " (05-05-2025).png"),
           gwr_coef_map, scale=1)
  }
}

forward_diff_id <- gwr_forward_lasso_data_norm %>% filter(y_forward_model_drop != y_forward_var_drop) %>% pull(id)
lasso_diff_id <- gwr_forward_lasso_data_norm %>% filter(y_lasso_model_drop != y_lasso_var_drop) %>% pull(id)
forward_diff_id %>% length # 67
lasso_diff_id %>% length # 105

local_gwr_coef_diff_map(forward_gwr_coefs_model_drop, forward_diff_id, dep_var = "hyd_destination", model_name = "forward model drop")
local_gwr_coef_diff_map(forward_gwr_coefs_var_drop, forward_diff_id, dep_var = "hyd_destination", model_name = "forward variable drop")
local_gwr_coef_diff_map(lasso_gwr_coefs_model_drop, lasso_diff_id, dep_var = "hyd_destination", model_name = "lasso model drop")
local_gwr_coef_diff_map(lasso_gwr_coefs_var_drop, lasso_diff_id, dep_var = "hyd_destination", model_name = "lasso variable drop")

gwr_forward_lasso_data_norm %>% filter(id %in% forward_diff_id)
forward_gwr_coefs_model_drop %>% filter(id %in% forward_diff_id)

forward_gwr_coefs_model_drop[,-(1:3)] %>% apply(1, function(x) sum(is.na(x))) %>% table
forward_gwr_coefs_model_drop %>% filter(id %in% forward_diff_id) %>% select(-id, -bw, -Intercept) %>% apply(1, function(x) sum(is.na(x)))
forward_gwr_coefs_var_drop[,-(1:3)] %>% apply(1, function(x) sum(is.na(x))) %>% table
forward_gwr_coefs_var_drop %>% filter(id %in% forward_diff_id) %>% select(-id, -bw, -Intercept) %>% apply(1, function(x) sum(is.na(x)))

forward_gwr_coefs_model_drop %>% filter(id %in% forward_diff_id) %>% ggplot() +
  geom_point(aes(x = 1:length(forward_diff_id), y = price_avg)) + 
  geom_point(aes(x = 1:length(forward_diff_id), y = price_avg), color = "red", data = forward_gwr_coefs_var_drop %>% filter(id %in% forward_diff_id))


local_gwr_coef_diff_plot <- function(local_GWR_coefs_model_drop, local_GWR_coefs_variable_drop, id_vec, dep_var, model_name) {
  indep_vars <- names(local_GWR_coefs_model_drop)[-1]
  
  for (i in 3:length(indep_vars)) {
    var_name <-indep_vars[i]
    gwr_coefs_model_drop_i <- data.frame(id=local_GWR_coefs_model_drop$id,
                                         coef=local_GWR_coefs_model_drop[[var_name]],
                                         rounded_coef=local_GWR_coefs_model_drop[[var_name]] %>% round(3))
    gwr_coefs_var_drop_i <- data.frame(id=local_GWR_coefs_variable_drop$id,
                                       coef=local_GWR_coefs_variable_drop[[var_name]],
                                       rounded_coef=local_GWR_coefs_variable_drop[[var_name]] %>% round(3))
    
    
    gwr_coef_diff_plot <- gwr_coefs_model_drop_i %>% filter(id %in% id_vec) %>% ggplot() +
      geom_point(aes(x = 1:length(id), y = coef), size=0.5) + 
      geom_point(aes(x = 1:length(id), y = coef),
                 color = "red", size=0.5, 
                 data = gwr_coefs_var_drop_i %>% filter(id %in% id_vec)) + 
      labs(x="", y=var_name, title=dep_var) +
      scale_color_manual(values = c("black", "red"),
                         labels = c("model drop", "var drop"))
    ggsave(paste0("Colombia Data/local GWR ", model_name, " result predicted prices/coef diff plot/local GWR ",
                  model_name, " coef diff plot ", var_name, " (05-05-2025).png"),
           gwr_coef_diff_plot, width = 10, height = 8, unit="cm")
  }
}

local_gwr_coef_misclassfied_plot <- function(local_GWR_coefs_, id_vec, dep_var, model_name) {
  forward_or_lasso <- ifelse(grepl("forward", model_name), "forward", "lasso")
  indep_vars <- names(local_GWR_coefs_)[-1]
  
  for (i in 3:length(indep_vars)) {
    var_name <-indep_vars[i]
    gwr_coefs_i <- data.frame(id=local_GWR_coefs_$id,
                              coef=local_GWR_coefs_[[var_name]],
                              rounded_coef=local_GWR_coefs_[[var_name]] %>% round(3))
    
    gwr_coef_plot <- gwr_coefs_i %>% ggplot() +
      geom_point(aes(x = 1:length(id), y = coef, color = id %in% id_vec), size=0.5) + 
      labs(x="", y=var_name, title=dep_var, color="") +
      scale_color_manual(values = c("black", "red"),
                         labels = c("correct", "mis."))
    ggsave(paste0("Colombia Data/local GWR ", forward_or_lasso, " result predicted prices/coef misclassified plot/",
                  model_name, "/local GWR ", model_name, " coef misclassified plot ", var_name, " (05-05-2025).png"),
           gwr_coef_plot, width = 10, height = 8, unit="cm")
  }
}

local_gwr_coef_diff_plot(forward_gwr_coefs_model_drop, forward_gwr_coefs_var_drop, forward_diff_id, dep_var = "hyd_destination", model_name = "forward")
local_gwr_coef_diff_plot(lasso_gwr_coefs_model_drop, lasso_gwr_coefs_var_drop, lasso_diff_id, dep_var = "hyd_destination", model_name = "lasso")

forward_model_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_forward_model_drop != y) %>% pull(id)
forward_var_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_forward_var_drop != y) %>% pull(id)
lasso_model_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_lasso_model_drop != y) %>% pull(id)
lasso_var_drop_misclassified_id <- gwr_forward_lasso_data_norm %>% filter(y_lasso_var_drop != y) %>% pull(id)

local_gwr_coef_misclassfied_plot(forward_gwr_coefs_model_drop, forward_model_drop_misclassified_id, dep_var = "hyd_destination", model_name = "forward model drop")
local_gwr_coef_misclassfied_plot(forward_gwr_coefs_var_drop, forward_var_drop_misclassified_id, dep_var = "hyd_destination", model_name = "forward var drop")
local_gwr_coef_misclassfied_plot(lasso_gwr_coefs_model_drop, lasso_model_drop_misclassified_id, dep_var = "hyd_destination", model_name = "lasso model drop")
local_gwr_coef_misclassfied_plot(lasso_gwr_coefs_var_drop, lasso_var_drop_misclassified_id, dep_var = "hyd_destination", model_name = "lasso var drop")


## kernel weight
kernel_gwr_coefs <- forward_gwr_coefs_model_drop %>% select(-bw)
kernel_gwr_coefs_mat <- matrix(NA, 1120, 13)
for (i in 1:nrow(kernel_gwr_coefs_model_drop)) {
  weight_i <- exp(-(gwr_forward_data$dist[i,])^2)
  logistic_model_i <- glm(y~., data=gwr_forward_data$norm[,-(1:2)], family = binomial, weight = weight_i)
  kernel_gwr_coefs_mat[i,] <- coef(logistic_model_i)
}
kernel_gwr_coefs[,-1] <- kernel_gwr_coefs_mat # overfitting with > 10E+20 coefs

