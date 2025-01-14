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
}

cv.auc_mat <- read.csv("Colombia Data/local GWR lasso rescaled cv AUC (12-03-2024).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv") %>% as_tibble
local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
load("Colombia Data/local GWR lasso rescaled result (12-03-2024).RData") # load local_GWR_coefs_lasso_result

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
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

municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
municipios_sf$area_km2 <- st_area(municipios_sf) %>% units::set_units("km^2") %>% as.numeric
gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination %>% 
  left_join(municipios_sf %>% as_tibble %>% select(id, area_km2), by="id") %>% 
  mutate(coca_area = coca_area / area_km2,
         river_length = river_length / area_km2,
         road_length = road_length / area_km2,
         hyd_destination = hyd_destination %>% as.character %>% as.integer) %>% 
  select(-area_km2)

minmax_scale <- function(vec) {
  return(vec/(max(vec) - min(vec)))
}
gwr_data_hyd_destination_by_area_11_scale <- gwr_data_hyd_destination_by_area %>% 
  mutate(across(PPI_lab_prob:population, ~ scales::rescale(.x, c(-1,1))))

gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination_by_area %>% 
  mutate(across(PPI_lab_prob:population, ~ scale(.x)[,1]))


bwd_range <- seq(0.5, 3, by=0.1)
local_gwr_data_id <- gwr_data_hyd_destination_by_area %>% select(id)
local_gwr_data <- gwr_data_hyd_destination_by_area %>% select(-id, -municipio, -year)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)


# extreme  id: 50270, 50450
# moderate id: 5792, 76041
local_GWR_coefs_bw_lasso %>% filter(id %in% c(50270, 50450, 5792, 76041)) %>% view
ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=ifelse(id %in% c(50270, 50450), "extreme", 
                                            ifelse(id %in% c(5792, 76041), "moderate", NA))),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = depto_map$long, y = depto_map$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  scale_fill_manual(values = c("extreme" = "red",
                               "moderate" = "orange"),
                    na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

neighbor_id <- function(id_i, bw_i) {
  id_i_index <- which(coord_unique$id == id_i)
  i <- id_i_index 
  result <- gwr_data_hyd_destination_by_area %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-(id:year))
  return(result)
}
neighbor_id_11 <- function(id_i, bw_i) { # LASSO for [-1, 1] scaled data
  id_i_index <- which(coord_unique$id == id_i)
  i <- id_i_index 
  result <- gwr_data_hyd_destination_by_area_11_scale %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-(id:year))
  return(result)
}

lasso_beta_check <- function(neighbor_id, measure, nfolds.=10, w=NULL, lambda=F, interact=F) {
  if (interact) {
    if ("coca_area" %in% names(neighbor_id)) {
      x_mat <- model.matrix(as.formula(hyd_destination~.+coca_area*coca_distance+hyd_avg*hyd_price_distance), neighbor_id)[, -1]
    }else{
      x_mat <- model.matrix(as.formula(hyd_destination~.+hyd_avg*hyd_price_distance), neighbor_id)[, -1]
    }
    
  }else{
    x_mat <- neighbor_id %>% select(-hyd_destination) %>% as.matrix
  }
  y_vec <- neighbor_id$hyd_destination
  id_cv.glmnet <- cv.glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            nfolds = nfolds.,
                            weights = w,
                            type.measure = measure)
  lambda_lasso <- ifelse(lambda, lambda, id_cv.glmnet$lambda.min)
  lasso_result_id <- glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            alpha = 1,
                            weights = w,
                            lambda = lambda_lasso)
  return(list(cv=id_cv.glmnet, lasso=lasso_result_id, x_mat=x_mat))
}

prediction_result <- function(lasso_result_) {
  lasso_result_fitted <- predict(lasso_result_$lasso, lasso_result_$x_mat, lasso_result_$cv$lambda.min, type="response")[,1]
  return(confusionMatrix(ifelse(lasso_result_fitted < 0.5, 0, 1) %>% as.factor, y_vec %>% as.factor, positive="1"))
}

  # id=50270
bw_id_50270 <- local_GWR_coefs_bw_lasso %>% filter(id == 50270) %>% pull(bw)
neighbor_id_50450 <- neighbor_id(50270, bw_id_50270)
neighbor_id_50270 <- neighbor_id(50270, bw_id_50270)
neighbor_id_11_50270 <- neighbor_id_11(50270, bw_id_50270)
local_GWR_coefs_lasso_result$id_50270$bw_1.6
local_GWR_coefs_lasso_result$id_50270$bw_1.6$beta

x_mat <- neighbor_id_50270 %>% select(-hyd_destination) %>% as.matrix
y_vec <- neighbor_id_50270$hyd_destination

lasso_result_id_50270 <- glmnet(x = x_mat,
                                y = y_vec,
                                family = "binomial",
                                alpha = 1,
                                lambda = 0.007506)
lasso_result_id_50270$beta # same lambda, same result

lasso_result <- lasso_beta_check(neighbor_id_50270, "default")
lasso_result <- lasso_beta_check(neighbor_id_50270, "default", interact=T)

lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

  ##
lasso_result <- lasso_beta_check(neighbor_id_50270 %>% select(-PPI_lab_prob, -coca_area, -coca_distance), "default")
lasso_result <- lasso_beta_check(neighbor_id_50270 %>% select(-PPI_lab_prob, -coca_area, -coca_distance), "default", interact = T)

lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

  ## Increased sensitivity and decreased specificity with [-1,1] scale
lasso_result <- lasso_beta_check(neighbor_id_11_50270, "default")
lasso_result <- lasso_beta_check(neighbor_id_11_50270, "default", interact=T)
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

  
lasso_result <- lasso_beta_check(neighbor_id_50270, "mse")
lasso_result$cv; lasso_result$lasso$beta
lasso_result <- lasso_beta_check(neighbor_id_11_50270, "mse")
lasso_result$cv; lasso_result$lasso$beta

  ## Too much false positive with AUC criteria
lasso_result <- lasso_beta_check(neighbor_id_50270, "auc")
lasso_result <- lasso_beta_check(neighbor_id_50270, "auc", interact=T) # interaction model does not converge
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

  ##
lasso_result <- lasso_beta_check(neighbor_id_50270, "default", w=ifelse(neighbor_id_50270$hyd_destination, 0.9, 0.1))
lasso_result <- lasso_beta_check(neighbor_id_50270, "default", w=ifelse(neighbor_id_50270$hyd_destination, 0.9, 0.1), interact = T)
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

lasso_result <- lasso_beta_check(neighbor_id_11_50270, "default", w=ifelse(neighbor_id_50270$hyd_destination, 0.9, 0.1))
lasso_result$cv; lasso_result$lasso$beta

  # id=50450
bw_id_50450 <- local_GWR_coefs_bw_lasso %>% filter(id == 50450) %>% pull(bw)
neighbor_id_50450 <- neighbor_id(50450, bw_id_50450)
neighbor_id_11_50450 <- neighbor_id_11(50450, bw_id_50450)
local_GWR_coefs_lasso_result$id_50450$bw_2.8
local_GWR_coefs_lasso_result$id_50450$bw_2.8$beta

x_mat <- neighbor_id_50450 %>% select(-hyd_destination) %>% as.matrix
y_vec <- neighbor_id_50450$hyd_destination
lasso_result_id_50450 <- glmnet(x = x_mat,
                                y = y_vec,
                                family = "binomial",
                                alpha = 1,
                                lambda = 0.0001232)
lasso_result_id_50450$beta # same lambda, similar result

lasso_result <- lasso_beta_check(neighbor_id_50450, "default")
lasso_result <- lasso_beta_check(neighbor_id_50450, "default", interact=T) # non-zero coca area*distance (higher false positive)
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

lasso_result <- lasso_beta_check(neighbor_id_50450 %>% select(-PPI_lab_prob, -coca_area, -coca_distance), "default")
lasso_result <- lasso_beta_check(neighbor_id_50450 %>% select(-PPI_lab_prob, -coca_area, -coca_distance), "default", interact = T) # does not converge
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

lasso_result <- lasso_beta_check(neighbor_id_11_50450, "default")
lasso_result <- lasso_beta_check(neighbor_id_11_50450, "default", interact = T)
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

  ## AUC does not converge
lasso_result <- lasso_beta_check(neighbor_id_50450, "auc")
lasso_result <- lasso_beta_check(neighbor_id_50450, "auc", interact = T)
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

lasso_result <- lasso_beta_check(neighbor_id_11_50450, "auc")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_50450, "default", w=ifelse(neighbor_id_50450$hyd_destination, 0.9, 0.1))
lasso_result <- lasso_beta_check(neighbor_id_50450, "default", w=ifelse(neighbor_id_50450$hyd_destination, 0.9, 0.1), interact = T)
lasso_result$cv; lasso_result$lasso$beta
prediction_result(lasso_result)

lasso_result <- lasso_beta_check(neighbor_id_11_50450, "default", w=ifelse(neighbor_id_50450$hyd_destination, 0.9, 0.1))
lasso_result$cv; lasso_result$lasso$beta


# id=5792
bw_id_5792 <- local_GWR_coefs_bw_lasso %>% filter(id == 5792) %>% pull(bw)
neighbor_id_5792 <- neighbor_id(5792, bw_id_5792)
neighbor_id_11_5792 <- neighbor_id_11(5792, bw_id_5792)
local_GWR_coefs_lasso_result$id_5792$bw_0.5
local_GWR_coefs_lasso_result$id_5792$bw_0.5$beta

x_mat <- neighbor_id_5792 %>% select(-hyd_destination) %>% as.matrix
y_vec <- neighbor_id_5792$hyd_destination
lasso_result_id_5792 <- glmnet(x = x_mat,
                               y = y_vec,
                               family = "binomial",
                               alpha = 1,
                               lambda = 0.07787)
lasso_result_id_5792$beta # same lambda, same result

lasso_result <- lasso_beta_check(neighbor_id_5792, "default")
lasso_result$cv; lasso_result$lasso$beta
lasso_result <- lasso_beta_check(neighbor_id_5792 %>% select(-PPI_lab_prob, -coca_area, -coca_distance), "default")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_11_5792, "default")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_5792, "auc")
lasso_result$cv; lasso_result$lasso$beta
lasso_result <- lasso_beta_check(neighbor_id_11_5792, "auc")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_5792, "default", w=ifelse(neighbor_id_5792$hyd_destination, 0.9, 0.1))
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_11_5792, "default", w=ifelse(neighbor_id_5792$hyd_destination, 0.9, 0.1))
lasso_result$cv; lasso_result$lasso$beta

# id=76041
bw_id_76041 <- local_GWR_coefs_bw_lasso %>% filter(id == 76041) %>% pull(bw)
neighbor_id_76041 <- neighbor_id(76041, bw_id_76041)
neighbor_id_11_76041 <- neighbor_id_11(76041, bw_id_76041)
local_GWR_coefs_lasso_result$id_76041$bw_0.8
local_GWR_coefs_lasso_result$id_76041$bw_0.8$beta

x_mat <- neighbor_id_76041 %>% select(-hyd_destination) %>% as.matrix
y_vec <- neighbor_id_76041$hyd_destination
lasso_result_id_76041 <- glmnet(x = x_mat,
                                y = y_vec,
                                family = "binomial",
                                alpha = 1,
                                lambda = 0.0004056)
lasso_result_id_76041$beta # same lambda, similar result

lasso_result <- lasso_beta_check(neighbor_id_76041, "default")
lasso_result$cv; lasso_result$lasso$beta
lasso_result <- lasso_beta_check(neighbor_id_76041 %>% select(-PPI_lab_prob, -coca_area, -coca_distance), "default")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_11_76041, "default")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_76041, "auc")
lasso_result$cv; lasso_result$lasso$beta
lasso_result <- lasso_beta_check(neighbor_id_11_76041, "auc")
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_76041, "default", w=ifelse(neighbor_id_76041$hyd_destination, 0.9, 0.1))
lasso_result$cv; lasso_result$lasso$beta

lasso_result <- lasso_beta_check(neighbor_id_11_76041, "default", w=ifelse(neighbor_id_76041$hyd_destination, 0.9, 0.1))
lasso_result$cv; lasso_result$lasso$beta