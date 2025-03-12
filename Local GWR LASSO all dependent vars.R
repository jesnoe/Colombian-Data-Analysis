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

cv.auc_mat <- read.csv("Colombia Data/local GWR lasso rescaled cv AUC (12-03-2024).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv") %>% as_tibble
local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
load("Colombia Data/local GWR lasso rescaled result (12-03-2024).RData")

regression_data_years <- read.csv("Colombia Data/regression data all municipios ever lab (02-05-2025).csv") %>% as_tibble
regression_data_years %>% select(id, year, n_hyd_labs, hyd_avg, hyd_price_distance, coca_area, coca_distance, hyd_seizures, n_armed_groups) %>% arrange(id)
regression_data_years$erad_aerial %>% table
regression_data_years$erad_manual %>% table # too small observations with non-zero eradication

minmax_scale <- function(vec) {
  return(vec/(max(vec) - min(vec)))
}

ever_regression_data_years <- function(dep_var) {
  names(regression_data_years)[which(names(regression_data_years) == dep_var)] <- "y"
  if (grepl("base", dep_var)) {
    regression_data_years_dep_var <- regression_data_years %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_PPI_labs = ifelse(sum(n_PPI_labs) > 0, 1, 0),
                price_avg = median(base_avg),
                price_distance = min(base_price_distance),
                coca_area = max(coca_area),
                coca_distance = min(coca_distance),
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
    regression_data_years_dep_var$lab_prob <- glm(n_PPI_labs~., data = regression_data_years_dep_var %>% select(-id, -y), family=binomial)$fitted
    gwr_data_coord <- left_join(regression_data_years_dep_var %>%
                                  select(-n_armed_groups, -n_PPI_labs),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }else if (grepl("hyd", dep_var)) {
    regression_data_years_dep_var <- regression_data_years %>% 
      group_by(id) %>% 
      summarize(y = ifelse(sum(y) > 0, 1, 0),
                n_hyd_labs = ifelse(sum(n_hyd_labs) > 0, 1, 0),
                price_avg = median(hyd_avg),
                price_distance = min(hyd_price_distance),
                coca_area = max(coca_area),
                coca_distance = min(coca_distance),
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
    regression_data_years_dep_var$lab_prob <- glm(n_hyd_labs~., data = regression_data_years_dep_var %>% select(-id, -y), family=binomial)$fitted
    gwr_data_coord <- left_join(regression_data_years_dep_var %>%
                                  select(-n_armed_groups, -n_hyd_labs),
                                municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
  }
  
  
  ### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
  
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
  


## reg coefs with LASSO (bandwidths are optimized by "AUC")

neighbor_id <- function(id_i, bw_i, scale_11_, coord_unique_, local_gwr_dist_) {
  if (scale_11_) gwr_data_id <- gwr_lasso_data$scale_11
  else gwr_data_id <- gwr_lasso_data$norm
  
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}
lasso_beta_check <- function(neighbor_id_, measure, nfolds.=10, w=NULL, lambda=F, interact=F) {
  if (interact) {
    if ("coca_area" %in% names(neighbor_id_)) {
      x_mat <- model.matrix(as.formula(y~.+coca_area*coca_distance+price_avg*price_distance+seizures*population+river_length*population+road_length*population),
                            neighbor_id_)[, -1]
    }else{
      x_mat <- model.matrix(as.formula(y~.+price_avg*price_distance+seizures*population+river_length*population+road_length*population), neighbor_id)[, -1]
    }
    
  }else{
    x_mat <- neighbor_id_ %>% select(-y) %>% as.matrix
  }
  y_vec <- neighbor_id_$y
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

predicted_y <- function(lasso_result_, pred_x_mat) {
  lasso_result_pred <- predict(lasso_result_$lasso, pred_x_mat, lasso_result_$cv$lambda.min, type="response")[,1]
  return(ifelse(lasso_result_pred < 0.5, 0, 1))
}

prediction_result <- function(lasso_result_) {
  lasso_result_fitted <- predict(lasso_result_$lasso, lasso_result_$x_mat, lasso_result_$cv$lambda.min, type="response")[,1]
  return(confusionMatrix(ifelse(lasso_result_fitted < 0.5, 0, 1) %>% as.factor, y_vec[id_index] %>% as.factor, positive="1"))
}

n_runs <- 10
measures <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1",
              "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy")

local_GWR_lasso <- function(type.measure_="default", interact_=F, scale_11_=F, weight_=NULL, dep_var, gwr_lasso_data_) {
  bwd_range <- seq(0.5, 3, by=0.1)
  coord_unique <- gwr_lasso_data_$coord
  local_gwr_dist <- gwr_lasso_data_$dist %>% as.matrix
  
  cv_dev_min_mat <- cv.auc_mat 
  local_GWR_coefs_lasso_result <- list()
  for (i in 1:nrow(local_GWR_coefs_bw_lasso)) {
    id_i <- local_GWR_coefs_bw_lasso$id[i]
    local_GWR_coefs_lasso_result[[paste0("id_", id_i)]] <- list()
    
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, coord_unique, local_gwr_dist)
      n_0_1 <- neighbor_ij$y %>% table
      
      if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      data_id_ij <- neighbor_ij %>% filter(id == id_i)
      neighbor_ij <- neighbor_ij %>% filter(id != id_i)
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      k <- 0
      while (k < 1) {
        tryCatch(
          {
            lasso_result_ij <- lasso_beta_check(neighbor_ij %>% select(-id), type.measure_, interact=interact_, w=weight_i)
            k <- k + 1
          },
          error = function(e) {print(e)}
        )
      }
      
      # n_nonzero <- sum(lasso_result_ij$lasso$beta != 0)
      # if (n_nonzero == 0) {
      #   cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
      #   local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      #   next
      # }
      
      cv_dev_min_mat[[paste0("bw_", bw_ij)]][i] <- min(lasso_result_ij$cv$cvm)
      local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- lasso_result_ij$lasso
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(cv_dev_min_mat=cv_dev_min_mat,
              lasso=local_GWR_coefs_lasso_result))
}

gwr_lasso_data <- ever_regression_data_years("hyd_destination")
set.seed(100)
start.time <- Sys.time()
local_GWR_coefs_lasso_hyd_dest_list <- local_GWR_lasso(dep_var = "hyd_destination", gwr_lasso_data_ = gwr_lasso_data)
end.time <- Sys.time()
end.time - start.time # 33.72533 mins for hyd_destination

local_GWR_coefs_lasso_hyd_dest <- local_GWR_coefs_lasso_hyd_dest_list$lasso
write.csv(local_GWR_coefs_lasso_hyd_dest_list$cv_dev_min_mat, "Colombia Data/local GWR lasso hyd_dest cv min dev (03-07-2024).csv", row.names = F)
save("local_GWR_coefs_lasso_hyd_dest", file = "Colombia Data/local GWR lasso hyd_dest (03-07-2025).RData")
rm(local_GWR_coefs_lasso_hyd_dest); rm(local_GWR_coefs_lasso_hyd_dest_list)

# set.seed(5468)
# start.time <- Sys.time()
# local_GWR_coefs_lasso_hyd_dest_list <- local_GWR_lasso(dep_var = "hyd_destination", interact_ = T, gwr_lasso_data_ = gwr_lasso_data)
# end.time <- Sys.time()
# end.time - start.time # 1.056492 hours for hyd_destination with interaction
# 
# local_GWR_coefs_lasso_hyd_dest_interact <- local_GWR_coefs_lasso_hyd_dest_list$lasso
# write.csv(local_GWR_coefs_lasso_hyd_dest_interact$cv_dev_min_mat, "Colombia Data/local GWR lasso hyd_dest interact cv min dev (03-07-2024).csv", row.names = F)
# save("local_GWR_coefs_lasso_hyd_dest_interact", file = "Colombia Data/local GWR lasso hyd_dest interact (03-07-2025).RData")
# rm(local_GWR_coefs_lasso_hyd_dest_interact); rm(local_GWR_coefs_lasso_hyd_dest_list)

gwr_lasso_data <- ever_regression_data_years("hyd_source")
# set.seed(100)
# start.time <- Sys.time()
# local_GWR_coefs_lasso_hyd_source_list <- local_GWR_lasso(dep_var = "hyd_source", gwr_lasso_data_ = gwr_lasso_data)
# end.time <- Sys.time()
# end.time - start.time # 34.92712 mins for hyd_source
# 
# local_GWR_coefs_lasso_hyd_source <- local_GWR_coefs_lasso_hyd_source_list$lasso
# local_GWR_coefs_lasso_hyd_source_list$cv_dev_min_mat[,-1] %>% as.matrix %>% as.vector %>% summary
# write.csv(local_GWR_coefs_lasso_hyd_source_list$cv_dev_min_mat, "Colombia Data/local GWR lasso hyd_source cv min dev (03-07-2024).csv", row.names = F)
# save("local_GWR_coefs_lasso_hyd_source", file = "Colombia Data/local GWR lasso hyd_source (03-07-2025).RData")
# rm(local_GWR_coefs_lasso_hyd_source); rm(local_GWR_coefs_lasso_hyd_source_list)

set.seed(5468)
start.time <- Sys.time()
local_GWR_coefs_lasso_hyd_source_list <- local_GWR_lasso(dep_var = "hyd_source", interact_ = T, gwr_lasso_data_ = gwr_lasso_data)
end.time <- Sys.time()
end.time - start.time # 2.568997 hours for hyd_source with interaction

local_GWR_coefs_lasso_hyd_source_interact <- local_GWR_coefs_lasso_hyd_source_list$lasso
write.csv(local_GWR_coefs_lasso_hyd_source_interact$cv_dev_min_mat, "Colombia Data/local GWR lasso hyd_source interact cv min dev (03-07-2024).csv", row.names = F)
save("local_GWR_coefs_lasso_hyd_source_interact", file = "Colombia Data/local GWR lasso hyd_source interact (03-07-2025).RData")
rm(local_GWR_coefs_lasso_hyd_source_interact); rm(local_GWR_coefs_lasso_hyd_source_list)


gwr_lasso_data <- ever_regression_data_years("base_destination")
# set.seed(100)
# start.time <- Sys.time()
# local_GWR_coefs_lasso_base_dest_list <- local_GWR_lasso(dep_var = "base_destination", gwr_lasso_data_ = gwr_lasso_data)
# end.time <- Sys.time()
# end.time - start.time # 36.30055 mins for base_destination
# 
# local_GWR_coefs_lasso_base_dest <- local_GWR_coefs_lasso_base_dest_list$lasso
# write.csv(local_GWR_coefs_lasso_base_dest_list$cv_dev_min_mat, "Colombia Data/local GWR lasso base_dest cv min dev (03-07-2024).csv", row.names = F)
# save("local_GWR_coefs_lasso_base_dest", file = "Colombia Data/local GWR lasso base_dest (03-07-2025).RData")
# rm(local_GWR_coefs_lasso_base_dest); rm(local_GWR_coefs_lasso_base_dest_list)

set.seed(5468)
start.time <- Sys.time()
local_GWR_coefs_lasso_base_dest_list <- local_GWR_lasso(dep_var = "base_destination", interact_ = T, gwr_lasso_data_ = gwr_lasso_data)
end.time <- Sys.time()
end.time - start.time # 1.809663 hours for base_destination with interaction

local_GWR_coefs_lasso_base_dest_interact <- local_GWR_coefs_lasso_base_dest_list$lasso
write.csv(local_GWR_coefs_lasso_base_dest_interact$cv_dev_min_mat, "Colombia Data/local GWR lasso base_dest interact cv min dev (03-07-2024).csv", row.names = F)
save("local_GWR_coefs_lasso_base_dest_interact", file = "Colombia Data/local GWR lasso base_dest interact (03-07-2025).RData")
rm(local_GWR_coefs_lasso_base_dest); rm(local_GWR_coefs_lasso_base_dest_interact)

gwr_lasso_data <- ever_regression_data_years("base_source")
# set.seed(100)
# start.time <- Sys.time()
# local_GWR_coefs_lasso_base_source_list <- local_GWR_lasso(dep_var = "base_source", gwr_lasso_data_ = gwr_lasso_data)
# end.time <- Sys.time()
# end.time - start.time # 34.13569 mins for base_source
# 
# local_GWR_coefs_lasso_base_source <- local_GWR_coefs_lasso_base_source_list$lasso
# local_GWR_coefs_lasso_base_source_list$cv_dev_min_mat[,-1] %>% as.matrix %>% as.vector %>% summary
# write.csv(local_GWR_coefs_lasso_base_source_list$cv_dev_min_mat, "Colombia Data/local GWR lasso base_source cv min dev (03-07-2024).csv", row.names = F)
# save("local_GWR_coefs_lasso_base_source", file = "Colombia Data/local GWR lasso base_source (03-07-2025).RData")
# rm(local_GWR_coefs_lasso_base_source); rm(local_GWR_coefs_lasso_base_source_list)

set.seed(5468)
start.time <- Sys.time()
local_GWR_coefs_lasso_base_source_list <- local_GWR_lasso(dep_var = "base_source", interact_ = T, gwr_lasso_data_ = gwr_lasso_data)
end.time <- Sys.time()
end.time - start.time # 1.787975 hours for base_source with interaction

local_GWR_coefs_lasso_base_source_interact <- local_GWR_coefs_lasso_base_source_list$lasso
write.csv(local_GWR_coefs_lasso_base_source_interact$cv_dev_min_mat, "Colombia Data/local GWR lasso base_source interact cv min dev (03-07-2024).csv", row.names = F)
save("local_GWR_coefs_lasso_base_source_interact", file = "Colombia Data/local GWR lasso base_source interact (03-07-2025).RData")
rm(local_GWR_coefs_lasso_base_source_interact); rm(local_GWR_coefs_lasso_base_source_list)


length(local_GWR_coefs_lasso_result)


local_gwr_lasso_coef_map <- function(local_GWR_coefs_lasso_list, cv_dev_min_mat, dep_var, interact_=NULL) {
  optimal_bw <- gsub("bw_", "",
                     cv_dev_min_mat[,-1] %>% apply(1, function(x) ifelse(sum(!is.na(x)) == 0, NA, bwd_range[which.min(x)]))
  ) %>% as.numeric
  
  indep_vars <- c("Intercept", rownames(coef(local_GWR_coefs_lasso_list$id_5001$bw_2.5))[-1])
  lasso_coef_table <- tibble(id = cv_dev_min_mat$id, bw=optimal_bw)
  lasso_coef_mat <- matrix(NA, nrow(lasso_coef_table), length(indep_vars))
  
  for (i in 1:nrow(lasso_coef_table)) {
    bw_i <- lasso_coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_lasso_i <- local_GWR_coefs_lasso_list[[i]][[paste0("bw_", bw_i)]]
    lasso_coef_mat[i,] <- coef(local_GWR_lasso_i)[,1]
  }
  
  lasso_coef_table <- bind_cols(lasso_coef_table, lasso_coef_mat)
  names(lasso_coef_table)[-(1:2)] <- indep_vars
  if (!is.null(interact_)) interact_ <- " interact"
  write.csv(lasso_coef_table,
            paste0("Colombia Data/local GWR lasso result/local GWR lasso coefs ", dep_var, interact_, " (03-11-2025).csv"),
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
    
    ggsave(paste0("Colombia Data/local GWR lasso result/coef maps/", dep_var, interact_, "/local GWR lasso coef map ", var_name, " ", dep_var, interact_, " (03-11-2025).png"),
           gwr_coef_map, scale=1)
  }
}

bwd_range <- seq(0.5, 3, by=0.1)
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
load("Colombia Data/local GWR lasso hyd_dest (03-07-2025).RData") # local_GWR_coefs_lasso_hyd_dest
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_dest cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_hyd_dest, cv_dev_min_mat_, "hyd_destination"); rm(local_GWR_coefs_lasso_hyd_dest)

load("Colombia Data/local GWR lasso hyd_dest interact (03-07-2025).RData") # local_GWR_coefs_lasso_hyd_dest_interact
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_dest interact cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_hyd_dest_interact, cv_dev_min_mat_, "hyd_destination", interact_ = T)

load("Colombia Data/local GWR lasso hyd_source (03-07-2025).RData") # local_GWR_coefs_lasso_hyd_source
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_source cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_hyd_source, cv_dev_min_mat_, "hyd_source"); rm(local_GWR_coefs_lasso_hyd_source)

load("Colombia Data/local GWR lasso hyd_source interact (03-07-2025).RData") # local_GWR_coefs_lasso_hyd_source_interact
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso hyd_source interact cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_hyd_source, cv_dev_min_mat_, "hyd_source"); rm(local_GWR_coefs_lasso_hyd_source_interact)

load("Colombia Data/local GWR lasso base_dest (03-07-2025).RData") # local_GWR_coefs_lasso_base_dest
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso base_dest cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_base_dest, cv_dev_min_mat_, "base_source"); rm(local_GWR_coefs_lasso_base_dest)

load("Colombia Data/local GWR lasso base_dest interact (03-07-2025).RData") # local_GWR_coefs_lasso_base_dest_interact
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso base_dest interact cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_base_dest_interact, cv_dev_min_mat_, "base_source"); rm(local_GWR_coefs_lasso_base_dest)

load("Colombia Data/local GWR lasso base_source (03-07-2025).RData") # local_GWR_coefs_lasso_base_source
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso base_source cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_base_source, cv_dev_min_mat_, "base_destination"); rm(local_GWR_coefs_lasso_base_source)

load("Colombia Data/local GWR lasso base_source interact (03-07-2025).RData") # local_GWR_coefs_lasso_base_source_interact
cv_dev_min_mat_ <- read.csv("Colombia Data/local GWR lasso base_source interact cv min dev (03-07-2024).csv") %>% as_tibble
local_gwr_lasso_coef_map(local_GWR_coefs_lasso_base_source_interact, cv_dev_min_mat_, "base_destination"); rm(local_GWR_coefs_lasso_base_source)

local_GWR_coefs_lasso_list <- local_GWR_coefs_lasso_hyd_dest_interact; dep_var <- "hyd_destination"; cv_dev_min_mat <- cv_dev_min_mat_

dep_var_map <- function(dep_var) {
  result <- map_df %>% 
    left_join(ever_regression_data_years(dep_var)$norm %>% select(id, y), by="id") %>% 
    ggplot(aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=y),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map$long, y = depto_map$lat) + 
    coord_quickmap() +
    scale_fill_viridis_d(na.value = "white") +
    labs(fill="y", x="", y="", title=paste(dep_var, "ever existed")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Colombia Data/local GWR lasso result/coef maps/", dep_var, " map.png"), result, scale=1)
}
dep_var_map("hyd_destination")
dep_var_map("hyd_source")
dep_var_map("base_destination")
dep_var_map("base_source")

## coef maps with LASSO


local_GWR_coefs_bw_lasso <- local_gwr_forward_coefs
local_GWR_coefs_bw_lasso$bw <- optimal_bw
local_GWR_coefs_bw_lasso_mat <- local_GWR_coefs_bw_lasso[,-(1:2)] %>% as.matrix
var_table <- tibble(var_name=names(local_GWR_coefs_bw_lasso)[-(1:2)])
for (i in 1:nrow(local_GWR_coefs_bw_lasso)) {
  id_i <- local_GWR_coefs_bw_lasso$id[i]
  bw_i <- local_GWR_coefs_bw_lasso$bw[i]
  id_name_i <- paste0("id_", id_i)
  bw_name_i <- paste0("bw_", bw_i)
  local_GWR_lasso_i <- local_GWR_coefs_lasso_result[[id_name_i]][[bw_name_i]]
  if (is.na(bw_i) | is.na(local_GWR_lasso_i)[1]) {
    local_GWR_coefs_bw_lasso_mat[i,] <- rep(NA, 13)
    next
  }
  
  coefs_i <- tibble(var_name = c("Intercept", row.names(local_GWR_lasso_i$beta)),
                    coef = c(local_GWR_lasso_i$a0, local_GWR_lasso_i$beta[,1]))
  coefs_i <- left_join(var_table, coefs_i, by="var_name")
  local_GWR_coefs_bw_lasso_mat[i,] <- coefs_i$coef
  if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
}

local_GWR_coefs_bw_lasso[, -(1:2)] <- local_GWR_coefs_bw_lasso_mat
# write.csv(local_GWR_coefs_bw_lasso, "Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv", row.names=F)



gwr_coefs_i <- data.frame(id=local_GWR_coefs_bw_lasso$id,
                          coef=local_GWR_coefs_bw_lasso$bw)
coef_map_coords <- map_df %>% 
  left_join(gwr_coefs_i, by="id")

gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=coef),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = depto_map$long, y = depto_map$lat) + 
  coord_quickmap() +
  scale_fill_viridis_c(na.value = "white") +
  labs(fill="bandwidth", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

ggsave("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso rescaled/hyd destination GWR lasso bandwidth.png",
       gwr_coef_map, scale=1)


##