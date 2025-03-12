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
load("Colombia Data/local GWR lasso rescaled result (12-03-2024).RData") # load local_GWR_coefs_lasso_result

regression_data_years <- read.csv("Colombia Data/regression data all municipios ever lab (02-05-2025).csv") %>% as_tibble
regression_data_years %>% select(id, year, n_hyd_labs, hyd_avg, hyd_price_distance, coca_area, coca_distance, hyd_seizures, n_armed_groups) %>% arrange(id)
regression_data_years$erad_aerial %>% table
regression_data_years$erad_manual %>% table

ever_regression_data_years <- regression_data_years %>% 
  group_by(id) %>% 
  summarize(hyd_destination = ifelse(sum(hyd_destination) > 0, 1, 0),
            n_hyd_labs = ifelse(sum(n_hyd_labs) > 0, 1, 0),
            hyd_avg = median(hyd_avg),
            hyd_price_distance = min(hyd_price_distance),
            coca_area = max(coca_area),
            coca_distance = min(coca_distance),
            hyd_seizures = sum(hyd_seizures),
            n_armed_groups = sum(n_armed_groups),
            river_length=river_length[1],
            road_length=road_length[1],
            population = population[1],
            airport = airport[1]) %>% 
  mutate(hyd_avg=scale(hyd_avg)[,1],
         population=scale(population)[,1],
         hyd_seizures = scale(hyd_seizures)[,1],
         armed_group = ifelse(n_armed_groups > 0, 1, 0))
ever_regression_data_years$hyd_lab_prob <- glm(n_hyd_labs~., data = ever_regression_data_years %>% select(-id, -hyd_destination), family=binomial)$fitted
ever_regression_data_years$n_hyd_labs %>% table
ever_regression_data_years$hyd_destination %>% table # 0: 643, 1: 477

### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
gwr_hyd_destination_coord <- left_join(ever_regression_data_years %>%
                                         select(-n_armed_groups, -n_hyd_labs),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
gwr_hyd_destination_coord$hyd_destination %>% table # 0: 2802, 1: 558 -> different by years

coord_unique <- gwr_hyd_destination_coord %>% select(id, long, lat) %>% unique
gwr_hyd_destination_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T)
dim(gwr_hyd_destination_dist)
gwr_data_hyd_destination <- gwr_hyd_destination_coord %>%
  select(-long, -lat) %>% 
  mutate(hyd_destination = as.factor(hyd_destination))

municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
municipios_sf$area_km2 <- st_area(municipios_sf) %>% units::set_units("km^2") %>% as.numeric
gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination %>% 
  left_join(municipios_sf %>% as_tibble %>% select(id, area_km2), by="id") %>% 
  mutate(coca_area = scale(coca_area / area_km2)[,1],
         river_length = scale(river_length / area_km2)[,1],
         road_length = scale(road_length / area_km2)[,1],
         hyd_destination = hyd_destination %>% as.character %>% as.integer) %>% 
  select(-area_km2)

minmax_scale <- function(vec) {
  return(vec/(max(vec) - min(vec)))
}
gwr_data_hyd_destination_by_area_11_scale <- gwr_data_hyd_destination_by_area %>% 
  mutate(across(hyd_avg:population, ~ scales::rescale(.x, c(-1,1))))

# gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination_by_area %>% 
#   mutate(across(hyd_avg:population, ~ scale(.x)[,1]))

# pred_tb (34 municipios are skipped due to na bw)
ferry <- ferry %>%
  mutate(ferry=ifelse(n_ferry > 0, 1, 0),
         police=ifelse(n_police > 0, 1, 0),
         military=ifelse(n_military > 0, 1, 0))
gwr_data_hyd_destination_by_area <- left_join(gwr_data_hyd_destination_by_area, ferry %>% select(id, ferry:military), by="id")# %>% 
  # mutate(airport=as.factor(airport),
  #        armed_group=as.factor(armed_group))

gwr_data_hyd_destination_by_area_11_scale <- left_join(gwr_data_hyd_destination_by_area_11_scale, ferry %>% select(id, ferry:military), by="id")# %>% 
  # mutate(airport=as.factor(airport),
  #        armed_group=as.factor(armed_group))


bwd_range <- seq(0.5, 3, by=0.1)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)


neighbor_id <- function(id_i, bw_i) {
  id_i_index <- which(coord_unique$id == id_i)
  i <- id_i_index 
  result <- gwr_data_hyd_destination_by_area %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}
neighbor_id_11 <- function(id_i, bw_i) { # LASSO for [-1, 1] scaled data
  id_i_index <- which(coord_unique$id == id_i)
  i <- id_i_index 
  result <- gwr_data_hyd_destination_by_area_11_scale %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}

lasso_beta_check <- function(neighbor_id_, measure, nfolds.=10, w=NULL, lambda=F, interact=F) {
  if (interact) {
    if ("coca_area" %in% names(neighbor_id_)) {
      x_mat <- model.matrix(as.formula(hyd_destination~.+coca_area*coca_distance+hyd_avg*hyd_price_distance+hyd_seizures*population+river_length*population+road_length*population),
                            neighbor_id_)[, -1]
    }else{
      x_mat <- model.matrix(as.formula(hyd_destination~.+hyd_avg*hyd_price_distance+hyd_seizures*population+river_length*population+road_length*population), neighbor_id)[, -1]
    }
    
  }else{
    x_mat <- neighbor_id_ %>% select(-hyd_destination) %>% as.matrix
  }
  y_vec <- neighbor_id_$hyd_destination
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
type.measure_ <- "default"; interact_<-F; scale_11<-F; weight_=NULL
lasso_exp <- function(type.measure_, interact_, scale_11, weight_=NULL) {
  y_pred_vec <- c()
  y_vec <- c()
  id_vec <- c()
  bw_vec <- c()
  for (i in 1:nrow(local_GWR_coefs_bw_lasso)) {
    id_i <- local_GWR_coefs_bw_lasso$id[i]
    bw_id_i <- local_GWR_coefs_bw_lasso$bw[i]
    if (is.na(bw_id_i)) next
    
    if (scale_11) {
      neighbor_11_i <- neighbor_id_11(id_i, bw_id_i)
      n_0_1 <- neighbor_11_i$hyd_destination %>% table
      while(sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        bw_id_i <- bw_id_i + 0.1
        neighbor_11_i <- neighbor_id_11(id_i, bw_id_i)
      }
      data_id_i <- neighbor_11_i %>% filter(id == id_i)
      neighbor_11_i <- neighbor_11_i %>% filter(id != id_i)
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_11_i$hyd_destination, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      lasso_result_ <- lasso_beta_check(neighbor_11_i %>% select(-id), type.measure_, interact=interact_, w=weight_i)
    }else{
      neighbor_i <- neighbor_id(id_i, bw_id_i)
      n_0_1 <- neighbor_i$hyd_destination %>% table
      while(sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        bw_id_i <- bw_id_i + 0.1
        neighbor_i <- neighbor_id(id_i, bw_id_i)
        n_0_1 <- neighbor_i$hyd_destination %>% table
      }
      data_id_i <- neighbor_i %>% filter(id == id_i)
      neighbor_i <- neighbor_i %>% filter(id != id_i)
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_i$hyd_destination, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      lasso_result_ <- lasso_beta_check(neighbor_i %>% select(-id), type.measure_, interact=interact_, w=weight_i)
    }
    
    if (interact_) {
      if ("coca_area" %in% names(neighbor_i)) {
        y_new <- model.matrix(as.formula(hyd_destination~.+coca_area*coca_distance+hyd_avg*hyd_price_distance+hyd_seizures*population+river_length*population+road_length*population), data_id_i)[, -1]
        var_names <- names(y_new)
        y_new <- y_new %>% matrix(nrow=1) %>% as_tibble
        names(y_new) <- var_names
      }else{
        y_new <- model.matrix(as.formula(hyd_destination~.+hyd_avg*hyd_price_distance+hyd_seizures*population+river_length*population+road_length*population), data_id_i)[, -1] %>% as_tibble
      }
      y_new$hyd_destination <- data_id_i$hyd_destination
      
    }else{
      y_new <- data_id_i
    }
    
    y_pred_i <- predicted_y(lasso_result_, y_new %>% select(-id, -hyd_destination) %>% as.matrix)
    y_pred_vec <- c(y_pred_vec, y_pred_i)
    y_vec <- c(y_vec, data_id_i$hyd_destination)
    id_vec <- c(id_vec, data_id_i$id)
    bw_vec <- c(bw_vec, bw_id_i)
  }
  return(tibble(id=id_vec, hyd_destination=y_vec, prediction=y_pred_vec, bw=bw_vec))
}

# generate results
n_runs <- 20
pred_tb_default <- list()
start.time <- Sys.time()
j <- 1
while (j < n_runs+1) {
  tryCatch(
    {
      pred_tb_default[[paste0("result", j)]] <- lasso_exp("default", F, F)
      message(j, "th complete")
      j <- j + 1
    },
    error = function(e) {print(e)}
  )
}
end.time <- Sys.time()
end.time - start.time # 
save("pred_tb_default", file = "Colombia Data/local GWR lasso default pred table ever lab (02-18-2025).RData")

pred_tb_default_weight_9_1 <- list()
start.time <- Sys.time()
j <- 1
while (j < n_runs+1) {
  if (Sys.time() - start.time > as.difftime(120, units="mins")) break
  tryCatch(
    {
      pred_tb_default_weight_9_1[[paste0("result", j)]] <- lasso_exp("default", F, F, weight_=c(0.9, 0.1))
      message(j, "th complete")
      j <- j + 1
    },
    error = function(e) {print(e)}
  )
}
end.time <- Sys.time()
end.time - start.time # 30.7978 mins
save("pred_tb_default_weight_9_1", file = "Colombia Data/local GWR lasso default-weight 9-1 pred table ever lab (02-18-2025).RData")


pred_tb_default_weight_7_3 <- list()
start.time <- Sys.time()
j <- 1
while (j < n_runs+1) {
  if (Sys.time() - start.time > as.difftime(120, units="mins")) break
  tryCatch(
    {
      pred_tb_default_weight_7_3[[paste0("result", j)]] <- lasso_exp("default", F, F, weight_=c(0.7, 0.3))
      message(j, "th complete")
      j <- j + 1
    },
    error = function(e) {print(e)}
  )
}
end.time <- Sys.time()
end.time - start.time # 29.03579 mins
save("pred_tb_default_weight_7_3", file = "Colombia Data/local GWR lasso default-weight 7-3 pred table ever lab (02-18-2025).RData")


pred_tb_default_interact <- list()
start.time <- Sys.time()
j <- 1
while (j < n_runs+1) {
  if (Sys.time() - start.time > as.difftime(120, units="mins")) break
  tryCatch(
    {
      pred_tb_default_interact[[paste0("result", j)]] <- lasso_exp("default", T, F)
      message(j, "th complete")
      j <- j + 1
    },
    error = function(e) {print(e)}
  )
}
end.time <- Sys.time()
end.time - start.time # 52.30188 mins
save("pred_tb_default_interact", file = "Colombia Data/local GWR lasso default-interact pred table ever lab (02-18-2025).RData")

pred_tb_default_interact_weight <- list()
start.time <- Sys.time()
j <- 1
while (j < n_runs+1) {
  if (Sys.time() - start.time > as.difftime(120, units="mins")) break
  tryCatch(
    {
      pred_tb_default_interact_weight[[paste0("result", j)]] <- lasso_exp("default", T, F, weight_=c(0.7, 0.3))
      message(j, "th complete")
      j <- j + 1
    },
    error = function(e) {
      print(e)
      }
  )
}
end.time <- Sys.time()
end.time - start.time # 55.78868 mins
save("pred_tb_default_interact_weight", file = "Colombia Data/local GWR lasso default-interact-weight 7-3 pred table ever lab (02-18-2025).RData")


# map
load("Colombia Data/local GWR lasso default pred table ever lab (02-18-2025).RData")
pred <- rep(0, nrow(pred_tb_default$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default[[i]]$prediction
}
pred_freq_default <- pred_tb_default$result1 %>%
  mutate(prediction=pred,
         accuracy = ifelse(hyd_destination, prediction/20, 1-prediction/20),
         FP = ifelse(hyd_destination, 0, prediction),
         FN = ifelse(hyd_destination, 20-prediction, 0))

load("Colombia Data/local GWR lasso default-weight 9-1 pred table ever lab (02-18-2025).RData")
pred <- rep(0, nrow(pred_tb_default_weight_9_1$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_weight_9_1[[i]]$prediction
}
pred_freq_default_weight_0.9_0.1 <- pred_tb_default_weight_9_1$result1 %>% 
  mutate(prediction=pred,
         accuracy = ifelse(hyd_destination, prediction/20, 1-prediction/20),
         FP = ifelse(hyd_destination, 0, prediction),
         FN = ifelse(hyd_destination, 20-prediction, 0))

load("Colombia Data/local GWR lasso default-weight 7-3 pred table ever lab (02-18-2025).RData")
pred <- rep(0, nrow(pred_tb_default_weight_7_3$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_weight_7_3[[i]]$prediction
}
pred_freq_default_weight_0.7_0.3 <- pred_tb_default_weight_7_3$result1 %>%
  mutate(prediction=pred,
         accuracy = ifelse(hyd_destination, prediction/20, 1-prediction/20),
         FP = ifelse(hyd_destination, 0, prediction),
         FN = ifelse(hyd_destination, 20-prediction, 0))

load("Colombia Data/local GWR lasso default-interact pred table ever lab (02-18-2025).RData")
pred <- rep(0, nrow(pred_tb_default_interact$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_interact[[i]]$prediction
}
pred_freq_default_interact <- pred_tb_default_interact$result1 %>%
  mutate(prediction=pred,
         accuracy = ifelse(hyd_destination, prediction/20, 1-prediction/20),
         FP = ifelse(hyd_destination, 0, prediction),
         FN = ifelse(hyd_destination, 20-prediction, 0))

load("Colombia Data/local GWR lasso default-interact-weight 7-3 pred table ever lab (02-18-2025).RData")
pred <- rep(0, nrow(pred_tb_default_interact_weight$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_interact_weight[[i]]$prediction
}
pred_freq_default_interact_weight <- pred_tb_default_interact_weight$result1 %>%
  mutate(prediction=pred,
         accuracy = ifelse(hyd_destination, prediction/20, 1-prediction/20),
         FP = ifelse(hyd_destination, 0, prediction),
         FN = ifelse(hyd_destination, 20-prediction, 0))

pred_tb_default_CM <- lapply(pred_tb_default,
                             function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)),
                                                         x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))
measures <- names(pred_tb_default_CM$result1$byClass)
pred_tb_default_result <- matrix(pred_tb_default_CM$result1$byClass, nrow=1) %>% as_tibble
for (i in 2:20) {
  pred_tb_default_result <- bind_rows(pred_tb_default_result,
                                      matrix(pred_tb_default_CM[[i]]$byClass, nrow=1) %>% as_tibble)
}
names(pred_tb_default_result) <- measures
pred_tb_default_result$run <- 1:20
pred_tb_default_result <- pred_tb_default_result %>% 
  select(run, Sensitivity, Specificity, F1) %>% 
  pivot_longer(-run, names_to="measure")

pred_tb_default_weight_9_1_CM <- lapply(pred_tb_default_weight_9_1,
                                        function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)),
                                                                    x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))
pred_tb_default_weight_9_1_result <- matrix(pred_tb_default_weight_9_1_CM$result1$byClass, nrow=1) %>% as_tibble
for (i in 2:20) {
  pred_tb_default_weight_9_1_result <- bind_rows(pred_tb_default_weight_9_1_result,
                                                 matrix(pred_tb_default_weight_9_1_CM[[i]]$byClass, nrow=1) %>% as_tibble)
}
names(pred_tb_default_weight_9_1_result) <- measures
pred_tb_default_weight_9_1_result$run <- 1:20
pred_tb_default_weight_9_1_result <- pred_tb_default_weight_9_1_result %>% 
  select(run, Sensitivity, Specificity, F1) %>% 
  pivot_longer(-run, names_to="measure")

pred_tb_default_weight_7_3_CM <- lapply(pred_tb_default_weight_7_3,
                                        function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)),
                                                                    x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))
pred_tb_default_weight_7_3_result <- matrix(pred_tb_default_weight_7_3_CM$result1$byClass, nrow=1) %>% as_tibble
for (i in 2:20) {
  pred_tb_default_weight_7_3_result <- bind_rows(pred_tb_default_weight_7_3_result,
                                                 matrix(pred_tb_default_weight_7_3_CM[[i]]$byClass, nrow=1) %>% as_tibble)
}
names(pred_tb_default_weight_7_3_result) <- measures
pred_tb_default_weight_7_3_result$run <- 1:20
pred_tb_default_weight_7_3_result <- pred_tb_default_weight_7_3_result %>% 
  select(run, Sensitivity, Specificity, F1) %>% 
  pivot_longer(-run, names_to="measure")

pred_tb_default_interact_CM <- lapply(pred_tb_default_interact,
                                      function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)),
                                                                  x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))
pred_tb_default_interact_result <- matrix(pred_tb_default_interact_CM$result1$byClass, nrow=1) %>% as_tibble
for (i in 2:20) {
  pred_tb_default_interact_result <- bind_rows(pred_tb_default_interact_result,
                                               matrix(pred_tb_default_interact_CM[[i]]$byClass, nrow=1) %>% as_tibble)
}
names(pred_tb_default_interact_result) <- measures
pred_tb_default_interact_result$run <- 1:20
pred_tb_default_interact_result <- pred_tb_default_interact_result %>% 
  select(run, Sensitivity, Specificity, F1) %>% 
  pivot_longer(-run, names_to="measure")

pred_tb_default_interact_weight_CM <- lapply(pred_tb_default_interact_weight,
                                             function(x) confusionMatrix(x$prediction %>% factor(levels=c(0,1)),
                                                                         x$hyd_destination %>% factor(levels=c(0,1)), positive="1"))
pred_tb_default_interact_weight_result <- matrix(pred_tb_default_interact_weight_CM$result1$byClass, nrow=1) %>% as_tibble
for (i in 2:20) {
  pred_tb_default_interact_weight_result <- bind_rows(pred_tb_default_interact_weight_result,
                                                      matrix(pred_tb_default_interact_weight_CM[[i]]$byClass, nrow=1) %>% as_tibble)
}
names(pred_tb_default_interact_weight_result) <- measures
pred_tb_default_interact_weight_result$run <- 1:20
pred_tb_default_interact_weight_result <- pred_tb_default_interact_weight_result %>% 
  select(run, Sensitivity, Specificity, F1) %>% 
  pivot_longer(-run, names_to="measure")

pred_tb_default_result %>% ggplot() +
  geom_line(aes(x=run, y=value, group=measure, color=measure)) +
  ylim(0, 1) + ggtitle("no interaction and wieght")

pred_tb_default_weight_9_1_result %>% ggplot() +
  geom_line(aes(x=run, y=value, group=measure, color=measure)) +
  ylim(0, 1) + ggtitle("no interaction and 9:1 weight")

pred_tb_default_weight_7_3_result %>% ggplot() +
  geom_line(aes(x=run, y=value, group=measure, color=measure)) +
  ylim(0, 1) + ggtitle("no interaction and 7:3 weight")

pred_tb_default_interact_result %>% ggplot() +
  geom_line(aes(x=run, y=value, group=measure, color=measure)) +
  ylim(0, 1) + ggtitle("interaction and no weight")

pred_tb_default_interact_weight_result %>% ggplot() +
  geom_line(aes(x=run, y=value, group=measure, color=measure)) +
  ylim(0, 1) + ggtitle("interaction and 7:3 weight")

# how many times was lasso model correct?

left_join(map_df, pred_freq_default, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=accuracy),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="Accuracy (deviance, no interaction, normalized, equal weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_acc_plot

left_join(map_df, pred_freq_default_weight_0.9_0.1, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=accuracy),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="Accuracy (deviance, no interaction, normalized, 0.9/0.1 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_acc_plot

left_join(map_df, pred_freq_default_weight_0.7_0.3, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=accuracy),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="Accuracy (deviance, no interaction, normalized, 0.7/0.3 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_0.7_0.3_acc_plot

left_join(map_df, pred_freq_default_interact, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=accuracy),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="Accuracy (deviance, with interaction, normalized, equal weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_acc_plot

left_join(map_df, pred_freq_default_interact_weight, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=accuracy),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="Accuracy (deviance, with interaction, normalized, 0.7/0.3 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_weigh_acc_plot

# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default accuracy map ever lab (02-12-2025).png"),
#        pred_freq_default_acc_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 9-1 accuracy map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_acc_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 7-3 accuracy map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_0.7_0.3_acc_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact accuracy map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_acc_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact-weight 7-3 accuracy map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_weigh_acc_plot, scale=1)
  
# map of # of positively predicted cases
left_join(map_df, pred_freq_default, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=prediction),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="deviance, no interaction, normalized, equal weight") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_plot

left_join(map_df, pred_freq_default_weight_0.9_0.1, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=prediction),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="deviance, no interaction, normalized, 0.9/0.1 weight") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_plot

left_join(map_df, pred_freq_default_weight_0.7_0.3, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=prediction),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="deviance, no interaction, normalized, 0.7/0.3 weight") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_0.7_0.3_plot

left_join(map_df, pred_freq_default_interact, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=prediction),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="deviance, with interaction, normalized, equal weight") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_plot

left_join(map_df, pred_freq_default_interact_weight, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=prediction),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="deviance, with interaction, normalized, 0.7/0.3 weight") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_weight_plot

# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default pred map ever lab (02-12-2025).png"),
#        pred_freq_default_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 9-1 pred map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 7-3 pred map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_0.7_0.3_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact pred map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact-weight 7-3 pred map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_weight_plot, scale=1)

left_join(map_df, pred_freq_default %>% mutate(hyd_destination = as.factor(hyd_destination)), by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=hyd_destination),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination ever existed") +
  scale_fill_viridis_d(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_destination_plot_year

ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso hyd_destination map.png"),
       hyd_destination_plot_year, scale=1)

# map of False Positive cases
left_join(map_df, pred_freq_default, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FP),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FP (deviance, no interaction, normalized, equal weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_FP_plot

left_join(map_df, pred_freq_default_weight_0.9_0.1, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FP),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FP (deviance, no interaction, normalized, 0.9/0.1 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_FP_plot

left_join(map_df, pred_freq_default_weight_0.7_0.3, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FP),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FP (deviance, no interaction, normalized, 0.7/0.3 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_0.7_0.3_FP_plot

left_join(map_df, pred_freq_default_interact, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FP),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FP (deviance, with interaction, normalized, equal weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_FP_plot

left_join(map_df, pred_freq_default_interact_weight, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FP),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FP (deviance, with interaction, normalized, 0.7/0.3 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_weigh_FP_plot

# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default FP map ever lab (02-12-2025).png"),
#        pred_freq_default_FP_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 9-1 FP map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_FP_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 7-3 FP map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_0.7_0.3_FP_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact FP map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_FP_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact-weight 7-3 FP map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_weigh_FP_plot, scale=1)


# map of False Negative cases
left_join(map_df, pred_freq_default, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FN),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FN (deviance, no interaction, normalized, equal weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_FN_plot

left_join(map_df, pred_freq_default_weight_0.9_0.1, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FN),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FN (deviance, no interaction, normalized, 0.9/0.1 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_FN_plot

left_join(map_df, pred_freq_default_weight_0.7_0.3, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FN),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FN (deviance, no interaction, normalized, 0.7/0.3 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_weight_0.7_0.3_FN_plot

left_join(map_df, pred_freq_default_interact, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FN),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FN (deviance, with interaction, normalized, equal weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_FN_plot

left_join(map_df, pred_freq_default_interact_weight, by="id") %>% 
  ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=FN),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="FN (deviance, with interaction, normalized, 0.7/0.3 weight)") +
  scale_fill_viridis_c(na.value = "white") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> pred_freq_default_interact_weigh_FN_plot

# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default FN map ever lab (02-12-2025).png"),
#        pred_freq_default_FN_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 9-1 FN map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_FN_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-weight 7-3 FN map ever lab (02-12-2025).png"),
#        pred_freq_default_weight_0.7_0.3_FN_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact FN map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_FN_plot, scale=1)
# ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso experiments/ever lab with aggregated data/local GWR lasso default-interact-weight 7-3 FN map ever lab (02-12-2025).png"),
#        pred_freq_default_interact_weigh_FN_plot, scale=1)


# check high FP/FN regions
pred_freq_default %>% filter(FP > 0)
pred_freq_default %>% filter(hyd_destination == 0 & FP > 0) # 223
pred_freq_default %>% filter(FN > 0)
pred_freq_default %>% filter(hyd_destination == 1 & FN > 0) # 88

gwr_data_with_result <- gwr_data_hyd_destination_by_area %>% filter(id %in% pred_freq_default$id)
gwr_data_with_result_PCR <- prcomp(gwr_data_with_result[,-(1:3)]) 

gwr_data_with_result_PCR
gwr_data_with_result_PC_scores <- gwr_data_with_result_PCR$x %>% as_tibble %>%
  mutate(id = gwr_data_with_result$id) %>% 
  left_join(pred_freq_default, by="id")

gwr_data_with_result_PC_scores %>% 
  filter(hyd_destination == 0) %>% 
  ggplot() +
  geom_point(aes(x=PC1, y=PC2, group=FP, fill=FP), shape=21, size=2, color="black") +
  scale_fill_viridis_c(option="D") +
  ggtitle("Plot of PC1 vs. PC2 (False Positive)")

gwr_data_with_result_PC_scores %>% 
  filter(hyd_destination == 1) %>% 
  ggplot() +
  geom_point(aes(x=PC1, y=PC2, group=FN, fill=FN), shape=21, size=2, color="black") +
  scale_fill_viridis_c(option="D") +
  ggtitle("Plot of PC1 vs. PC2 (False Negative")
