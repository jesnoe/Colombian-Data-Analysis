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
library(ROCR)
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

### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
gwr_hyd_destination_coord <- left_join(regression_data_years %>%
                                         select(-n_PPI_labs, -n_hyd_labs, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance,
                                                 -coca_seizures, -base_seizures, -base_group, -hyd_group, -erad_aerial, -(base_source_all:hyd_source),
                                                -general_source, -general_destination),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)

coord_unique <- gwr_hyd_destination_coord %>% select(id, long, lat) %>% unique
gwr_hyd_destination_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T)
dim(gwr_hyd_destination_dist)
gwr_data_hyd_destination <- gwr_hyd_destination_coord %>% select(-long, -lat, -erad_manual)

bwd_range <- seq(0.5, 3, by=0.1)
local_gwr_data_id <- gwr_hyd_destination_coord %>% select(id)
local_gwr_data <- gwr_hyd_destination_coord %>% select(-id, -municipio, -year, -long, -lat)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)

id_11001_index <- which(coord_unique$id == 11001)
gwr_hyd_destination_coord %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[id_11001_index,] <= 1.1)]) %>% 
  select(-long, -lat) -> neighbors_11001

id_41078_index <- which(coord_unique$id == 41078)
gwr_hyd_destination_coord %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[id_41078_index,] <= 1.1)]) %>% 
  select(-long, -lat) -> neighbors_41078


## id=41078 forward selection (most significant var)
forward <- function(reg_data, start_index) {
  model_index <- start_index-1
  result <- tibble()
  for (i in start_index:ncol(reg_data)) {
    reg_result <- glm(hyd_destination~.,
                      data = reg_data[,c(1:model_index, i)],
                      family = binomial)
    p_val_i <- summary(reg_result)$coefficients[start_index, 4]
    result <- rbind(result, c(names(reg_data)[i], p_val_i))
  }
  names(result) <- c("var", "coef")
  return(result %>% mutate(coef=as.numeric(coef)) %>%  as_tibble)
}
forward_data_1 <- neighbors_41078 %>% select(-(id:year), -erad_manual) %>% relocate(hyd_destination)
forward(forward_data_1, 2) # hyd_avg: 0.00425
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg), 3) # PPI_lab_prob: 0.00861
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg,
                                    PPI_lab_prob), 4) # hyd_price_distance: 0.0142
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg,
                                    PPI_lab_prob,
                                    hyd_price_distance), 5) # population: 0.0987
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg,
                                    PPI_lab_prob,
                                    hyd_price_distance,
                                    population), 6) # hyd_lab_prob: 0.14
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg,
                                    PPI_lab_prob,
                                    hyd_price_distance,
                                    population,
                                    hyd_lab_prob), 7) # coca_distance: 0.0868
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg,
                                    PPI_lab_prob,
                                    hyd_price_distance,
                                    population,
                                    hyd_lab_prob,
                                    coca_distance), 8) # n_armed_group: 0.0799
forward(forward_data_1 %>% relocate(hyd_destination,
                                    hyd_avg,
                                    PPI_lab_prob,
                                    hyd_price_distance,
                                    population,
                                    hyd_lab_prob,
                                    coca_distance,
                                    n_armed_groups), 9) # road_length: 0.336
glm(hyd_destination~.,
    data=forward_data_1 %>% select(hyd_destination,
                                   hyd_avg,
                                   PPI_lab_prob,
                                   hyd_price_distance,
                                   population,
                                   hyd_lab_prob,
                                   coca_distance,
                                   n_armed_groups),
    family=binomial) %>% summary
glm(hyd_destination~.,
    data=neighbors_41078 %>% select(-(id:year)),
    family=binomial) %>% summary


## id=41078 LASSO
x_mat <- neighbors_41078 %>% select(-(id:year), -hyd_destination) %>% as.matrix
y_vec <- neighbors_41078$hyd_destination
set.seed(100)
lambda_fit_auc <- cv.glmnet(x = x_mat,
                            y = y_vec %>% as.factor,
                            family = "binomial",
                            type.measure = "auc")
lambda_fit_auc$lambda.min # the minimum cvm for bw=0.5 is higher (1.072016) than that for bw=1.1 (0.7387358)
lambda_fit_auc$cvm[lambda_fit_auc$index[1]]
plot(lambda_fit_auc)

set.seed(100)
lambda_fit_mse <- cv.glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            type.measure = "mse")
lambda_fit_mse$lambda.min
plot(lambda_fit_mse)

id_41078_data_lasso <- glmnet(x = x_mat,
                              y = y_vec,
                              family = "binomial",
                              alpha = 1,
                              lambda = lambda_fit_auc$lambda.min,
                              type.measure = "auc")

plot(id_41078_data_lasso)
max_n <- ncol(id_41078_data_lasso$beta)
id_41078_data_lasso$beta[,max_n] %>% sort %>% as.matrix(ncol=1)
id_41078_data_lasso_coefs <- id_41078_data_lasso$beta %>% t %>% as.matrix %>% as_tibble
id_41078_data_lasso_coefs <- id_41078_data_lasso_coefs %>% 
  mutate(lambda=id_41078_data_lasso$lambda[max_n:1]) %>% 
  gather(key = "variable", value = "value", -lambda)
id_41078_data_lasso_coefs %>% 
  ggplot(aes(x=lambda, y=value)) +
  geom_line(aes(color=variable))
id_41078_data_lasso_coefs %>% 
  ggplot(aes(x=log(lambda), y=value)) +
  geom_line(aes(color=variable))

X_beta <- id_41078_data_lasso$a0 + as.vector(x_mat %*% as.matrix(id_41078_data_lasso$beta))
pi_hat <- exp(X_beta)/(1+exp(X_beta))
pi_hat <- ifelse(is.nan(pi_hat), 1, pi_hat)
predict(id_41078_data_lasso, x_mat) %>% exp
f1_score <- performance(prediction(pi_hat, y_vec), "f", alpha.values=0.7)
f1_score@y.values


## reg coefs with LASSO (bandwidths are the same with "local_GWR_coefs_bw.csv")


cv.mse <- aic_score_mat
local_GWR_coefs_lasso_result <- list()
set.seed(100)
for (i in 1:nrow(coord_unique)) {
  id_i <- coord_unique$id[i]
  local_GWR_coefs_lasso_result[[paste0("id_", id_i)]] <- list()
  
  for (j in 1:length(bwd_range)) {
    bw_ij <- bwd_range[j]
    neighbors_ij <- gwr_hyd_destination_coord %>% 
      filter(id %in% coord_unique$id[which(local_gwr_dist[i,] <= bw_ij)]) %>% 
      select(-(id:year), -long, -lat)
    
    x_mat <- neighbors_ij %>% select(-hyd_destination) %>% as.matrix
    y_vec <- neighbors_ij$hyd_destination
    n_obs <- nrow(neighbors_ij)
    n0 <- sum(y_vec == 0)
    n1 <- sum(y_vec)
    y_ratio <- n1/n_obs
    
    if (n_obs < 30) {
      cv.mse[[paste0("bw_", bw_ij)]][i] <- NA
      local_GWR_coefs_lasso_result[[i]][[paste0("bw_", bw_ij)]] <- NA
      next
    }
    
    if ((n0 < 3 | n1 < 3) | (y_ratio < 0.1 | y_ratio > 0.9)) {
      cv.mse[[paste0("bw_", bw_ij)]][i] <- NA
      local_GWR_coefs_lasso_result[[i]][[paste0("bw_", bw_ij)]] <- NA
      next
    }
    
    lambda_fit_auc_ij <- cv.glmnet(x = x_mat,
                                   y = y_vec,
                                   family = "binomial",
                                   type.measure = "auc")
    lambda_lasso <- lambda_fit_auc_ij$lambda.min
    cv.mse_ij <- lambda_fit_auc_ij$cvm[lambda_fit_auc_ij$index[1]]
    
    
    lasso_result_ij <- glmnet(x = x_mat,
                              y = y_vec,
                              family = "binomial",
                              alpha = 1,
                              lambda = lambda_lasso)
    
    n_nonzero <- sum(lasso_result_ij$beta != 0)
    if (n_nonzero == 0) {
      cv.mse[[paste0("bw_", bw_ij)]][i] <- NA
      local_GWR_coefs_lasso_result[[i]][[paste0("bw_", bw_ij)]] <- NA
      next
    }
    cv.mse[[paste0("bw_", bw_ij)]][i] <- cv.mse_ij
    local_GWR_coefs_lasso_result[[i]][[paste0("bw_", bw_ij)]] <- lasso_result_ij
  }
  if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
}

local_GWR_coefs_lasso <- tibble()

lasso_coefs <- c(lasso_result_ij$a0, as.vector(lasso_result_ij$beta))
names(lasso_coefs) <- c("intercept", row.names(lasso_result_ij$beta))
X_beta <- lasso_coefs_intercept + as.vector(x_mat %*% as.matrix(lasso_coefs))
pi_hat <- exp(X_beta)/(1+exp(X_beta))
pi_hat <- ifelse(is.nan(pi_hat), 1, pi_hat)
lasso_coefs <- c(lasso_result_ij$a0, as.vector(lasso_result_ij$beta))
    names(lasso_coefs) <- c("intercept", row.names(lasso_result_ij$beta))

## coef maps with LASSO
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

ceof_template <- local_GWR_coefs_bw[1,] %>% select(-(id:n_NA), -n_PPI_labs, -n_hyd_labs, -hyd_group)
local_GWR_coefs_bw_lasso <- local_GWR_coefs_bw %>% select(-n_neighbors, -n_NA, -n_PPI_labs, -n_hyd_labs, -hyd_group)
local_GWR_coefs_bw_lasso_mat <- local_GWR_coefs_bw_lasso[,-(1:2)] %>% as.matrix
set.seed(3451)
for (i in 1:nrow(local_GWR_coefs_bw_lasso)) {
  id_i <- local_GWR_coefs_bw$id[i]
  bw_i <- local_GWR_coefs_bw$bw[i]
  neighbors_i <- gwr_hyd_destination_coord %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[i,] <= bw_i)]) %>% 
    select(-(id:year), -long, -lat)
  
  x_mat <- neighbors_i %>% select(-hyd_destination) %>% as.matrix
  y_vec <- neighbors_i$hyd_destination
  
  n_obs <- nrow(neighbors_i)
  n0 <- sum(y_vec == 0)
  n1 <- sum(y_vec)
  y_ratio <- n1/n_obs
  
  while ((bw_i < 4) & ((n0 < 3 | n1 < 3) | (y_ratio < 0.1 | y_ratio > 0.9))) {
    bw_i <- bw_i + 0.1
    neighbors_i <- gwr_hyd_destination_coord %>% 
      filter(id %in% coord_unique$id[which(local_gwr_dist[i,] <= bw_i)]) %>% 
      select(-(id:year), -long, -lat)
    
    x_mat <- neighbors_i %>% select(-hyd_destination) %>% as.matrix
    y_vec <- neighbors_i$hyd_destination
    
    n_obs <- nrow(neighbors_i)
    n0 <- sum(y_vec == 0)
    n1 <- sum(y_vec)
    y_ratio <- n1/n_obs
  }
  
  if (bw_i >= 4) {
    local_GWR_coefs_bw_lasso_mat[i,] <- NA
    next
  }
  
  lambda_fit_auc_i <- cv.glmnet(x = x_mat,
                                y = y_vec,
                                family = "binomial",
                                type.measure = "auc")
  lambda_lasso <- lambda_fit_auc_i$lambda.min
  
  lasso_result_i <- glmnet(x = x_mat,
                           y = y_vec,
                           family = "binomial",
                           alpha = 1,
                           lambda = lambda_lasso)
  lasso_coef_i <- c(lasso_result_i$a0, as.vector(lasso_result_i$beta))
  local_GWR_coefs_bw_lasso_mat[i,] <- lasso_coef_i
  local_GWR_coefs_bw_lasso$bw[i] <- bw_i
  if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
}
local_GWR_coefs_bw_lasso[, -(1:2)] <- local_GWR_coefs_bw_lasso_mat
# write.csv(local_GWR_coefs_bw_lasso, "Colombia Data/local GWR lasso coefs (10-16-2024).csv")

for (i in 4:length(local_GWR_coefs_bw_lasso)) {
  var_name <- names(local_GWR_coefs_bw_lasso)[i]
  gwr_coefs_i <- data.frame(id=local_GWR_coefs_bw_lasso$id,
                            coef=local_GWR_coefs_bw_lasso[[var_name]],
                            rounded_coef=local_GWR_coefs_bw_lasso[[var_name]] %>% round(2))
  min_coef <- min(gwr_coefs_i$coef, na.rm=T)
  max_coef <- max(gwr_coefs_i$coef, na.rm=T)
  coef_map_coords <- map_df %>% 
    left_join(gwr_coefs_i, by="id")
  
  gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=coef),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map$long, y = depto_map$lat) + 
    coord_quickmap() +
    scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                         values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                         na.value = "white") +
    labs(fill=var_name, x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso/hyd destination GWR lasso coef ",
                var_name, ".png"),
         gwr_coef_map, scale=1)
}
