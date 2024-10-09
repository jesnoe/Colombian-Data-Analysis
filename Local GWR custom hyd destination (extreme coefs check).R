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
library(spdep)
library(caret)
library(randomForest)
library(pracma)
library(GWmodel)
library(ROCR)
library(pls)
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
local_gwr_data <- gwr_hyd_destination_coord %>% select(-id, -municipio, -year, -long, -lat)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)

n_reg_data_mat <- read.csv("Colombia Data/local GWR number of neighbors (08-13-2024).csv") %>% as_tibble
aic_score_mat <- read.csv("Colombia Data/local GWR AIC (08-13-2024).csv") %>% as_tibble
y_ratio_mat <- read.csv("Colombia Data/local GWR response ratio (09-12-2024).csv") %>% as_tibble
local_GWR_coefs_bw <- read.csv("Colombia Data/local GWR best coefs (08-13-2024).csv") %>% as_tibble
local_GWR_no_overfitting_coefs_bw <- read.csv("Colombia Data/local GWR no overfitting best coefs (10-01-2024).csv") %>% as_tibble
local_GWR_no_overfitting_pvalues_bw <- read.csv("Colombia Data/local GWR no overfitting best p-values (10-01-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result (08-13-2024).RData")

y_ratio_mat %>% filter(id == 41078)

## why longer bandwidth caused overfitting
bw_comparison <- left_join(local_GWR_coefs_bw %>% select(id, bw, n_neighbors),
                           local_GWR_no_overfitting_coefs_bw %>% select(id, bw, n_neighbors) %>% rename(bw_no_overfit=bw, n_neighbors_no_overfit=n_neighbors),
                           by="id") %>% 
  filter(bw != bw_no_overfit) # In all cases, bw < bw_no_overfit
longer_bw_id <- bw_comparison %>% filter(bw < bw_no_overfit) %>% pull(id)
bw_ratio <- c()
bw_no_overfit_ratio <- c()
for (i in 1:nrow(bw_comparison)) {
  bw_ratio <- c(bw_ratio, y_ratio_mat[[paste0("bw_", bw_comparison$bw[i])]][i])
  bw_no_overfit_ratio <- c(bw_no_overfit_ratio, y_ratio_mat[[paste0("bw_", bw_comparison$bw_no_overfit[i])]][i])
}
bw_comparison$bw_ratio <- bw_ratio
bw_comparison$bw_no_overfit_ratio <- bw_no_overfit_ratio
rm(bw_ratio)
rm(bw_no_overfit_ratio)
bw_comparison$bw_ratio_diff <- bw_comparison$bw_no_overfit_ratio - bw_comparison$bw_ratio
bw_comparison$bw_ratio_change_to_0.5 <- ifelse(bw_comparison$bw_ratio > 0.5, bw_comparison$bw_ratio_diff, -bw_comparison$bw_ratio_diff)
bw_comparison$n_neighbors_diff <- bw_comparison$n_neighbors_no_overfit - bw_comparison$n_neighbors
bw_comparison %>% relocate(id, bw, bw_no_overfit, n_neighbors, n_neighbors_no_overfit) %>% view

local_GWR_no_overfitting_y_ratio <- c()
for (i in 1:nrow(local_GWR_no_overfitting_pvalues_bw)) {
  id_i <- local_GWR_no_overfitting_pvalues_bw$id[i]
  bw_i <- local_GWR_no_overfitting_pvalues_bw$bw[i]
  y_ratio_bw <- y_ratio_mat %>% filter(id == id_i)
  local_GWR_no_overfitting_y_ratio <- c(local_GWR_no_overfitting_y_ratio, y_ratio_bw[[paste0("bw_", bw_i)]])
}
local_GWR_no_overfitting_pvalues_bw$y_ratio <- local_GWR_no_overfitting_y_ratio
local_GWR_no_overfitting_pvalues_bw <- local_GWR_no_overfitting_pvalues_bw %>% relocate(id, bw, n_neighbors, n_NA, y_ratio)
local_GWR_no_overfitting_pvalues_bw

#### extreme ceof check with forward regression
gwr_result_id_41078_bw_1.1 <- glm(hyd_destination~.,
                                  data=gwr_result_list$id_41078$bw_1.1$data,
                                  family=binomial)
gwr_result_id_41078_bw_1.1_forward <- MASS::stepAIC(gwr_result_id_41078_bw_1.1, trace=F, steps=10000, direction="forward")
gwr_result_id_41078_bw_1.1_step <- MASS::stepAIC(gwr_result_id_41078_bw_1.1, trace=F, steps=10000, direction="both")
summary(gwr_result_id_41078_bw_1.1_forward)
summary(gwr_result_id_41078_bw_1.1_step)

var_names <- names(gwr_result_list$id_41078$bw_1.1$data)[-17]
gwr_result_forward_p_values <- tibble()
base_data <- gwr_result_list$id_41078$bw_1.1$data %>% select(hyd_destination)
for(i in 1:length(var_names)) {
  var_name <- var_names[i]
  gwr_result_1_var <- glm(hyd_destination~.,
                          data=cbind(base_data, gwr_result_list$id_41078$bw_1.1$data[,i]),
                          family=binomial)
  gwr_result_1_var_summary <- summary(gwr_result_1_var)
  if (is.na(coef(gwr_result_1_var)[2])) {
    gwr_result_forward_p_values <- rbind(gwr_result_forward_p_values, c(var_name, NA, NA))
    next
  }
  gwr_result_forward_p_values <- rbind(gwr_result_forward_p_values, c(var_name, gwr_result_1_var_summary$coefficients[2,c(1,4)])) %>% as_tibble
}
names(gwr_result_forward_p_values) <- c("var_name", "coefficient", "p_value")
gwr_result_forward_p_values <- gwr_result_forward_p_values %>% mutate(coefficient=as.numeric(coefficient), p_value=as.numeric(p_value))

id_41078_data_sig_vars <- gwr_result_list$id_41078$bw_1.1$data[,c(which(gwr_result_forward_p_values$p_value <= 0.05), 17)]
gwr_result_sig_vars <- glm(hyd_destination~.,
                           data=id_41078_data_sig_vars,
                           family=binomial)
summary(gwr_result_sig_vars)
gwr_result_sig_vars_forward <- MASS::stepAIC(gwr_result_sig_vars, trace=F, steps=10000, direction="forward")
gwr_result_sig_vars_step <- MASS::stepAIC(gwr_result_sig_vars, trace=F, steps=10000, direction="both")
summary(gwr_result_sig_vars_step)

glm(hyd_destination~., data=id_41078_data_sig_vars %>% 
      select(hyd_destination, n_PPI_labs, hyd_lab_prob, hyd_price_distance, river_length, road_length, airport),
    family=binomial) %>% summary
names(id_41078_data_sig_vars)

#### LASSO regression
lambda_fit_auc <- cv.glmnet(x = gwr_result_list$id_41078$bw_1.1$data[,-17] %>% as.matrix,
                            y = gwr_result_list$id_41078$bw_1.1$data$hyd_destination,
                            family = "binomial",
                            type.measure = "auc")
lambda_fit_auc$lambda.min
plot(lambda_fit_auc)

lambda_fit_mse <- cv.glmnet(x = gwr_result_list$id_41078$bw_1.1$data[,-17] %>% as.matrix,
                            y = gwr_result_list$id_41078$bw_1.1$data$hyd_destination,
                            family = "binomial",
                            type.measure = "mse")
lambda_fit_mse$lambda.min
plot(lambda_fit_mse)

id_41078_data_lasso <- glmnet(x = gwr_result_list$id_41078$bw_1.1$data %>% select(-hyd_destination) %>% as.matrix,
                              y = gwr_result_list$id_41078$bw_1.1$data$hyd_destination,
                              family = "binomial",
                              alpha = 1,
                              type.measure = "auc")
plot(id_41078_data_lasso)
id_41078_data_lasso$beta[,100] %>% sort %>% as.matrix(ncol=1)
id_41078_data_lasso_coefs <- id_41078_data_lasso$beta %>% t %>% as.matrix %>% as_tibble
id_41078_data_lasso_coefs <- id_41078_data_lasso_coefs %>% 
  mutate(lambda=id_41078_data_lasso$lambda[100:1]) %>% 
  gather(key = "variable", value = "value", -lambda)
id_41078_data_lasso_coefs %>% 
  ggplot(aes(x=lambda, y=value)) +
  geom_line(aes(color=variable)) -> lasso_coef_plot
id_41078_data_lasso_coefs %>% 
  ggplot(aes(x=log(lambda), y=value)) +
  geom_line(aes(color=variable)) -> lasso_coef_plot_log_lambda
# ggsave("Colombia Data/Figs/hyd destintion local GWR LASSO coefs plot.png", lasso_coef_plot, scale=1)
# ggsave("Colombia Data/Figs/hyd destintion local GWR LASSO coefs plot (log lambda).png", lasso_coef_plot_log_lambda, scale=1)

id_41078_data_lasso_coefs <- coef(id_41078_data_lasso, s=lambda_fit$lambda.min)
X_beta <- id_41078_data_lasso_coefs[1] + (gwr_result_list$id_41078$bw_1.1$data[,-17] %>% as.matrix) %*% id_41078_data_lasso_coefs[-1]
pi_hat <- exp(X_beta)/(1+exp(X_beta))
pi_hat <- ifelse(is.nan(pi_hat), 1, pi_hat)

lasso_result <- tibble(pi_hat=pi_hat, y=gwr_result_list$id_41078$bw_1.1$data$hyd_destination)
lasso_pred <- prediction(lasso_result$pi_hat, lasso_result$y)
roc <- performance(lasso_pred, "tpr", "fpr")
plot(roc, lwd=2, colorize=F)
auc <- performance(lasso_pred, "auc")
auc@y.values

#### extreme ceof check
local_GWR_coefs_bw$coefs_sum <- apply(local_GWR_coefs_bw, 1, function(x) sum(abs(x[-(1:5)]), na.rm=T))
local_GWR_coefs_bw <- local_GWR_coefs_bw %>% 
  relocate(id, bw, n_neighbors, n_NA, coefs_sum)
local_GWR_coefs_bw %>% select(id:coefs_sum) %>% arrange(desc(coefs_sum)) %>% view
summary(local_GWR_coefs_bw$coefs_sum)

y_ratio_mat

aic_score_mat %>% filter(id == 41078)
tibble(bw=seq(1, 2, by=0.1), AIC=(aic_score_mat %>% filter(id == 41078))[, 7:17] %>% t %>% as.vector) %>% 
  ggplot() + geom_line(aes(x=bw, y=AIC))
aic_score_mat %>% filter(id == 85225) %>% view

gwr_result_list$id_41078$bw_1.1 %>% summary
gwr_result_list$id_41078$bw_1.2 %>% summary
gwr_result_list$id_41078$bw_1.3 %>% summary
gwr_result_list$id_41078$bw_1.4 %>% summary

gwr_result_list$id_41078$bw_1.1$data$hyd_destination %>% table # 0:  18, 1: 126
gwr_result_list$id_41078$bw_1.2$data$hyd_destination %>% table # 0:  36, 1: 141
gwr_result_list$id_41078$bw_1.3$data$hyd_destination %>% table # 0:  72, 1: 262
gwr_result_list$id_41078$bw_1.4$data$hyd_destination %>% table # 0: 108, 1: 198

gwr_result_id_41078_bw_1.1 <- glm(hyd_destination~.,
                                  data=gwr_result_list$id_41078$bw_1.1$data,
                                  family=binomial)
gwr_result_id_41078_bw_1.1_step <- MASS::stepAIC(gwr_result_id_41078_bw_1.1, trace=F, steps=10000, direction="both")
summary(gwr_result_id_41078_bw_1.1_step)
data.frame(gwr_result_id_41078_bw_1.1_step$fitted.values, # overfitting even with variable selection
           gwr_result_id_41078_bw_1.1_step$y) %>% arrange(gwr_result_id_41078_bw_1.1_step$fitted.values)

gwr_cooks_id_41078_bw_1.1 <- tibble(cooks=cooks.distance(gwr_result_list$id_41078$bw_1.1),
                                    n=1:nrow(gwr_result_list$id_41078$bw_1.1$data))
gwr_cooks_id_41078_bw_1.2 <- tibble(cooks=cooks.distance(gwr_result_list$id_41078$bw_1.2),
                                    n=1:nrow(gwr_result_list$id_41078$bw_1.2$data))
gwr_cooks_id_41078_bw_1.3 <- tibble(cooks=cooks.distance(gwr_result_list$id_41078$bw_1.3),
                                    n=1:nrow(gwr_result_list$id_41078$bw_1.3$data))
gwr_cooks_id_41078_bw_1.4 <- tibble(cooks=cooks.distance(gwr_result_list$id_41078$bw_1.4),
                                    n=1:nrow(gwr_result_list$id_41078$bw_1.4$data))
p1 <- gwr_cooks_id_41078_bw_1.1 %>% ggplot() + geom_point(aes(x=n, y=cooks)) + ggtitle("bw=1.1")
p2 <- gwr_cooks_id_41078_bw_1.2 %>% ggplot() + geom_point(aes(x=n, y=cooks)) + ggtitle("bw=1.2")
p3 <- gwr_cooks_id_41078_bw_1.3 %>% ggplot() + geom_point(aes(x=n, y=cooks)) + ggtitle("bw=1.3")
p4 <- gwr_cooks_id_41078_bw_1.4 %>% ggplot() + geom_point(aes(x=n, y=cooks)) + ggtitle("bw=1.4")
grid.arrange(p1, p2, p3, p4, ncol=2)

local_gwr_data_id_41078_bw_1.1 <- ever_anecdotal_data_years %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[which(coord_unique$id == 41078),] <= 1.1)])
local_gwr_data_id_41078_bw_1.2 <- ever_anecdotal_data_years %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[which(coord_unique$id == 41078),] <= 1.2)])
id_41078_bw_1.1_outliers <- which(gwr_cooks_id_41078_bw_1.1$cooks > 1e+10)
id_41078_bw_1.2_outliers <- which(gwr_cooks_id_41078_bw_1.2$cooks > 1e+10)

local_gwr_data_id_41078_bw_1.1[id_41078_bw_1.1_outliers,] %>% view
local_gwr_data_id_41078_bw_1.2[id_41078_bw_1.2_outliers,] %>% arrange(id) %>% view

rbind(local_gwr_data_id_41078_bw_1.1[id_41078_bw_1.1_outliers,],
      c(0,0, apply(local_gwr_data_id_41078_bw_1.1 %>% select(-id, -year), 2, mean)),
      c(0,0, apply(local_gwr_data_id_41078_bw_1.1 %>% select(-id, -year), 2, median))) -> id_41078_bw_1.1_outliers_df
rbind(local_gwr_data_id_41078_bw_1.2[id_41078_bw_1.2_outliers,],
      c(0,0, apply(local_gwr_data_id_41078_bw_1.2 %>% select(-id, -year), 2, mean)),
      c(0,0, apply(local_gwr_data_id_41078_bw_1.2 %>% select(-id, -year), 2, median))) -> id_41078_bw_1.2_outliers_df
id_41078_bw_1.1_outliers_df
id_41078_bw_1.2_outliers_df

  # regression with log vars
local_gwr_data_id_41078_bw_1.1 %>% 
  mutate()

# id=52227 check
aic_score_mat %>% filter(id == 52227) %>% view
gwr_result_list$id_52227$bw_1.1 %>% summary
gwr_result_list$id_52227$bw_1.2 %>% summary
gwr_result_list$id_52227$bw_1.3 %>% summary
gwr_result_list$id_52227$bw_1.4 %>% summary
gwr_result_list$id_52227$bw_1.5 %>% summary

gwr_cooks_id_52227_bw_1.1 <- tibble(cooks=cooks.distance(gwr_result_list$id_52227$bw_1.1),
                                    n=1:nrow(gwr_result_list$id_52227$bw_1.1$data))
gwr_cooks_id_52227_bw_1.2 <- tibble(cooks=cooks.distance(gwr_result_list$id_52227$bw_1.2),
                                    n=1:nrow(gwr_result_list$id_52227$bw_1.2$data))
gwr_cooks_id_52227_bw_1.3 <- tibble(cooks=cooks.distance(gwr_result_list$id_52227$bw_1.3),
                                    n=1:nrow(gwr_result_list$id_52227$bw_1.3$data))
gwr_cooks_id_52227_bw_1.4 <- tibble(cooks=cooks.distance(gwr_result_list$id_52227$bw_1.4),
                                    n=1:nrow(gwr_result_list$id_52227$bw_1.4$data))
gwr_cooks_id_52227_bw_1.5 <- tibble(cooks=cooks.distance(gwr_result_list$id_52227$bw_1.5),
                                    n=1:nrow(gwr_result_list$id_52227$bw_1.5$data))
gwr_cooks_id_52227_bw_1.1 %>% ggplot() + geom_point(aes(x=n, y=cooks))
gwr_cooks_id_52227_bw_1.2 %>% ggplot() + geom_point(aes(x=n, y=cooks))
gwr_cooks_id_52227_bw_1.3 %>% ggplot() + geom_point(aes(x=n, y=cooks))
gwr_cooks_id_52227_bw_1.4 %>% ggplot() + geom_point(aes(x=n, y=cooks))
gwr_cooks_id_52227_bw_1.5 %>% ggplot() + geom_point(aes(x=n, y=cooks))


local_gwr_data_id_52227_bw_1.1 <- ever_anecdotal_data_years %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[which(coord_unique$id == 52227),] <= 1.1)])
local_gwr_data_id_52227_bw_1.2 <- ever_anecdotal_data_years %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[which(coord_unique$id == 52227),] <= 1.2)])
local_gwr_data_id_52227_bw_1.3 <- ever_anecdotal_data_years %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[which(coord_unique$id == 52227),] <= 1.3)])
id_52227_bw_1.1_outliers <- which(gwr_cooks_id_52227_bw_1.1$cooks > 1e+10)
id_52227_bw_1.2_outliers <- which(gwr_cooks_id_52227_bw_1.2$cooks > 1e+5)
id_52227_bw_1.3_outliers <- which(gwr_cooks_id_52227_bw_1.3$cooks > 1e+10)

local_gwr_data_id_52227_bw_1.1[id_52227_bw_1.1_outliers,] %>% arrange(id) %>% view
local_gwr_data_id_52227_bw_1.2[id_52227_bw_1.2_outliers,] %>% view
local_gwr_data_id_52227_bw_1.3[id_52227_bw_1.3_outliers,] %>% arrange(id) %>% view

rbind(local_gwr_data_id_52227_bw_1.1[id_52227_bw_1.1_outliers,],
      c(0,0, apply(local_gwr_data_id_52227_bw_1.1 %>% select(-id, -year), 2, mean)),
      c(0,0, apply(local_gwr_data_id_52227_bw_1.1 %>% select(-id, -year), 2, median))) -> id_52227_bw_1.1_outliers_df
rbind(local_gwr_data_id_52227_bw_1.2[id_52227_bw_1.2_outliers,],
      c(0,0, apply(local_gwr_data_id_52227_bw_1.2 %>% select(-id, -year), 2, mean)),
      c(0,0, apply(local_gwr_data_id_52227_bw_1.2 %>% select(-id, -year), 2, median))) -> id_52227_bw_1.2_outliers_df
rbind(local_gwr_data_id_52227_bw_1.3[id_52227_bw_1.3_outliers,],
      c(0,0, apply(local_gwr_data_id_52227_bw_1.3 %>% select(-id, -year), 2, mean)),
      c(0,0, apply(local_gwr_data_id_52227_bw_1.3 %>% select(-id, -year), 2, median))) -> id_52227_bw_1.3_outliers_df
id_52227_bw_1.1_outliers_df
id_52227_bw_1.2_outliers_df
id_52227_bw_1.3_outliers_df


gwr_result_list$id_52227$bw_1.1$data$hyd_destination %>% table # 0:  6, 1: 177
gwr_result_list$id_52227$bw_1.2$data$hyd_destination %>% table # 0:  9, 1: 189
gwr_result_list$id_52227$bw_1.3$data$hyd_destination %>% table # 0:  9, 1: 201
gwr_result_list$id_52227$bw_1.4$data$hyd_destination %>% table # 0: 18, 1: 210
gwr_result_list$id_52227$bw_1.5$data$hyd_destination %>% table # 0: 27, 1: 219
gwr_result_list$id_52227$bw_1.6$data$hyd_destination %>% table # 0: 39, 1: 222

gwr_result_id_52227_bw_1.1 <- glm(hyd_destination~.,
                                  data=gwr_result_list$id_52227$bw_1.1$data,
                                  family=binomial)
gwr_result_id_52227_bw_1.2 <- glm(hyd_destination~.,
                                  data=gwr_result_list$id_52227$bw_1.2$data,
                                  family=binomial)
gwr_result_id_52227_bw_1.3 <- glm(hyd_destination~.,
                                  data=gwr_result_list$id_52227$bw_1.3$data,
                                  family=binomial)
gwr_result_id_52227_bw_1.4 <- glm(hyd_destination~,
                                  data=gwr_result_list$id_52227$bw_1.4$data,
                                  family=binomial)

MASS::stepAIC(gwr_result_id_52227_bw_1.1, trace=F, direction="both")
MASS::stepAIC(gwr_result_id_52227_bw_1.2, trace=F, direction="both")
MASS::stepAIC(gwr_result_id_52227_bw_1.3, trace=F, direction="both")
MASS::stepAIC(gwr_result_id_52227_bw_1.4, trace=F, direction="both")


## GWR with normalized/logged data
gwr_result_list$id_41078$bw_1.1 %>% summary
gwr_result_list$id_41078$bw_1.1$data %>% summary
log_gwr_result_list <- glm(hyd_destination~., data=gwr_result_list$id_41078$bw_1.1$data %>% 
                             mutate(coca_area=log(1+coca_area),
                                    hyd_seizures=log(1+hyd_seizures),
                                    population=log(population)),
                           family=binomial)
summary(log_gwr_result_list)
data.frame(log_gwr_result_list$fitted.values, log_gwr_result_list$data$hyd_destination) # overfitting
gwr_result_list$id_41078$bw_1.1$data %>% filter(hyd_destination == 1) %>% select(PPI_lab_prob, hyd_lab_prob, coca_area, coca_distance) %>% as.data.frame
gwr_result_list$id_41078$bw_1.1$data %>% filter(hyd_destination == 0) %>% select(PPI_lab_prob, hyd_lab_prob, coca_area, coca_distance) %>% as.data.frame

  # try RF, nonlinear models?
set.seed(2173)
rf_id_41078 <- randomForest(hyd_destination~.,
                            data=gwr_result_list$id_41078$bw_1.1$data %>%
                              mutate(hyd_destination=as.factor(hyd_destination)),
                            type="classification", ntree=50, mtry=8)
rf_id_41078
gwr_result_list$id_41078$bw_1.1$data[rf_id_41078$predicted != rf_id_41078$y,]
gwr_result_list$id_41078$bw_1.1$data$hyd_destination %>% table

## PC Regression (https://www.r-bloggers.com/2016/07/performing-principal-components-regression-pcr-in-r/)
set.seed(100)
pcr_id_41078 <- pcr(hyd_destination~., data=gwr_result_list$id_41078$bw_1.1$data, validation = "CV")
summary(pcr_id_41078)
pcr_id_41078$loadings
pcr_id_41078$Xvar
pcr_id_41078$Yloadings
pcr_id_41078$fitted.values

predplot(pcr_id_41078)
coef(pcr_id_41078, comps=1:2)
fitted(pcr_id_41078, comps=1)
coefplot(pcr_id_41078)

## Alternative to Moran's I: local distances & standard deviations of each variable
local_med_dist <- c()
local_avg_dist <- c()
local_dist_list <- list()
local_GWR_var_sd <- local_GWR_coefs_bw
var1_index <- which(names(local_GWR_var_sd) == "n_PPI_labs")
var_end_index <- which(names(local_GWR_var_sd) == "airport")
for (i in 1:nrow(local_GWR_coefs_bw)) {
  id_i <- local_GWR_coefs_bw$id[i]
  bw_i <- local_GWR_coefs_bw$bw[i]
  gwr_result_data_i <- gwr_result_list[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]$data
  local_GWR_var_sd[i, var1_index:var_end_index] <- apply(gwr_result_data_i %>% select(-hyd_destination), 2, sd) %>% matrix(nrow=1)
  muni_i_dist <- dist(gwr_result_data_i) %>% as.vector
  muni_i_dist_med <- median(muni_i_dist)
  muni_i_dist_avg <- mean(muni_i_dist)
  local_dist_list[[paste0("id_", id_i)]] <- muni_i_dist
  local_med_dist <- c(local_med_dist, muni_i_dist_med)
  local_avg_dist <- c(local_avg_dist, muni_i_dist_avg)
}
local_GWR_coefs_bw$med_dist <- local_med_dist
local_GWR_coefs_bw$avg_dist <- local_avg_dist
local_GWR_coefs_bw <- local_GWR_coefs_bw %>% relocate(id:coefs_sum, med_dist, avg_dist)
local_GWR_var_sd$med_dist <- local_med_dist
local_GWR_var_sd$avg_dist <- local_avg_dist
local_GWR_var_sd <- local_GWR_var_sd %>% relocate(id:coefs_sum, med_dist, avg_dist) %>% select(-Intercept)

  # global data distances
local_GWR_coefs_bw %>% arrange(desc(coefs_sum)) # overfitting occurs with med_dist/avg_dist lower than global stats
local_GWR_coefs_bw %>% filter(avg_dist > global_avg_dist)
local_GWR_coefs_bw %>% filter(avg_dist <= global_avg_dist)
local_GWR_coefs_bw %>% filter(med_dist > global_med_dist)
local_GWR_coefs_bw %>% filter(med_dist <= global_med_dist)
upper_tail_dist <- dist(local_gwr_data %>% select(-hyd_destination), diag=F, upper=T)
global_med_dist <- median(upper_tail_dist %>% as.vector) # 11615
global_avg_dist <- mean(upper_tail_dist %>% as.vector)   # 58950.88
upper_tail_dist %>% summary

high_coefs_sum_id <- local_GWR_coefs_bw %>% arrange(desc(coefs_sum)) %>% filter(coefs_sum > 1e5) %>% pull(id)
rbind(local_GWR_var_sd %>% filter(id == 41078) %>% select(id, n_PPI_labs:airport),
      c("global_sd", apply(local_gwr_data %>% select(-hyd_destination), 2, sd)),
      c("avg_local_sd", local_GWR_var_sd %>% select(n_PPI_labs:airport) %>% apply(2, mean)),
      c("med_local_sd", local_GWR_var_sd %>% select(n_PPI_labs:airport) %>% apply(2, median))) %>% view

zero_hyd_destination_id <- y_ratio_mat %>% filter(n_bw_within == 0) %>% pull(id)
rbind(local_GWR_var_sd %>% filter(id %in% zero_hyd_destination_id) %>% select(id, n_PPI_labs:airport),
      c("global_sd", apply(local_gwr_data %>% select(-hyd_destination), 2, sd)),
      c("avg_local_sd", local_GWR_var_sd %>% select(n_PPI_labs:airport) %>% apply(2, mean)),
      c("med_local_sd", local_GWR_var_sd %>% select(n_PPI_labs:airport) %>% apply(2, median))) %>% view

local_dist_list$id_41078 %>% summary



# Incomplete Moran's I 
local_gwr_data_centered <- local_gwr_data %>% select(-hyd_destination) %>% apply(2, function(x) scale(x, scale=F)) %>% as_tibble
z_var <- rbind(local_gwr_data[1,-17], local_gwr_data %>% select(-hyd_destination) %>% apply(2, var))[-1,] * 
for (i in i:nrow(local_GWR_coefs_bw)) {
  id_i <- local_GWR_coefs_bw$id[i]
  bw_i <- local_GWR_coefs_bw$bw[i]
  muni_index_i <- which(local_gwr_data_id$id == id_i)
  muni_data_i <- local_gwr_data[muni_index_i,]
  muni_data_centered_i <- local_gwr_data_centered[muni_index_i,]
  neighbors_id_i <- coord_unique$id[local_gwr_dist[which(coord_unique$id == id_i),] <= bw_i & coord_unique$id != id_i]
  neighbors_index_i <- which(local_gwr_data_id$id %in% neighbors_id_i)
  neighbors_data_i <- local_gwr_data[neighbors_index_i,]
  neighbors_data_centered_i <- local_gwr_data_centered[neighbors_index_i,]
  
  local_moran_i <- muni_data_centered_i[1,]
  z_i <- apply(muni_data_centered_i, 2, mean)
  for (j in 1:ncol(muni_data_centered_i)) {
    local_moran_i[j] <- sum(neighbors_data_centered_i[[j]] * z_i[j]) / z_var[j]
  }
  
}
municipios_i <- municipios[municipios@data$id %in% c(id_i, neighbors_id_i),]
municipios_i$id <- as.numeric(municipios_i$id)
nb_obj_i <- poly2nb(municipios_i)
nb_mat_i <- nb2mat(nb_obj_i)
listw_i$weights

gwr_result_list$id_41078$bw_1.1$data %>% filter(id==41078)
var(scale(gwr_result_list$id_41078$bw_1.1$data$n_PPI_labs, scale=F)[,1])
local_GWR_coefs_bw
