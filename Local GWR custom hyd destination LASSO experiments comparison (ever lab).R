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

## ever lab data result
{
load("Colombia Data/local GWR lasso default pred table ever lab (02-05-2025).RData")
pred_tb_default_lab <- pred_tb_default
pred <- rep(0, nrow(pred_tb_default$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default[[i]]$prediction
}
pred_freq_default_lab <- pred_tb_default$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))

load("Colombia Data/local GWR lasso default-weight pred table ever lab (02-05-2025).RData")
pred_tb_default_weight_lab <- pred_tb_default_weight
pred <- rep(0, nrow(pred_tb_default_weight$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_weight[[i]]$prediction
}
pred_freq_default_weight_lab <- pred_tb_default_weight$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))

load("Colombia Data/local GWR lasso default-weight 7-3 pred table ever lab (02-05-2025).RData")
pred_tb_default_weight_lab <- pred_tb_default_weight_7_3
pred <- rep(0, nrow(pred_tb_default_weight_7_3$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_weight_7_3[[i]]$prediction
}
pred_freq_default_weight_0.7_0.3_lab <- pred_tb_default_weight_7_3$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))

load("Colombia Data/local GWR lasso default-interact pred table ever lab (02-05-2025).RData")
pred_tb_default_interact_lab <- pred_tb_default_interact
pred <- rep(0, nrow(pred_tb_default_interact$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_interact[[i]]$prediction
}
pred_freq_default_interact_lab <- pred_tb_default_interact$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))

load("Colombia Data/local GWR lasso default-interact-weight pred table ever lab (02-05-2025).RData")
pred_tb_default_interact_weight_lab <- pred_tb_default_interact_weight
pred <- rep(0, nrow(pred_tb_default_interact_weight$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_interact_weight[[i]]$prediction
}
pred_freq_default_interact_weight_lab <- pred_tb_default_interact_weight$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))
}

pred_tb_default_CM <- lapply(pred_tb_default, function(x) confusionMatrix(x$hyd_destination %>% as.factor, x$prediction %>% as.factor, positive="1"))
pred_tb_default_accuracy <- lapply(pred_tb_default_CM, function(x) x$byClass[1]) %>% unlist
pred_tb_default_F1 <- lapply(pred_tb_default_CM, function(x) x$byClass[7]) %>% unlist

plot(1:20, pred_tb_default_accuracy, type="b", pch=19, xlab="LASSO run", ylab="Accuracy", ylim=c(0, 1))
plot(1:20, pred_tb_default_F1, type="b", pch=19, xlab="LASSO run", ylab="F1 Score", ylim=c(0, 1))

pred_tb_default_weight_7_3_CM <- lapply(pred_tb_default_weight_7_3, 
                                        function(x) confusionMatrix(x$hyd_destination %>% as.factor, x$prediction %>% as.factor, positive="1"))
pred_tb_default_weight_7_3_accuracy <- lapply(pred_tb_default_weight_7_3_CM, function(x) x$byClass[1]) %>% unlist
pred_tb_default_weight_7_3_F1 <- lapply(pred_tb_default_weight_7_3_CM, function(x) x$byClass[7]) %>% unlist
plot(1:20, pred_tb_default_weight_7_3_accuracy, type="b", pch=19, xlab="LASSO run", ylab="Accuracy", ylim=c(0, 1))
plot(1:20, pred_tb_default_weight_7_3_F1, type="b", pch=19, xlab="LASSO run", ylab="F1 Score", ylim=c(0, 1))

## separate lab data result
load("Colombia Data/local GWR lasso default pred table (01-25-2025).RData")
pred <- rep(0, nrow(pred_tb_default$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default[[i]]$prediction
}
pred_freq_default <- pred_tb_default$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))


load("Colombia Data/local GWR lasso default-weight 7-3 pred table (01-25-2025).RData")
pred <- rep(0, nrow(pred_tb_default_weight$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_weight[[i]]$prediction
}
pred_freq_default_weight_0.7_0.3 <- pred_tb_default_weight$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))

load("Colombia Data/local GWR lasso default-interact pred table (01-25-2025).RData")
pred <- rep(0, nrow(pred_tb_default_interact$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_interact[[i]]$prediction
}
pred_freq_default_interact <- pred_tb_default_interact$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))

load("Colombia Data/local GWR lasso default-interact-weight pred table (01-25-2025).RData")
pred <- rep(0, nrow(pred_tb_default_interact_weight$result1))
for (i in 1:20) {
  pred <- pred + pred_tb_default_interact_weight[[i]]$prediction
}
pred_freq_default_interact_weight <- pred_tb_default_interact_weight$result1 %>% mutate(prediction=pred, year=rep(c(2013, 2014, 2016), times=1086))


## comparison
regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1]) %>% 
  select(-n_armed_groups, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance,
         -coca_seizures, -base_seizures, -base_group, -hyd_group, -erad_aerial, -(base_source_all:hyd_source),
         -general_source, -general_destination, -n_PPI_labs, -n_hyd_labs, -erad_manual)
regression_data_years_lab <- read.csv("Colombia Data/regression data all municipios ever lab (02-05-2025).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1]) %>% 
  select(-n_armed_groups, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance,
       -coca_seizures, -base_seizures, -base_group, -hyd_group, -erad_aerial, -(base_source_all:hyd_source),
       -general_source, -general_destination, -n_PPI_labs, -n_hyd_labs, -erad_manual)
(pred_freq_default_weight_0.7_0.3$prediction - pred_freq_default_weight_0.7_0.3_lab$prediction) %>% table

hist(regression_data_years$hyd_lab_prob - regression_data_years_lab$hyd_lab_prob, main="hyd_lab differences previous vs. ever lab", xlab="Differences")

abs(pred_freq_default$prediction - pred_freq_default_lab$prediction) %>% table # 3258 in total
pred_freq_default[which(abs(pred_freq_default$prediction - pred_freq_default_lab$prediction) > 0),]$prediction %>% table
pred_freq_default_lab[which(abs(pred_freq_default$prediction - pred_freq_default_lab$prediction) > 0),]$prediction %>% table

abs(pred_freq_default_weight_0.7_0.3$prediction - pred_freq_default_weight_0.7_0.3_lab$prediction) %>% table
pred_freq_default_weight_0.7_0.3[which(abs(pred_freq_default_weight_0.7_0.3$prediction - pred_freq_default_weight_0.7_0.3_lab$prediction) > 0),]$prediction %>% table
pred_freq_default_weight_0.7_0.3_lab[which(abs(pred_freq_default_weight_0.7_0.3$prediction - pred_freq_default_weight_0.7_0.3_lab$prediction) > 0),]$prediction %>% table

which(abs(pred_freq_default$prediction - pred_freq_default_lab$prediction) > 8) # 1533
pred_freq_default[1533,]

pred_freq_default
pred_freq_default_lab
regression_data_years %>% filter(id == 5002) %>% view
regression_data_years_lab %>% filter(id == 5002) %>% view

regression_data_years %>% filter(id == 5040) %>% view
regression_data_years_lab %>% filter(id == 5040) %>% view

regression_data_years %>% filter(id == 76616) %>% view
regression_data_years_lab %>% filter(id == 76616) %>% view


regression_data_years[which(regression_data_years$hyd_lab_prob != regression_data_years_lab$hyd_lab_prob),] %>% head %>% view
regression_data_years_lab[which(regression_data_years$hyd_lab_prob != regression_data_years_lab$hyd_lab_prob),] %>% head %>% view
