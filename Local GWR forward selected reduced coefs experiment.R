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
# library(ROCR)
library(glmnet)
library(reshape2)
library(regclass)

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
  
  local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
  local_gwr_forward_pvals <- read.csv("Colombia Data/local GWR best p-values forward selection (10-29-2024).csv") %>% as_tibble
  local_gwr_performance <- read.csv("Colombia Data/local GWR performance forward selection (10-29-2024).csv") %>% as_tibble
  load("Colombia Data/local GWR result forward selection (10-29-2024).RData")
}

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

### armed_group = 1 if there is at lease 1 armed group, and 0 otherwise 
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


bwd_range <- seq(0.5, 4, by=0.1)
local_gwr_data_id <- gwr_data_hyd_destination %>% select(id)
local_gwr_data <- gwr_data_hyd_destination %>% select(-id, -municipio, -year)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)

## id=5353 case study
sig_level <- 0.1
id_i <- 5353
id_i_index <- which(coord_unique$id == id_i)
i <- id_i_index 
bw_i <- local_gwr_forward_coefs$bw[i]
neighbors_i <- gwr_data_hyd_destination %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
  select(-(id:year))

prev_data <- neighbors_i %>% select(hyd_destination)
remaining_data <- neighbors_i %>% select(-hyd_destination)
non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
remaining_data <- remaining_data[,non_sigular_col_index]

id_5353_forward_list <- list()
k <- 1
significance <- 1
while (significance) {
  p_values_i <- tibble()
  for (j in 1:ncol(remaining_data)) {
    new_var_j <- remaining_data[,j]
    reg_data_j <- bind_cols(prev_data, new_var_j)
    reg_model_j <- glm(hyd_destination~.,
                       data = reg_data_j,
                       family = binomial)
    var_name_j <- names(new_var_j)
    reg_model_coefs_j <- summary(reg_model_j)$coefficients
    p_value_j <- reg_model_coefs_j[which(rownames(reg_model_coefs_j) == var_name_j), 4]
    p_value_j <- ifelse(p_value_j == 0, 1, p_value_j)
    p_values_i <- bind_rows(p_values_i, tibble(var_name=var_name_j, p_value=p_value_j))
  }
  if (sum(p_values_i$p_value <= sig_level) > 0) {
    best_col_index <- which.min(p_values_i$p_value)
    prev_data <- bind_cols(prev_data, remaining_data[, best_col_index])
    next_best_reg <- glm(hyd_destination~., data = prev_data, family = binomial)
    id_5353_forward_list[[paste0("model",k)]] <- next_best_reg
    k <- k + 1
    remaining_data <- remaining_data[, -best_col_index]
    next
  }else{
    significance <- 0
  }
}


id_5353_forward_model5_data <- id_5353_forward_list$model5$data
id_5353_forward_list_model5 <- tibble(fitted.values=id_5353_forward_list$model5$fitted.values)
for (k in seq(0.9, 0.5, -0.1)) {
  coefs_k <- k * id_5353_forward_list$model5$coefficients
  id_5353_forward_list$model5[[paste0("coefficients_", k)]] <- coefs_k
  X_beta <- coefs_k[1] + as.matrix(id_5353_forward_model5_data[,-1]) %*% matrix(coefs_k[-1], ncol=1)
  id_5353_forward_list_model5[[paste0("fitted.values_", k)]] <- as.vector(exp(X_beta)/(1+exp(X_beta)))
}

id_5353_forward_list_model5$hyd_destination <- id_5353_forward_model5_data$hyd_destination %>% as.character %>% as.numeric
id_5353_forward_list %>% lapply(summary)

id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=fitted.values, y=fitted.values_0.9)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat 1.0 coefs", y=paste0("pi hat 0.9 coefs"), title="pi hat with 0.9 vs. 1.0 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=fitted.values, y=fitted.values_0.8)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat 1.0 coefs", y=paste0("pi hat 0.8 coefs"), title="pi hat with 0.8 vs. 1.0 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=fitted.values, y=fitted.values_0.7)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat 1.0 coefs", y=paste0("pi hat 0.7 coefs"), title="pi hat with 0.7 vs. 1.0 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=fitted.values, y=fitted.values_0.6)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat 1.0 coefs", y=paste0("pi hat 0.6 coefs"), title="pi hat with 0.6 vs. 1.0 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=fitted.values, y=fitted.values_0.5)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat 1.0 coefs", y=paste0("pi hat 0.5 coefs"), title="pi hat with 0.5 vs. 1.0 coefs")

# reduced coefs' fitted values vs. response
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=1:nrow(id_5353_forward_model5_data), y=abs(fitted.values-hyd_destination))) +
  ylim(c(0,1)) +
  labs(x="index", y=paste0("abs diffs"), title="Differences between response and pi hat with 1.0 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=1:nrow(id_5353_forward_model5_data), y=abs(fitted.values_0.9-hyd_destination))) +
  ylim(c(0,1)) +
  labs(x="index", y=paste0("abs diffs"), title="Differences between response and pi hat with 0.9 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=1:nrow(id_5353_forward_model5_data), y=abs(fitted.values_0.8-hyd_destination))) +
  ylim(c(0,1)) +
  labs(x="index", y=paste0("abs diffs"), title="Differences between response and pi hat with 0.8 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=1:nrow(id_5353_forward_model5_data), y=abs(fitted.values_0.7-hyd_destination))) +
  ylim(c(0,1)) +
  labs(x="index", y=paste0("abs diffs"), title="Differences between response and pi hat with 0.7 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=1:nrow(id_5353_forward_model5_data), y=abs(fitted.values_0.6-hyd_destination))) +
  ylim(c(0,1)) +
  labs(x="index", y=paste0("abs diffs"), title="Differences between response and pi hat with 0.6 coefs")
id_5353_forward_list_model5 %>% ggplot() +
  geom_point(aes(x=1:nrow(id_5353_forward_model5_data), y=abs(fitted.values_0.5-hyd_destination))) +
  ylim(c(0,1)) +
  labs(x="index", y=paste0("abs diffs"), title="Differences between response and pi hat with 0.5 coefs")

id_5353_forward_list_model5_diff <- tibble(hyd_destination=id_5353_forward_list_model5$hyd_destination,
                                           diff_1.0 = abs(id_5353_forward_list_model5$hyd_destination - id_5353_forward_list_model5$fitted.values),
                                           diff_0.9 = abs(id_5353_forward_list_model5$hyd_destination - id_5353_forward_list_model5$fitted.values_0.9))

id_5353_forward_list_model5_diff[-(1:36),] %>% filter(diff_0.9 > diff_1.0) %>% print(n=78)

hurt_index <- which(id_5353_forward_list_model5_diff$diff_0.9 > id_5353_forward_list_model5_diff$diff_1.0)
hurt_index <- hurt_index[!(hurt_index %in% 1:36)]

id_5353_forward_list_model5$y_pred <- ifelse(id_5353_forward_list_model5$fitted.values < 0.5, 0, 1)

confusionMatrix(id_5353_forward_list_model5$hyd_destination[hurt_index] %>% as.factor,
                id_5353_forward_list_model5$y_pred[hurt_index] %>% as.factor)
confusionMatrix(id_5353_forward_list_model5$hyd_destination[-hurt_index] %>% as.factor,
                id_5353_forward_list_model5$y_pred[-hurt_index] %>% as.factor)

id_5353_forward_list_model5_cooks <- data.frame(index=1:nrow(id_5353_forward_list_model5),
                                                cooks=cooks.distance(id_5353_forward_list$model5),
                                                rstudent=rstudent(id_5353_forward_list$model5),
                                                hatval=hatvalues(id_5353_forward_list$model5))
id_5353_forward_list_model5_cooks %>% ggplot() +
  geom_point(aes(x=index, y=cooks, color=index %in% hurt_index))
which(id_5353_forward_list_model5_cooks$cooks > 0.09) # no hurt cases
id_5353_forward_list_model5_cooks %>% ggplot() +
  geom_point(aes(x=index, y=rstudent, color=index %in% hurt_index))
which(abs(id_5353_forward_list_model5_cooks$rstudent) > 1) # includes some hurt cases
id_5353_forward_list_model5_cooks %>% ggplot() +
  geom_point(aes(x=index, y=hatval, color=index %in% hurt_index))

VIF(glm(hyd_destination~., data=id_5353_forward_model5_data[hurt_index,], family=binomial))
VIF(glm(hyd_destination~., data=id_5353_forward_model5_data[-hurt_index,], family=binomial))

id_5353_forward_model5_data[hurt_index,] %>% ggplot() +
  geom_point(aes(x=hyd_lab_prob, y=hyd_destination))

a <- glm(hyd_destination~., data = id_5353_forward_model5_data[hurt_index,], family = binomial) %>% summary
a <- glm(hyd_destination~., data = id_5353_forward_model5_data[-hurt_index,], family = binomial) %>% summary
pi_hat <-a$fitted.values
confusion.matrix(id_5353_forward_model5_data[hurt_index,]$hyd_destination, ifelse(pi_hat < 0.5, 0, 1) %>% as.factor)

corr_model5_hurt_data <- cor(id_5353_forward_model5_data[hurt_index,] %>%
                               mutate(hyd_destination=hyd_destination %>% as.character %>% as.numeric)) %>% abs %>% melt
corr_model5_rest_data <- cor(id_5353_forward_model5_data[-hurt_index,] %>%
                               mutate(hyd_destination=hyd_destination %>% as.character %>% as.numeric)) %>% abs %>% melt
corr_model5_hurt_data$diff <- corr_model5_hurt_data$value - corr_model5_rest_data$value
corr_model5_hurt_data[c(2:6, 9:12, 16:18, 23:24, 30),]

corr_model5_hurt_data %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for hurt data", x="", y="")

corr_model5_rest_data %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for other data", x="", y="")



id_5353_forward_list_model5$hyd_destination %>% table # 1: 30 
id_5353_forward_list_model5 %>% filter(fitted.values_0.9 >= 0.5 & hyd_destination == 1) # 26 rows


abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.9) %>% summary
abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.8) %>% summary
abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.7) %>% summary
abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.6) %>% summary
abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.5) %>% summary

id_5353_forward_list_model5$diff_1_0.9 <- abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.9)
id_5353_forward_list_model5$diff_1_0.8 <- abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.8)
id_5353_forward_list_model5$diff_1_0.7 <- abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.7)
id_5353_forward_list_model5$diff_1_0.6 <- abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.6)
id_5353_forward_list_model5$diff_1_0.5 <- abs(id_5353_forward_list_model5$fitted.values - id_5353_forward_list_model5$fitted.values_0.5)

diff_1_0.9_index <- which(id_5353_forward_list_model5$diff_1_0.9 > quantile(id_5353_forward_list_model5$diff_1_0.9, 0.75))
diff_1_0.8_index <- which(id_5353_forward_list_model5$diff_1_0.8 > quantile(id_5353_forward_list_model5$diff_1_0.8, 0.75))
diff_1_0.7_index <- which(id_5353_forward_list_model5$diff_1_0.7 > quantile(id_5353_forward_list_model5$diff_1_0.7, 0.75))
diff_1_0.6_index <- which(id_5353_forward_list_model5$diff_1_0.6 > quantile(id_5353_forward_list_model5$diff_1_0.6, 0.75))
diff_1_0.5_index <- which(id_5353_forward_list_model5$diff_1_0.5 > quantile(id_5353_forward_list_model5$diff_1_0.5, 0.75))

id_5353_forward_list_model5$hyd_destination <- id_5353_forward_model5_data$hyd_destination
id_5353_forward_list_model5[diff_1_0.9_index,] %>% ggplot() +
  geom_point(aes(x=fitted.values, y=hyd_destination)) +
  labs(x="pi hat of 1.0 coefs", y="hyd_destination", title="response by pi hat of 1.0 coefs (hurt cases)")
id_5353_forward_list_model5[-diff_1_0.9_index,] %>% ggplot() +
  geom_point(aes(x=fitted.values, y=hyd_destination)) +
  labs(x="pi hat of 1.0 coefs", y="hyd_destination", title="response by pi hat of 1.0 coefs (remaining cases)")

id_5353_forward_list_model5[diff_1_0.9_index,] %>% select(fitted.values, hyd_destination) %>% arrange(fitted.values) %>% print(n=27)

id_5353_forward_list_model5[diff_1_0.9_index,] %>% ggplot() +
  geom_point(aes(x=fitted.values_0.9, y=hyd_destination)) +
  labs(x="pi hat of 0.9 coefs", y="hyd_destination", title="response by pi hat of 0.9 coefs (hurt cases)")
id_5353_forward_list_model5[-diff_1_0.9_index,] %>% ggplot() +
  geom_point(aes(x=fitted.values_0.9, y=hyd_destination)) +
  labs(x="pi hat of 0.9 coefs", y="hyd_destination", title="response by pi hat of 0.9 coefs (remaining cases)")


corr_model5_data <- cor(id_5353_forward_model5_data %>% select(-hyd_destination)) %>% melt
corr_model5_hurt_data <- cor(id_5353_forward_model5_data[diff_1_0.9_index,] %>% select(-hyd_destination)) %>% abs %>% melt
corr_model5_rest_data <- cor(id_5353_forward_model5_data[-diff_1_0.9_index,] %>% select(-hyd_destination)) %>% abs %>% melt

corr_model5_hurt_data$diff <- corr_model5_hurt_data$value - corr_model5_rest_data$value
corr_model5_hurt_data[c(2:5, 8:10, 14:15, 20),]

corr_model5_hurt_data %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for hurt data")

corr_model5_rest_data %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for other data")



corr_model5_hurt_data_all <- cor(id_5353_forward_model5_data[diff_1_0.9_index,] %>%
                                   mutate(hyd_destination=hyd_destination %>% as.character %>% as.numeric)) %>% abs %>% melt
corr_model5_rest_data_all <- cor(id_5353_forward_model5_data[-diff_1_0.9_index,] %>%
                                   mutate(hyd_destination=hyd_destination %>% as.character %>% as.numeric)) %>% abs %>% melt
corr_model5_hurt_data_all$diff <- corr_model5_hurt_data_all$value - corr_model5_rest_data_all$value
corr_model5_hurt_data_all[c(2:6, 9:12, 16:18, 23:24, 30),]

nrow(id_5353_forward_list_model5) # 108
id_5353_forward_list_model5 %>% filter(fitted.values <= 0.1) # 60
id_5353_forward_list_model5 %>% filter(fitted.values >= 0.9) # 12

corr_model5_hurt_data_all %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for hurt data")

corr_model5_rest_data_all %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for other data")


id_5353_forward_model_pi_hat <- tibble(model1=id_5353_forward_list$model1$fitted.values)
for (k in 2:5) {
  id_5353_forward_model_pi_hat[[paste0("model", k)]] <- id_5353_forward_list[[paste0("model", k)]]$fitted.values
}


id_5353_forward_model_pi_hat %>% ggplot() +
  geom_point(aes(x=model4, y=model3)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat of model 4", y=paste0("pi hat of model 3"), title="pi hat of model 4 vs. 3")

id_5353_forward_model_pi_hat %>% ggplot() +
  geom_point(aes(x=model4, y=model2)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat of model 4", y=paste0("pi hat of model 2"), title="pi hat of model 4 vs. 2")

id_5353_forward_model_pi_hat %>% ggplot() +
  geom_point(aes(x=model4, y=model1)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat of model 4", y=paste0("pi hat of model 1"), title="pi hat of model 4 vs. 1")

id_5353_forward_model_pi_hat %>% ggplot() +
  geom_point(aes(x=model3, y=model2)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat of model 3", y=paste0("pi hat of model 2"), title="pi hat of model 3 vs. 2")

id_5353_forward_model_pi_hat %>% ggplot() +
  geom_point(aes(x=model3, y=model1)) +
  geom_abline(slope=1) +
  ylim(c(0,1)) +
  labs(x="pi hat of model 3", y=paste0("pi hat of model 1"), title="pi hat of model 3 vs. 1")


id_5353_forward_model_pi_hat$diff_model4_model3 <- abs(id_5353_forward_model_pi_hat$model4 - id_5353_forward_model_pi_hat$model3)
id_5353_forward_model_pi_hat$diff_model3_model2 <- abs(id_5353_forward_model_pi_hat$model3 - id_5353_forward_model_pi_hat$model2)
diff_model4_3_index <- which(id_5353_forward_model_pi_hat$diff_model4_model3 > quantile(id_5353_forward_model_pi_hat$diff_model4_model3, 0.75))
diff_model3_2_index <- which(id_5353_forward_model_pi_hat$diff_model3_model2 > quantile(id_5353_forward_model_pi_hat$diff_model3_model2, 0.75))
diff_1_0.9_index

id_5353_forward_list_model4 <- tibble(fitted.values=id_5353_forward_list$model4$fitted.values,
                                      hyd_destination=id_5353_forward_list$model4$data$hyd_destination)
corr_model4_3_hurt_data_all <- cor(id_5353_forward_list$model4$data[diff_model4_3_index,] %>%
                                   mutate(hyd_destination=hyd_destination %>% as.character %>% as.numeric)) %>% abs %>% melt
corr_model4_3_rest_data_all <- cor(id_5353_forward_list$model4$data[-diff_model4_3_index,] %>%
                                   mutate(hyd_destination=hyd_destination %>% as.character %>% as.numeric)) %>% abs %>% melt
corr_model4_3_hurt_data_all$diff <- corr_model4_3_hurt_data_all$value - corr_model4_3_rest_data_all$value
corr_model4_3_hurt_data_all[c(2:5, 8:10, 14:15, 20),]


corr_model4_3_hurt_data_all %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for hurt data")

corr_model4_3_rest_data_all %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_viridis_c(limits=c(0,1)) +
  labs(title="abs(correlations) for other data")


id_5353_forward_list_model4[diff_model4_3_index,] %>% ggplot() +
  geom_point(aes(x=fitted.values, y=hyd_destination)) +
  labs(x="pi hat of 1.0 coefs", y="hyd_destination", title="response by pi hat of 1.0 coefs (hurt cases)")
id_5353_forward_list_model4[-diff_model4_3_index,] %>% ggplot() +
  geom_point(aes(x=fitted.values, y=hyd_destination)) +
  labs(x="pi hat of 1.0 coefs", y="hyd_destination", title="response by pi hat of 1.0 coefs (remaining cases)")

## river/road divided by area
municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
municipios_sf$area_m2 <- st_area(municipios_sf) %>% as.numeric
gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination 

id_5353_forward_data_area <- gwr_data_hyd_destination %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
  select(-(municipio:year)) %>% 
  left_join(municipios_sf %>% as_tibble %>% select(id, area_m2), by="id") %>% 
  mutate(coca_area = coca_area / area_m2,
         river_length = river_length / area_m2,
         road_length = road_length / area_m2) %>% 
  select(-area_m2)

glm_id_5353_forward_data_area <- glm(hyd_destination~.,
                                     data = id_5353_forward_data_area %>% select(-id), 
                                     family = binomial)
summary(glm_id_5353_forward_data_area)

prev_data <- id_5353_forward_data_area %>% select(hyd_destination)
remaining_data <- id_5353_forward_data_area %>% select(-id, -hyd_destination)
non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
remaining_data <- remaining_data[,non_sigular_col_index]

id_5353_forward_area_list <- list()
k <- 1
significance <- 1
while (significance) {
  p_values_i <- tibble()
  for (j in 1:ncol(remaining_data)) {
    new_var_j <- remaining_data[,j]
    reg_data_j <- bind_cols(prev_data, new_var_j)
    reg_model_j <- glm(hyd_destination~.,
                       data = reg_data_j,
                       family = binomial)
    var_name_j <- names(new_var_j)
    reg_model_coefs_j <- summary(reg_model_j)$coefficients
    p_value_j <- reg_model_coefs_j[which(rownames(reg_model_coefs_j) == var_name_j), 4]
    p_value_j <- ifelse(p_value_j == 0, 1, p_value_j)
    p_values_i <- bind_rows(p_values_i, tibble(var_name=var_name_j, p_value=p_value_j))
  }
  if (sum(p_values_i$p_value <= sig_level) > 0) {
    best_col_index <- which.min(p_values_i$p_value)
    prev_data <- bind_cols(prev_data, remaining_data[, best_col_index])
    next_best_reg <- glm(hyd_destination~., data = prev_data, family = binomial)
    id_5353_forward_area_list[[paste0("model",k)]] <- next_best_reg
    k <- k + 1
    remaining_data <- remaining_data[, -best_col_index]
    next
  }else{
    significance <- 0
  }
}
lapply(id_5353_forward_area_list, summary)
