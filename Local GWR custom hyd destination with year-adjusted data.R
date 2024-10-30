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
local_GWR_no_overfitting_coefs_bw <- read.csv("Colombia Data/local GWR no overfitting best coefs (10-01-2024).csv") %>% as_tibble
local_GWR_no_overfitting_pvalues_bw <- read.csv("Colombia Data/local GWR no overfitting best p-values (10-01-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result (08-13-2024).RData")

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

### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
gwr_hyd_destination_coord <- left_join(regression_data_years %>%
                                         select(id, year, n_PPI_labs:population, airport, hyd_destination, -base_avg, -base_price_distance,
                                                -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group, -erad_aerial),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)

coord_unique <- gwr_hyd_destination_coord %>% select(id, long, lat) %>% unique
gwr_hyd_destination_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T)
dim(gwr_hyd_destination_dist)
gwr_data_hyd_destination <- gwr_hyd_destination_coord %>% select(-long, -lat, -erad_manual)

regression_data_years %>% select(id, hyd_destination) %>% 
  group_by(id) %>% 
  summarise(across(everything(), mean)) %>% filter(hyd_destination > 0 & hyd_destination < 1)

bwd_range <- seq(0.5, 4, by=0.1)
local_gwr_data_id <- gwr_hyd_destination_coord %>% select(id)
local_gwr_data <- gwr_hyd_destination_coord %>% select(-id, -municipio, -year, -long, -lat)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)
local_gwr_data$hyd_destination %>% table # 0: 2802, 1: 558
ever_anecdotal_data_years$hyd_destination %>% table # 0: 1929, 1: 1431

# experiment with id = 41078
id_41078_index <- which(coord_unique$id == 41078)
local_GWR_coefs_bw %>% filter(id == 41078)
bandwidth <- 1.1

gwr_result_41078_3years_list <- list()
gwr_result_41078_avg_list <- list()
gwr_data_41078_response_3years <- tibble()
gwr_data_41078_p_hat_3years <- list()
gwr_data_41078_p_hat_avg <- list()
gwr_data_41078_response_avg <- tibble()
gwr_data_41078_response_ever <- tibble()
for (bandwidth in seq(0.5, 2.0, by=0.1)) {
  neighbors_41078 <- gwr_hyd_destination_coord %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_41078_index,] <= bandwidth)]) %>% 
    select(-long, -lat)
  ever_anecdotal_neighbors_41078 <- ever_anecdotal_data_years %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_41078_index,] <= bandwidth)]) %>% 
    pull(hyd_destination) %>% factor(levels=c(0, 1))
  
  gwr_data_41078_3years <- neighbors_41078 %>% select(-(id:year))
  gwr_data_41078_avg <- neighbors_41078 %>% select(-municipio, -year) %>% 
    group_by(id) %>% 
    summarise(across(everything(), mean)) %>% 
    mutate(hyd_destination = ifelse(hyd_destination > 0.4, 1, 0))
  
  gwr_data_41078_response_3years <- rbind(gwr_data_41078_response_3years, c(bandwidth, gwr_data_41078_3years$hyd_destination %>% table))
  gwr_data_41078_response_avg <- rbind(gwr_data_41078_response_avg, c(bandwidth, gwr_data_41078_avg$hyd_destination %>% table))
  gwr_data_41078_response_ever <- rbind(gwr_data_41078_response_ever, c(bandwidth, ever_anecdotal_neighbors_41078 %>% table))
  
  gwr_result_41078_3years <- glm(hyd_destination~., data=gwr_data_41078_3years, family=binomial)
  gwr_result_41078_avg <- glm(hyd_destination~., data=gwr_data_41078_avg %>% select(-id), family=binomial)
  gwr_data_41078_p_hat_3years[[paste0("p_hat_bw_", bandwidth)]] <- gwr_result_41078_3years$fitted.values
  gwr_data_41078_p_hat_3years[[paste0("y_bw_", bandwidth)]] <- gwr_result_41078_3years$data$hyd_destination
  gwr_data_41078_p_hat_avg[[paste0("p_hat_bw_", bandwidth)]] <- gwr_result_41078_avg$fitted.values
  gwr_data_41078_p_hat_avg[[paste0("y_bw_", bandwidth)]] <- gwr_result_41078_avg$data$hyd_destination
  
  gwr_result_41078_3years_summary_bw <- summary(gwr_result_41078_3years)
  gwr_result_41078_avg_summary_bw <- summary(gwr_result_41078_avg)
  gwr_result_41078_3years_list[[paste0("bw_", bandwidth)]] <- gwr_result_41078_3years_summary_bw
  gwr_result_41078_avg_list[[paste0("bw_", bandwidth)]] <- gwr_result_41078_avg_summary_bw
}
names(gwr_data_41078_response_3years) <- c("bw", "n0", "n1")
names(gwr_data_41078_response_avg) <- c("bw", "n0", "n1")
names(gwr_data_41078_response_ever) <- c("bw", "n0", "n1")

gwr_result_41078_3years_list[1:8]
gwr_data_41078_response_3years
gwr_result_41078_avg_list[1:8]
gwr_data_41078_response_avg
gwr_data_41078_response_ever

gwr_data_41078_p_hat_3years$p_hat_bw_1.1 %>% round(2)
gwr_data_41078_p_hat_avg$p_hat_bw_1.1 %>% round(2)
roc_41078 <- roc(gwr_data_41078_p_hat_3years$y_bw_1.1, gwr_data_41078_p_hat_3years$p_hat_bw_1.1)
auc(roc_41078)

plot(gwr_data_41078_p_hat_3years$p_hat_bw_0.8,
     gwr_data_41078_p_hat_3years$p_hat_bw_0.9[id_0.8],
     xlab="prob of bw=0.8",
     ylab="prob of bw=0.9")

confusionMatrix(ifelse(gwr_data_41078_p_hat_3years$p_hat_bw_0.8 >= 0.5, 1, 0) %>% as.factor,
                neighbors_41078_0.8$hyd_destination %>% as.factor,
                positive = "1")
confusionMatrix(ifelse(gwr_data_41078_p_hat_3years$p_hat_bw_0.9[id_0.8] >= 0.5, 1, 0) %>% as.factor,
                neighbors_41078_0.9$hyd_destination[id_0.8] %>% as.factor,
                positive = "1")
confusionMatrix(ifelse(gwr_data_41078_p_hat_3years$p_hat_bw_0.9 >= 0.5, 1, 0) %>% as.factor,
                neighbors_41078_0.9$hyd_destination %>% as.factor,
                positive = "1")

large_gap_id <- which(gwr_data_41078_p_hat_3years$p_hat_bw_0.9[id_0.8] - gwr_data_41078_p_hat_3years$p_hat_bw_0.8 > 0.5)
neighbors_41078_0.9[id_0.8,][large_gap_id,] %>% view

gwr_hyd_destination_coord %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[id_41078_index,] <= 0.8)]) %>% 
  select(-long, -lat) -> neighbors_41078_0.8
gwr_hyd_destination_coord %>% 
  filter(id %in% coord_unique$id[which(local_gwr_dist[id_41078_index,] <= 0.9)]) %>% 
  select(-long, -lat) -> neighbors_41078_0.9

id_0.8 <- which(neighbors_41078_0.9$id %in% neighbors_41078_0.8$id)
neighbors_41078_0.9 %>% filter(!(id %in% neighbors_41078_0.8$id)) %>% view
new_data <- neighbors_41078_0.9 %>% filter(!(id %in% neighbors_41078_0.8$id))
new_data[18,] %>% view

results <- list()
for (i in 1:24) {
  added_data <- rbind(neighbors_41078_0.8, new_data[i,])
  added_glm <- glm(hyd_destination~., data=added_data %>% select(-(id:year)), family=binomial)
  results[[paste0("index", i)]] <- added_glm %>% summary
}

new_data_18 <- new_data[18,] %>% select(-(id:year), -n_hyd_labs, -erad_manual, -hyd_destination) %>% t
-6.145113e+01 + matrix(results$index17$coefficients[,1][-1], nrow=1) %*% matrix(new_data_18, ncol=1)
results$index17$coefficients[,1][-1] * t(new_data_18)
# re-run with lab existence
gwr_hyd_destination_ex <- gwr_hyd_destination_coord %>% 
  select(-long, -lat) %>% 
  mutate(across(c(n_PPI_labs, n_hyd_labs, n_armed_groups), ~ ifelse(.x > 0, 1, 0)))
gwr_result_41078_3years_ex_list <- list()
gwr_result_41078_avg_ex_list <- list()
gwr_data_41078_p_hat_3years_ex <- list()
gwr_data_41078_p_hat_avg_ex <- list()
for (bandwidth in seq(0.5, 2.0, by=0.1)) {
  neighbors_41078 <- gwr_hyd_destination_ex %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_41078_index,] <= bandwidth)])
  
  gwr_data_41078_3years <- neighbors_41078 %>% select(-(id:year))
  gwr_data_41078_avg <- neighbors_41078 %>% select(-municipio, -year) %>% 
    group_by(id) %>% 
    summarise(across(everything(), mean)) %>% 
    mutate(hyd_destination = ifelse(hyd_destination > 0.4, 1, 0))
  
  
  gwr_result_41078_3years <- glm(hyd_destination~., data=gwr_data_41078_3years, family=binomial)
  gwr_result_41078_avg <- glm(hyd_destination~., data=gwr_data_41078_avg %>% select(-id), family=binomial)
  gwr_data_41078_p_hat_3years_ex[[paste0("p_hat_bw_", bandwidth)]] <- gwr_result_41078_3years$fitted.values
  gwr_data_41078_p_hat_3years_ex[[paste0("y_bw_", bandwidth)]] <- gwr_result_41078_3years$data$hyd_destination
  gwr_data_41078_p_hat_avg_ex[[paste0("p_hat_bw_", bandwidth)]] <- gwr_result_41078_avg$fitted.values
  gwr_data_41078_p_hat_avg_ex[[paste0("y_bw_", bandwidth)]] <- gwr_result_41078_avg$data$hyd_destination
  
  gwr_result_41078_3years_summary_bw <- summary(gwr_result_41078_3years)
  gwr_result_41078_avg_summary_bw <- summary(gwr_result_41078_avg)
  gwr_result_41078_3years_ex_list[[paste0("bw_", bandwidth)]] <- gwr_result_41078_3years_summary_bw
  gwr_result_41078_avg_ex_list[[paste0("bw_", bandwidth)]] <- gwr_result_41078_avg_summary_bw
}
gwr_result_41078_3years_ex_list[1:8] # overfitting at bw=0.8 and then stabilize
gwr_result_41078_avg_ex_list[1:8]    # underfitting until bw=0.8, overfitting at bw=0.9
