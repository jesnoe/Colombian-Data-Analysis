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
library(distances)
{
binary_vars <- c("y", "airport", "armed_group", "ferry", "police", "military")
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
empty_map <- ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group),
               color = "black",
               fill="white",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

base_gwr_data <- read.csv("Colombia Data/base gwr data.csv") %>% as_tibble
hyd_gwr_data <- read.csv("Colombia Data/hyd gwr data.csv") %>% as_tibble
gwr_data_dist <- read.csv("Colombia Data/gwr data dist.csv") %>% as_tibble
influence_tbl <- read.csv("Colombia Data/influence_tbl.csv") %>% as_tibble

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (08-20-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble 
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (09-08-2025).csv") %>% as_tibble 

PML_GWR_pred_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML hyd_source predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_destination predictions leave-one-out n_drop=10.csv") %>% as_tibble
PML_GWR_pred_10_loo_hyd_source_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML hyd_source predictions leave-one-out n_drop=10 weight 7-3.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_source_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_source predictions leave-one-out n_drop=10 weight 7-3.csv") %>% as_tibble
PML_GWR_pred_10_loo_base_dest_7_3 <- read.csv("Colombia Data/local GWR PML result predicted prices/GWR PML base_destination predictions leave-one-out n_drop=10 weight 7-3.csv") %>% as_tibble

CM_var_drop_10_loo_hyd_dest <- confusionMatrix(PML_GWR_pred_10_loo_hyd_dest$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_dest$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_hyd_source <- confusionMatrix(PML_GWR_pred_10_loo_hyd_source$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_source$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_dest <- confusionMatrix(PML_GWR_pred_10_loo_base_dest$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_dest$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_source <- confusionMatrix(PML_GWR_pred_10_loo_base_source$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_source$y %>% as.factor, positive = "1")

CM_var_drop_10_loo_hyd_source_7_3 <- confusionMatrix(PML_GWR_pred_10_loo_hyd_source_7_3$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_hyd_source_7_3$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_source_7_3 <- confusionMatrix(PML_GWR_pred_10_loo_base_source_7_3$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_source_7_3$y %>% as.factor, positive = "1")
CM_var_drop_10_loo_base_dest_7_3 <- confusionMatrix(PML_GWR_pred_10_loo_base_dest_7_3$y_PML_var_drop_loo %>% as.factor, PML_GWR_pred_10_loo_base_dest_7_3$y %>% as.factor, positive = "1")
}
CM_var_drop_10_loo_hyd_dest
CM_var_drop_10_loo_hyd_source
CM_var_drop_10_loo_base_dest
CM_var_drop_10_loo_base_source

CM_var_drop_10_loo_hyd_dest
CM_var_drop_10_loo_hyd_source_7_3
CM_var_drop_10_loo_base_dest_7_3
CM_var_drop_10_loo_base_source_7_3

CM_var_drop_10_loo_hyd_source_9_1
CM_var_drop_10_loo_base_dest_9_1
CM_var_drop_10_loo_base_source_9_1

CM_var_drop_10_loo_hyd_dest$byClass
CM_var_drop_10_loo_hyd_source$byClass
CM_var_drop_10_loo_base_dest$byClass
CM_var_drop_10_loo_base_source$byClass

# gwr_data <- ever_regression_data_years_price_pred("hyd_destination")
# gwr_data$norm$seizures <- regression_data_aggr$seizures_log_scale
# gwr_data$norm$coca_area <- regression_data_aggr$coca_area_log_scale
# gwr_data$norm$lab_prob <- scale(log(1+gwr_data$norm$lab_prob))[,1]
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest

check_most_influential_var <- function(check_id, PML_gwr_coefs, gwr_data_norm) {
  coef_id <- PML_gwr_coefs %>% filter(id == check_id)
  data_id <- gwr_data_norm %>% filter(id == check_id)
  location <- municipio_centroid %>% filter(id==check_id)
  influence <- t((coef_id[,-(1:3)]) * (data_id[,-(1:3)])) %>% as_tibble
  names(influence)[1] <- location$municipio
  return(list(influence=influence, location=location))
}

PML_GWR_pred_10_loo_hyd_dest %>% filter(y == 0 & y_PML_var_drop_loo == 1) %>% arrange(desc(PML_gwr_pi_hat_var_drop_loo)) %>% pull(PML_gwr_pi_hat_var_drop_loo)
PML_GWR_pred_10_loo_hyd_dest_FP <- PML_GWR_pred_10_loo_hyd_dest %>% filter(y == 0 & y_PML_var_drop_loo == 1) %>% arrange(desc(PML_gwr_pi_hat_var_drop_loo))
PML_GWR_pred_10_loo_hyd_dest_FP %>% print(n=20)
check_most_influential_var(27001, PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, hyd_gwr_data) # population
check_most_influential_var(50287, PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, hyd_gwr_data) # lab_prob, seizures, police
check_most_influential_var(23419, PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, hyd_gwr_data) # seizure, police

PML_GWR_pred_10_loo_hyd_dest <- PML_GWR_pred_10_loo_hyd_dest %>% mutate(prediction = ifelse(y == 0 & y_PML_var_drop_loo == 1, "FP",
                                                                                            ifelse(y == 1 & y_PML_var_drop_loo == 1, "TP",
                                                                                                   ifelse(y == 1 & y_PML_var_drop_loo == 0, "FN", "TN"))) %>% as.factor)
# influence_tbl <- tibble(var_name = names(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:3)])
# location_tbl <- tibble()
# for (i in 1:nrow(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)) {
#   id_i <- PML_GWR_pred_10_loo_hyd_dest$id[i]
#   list_i <- check_most_influential_var(id_i, PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, hyd_gwr_data)
#   influence_tbl <- bind_cols(influence_tbl, list_i$influence)
#   location_tbl <- bind_rows(location_tbl, list_i$location)
# }
# var_names <- influence_tbl$var_name
# influence_tbl <- as_tibble(t(influence_tbl[,-1]))
# names(influence_tbl) <- var_names
# influence_tbl <- bind_cols(location_tbl[,-(4:5)], influence_tbl) %>% mutate(pi_hat = PML_GWR_pred_10_loo_hyd_dest$PML_gwr_pi_hat_var_drop_loo) %>% relocate(id:depto, pi_hat)
# influence_tbl <- influence_tbl %>% left_join(PML_GWR_pred_10_loo_hyd_dest %>% select(id, prediction), by="id") %>% relocate(id:depto, prediction)
# write.csv(influence_tbl, "Colombia Data/influence_tbl.csv", row.names = F)

influence_tbl
influence_tbl_FP <- influence_tbl %>% filter(prediction == "FP") %>% arrange(desc(pi_hat))
influence_tbl_TP <- influence_tbl %>% filter(prediction == "TP") %>% arrange(desc(pi_hat))
influence_tbl_FP
influence_tbl_TP
hyd_gwr_data %>% filter(id == 27001)

sum(hyd_gwr_data$id != gwr_data$coord$id)

major_cities <- str_to_upper(c("Medellin", "Cali", "Barranquilla", "Cartagena DE INDIAS", "Santa Marta", "Cucuta", "Ibague", "Villavicencio"))
influence_tbl_major_cities <- influence_tbl %>% filter(municipio %in% major_cities)

ggplot(map_df %>% mutate(city = ifelse(id %in% influence_tbl_major_cities$id, "major cities", "else")),
                         aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=city),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_manual(values = c("else" = "white", "major cities" = "red")) +
    labs(fill="", x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
influence_tbl_major_cities %>% select(id:pi_hat) %>% mutate(pi_hat = round(pi_hat, 5))
for (i in 1:nrow(influence_tbl_major_cities)) {
  id_i <- influence_tbl_major_cities$id[i]
  j <- which(hyd_gwr_data$id == id_i)
  neighbors_id <- hyd_gwr_data$id[order(gwr_data_dist[j,] %>% t)[2:6]]
  location_map <- ggplot(map_df %>% mutate(neighbors = ifelse(id == id_i, "major city",
                                                              ifelse(id %in% neighbors_id, "neighbors", "else"))),
                         aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=neighbors),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_manual(values = c("else" = "white", "neighbors" = "deepskyblue3", "major city" = "red")) +
    labs(fill=major_cities[i], x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/major city maps/%s location.png", major_cities[i]), location_map, scale=1)
  print(bind_rows(influence_tbl_major_cities[i,], influence_tbl %>% filter(id %in% neighbors_id) %>% arrange(id)) %>% mutate(pi_hat = round(pi_hat, 3)))
  print(bind_rows(hyd_gwr_data[j,], hyd_gwr_data %>% filter(id %in% neighbors_id) %>% arrange(id)))
  print(bind_rows(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == id_i),
                  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id %in% neighbors_id) %>% arrange(id)))
}

influence_tbl_FP %>% head
for (i in 1:5) {
  id_i <- influence_tbl_FP$id[i]
  j <- which(hyd_gwr_data$id == id_i)
  neighbors_id <- hyd_gwr_data$id[order(gwr_data_dist[j,] %>% t)[2:6]]
  # location_map <- ggplot(map_df %>% mutate(neighbors = ifelse(id == id_i, "major city",
  #                                                             ifelse(id %in% neighbors_id, "neighbors", "else"))),
  #                        aes(x=long, y=lat)) + 
  #   geom_polygon(aes(group=group, fill=neighbors),
  #                color = "black",
  #                linewidth = 0.1) + 
  #   expand_limits(x = map_df$long, y = map_df$lat) + 
  #   coord_quickmap() +
  #   scale_fill_manual(values = c("else" = "white", "neighbors" = "deepskyblue3", "major city" = "red")) +
  #   labs(fill=major_cities[i], x="", y="", title="") +
  #   theme_bw() +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.border = element_blank(),
  #         axis.text = element_blank(),
  #         line = element_blank()
  #   )
  # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/FP location maps/location %s, %s.png", influence_tbl_FP$municipio[i], influence_tbl_FP$depto[i]), location_map, scale=1)
  print(bind_rows(influence_tbl_FP[i,], influence_tbl %>% filter(id %in% neighbors_id) %>% arrange(id)) %>% mutate(pi_hat = round(pi_hat, 3)))
  print(bind_rows(hyd_gwr_data[j,], hyd_gwr_data %>% filter(id %in% neighbors_id) %>% arrange(id)))
  print(bind_rows(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == id_i),
                  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id %in% neighbors_id) %>% arrange(id)))
}



# adding data-distance neighbors for overffiting cases
  # local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out all var drop log seizure coca scaled n_drop=10 (08-20-2025).RData")
hyd_gwr_data_dist <- dist(hyd_gwr_data %>% select(-(id:y))) %>% as.matrix
hyd_gwr_data %>% filter(id == id_i)

id_i <- 44090 # opposite influence of seizures compared to Santa Marta, Magdalena (a major city)
id_i <- 27001
id_i <- 50287
id_i <- 25851
id_i <- 50350
neighbors %>% select(price_avg:lab_prob) %>% pivot_longer(price_avg:lab_prob, names_to = "variable", values_to = "obs.") %>% ggplot() + geom_boxplot(aes(x=variable, y=obs.))

local_GWR_data_neighbors <- function(id_i, n_neighbors, within_range_weight=1, within_range_drop) {
  j <- which(hyd_gwr_data$id == id_i)
  k <- which(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$id == id_i)
  bw_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$bw[k]
  
  neighbors <- hyd_gwr_data[which(gwr_data_dist[j,] <= bw_i),] %>% filter(id != id_i)
  hyd_gwr_data_i <- hyd_gwr_data %>% filter(id == id_i)
  
  local_GWR_i <- local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  model_vars_i <- (local_GWR_i$coefficients %>% names)[-1]
  # hyd_gwr_data_dist <- dist(hyd_gwr_data %>% select(all_of(model_vars_i))) %>% as.matrix
  ### variables have different scale (normalized, logged, and binary): how can I measure the deviation fairly?
  if (sum(model_vars_i %in% binary_vars) > 0) {
    model_vars_i_continous <- model_vars_i[-which(model_vars_i %in% binary_vars)]
  }else{
    model_vars_i_continous <- model_vars_i
  }
  hyd_gwr_data_i_min <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) min(x))
  hyd_gwr_data_i_max <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) max(x))
  hyd_gwr_data_i_range <- hyd_gwr_data_i_max - hyd_gwr_data_i_min
  hyd_gwr_data_i_continous <- hyd_gwr_data_i %>% select(all_of(model_vars_i_continous))
  hyd_gwr_data_i_vec <- hyd_gwr_data_i_continous %>% t %>% as.vector
  data_distance_weights <- bind_rows(hyd_gwr_data_i_continous, hyd_gwr_data_i_min, hyd_gwr_data_i_max, hyd_gwr_data_i_range) %>% 
    apply(2, function(x) ifelse(x[1] < x[2], 1+(x[2] - x[1])/x[4],
                                ifelse(x[1] > x[3], 1+(x[1] - x[3])/x[4], within_range_weight))) ### make within_range_weight as parameter? or as a function of out-of-range weights?
  outlier_vars <- names(data_distance_weights)[which(data_distance_weights > 1)]
  outlier_vars_weight <- data_distance_weights[which(names(data_distance_weights) %in% outlier_vars)]
  
  if (within_range_drop) {
    hyd_gwr_data_dist <- distances(hyd_gwr_data %>% select(all_of(outlier_vars)) %>% as.data.frame, weights = outlier_vars_weight) %>% as.matrix
    }else{hyd_gwr_data_dist <- distances(hyd_gwr_data %>% select(all_of(model_vars_i_continous)) %>% as.data.frame, weights = data_distance_weights) %>% as.matrix}
  
  data_neighbors_id <- hyd_gwr_data$id[order(hyd_gwr_data_dist[j,] %>% t)[2:(n_neighbors+1)]]
  data_neighbors <- hyd_gwr_data %>% filter(id %in% data_neighbors_id)
  neighbors_i <- bind_rows(neighbors, data_neighbors)
  var_names_i <- names(neighbors_i)[names(neighbors_i) %in% model_vars_i]
  
  model_i <- logistf(y~., neighbors_i %>% select(y, all_of(var_names_i)), alpha = 0.1)
  model_include_i <- logistf(y~., bind_rows(neighbors_i, hyd_gwr_data_i) %>% select(y, all_of(var_names_i)), alpha = 0.1)
  
  data_pred_i <- hyd_gwr_data %>% filter(id == id_i) %>% select(y, all_of(var_names_i)) %>% relocate(y, model_vars_i)
  pi_hat_i <- predict(model_i, data_pred_i, type="response")
  
  data_neighbors_index <- which(hyd_gwr_data$id %in% data_neighbors_id)
  data_neighbors_dist <- tibble(id = hyd_gwr_data$id[data_neighbors_index], dist = hyd_gwr_data_dist[j,data_neighbors_index])
  
  print(hyd_gwr_data %>% filter(id %in% c(id_i, data_neighbors_id)) %>% left_join(data_neighbors_dist, by="id"))
  print(local_GWR_i)
  print(model_i)
  print(model_include_i)
  print(pi_hat_i)
}
hyd_gwr_data %>% filter(id %in% c(id_i, data_neighbors_id)) %>% left_join(data_neighbors_dist, by="id")
local_GWR_i
model_i
pi_hat_i

local_GWR_data_neighbors(27001, 5, within_range_drop = F)
local_GWR_data_neighbors(50287, 5, within_range_drop = F)
local_GWR_data_neighbors(25851, 5, within_range_drop = F)
local_GWR_data_neighbors(44090, 5, within_range_drop = F)

local_GWR_data_neighbors(27001, 1, within_range_drop = F)
local_GWR_data_neighbors(50287, 1, within_range_drop = F)
local_GWR_data_neighbors(25851, 1, within_range_drop = F)
local_GWR_data_neighbors(44090, 1, within_range_drop = F)


local_GWR_data_neighbors(27001, 1, within_range_drop = T)
local_GWR_data_neighbors(50287, 1, within_range_drop = T)
local_GWR_data_neighbors(25851, 1, within_range_drop = T)
local_GWR_data_neighbors(44090, 1, within_range_drop = T)

local_GWR_data_neighbors(27001, 1, within_range_weight = 0.1, within_range_drop = F)
local_GWR_data_neighbors(50287, 1, within_range_weight = 0.5, within_range_drop = F)
local_GWR_data_neighbors(25851, 1, within_range_weight = 0.5, within_range_drop = F)
local_GWR_data_neighbors(44090, 1, within_range_weight = 0.5, within_range_drop = F)

local_GWR_data_neighbors(27001, 1)
local_GWR_data_neighbors(27001, 2)
local_GWR_data_neighbors(27001, 3)
local_GWR_data_neighbors(27001, 4)
local_GWR_data_neighbors(27001, 6)
local_GWR_data_neighbors(27001, 7)
local_GWR_data_neighbors(27001, 8)
local_GWR_data_neighbors(27001, 9)
local_GWR_data_neighbors(27001, 10)

local_GWR_data_neighbors(50287, 5)
local_GWR_data_neighbors(50287, 1)
local_GWR_data_neighbors(50287, 2)
local_GWR_data_neighbors(50287, 3)
local_GWR_data_neighbors(50287, 4)
local_GWR_data_neighbors(50287, 6)
local_GWR_data_neighbors(50287, 7)
local_GWR_data_neighbors(50287, 8)
local_GWR_data_neighbors(50287, 9)
local_GWR_data_neighbors(50287, 10)

neighbors_id <- bind_rows(hyd_gwr_data %>% filter(id == id_i), data_neighbors) 
ggplot(map_df %>% mutate(neighbors = ifelse(id == id_i, "Quibdo",
                                                               ifelse(id %in% neighbors_id$id, "data neighbors", "else"))),
                       aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=neighbors),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = map_df$long, y = map_df$lat) +
  coord_quickmap() +
  scale_fill_manual(values = c("else" = "white", "data neighbors" = "deepskyblue3", "Quibdo" = "red")) +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )


check_id <- id_i
coef_id <- model_i$coefficients[-1]
# location <- municipio_centroid %>% filter(id==check_id)
influence <- coef_id * data_pred_i[-1]
influence

id_i <- 27001 # population coef decreased, but still influential
id_i <- 44090 # population is no longer dominantly influential


influence_tbl_FP
indep_vars <- names(hyd_gwr_data)[-(1:3)]
indep_vars_df <- data.frame(var_name=indep_vars)
coef_table <- tibble(id = influence_tbl_FP$id)
coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars))
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP <- influence_tbl_FP
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_bw <- c()
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_pi_hat <- c()
within_range_weight <- 1
n_neighbors <- 5
data_neighbors_list <- list()
### FP data neighbors local GWR
for (i in 1:nrow(influence_tbl_FP)) {
  id_i <- influence_tbl_FP$id[i]
  j <- which(hyd_gwr_data$id == id_i)
  k <- which(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$id == id_i)
  bw_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$bw[k]
  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_bw <- c(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_bw, bw_i)
  
  neighbors <- hyd_gwr_data[which(gwr_data_dist[j,] <= bw_i),] %>% filter(id != id_i)
  hyd_gwr_data_i <- hyd_gwr_data %>% filter(id == id_i)
  
  local_GWR_i <- local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  model_vars_i <- (local_GWR_i$coefficients %>% names)[-1]
  
  if (sum(model_vars_i %in% binary_vars) > 0) {
    model_vars_i_continous <- model_vars_i[-which(model_vars_i %in% binary_vars)]
  }else{
    model_vars_i_continous <- model_vars_i
  }
  
  hyd_gwr_data_i_min <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) min(x))
  hyd_gwr_data_i_max <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) max(x))
  hyd_gwr_data_i_range <- hyd_gwr_data_i_max - hyd_gwr_data_i_min
  hyd_gwr_data_i_continous <- hyd_gwr_data_i %>% select(all_of(model_vars_i_continous))
  hyd_gwr_data_i_vec <- hyd_gwr_data_i_continous %>% t %>% as.vector
  data_distance_weights <- bind_rows(hyd_gwr_data_i_continous, hyd_gwr_data_i_min, hyd_gwr_data_i_max, hyd_gwr_data_i_range) %>% 
    apply(2, function(x) ifelse(x[1] < x[2], 1+(x[2] - x[1])/x[4],
                                ifelse(x[1] > x[3], 1+(x[1] - x[3])/x[4], within_range_weight)))
  hyd_gwr_data_dist <- distances(hyd_gwr_data %>% select(all_of(model_vars_i_continous)) %>% as.data.frame, weights = data_distance_weights) %>% as.matrix
  
  data_neighbors_id <- hyd_gwr_data$id[order(hyd_gwr_data_dist[j,] %>% t)[2:(n_neighbors+1)]]
  data_neighbors <- hyd_gwr_data %>% filter(id %in% data_neighbors_id)
  neighbors_i <- bind_rows(neighbors, data_neighbors)
  var_names_i <- names(neighbors_i)[names(neighbors_i) %in% model_vars_i]
  data_neighbors_list[[paste0("id_", id_i)]] <- data_neighbors
  
  data_neighbor_model_i <- logistf(y~., neighbors_i %>% select(y, all_of(var_names_i)), alpha = 0.1)
  
  data_pred_i <- hyd_gwr_data %>% filter(id == id_i) %>% select(y, all_of(var_names_i)) %>% relocate(y, model_vars_i)
  pi_hat_i <- predict(data_neighbor_model_i, data_pred_i, type="response")
  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_pi_hat <- c(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_pi_hat, pi_hat_i)
  
  data_neighbors_index <- which(hyd_gwr_data$id %in% data_neighbors_id)
  
  coef_i <- coef(data_neighbor_model_i)
  coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
  coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
  coef_mat[i,] <- coef_i_df$coef
}
coef_table <- bind_cols(coef_table, coef_mat)
names(coef_table)[-1] <- indep_vars
# coef_table$bw <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_bw
coef_table$data_neighbors_pi_hat <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest_FP_pi_hat
influence_tbl_FP %>% select(id:pi_hat) %>% left_join(coef_table, by="id") %>% relocate(id:pi_hat, data_neighbors_pi_hat)
influence_tbl_FP %>% select(id:pi_hat) %>% left_join(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% select(-bw, -Intercept), by="id")


  # maximum relative data distance (calculated for each variable)
data_distance_avg <- c()
data_distance_max <- c()
data_distance_max_var <- c()
data_summary_list <- list()
binary_variable_summary_list <- list()
for (i in 1:nrow(influence_tbl)) {
  id_i <- influence_tbl$id[i]
  j <- which(hyd_gwr_data$id == id_i)
  k <- which(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$id == id_i)
  bw_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$bw[k]
  
  if (is.na(bw_i)) {
    data_distance_avg <- c(data_distance_avg, NA)
    data_distance_max <- c(data_distance_max, NA)
    data_distance_max_var <- c(data_distance_max_var, NA)
    data_summary_list[[paste0("id_", id_i)]] <- NA
    binary_variable_summary_list[[paste0("id_", id_i)]] <- NA
    next
  }
  
  neighbors <- hyd_gwr_data[which(gwr_data_dist[j,] <= bw_i),] %>% filter(id != id_i)
  hyd_gwr_data_i <- hyd_gwr_data %>% filter(id == id_i)
  
  local_GWR_i <- local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  model_vars_i <- (local_GWR_i$coefficients %>% names)[-1]
  
  if (sum(model_vars_i %in% binary_vars) > 0) {
    model_vars_i_continous <- model_vars_i[-which(model_vars_i %in% binary_vars)]
  }else{
    model_vars_i_continous <- model_vars_i
  }
  binary_variable_summary_list[[paste0("id_", id_i)]] <- neighbors %>% select(y, all_of(model_vars_i)) %>% select(any_of(binary_vars)) %>% apply(2, function(x) mean(x))
  
  hyd_gwr_data_i_min <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) min(x))
  hyd_gwr_data_i_max <- neighbors %>% select(all_of(model_vars_i_continous)) %>% apply(2, function(x) max(x))
  hyd_gwr_data_i_range <- hyd_gwr_data_i_max - hyd_gwr_data_i_min
  hyd_gwr_data_i_continous <- hyd_gwr_data_i %>% select(all_of(model_vars_i_continous))
  hyd_gwr_data_i_vec <- hyd_gwr_data_i_continous %>% t %>% as.vector
  data_summary <- bind_rows(hyd_gwr_data_i_continous, hyd_gwr_data_i_min, hyd_gwr_data_i_max, hyd_gwr_data_i_range)
  data_distance_weights <- data_summary %>%
    apply(2, function(x) ifelse(x[1] < x[2], 1+(x[2] - x[1])/x[4],
                                ifelse(x[1] > x[3], 1+(x[1] - x[3])/x[4], 1)))
  data_summary <- bind_rows(data_summary, data_distance_weights) %>% mutate(value = c("obs.", "min", "max", "range", "weight")) %>% relocate(value)
  data_distance_weights %>% t %>% as.vector
  var_names_i <- names(data_distance_weights)
  data_summary_list[[paste0("id_", id_i)]] <- data_summary
  
  data_distance_avg <- c(data_distance_avg, mean(data_distance_weights))
  data_distance_max <- c(data_distance_max, max(data_distance_weights))
  data_distance_max_var <- c(data_distance_max_var, var_names_i[which.max(data_distance_weights)])
}

local_GWR_coefs_without_data_neighbors <- influence_tbl %>% select(id:pi_hat) %>% left_join(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% select(-bw, -Intercept), by="id") %>% 
  mutate(avg_data_dist_weight = data_distance_avg,
         max_data_dist_weight = data_distance_max,
         variable = data_distance_max_var) %>% relocate(id:pi_hat, avg_data_dist_weight, max_data_dist_weight, variable)
local_GWR_coefs_without_data_neighbors
local_GWR_coefs_without_data_neighbors_FP <- local_GWR_coefs_without_data_neighbors %>% filter(prediction == "FP") %>% arrange(desc(pi_hat))

local_GWR_coefs_without_data_neighbors_FP %>% select(id:variable, population, lab_prob)
local_GWR_coefs_without_data_neighbors_FP %>% select(id:variable, population, lab_prob) %>% arrange(desc(avg_data_dist_weight))
local_GWR_coefs_without_data_neighbors_FP %>% select(id:variable, population, lab_prob) %>% arrange(desc(abs(population)))
local_GWR_coefs_without_data_neighbors_FP %>% select(id:variable, population, lab_prob) %>% arrange(desc(abs(lab_prob)))

high_coef_id_population <- local_GWR_coefs_without_data_neighbors_FP %>% arrange(desc(abs(population))) %>% head %>% pull(id)
for (i in 1:length(high_coef_id_population)) {
  id_i <- high_coef_id_population[i]
  k <- which(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$id == id_i)
  bw_i <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$bw[k]
  print(paste("id", id_i))
  print(data_summary_list[[paste0("id_", id_i)]])
  print(binary_variable_summary_list[[paste0("id_", id_i)]])
  print(paste("n_neighbors:", local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]$model %>% nrow))
}
data_summary_list$id_15183
binary_variable_summary_list$id_15183

PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_dest leave-one-out F1 all var drop log seizure scaled n_drop=10 (08-20-2025).csv") %>% as_tibble

bind_rows(PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo %>% filter(id == high_coef_id_population[1]),
          PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo %>% filter(id == high_coef_id_population[2]),
          PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo %>% filter(id == high_coef_id_population[3]),
          PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo %>% filter(id == high_coef_id_population[4]),
          PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo %>% filter(id == high_coef_id_population[5]),
          PML_F1_score_hyd_dest_var_drop_log_seizure_10_loo %>% filter(id == high_coef_id_population[6]))
bind_rows(coef_table %>% filter(id == high_coef_id_population[1]) %>% select(id, population, lab_prob),
          coef_table %>% filter(id == high_coef_id_population[2]) %>% select(id, population, lab_prob),
          coef_table %>% filter(id == high_coef_id_population[3]) %>% select(id, population, lab_prob),
          coef_table %>% filter(id == high_coef_id_population[4]) %>% select(id, population, lab_prob),
          coef_table %>% filter(id == high_coef_id_population[5]) %>% select(id, population, lab_prob),
          coef_table %>% filter(id == high_coef_id_population[6]) %>% select(id, population, lab_prob))


local_GWR_data_neighbors(15183, 5, within_range_drop = F)
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == 15183)
data_summary_list$id_15183; binary_variable_summary_list$id_15183
data_neighbors_list$id_15183
j <- which(hyd_gwr_data$id == 15183)
neighbor_id <- hyd_gwr_data[which(gwr_data_dist[j,] <= 0.5),] %>% pull(id)
data_neighbors_list$id_15183 %>% filter(id %in% neighbor_id) # 0
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_15183$bw_0.5
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_15183$bw_0.5$model

local_GWR_data_neighbors(27361, 5, within_range_drop = F)
data_summary_list$id_27361; binary_variable_summary_list$id_27361
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == 27361)
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_27361$bw_0.8
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_27361$bw_0.8$model

local_GWR_data_neighbors(25851, 5, within_range_drop = F)
data_summary_list$id_25851; binary_variable_summary_list$id_25851
data_neighbors_list$id_25851
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == 25851)
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.5

j <- which(hyd_gwr_data$id == 25851)
neighbor_id <- hyd_gwr_data[which(gwr_data_dist[j,] <= 0.5),] %>% pull(id)
data_neighbors_list$id_25851 %>% filter(id %in% neighbor_id) # 2

local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.5$model %>% as_tibble
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.6$model %>% as_tibble

local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.5$model %>% summary
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.6$model %>% summary

local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.5$model %>% apply(2, sd)
local_GWR_coefs_PML_hyd_dest_var_drop_log_seizure_scaled$id_25851$bw_0.6$model %>% apply(2, sd)

map_df %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=id %in% high_coef_id_population),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())

map_df %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=id == high_coef_id_population[1]),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())

map_df %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=id %in% high_coef_id_population[1:3]),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())

map_df %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=id == 15183),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())

map_df %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=id == 25851),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank())
