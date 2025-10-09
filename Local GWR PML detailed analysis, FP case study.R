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
influence_tbl <- tibble(var_name = names(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:3)])
location_tbl <- tibble()
for (i in 1:nrow(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)) {
  id_i <- PML_GWR_pred_10_loo_hyd_dest$id[i]
  list_i <- check_most_influential_var(id_i, PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, hyd_gwr_data)
  influence_tbl <- bind_cols(influence_tbl, list_i$influence)
  location_tbl <- bind_rows(location_tbl, list_i$location)
}
var_names <- influence_tbl$var_name
influence_tbl <- as_tibble(t(influence_tbl[,-1]))
names(influence_tbl) <- var_names
influence_tbl <- bind_cols(location_tbl[,-(4:5)], influence_tbl) %>% mutate(pi_hat = PML_GWR_pred_10_loo_hyd_dest$PML_gwr_pi_hat_var_drop_loo) %>% relocate(id:depto, pi_hat)
influence_tbl <- influence_tbl %>% left_join(PML_GWR_pred_10_loo_hyd_dest %>% select(id, prediction), by="id") %>% relocate(id:depto, prediction)
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
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/FP location maps/location %s, %s.png", influence_tbl_FP$municipio[i], influence_tbl_FP$depto[i]), location_map, scale=1)
  print(bind_rows(influence_tbl_FP[i,], influence_tbl %>% filter(id %in% neighbors_id) %>% arrange(id)) %>% mutate(pi_hat = round(pi_hat, 3)))
  print(bind_rows(hyd_gwr_data[j,], hyd_gwr_data %>% filter(id %in% neighbors_id) %>% arrange(id)))
  print(bind_rows(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id == id_i),
                  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id %in% neighbors_id) %>% arrange(id)))
}
