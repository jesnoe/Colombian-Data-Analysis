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
local_GWR_coefs_bw <- read.csv("Colombia Data/local GWR best coefs (08-13-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result (08-13-2024).RData")

#### extreme ceof check
local_GWR_coefs_bw$coefs_sum <- apply(local_GWR_coefs_bw, 1, function(x) sum(abs(x[-(1:5)]), na.rm=T))
local_GWR_coefs_bw <- local_GWR_coefs_bw %>% 
  relocate(id, bw, n_neighbors, n_NA, coefs_sum)
local_GWR_coefs_bw %>% select(id:coefs_sum) %>% arrange(desc(coefs_sum)) %>% view
summary(local_GWR_coefs_bw$coefs_sum)

aic_score_mat %>% filter(id == 41078)
tibble(bw=seq(1, 2, by=0.1), AIC=(aic_score_mat %>% filter(id == 41078))[, 7:17] %>% t %>% as.vector) %>% 
  ggplot() + geom_line(aes(x=bw, y=AIC))
aic_score_mat %>% filter(id == 85225) %>% view
gwr_result_list$id_41078$bw_1.1
gwr_result_list$id_41078$bw_1.3 %>% summary

gwr_result_list$id_41078$bw_1.1 %>% summary
gwr_result_list$id_41078$bw_1.2 %>% summary
gwr_result_list$id_41078$bw_1.3 %>% summary
gwr_result_list$id_41078$bw_1.4 %>% summary


gwr_result_list$id_41078$bw_1.1$data$hyd_destination %>% table # 0:  18, 1: 126
gwr_result_list$id_41078$bw_1.2$data$hyd_destination %>% table # 0:  36, 1: 141
gwr_result_list$id_41078$bw_1.3$data$hyd_destination %>% table # 0:  72, 1: 262
gwr_result_list$id_41078$bw_1.4$data$hyd_destination %>% table # 0: 108, 1: 198

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
gwr_result_id_52227_bw_1.4 <- glm(hyd_destination~.,
                                  data=gwr_result_list$id_52227$bw_1.4$data,
                                  family=binomial)

MASS::stepAIC(gwr_result_id_52227_bw_1.1, trace=F, direction="both")
MASS::stepAIC(gwr_result_id_52227_bw_1.2, trace=F, direction="both")
MASS::stepAIC(gwr_result_id_52227_bw_1.3, trace=F, direction="both")
MASS::stepAIC(gwr_result_id_52227_bw_1.4, trace=F, direction="both")



