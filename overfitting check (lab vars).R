# Overfitting in lab variables seems to result from high correlation between lab_reported and lab_residual
# It happens in region that have different lab detection patterns from the global lab occurrence pattern
# Should we use just lab_residual? Then we need to revise discussion on the control function method

# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(ggpattern)
library(gridExtra)
library(lubridate)
library(colmaps) # https://github.com/nebulae-co/colmaps
library(sf)
library(sp)
library(caret)
library(pracma)
library(GWmodel)
library(pROC)
library(glmnet)
library(reshape2)
library(regclass)
library(logistf)
library(knitr)
library(collinear)
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
  
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id) %>% 
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
}

## lab_prob model

# local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
# rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo)
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_prob (07-15-2026).RData")

PML_gwr_coefs_AUC_lab_prob_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_prob (07-15-2026).csv") %>% as_tibble
lab_var_check <- PML_gwr_coefs_AUC_lab_prob_1617 %>% select(id, bw, lab_prob)

PML_gwr_coefs_AUC_lab_prob_1617 %>% filter(abs(lab_prob) >= 60)

id_j <- 15109
id_j <- 15131
id_j <- 15276
bw_j <- lab_var_check %>% filter(id == id_j) %>% pull(bw)
municipios_capital %>% filter(id == id_j)
GWR_j <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_j)]][[paste0("bw_", bw_j)]]
GWR_j$model %>% select(-y) %>% cor
GWR_j %>% summary
GWR_j$model %>% arrange(y)
GWR_j$model %>% arrange(lab_reported)
logistf(y~., GWR_j$model %>% select(-population))
logistf(y~., GWR_j$model %>% select(-lab_reported))


indep_vars <- c("coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_prob", "left_wing", "right_paramilitary", "left_wing:right_paramilitary")
var_names <- tibble(var_name = indep_vars)
VIF_tbl <- matrix(NA, nrow(PML_gwr_coefs_AUC_lab_prob_1617), 1+length(indep_vars))
colnames(VIF_tbl) <- c("id", indep_vars)
for (i in 1:nrow(lab_var_check)) {
  id_i <- lab_var_check$id[i]
  bw_i <- lab_var_check$bw[i]
  model_i <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  if (is.null(model_i)) {
    VIF_tbl[i,1] <- id_i
    next
  }
  coef_names_i <- names(coef(model_i))[-1]
  VIF_i <- left_join(var_names, vif_df(model_i$model, coef_names_i) %>% rename(var_name = predictor), by="var_name")
  VIF_tbl[i,1] <- id_i
  VIF_tbl[i,-1] <- VIF_i$vif
}
VIF_tbl_lab_prob <- VIF_tbl %>% as_tibble
VIF_tbl_lab_prob <- left_join(VIF_tbl_lab_prob, PML_gwr_coefs_AUC_lab_prob_1617 %>% select(id, lab_prob) %>% rename(coef_lab_prob = lab_prob), by="id")
VIF_tbl_lab_prob

######

PML_gwr_coefs_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price CF (05-08-2026).csv") %>% as_tibble
PML_gwr_pvals_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price CF (05-08-2026).csv") %>% as_tibble

# local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
# rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo)
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price CF (05-08-2026).RData")
lab_var_check <- PML_gwr_coefs_AUC_CF_1617 %>% select(id, bw, lab_reported, lab_residual)

#VIF check
indep_vars <- c("coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary", "left_wing:right_paramilitary")
var_names <- tibble(var_name = indep_vars)
VIF_tbl <- matrix(NA, nrow(PML_gwr_coefs_AUC_CF_1617), 1+length(indep_vars))
colnames(VIF_tbl) <- c("id", indep_vars)
for (i in 1:nrow(lab_var_check)) {
  id_i <- lab_var_check$id[i]
  bw_i <- lab_var_check$bw[i]
  model_i <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  if (is.null(model_i)) {
    VIF_tbl[i,1] <- id_i
    next
  }
  coef_names_i <- names(coef(model_i))[-1]
  VIF_i <- left_join(var_names, vif_df(model_i$model, coef_names_i) %>% rename(var_name = predictor), by="var_name")
  VIF_tbl[i,1] <- id_i
  VIF_tbl[i,-1] <- VIF_i$vif
}
VIF_tbl_both_lab_vars <- VIF_tbl %>% as_tibble
VIF_tbl_both_lab_vars <- left_join(VIF_tbl_both_lab_vars, PML_gwr_coefs_AUC_CF_1617 %>% select(id, lab_residual, lab_reported) %>% rename(coef_lab_res = lab_residual, coef_lab_rep = lab_reported), by="id")
VIF_tbl_both_lab_vars

n_neighbors <- c()
y_ratio <- c()
for (i in 1:nrow(lab_var_check)) {
  id_i <- lab_var_check$id[i]
  bw_i <- lab_var_check$bw[i]
  neighbors_i <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]$model
  n_neighbors_i <- nrow(neighbors_i)
  if (is.null(n_neighbors_i)) {
    n_neighbors_i <- NA
    y_ratio_i <- NA
  }else{
    y_ratio_i <- sum(neighbors_i$y)/n_neighbors_i
  }
  
  n_neighbors <- c(n_neighbors, n_neighbors_i)
  y_ratio <- c(y_ratio, y_ratio_i)
}
n_neighbors
y_ratio
lab_var_check$n_neighbors <- n_neighbors
lab_var_check$y_ratio <- y_ratio

municipios_capital %>% filter(id == 5376)
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5 %>% summary
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% arrange(y)
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% arrange(lab_reported)
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% select(-y) %>% cor
logistf(y~., local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% select(-population))
logistf(y~., local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% select(-lab_reported))

lab_var_check %>% arrange(lab_reported)
lab_var_check %>% arrange(desc(lab_reported))

id_j <- 5197
bw_j <- lab_var_check %>% filter(id == id_j) %>% pull(bw)
municipios_capital %>% filter(id == id_j)
GWR_j <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_j)]][[paste0("bw_", bw_j)]]
GWR_j %>% summary
GWR_j$model %>% arrange(y)
GWR_j$model %>% arrange(lab_reported)
GWR_j$model %>% select(-y) %>% cor
logistf(y~., GWR_j$model %>% select(-population))
logistf(y~., GWR_j$model %>% select(-lab_reported))



lab_var_check %>% ggplot +
  geom_point(aes(x = n_neighbors, y = lab_reported))
lab_var_check %>% ggplot +
  geom_point(aes(x = n_neighbors, y = abs(lab_reported)))

lab_var_check %>% ggplot +
  geom_point(aes(x = y_ratio, y = lab_reported))
lab_var_check %>% ggplot +
  geom_point(aes(x = y_ratio, y = abs(lab_reported)))

# lab_seridual only model check
lab_res_PML_gwr_coefs_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_residual (07-15-2026).csv") %>% as_tibble
lab_res_PML_gwr_pvals_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_residual (07-15-2026).csv") %>% as_tibble

# local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
# rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo)
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_residual (07-15-2026).RData")
lab_var_check <- lab_res_PML_gwr_coefs_AUC_CF_1617 %>% select(id, bw, lab_residual)

#VIF check
indep_vars <- c("coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary", "left_wing:right_paramilitary")
var_names <- tibble(var_name = indep_vars)
VIF_tbl <- matrix(NA, nrow(PML_gwr_coefs_AUC_CF_1617), 1+length(indep_vars))
colnames(VIF_tbl) <- c("id", indep_vars)
for (i in 1:nrow(lab_var_check)) {
  id_i <- lab_var_check$id[i]
  bw_i <- lab_var_check$bw[i]
  model_i <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]
  if (is.null(model_i)) {
    VIF_tbl[i,1] <- id_i
    next
  }
  coef_names_i <- names(coef(model_i))[-1]
  VIF_i <- left_join(var_names, vif_df(model_i$model, coef_names_i) %>% rename(var_name = predictor), by="var_name")
  VIF_tbl[i,1] <- id_i
  VIF_tbl[i,-1] <- VIF_i$vif
}
VIF_tbl_lab_res <- VIF_tbl %>% as_tibble
VIF_tbl_lab_res <- left_join(VIF_tbl_lab_res, lab_res_PML_gwr_coefs_AUC_CF_1617 %>% select(id, lab_residual) %>% rename(coef_lab_res = lab_residual), by="id")
VIF_tbl_lab_res

n_neighbors <- c()
y_ratio <- c()
for (i in 1:nrow(lab_var_check)) {
  id_i <- lab_var_check$id[i]
  bw_i <- lab_var_check$bw[i]
  neighbors_i <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]]$model
  n_neighbors_i <- nrow(neighbors_i)
  if (is.null(n_neighbors_i)) {
    n_neighbors_i <- NA
    y_ratio_i <- NA
  }else{
    y_ratio_i <- sum(neighbors_i$y)/n_neighbors_i
  }
  
  n_neighbors <- c(n_neighbors, n_neighbors_i)
  y_ratio <- c(y_ratio, y_ratio_i)
}
n_neighbors
y_ratio
lab_var_check$n_neighbors <- n_neighbors
lab_var_check$y_ratio <- y_ratio

municipios_capital %>% filter(id == 5376)
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5 %>% summary
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% arrange(y)
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% arrange(lab_reported)
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% select(-y) %>% cor
logistf(y~., local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% select(-population))
logistf(y~., local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5376$bw_0.5$model %>% select(-lab_reported))

lab_var_check %>% arrange(lab_residual)
lab_var_check %>% arrange(desc(lab_residual))

id_j <- 23168
id_j <- 15276
id_j <- 5197
id_j <- 5376
bw_j <- lab_var_check %>% filter(id == id_j) %>% pull(bw)
municipios_capital %>% filter(id == id_j)
GWR_j <- local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo[[paste0("id_", id_j)]][[paste0("bw_", bw_j)]]
GWR_j %>% summary
GWR_j$model %>% arrange(y)
GWR_j$model %>% arrange(lab_residual)
GWR_j$model %>% select(-y) %>% cor
logistf(y~., GWR_j$model %>% select(-population))
logistf(y~., GWR_j$model %>% select(-lab_reported))
