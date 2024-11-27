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
  ferry <- read.csv("Colombia Data/ferry terminals.csv") %>% as_tibble
  police <- read.csv("Colombia Data/polices.csv") %>% as_tibble
  military <- read.csv("Colombia Data/military.csv") %>% as_tibble
  ferry$n_police <- police$n_polices
  ferry$n_military <- military$n_military
}

n_reg_data_mat <- read.csv("Colombia Data/local GWR number of neighbors (08-13-2024).csv") %>% as_tibble
aic_score_mat <- read.csv("Colombia Data/local GWR AIC (08-13-2024).csv") %>% as_tibble
local_GWR_coefs_bw <- read.csv("Colombia Data/local GWR best coefs (08-13-2024).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs (10-16-2024).csv") %>% as_tibble

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1]) %>% 
  left_join()
  

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

local_gwr_forward_coefs <- local_GWR_coefs_bw %>% 
  select(-n_neighbors, -n_NA, -n_PPI_labs, -n_hyd_labs, -erad_manual, -hyd_group) %>%
  rename(armed_group=n_armed_groups)
local_gwr_forward_models <- list()
local_gwr_performance <- tibble()
sig_level <- 0.1
for (i in 1:nrow(local_gwr_forward_coefs)) {
  id_i <- local_gwr_forward_coefs$id[i]
  bw_i <- local_gwr_forward_coefs$bw[i]
  id_i_index <- which(coord_unique$id == id_i)
  neighbors_i <- gwr_data_hyd_destination %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw_i)]) %>% 
    select(-(id:year))
  
  if (table(neighbors_i$hyd_destination)[2] == 0) {
    local_gwr_forward_models[[paste0("id_", id_i)]] <- NA
    local_gwr_forward_coefs[i,] <- c(id_i, bw_i, rep(NA, 13)) %>% matrix(nrow=1) %>% as_tibble
    performance_i <- c(id_i, rep(NA, 5))
    names(performance_i) <- c("id", "n_var", "AUC", "threshold", "specificity", "sensitivity")
    local_gwr_performance <- bind_rows(local_gwr_performance, performance_i)
    next
  }
  
  prev_data <- neighbors_i %>% select(hyd_destination)
  remaining_data <- neighbors_i %>% select(-hyd_destination)
  non_sigular_col_index <- which((remaining_data %>% apply(2, function(x) x %>% table %>% length)) > 1)
  remaining_data <- remaining_data[,non_sigular_col_index]
  
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
      remaining_data <- remaining_data[, -best_col_index]
      next
    }else{
      significance <- 0
    }
  }
  reg_model_i <- glm(hyd_destination~.,
                     data = prev_data,
                     family = binomial)
  local_gwr_forward_models[[paste0("id_", id_i)]] <- reg_model_i
  roc_i <- roc(prev_data$hyd_destination, reg_model_i$fitted.values)
  performance_i <- c(id_i, ncol(prev_data)-1, auc(roc_i), (coords(roc_i, "best"))[1,] %>% unlist)
  names(performance_i) <- c("id", "n_var", "AUC", "threshold", "specificity", "sensitivity")
  local_gwr_performance <- bind_rows(local_gwr_performance, performance_i)
  local_gwr_forward_coefs$bw[i] <- bw_i
  if (i %% 100 == 0) print(paste(i, "complete"))
}

local_gwr_forward_pvals <- local_gwr_forward_coefs
var_table <- tibble(var_name=names(local_gwr_forward_coefs)[-(1:2)])
for (i in 1:nrow(local_gwr_forward_coefs)) {
  id_i <- local_gwr_forward_coefs$id[i]
  bw_i <- local_gwr_forward_coefs$bw[i]
  reg_model_i <- local_gwr_forward_models[[paste0("id_", id_i)]]
  
  if (is.na(reg_model_i[1])) next
  
  coefs_i <- tibble(var_name = names(reg_model_i$coefficients),
                    coef = reg_model_i$coefficients,
                    p_val = summary(reg_model_i)$coefficients[,4])
  coefs_i[1,1] <- "Intercept"
  coefs_i <- left_join(var_table, coefs_i, by="var_name")
  local_gwr_forward_coefs[i,] <- c(id_i, bw_i, coefs_i$coef) %>% matrix(ncol=ncol(local_gwr_forward_coefs)) %>% as_tibble
  local_gwr_forward_pvals[i,] <- c(id_i, bw_i, coefs_i$p_val) %>% matrix(ncol=ncol(local_gwr_forward_pvals)) %>% as_tibble
}

local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
local_gwr_forward_pvals <- read.csv("Colombia Data/local GWR best p-values forward selection (10-29-2024).csv") %>% as_tibble
local_gwr_performance <- read.csv("Colombia Data/local GWR performance forward selection (10-29-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result forward selection (10-29-2024).RData")

local_gwr_performance
local_gwr_forward_coefs
local_gwr_forward_pvals
# save("local_gwr_forward_models", file = "Colombia Data/local GWR result forward selection (10-29-2024).RData")
# local_gwr_performance %>% write.csv("Colombia Data/local GWR performance forward selection (10-29-2024).csv", row.names=F)
# local_gwr_forward_coefs %>% write.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv", row.names=F)
# local_gwr_forward_pvals %>% write.csv("Colombia Data/local GWR best p-values forward selection (10-29-2024).csv", row.names=F)

prev_data <- local_gwr_forward_models$id_41078$data %>% select(hyd_destination)
remaining_data <- local_gwr_forward_models$id_41078$data %>% select(-hyd_destination)

for (k in 1:(ncol(remaining_data)-1)) {
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
    remaining_data <- remaining_data[, -best_col_index]
    next
  }
}
glm(hyd_destination~., data=prev_data, family=binomial) %>% summary
p_values_i

local_gwr_forward_coefs %>% apply(1, function(x) sum(!is.na(x[-(1:3)]))) %>% table
# n_coef    0   1   2   3   4   5   6   7   8 
# n_muni  132 291 194 168 155  96  52  25   7

depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

for (i in 4 :length(local_gwr_forward_coefs)) {
  var_name <- names(local_gwr_forward_coefs)[i]
  gwr_coefs_i <- data.frame(id=local_gwr_forward_coefs$id,
                            coef=local_gwr_forward_coefs[[var_name]],
                            rounded_coef=local_gwr_forward_coefs[[var_name]] %>% round(2))
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
  
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR forward selection/hyd destination GWR FS coef ",
                var_name, ".png"),
         gwr_coef_map, scale=1)
}

## overfitting case study
local_gwr_forward_coefs %>% filter(abs(hyd_lab_prob) > 1000) %>% arrange(id)
local_gwr_forward_coefs %>% filter(abs(PPI_lab_prob) > 500) %>% arrange(id)
local_gwr_forward_coefs %>% filter(abs(hyd_seizures) > 500) %>% arrange(id)

i <- which(local_gwr_forward_coefs$id == 5353)

local_gwr_forward_VIF <- local_gwr_forward_coefs
local_gwr_forward_VIF[,-(1:2)] <- matrix(NA, nrow(local_gwr_forward_VIF), 13)
local_gwr_forward_VDP <- local_gwr_forward_VIF
local_gwr_forward_corr <- list()
local_gwr_forward_CN <- c()
for (i in 1:nrow(local_gwr_forward_VIF)) {
  id_i <- local_gwr_forward_VIF$id[i]
  bw_i <- local_gwr_forward_VIF$bw[i]
  neighbor_index <- which(local_gwr_dist[i,] <= bw_i)
  neighbor_id <- coord_unique$id[neighbor_index]
  id_i_data_coord <- gwr_hyd_destination_coord %>% 
    filter(id %in% neighbor_id) %>% 
    select(-municipio, -year, -n_PPI_labs, -n_hyd_labs, -erad_manual)
  id_i_index <- which(id_i_data_coord$id == id_i)[1]
  
  if (table(id_i_data_coord$hyd_destination) %>% length == 1 | bw_i > 2.9) {
    corr_i <- cor(id_i_data_coord %>% select(-id, -hyd_destination, -long, -lat))
    local_gwr_forward_corr[[paste0("id_", id_i)]] <- corr_i
    local_gwr_forward_CN <- c(local_gwr_forward_CN, NA)
    next
  }
  sigular_col <- -which((id_i_data_coord %>% apply(2, function(x) table(x) %>% length)) == 1)
  if (!is.na(sigular_col[1])) id_i_data_coord <- id_i_data_coord[, sigular_col]
  id_i_data <- id_i_data_coord %>% select(-id, -long, -lat)
  collinearity_data_sp <- SpatialPointsDataFrame(id_i_data_coord %>% select(long, lat), id_i_data)
  
  
  col_diag <- gwr.collin.diagno_i(hyd_destination ~ ., collinearity_data_sp, bw_i, kernel="boxcar",local_i=id_i_index)
  col_diag_i <- as_tibble(col_diag$SDF)[id_i_index,]
  col_diag_i <- tibble(collinearity_measure=names(col_diag_i), value=t(col_diag_i) %>% as.vector) %>% 
    filter(!grepl("long", collinearity_measure)) %>% 
    filter(!grepl("lat", collinearity_measure))
  var_name_df <- data.frame(var_name=names(local_gwr_forward_VIF)[-(1:2)])
  VIF_i <- col_diag_i %>% filter(grepl("VIF", collinearity_measure)) %>% rename(var_name=collinearity_measure) %>% 
    mutate(var_name=gsub("_VIF", "", var_name))
  VDP_i <- col_diag_i %>% filter(grepl("VDP", collinearity_measure)) %>% rename(var_name=collinearity_measure) %>% 
    mutate(var_name=gsub("_VDP", "", var_name))
  corr_i <- col_diag_i %>% filter(grepl("Corr", collinearity_measure)) %>% rename(correlation=collinearity_measure)
  local_CN_i <- col_diag_i %>% filter(grepl("local_CN", collinearity_measure)) %>% pull(value)
  
  VIF_i <- left_join(var_name_df, VIF_i, by="var_name")
  VDP_i <- left_join(var_name_df, VDP_i, by="var_name")
  local_gwr_forward_VIF[i,] <- matrix(c(id_i, bw_i, VIF_i$value), 1)
  local_gwr_forward_VDP[i,] <- matrix(c(id_i, bw_i, VDP_i$value), 1)
  local_gwr_forward_corr[[paste0("id_", id_i)]] <- corr_i
  local_gwr_forward_CN <- c(local_gwr_forward_CN, local_CN_i)
}
local_gwr_forward_VIF$condition_number <- local_gwr_forward_CN
local_gwr_forward_VIF
local_gwr_forward_VDP
local_gwr_forward_CN
local_gwr_forward_corr$id_5353
# write.csv(local_gwr_forward_VIF, "Colombia Data/local GWR VIF forward selection (11-06-2024).csv", row.names = F)
# write.csv(local_gwr_forward_VDP, "Colombia Data/local GWR VDF forward selection (11-06-2024).csv", row.names = F)
# save("local_gwr_forward_corr", file = "Colombia Data/local GWR VDF forward selection (11-06-2024).RData", row.names = F)

for (i in 4 :length(local_gwr_forward_VIF)) {
  var_name <- names(local_gwr_forward_VIF)[i]
  gwr_VIF_i <- data.frame(id=local_gwr_forward_VIF$id,
                            value=local_gwr_forward_VIF[[var_name]],
                            rounded_value=local_gwr_forward_VIF[[var_name]] %>% round(2))
  # max_value <- max(gwr_VIF_i$value, na.rm=T)
  # min_value <- min(gwr_VIF_i$value, na.rm=T)
  coef_map_coords <- map_df %>% 
    left_join(gwr_VIF_i, by="id")
  
  gwr_VIF_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=value),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map$long, y = depto_map$lat) + 
    coord_quickmap() +
    scale_fill_viridis_c(na.value = "white")+
    # scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
    #                      values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_value/abs(min_value))),
    #                      na.value = "white") +
    labs(fill=var_name, x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR forward selection/multicollinearity/hyd destination GWR FS VIF ",
                var_name, ".png"),
         gwr_VIF_map, scale=1)
  if (i == 16) break
  
  gwr_VDP_i <- data.frame(id=local_gwr_forward_VDP$id,
                          value=local_gwr_forward_VDP[[var_name]],
                          rounded_value=local_gwr_forward_VDP[[var_name]] %>% round(2))
  coef_map_coords <- map_df %>% 
    left_join(gwr_VDP_i, by="id")
  
  
  gwr_VDP_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=value),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map$long, y = depto_map$lat) + 
    coord_quickmap() +
    scale_fill_viridis_c(na.value = "white")+
    # scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
    #                      values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_value/abs(min_value))),
    #                      na.value = "white") +
    labs(fill=var_name, x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR forward selection/multicollinearity/hyd destination GWR FS VDP ",
                var_name, ".png"),
         gwr_VDP_map, scale=1) 
}

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

lapply(id_5353_forward_list, summary)

for (i in 1:length(id_5353_forward_list)) {
  model_i <- id_5353_forward_list[[i]]
  data_i <- model_i$data
  data_i$fitted <- model_i$fitted.values
  fitted_plot <- data_i %>% ggplot() +
    geom_point(aes(x=fitted, y=hyd_destination)) +
    xlim(0, 1)
  ggsave(paste0("Colombia Data/Figs/id=5353 fitted plot model", i, ".png"), fitted_plot)
}

glm_bw_plot <- function(model, model_num, bw) {
  data_ <- model$data
  data_$fitted <- model$fitted.values
  fitted_plot <- data_ %>% ggplot() +
    geom_point(aes(x=fitted, y=hyd_destination)) +
    xlim(0, 1)
  ggsave(paste0("Colombia Data/Figs/id=5353 fitted plots/bw=", bw, " fitted plot model", model_num, ".png"), fitted_plot)
}

id_5353_model2 <- list()
id_5353_model3 <- list()
id_5353_model4 <- list()
id_5353_model5 <- list()
id_5353_model6 <- list()
for (bw in seq(0.5, 2.0, 0.1)) {
  neighbors_bw <- gwr_data_hyd_destination %>% 
    filter(id %in% coord_unique$id[which(local_gwr_dist[id_i_index,] <= bw)]) %>% 
    select(-(id:year))
  model2 <- glm(hyd_destination~.,
                data = neighbors_bw %>%
                  select(hyd_destination, hyd_lab_prob, hyd_price_distance),
                family = binomial)
  model3 <- glm(hyd_destination~.,
                data = neighbors_bw %>%
                  select(hyd_destination, hyd_lab_prob, hyd_price_distance, coca_distance),
                family = binomial)
  model4 <- glm(hyd_destination~.,
                data = neighbors_bw %>%
                  select(hyd_destination, hyd_lab_prob, hyd_price_distance, coca_distance, armed_group),
                family = binomial)
  model5 <- glm(hyd_destination~.,
                data = neighbors_bw %>%
                  select(hyd_destination, hyd_lab_prob, hyd_price_distance, coca_distance, armed_group, road_length),
                family = binomial)
  model6 <- glm(hyd_destination~.,
                data = neighbors_bw %>%
                  select(hyd_destination, hyd_lab_prob, hyd_price_distance, coca_distance, armed_group, road_length, population),
                family = binomial)
  id_5353_model2[[paste0("bw_", bw)]] <- model2
  id_5353_model3[[paste0("bw_", bw)]] <- model3
  id_5353_model4[[paste0("bw_", bw)]] <- model4
  id_5353_model5[[paste0("bw_", bw)]] <- model5
  id_5353_model6[[paste0("bw_", bw)]] <- model6
  glm_bw_plot(model2, 2, bw)
  glm_bw_plot(model3, 3, bw)
  glm_bw_plot(model4, 4, bw)
  glm_bw_plot(model5, 5, bw)
  glm_bw_plot(model6, 6, bw)
}

next_best_reg <- glm(hyd_destination~., data = prev_data %>% select(-population), family = binomial)
summary(next_best_reg)
data_1 <- next_best_reg$data
data_1$fitted <- next_best_reg$fitted.values
data_1 %>% ggplot() +
  geom_point(aes(x=fitted, y=hyd_destination)) +
  xlim(0, 1)

alpha <- 0.8
weighted_glm <- function (alpha) {
  weighted_reg <- glm(hyd_destination~., data = prev_data %>% select(-population), family = binomial,
                      weights = ifelse(prev_data$hyd_destination == 1, alpha, 1-alpha))
  print(summary(weighted_reg))
  data_2 <- weighted_reg$data
  data_2$fitted <- weighted_reg$fitted.values
  data_2 %>% ggplot() +
    geom_point(aes(x=fitted, y=hyd_destination)) +
    xlim(0, 1)
}
weighted_glm(0.5)
weighted_glm(0.6)
weighted_glm(0.7)
weighted_glm(0.8)
weighted_glm(0.9)
weighted_glm(0.92)
weighted_glm(0.95)
weighted_glm(0.99)

map_df %>% mutate(id=ifelse(id == 5353, 1, 0) %>% as.factor) %>% 
  ggplot(aes(x=long, y=lat, fill=id)) + 
  geom_polygon(aes(group=group),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  scale_fill_manual(values=c("1"="red", "0"="white")) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )
