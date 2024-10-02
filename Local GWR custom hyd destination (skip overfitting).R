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

# Finding the optimal GWR regression without overfitting
bwd_range <- seq(0.5, 4, by=0.1)
sig_level <- 0.05
local_GWR_no_overfitting_coefs_bw <- local_GWR_coefs_bw
local_GWR_no_overfitting_pvalues_bw <- local_GWR_coefs_bw
for (i in 1:nrow(local_GWR_no_overfitting_coefs_bw)) {
  id_i <- local_GWR_no_overfitting_coefs_bw$id[i]
  AIC_i <- aic_score_mat %>% filter(id == id_i) %>% select(-id)
  local_gwr_models_i <- gwr_result_list[[paste0("id_", id_i)]]
  
  for (j in 1:length(local_gwr_models_i)) {
    bw_name_ij <- names(local_gwr_models_i)[j]
    AIC_i_bw <- AIC_i[[bw_name_ij]]
    local_gwr_models_i_bw <- local_gwr_models_i[[j]]
    local_gwr_models_i_bw_summary <- local_gwr_models_i_bw %>% summary
    p_vals_i_bw <- local_gwr_models_i_bw_summary$coefficients[,4][-1]
    
    if (sum(p_vals_i_bw <= sig_level) == 0) {
      AIC_i[[bw_name_ij]] <- Inf # Overfitting if all variables are insignificant. Make AIC Inf
      next
    }
    
    if (p_vals_i_bw %>% round(1) %>% table %>% length == 1) {
      AIC_i[[bw_name_ij]] <- Inf # Overfitting if p-values are identical for all variables. Make AIC Inf
    }
  }
  
  AIC_i <- t(AIC_i) %>% as.vector
  optimal_reg_index <- which.min(AIC_i)
  optimal_reg_bw <- bwd_range[optimal_reg_index]
  bw_name_i <- paste0("bw_", optimal_reg_bw)
  local_GWR_no_overfitting_coefs_bw$bw[i] <- optimal_reg_bw
  local_GWR_no_overfitting_coefs_bw$n_neighbors[i] <- n_reg_data_mat %>% filter(id == id_i) %>% pull(bw_name_i)
  coef_i <- local_gwr_models_i[[bw_name_i]]$coefficients
  coef_i_df <- data.frame(var_name=names(coef_i), coef=coef_i)
  p_values_i <- summary(local_gwr_models_i[[bw_name_i]])$coefficients[,4]
  p_value_df <- data.frame(var_name=names(p_values_i), p_value=p_values_i)
  coef_p_value_df <- left_join(coef_i_df, p_value_df, by="var_name")
  for (var_idx in 1:length(coef_i)) {
    local_GWR_no_overfitting_coefs_bw[i, var_idx+4] <- coef_p_value_df$coef[var_idx]
    local_GWR_no_overfitting_pvalues_bw[i, var_idx+4] <- coef_p_value_df$p_value[var_idx]
  }
  local_GWR_no_overfitting_coefs_bw$n_NA[i] <- sum(is.na(coef_i[-1]))
  
}

local_GWR_no_overfitting_coefs_bw <- local_GWR_no_overfitting_coefs_bw %>% 
  mutate(sum_coefs=local_GWR_no_overfitting_coefs_bw %>% apply(1, function(x) sum(abs(x[6:21]), na.rm=T))) %>% 
  relocate(id, bw, n_neighbors, n_NA, sum_coefs)
local_GWR_no_overfitting_pvalues_bw <- local_GWR_no_overfitting_pvalues_bw %>% 
  mutate(sum_coefs = local_GWR_no_overfitting_coefs_bw$sum_coefs)
# local_GWR_no_overfitting_coefs_bw %>% write.csv("Colombia Data/local GWR no overfitting best coefs (10-01-2024).csv", row.names=F)
# local_GWR_no_overfitting_pvalues_bw %>% write.csv("Colombia Data/local GWR no overfitting best p-values (10-01-2024).csv", row.names=F)
local_GWR_no_overfitting_pvalues_bw %>% filter(id == 85225)


local_gwr_data_with_id <- cbind(local_gwr_data_id, local_gwr_data) %>% as_tibble
pi_hat <- c()
for (i in 1:nrow(local_gwr_data_with_id)) {
  id_i <- local_gwr_data_with_id$id[i]
  beta_i <- local_GWR_no_overfitting_coefs_bw %>% filter(id == id_i) %>% select(-(id:n_NA))
  intercept_i <- beta_i$Intercept
  beta_i <- beta_i[,-1] %>% as.matrix %>% t
  beta_i[is.na(beta_i)] <- 0
  X_beta_i <- intercept_i + as.matrix(local_gwr_data %>% select(-hyd_destination))[i,] %*% beta_i
  pi_hat_i <- exp(X_beta_i)/(1+exp(X_beta_i))
  pi_hat_i <- ifelse(is.nan(pi_hat_i), 1, pi_hat_i)
  pi_hat <- c(pi_hat, pi_hat_i)
}

local_gwr_data_with_id$pi_hat <- pi_hat
threshold <- 0.5
local_gwr_data_with_id$pred <- ifelse(local_gwr_data_with_id$pi_hat < threshold, 0, 1) %>% as.factor
confusionMatrix(local_gwr_data_with_id$pred,
                local_gwr_data_with_id$hyd_destination %>% as.factor,
                positive="1")

local_gwr_data_roc <- roc(local_gwr_data_with_id$hyd_destination, local_gwr_data_with_id$pred %>% as.character %>% as.numeric)
auc(local_gwr_data_roc) # 0.8994


# coef maps
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

for (i in 1:ncol(local_GWR_no_overfitting_coefs_bw)) {
  if (i %in% c(1, 3, 4, 5, 6)) next
  var_name <- names(local_GWR_no_overfitting_coefs_bw)[i]
  gwr_coefs_i <- data.frame(id=local_GWR_no_overfitting_coefs_bw$id,
                            coef=local_GWR_no_overfitting_coefs_bw[[var_name]],
                            rounded_coef=local_GWR_no_overfitting_coefs_bw[[var_name]] %>% round(2),
                            p_value=local_GWR_no_overfitting_pvalues_bw[[var_name]])
  min_coef <- min(gwr_coefs_i$coef, na.rm=T)
  max_coef <- max(gwr_coefs_i$coef, na.rm=T)
  coef_map_coords <- map_df %>% 
    left_join(gwr_coefs_i, by="id")
  
  if (i==2) {
    gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
      geom_polygon(aes(group=group, fill=coef),
                   color = "black",
                   linewidth = 0.1) + 
      expand_limits(x = depto_map$long, y = depto_map$lat) + 
      coord_quickmap() +
      scale_fill_viridis_c(na.value = "white") +
      labs(fill=var_name, x="", y="", title="") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank()
      )
  }else{
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
  } 
  
  gwr_pval_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=p_value),
                 color = "black",
                 linewidth = 0.1) + 
    expand_limits(x = depto_map$long, y = depto_map$lat) + 
    coord_quickmap() +
    scale_fill_gradientn(colors = c("red", "red", "skyblue","blue"),
                         values = scales::rescale(c(0,  sig_level, sig_level+.Machine$double.eps, 1)),
                         na.value = "white") +
    labs(fill=var_name, x="", y="", title="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR without overfitting/hyd destination GWR without overfitting coef ",
                var_name, ".png"),
         gwr_coef_map, scale=1)
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR without overfitting/p-values/hyd destination GWR without overfitting p-value ",
                var_name, ".png"),
         gwr_pval_map, scale=1)
}
