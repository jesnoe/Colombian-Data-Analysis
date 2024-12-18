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
########## used bandwidth range 0.5~3.0 due to the time limit
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

cv.auc_mat <- read.csv("Colombia Data/local GWR lasso rescaled cv AUC (12-03-2024).csv") %>% as_tibble
local_GWR_coefs_bw_lasso <- read.csv("Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv") %>% as_tibble
local_gwr_forward_coefs <- read.csv("Colombia Data/local GWR best coefs forward selection (10-29-2024).csv") %>% as_tibble
load("Colombia Data/local GWR lasso rescaled result (12-03-2024).RData")

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

### use regression_data_years to make hyd_destination is 1 if a municipio was destination in at least 2 years
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

municipios_sf <- st_as_sf(municipios) %>% mutate(id = id %>% as.numeric) %>% filter(!(id %in% c(88001, 88564)))
municipios_sf$area_km2 <- st_area(municipios_sf) %>% units::set_units("km^2") %>% as.numeric
gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination %>% 
  left_join(municipios_sf %>% as_tibble %>% select(id, area_km2), by="id") %>% 
  mutate(coca_area = coca_area / area_km2,
         river_length = river_length / area_km2,
         road_length = road_length / area_km2,
         hyd_destination = hyd_destination %>% as.character %>% as.integer) %>% 
  select(-area_km2)

gwr_data_hyd_destination_by_area <- gwr_data_hyd_destination_by_area %>% 
  mutate(across(PPI_lab_prob:population, ~ scale(.x)[,1]))

gwr_data_corr <- cor(gwr_data_hyd_destination_by_area %>%
                       select(-(id:year)) %>%
                       relocate(hyd_destination)) %>% melt
min_corr <- min(gwr_data_corr$value)
gwr_data_corr %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                       values = scales::rescale(c(min_corr, -.Machine$double.eps, 0 , .Machine$double.eps, 1)),
                       na.value = "white") +
  labs(title="Correlation Matrix", x="", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

bwd_range <- seq(0.5, 3, by=0.1)
local_gwr_data_id <- gwr_data_hyd_destination_by_area %>% select(id)
local_gwr_data <- gwr_data_hyd_destination_by_area %>% select(-id, -municipio, -year)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)


## reg coefs with LASSO (bandwidths are optimized by "AUC")


cv.auc <- aic_score_mat %>% select(-(bw_3.1:bw_4))
local_GWR_coefs_lasso_result <- list()
set.seed(100)
for (i in 1:nrow(coord_unique)) {
  # for (i in 541:nrow(coord_unique)) {
  id_i <- coord_unique$id[i]
  local_GWR_coefs_lasso_result[[paste0("id_", id_i)]] <- list()
  
  for (j in 1:length(bwd_range)) {
    bw_ij <- bwd_range[j]
    neighbors_ij <- gwr_data_hyd_destination_by_area %>% 
      filter(id %in% coord_unique$id[which(local_gwr_dist[i,] <= bw_ij)]) %>% 
      select(-(id:year))
    
    x_mat <- neighbors_ij %>% select(-hyd_destination) %>% as.matrix
    y_vec <- neighbors_ij$hyd_destination
    n_obs <- nrow(neighbors_ij)
    n0 <- sum(y_vec == 0)
    n1 <- sum(y_vec)
    y_ratio <- n1/n_obs
    
    if (n_obs < 30) {
      cv.auc[[paste0("bw_", bw_ij)]][i] <- NA
      local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      next
    }
    
    if ((n0 < 3 | n1 < 3) | (y_ratio < 0.1 | y_ratio > 0.9)) {
      cv.auc[[paste0("bw_", bw_ij)]][i] <- NA
      local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      next
    }
    
    lambda_fit_auc_ij <- cv.glmnet(x = x_mat,
                                   y = y_vec,
                                   family = "binomial",
                                   type.measure = "auc")
    lambda_lasso <- lambda_fit_auc_ij$lambda.min
    cv.auc_ij <- lambda_fit_auc_ij$cvm[lambda_fit_auc_ij$index[1]]
    
    
    lasso_result_ij <- glmnet(x = x_mat,
                              y = y_vec,
                              family = "binomial",
                              alpha = 1,
                              lambda = lambda_lasso)
    
    n_nonzero <- sum(lasso_result_ij$beta != 0)
    if (n_nonzero == 0) {
      cv.auc[[paste0("bw_", bw_ij)]][i] <- NA
      local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      next
    }
    cv.auc[[paste0("bw_", bw_ij)]][i] <- cv.auc_ij
    local_GWR_coefs_lasso_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- lasso_result_ij
  }
  if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
}

# write.csv(cv.auc, "Colombia Data/local GWR lasso rescaled cv AUC (12-03-2024).csv", row.names = F)
# save("local_GWR_coefs_lasso_result", file = "Colombia Data/local GWR lasso rescaled result (12-03-2024).RData")
length(local_GWR_coefs_lasso_result)

optimal_bw <- gsub("bw_", "",
                   cv.auc_mat[,-1] %>% apply(1, function(x) ifelse(sum(!is.na(x)) == 0, NA, bwd_range[which.max(x)]))
                   ) %>% as.numeric


## coef maps with LASSO
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

coef_template <- local_gwr_forward_coefs[1,]
local_GWR_coefs_bw_lasso <- local_gwr_forward_coefs
local_GWR_coefs_bw_lasso$bw <- optimal_bw
local_GWR_coefs_bw_lasso_mat <- local_GWR_coefs_bw_lasso[,-(1:2)] %>% as.matrix
var_table <- tibble(var_name=names(local_GWR_coefs_bw_lasso)[-(1:2)])
for (i in 1:nrow(local_GWR_coefs_bw_lasso)) {
  id_i <- local_GWR_coefs_bw_lasso$id[i]
  bw_i <- local_GWR_coefs_bw_lasso$bw[i]
  id_name_i <- paste0("id_", id_i)
  bw_name_i <- paste0("bw_", bw_i)
  local_GWR_lasso_i <- local_GWR_coefs_lasso_result[[id_name_i]][[bw_name_i]]
  if (is.na(bw_i) | is.na(local_GWR_lasso_i)[1]) {
    local_GWR_coefs_bw_lasso_mat[i,] <- rep(NA, 13)
    next
  }
  
  coefs_i <- tibble(var_name = c("Intercept", row.names(local_GWR_lasso_i$beta)),
                    coef = c(local_GWR_lasso_i$a0, local_GWR_lasso_i$beta[,1]))
  coefs_i <- left_join(var_table, coefs_i, by="var_name")
  local_GWR_coefs_bw_lasso_mat[i,] <- coefs_i$coef
  if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
}

local_GWR_coefs_bw_lasso[, -(1:2)] <- local_GWR_coefs_bw_lasso_mat
# write.csv(local_GWR_coefs_bw_lasso, "Colombia Data/local GWR lasso coefs rescaled (12-03-2024).csv", row.names=F)

for (i in 4:length(local_GWR_coefs_bw_lasso)) {
  var_name <- names(local_GWR_coefs_bw_lasso)[i]
  gwr_coefs_i <- data.frame(id=local_GWR_coefs_bw_lasso$id,
                            coef=local_GWR_coefs_bw_lasso[[var_name]],
                            rounded_coef=local_GWR_coefs_bw_lasso[[var_name]] %>% round(2))
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
  
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso rescaled/hyd destination GWR lasso coef ",
                var_name, ".png"),
         gwr_coef_map, scale=1)
}

gwr_coefs_i <- data.frame(id=local_GWR_coefs_bw_lasso$id,
                          coef=local_GWR_coefs_bw_lasso$bw)
coef_map_coords <- map_df %>% 
  left_join(gwr_coefs_i, by="id")

gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=coef),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = depto_map$long, y = depto_map$lat) + 
  coord_quickmap() +
  scale_fill_viridis_c(na.value = "white") +
  labs(fill="bandwidth", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

ggsave("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR lasso rescaled/hyd destination GWR lasso bandwidth.png",
       gwr_coef_map, scale=1)


##