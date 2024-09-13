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
load("Colombia Data/local GWR result (08-13-2024).RData")
aic_score_mat <- read.csv("Colombia Data/local GWR AIC with variable selection (09-04-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result with variable selection (09-04-2024).RData")

regression_data_years <- read.csv("Colombia Data/regression data all municipios (07-05-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

# regression_data_years_old <- read.csv("Colombia Data/regression data (05-15-2024).csv") %>% as_tibble %>% 
#   left_join(airports, by="id") %>% 
#   mutate(base_avg=scale(base_avg)[,1],
#          paste_avg=scale(paste_avg)[,1],
#          hyd_avg=scale(hyd_avg)[,1],
#          airport=ifelse(n_airports > 0, 1, 0))
# 
# regression_data_years %>% filter(!(id %in% regression_data_years_old$id)) %>% view

ever_anecdotal <- regression_data_years %>% 
  group_by(id) %>% 
  summarise(base_source=ifelse(sum(base_source)>0, 1, 0),
            base_destination=ifelse(sum(base_destination)>0, 1, 0),
            hyd_source=ifelse(sum(hyd_source)>0, 1, 0),
            hyd_destination=ifelse(sum(hyd_destination)>0, 1, 0))
# ever_anecdotal_516 <- regression_data_years_old %>% 
#   group_by(id) %>% 
#   summarise(base_source=ifelse(sum(base_source)>0, 1, 0),
#             base_destination=ifelse(sum(base_destination)>0, 1, 0),
#             hyd_source=ifelse(sum(hyd_source)>0, 1, 0),
#             hyd_destination=ifelse(sum(hyd_destination)>0, 1, 0))
# 
# ever_anecdotal_516[,-1] %>% apply(2, sum)
# ever_anecdotal[,-1] %>% apply(2, sum)


ever_anecdotal_data_years <- regression_data_years %>% 
  select(-(base_source_all:general_destination)) %>% 
  left_join(ever_anecdotal, by="id")
# 
# ever_anecdotal %>% select(-id) %>% apply(2, sum) # 541 municipalities
# ever_anecdotal_data_years %>% select(base_source:hyd_destination) %>% apply(2, sum) # 1,623 rows

# filter out departments without anecdotal evidence
# anecdotal_id_depto <- ever_anecdotal_data_years %>% 
#   left_join(municipios_capital %>% select(id, id_depto), by="id") %>% 
#   group_by(id_depto) %>% 
#   summarise(hyd_source=sum(hyd_source),
#             hyd_destination=sum(hyd_destination)) %>% 
#   mutate(anecdotal=hyd_source+hyd_destination>0) %>% 
#   filter(anecdotal) %>% pull(id_depto)
# ever_anecdotal_data_years <- ever_anecdotal_data_years %>% 
#   left_join(municipios_capital %>% select(id, id_depto), by="id") %>% 
#   filter(id_depto %in% anecdotal_id_depto)

gwr_hyd_destination_coord <- left_join(ever_anecdotal_data_years %>%
                                         select(id, year, n_PPI_labs:population, airport, hyd_destination, -base_avg, -base_price_distance,
                                                -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group, -erad_aerial),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)

coord_unique <- gwr_hyd_destination_coord %>% select(id, long, lat) %>% unique
gwr_hyd_destination_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T)
dim(gwr_hyd_destination_dist)
gwr_data_hyd_destination <- gwr_hyd_destination_coord %>% select(-long, -lat, -erad_manual)

bwd_range <- seq(0.5, 8, by=0.1)
y_ratio_mat <- matrix(nrow=nrow(coord_unique), ncol=length(bwd_range), data = NA) %>% as_tibble() %>% 
  mutate(id=coord_unique$id) %>% 
  relocate(id)
names(y_ratio_mat)[-1] <- paste0("bw_", bwd_range) 
n_reg_data_mat <- matrix(nrow=nrow(coord_unique), ncol=length(bwd_range), data = 0) %>% as_tibble() %>% 
  mutate(id=coord_unique$id) %>% 
  relocate(id)
names(n_reg_data_mat)[-1] <- paste0("bw_", bwd_range) 
local_gwr_data_id <- gwr_hyd_destination_coord %>% select(id)
local_gwr_data <- gwr_hyd_destination_coord %>% select(-id, -municipio, -year, -long, -lat)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)
# number of neigbors and response ratio
for(i in 1:length(bwd_range)) {
  bw <- bwd_range[i]
  n_neighbors_i <- local_gwr_dist %>%
    apply(1, function(x) which(x<=bw)) %>%
    lapply(function(x) local_gwr_data_id %>% filter(id %in% coord_unique$id[x]) %>% nrow) %>% unlist
  n_reg_data_mat[, i+1] <- n_neighbors_i - 1
  
  y_ratio__i <- local_gwr_dist %>%
    apply(1, function(x) which(x<=bw)) %>%
    lapply(function(x) sum((gwr_hyd_destination_coord %>% filter(id %in% coord_unique$id[x]) %>% pull(hyd_destination) == 1))) %>% unlist
  y_ratio__i <- y_ratio__i / n_neighbors_i
  y_ratio_mat[, i+1] <- y_ratio__i
}
ratio_LB <- 0.2
ratio_UB <- 0.8
y_ratio_mat$n_bw_within <- apply(y_ratio_mat, 1, function(x) sum(x[-1] >= ratio_LB & x[-1] <= ratio_UB))
y_ratio_mat <- y_ratio_mat %>% relocate(id, n_bw_within)
# write.csv(n_reg_data_mat, "Colombia Data/local GWR number of neighbors bw 8 (09-12-2024).csv", row.names=F)
# write.csv(y_ratio_mat, "Colombia Data/local GWR response ratio (09-12-2024).csv", row.names=F)
# write.csv(y_ratio_mat, "Colombia Data/local GWR response ratio bw 8 (09-12-2024).csv", row.names=F)
n_reg_data_mat <- read.csv("Colombia Data/local GWR number of neighbors (08-13-2024).csv") %>% as_tibble
y_ratio_mat <- read.csv("Colombia Data/local GWR response ratio (09-12-2024).csv") %>% as_tibble
# y_ratio_mat <- read.csv("Colombia Data/local GWR response ratio bw 8 (09-12-2024).csv") %>% as_tibble
  
y_ratio_mat %>% arrange(n_bw_within)
y_ratio_mat %>% filter(n_bw_within == 0)
zero_hyd_destination_id <- y_ratio_mat %>% filter(n_bw_within == 0) %>% pull(id)
municipios_capital %>% filter(id %in% zero_hyd_destination_id)
y_ratio_mat %>% filter(id %in% c(41078, 85225))

ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group),
               fill = ifelse(map_df$id %in% zero_hyd_destination_id, "red", "white"),
               color = "black",
               linewidth = 0.1) + 
  expand_limits(x = map_df$long, y = map_df$lat) + 
  coord_quickmap() +
  # scale_fill_manual(na.value = "white") +
  labs(fill="no hyd destination", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

for (i in 1:nrow(local_GWR_coefs_bw)) {
  id_i <- local_GWR_coefs_bw$id[i]
  bw_i <- local_GWR_coefs_bw$bw[i]
}
paste0("bw_", local_GWR_coefs_bw$bw)
local_GWR_coefs_bw$y_ratio <- apply(local_GWR_coefs_bw, 1,
                                    function(x) y_ratio_mat[[paste0("bw_", x[2])]][coord_unique == x[1]])
local_GWR_coefs_bw <- local_GWR_coefs_bw %>% relocate(id, bw, y_ratio)
# write.csv(local_GWR_coefs_bw, "Colombia Data/local GWR best coefs with y_ratio (09-12-2024).csv", row.names=F)

local_GWR_coefs_bw %>% filter(y_ratio > 0.8 | y_ratio < 0.2) # 558 municipios out of 1120 (49.8%)


# balanced optimal GWR regression
# bw=bwd_range[aic_score_mat[,-1] %>% apply(1, function(x) which.min(ifelse(x<35, Inf, x)))]
bwd_range <- seq(0.5, 4, by=0.1)
local_GWR_balanced_coefs_bw <- tibble(id=aic_score_mat$id)
balanced_optimal_bw <- c()
for (i in 1:nrow(local_GWR_balanced_coefs_bw)) {
  AIC_i <- aic_score_mat[i, -1]
  y_ratio_i <- y_ratio_mat[i,]
  if (y_ratio_i$n_bw_within == 0) {
    balanced_optimal_bw <- c(balanced_optimal_bw, 4)
    next
  }
  AIC_i <- t(AIC_i) %>% as.vector
  y_ratio_i <- t(y_ratio_i[, -(1:2)]) %>% as.vector
  # AIC_i <- ifelse(AIC_i < 35 | y_ratio_i > ratio_UB | y_ratio_i < ratio_LB, Inf, AIC_i) # for imbalanced
  AIC_i <- ifelse(y_ratio_i > ratio_UB | y_ratio_i < ratio_LB, Inf, AIC_i) # for balanced
  balanced_optimal_bw <- c(balanced_optimal_bw, bwd_range[which.min(AIC_i)])
}
local_GWR_balanced_coefs_bw$bw <- balanced_optimal_bw
local_GWR_balanced_coefs_bw$n_neighbors <- local_GWR_balanced_coefs_bw %>% apply(1, function(x) n_reg_data_mat[[paste0("bw_", x[2])]][which(x[1] == local_GWR_balanced_coefs_bw$id)])
local_GWR_balanced_coefs_mat <- matrix(0, nrow(local_GWR_balanced_coefs_bw), ncol(local_gwr_data))
for(i in 1:nrow(local_GWR_balanced_coefs_bw)) {
  id_i <- local_GWR_balanced_coefs_bw$id[i]
  bw_i <- local_GWR_balanced_coefs_bw$bw[i]
  # local_GWR_balanced_coefs_mat[i,] <- model_coef_list[[paste0("coefs_", id_i)]][[paste0("bw_", bw_i)]]
  local_GWR_balanced_coefs_mat[i,] <- model_vs_coef_list[[paste0("coefs_", id_i)]][[paste0("bw_", bw_i)]]
}

local_GWR_balanced_coefs_mat <- as_tibble(local_GWR_balanced_coefs_mat)
# names(local_GWR_balanced_coefs_mat) <- model_coef_list$coefs_5002$var_name
names(local_GWR_balanced_coefs_mat) <- model_vs_coef_list$coefs_5002$var_name
local_GWR_balanced_coefs_mat$n_NA <- local_GWR_balanced_coefs_mat %>% apply(1, function(x) sum(is.na(x)))
local_GWR_balanced_coefs_mat <- local_GWR_balanced_coefs_mat %>% relocate(n_NA)
local_GWR_balanced_coefs_bw <- cbind(local_GWR_balanced_coefs_bw, local_GWR_balanced_coefs_mat) %>% as_tibble
# local_GWR_balanced_coefs_bw %>% write.csv("Colombia Data/local GWR balanced best coefs (09-12-2024).csv", row.names=F)


local_gwr_data_with_id %>% filter(id %in% c(5642, 94888))
local_gwr_data_with_id <- cbind(local_gwr_data_id, local_gwr_data) %>% as_tibble
pi_hat <- c()
for (i in 1:nrow(local_gwr_data_with_id)) {
  id_i <- local_gwr_data_with_id$id[i]
  beta_i <- local_GWR_balanced_coefs_bw %>% filter(id == id_i) %>% select(-(id:n_NA))
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

# outliers
gwr_result_list$id_41078$bw_1.1$data %>% view
gwr_result_list$id_85225$bw_0.8$data %>% view

gwr_result_list$id_41078$bw_1.1$data$hyd_destination %>% table
gwr_result_list$id_85225$bw_0.8$data$hyd_destination %>% table

local_gwr_model_41078 <- glm(hyd_destination~.,
                             data=gwr_result_list$id_41078$bw_1.1$data,
                             family=binomial)
local_gwr_model_85225 <- glm(hyd_destination~.,
                             data=gwr_result_list$id_85225$bw_0.8$data,
                             family=binomial)

local_gwr_model_41078_vs <- MASS::stepAIC(local_gwr_model_41078, trace=F, direction="both")
local_gwr_model_85225_vs <- MASS::stepAIC(local_gwr_model_85225, trace=F, direction="both")
summary(local_gwr_model_41078_vs)
summary(local_gwr_model_85225_vs)

# coef maps
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

for (i in 1:ncol(local_GWR_balanced_coefs_bw)) {
  if (i %in% c(1, 3, 4, 5)) next
  var_name <- names(local_GWR_balanced_coefs_bw)[i]
  gwr_coefs_i <- data.frame(id=local_GWR_balanced_coefs_bw$id,
                            coef=local_GWR_balanced_coefs_bw[[var_name]])
  
  coef_map_coords <- map_df %>% 
    left_join(gwr_coefs_i, by="id")
  
  
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
  
  # ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR balanced results/hyd destination GWR balanced coef ", var_name, ".png"),
  #        gwr_coef_map, scale=1)
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR balanced results (variable selection)/hyd destination GWR balanced coef ",
                var_name, "(variable selection).png"),
         gwr_coef_map, scale=1)
}


local_GWR_coefs_bw_without_41078 <- local_GWR_coefs_bw %>% filter(id != 41078)
for (i in 1:ncol(local_GWR_coefs_bw_without_41078)) {
  if (i %in% c(1, 3, 4, 5)) next
  var_name <- names(local_GWR_coefs_bw_without_41078)[i]
  gwr_coefs_i <- data.frame(id=local_GWR_coefs_bw_without_41078$id,
                            coef=local_GWR_coefs_bw_without_41078[[var_name]])
  
  coef_map_coords <- map_df %>% 
    left_join(gwr_coefs_i, by="id")
  
  
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
  
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR results without id=41078/hyd destination GWR coef ", var_name, ".png"),
         gwr_coef_map, scale=1)
}

empty_map <- ggplot(depto_map, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill = ""),
               color = "black",
               linewidth = 0.1,
               fill="white") + 
  expand_limits(x = depto_map$long, y = depto_map$lat) + 
  coord_quickmap() +
  scale_fill_manual(values="", na.value = "") +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

empty_map +
  geom_point(data=gwr_hyd_destination_coord %>% filter(airport == 1),
             aes(x=long, y=lat),
             size=0.5) +
  ggtitle("airports")

hyd_destination_map_data <- 
  gwr_hyd_destination_coord %>% 
  select(id, hyd_destination) %>% 
  left_join(municipio_centroid %>% select(id, long, lat), by="id") %>% 
  mutate(posterior_glm=pi_hat_bw)

threshold <- 0.5
hyd_gwr_destination_pred_map <- empty_map +
  geom_point(data=hyd_destination_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_glm < threshold, 2*(threshold-posterior_glm), 2*(posterior_glm-threshold)),
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="GWR Predictions (hyd Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") +
  theme(legend.position="right")
# ggsave("Colombia Data/Figs/prediction map/hyd destinations gwr pred (2013-2016).png", hyd_gwr_destination_pred_map, scale=1)

empty_map +
  geom_point(data=data.frame(long=c(-75.6, -73.6), lat=c(6.26, 6.26)),
             aes(x=long, y=lat),
             color=c("black", "red"))