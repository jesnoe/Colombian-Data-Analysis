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
n_reg_data_mat <- read.csv("Colombia Data/local GWR number of neighbors (08-13-2024).csv") %>% as_tibble
aic_score_vs_mat <- n_reg_data_mat
local_gwr_data_id <- gwr_hyd_destination_coord %>% select(id)
local_gwr_data <- gwr_hyd_destination_coord %>% select(-id, -municipio, -year, -long, -lat)
local_gwr_dist <- as.matrix(gwr_hyd_destination_dist)

# Local GWR with variable selection (stepwise regression)
# GWR coefficients and AIC
response_var <- "hyd_destination"
min_n_reg_data <- 34
gwr_vs_result_list <- list()
model_vs_coef_list <- list()
for (i in 1:nrow(coord_unique)) {
  id_i <- coord_unique$id[i]
  model_coef_mat_i <- matrix(nrow=1+length(local_gwr_data), ncol=length(bwd_range), data = NA) %>% as_tibble() %>%
    mutate(var_name=c("Intercept", names(local_gwr_data))) %>%
    filter(var_name != response_var) %>%
    relocate(var_name)
  names(model_coef_mat_i)[-1] <- paste0("bw_", bwd_range)
  gwr_vs_result_list[[paste0("id_", id_i)]] <- list()
  var_name <- model_coef_mat_i %>% select(var_name)
  
  for(j in 1:length(bwd_range)) {
    bw_j <- bwd_range[j]
    n_reg_data_i_bw <- (n_reg_data_mat %>% filter(id == id_i))[[paste0("bw_", bw_j)]]
    if (n_reg_data_i_bw < min_n_reg_data) {
      aic_score_vs_mat[i, j+1] <- NA
      next
    }
    
    neighbor_dist_index <- which(local_gwr_dist[i,] <= bw_j)
    neighbor_id_i <- coord_unique$id[neighbor_dist_index]
    neighbor_dist <- tibble(id=neighbor_id_i, dist=local_gwr_dist[i,neighbor_dist_index])
    neighbor_index <- which(local_gwr_data_id$id %in% coord_unique$id[neighbor_dist_index])
    dist_weights_df <- left_join(local_gwr_data_id, neighbor_dist, by="id")
    
    local_gwr_data_bw_i <- local_gwr_data[neighbor_index,]
    
    gwr_result_bw_i <- glm(hyd_destination~.,
                           data=local_gwr_data_bw_i,
                           # weight=1/(1+local_gwr_data_bw_i[i,neighbor_index]),
                           family="binomial")
    gwr_vs_result_bw_i <- MASS::stepAIC(gwr_result_bw_i, trace=F, direction="both")
    
    model_var_name <- tibble(var_name=names(gwr_vs_result_bw_i$coefficients), coefficients=gwr_vs_result_bw_i$coefficients)
    model_coef_i <- left_join(var_name, model_var_name, by="var_name") %>% pull(coefficients)
    model_coef_mat_i[,j+1] <- model_coef_i
    aic_score_vs_mat[i, j+1] <- gwr_vs_result_bw_i$aic
    gwr_vs_result_list[[i]][[paste0("bw_", bw_j)]] <- gwr_result_bw_i
  }
  
  model_vs_coef_list[[paste0("coefs_", id_i)]] <- model_coef_mat_i
}

# write.csv(aic_score_vs_mat, "Colombia Data/local GWR AIC with variable selection (09-02-2024).csv", row.names=F)
# save(list = c("gwr_vs_result_list", "model_vs_coef_list"), file = "Colombia Data/local GWR result with variable selection (09-02-2024).RData")
aic_score_vs_mat <- read.csv("Colombia Data/local GWR AIC with variable selection (09-02-2024).csv") %>% as_tibble
local_GWR_vs_coefs_bw <- read.csv("Colombia Data/local GWR best coefs with variable selection (09-02-2024).csv") %>% as_tibble
load("Colombia Data/local GWR result with variable selection (09-02-2024).RData")

# n_reg_data_mat
local_GWR_vs_coefs_bw <- tibble(id=aic_score_vs_mat$id,
                             bw=bwd_range[aic_score_vs_mat[,-1] %>% apply(1, function(x) which.min(ifelse(x<35, Inf, x)))])
local_GWR_vs_coefs_bw$n_neighbors <- local_GWR_vs_coefs_bw %>% apply(1, function(x) n_reg_data_mat[[paste0("bw_", x[2])]][which(x[1] == local_GWR_vs_coefs_bw$id)])
local_GWR_coefs_mat <- matrix(0, nrow(local_GWR_vs_coefs_bw), ncol(local_gwr_data))
for(i in 1:nrow(local_GWR_vs_coefs_bw)) {
  id_i <- local_GWR_vs_coefs_bw$id[i]
  bw_i <- local_GWR_vs_coefs_bw$bw[i]
  local_GWR_coefs_mat[i,] <- model_coef_list[[paste0("coefs_", id_i)]][[paste0("bw_", bw_i)]]
}
local_GWR_coefs_mat <- as_tibble(local_GWR_coefs_mat)
names(local_GWR_coefs_mat) <- model_coef_list$coefs_5002$var_name
local_GWR_coefs_mat$n_NA <- local_GWR_coefs_mat %>% apply(1, function(x) sum(is.na(x)))
local_GWR_coefs_mat <- local_GWR_coefs_mat %>% relocate(n_NA)
local_GWR_vs_coefs_bw <- cbind(local_GWR_vs_coefs_bw, local_GWR_coefs_mat) %>% as_tibble
# local_GWR_vs_coefs_bw %>% write.csv("Colombia Data/local GWR best coefs with variable selection (09-02-2024).csv", row.names=F)

local_gwr_data_with_id %>% filter(id %in% c(5642, 94888))
gwr_result_list$id_5642$bw_0.7 %>% summary

local_gwr_data_with_id <- cbind(local_gwr_data_id, local_gwr_data) %>% as_tibble
pi_hat <- c()
for (i in 1:nrow(local_gwr_data_with_id)) {
  id_i <- local_gwr_data_with_id$id[i]
  beta_i <- local_GWR_vs_coefs_bw %>% filter(id == id_i) %>% select(-(id:n_NA))
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
confusionMatrix(ifelse(local_gwr_data_with_id$pi_hat < threshold, 0, 1) %>% as.factor,
                local_gwr_data_with_id$hyd_destination %>% as.factor,
                positive="1")

depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

for (i in 1:ncol(local_GWR_vs_coefs_bw)) {
  if (i %in% c(1, 3, 4, 5)) next
  var_name <- names(local_GWR_vs_coefs_bw)[i]
  gwr_coefs_i <- data.frame(id=local_GWR_vs_coefs_bw$id,
                            coef=local_GWR_vs_coefs_bw[[var_name]])
  
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
  
  ggsave(paste0("Colombia Data/Figs/local GWR coef maps/hyd destintion local GWR results/hyd destination GWR coef ", var_name, ".png"),
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

##
gwr_coefs_bw2.1 <- gwr_models_hyd_destination$bw_2.1$beta_mat %>% as_tibble
# write.csv(gwr_coefs_bw2 %>% mutate(id=gwr_data_hyd_destination$id), "Colombia Data/GWR coefs (bw=2).csv", row.names=F)
# gwr_coefs_bw2.1 %>% apply(2, sd) %>% t %>% write.csv("Colombia Data/GWR coefs sd (bw=2.1).csv", row.names=F)


for (i in 2:ncol(gwr_coefs_bw2.1)) {
  var_name <- names(gwr_coefs_bw2.1)[i]
  gwr_coefs_i <- data.frame(id=gwr_data_hyd_destination$id,
                            coef=gwr_coefs_bw2.1[[var_name]])
  
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
  
  # ggsave(paste0("Colombia Data/Figs/GWR coef maps/hyd destintion GWR results with id=18756/hyd destination GWR coef ", var_name, ".png"),
  #        gwr_coef_map, scale=1)
  ggsave(paste0("Colombia Data/Figs/GWR coef maps/hyd destintion GWR results without id=18756/hyd destination GWR coef ", var_name, " without id=18756.png"),
         gwr_coef_map, scale=1)
}

bw_2.1_p_values <- tibble()
for (i in 1:length(gwr_models_hyd_destination$bw_2.1$gwr_model)) {
  gwr_model_summary_i <- gwr_models_hyd_destination$bw_2.1$gwr_model[[i]] %>% summary
  bw_2.1_p_values <- rbind(bw_2.1_p_values,
                           c(gwr_data_hyd_destination$id[i], gwr_data_hyd_destination$municipio[i],
                             gwr_data_hyd_destination$year[i], gwr_model_summary_i$coefficients[,4]))
}
bw_2.1_p_values <- as_tibble(bw_2.1_p_values)
names(bw_2.1_p_values) <- c("id", "municipio", "year", "Intercept", row.names(gwr_model_summary_i$coefficients)[-1])
bw_2.1_p_values <- bw_2.1_p_values %>% select(-year) %>% unique
bw_2.1_p_values$n_significant_vars <- bw_2.1_p_values %>% 
  apply(1, function(x) sum(x[-1] <= 0.05))
bw_2.1_p_values %>% relocate(id, municipio, n_significant_vars) %>% arrange(desc(n_significant_vars))# %>% 
# write.csv("Colombia Data/GWR results/gwr coefs and pvalues bw=2.1.csv", row.names=F)
# bw_2.1_p_values %>% relocate(id, municipio, n_significant_vars) %>% arrange(desc(n_significant_vars)) %>%
#   write.csv("Colombia Data/GWR results/gwr coefs and pvalues bw=2.1 without id=18756.csv", row.names=F)

gwr_models_hyd_destination$bw_2.1$gwr_model$gwr_model_166 %>% summary

gwr_models_hyd_destination$bw_2.1$gwr_model$gwr_model_1


gwr_data_hyd_destination %>% select(-id, -municipio, -year) %>% summary
gwr_data_hyd_destination %>% filter(id == 18756) %>% view
municipio_centroid %>% filter(id == 18756) # SOLANO, Caqueta

# GWR coef maps without id=18756
no_18756_index <- which(gwr_data_hyd_destination$id != 18756)
gwr_coefs_bw2.1_without_18756 <- gwr_coefs_bw2.1[no_18756_index,]
for (i in 2:ncol(gwr_coefs_bw2)) {
  var_name <- names(gwr_coefs_bw2.1_without_18756)[i]
  gwr_coefs_i <- data.frame(id=ever_anecdotal_data_years$id[no_18756_index],
                            coef=gwr_coefs_bw2.1_without_18756[[var_name]])
  
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
  
  ggsave(paste0("Colombia Data/Figs/GWR coef maps/hyd destintion GWR results with id=18756/hyd destination GWR coef without id=18756", var_name, ".png"),
         gwr_coef_map, scale=1)
}

