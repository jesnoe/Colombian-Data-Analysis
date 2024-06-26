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

regression_data_years <- read.csv("Colombia Data/regression data (05-15-2024).csv") %>% as_tibble %>% 
  left_join(airports, by="id") %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1],
         airport=ifelse(n_airports > 0, 1, 0))

ever_anecdotal <- regression_data_years %>% 
  group_by(id) %>% 
  summarise(base_source=ifelse(sum(base_source)>0, 1, 0),
            base_destination=ifelse(sum(base_destination)>0, 1, 0),
            hyd_source=ifelse(sum(hyd_source)>0, 1, 0),
            hyd_destination=ifelse(sum(hyd_destination)>0, 1, 0))

ever_anecdotal_data_years <- regression_data_years %>% 
  select(-(base_source_all:general_destination)) %>% 
  left_join(ever_anecdotal, by="id")

ever_anecdotal %>% select(-id) %>% apply(2, sum) # 541 municipalities
ever_anecdotal_data_years %>% select(base_source:hyd_destination) %>% apply(2, sum) # 1,623 rows

# filter out departments without anecdotal evidence
anecdotal_id_depto <- ever_anecdotal_data_years %>% 
  left_join(municipios_capital %>% select(id, id_depto), by="id") %>% 
  group_by(id_depto) %>% 
  summarise(hyd_source=sum(hyd_source),
            hyd_destination=sum(hyd_destination)) %>% 
  mutate(anecdotal=hyd_source+hyd_destination>0) %>% 
  filter(anecdotal) %>% pull(id_depto)
ever_anecdotal_data_years <- ever_anecdotal_data_years %>% 
  left_join(municipios_capital %>% select(id, id_depto), by="id") %>% 
  filter(id_depto %in% anecdotal_id_depto)

gwr_hyd_source_coord <- left_join(ever_anecdotal_data_years %>% 
                                    select(id, year, n_PPI_labs:population, airport, hyd_source, -base_avg, -base_price_distance,
                                           -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group, -erad_aerial),
                                  municipio_centroid %>% select(id, long, lat), by="id") %>% relocate(id, municipio)
gwr_hyd_source_dist <- dist(gwr_hyd_source_coord %>% select(long, lat), diag=T, upper=T)
as.vector(gwr_hyd_source_dist) %>% unique %>% summary
as.vector(gwr_hyd_source_dist) %>% unique %>% sort %>% head(100)


gwr_data_hyd_source <- gwr_hyd_source_coord %>% select(-long, -lat)
gwr_logistic_reg <- function(reg_data, reg_data_dist, bandwidth) {
  beta_mat <- matrix(0, nrow(reg_data), ncol(reg_data))
  for (i in 1:nrow(reg_data)) {
    neighbor_index <- which(reg_data_dist[i,] <= bandwidth)
    reg_data_i <- reg_data[neighbor_index, ]
    reg_result_i <- glm(hyd_source~.,
                        data=reg_data_i,
                        weight=1/(1+reg_data_dist[i,neighbor_index]),
                        family="binomial")
    beta_mat[i,] <- reg_result_i$coefficients
  }
  colnames(beta_mat) <- names(reg_result_i$coefficients)
  
  return(beta_mat)
}

na_columns <- tibble()
for (bw in seq(2, 10, by=0.2)) {
  betas_bw <- gwr_logistic_reg(gwr_data_hyd_source %>% select(-id, -municipio, -year), as.matrix(gwr_hyd_source_dist), bw)
  if (sum(is.na(betas_bw)) > 0) {
    na_columns <- rbind(na_columns, c(bw, apply(betas_bw, 2, function(x) ifelse(sum(is.na(x))>0, "NA", "Estimated"))))
  }else{
    na_columns <- rbind(na_columns, c(bw, rep("Estimated", ncol(gwr_data_hyd_source %>% select(-id, -municipio, -year)))))
  }
  
  print(paste0("bandwidth=", bw, " complete"))
}
names(na_columns) <- c("bandwidth", colnames(betas_bw))
# write.csv(na_columns, "Colombia Data/GWR hyd_source coefs NA with airport.csv", row.names=F)


gwr_data_hyd_source <- gwr_hyd_source_coord %>% select(-long, -lat, -erad_manual)
n_neighbors <- tibble(id=gwr_data_hyd_source$id, municipio=gwr_data_hyd_source$municipio)
hyd_source_dist_mat <- as.matrix(gwr_hyd_source_dist)

for (bw in seq(2, 10, by=0.2)) {
  n_neighbors[[paste0("bw=", bw)]] <- hyd_source_dist_mat %>% 
    apply(1, function(x) which(x<=bw)) %>% 
    lapply(function(x) gwr_data_hyd_source$id[x] %>% unique %>% length) %>% 
    unlist
}
# write.csv(n_neighbors, "Colombia Data/GWR hyd_source number of neighbors by bandwidth.csv", row.names=F)

y_bw <- gwr_data_hyd_source$hyd_source
gwr_hyd_source_result <- tibble(bandwidth=0, log_lik=0, error=0)
for (bw in seq(2.2, 3, by=0.2)) {
  gwr_data_bw <- gwr_data_hyd_source %>% select(-id, -municipio, -year)
  test <- try(gwr_logistic_reg(gwr_data_bw, hyd_source_dist_mat, bw))
  
  if (inherits(test, "try-error")) {
    gwr_data_hyd_source <- rbind(gwr_data_hyd_source, c(bw, 0, 1))
    next
  }
  
  betas_bw <- gwr_logistic_reg(gwr_data_bw, hyd_source_dist_mat, bw)
  X_beta_bw <- betas_bw[,1] + apply(betas_bw[,-1] * as.matrix(gwr_data_bw %>% select(-hyd_source)), 1, sum)
  pi_hat_bw <- exp(X_beta_bw)/(1+exp(X_beta_bw))
  pi_hat_bw <- ifelse(is.infinite(exp(X_beta_bw)), 1, pi_hat_bw)
  pi_hat_bw <- ifelse(pi_hat_bw == 1, 0.999999999,
                      ifelse(pi_hat_bw == 0, 0.000000001, pi_hat_bw))
  
  log_lik_bw <- sum(y_bw*log(pi_hat_bw) + (1-y_bw)*log(1-pi_hat_bw))
  
  gwr_hyd_source_result <- rbind(gwr_hyd_source_result, c(bw, log_lik_bw, 0))
  
  print(paste0("bandwidth=", bw, " complete"))
}
gwr_hyd_source_result <- gwr_hyd_source_result[-1,]
# write.csv(gwr_hyd_source_result, "Colombia Data/GWR hyd_source log-lik results.csv", row.names=F)
gwr_hyd_source_result %>% as.data.frame

confusionMatrix(ifelse(pi_hat_bw < 0.5, 0, 1) %>% as.factor,
                y_bw %>% as.factor,
                positive="1")

ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill = ifelse(id==5002, "Abejorral", NA)),
               color = "black",
               linewidth = 0.1
  ) + 
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


depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
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

hyd_source_map_data <- 
  gwr_hyd_source_coord %>% 
  select(id, hyd_source) %>% 
  left_join(municipio_centroid %>% select(id, long, lat), by="id") %>% 
  mutate(posterior_glm=pi_hat_bw)

threshold <- 0.5
hyd_gwr_source_pred_map <- empty_map +
  geom_point(data=hyd_source_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_glm < threshold, 2*(threshold-posterior_glm), 2*(posterior_glm-threshold)),
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="GWR Predictions (hyd sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") +
  theme(legend.position="right")
# ggsave("Colombia Data/Figs/prediction map/hyd sources gwr pred (2013-2016).png", hyd_gwr_source_pred_map, scale=1)

empty_map +
  geom_point(data=data.frame(long=c(-75.6, -73.6), lat=c(6.26, 6.26)),
             aes(x=long, y=lat),
             color=c("black", "red"))

##
gwr_coefs_bw2 <- (gwr_logistic_reg(gwr_data_hyd_source %>% select(-erad_manual), as.matrix(gwr_hyd_source_dist), 2) %>% as_tibble)[,-1]
# write.csv(gwr_coefs_bw2 %>% mutate(id=ever_anecdotal_data_years$id), "Colombia Data/GWR coefs (bw=2).csv", row.names=F)
gwr_coefs_bw2 %>% apply(2, sd) %>% t %>% write.csv("Colombia Data/GWR coefs sd (bw=2).csv", row.names=F)


for (i in 2:ncol(gwr_coefs_bw2)) {
  var_name <- names(gwr_coefs_bw2)[i]
  gwr_coefs_i <- data.frame(id=ever_anecdotal_data_years$id,
                            coef=gwr_coefs_bw2[[var_name]])
  
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
  
  ggsave(paste0("Colombia Data/Figs/GWR coef maps/hyd source GWR coef ", var_name, ".png"),
         gwr_coef_map, scale=1)
}

gwr_coefs_bw2 %>% mutate(id=ever_anecdotal_data_years$id) %>%
  left_join(municipios_capital %>% select(id, municipio, depto)) %>% 
  relocate(id, municipio, depto) %>%  filter(n_PPI_labs < -1E14)

ever_anecdotal_data_years %>% filter(id == 81591) %>% view
municipios_capital %>% filter(id==81591) # PUERTO RONDON, Arauca

# GWR coef maps without id=81591
no_81591_index <- which(ever_anecdotal_data_years$id != 81591)
gwr_coefs_bw2_without_81591 <- gwr_coefs_bw2[no_81591_index,]
for (i in 2:ncol(gwr_coefs_bw2)) {
  var_name <- names(gwr_coefs_bw2_without_81591)[i]
  gwr_coefs_i <- data.frame(id=ever_anecdotal_data_years$id[no_81591_index],
                            coef=gwr_coefs_bw2_without_81591[[var_name]])
  
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
  
  ggsave(paste0("Colombia Data/Figs/GWR coef maps/hyd source GWR coef without id=81591", var_name, ".png"),
         gwr_coef_map, scale=1)
}
