# install.packages("repmis")
library(repmis)
source_data("https://github.com/lexcomber/GWRroutemap/blob/master/Liudaogou.RData?raw=True")
data
soil_data <- data %>% mutate(Latitude = Latitude/10000, 
                             Longitude = Longitude/1000,
                             TNPC = log(1+TNPC),
                             SOCgkg = log(1+SOCgkg),
                             NO3Ngkg = log(1+abs(NO3Ngkg)),
                             NH4Ngkg = log(1+NH4Ngkg),
                             TPPC = sqrt(TPPC),
                             ClayPC = sqrt(ClayPC))
soil_data_dist <- dist(soil_data %>% select(Latitude, Longitude)) %>% as.matrix
summary(soil_data_dist %>% as.vector)

soil_data
soil_data %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color=TNPC), size=4) + scale_color_viridis_c()
soil_data %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color=TPPC), size=4) + scale_color_viridis_c()
soil_data %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color=SOCgkg), size=4) + scale_color_viridis_c()
soil_data %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color=ClayPC), size=4) + scale_color_viridis_c()
soil_data %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color=SiltPC), size=4) + scale_color_viridis_c()

soil_bandwidths <- seq(0.1, 2, by=0.1)
soil_lm_list <- list()
STN_coefs <- matrix(NA, nrow(soil_data), 2+ncol(soil_neighbors_i %>% select(TNPC, SOCgkg:NH4Ngkg)))
colnames(STN_coefs) <- c("id", "bw", "Intercept", names(soil_neighbors_i %>% select(TNPC, SOCgkg:NH4Ngkg))[-1])
RMSE_mat <- matrix(NA, nrow(soil_data), length(soil_bandwidths))
colnames(RMSE_mat) <- paste0("bw_", soil_bandwidths)
for (i in 1:nrow(soil_data)) {
  id_i <- soil_data$ID[i]
  soil_lm_list[[paste0("id_", id_i)]] <- list()
  for (j in 1:length(soil_bandwidths)) {
    bw_i <- soil_bandwidths[j]
    soil_neighbors_i <- soil_data[which(soil_data_dist[i,] < bw_i),] %>% filter(ID != id_i)
    if (nrow(soil_neighbors_i) < 2*6) {
      soil_lm_list[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]] <- NA
      next
    }
    reg_model_bw_i <- lm(TNPC~., soil_neighbors_i %>% select(TNPC, SOCgkg:NH4Ngkg))
    RMSE_mat[i,j] <- sqrt(mean((reg_model_bw_i$residuals)^2 / nrow(soil_neighbors_i)))
    soil_lm_list[[paste0("id_", id_i)]][[paste0("bw_", bw_i)]] <- reg_model_bw_i
  }
  best_bw_i <- soil_bandwidths[which.min(RMSE_mat[i,])]
  STN_coefs[i,] <- c(id_i, best_bw_i, coef(soil_lm_list[[paste0("id_", id_i)]][[paste0("bw_", best_bw_i)]]))
}
STN_coefs <- as_tibble(STN_coefs)
STN_coefs
soil_lm_list$id_WYQ300$bw_0.1$model
lm(TNPC~., soil_lm_list$id_WYQ300$bw_0.1$model) %>% summary
