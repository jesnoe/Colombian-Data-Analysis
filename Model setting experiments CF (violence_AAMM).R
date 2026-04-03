#### seizures are last year data t-1
## bandwidth range: 0~4
indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual", "FARC")
max_bwd <- 4
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
# library(randomForest)
library(pracma)
library(GWmodel)
library(pROC)
library(glmnet)
library(reshape2)
library(regclass)
library(logistf)
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
  ferry <- read.csv("Colombia Data/ferry terminals.csv") %>% as_tibble
  police <- read.csv("Colombia Data/polices.csv") %>% as_tibble
  military <- read.csv("Colombia Data/military.csv") %>% as_tibble
  ferry$n_police <- police$n_polices
  ferry$n_military <- military$n_military
  
  n_drop_ <- 10
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  depto_map <- suppressMessages(fortify(departamentos)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(id != 88) %>% 
    left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
  
  violence_AAMM <- read.csv("Colombia Data/violence with id (AAMM).csv") %>% as_tibble
  conflict <- read.csv("Colombia Data/Conflict events.csv") %>% as_tibble
  violence_combined <- bind_rows(violence_AAMM %>% select(id, year, FARC) %>% mutate(FARC = ifelse(FARC == "yes", 1 , 0)),
                                 conflict %>% mutate(FARC = ifelse(grepl("FARC", dyad_name), 1, 0)) %>% select(id, year, FARC)) %>% 
    group_by(id, year) %>% summarize(FARC = ifelse(any(FARC == 1), 1, 0)) %>% ungroup
  
  regression_data_CF_2013 <- read.csv("Colombia Data/regression data all municipios CF 2013.csv") %>% as_tibble
  regression_data_CF_2014 <- read.csv("Colombia Data/regression data all municipios CF 2014.csv") %>% as_tibble
  regression_data_CF_2016 <- read.csv("Colombia Data/regression data all municipios CF 2016.csv") %>% as_tibble
  regression_data_CF_2017 <- read.csv("Colombia Data/regression data all municipios CF 2017.csv") %>% as_tibble
  
  regression_data_CF_2013 <- regression_data_CF_2013 %>% left_join(violence_combined %>% filter(year == 2013) %>% select(-year), by = "id") %>% mutate(FARC = ifelse(is.na(FARC), 0, FARC))
  regression_data_CF_2014 <- regression_data_CF_2014 %>% left_join(violence_combined %>% filter(year == 2014) %>% select(-year), by = "id") %>% mutate(FARC = ifelse(is.na(FARC), 0, FARC))
  regression_data_CF_2016 <- regression_data_CF_2016 %>% left_join(violence_combined %>% filter(year == 2016) %>% select(-year), by = "id") %>% mutate(FARC = ifelse(is.na(FARC), 0, FARC))
  regression_data_CF_2017 <- regression_data_CF_2017 %>% left_join(violence_combined %>% filter(year == 2017) %>% select(-year), by = "id") %>% mutate(FARC = ifelse(is.na(FARC), 0, FARC))
  
  
  coord_unique <- left_join(regression_data_CF_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  # PML_gwr_coefs_AUC_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_AUC all var drop 10 2016 data CF (02-04-2026).csv") %>% as_tibble
}

# the number of municipios with y=1
regression_data_CF_2013 %>% filter(hyd_destination == 1) %>% nrow # 157
regression_data_CF_2014 %>% filter(hyd_destination == 1) %>% nrow # 278
regression_data_CF_2016 %>% filter(hyd_destination == 1) %>% nrow # 123
regression_data_CF_2017 %>% filter(hyd_destination == 1) %>% nrow # 365

ROC_pred <- function(GWR_pred) {
  result <- roc(GWR_pred$y, GWR_pred$pi_hat, positive = "1", quiet = T)
  return(result)
}

neighbor_id <- function(id_i, bw_i, scale_11_, gwr_data_) {
  if (scale_11_) gwr_data_id <- gwr_data_$scale_11
  else gwr_data_id <- gwr_data_$norm
  
  coord_unique_ <- gwr_data_$coord
  local_gwr_dist_ <- gwr_data_$dist %>% as.matrix
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)])
  return(result)
}

regression_data_CF_2013$hyd_seizures <- round(regression_data_CF_2013$hyd_seizures, 2)
regression_data_CF_2014$hyd_seizures <- round(regression_data_CF_2014$hyd_seizures, 2)
regression_data_CF_2016$hyd_seizures <- round(regression_data_CF_2016$hyd_seizures, 2)
regression_data_CF_2017$hyd_seizures <- round(regression_data_CF_2017$hyd_seizures, 2)

gwr_data_year <- function(dep_var_, reg_data_cor, price=F) {
  dep_var_index <- which(names(reg_data_cor) == dep_var_)
  names(reg_data_cor)[dep_var_index] <- "y"
  
  if (grepl("hyd", dep_var_)) {
    reg_data_cor <- reg_data_cor %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_cor <- reg_data_cor %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  reg_data_cor <- reg_data_cor %>% select(-price_avg)
  gwr_data_year <- list(norm = reg_data_cor, coord = coord_unique, dist = gwr_data_dist)
  return(gwr_data_year)
}


gwr_data_2013 <- gwr_data_year("hyd_destination", regression_data_CF_2013)
gwr_data_2014 <- gwr_data_year("hyd_destination", regression_data_CF_2014)
gwr_data_2016 <- gwr_data_year("hyd_destination", regression_data_CF_2016)
gwr_data_2017 <- gwr_data_year("hyd_destination", regression_data_CF_2017)

PML_gwr_coefs_AUC_CF_2013 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2013 data no price CF (03-24-2026).csv") %>% as_tibble
PML_gwr_coefs_AUC_CF_2014 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2014 data no price CF (03-24-2026).csv") %>% as_tibble
PML_gwr_coefs_AUC_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2016 data no price CF (03-24-2026).csv") %>% as_tibble
PML_gwr_coefs_AUC_CF_2017 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2017 data no price CF (03-24-2026).csv") %>% as_tibble
PML_gwr_coefs_AUC_CF_2013_2014 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2013-2014 data no price CF (03-24-2026).csv") %>% as_tibble
PML_gwr_coefs_AUC_CF_2016_2017 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_AAMM all var drop by AUC n_drop=10 2016-2017 data no price CF (03-24-2026).csv") %>% as_tibble

# correlation check
PML_gwr_coefs_AUC_CF <- PML_gwr_coefs_AUC_CF_2013

reg_data_cor_year <- function(PML_gwr_coefs_AUC_CF, gwr_data_1) {
  reg_data_cor <- PML_gwr_coefs_AUC_CF %>% select(-Intercept)
  cor_mat <- matrix(NA, nrow(PML_gwr_coefs_AUC_CF), ncol(PML_gwr_coefs_AUC_CF) - 3)
  for (i in 1:nrow(PML_gwr_coefs_AUC_CF)) {
    id_i <- PML_gwr_coefs_AUC_CF$id[i]
    bw_i <- PML_gwr_coefs_AUC_CF$bw[i]
    if (is.na(bw_i)) next
    coefs_i <- PML_gwr_coefs_AUC_CF[i,-(1:3)]
    reg_data_id_i <- neighbor_id(id_i, bw_i, F, gwr_data_1) %>% filter(id != id_i)
    reg_data_cor_id_i <- cor(reg_data_id_i %>% select(-id, -y))
    reg_data_cor_id_i[reg_data_cor_id_i == 1] <- 0
    if (any(is.na(coefs_i))) {
      NA_index <- which(is.na(coefs_i))
      reg_data_cor_id_i[,NA_index] <- NA
      reg_data_cor_id_i[NA_index,] <- NA
    }
    
    max_cor_id_i <- reg_data_cor_id_i %>% apply(1, function(x) ifelse(all(is.na(x)), NA, x[which.max(abs(x))]))
    cor_mat[i,] <- max_cor_id_i
  }
  reg_data_cor[, -(1:2)] <- cor_mat
  return(reg_data_cor)
}

reg_data_cor_two_year <- function(PML_gwr_coefs_AUC_CF, gwr_data_1, gwr_data_2, year1, year2) {
  reg_data_cor <- PML_gwr_coefs_AUC_CF %>% select(-Intercept)
  cor_mat <- matrix(NA, nrow(PML_gwr_coefs_AUC_CF), ncol(PML_gwr_coefs_AUC_CF) - 3)
  for (i in 1:nrow(PML_gwr_coefs_AUC_CF)) {
    id_i <- PML_gwr_coefs_AUC_CF$id[i]
    bw_i <- PML_gwr_coefs_AUC_CF$bw[i]
    if (is.na(bw_i)) next
    neighbor_id_year1_i <- neighbor_id(id_i, bw_i, F, gwr_data_1) %>% filter(id != id_i)
    neighbor_id_year2_i <- neighbor_id(id_i, bw_i, F, gwr_data_2) %>% filter(id != id_i)
    coefs_i <- PML_gwr_coefs_AUC_CF[i,-(1:3)]
    reg_data_id_i <- bind_rows(neighbor_id_year1_i, neighbor_id_year2_i)
    reg_data_cor_id_i <- cor(reg_data_id_i %>% select(-id, -y))
    reg_data_cor_id_i[reg_data_cor_id_i == 1] <- 0
    if (any(is.na(coefs_i))) {
      NA_index <- which(is.na(coefs_i))
      reg_data_cor_id_i[,NA_index] <- NA
      reg_data_cor_id_i[NA_index,] <- NA
    }
    
    max_cor_id_i <- reg_data_cor_id_i %>% apply(1, function(x) ifelse(all(is.na(x)), NA, x[which.max(abs(x))]))
    cor_mat[i,] <- max_cor_id_i
  }
  reg_data_cor[, -(1:2)] <- cor_mat
  return(reg_data_cor)
}

reg_data_cor_2013 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2013, gwr_data_2013)
reg_data_cor_2014 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2014, gwr_data_2014)
reg_data_cor_2016 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2016, gwr_data_2016)
reg_data_cor_2017 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2017, gwr_data_2017)

reg_data_cor_1314 <- reg_data_cor_two_year(PML_gwr_coefs_AUC_CF_2013_2014, gwr_data_2013, gwr_data_2014, 2013, 2014)
reg_data_cor_1617 <- reg_data_cor_two_year(PML_gwr_coefs_AUC_CF_2016_2017, gwr_data_2016, gwr_data_2017, 2016, 2017)

# write.csv(reg_data_cor_2013, "Colombia Data/regression data max correlations (2013).csv", row.names = F)
# write.csv(reg_data_cor_2014, "Colombia Data/regression data max correlations (2014).csv", row.names = F)
# write.csv(reg_data_cor_2016, "Colombia Data/regression data max correlations (2016).csv", row.names = F)
# write.csv(reg_data_cor_2017, "Colombia Data/regression data max correlations (2017).csv", row.names = F)
# write.csv(reg_data_cor_1314, "Colombia Data/regression data max correlations (2013-2014).csv", row.names = F)
# write.csv(reg_data_cor_1617, "Colombia Data/regression data max correlations (2016-2017).csv", row.names = F)

reg_data_cor_2013 <- read.csv("Colombia Data/regression data max correlations (2013).csv")
reg_data_cor_2014 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2014, gwr_data_2014)
reg_data_cor_2016 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2016, gwr_data_2016)
reg_data_cor_2017 <- reg_data_cor_year(PML_gwr_coefs_AUC_CF_2017, gwr_data_2017)

reg_data_cor_1314 <- reg_data_cor_two_year(PML_gwr_coefs_AUC_CF_2013_2014, gwr_data_2013, gwr_data_2014, 2013, 2014)
reg_data_cor_1617 <- reg_data_cor_two_year(PML_gwr_coefs_AUC_CF_2016_2017, gwr_data_2016, gwr_data_2017, 2016, 2017)

cor_map_year <- function(year_, reg_data_cor, dep_var) {
  for (i in 3:15) {
    var_name_ <- names(reg_data_cor)[i]
    reg_data_cor_i <- data.frame(id=reg_data_cor$id, cor=reg_data_cor[[var_name_]])
    data_map_coords <- map_df %>% left_join(reg_data_cor_i, by="id")
    
    ggplot(data_map_coords, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group, fill=cor),
                   color = "black",
                   linewidth = 0.1) +
      expand_limits(x = depto_map$long, y = depto_map$lat) +
      coord_quickmap() +
      scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                           limits = c(-1, 1),
                           na.value = "white") +
      labs(fill=var_name_, x="", y="", title=sprintf("maximum abs correlations with %s (%s)", var_name_, year_)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank()
      ) -> cor_map_i
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/correlation maps/%s (%s)/max correlation map %s (%s).png", dep_var, year_, var_name_, year_), cor_map_i, scale=1)
  }
}

cor_map_year(2013, reg_data_cor_2013, "hyd_destination")
cor_map_year(2014, reg_data_cor_2014, "hyd_destination")
cor_map_year(2016, reg_data_cor_2016, "hyd_destination")
cor_map_year(2017, reg_data_cor_2017, "hyd_destination")
cor_map_year("2013-2014", reg_data_cor_1314, "hyd_destination")
cor_map_year("2016-2017", reg_data_cor_1617, "hyd_destination")

as_tibble(abs(reg_data_cor_2013) > abs(reg_data_cor_1314)) %>% select(-id, -bw) %>% sum(na.rm=T) # 4652
as_tibble(abs(reg_data_cor_2013) < abs(reg_data_cor_1314)) %>% select(-id, -bw) %>% sum(na.rm=T) # 5512
as_tibble(abs(reg_data_cor_2014) > abs(reg_data_cor_1314)) %>% select(-id, -bw) %>% sum(na.rm=T) # 4687
as_tibble(abs(reg_data_cor_2014) < abs(reg_data_cor_1314)) %>% select(-id, -bw) %>% sum(na.rm=T) # 5315

as_tibble(abs(reg_data_cor_2016) > abs(reg_data_cor_1617)) %>% select(-id, -bw) %>% sum(na.rm=T) # 4210
as_tibble(abs(reg_data_cor_2016) < abs(reg_data_cor_1617)) %>% select(-id, -bw) %>% sum(na.rm=T) # 5880
as_tibble(abs(reg_data_cor_2017) > abs(reg_data_cor_1617)) %>% select(-id, -bw) %>% sum(na.rm=T) # 4570
as_tibble(abs(reg_data_cor_2017) < abs(reg_data_cor_1617)) %>% select(-id, -bw) %>% sum(na.rm=T) # 4551

as_tibble(abs(reg_data_cor_2013) > abs(reg_data_cor_1314)) %>% select(-id, -bw) %>% apply(2, function(x) sum(x, na.rm=T))
as_tibble(abs(reg_data_cor_2013) < abs(reg_data_cor_1314)) %>% select(-id, -bw) %>% apply(2, function(x) sum(x, na.rm=T))

PML_gwr_coefs_AUC_CF_2013_2014
PML_gwr_coefs_AUC_CF_2013_2014_year_effect <- PML_gwr_coefs_AUC_CF_2013_2014 %>% mutate(year = NA)
coef_tbl <- tibble(var_name = names(PML_gwr_coefs_AUC_CF_2013_2014_year_effect)[-(1:2)])
for (i in 1:nrow(PML_gwr_coefs_AUC_CF_2013_2014)) {
  id_i <- PML_gwr_coefs_AUC_CF_2013_2014$id[i]
  bw_i <- PML_gwr_coefs_AUC_CF_2013_2014$bw[i]
  neighbor_id_2013_i <- neighbor_id(id_i, bw_i, F, gwr_data_2013) %>% filter(id != id_i)
  neighbor_id_2014_i <- gwr_data_2014$norm %>% filter(id %in% neighbor_id_2013_i$id)
  reg_data_id_i <- bind_rows(neighbor_id_2013_i %>% mutate(year = 2013), neighbor_id_2014_i %>% mutate(year = 2014)) %>% select(-id) %>% mutate(year = as.factor(year))
  vars_in_model <- coef_tbl$var_name[-1][which(!is.na(PML_gwr_coefs_AUC_CF_2013_2014[i,][-(1:3)]))]
  PML_gwr_coefs_AUC_CF_2013_2014[i,]
  logistf(y~., reg_data_id_i %>% select(y, all_of(vars_in_model)) %>% mutate(y = as.factor(y)))
  GWR_id_i <- logistf(y~., reg_data_id_i %>% select(y, all_of(vars_in_model)))
}

# local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
load("Colombia Data/local GWR PML result predicted prices/local GWR PML hyd_destination violence_AAMM all var drop by AUC n_drop=10 2013-2014 data no price CF (03-24-2026).RData")
local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_5031$bw_0.5$model %>% as_tibble

neighbor_id_5002_2013 <- neighbor_id(5002, 0.7, F, gwr_data_2013)
neighbor_id_5002_2014 <- neighbor_id(5002, 0.7, F, gwr_data_2014)
reg_data_id_5002 <- bind_rows(neighbor_id_5002_2013, neighbor_id_5002_2014) %>% select(-id)
reg_data_with_year_id_5002 <- bind_rows(neighbor_id_5002_2013 %>% mutate(year = 2013), neighbor_id_5002_2014 %>% mutate(year = 2014)) %>% select(-id) %>% mutate(year = as.factor(year))

logistf_result1 <- logistf(y~., reg_data_id_5002)
logistf_result2 <- logistf(y~., reg_data_with_year_id_5002)

roc(reg_data_id_5002$y, logistf_result1$predict, positive = "1", quiet = T)
roc(reg_data_with_year_id_5002$y, logistf_result2$predict, positive = "1", quiet = T)

