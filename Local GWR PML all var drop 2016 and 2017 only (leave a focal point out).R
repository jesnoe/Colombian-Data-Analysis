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
bwd_range <- seq(0.5, 5, by=0.1)
depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")

regression_data_2016 <- read.csv("Colombia Data/regression data all municipios lab_prob 2016.csv") %>% as_tibble
regression_data_2017 <- read.csv("Colombia Data/regression data all municipios lab_prob 2017.csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
}

neighbor_id <- function(id_i, bw_i, scale_11_, gwr_data_) {
  if (scale_11_) gwr_data_id <- gwr_data_$scale_11
  else gwr_data_id <- gwr_data_$norm
  
  coord_unique_ <- gwr_data_$coord
  local_gwr_dist_ <- gwr_data_$dist %>% as.matrix
  id_i_index <- which(coord_unique_$id == id_i)
  i <- id_i_index 
  result <- gwr_data_id %>% 
    filter(id %in% coord_unique_$id[which(local_gwr_dist_[id_i_index,] <= bw_i)]) %>% 
    select(-municipio)
  return(result)
}

local_GWR_PML <- function(type.measure_="default", cv_aic_min_mat, sig_level_=0.05, gwr_PML_data_, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var) {
  bwd_range <- seq(0.5, 5, by=0.1)
  coord_unique <- gwr_PML_data_$coord
  local_gwr_dist <- gwr_PML_data_$dist %>% as.matrix
  
  local_GWR_coefs_PML_result <- list()
  for (i in 1:nrow(cv_aic_min_mat)) {
    id_i <- cv_aic_min_mat$id[i]
    local_GWR_coefs_PML_result[[paste0("id_", id_i)]] <- list()
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      bw_name <- paste0("bw_", bw_ij)
      local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[bw_name]] <- NA
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, gwr_data_=gwr_PML_data_) %>% 
        filter(id != id_i) #### leave a focal point out
      n_0_1 <- neighbor_ij$y %>% table
      
      # restrict too unbalanced responses
      if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
        cv_aic_min_mat[[bw_name]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      n_unique_vals <- neighbor_ij %>% select(-id) %>% apply(2, function(x) length(table(x)))
      if (method_ == "model drop") {
        if (any(n_unique_vals < 2) | any(n_unique_vals[3:4] < n_drop)) {
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
          next
        }
      }
      
      
      if (method_ == "var drop") {
        if (n_unique_vals[["coca_area"]] < n_drop) {
          neighbor_ij$coca_area <- NULL
          n_unique_vals[["coca_area"]] <- n_drop
        }
        if (n_unique_vals[["seizures"]] < n_drop) {
          neighbor_ij$seizures <- NULL
          n_unique_vals[["seizures"]] <- n_drop
        }
        if (n_unique_vals[["price_avg"]] < n_drop) {
          neighbor_ij$price_avg <- NULL
          n_unique_vals[["price_avg"]] <- n_drop
        }
        if (n_unique_vals[["river_length"]] < n_drop) {
          neighbor_ij$river_length <- NULL
          n_unique_vals[["river_length"]] <- n_drop
        }
        if (n_unique_vals[["road_length"]] < n_drop) {
          neighbor_ij$road_length <- NULL
          n_unique_vals[["road_length"]] <- n_drop
        }
        if (n_unique_vals[["lab_prob"]] < n_drop) {
          neighbor_ij$lab_prob <- NULL
          n_unique_vals[["lab_prob"]] <- n_drop
        }
        
        if (neighbor_ij$airport %>% table %>% min < n_drop | n_unique_vals[["airport"]] < 2) neighbor_ij$airport <- NULL
        if (neighbor_ij$armed_group %>% table %>% min < n_drop | n_unique_vals[["armed_group"]] < 2) neighbor_ij$armed_group <- NULL
        if (neighbor_ij$ferry %>% table %>% min < n_drop | n_unique_vals[["ferry"]] < 2) neighbor_ij$ferry <- NULL
        if (neighbor_ij$police %>% table %>% min < n_drop | n_unique_vals[["police"]] < 2) neighbor_ij$police <- NULL
        if (neighbor_ij$military %>% table %>% min < n_drop | n_unique_vals[["military"]] < 2) neighbor_ij$military <- NULL
      }
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      result_i <- tryCatch(
        {
          PML_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)
          AIC_i <- extractAIC(PML_result_ij)[[2]]
          cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- AIC_i
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- PML_result_ij
        },
        error = function(e) {
          return(e)
        }
      )
      
      if (inherits(result_i, "error")) {
        cv_aic_min_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      }
      
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(cv_aic_min_mat=cv_aic_min_mat,
              PML=local_GWR_coefs_PML_result))
}

local_GWR_PML_1_year <- function(dep_var_, seed_model, reg_data_year, year_, weight_in=NULL) {
  PML_gwr_coefs_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
  
  dep_var_index <- which(names(reg_data_year) == dep_var_)
  names(reg_data_year)[dep_var_index] <- "y"
  
  if (grepl("hyd", dep_var_)) {
    reg_data_year <- reg_data_year %>% 
      select(-PPI_lab_prob, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_prob=hyd_lab_prob, seizures=hyd_seizures) %>% 
      relocate(id, y, names(PML_gwr_coefs_hyd_dest)[-(1:3)])
  }else{
    reg_data_year <- reg_data_year %>% 
      select(-hyd_lab_prob, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_prob=PPI_lab_prob, seizures=base_seizures) %>% 
      relocate(id, y, names(PML_gwr_coefs_hyd_dest)[-(1:3)])
  }
  
  
  gwr_data <- ever_regression_data_years(dep_var_)
  reg_data_year <- left_join(gwr_data$norm %>% select(id, municipio), reg_data_year, by="id")
  # reg_data_year <- left_join(gwr_data$norm %>% select(id, municipio, y), reg_data_year %>% select(-y), by="id")
  gwr_data$norm <- reg_data_year
  bwd_range <- seq(0.5, 5, by=0.1)
  cv_aic_min_mat_ <- matrix(NA, 1120, 1+length(bwd_range))
  colnames(cv_aic_min_mat_) <- c("id", paste0("bw_", bwd_range))
  cv_aic_min_mat_ <- as_tibble(cv_aic_min_mat_)
  cv_aic_min_mat_$id <- read.csv("Colombia Data/local GWR forward result predicted prices/local GWR forward hyd_dest predicted price cv min dev (04-09-2025).csv")$id
  
  min_seizure_scaled <- min(gwr_data$norm$seizures) %>% round(3)
  min_coca_area_scaled <- min(gwr_data$norm$coca_area) %>% round(3)
  coord_unique <- gwr_data$coord
  local_gwr_dist <- gwr_data$dist %>% as.matrix
  
  set.seed(seed_model)
  local_GWR_coefs_PML_list <- local_GWR_PML(dep_var = dep_var_, cv_aic_min_mat=cv_aic_min_mat_, gwr_PML_data_ = gwr_data, method_="var drop", sig_level_ = 0.1, n_drop = 10, weight_ = weight_in)
  
  weight_in_1 <- weight_in[1]*10
  weight_in_0 <- weight_in[2]*10
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo <- local_GWR_coefs_PML_list$PML
  # write.csv(local_GWR_coefs_PML_list$cv_aic_min_mat,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).csv",
  #                   dep_var_, year_), row.names = F)
  # save("local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo",
  #      file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).RData",
  #                     dep_var_, year_))
  # rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo); rm(local_GWR_coefs_PML_list)

  local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3 <- local_GWR_coefs_PML_list$PML
  write.csv(local_GWR_coefs_PML_list$cv_aic_min_mat,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight %i-%i %i data (01-27-2026).csv",
                    dep_var_, weight_in_1, weight_in_0, year_), row.names = F)
  save("local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3",
       file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight %i-%i %i data (01-27-2026).RData",
                      dep_var_, weight_in_1, weight_in_0, year_))
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3); rm(local_GWR_coefs_PML_list)
}

weights <- c(0.7 , 0.3) # 0.7 for postive outcomes
weights <- c(0.9 , 0.1) # 0.9 for postive outcomes
# local_GWR_PML_1_year("hyd_destination", 100, regression_data_2016, 2016)
local_GWR_PML_1_year("hyd_destination", 100, regression_data_2016, 2016, weight_in = weights)
# local_GWR_PML_1_year("hyd_destination", 100, regression_data_2016, 2016)
local_GWR_PML_1_year("hyd_destination", 100, regression_data_2017, 2017)
# local_GWR_PML_1_year("hyd_source", 424271, weight_in = weights) # 424271
# local_GWR_PML_1_year("base_source", 700630, weight_in = weights) # 700630
# local_GWR_PML_1_year("base_destination", 49056, weight_in = weights) # 49056


# coef map by F1 scores
local_gwr_PML_coef_map_by_F1 <- function(local_GWR_coefs_list, PML_best_bw_tbl_, criteria, dep_var, indep_vars, alpha=0.1, n_drop, date_, year_) {
  indep_vars <- c("Intercept", indep_vars)
  coef_table <- tibble(id = PML_best_bw_tbl_$id, bw=PML_best_bw_tbl_[[criteria]])
  pval_table <- coef_table
  coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars))
  pval_mat <- coef_mat
  
  indep_vars_df <- data.frame(var_name=indep_vars)
  for (i in 1:nrow(coef_table)) {
    bw_i <- coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_model_i <- local_GWR_coefs_list[[i]][[paste0("bw_", bw_i)]]
    coef_i <- coef(local_GWR_model_i)
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i, p_value=local_GWR_model_i$prob)
    coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
    coef_mat[i,] <- coef_i_df$coef
    pval_mat[i,] <- coef_i_df$p_value
  }
  
  coef_table <- bind_cols(coef_table, coef_mat)
  pval_table <- bind_cols(pval_table, pval_mat)
  names(coef_table)[-(1:2)] <- indep_vars
  names(pval_table)[-(1:2)] <- indep_vars
  # write.csv(coef_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out %s all var drop %i %i data (%s).csv", dep_var, criteria, n_drop, year_, date_),
  #           row.names = F)
  # 
  # write.csv(pval_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value %s leave-one-out %s all var drop %i %i data (%s).csv", dep_var, criteria, n_drop, year_, date_),
  #           row.names = F)
  
  # for weighted reg
  write.csv(coef_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out %s all var drop %i weight 7-3 %i data (%s).csv", dep_var, criteria, n_drop, year_, date_),
            row.names = F)
  write.csv(pval_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value %s leave-one-out %s all var drop %i weight 7-3 %i data (%s).csv", dep_var, criteria, n_drop, year_, date_),
            row.names = F)
  
  for (i in c(2, 4:length(coef_table))) {
    var_name <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              coef=coef_table[[var_name]],
                              rounded_coef=coef_table[[var_name]] %>% round(3),
                              p_value=pval_table[[var_name]])
    min_coef <- min(gwr_coefs_i$coef, na.rm=T)
    max_coef <- max(gwr_coefs_i$coef, na.rm=T)
    coef_map_coords_bw <- map_df %>%
      left_join(gwr_coefs_i, by="id")
    # gwr_coefs_i$coef <- ifelse(gwr_coefs_i$p_value > alpha, NA, gwr_coefs_i$coef)
    coef_map_coords <- map_df %>%
      left_join(gwr_coefs_i, by="id")
    
    if (i == 2) {
      gwr_coef_map <- ggplot(coef_map_coords_bw, aes(x=long, y=lat)) +
        geom_polygon(aes(group=group, fill=coef),
                     color = "black",
                     linewidth = 0.1) +
        expand_limits(x = depto_map$long, y = depto_map$lat) +
        coord_quickmap() +
        scale_fill_viridis_c(na.value = "white") +
        labs(fill=var_name, x="", y="", title=dep_var) +
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
        geom_point(aes(x=long, y=lat), data=municipio_centroid %>% filter(id %in% (gwr_coefs_i %>% filter(p_value <= alpha) %>% pull(id))), size=0.7) + # add significant locations
        expand_limits(x = depto_map$long, y = depto_map$lat) +
        coord_quickmap() +
        scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                             values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                             na.value = "white") +
        labs(fill=var_name, x="", y="", title=dep_var) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
    }
    
    # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%i)/local GWR PML coef by drop %s %s %s all var drop %i %i data (%s).png", dep_var, year_, var_name, dep_var, criteria, n_drop, year_, date_),
    #        gwr_coef_map, scale=1)
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s weight 7-3 (%i)/local GWR PML coef by drop %s %s %s all var drop %i weight 7-3 %i data (%s).png", dep_var, year_, var_name, dep_var, criteria, n_drop, year_, date_),
           gwr_coef_map, scale=1)
  }
}

PML_F1_score <- function(PML_model_list, cv_aic_min_mat) {
  F1_mat <- cv_aic_min_mat
  for (i in 1:nrow(cv_aic_min_mat)) {
    id_i <- cv_aic_min_mat$id[i]
    PML_model_list_id <- PML_model_list[[i]]
    for (j in 1:length(PML_model_list_id)) {
      if (is.na(cv_aic_min_mat[i, j+1])) {
        F1_mat[i,j+1] <- NA
        next
      }
      
      PML_model_list_id_bw <- PML_model_list_id[[j]]
      PML_model_list_id_bw_y <- PML_model_list_id_bw$model$y %>% as.factor
      PML_model_list_id_bw_pred <- ifelse(PML_model_list_id_bw$predict < 0.5, 0, 1) %>% factor(levels = c("0", "1"))
      PML_model_list_id_bw_CM <- confusionMatrix(PML_model_list_id_bw_pred, PML_model_list_id_bw_y, positive = "1")
      F1_mat[i,j+1] <- PML_model_list_id_bw_CM$byClass[7] # F1 score
      
    }
  }
  return(F1_mat)
}

PML_F1_score_year <- function(dep_var_, year_) {
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
  # load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).RData", dep_var_, year_))
  # PML_gwr_aic_var_drop_log_seizure_coca <-
  #   read_csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).csv", dep_var_, year_)) %>% as_tibble
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data (01-27-2026).RData", dep_var_, year_))
  PML_gwr_aic_var_drop_log_seizure_coca <-
    read_csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out aic all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data (01-27-2026).csv", dep_var_, year_)) %>% as_tibble
  
  # PML_F1_score_var_drop_log_seizure <- PML_F1_score(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo, PML_gwr_aic_var_drop_log_seizure_coca)
  # write.csv(PML_F1_score_var_drop_log_seizure,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).csv", dep_var_, year_),
  #           row.names = F)
  # rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo)
  PML_F1_score_var_drop_log_seizure <- PML_F1_score(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3, PML_gwr_aic_var_drop_log_seizure_coca)
  write.csv(PML_F1_score_var_drop_log_seizure,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data (01-27-2026).csv", dep_var_, year_),
            row.names = F)
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
}

PML_F1_score_year("hyd_destination", 2016)
PML_F1_score_year("hyd_destination", 2017)
# PML_F1_score_year("hyd_source")
# PML_F1_score_year("base_destination")
# PML_F1_score_year("base_source")

## coef map by F1 var drop 
local_gwr_PML_coef_map_by_F1_year <- function(dep_var_, year_) {
  indep_vars_ <- names(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:3)]
  # PML_F1_score_var_drop_log_seizure_10_loo <-
  #   read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).csv", dep_var_, year_)) %>% as_tibble
  PML_F1_score_var_drop_log_seizure_10_loo <-
    read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data (01-27-2026).csv", dep_var_, year_)) %>% as_tibble
  
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
  # load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 %i data (01-27-2026).RData", dep_var_, year_))
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data (01-27-2026).RData", dep_var_, year_))
  
  PML_best_bw_tbl_var_drop  <- tibble(id = PML_F1_score_var_drop_log_seizure_10_loo$id,
                                      PML_log_seizure_coca_bw_F1 = PML_F1_score_var_drop_log_seizure_10_loo[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
  
  # local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo, PML_best_bw_tbl_var_drop, criteria="PML_log_seizure_coca_bw_F1", dep_var = dep_var_,
  #                              indep_vars = indep_vars_, n_drop=10, date_="01-27-2026", year_=year_)
  # rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo)
  local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3, PML_best_bw_tbl_var_drop, criteria="PML_log_seizure_coca_bw_F1", dep_var = dep_var_,
                               indep_vars = indep_vars_, n_drop=10, date_="01-27-2026", year_=year_)
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
}

local_gwr_PML_coef_map_by_F1_year("hyd_destination", 2016)
local_gwr_PML_coef_map_by_F1_year("hyd_destination", 2017)
# local_gwr_PML_coef_map_by_F1_year("hyd_source")
# local_gwr_PML_coef_map_by_F1_year("base_source")
# local_gwr_PML_coef_map_by_F1_year("base_destination")