#### seizures are last year data t-1
## bandwidth range: 0~4
indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual")
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

regression_data_CF_2013 <- read.csv("Colombia Data/regression data all municipios CF 2013.csv") %>% as_tibble
regression_data_CF_2014 <- read.csv("Colombia Data/regression data all municipios CF 2014.csv") %>% as_tibble
regression_data_CF_2016 <- read.csv("Colombia Data/regression data all municipios CF 2016.csv") %>% as_tibble
regression_data_CF_2017 <- read.csv("Colombia Data/regression data all municipios CF 2017.csv") %>% as_tibble

coord_unique <- left_join(regression_data_CF_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix

# PML_gwr_coefs_F1_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2016 data CF (02-04-2026).csv") %>% as_tibble
}

# the number of municipios with y=1
regression_data_CF_2013 %>% filter(hyd_destination == 1) %>% nrow # 157
regression_data_CF_2014 %>% filter(hyd_destination == 1) %>% nrow # 278
regression_data_CF_2016 %>% filter(hyd_destination == 1) %>% nrow # 123
regression_data_CF_2017 %>% filter(hyd_destination == 1) %>% nrow # 365

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

local_GWR_PML_CF <- function(type.measure_="default", F1_mat, sig_level_=0.05, gwr_PML_data_, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var, price_) {
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  coord_unique <- gwr_PML_data_$coord
  local_gwr_dist <- gwr_PML_data_$dist %>% as.matrix
  
  local_GWR_coefs_PML_result <- list()
  for (i in 1:nrow(F1_mat)) {
    id_i <- F1_mat$id[i]
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
        F1_mat[[bw_name]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
        next
      }
      
      n_unique_vals <- neighbor_ij %>% select(-id) %>% apply(2, function(x) length(table(x)))
      
      # variable drop
      if (n_unique_vals[["coca_area"]] < n_drop) {
        neighbor_ij$coca_area <- NULL
        n_unique_vals[["coca_area"]] <- n_drop
      }
      if (n_unique_vals[["seizures"]] < n_drop) {
        neighbor_ij$seizures <- NULL
        n_unique_vals[["seizures"]] <- n_drop
      }
      if (price_) {
        if (n_unique_vals[["price_avg"]] < n_drop) {
          neighbor_ij$price_avg <- NULL
          n_unique_vals[["price_avg"]] <- n_drop
        }
      }
      if (n_unique_vals[["river_length"]] < n_drop) {
        neighbor_ij$river_length <- NULL
        n_unique_vals[["river_length"]] <- n_drop
      }
      if (n_unique_vals[["road_length"]] < n_drop) {
        neighbor_ij$road_length <- NULL
        n_unique_vals[["road_length"]] <- n_drop
      }
      
      if (neighbor_ij$airport %>% table %>% min < n_drop | n_unique_vals[["airport"]] < 2) neighbor_ij$airport <- NULL
      if (neighbor_ij$armed_group %>% table %>% min < n_drop | n_unique_vals[["armed_group"]] < 2) neighbor_ij$armed_group <- NULL
      if (neighbor_ij$ferry %>% table %>% min < n_drop | n_unique_vals[["ferry"]] < 2) neighbor_ij$ferry <- NULL
      if (neighbor_ij$police %>% table %>% min < n_drop | n_unique_vals[["police"]] < 2) neighbor_ij$police <- NULL
      if (neighbor_ij$military %>% table %>% min < n_drop | n_unique_vals[["military"]] < 2) neighbor_ij$military <- NULL
      if (neighbor_ij$lab_reported %>% table %>% min < n_drop | n_unique_vals[["lab_reported"]] < 2) neighbor_ij$lab_reported <- NULL
      # variable drop end
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      result_i <- tryCatch(
        {
          PML_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)

          PML_result_ij_y <- PML_result_ij$model$y %>% as.factor
          PML_result_ij_pred <- ifelse(PML_result_ij$predict < 0.5, 0, 1) %>% factor(levels = c("0", "1"))
          PML_result_ij_CM <- confusionMatrix(PML_result_ij_pred, PML_result_ij_y, positive = "1")
          F1_mat[[paste0("bw_", bw_ij)]][i] <- PML_result_ij_CM$byClass[7] # F1 score

          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- PML_result_ij
        },
        error = function(e) {
          return(e)
        }
      )

      if (inherits(result_i, "error")) {
        F1_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      }
      
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(F1_mat=F1_mat,
              PML=local_GWR_coefs_PML_result))
}

local_GWR_PML_1_year <- function(dep_var_, seed_model, reg_data_year, year_, price=F, weight_in=NULL) {
  dep_var_index <- which(names(reg_data_year) == dep_var_)
  names(reg_data_year)[dep_var_index] <- "y"
  
  if (grepl("hyd", dep_var_)) {
    reg_data_year <- reg_data_year %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year <- reg_data_year %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (price) reg_data_year <- reg_data_year %>% select(-price_avg)
  title_for_price <- ifelse(price, "with price", "no price")
  
  gwr_data <- list(norm = reg_data_year, coord = coord_unique, dist = gwr_data_dist)

    ### use this if allow 2013-2016 only for y
  # reg_data_year <- left_join(gwr_data$norm %>% select(id, municipio, y), reg_data_year %>% select(-y), by="id")
  
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  F1_mat_ <- matrix(NA, 1120, 1+length(bwd_range))
  colnames(F1_mat_) <- c("id", paste0("bw_", bwd_range))
  F1_mat_ <- as_tibble(F1_mat_)
  F1_mat_$id <- gwr_data$norm$id
  
  set.seed(seed_model)
  local_GWR_coefs_PML_list <- local_GWR_PML_CF(dep_var = dep_var_, F1_mat=F1_mat_, gwr_PML_data_ = gwr_data, method_="var drop", sig_level_ = 0.1, n_drop = 10, weight_ = weight_in, price_=price)
  
  weight_in_1 <- weight_in[1]
  weight_in_0 <- weight_in[2]
  local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo <- local_GWR_coefs_PML_list$PML
  write.csv(local_GWR_coefs_PML_list$F1_mat,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 %i data %s CF (01-27-2026).csv",
                    dep_var_, year_, title_for_price), row.names = F)
  save("local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo",
       file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 %i data %s CF (01-27-2026).RData",
                      dep_var_, year_, title_for_price))
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo); rm(local_GWR_coefs_PML_list)

  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3 <- local_GWR_coefs_PML_list$PML
  # write.csv(local_GWR_coefs_PML_list$F1_mat,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight %i-%i %i data CF (01-27-2026).csv",
  #                   dep_var_, weight_in_1, weight_in_0, year_, title_for_price), row.names = F)
  # save("local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3",
  #      file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight %i-%i %i data CF (01-27-2026).RData",
  #                     dep_var_, weight_in_1, weight_in_0, year_, title_for_price))
  # rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3); rm(local_GWR_coefs_PML_list)
}

# Think about if weight_1 = 1 / positive ratio is a good idea
# weights <- c(7, 0.3) # 7 for postive outcomes
# sample(1:1000000, 4)
local_GWR_PML_1_year("hyd_destination", 178373, regression_data_CF_2013, 2013)
local_GWR_PML_1_year("hyd_destination", 284840, regression_data_CF_2014, 2014)
local_GWR_PML_1_year("hyd_destination", 531190, regression_data_CF_2016, 2016)
local_GWR_PML_1_year("hyd_destination", 749864, regression_data_CF_2017, 2017)
# local_GWR_PML_1_year("hyd_destination", 100, regression_data_2016, 2016, weight_in = weights)
# local_GWR_PML_1_year("hyd_destination", 100, regression_data_2016, 2016)
local_GWR_PML_1_year("hyd_destination", 100, regression_data_2017, 2017)
# local_GWR_PML_1_year("hyd_source", 424271, weight_in = weights) # 424271
# local_GWR_PML_1_year("base_source", 700630, weight_in = weights) # 700630
# local_GWR_PML_1_year("base_destination", 49056, weight_in = weights) # 49056


# coef map by F1 scores
local_gwr_PML_coef_map_by_F1 <- function(local_GWR_coefs_list, PML_best_bw_tbl_, criteria, dep_var, alpha=0.1, n_drop, date_, year_, indep_vars_, price) {
  coef_table <- tibble(id = PML_best_bw_tbl_$id, bw=PML_best_bw_tbl_[[criteria]])
  title_for_price <- ifelse(price, "with price", "no price")
  pval_table <- coef_table
  coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars_))
  pval_mat <- coef_mat
  
  indep_vars_df <- data.frame(var_name=indep_vars_)
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
  rm(local_GWR_coefs_list)
  
  coef_table <- bind_cols(coef_table, coef_mat)
  pval_table <- bind_cols(pval_table, pval_mat)
  names(coef_table)[-(1:2)] <- indep_vars_
  names(pval_table)[-(1:2)] <- indep_vars_
  write.csv(coef_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out %s all var drop %i %i data %s CF (%s).csv", dep_var, criteria, n_drop, year_, title_for_price, date_),
            row.names = F)

  write.csv(pval_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value %s leave-one-out %s all var drop %i %i data %s CF (%s).csv", dep_var, criteria, n_drop, year_, title_for_price, date_),
            row.names = F)
  
  # for weighted reg
  # write.csv(coef_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out %s all var drop %i weight 7-3 %i data %s CF (%s).csv", dep_var, criteria, n_drop, year_, title_for_price, date_),
  #           row.names = F)
  # write.csv(pval_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value %s leave-one-out %s all var drop %i weight 7-3 %i data %s CF (%s).csv", dep_var, criteria, n_drop, year_, title_for_price, date_),
  #           row.names = F)
  
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
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%i)/local GWR PML coef by drop %s %s %s all var drop %i %i data %s CF (%s).png",
                   dep_var, year_, var_name, dep_var, criteria, n_drop, year_, title_for_price, date_),
           gwr_coef_map, scale=1)
    
    # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s weight 7-3 (%i)/local GWR PML coef by drop %s %s %s all var drop %i weight 7-3 %i data %s CF (%s).png",
    #                dep_var, year_, var_name, dep_var, criteria, n_drop, year_, title_for_price, date_),
    #        gwr_coef_map, scale=1)
  }
}

## coef map by F1 var drop 
local_gwr_PML_coef_map_by_F1_year <- function(dep_var_, year_, price_=F) {
  if (price_) {
    indep_vars_in <- indep_vars
    title_for_price <- "with price"
  }else{
    indep_vars_in <- indep_vars[-which(indep_vars == "price_avg")]
    title_for_price <- "no price"
  } 
  indep_vars_in <- c("Intercept", indep_vars_in)
  
  PML_F1_score_var_drop_log_seizure_10_loo <-
    read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 %i data %s CF (01-27-2026).csv",
                     dep_var_,year_, title_for_price)) %>% as_tibble
  # PML_F1_score_var_drop_log_seizure_10_loo <-
  #   read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out F1 all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data CF %s (01-27-2026).csv",
  #                    dep_var_, year_, title_for_price)) %>% as_tibble
  
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 %i data %s CF (01-27-2026).RData",
               dep_var_, year_, title_for_price))
  # local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3
  # load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML %s leave-one-out all var drop log seizure coca scaled n_drop=10 weight 7-3 %i data %s CF (01-27-2026).RData",
  #              dep_var_, year_, title_for_price))
  
  PML_best_bw_tbl_var_drop  <- tibble(id = PML_F1_score_var_drop_log_seizure_10_loo$id,
                                      PML_log_seizure_coca_bw_F1 = PML_F1_score_var_drop_log_seizure_10_loo[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
  
  local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo, PML_best_bw_tbl_var_drop, criteria="PML_log_seizure_coca_bw_F1", dep_var = dep_var_,
                               indep_vars_ = indep_vars_in, n_drop=10, date_="02-04-2026", year_=year_, price=price_)
  rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo)
  # local_gwr_PML_coef_map_by_F1(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3, PML_best_bw_tbl_var_drop, criteria="PML_log_seizure_coca_bw_F1", dep_var = dep_var_,
  #                              indep_vars_ = indep_vars_in, n_drop=10, date_="02-04-2026", year_=year_, price=price_)
  # rm(local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo_7_3)
}

local_gwr_PML_coef_map_by_F1_year("hyd_destination", 2013, price_=F)
local_gwr_PML_coef_map_by_F1_year("hyd_destination", 2014, price_=F)
local_gwr_PML_coef_map_by_F1_year("hyd_destination", 2016, price_=F)
local_gwr_PML_coef_map_by_F1_year("hyd_destination", 2017, price_=F)
# local_gwr_PML_coef_map_by_F1_year("hyd_source")
# local_gwr_PML_coef_map_by_F1_year("base_source")
# local_gwr_PML_coef_map_by_F1_year("base_destination")

# prediction check
PML_gwr_coefs_F1_CF_2013 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2013 data no price CF (02-04-2026).csv") %>% as_tibble
PML_gwr_coefs_F1_CF_2014 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2014 data no price CF (02-04-2026).csv") %>% as_tibble
PML_gwr_coefs_F1_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2016 data no price CF (02-04-2026).csv") %>% as_tibble
PML_gwr_coefs_F1_CF_2017 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2017 data no price CF (02-04-2026).csv") %>% as_tibble

PML_gwr_coefs_F1_CF_2013 %>% filter(seizures > 100)

GWR_predict_year_CF <- function(PML_gwr_coefs, reg_data_year, dep_var, no_price, threshold=0.5) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual")
  dep_var_index <- which(names(reg_data_year) == dep_var)
  names(reg_data_year)[dep_var_index] <- "y"
  if (grepl("hyd", dep_var)) {
    reg_data_year_pred <- reg_data_year %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year_pred <- reg_data_year %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (no_price) reg_data_year_pred$price_avg <- NULL
  
  PML_gwr_coefs_mat <- PML_gwr_coefs[,-(1:3)]
  PML_gwr_coefs_mat[is.na(PML_gwr_coefs_mat)] <- 0
  reg_data_year_pred_mat <- reg_data_year_pred[,-(1:2)]
  bX_year <- PML_gwr_coefs$Intercept + apply(PML_gwr_coefs_mat * reg_data_year_pred_mat, 1, sum)
  pi_hat_year <- 1/(1+exp(-bX_year))
  result <- tibble(id = PML_gwr_coefs$id, pi_hat = pi_hat_year)
  result$y_pred <- ifelse(result$pi_hat < threshold, 0, 1) %>% as.factor
  result$y <- reg_data_year_pred$y
  
  return(result)
}

confusion_matrix_pred <- function(GWR_pred) {
  result <- confusionMatrix(GWR_pred$y_pred %>% as.factor, GWR_pred$y %>% as.factor, positive = "1")
  return(result)
}

ROC_pred <- function(GWR_pred) {
  result <- roc(GWR_pred$y, GWR_pred$pi_hat, positive = "1")
  return(result)
}

GWR_predict_2013_CF_with_2013_coef <- GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T)
GWR_predict_2014_CF_with_2014_coef <- GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T)
GWR_predict_2016_CF_with_2016_coef <- GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T)
GWR_predict_2017_CF_with_2017_coef <- GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T)

confusion_matrix_pred(GWR_predict_2013_CF_with_2013_coef)
confusion_matrix_pred(GWR_predict_2014_CF_with_2014_coef)
confusion_matrix_pred(GWR_predict_2016_CF_with_2016_coef)
confusion_matrix_pred(GWR_predict_2017_CF_with_2017_coef)

ROC_2013_CF_with_2013_coef <- ROC_pred(GWR_predict_2013_CF_with_2013_coef)
ROC_2014_CF_with_2014_coef <- ROC_pred(GWR_predict_2014_CF_with_2014_coef)
ROC_2016_CF_with_2016_coef <- ROC_pred(GWR_predict_2016_CF_with_2016_coef)
ROC_2017_CF_with_2017_coef <- ROC_pred(GWR_predict_2017_CF_with_2017_coef)

plot(ROC_2013_CF_with_2013_coef)
plot(ROC_2014_CF_with_2014_coef)
plot(ROC_2016_CF_with_2016_coef)
plot(ROC_2017_CF_with_2017_coef)

png("Colombia Data/local GWR PML result predicted prices/roc curves CF/roc curve GWR hyd destinations 2013.png")
plot(ROC_2013_CF_with_2013_coef, main="hyd destination - predictions 2013"); text(0.1, 0, paste("AUC:", round(ROC_2013_CF_with_2013_coef$auc, 2)))
dev.off()
png("Colombia Data/local GWR PML result predicted prices/roc curves CF/roc curve GWR hyd destinations 2014.png")
plot(ROC_2014_CF_with_2014_coef, main="hyd destination - predictions 2014"); text(0.1, 0, paste("AUC:", round(ROC_2014_CF_with_2014_coef$auc, 2)))
dev.off()
png("Colombia Data/local GWR PML result predicted prices/roc curves CF/roc curve GWR hyd destinations 2016.png")
plot(ROC_2016_CF_with_2016_coef, main="hyd destination - predictions 2016"); text(0.1, 0, paste("AUC:", round(ROC_2016_CF_with_2016_coef$auc, 2)))
dev.off()
png("Colombia Data/local GWR PML result predicted prices/roc curves CF/roc curve GWR hyd destinations 2017.png")
plot(ROC_2017_CF_with_2017_coef, main="hyd destination - predictions 2017"); text(0.1, 0, paste("AUC:", round(ROC_2017_CF_with_2017_coef$auc, 2)))
dev.off()

GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1) %>% mutate(y = as.factor(y)) %>% 
  ggplot(aes(x=pi_hat, y=y)) + geom_point() + labs(title="predicted probabilities for y=1 cases 2013")
GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1) %>% mutate(y = as.factor(y)) %>% 
  ggplot(aes(x=pi_hat, y=y)) + geom_point() + labs(title="predicted probabilities for y=1 cases 2014")
GWR_predict_2016_CF_with_2016_coef %>% filter(y == 1) %>% mutate(y = as.factor(y)) %>% 
  ggplot(aes(x=pi_hat, y=y)) + geom_point() + labs(title="predicted probabilities for y=1 cases 2016")
GWR_predict_2017_CF_with_2017_coef %>% filter(y == 1) %>% mutate(y = as.factor(y)) %>% 
  ggplot(aes(x=pi_hat, y=y)) + geom_point() + labs(title="predicted probabilities for y=1 cases 2017")

threshold_tables <- function(ROC_obs, specificity_limit) {
  result <- tibble(thresholds = ROC_obs$thresholds,
                   sensitivities = ROC_obs$sensitivities,
                   specificities = ROC_obs$specificities)
  result <- result %>% filter(specificities > specificity_limit) %>% arrange(desc(sensitivities))
  return(result)
}
threshold_tables(ROC_2013_CF_with_2013_coef, 0.6) # 0.0944
threshold_tables(ROC_2014_CF_with_2014_coef, 0.6) # 0.167
threshold_tables(ROC_2016_CF_with_2016_coef, 0.6) # 0.0482
threshold_tables(ROC_2017_CF_with_2017_coef, 0.6) # 0.324 

# 2013 pred
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T, threshold = 0.0944))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.0944))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.5))

roc_curve_pred <- ROC_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.5))
png("Colombia Data/local GWR PML result predicted prices/roc curves CF/roc curve GWR predictions of hyd destinations in 2014 with 2013.png")
plot(roc_curve_pred, main="hyd destination - predictions in 2014 with 2013 data"); text(0.1, 0, paste("AUC:", round(roc_curve_pred$auc, 2)))
dev.off()

confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.0944))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.5))

ROC_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.5)) %>% plot

confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.0944))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))

ROC_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5)) %>% plot

# 2014 pred
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.167))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.5))

confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.167))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.5))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.5))

ROC_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.5)) %>% plot

confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.167))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))

ROC_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5)) %>% plot

# 2016 pred
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.0482))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.5))

confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.0482))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))

roc_curve_pred <- ROC_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.5))
png("Colombia Data/local GWR PML result predicted prices/roc curves CF/roc curve GWR predictions of hyd destinations in 2017 with 2016.png")
plot(roc_curve_pred, main="hyd destination - predictions in 2017 with 2016 data"); text(0.1, 0, paste("AUC:", round(roc_curve_pred$auc, 2)))
dev.off()

# CM with smaller thresholds
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T, threshold = 0.0944))$byClass
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.167))$byClass
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.0482))$byClass
confusion_matrix_pred(GWR_predict_year_CF(PML_gwr_coefs_F1_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.324))$byClass

# prediction with global models
global_reg_year_CF <- function(reg_data_year, dep_var, no_price) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual")
  dep_var_index <- which(names(reg_data_year) == dep_var)
  names(reg_data_year)[dep_var_index] <- "y"
  if (grepl("hyd", dep_var)) {
    reg_data_year_pred <- reg_data_year %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year_pred <- reg_data_year %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (no_price) reg_data_year_pred$price_avg <- NULL
  
  global_reg_model_CF_year <- glm(y~., data=reg_data_year_pred %>% select(-id), family=binomial)
  return(global_reg_model_CF_year)
}

global_reg_prediction_year_CF <- function(global_model_year, reg_data_year, dep_var, no_price, threshold=0.5) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual")
  dep_var_index <- which(names(reg_data_year) == dep_var)
  names(reg_data_year)[dep_var_index] <- "y"
  if (grepl("hyd", dep_var)) {
    reg_data_year_pred <- reg_data_year %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year_pred <- reg_data_year %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (no_price) reg_data_year_pred$price_avg <- NULL
  
  result <- tibble(id = reg_data_year_pred$id, pi_hat = predict(global_model_year, reg_data_year_pred, type="response"))
  result$y_pred <- ifelse(result$pi_hat < threshold, 0, 1) %>% as.factor
  result$y <- reg_data_year_pred$y
  
  return(result)
}

global_reg_coefs_CF_2013 <- global_reg_year_CF(regression_data_CF_2013, "hyd_destination", no_price=T)
global_reg_coefs_CF_2014 <- global_reg_year_CF(regression_data_CF_2014, "hyd_destination", no_price=T)
global_reg_coefs_CF_2016 <- global_reg_year_CF(regression_data_CF_2016, "hyd_destination", no_price=T)
global_reg_coefs_CF_2017 <- global_reg_year_CF(regression_data_CF_2017, "hyd_destination", no_price=T)

global_predict_2016_CF_with_2016_coef <- global_reg_prediction_year_CF(global_reg_coefs_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T)
comparison_with_GWR_2016 <- left_join(GWR_predict_2016_CF_with_2016_coef, global_predict_2016_CF_with_2016_coef %>% mutate(pi_hat_global = pi_hat) %>% select(id, pi_hat_global), by="id")
comparison_with_GWR_2016

ROC_2013_CF_with_2013_coef$auc
ROC_2014_CF_with_2014_coef$auc
ROC_2016_CF_with_2016_coef$auc
ROC_2017_CF_with_2017_coef$auc

global_reg_prediction_year_CF(global_reg_coefs_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T) %>% ROC_pred %>% auc
global_reg_prediction_year_CF(global_reg_coefs_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T) %>% ROC_pred %>% auc
global_reg_prediction_year_CF(global_reg_coefs_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T) %>% ROC_pred %>% auc
global_reg_prediction_year_CF(global_reg_coefs_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T) %>% ROC_pred %>% auc

global_reg_prediction_year_CF(global_reg_coefs_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T) %>% ROC_pred %>% plot
global_reg_prediction_year_CF(global_reg_coefs_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T) %>% ROC_pred %>% plot
global_reg_prediction_year_CF(global_reg_coefs_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T) %>% ROC_pred %>% plot
global_reg_prediction_year_CF(global_reg_coefs_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T) %>% ROC_pred %>% plot

global_reg_prediction_year_CF(global_reg_coefs_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T) %>% ROC_pred %>% threshold_tables(0.6) # 0.125
global_reg_prediction_year_CF(global_reg_coefs_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T) %>% ROC_pred %>% threshold_tables(0.6) # 0.240
global_reg_prediction_year_CF(global_reg_coefs_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T) %>% ROC_pred %>% threshold_tables(0.6) # 0.066
global_reg_prediction_year_CF(global_reg_coefs_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T) %>% ROC_pred %>% threshold_tables(0.6) # 0.302

confusion_matrix_pred(global_reg_prediction_year_CF(global_reg_coefs_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T, threshold = 0.125))$byClass
confusion_matrix_pred(global_reg_prediction_year_CF(global_reg_coefs_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T, threshold = 0.24))$byClass
confusion_matrix_pred(global_reg_prediction_year_CF(global_reg_coefs_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T, threshold = 0.066))$byClass
confusion_matrix_pred(global_reg_prediction_year_CF(global_reg_coefs_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T, threshold = 0.302))$byClass

# variable influence (bX)
influential_var <- function(GWR_coef, reg_data_year_in, dep_var, no_price) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual")
  dep_var_index <- which(names(reg_data_year_in) == dep_var)
  names(reg_data_year_in)[dep_var_index] <- "y"
  if (grepl("hyd", dep_var)) {
    reg_data_year <- reg_data_year_in %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year <- reg_data_year_in %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (no_price) {
    reg_data_year$price_avg <- NULL 
  }
  
  reg_data_year <- reg_data_year %>% arrange(id)
  coefs_data <- GWR_coef %>% filter(id %in% reg_data_year$id) %>% arrange(id) %>% select(Intercept, any_of(indep_vars))
  
  coefs_data[is.na(coefs_data)] <- 0
  influences <- as_tibble(as.matrix(coefs_data)[,-1] * as.matrix(reg_data_year %>% select(-id, -y))) %>% 
    mutate(id = reg_data_year$id, Intercept = coefs_data$Intercept) %>% relocate(id, Intercept)
  return(influences)
}

variable_influence_CF_2013 <- influential_var(PML_gwr_coefs_F1_CF_2013, regression_data_CF_2013, "hyd_destination", no_price=T)
variable_influence_CF_2014 <- influential_var(PML_gwr_coefs_F1_CF_2014, regression_data_CF_2014, "hyd_destination", no_price=T)
variable_influence_CF_2016 <- influential_var(PML_gwr_coefs_F1_CF_2016, regression_data_CF_2016, "hyd_destination", no_price=T)
variable_influence_CF_2017 <- influential_var(PML_gwr_coefs_F1_CF_2017, regression_data_CF_2017, "hyd_destination", no_price=T)

variable_influence_map_CF <- function(variable_influence_CF_year, selected_id=NULL, dep_var, n_drop, year_, price) {
  title_for_price <- ifelse(price, "with price", "no price")
  
  for (i in c(2:length(variable_influence_CF_year))) {
    var_name <- names(variable_influence_CF_year)[i]
    gwr_influence_i <- data.frame(id=variable_influence_CF_year$id,
                                  influence=variable_influence_CF_year[[var_name]])
    if (!is.null(selected_id)) gwr_influence_i <- gwr_influence_i %>% filter(id %in% selected_id)
    
    min_coef <- min(gwr_influence_i$influence, na.rm=T)
    max_coef <- max(gwr_influence_i$influence, na.rm=T)
    coef_map_coords_bw <- map_df %>%
      left_join(gwr_influence_i, by="id")
    coef_map_coords <- map_df %>%
      left_join(gwr_influence_i, by="id")
    
    gwr_VI_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group, fill=influence),
                   color = "black",
                   linewidth = 0.1) +
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
    
    if (is.null(selected_id)) {
      ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/VI maps CF/%s (%i)/local GWR PML variable influence %s %s all var drop %i %i data %s CF.png",
                     dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price),
             gwr_VI_map, scale=1)
    }else{
      ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/VI maps CF (y=1)/%s (%i)/local GWR PML variable influence %s %s all var drop %i %i data %s CF.png",
                     dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price),
             gwr_VI_map, scale=1)
    }
  }
}

variable_influence_map_CF(variable_influence_CF_2013,
                          selected_id = GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1) %>% pull(id),
                          dep_var = "hyd_destination",
                          n_drop = 10,
                          year_ = 2013,
                          price = F)
variable_influence_map_CF(variable_influence_CF_2014,
                          selected_id = GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1) %>% pull(id),
                          dep_var = "hyd_destination",
                          n_drop = 10,
                          year_ = 2014,
                          price = F)
variable_influence_map_CF(variable_influence_CF_2016,
                          selected_id = GWR_predict_2016_CF_with_2016_coef %>% filter(y == 1) %>% pull(id),
                          dep_var = "hyd_destination",
                          n_drop = 10,
                          year_ = 2016,
                          price = F)
variable_influence_map_CF(variable_influence_CF_2017,
                          selected_id = GWR_predict_2017_CF_with_2017_coef %>% filter(y == 1) %>% pull(id),
                          dep_var = "hyd_destination",
                          n_drop = 10,
                          year_ = 2017,
                          price = F)
threshold_tables(ROC_2013_CF_with_2013_coef, 0.8) # 0.0944
PML_gwr_coefs_F1_CF_2013 %>% filter(id %in% (GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1 & pi_hat > 0.9) %>% pull(id))) %>% arrange(id) %>% print(n=100)
PML_gwr_coefs_F1_CF_2013 %>% filter(id %in% (GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1 & pi_hat < 0.1) %>% pull(id))) %>% arrange(id) %>% print(n=100)

variable_influence_CF_2013 %>% filter(id %in% (GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1 & pi_hat > 0.9) %>% pull(id))) %>% arrange(id) %>% print(n=100)
variable_influence_CF_2013 %>% filter(id %in% (GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1 & pi_hat < 0.1) %>% pull(id))) %>% arrange(id) %>% print(n=100)

variable_influence_CF_2014 %>% filter(id %in% (GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1 & pi_hat > 0.9) %>% pull(id))) %>% arrange(id) %>% print(n=100)
variable_influence_CF_2014 %>% filter(id %in% (GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1 & pi_hat < 0.1) %>% pull(id))) %>% arrange(id) %>% print(n=100)

variable_influence_CF_2016 %>% filter(id %in% (GWR_predict_2016_CF_with_2016_coef %>% filter(y == 1 & pi_hat > 0.9) %>% pull(id))) %>% arrange(id) %>% print(n=100)
variable_influence_CF_2016 %>% filter(id %in% (GWR_predict_2016_CF_with_2016_coef %>% filter(y == 1 & pi_hat < 0.1) %>% pull(id))) %>% arrange(id) %>% print(n=100)

variable_influence_CF_2017 %>% filter(id %in% (GWR_predict_2017_CF_with_2017_coef %>% filter(y == 1 & pi_hat > 0.9) %>% pull(id))) %>% arrange(id) %>% print(n=100)
variable_influence_CF_2017 %>% filter(id %in% (GWR_predict_2017_CF_with_2017_coef %>% filter(y == 1 & pi_hat < 0.1) %>% pull(id))) %>% arrange(id) %>% print(n=100)

regression_data_CF_2013 %>% filter(hyd_destination == 1) %>% pull(hyd_lab) %>% table
regression_data_CF_2014 %>% filter(hyd_destination == 1) %>% pull(hyd_lab) %>% table
regression_data_CF_2016 %>% filter(hyd_destination == 1) %>% pull(hyd_lab) %>% table
regression_data_CF_2017 %>% filter(hyd_destination == 1) %>% pull(hyd_lab) %>% table

threshold_tables(ROC_2014_CF_with_2014_coef, 0.8) # 0.0944
PML_gwr_coefs_F1_CF_2014 %>% filter(id %in% (GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1 & pi_hat < 0.458) %>% pull(id)))
variable_influence_CF_2014 %>% filter(id %in% (GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1 & pi_hat < 0.458) %>% pull(id)))



# hyd_dest maps
hyd_dest_2013_map_df <- left_join(map_df, regression_data_CF_2013 %>% select(id, hyd_destination), by="id") %>% mutate(hyd_destination = as.factor(hyd_destination))
hyd_dest_2014_map_df <- left_join(map_df, regression_data_CF_2014 %>% select(id, hyd_destination), by="id") %>% mutate(hyd_destination = as.factor(hyd_destination))
hyd_dest_2016_map_df <- left_join(map_df, regression_data_CF_2016 %>% select(id, hyd_destination), by="id") %>% mutate(hyd_destination = as.factor(hyd_destination))
hyd_dest_2017_map_df <- left_join(map_df, regression_data_CF_2017 %>% select(id, hyd_destination), by="id") %>% mutate(hyd_destination = as.factor(hyd_destination))

hyd_dest_2013_map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=hyd_destination),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = hyd_dest_2013_map_df$long, y = hyd_dest_2013_map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination 2013") +
  scale_fill_manual(
    values = c("1"="red", "0"="white"),
    na.value = "white"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_map_2013

hyd_dest_2014_map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=hyd_destination),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = hyd_dest_2014_map_df$long, y = hyd_dest_2014_map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination 2014") +
  scale_fill_manual(
    values = c("1"="red", "0"="white"),
    na.value = "white"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_map_2014

hyd_dest_2016_map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=hyd_destination),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = hyd_dest_2016_map_df$long, y = hyd_dest_2016_map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination 2016") +
  scale_fill_manual(
    values = c("1"="red", "0"="white"),
    na.value = "white"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_map_2016

hyd_dest_2017_map_df %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=hyd_destination),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = hyd_dest_2017_map_df$long, y = hyd_dest_2017_map_df$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination 2017") +
  scale_fill_manual(
    values = c("1"="red", "0"="white"),
    na.value = "white"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_map_2017

# ggsave("Colombia Data/Figs/hyd destination map (2013).png", hyd_dest_map_2013, scale=1)
# ggsave("Colombia Data/Figs/hyd destination map (2014).png", hyd_dest_map_2014, scale=1)
# ggsave("Colombia Data/Figs/hyd destination map (2016).png", hyd_dest_map_2016, scale=1)
# ggsave("Colombia Data/Figs/hyd destination map (2017).png", hyd_dest_map_2017, scale=1)

GWR_predict_2013_CF_with_2013_map_1 <- left_join(map_df, GWR_predict_2013_CF_with_2013_coef %>% filter(y == 1) %>% select(id, pi_hat), by="id")
GWR_predict_2013_CF_with_2013_map_1 %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=pi_hat),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = GWR_predict_2013_CF_with_2013_map_1$long, y = GWR_predict_2013_CF_with_2013_map_1$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination predicted probabilities 2013") +
  scale_fill_gradient(
    low = "white",
    high = "red",
    na.value = "grey60"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_pi_hat_map_2013

GWR_predict_2014_CF_with_2014_map_1 <- left_join(map_df, GWR_predict_2014_CF_with_2014_coef %>% filter(y == 1) %>% select(id, pi_hat), by="id")
GWR_predict_2014_CF_with_2014_map_1 %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=pi_hat),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = GWR_predict_2014_CF_with_2014_map_1$long, y = GWR_predict_2014_CF_with_2014_map_1$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination predicted probabilities 2014") +
  scale_fill_gradient(
    low = "white",
    high = "red",
    na.value = "grey60"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_pi_hat_map_2014

GWR_predict_2016_CF_with_2016_map_1 <- left_join(map_df, GWR_predict_2016_CF_with_2016_coef %>% filter(y == 1) %>% select(id, pi_hat), by="id")
GWR_predict_2016_CF_with_2016_map_1 %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=pi_hat),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = GWR_predict_2016_CF_with_2016_map_1$long, y = GWR_predict_2016_CF_with_2016_map_1$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination predicted probabilities 2016") +
  scale_fill_gradient(
    low = "white",
    high = "red",
    na.value = "grey60"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_pi_hat_map_2016

GWR_predict_2017_CF_with_2017_map_1 <- left_join(map_df, GWR_predict_2017_CF_with_2017_coef %>% filter(y == 1) %>% select(id, pi_hat), by="id")
GWR_predict_2017_CF_with_2017_map_1 %>% ggplot(aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=pi_hat),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = GWR_predict_2017_CF_with_2017_map_1$long, y = GWR_predict_2017_CF_with_2017_map_1$lat) +
  coord_quickmap() +
  labs(fill="", x="", y="", title="hyd_destination predicted probabilities 2017") +
  scale_fill_gradient(
    low = "white",
    high = "red",
    na.value = "grey60"
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  ) -> hyd_dest_pi_hat_map_2017

# ggsave("Colombia Data/local GWR PML result predicted prices/VI maps CF (y=1)/hyd destination predicted probabilities (2013).png", hyd_dest_pi_hat_map_2013, scale=1)
# ggsave("Colombia Data/local GWR PML result predicted prices/VI maps CF (y=1)/hyd destination predicted probabilities (2014).png", hyd_dest_pi_hat_map_2014, scale=1)
# ggsave("Colombia Data/local GWR PML result predicted prices/VI maps CF (y=1)/hyd destination predicted probabilities (2016).png", hyd_dest_pi_hat_map_2016, scale=1)
# ggsave("Colombia Data/local GWR PML result predicted prices/VI maps CF (y=1)/hyd destination predicted probabilities (2017).png", hyd_dest_pi_hat_map_2017, scale=1)
