#### seizures are last year data t-1
## bandwidth range: 0~4
indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary")
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
  regression_data_CF_1617 <- read.csv("Colombia Data/regression data all municipios CF 1617.csv") %>% as_tibble
  
  coord_unique <- left_join(regression_data_CF_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  regression_data_CF_2013$hyd_seizures <- round(regression_data_CF_2013$hyd_seizures, 2)
  regression_data_CF_2014$hyd_seizures <- round(regression_data_CF_2014$hyd_seizures, 2)
  regression_data_CF_2016$hyd_seizures <- round(regression_data_CF_2016$hyd_seizures, 2)
  regression_data_CF_2017$hyd_seizures <- round(regression_data_CF_2017$hyd_seizures, 2)
  regression_data_CF_1617$hyd_seizures <- round(regression_data_CF_1617$hyd_seizures, 2)
  
  collapsed_armed_group <- tibble(id = regression_data_CF_2013$id,
                                  armed_group_2013 = regression_data_CF_2013$armed_group,
                                  armed_group_2014 = regression_data_CF_2014$armed_group,
                                  armed_group_2016 = regression_data_CF_2016$armed_group,
                                  armed_group_2017 = regression_data_CF_2017$armed_group) %>% 
    apply(1, function(x) ifelse(sum(x[2:5]) > 0, 1, 0))
  
  regression_data_CF_2013$armed_group <- collapsed_armed_group
  regression_data_CF_2014$armed_group <- collapsed_armed_group
  regression_data_CF_2016$armed_group <- collapsed_armed_group
  regression_data_CF_2017$armed_group <- collapsed_armed_group
  
  violence_all <- read.csv("Colombia Data/violence with id (all).csv") %>% as_tibble %>% filter(!is.na(id))
  violence_combined <- violence_all %>%
    mutate(Guerrilla = ifelse(Guerrilla == "yes", 1 , 0),
           FARC = ifelse(FARC == "yes", 1 , 0),
           ELN = ifelse(ELN == "yes", 1 , 0),
           AUC = ifelse(AUC == "yes", 1 , 0),
           Front = ifelse(Front != -1, 1 , 0),
           Bloque = ifelse(Bloque != -1, 1 , 0)) %>% 
    group_by(id) %>% # removed year under the assumption that paramilitary and guerrilla groups do not relocate that much
    summarize(Guerrilla = ifelse(any(Guerrilla == 1), 1, 0),
              FARC = ifelse(any(FARC == 1), 1, 0),
              ELN = ifelse(any(ELN == 1), 1, 0),
              AUC = ifelse(any(AUC == 1), 1, 0),
              Front = ifelse(any(Front == 1), 1, 0),
              Bloque = ifelse(any(Bloque == 1), 1, 0)) %>% ungroup %>% 
    right_join(regression_data_CF_2016 %>% select(id, armed_group) %>% rename(paramilitary = armed_group), by="id") %>% 
    mutate(left_wing = if_any(c(Guerrilla:ELN, Front), ~ . == 1) %>% as.numeric,
           right_paramilitary = if_any(c(AUC, Bloque, paramilitary), ~ . == 1) %>%  as.numeric)
  violence_combined[is.na(violence_combined)] <- 0
  
  regression_data_CF_2013 <- regression_data_CF_2013 %>% select(-armed_group) %>% left_join(violence_combined %>% select(id, left_wing, right_paramilitary), by = "id")
  regression_data_CF_2014 <- regression_data_CF_2014 %>% select(-armed_group) %>% left_join(violence_combined %>% select(id, left_wing, right_paramilitary), by = "id")
  regression_data_CF_2016 <- regression_data_CF_2016 %>% select(-armed_group) %>% left_join(violence_combined %>% select(id, left_wing, right_paramilitary), by = "id")
  regression_data_CF_2017 <- regression_data_CF_2017 %>% select(-armed_group) %>% left_join(violence_combined %>% select(id, left_wing, right_paramilitary), by = "id")
  regression_data_CF_1617 <- regression_data_CF_1617 %>% select(-armed_group) %>% left_join(violence_combined %>% select(id, left_wing, right_paramilitary), by = "id")
  
  hyd_lab_glm_year <- glm(hyd_lab~.+left_wing*right_paramilitary, family=binomial(link="probit"),
                          data=regression_data_CF_1617 %>% select(hyd_lab, coca_area, hyd_seizures, river_length:right_paramilitary))
  summary(hyd_lab_glm_year)
  z_vals <- predict(hyd_lab_glm_year, type="link")
  Inv_Mill_1 <- dnorm(z_vals) / pnorm(z_vals)
  Inv_Mill_0 <- -dnorm(z_vals) / (1-pnorm(z_vals))
  
  hyd_lab_res <- numeric(nrow(regression_data_CF_1617))
  pos_index <- which(regression_data_CF_1617$hyd_lab == "1")
  neg_index <- which(regression_data_CF_1617$hyd_lab == "0")
  hyd_lab_res[pos_index] <- Inv_Mill_1[pos_index]
  hyd_lab_res[neg_index] <- Inv_Mill_0[neg_index]
  regression_data_CF_1617$hyd_lab_res <- hyd_lab_res
}

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

## two-year combined

lasso_local_model <- function(x_mat, y_vec, measure, nfolds.=10, w=NULL, lambda=F, interact=F) {
  id_cv.glmnet <- cv.glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            alpha = 1,
                            nfolds = nfolds.,
                            weights = w,
                            type.measure = measure)
  lambda_lasso <- ifelse(lambda, lambda, id_cv.glmnet$lambda.min)
  lasso_result_id <- glmnet(x = x_mat,
                            y = y_vec,
                            family = "binomial",
                            alpha = 1,
                            weights = w,
                            lambda = lambda_lasso)
  return(list(cv=id_cv.glmnet, lasso=lasso_result_id, x_mat=x_mat))
}

local_GWR_LASSO_CF_2years_combined <- function(type.measure_="default", AUC_mat, sig_level_=0.05, gwr_LASSO_data_1, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var, price_) {
  # type.measure_="default"; AUC_mat=AUC_mat_; sig_level_=0.1; gwr_LASSO_data_1=gwr_data1; method_="var drop"; n_drop=10; interact_=F; scale_11_=F; weight_=NULL; dep_var="hyd_destination"; price_=F
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  coord_unique <- gwr_LASSO_data_1$coord
  local_gwr_dist <- gwr_LASSO_data_1$dist %>% as.matrix
  
  local_GWR_coefs_LASSO_result <- list()
  for (i in 1:nrow(AUC_mat)) {
    id_i <- AUC_mat$id[i]
    local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]] <- list()
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      bw_name <- paste0("bw_", bw_ij)
      local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[bw_name]] <- NA
      
      neighbor_ij <- neighbor_id(id_i, bw_ij, scale_11_, gwr_data_=gwr_LASSO_data_1) %>% filter(id != id_i) #### leave a focal point out
      n_0_1 <- neighbor_ij$y %>% table
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      result_i <- tryCatch(
        {
          x_mat_ <- model.matrix(as.formula(y~.+left_wing*right_paramilitary), neighbor_ij %>% select(-id))[, -1]
          y_vec_ <- neighbor_ij$y
          LASSO_result_ij <- lasso_local_model(x_mat_, y_vec_, type.measure_)
          LASSO_result_coef_ij <- coef(LASSO_result_ij$lasso)
          
          if (is.na(LASSO_result_coef_ij["lab_reported"] | is.na(LASSO_result_coef_ij["lab_residual"]))) {
            x_mat_ <- model.matrix(as.formula(y~.+left_wing*right_paramilitary), neighbor_ij %>% select(-id, -lab_reported, -lab_residual))[, -1]
            y_vec_ <- neighbor_ij$y
            LASSO_result_ij <- lasso_local_model(x_mat_, y_vec_, type.measure_)
          }
          
          LASSO_result_ij_y <- y_vec_ %>% as.factor
          LASSO_result_ij_pi_hat <- predict(LASSO_result_ij$lasso, x_mat_, type="response")[,1]
          LASSO_result_ij_pi_hat_tbl <- tibble(y=LASSO_result_ij_y, pi_hat=LASSO_result_ij_pi_hat)
          LASSO_result_ij_ROC <- ROC_pred(LASSO_result_ij_pi_hat_tbl)
          AUC_mat[[paste0("bw_", bw_ij)]][i] <- LASSO_result_ij_ROC$auc # AUC
          
          local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- LASSO_result_ij
        },
        error = function(e) {
          return(e)
        }
      )
      
      if (inherits(result_i, "error")) {
        AUC_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      }
      
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(AUC_mat=AUC_mat,
              LASSO=local_GWR_coefs_LASSO_result))
}

local_GWR_LASSO_2_years_combined_input <- function(dep_var_, seed_model, reg_data_year1, year_, price=F, weight_in=NULL) {
  # dep_var_= "hyd_destination"; seed_model=531190; reg_data_year1=regression_data_CF_1617; year_="2016-2017"; price=F; weight_in=NULL
  dep_var_index <- which(names(reg_data_year1) == dep_var_)
  names(reg_data_year1)[dep_var_index] <- "y"
  
  if (grepl("hyd", dep_var_)) {
    reg_data_year1 <- reg_data_year1 %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year1 <- reg_data_year1 %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (!price) {
    reg_data_year1 <- reg_data_year1 %>% select(-price_avg)
  }
  title_for_price <- ifelse(price, "with price", "no price")
  
  gwr_data1 <- list(norm = reg_data_year1, coord = coord_unique, dist = gwr_data_dist)
  
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  AUC_mat_ <- matrix(NA, 1120, 1+length(bwd_range))
  colnames(AUC_mat_) <- c("id", paste0("bw_", bwd_range))
  AUC_mat_ <- as_tibble(AUC_mat_)
  AUC_mat_$id <- gwr_data1$norm$id
  
  set.seed(seed_model)
  local_GWR_coefs_LASSO_list <- local_GWR_LASSO_CF_2years_combined(dep_var = dep_var_, AUC_mat=AUC_mat_, gwr_LASSO_data_1 = gwr_data1, method_="var drop",
                                                                   sig_level_ = 0.1, n_drop = 10, weight_ = weight_in, price_=price)
  
  weight_in_1 <- weight_in[1]
  weight_in_0 <- weight_in[2]
  local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo <- local_GWR_coefs_LASSO_list$LASSO
  write.csv(local_GWR_coefs_LASSO_list$AUC_mat,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right AUC n_drop=10 %s data combined %s CF (05-12-2026).csv",
                    dep_var_, year_, title_for_price), row.names = F)
  save("local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo",
       file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right by AUC n_drop=10 %s data combined %s CF (05-12-2026).RData",
                      dep_var_, year_, title_for_price))
  rm(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo); rm(local_GWR_coefs_LASSO_list)
}

local_GWR_LASSO_2_years_combined_input("hyd_destination", 531190, regression_data_CF_1617, "2016-2017")


# coef map by AUC scores
local_gwr_LASSO_coef_map_by_AUC <- function(local_GWR_coefs_list, LASSO_best_bw_tbl_, criteria, dep_var, alpha=0.1, n_drop, year_, indep_vars_, price) {
  # local_GWR_coefs_list=local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo; LASSO_best_bw_tbl_=LASSO_best_bw_tbl_var_drop; criteria="LASSO_log_seizure_coca_bw_AUC"; alpha=0.1; n_drop=10; year_="2016-2017"; indep_vars_=indep_vars_in; price=F
  indep_vars_ <- c(indep_vars_, "left_wing:right_paramilitary")
  coef_table <- tibble(id = LASSO_best_bw_tbl_$id, bw=LASSO_best_bw_tbl_[[criteria]])
  title_for_price <- ifelse(price, "with price", "no price")
  # pval_table <- coef_table
  coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars_))
  # pval_mat <- coef_mat
  
  indep_vars_df <- data.frame(var_name=indep_vars_)
  for (i in 1:nrow(coef_table)) {
    bw_i <- coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_model_i <- local_GWR_coefs_list[[i]][[paste0("bw_", bw_i)]]$lasso
    # local_GWR_model_i_p_val <- summary(local_GWR_model_i)$coefficients[,"Pr(>|z|)"]
    # p_val_i <- data.frame(var_name = c("Intercept", names(local_GWR_model_i_p_val)[-1]), p_value = local_GWR_model_i_p_val)
    coef_i <- coef(local_GWR_model_i)[,1]
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
    coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")# %>% left_join(p_val_i, by="var_name")
    coef_mat[i,] <- coef_i_df$coef
    # pval_mat[i,] <- coef_i_df$p_value
  }
  rm(local_GWR_coefs_list)
  
  coef_table <- bind_cols(coef_table, coef_mat)
  # pval_table <- bind_cols(pval_table, pval_mat)
  indep_vars_[length(indep_vars_)] <- "left_right"
  names(coef_table)[-(1:2)] <- indep_vars_
  # names(pval_table)[-(1:2)] <- indep_vars_
  write.csv(coef_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs %s violence_all left-right by AUC n_drop=%i %s data combined %s CF.csv", dep_var, n_drop, year_, title_for_price),
            row.names = F)
  
  # write.csv(pval_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO p-value %s violence_all left-right by AUC n_drop=%i %s data combined %s CF (%s).csv", dep_var, n_drop, year_, title_for_price, date_),
  #           row.names = F)
  
  for (i in c(2, 4:length(coef_table))) {
    var_name <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              # p_value=pval_table[[var_name]],
                              coef=coef_table[[var_name]],
                              rounded_coef=coef_table[[var_name]] %>% round(3))
    min_coef <- min(gwr_coefs_i$coef, na.rm=T)
    max_coef <- max(gwr_coefs_i$coef, na.rm=T)
    coef_map_coords_bw <- map_df %>%
      left_join(gwr_coefs_i, by="id")
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
        # geom_point(aes(x=long, y=lat), data=municipio_centroid %>% filter(id %in% (gwr_coefs_i %>% filter(p_value <= alpha) %>% pull(id))), size=0.7) + # add significant locations
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
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%s)/local GWR lasso coef by AUC violence_all left-right %s %s %i %s data combined %s CF.png",
                   dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price),
           gwr_coef_map, scale=1)
    
  }
}

## coef map by AUC var drop 
local_gwr_LASSO_coef_map_by_AUC_two_year_combined <- function(dep_var_, year_, price_=F) {
  if (price_) {
    indep_vars_in <- indep_vars
    title_for_price <- "with price"
  }else{
    indep_vars_in <- indep_vars[-which(indep_vars == "price_avg")]
    title_for_price <- "no price"
  } 
  indep_vars_in <- c("Intercept", indep_vars_in)
  
  LASSO_AUC_score_var_drop_log_seizure_10_loo <-
    read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right AUC n_drop=10 %s data combined %s CF (05-12-2026).csv",
                     dep_var_, year_, title_for_price)) %>% as_tibble
  
  # local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right by AUC n_drop=10 %s data combined %s CF (05-12-2026).RData",
               dep_var_, year_, title_for_price))
  
  LASSO_best_bw_tbl_var_drop  <- tibble(id = LASSO_AUC_score_var_drop_log_seizure_10_loo$id,
                                        LASSO_log_seizure_coca_bw_AUC = LASSO_AUC_score_var_drop_log_seizure_10_loo[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
  
  local_gwr_LASSO_coef_map_by_AUC(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo, LASSO_best_bw_tbl_var_drop, criteria="LASSO_log_seizure_coca_bw_AUC", dep_var = dep_var_,
                                  indep_vars_ = indep_vars_in, n_drop=10, year_=year_, price=price_)
  rm(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo)
}

local_gwr_LASSO_coef_map_by_AUC_two_year_combined("hyd_destination", "2016-2017", price_=F)


LASSO_gwr_coefs_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs hyd_destination violence_all left-right by AUC n_drop=10 2016-2017 data combined no price CF.csv") %>% as_tibble

GWR_predict_year_CF <- function(LASSO_gwr_coefs, reg_data_year, dep_var, no_price, threshold=0.5) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary")
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
  
  reg_data_year_pred <- reg_data_year_pred %>% mutate(left_right = left_wing*right_paramilitary)
  LASSO_gwr_coefs_mat <- LASSO_gwr_coefs[,-(1:3)]
  LASSO_gwr_coefs_mat[is.na(LASSO_gwr_coefs_mat)] <- 0
  reg_data_year_pred_mat <- reg_data_year_pred[,-(1:2)]
  bX_year <- LASSO_gwr_coefs$Intercept + apply(LASSO_gwr_coefs_mat * reg_data_year_pred_mat, 1, sum)
  pi_hat_year <- 1/(1+exp(-bX_year))
  result <- tibble(id = LASSO_gwr_coefs$id, pi_hat = pi_hat_year)
  result$y_pred <- ifelse(result$pi_hat < threshold, 0, 1) %>% as.factor
  result$y <- reg_data_year_pred$y
  
  return(result)
}

confusion_matrix_pred <- function(GWR_pred) {
  result <- confusionMatrix(GWR_pred$y_pred %>% as.factor, GWR_pred$y %>% as.factor, positive = "1")
  return(result)
}


GWR_predict_1617_CF_with_1617_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_1617, regression_data_CF_1617, "hyd_destination", no_price=T)
ROC_1617_CF_with_1617_coef <- ROC_pred(GWR_predict_1617_CF_with_1617_coef)

png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR LASSO hyd destinations 2016-2017 combined.png")
plot(ROC_1617_CF_with_1617_coef, main="hyd destination - predictions 2016-2017 combined"); text(0.1, 0, paste("AUC:", round(ROC_1617_CF_with_1617_coef$auc, 2)))
dev.off()

######### two-year separate
local_GWR_LASSO_CF_2years <- function(type.measure_="default", AUC_mat, sig_level_=0.05, gwr_LASSO_data_1, gwr_LASSO_data_2, method_, n_drop, interact_=F, scale_11_=F, weight_=NULL, dep_var, price_) {
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  coord_unique <- gwr_LASSO_data_1$coord
  local_gwr_dist <- gwr_LASSO_data_1$dist %>% as.matrix
  
  local_GWR_coefs_LASSO_result <- list()
  for (i in 1:nrow(AUC_mat)) {
    id_i <- AUC_mat$id[i]
    local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]] <- list()
    for (j in 1:length(bwd_range)) {
      bw_ij <- bwd_range[j]
      bw_name <- paste0("bw_", bw_ij)
      local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[bw_name]] <- NA
      
      neighbor_ij_year1 <- neighbor_id(id_i, bw_ij, scale_11_, gwr_data_=gwr_LASSO_data_1) %>% 
        filter(id != id_i) #### leave a focal point out
      neighbor_ij_year2 <- neighbor_id(id_i, bw_ij, scale_11_, gwr_data_=gwr_LASSO_data_2) %>% filter(id != id_i)
      neighbor_ij <- bind_rows(neighbor_ij_year1, neighbor_ij_year2)
      n_0_1 <- neighbor_ij$y %>% table
      
      # restrict too unbalanced responses
      # if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
      #   AUC_mat[[bw_name]][i] <- NA
      #   local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      #   next
      # }
      # 
      # n_unique_vals <- neighbor_ij %>% select(-id) %>% apply(2, function(x) length(table(x)))
      # 
      # # variable drop
      # if (n_unique_vals[["coca_area"]] < n_drop) {
      #   neighbor_ij$coca_area <- NULL
      #   n_unique_vals[["coca_area"]] <- n_drop
      # }
      # if (n_unique_vals[["seizures"]] < n_drop) {
      #   neighbor_ij$seizures <- NULL
      #   n_unique_vals[["seizures"]] <- n_drop
      # }
      # if (price_) {
      #   if (n_unique_vals[["price_avg"]] < n_drop) {
      #     neighbor_ij$price_avg <- NULL
      #     n_unique_vals[["price_avg"]] <- n_drop
      #   }
      # }
      # if (n_unique_vals[["river_length"]] < n_drop) {
      #   neighbor_ij$river_length <- NULL
      #   n_unique_vals[["river_length"]] <- n_drop
      # }
      # if (n_unique_vals[["road_length"]] < n_drop) {
      #   neighbor_ij$road_length <- NULL
      #   n_unique_vals[["road_length"]] <- n_drop
      # }
      # 
      # if (neighbor_ij$airport %>% table %>% min < n_drop | n_unique_vals[["airport"]] < 2) neighbor_ij$airport <- NULL
      # if (neighbor_ij$ferry %>% table %>% min < n_drop | n_unique_vals[["ferry"]] < 2) neighbor_ij$ferry <- NULL
      # if (neighbor_ij$police %>% table %>% min < n_drop | n_unique_vals[["police"]] < 2) neighbor_ij$police <- NULL
      # if (neighbor_ij$military %>% table %>% min < n_drop | n_unique_vals[["military"]] < 2) neighbor_ij$military <- NULL
      # if (neighbor_ij$lab_reported %>% table %>% min < n_drop | n_unique_vals[["lab_reported"]] < 2) neighbor_ij$lab_reported <- NULL
      # if (neighbor_ij$left_wing %>% table %>% min < n_drop | n_unique_vals[["left_wing"]] < 2) neighbor_ij$left_wing <- NULL
      # if (neighbor_ij$right_paramilitary %>% table %>% min < n_drop | n_unique_vals[["right_paramilitary"]] < 2) neighbor_ij$right_paramilitary <- NULL
      # if (is.null(neighbor_ij$lab_reported)) neighbor_ij$lab_residual <- NULL
      # variable drop end
      
      if (!is.null(weight_)) {
        weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
      }else{
        weight_i <- NULL
      }
      
      result_i <- tryCatch(
        {
          # if (("left_wing" %in% names(neighbor_ij)) & ("right_paramilitary" %in% names(neighbor_ij))) {
          #   left_right <- (neighbor_ij$left_wing * neighbor_ij$right_paramilitary) %>% table
          #   if (left_right %>% min < n_drop | length(left_right) < 2) {
          #     LASSO_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)
          #   }else{
          #     LASSO_result_ij <- logistf(y~.+left_wing*right_paramilitary, neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)
          #   }
          # }else{
          #   LASSO_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=sig_level_)
          # }
          
          LASSO_result_ij <- glm(y~.+left_wing*right_paramilitary, neighbor_ij %>% select(-id), weights=weight_i, family="binomial")
          LASSO_result_ij_y <- LASSO_result_ij$y %>% as.factor
          LASSO_result_ij_pi_hat_tbl <- tibble(y=LASSO_result_ij_y, pi_hat=LASSO_result_ij$fitted.values)
          LASSO_result_ij_ROC <- ROC_pred(LASSO_result_ij_pi_hat_tbl)
          AUC_mat[[paste0("bw_", bw_ij)]][i] <- LASSO_result_ij_ROC$auc # AUC
          
          local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- LASSO_result_ij
        },
        error = function(e) {
          return(e)
        }
      )
      
      if (inherits(result_i, "error")) {
        AUC_mat[[paste0("bw_", bw_ij)]][i] <- NA
        local_GWR_coefs_LASSO_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
      }
      
    }
    if (i %% 100 == 0) print(paste0(i, "th municipio complete"))
  }
  
  return(list(AUC_mat=AUC_mat,
              LASSO=local_GWR_coefs_LASSO_result))
}

local_GWR_LASSO_2_years <- function(dep_var_, seed_model, reg_data_year1, reg_data_year2, year_, price=F, weight_in=NULL) {
  dep_var_index <- which(names(reg_data_year1) == dep_var_)
  names(reg_data_year1)[dep_var_index] <- "y"
  names(reg_data_year2)[dep_var_index] <- "y"
  
  if (grepl("hyd", dep_var_)) {
    reg_data_year1 <- reg_data_year1 %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
    reg_data_year2 <- reg_data_year2 %>% 
      select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year1 <- reg_data_year1 %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
    reg_data_year2 <- reg_data_year2 %>% 
      select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (!price) {
    reg_data_year1 <- reg_data_year1 %>% select(-price_avg)
    reg_data_year2 <- reg_data_year2 %>% select(-price_avg)
  }
  title_for_price <- ifelse(price, "with price", "no price")
  
  gwr_data1 <- list(norm = reg_data_year1, coord = coord_unique, dist = gwr_data_dist)
  gwr_data2 <- list(norm = reg_data_year2, coord = coord_unique, dist = gwr_data_dist)
  
  ### use this if allow 2013-2016 only for y
  # reg_data_year <- left_join(gwr_data1$norm %>% select(id, municipio, y), reg_data_year %>% select(-y), by="id")
  
  bwd_range <- seq(0.5, max_bwd, by=0.1)
  AUC_mat_ <- matrix(NA, 1120, 1+length(bwd_range))
  colnames(AUC_mat_) <- c("id", paste0("bw_", bwd_range))
  AUC_mat_ <- as_tibble(AUC_mat_)
  AUC_mat_$id <- gwr_data1$norm$id
  
  set.seed(seed_model)
  local_GWR_coefs_LASSO_list <- local_GWR_LASSO_CF_2years(dep_var = dep_var_, AUC_mat=AUC_mat_, gwr_LASSO_data_1 = gwr_data1, gwr_LASSO_data_2 = gwr_data2, method_="var drop",
                                                      sig_level_ = 0.1, n_drop = 10, weight_ = weight_in, price_=price)
  
  weight_in_1 <- weight_in[1]
  weight_in_0 <- weight_in[2]
  local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo <- local_GWR_coefs_LASSO_list$LASSO
  write.csv(local_GWR_coefs_LASSO_list$AUC_mat,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right AUC n_drop=10 %s data %s CF (05-01-2026).csv",
                    dep_var_, year_, title_for_price), row.names = F)
  save("local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo",
       file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right by AUC n_drop=10 %s data %s CF (05-01-2026).RData",
                      dep_var_, year_, title_for_price))
  rm(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo); rm(local_GWR_coefs_LASSO_list)
  
  # local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo_7_3 <- local_GWR_coefs_LASSO_list$LASSO
  # write.csv(local_GWR_coefs_LASSO_list$AUC_mat,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s leave-one-out AUC log seizure coca scaled n_drop=10 weight %i-%i %s data CF (05-01-2026).csv",
  #                   dep_var_, weight_in_1, weight_in_0, year_, title_for_price), row.names = F)
  # save("local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo_7_3",
  #      file = sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s leave-one-out log seizure coca scaled n_drop=10 weight %i-%i %s data CF (05-01-2026).RData",
  #                     dep_var_, weight_in_1, weight_in_0, year_, title_for_price))
  # rm(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo_7_3); rm(local_GWR_coefs_LASSO_list)
}

# Think about if weight_1 = 1 / positive ratio is a good idea
# weights <- c(7, 0.3) # 7 for postive outcomes
# sample(1:1000000, 4)
# local_GWR_LASSO_2_years("hyd_destination", 178373, regression_data_CF_2013, regression_data_CF_2014, "2013-2014")
local_GWR_LASSO_2_years("hyd_destination", 531190, regression_data_CF_2016, regression_data_CF_2017, "2016-2017")
# local_GWR_LASSO_2_years("hyd_destination", 100, regression_data_2016, 2016, weight_in = weights)
# local_GWR_LASSO_2_years("hyd_destination", 100, regression_data_2016, 2016)
# local_GWR_LASSO_2_years("hyd_source", 424271, weight_in = weights) # 424271
# local_GWR_LASSO_2_years("base_source", 700630, weight_in = weights) # 700630
# local_GWR_LASSO_2_years("base_destination", 49056, weight_in = weights) # 49056


# coef map by AUC scores
local_gwr_LASSO_coef_map_by_AUC <- function(local_GWR_coefs_list, LASSO_best_bw_tbl_, criteria, dep_var, alpha=0.1, n_drop, date_, year_, indep_vars_, price) {
  indep_vars_ <- c(indep_vars_, "left_wing:right_paramilitary")
  coef_table <- tibble(id = LASSO_best_bw_tbl_$id, bw=LASSO_best_bw_tbl_[[criteria]])
  title_for_price <- ifelse(price, "with price", "no price")
  pval_table <- coef_table
  coef_mat <- matrix(NA, nrow(coef_table), length(indep_vars_))
  pval_mat <- coef_mat
  
  indep_vars_df <- data.frame(var_name=indep_vars_)
  for (i in 1:nrow(coef_table)) {
    bw_i <- coef_table$bw[i]
    if (is.na(bw_i)) next
    local_GWR_model_i <- local_GWR_coefs_list[[i]][[paste0("bw_", bw_i)]]
    local_GWR_model_i_p_val <- summary(local_GWR_model_i)$coefficients[,"Pr(>|z|)"]
    p_val_i <- data.frame(var_name = c("Intercept", names(local_GWR_model_i_p_val)[-1]), p_value = local_GWR_model_i_p_val)
    coef_i <- coef(local_GWR_model_i)
    coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i)
    coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name") %>% left_join(p_val_i, by="var_name")
    coef_mat[i,] <- coef_i_df$coef
    pval_mat[i,] <- coef_i_df$p_value
  }
  rm(local_GWR_coefs_list)
  
  coef_table <- bind_cols(coef_table, coef_mat)
  pval_table <- bind_cols(pval_table, pval_mat)
  indep_vars_[length(indep_vars_)] <- "left_right"
  names(coef_table)[-(1:2)] <- indep_vars_
  names(pval_table)[-(1:2)] <- indep_vars_
  write.csv(coef_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs %s violence_all left-right by AUC n_drop=%i %s data %s CF (%s).csv", dep_var, n_drop, year_, title_for_price, date_),
            row.names = F)
  
  write.csv(pval_table,
            sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO p-value %s violence_all left-right by AUC n_drop=%i %s data %s CF (%s).csv", dep_var, n_drop, year_, title_for_price, date_),
            row.names = F)
  
  # for weighted reg
  # write.csv(coef_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs %s leave-one-out %s %i weight 7-3 %s data %s CF (%s).csv", dep_var, criteria, n_drop, year_, title_for_price, date_),
  #           row.names = F)
  # write.csv(pval_table,
  #           sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO p-value %s leave-one-out %s %i weight 7-3 %s data %s CF (%s).csv", dep_var, criteria, n_drop, year_, title_for_price, date_),
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
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%s)/local GWR LASSO coef by AUC violence_all left-right %s %s %i %s data %s CF (%s).png",
                   dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price, date_),
           gwr_coef_map, scale=1)
    
    # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s weight 7-3 (%s)/local GWR LASSO coef by drop %s %s %i weight 7-3 %s data %s CF (%s).png",
    #                dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price, date_),
    #        gwr_coef_map, scale=1)
  }
}

## coef map by AUC var drop 
local_gwr_LASSO_coef_map_by_AUC_year <- function(dep_var_, year_, price_=F) {
  if (price_) {
    indep_vars_in <- indep_vars
    title_for_price <- "with price"
  }else{
    indep_vars_in <- indep_vars[-which(indep_vars == "price_avg")]
    title_for_price <- "no price"
  } 
  indep_vars_in <- c("Intercept", indep_vars_in)
  
  LASSO_AUC_score_var_drop_log_seizure_10_loo <-
    read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right AUC n_drop=10 %s data %s CF (05-01-2026).csv",
                     dep_var_, year_, title_for_price)) %>% as_tibble
  # LASSO_AUC_score_var_drop_log_seizure_10_loo <-
  #   read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s leave-one-out AUC log seizure coca scaled n_drop=10 weight 7-3 %s data CF %s (05-01-2026).csv",
  #                    dep_var_, year_, title_for_price)) %>% as_tibble
  
  # local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo
  load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s violence_all left-right by AUC n_drop=10 %s data %s CF (05-01-2026).RData",
               dep_var_, year_, title_for_price))
  # local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo_7_3
  # load(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR LASSO %s leave-one-out log seizure coca scaled n_drop=10 weight 7-3 %s data %s CF (05-01-2026).RData",
  #              dep_var_, year_, title_for_price))
  
  LASSO_best_bw_tbl_var_drop  <- tibble(id = LASSO_AUC_score_var_drop_log_seizure_10_loo$id,
                                      LASSO_log_seizure_coca_bw_AUC = LASSO_AUC_score_var_drop_log_seizure_10_loo[,-1] %>% apply(1, function(x) return(ifelse(all(is.na(x)), NA, bwd_range[which.max(x)]))) %>% unlist)
  
  local_gwr_LASSO_coef_map_by_AUC(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo, LASSO_best_bw_tbl_var_drop, criteria="LASSO_log_seizure_coca_bw_AUC", dep_var = dep_var_,
                                indep_vars_ = indep_vars_in, n_drop=10, date_="05-01-2026", year_=year_, price=price_)
  rm(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo)
  # local_gwr_LASSO_coef_map_by_AUC(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo_7_3, LASSO_best_bw_tbl_var_drop, criteria="LASSO_log_seizure_coca_bw_AUC", dep_var = dep_var_,
  #                              indep_vars_ = indep_vars_in, n_drop=10, date_="02-17-2026", year_=year_, price=price_)
  # rm(local_GWR_coefs_LASSO_var_drop_log_seizure_scaled_loo_7_3)
}

# local_gwr_LASSO_coef_map_by_AUC_year("hyd_destination", "2013-2014", price_=F)
local_gwr_LASSO_coef_map_by_AUC_year("hyd_destination", "2016-2017", price_=F)
# local_gwr_LASSO_coef_map_by_AUC_year("hyd_source")
# local_gwr_LASSO_coef_map_by_AUC_year("base_source")
# local_gwr_LASSO_coef_map_by_AUC_year("base_destination")


# prediction check
LASSO_gwr_coefs_AUC_CF_2013_2014 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs hyd_destination violence_all left-right by AUC n_drop=10 2013-2014 data no price CF (05-01-2026).csv") %>% as_tibble
LASSO_gwr_coefs_AUC_CF_2016_2017 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs hyd_destination violence_all left-right by AUC n_drop=10 2016-2017 data no price CF (05-01-2026).csv") %>% as_tibble

GWR_predict_year_CF <- function(LASSO_gwr_coefs, reg_data_year, dep_var, no_price, threshold=0.5) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary")
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
  
  reg_data_year_pred <- reg_data_year_pred %>% mutate(left_right = left_wing*right_paramilitary)
  LASSO_gwr_coefs_mat <- LASSO_gwr_coefs[,-(1:3)]
  LASSO_gwr_coefs_mat[is.na(LASSO_gwr_coefs_mat)] <- 0
  reg_data_year_pred_mat <- reg_data_year_pred[,-(1:2)]
  bX_year <- LASSO_gwr_coefs$Intercept + apply(LASSO_gwr_coefs_mat * reg_data_year_pred_mat, 1, sum)
  pi_hat_year <- 1/(1+exp(-bX_year))
  result <- tibble(id = LASSO_gwr_coefs$id, pi_hat = pi_hat_year)
  result$y_pred <- ifelse(result$pi_hat < threshold, 0, 1) %>% as.factor
  result$y <- reg_data_year_pred$y
  
  return(result)
}

confusion_matrix_pred <- function(GWR_pred) {
  result <- confusionMatrix(GWR_pred$y_pred %>% as.factor, GWR_pred$y %>% as.factor, positive = "1")
  return(result)
}

# GWR_predict_2013_CF_with_1314_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_2013_2014, regression_data_CF_2013, "hyd_destination", no_price=T)
# GWR_predict_2014_CF_with_1314_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_2013_2014, regression_data_CF_2014, "hyd_destination", no_price=T)
# GWR_predict_2016_CF_with_1314_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_2013_2014, regression_data_CF_2016, "hyd_destination", no_price=T)
# GWR_predict_2017_CF_with_1314_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_2013_2014, regression_data_CF_2017, "hyd_destination", no_price=T)
GWR_predict_2016_CF_with_1617_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_2016_2017, regression_data_CF_2016, "hyd_destination", no_price=T)
GWR_predict_2017_CF_with_1617_coef <- GWR_predict_year_CF(LASSO_gwr_coefs_AUC_CF_2016_2017, regression_data_CF_2017, "hyd_destination", no_price=T)

ROC_2016_CF_with_1617_coef <- ROC_pred(GWR_predict_2016_CF_with_1617_coef)
ROC_2017_CF_with_1617_coef <- ROC_pred(GWR_predict_2017_CF_with_1617_coef)

plot(ROC_2016_CF_with_1617_coef)
plot(ROC_2017_CF_with_1617_coef)

# png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR hyd destinations 2013 with 1314 coef.png")
# plot(ROC_2013_CF_with_1314_coef, main="hyd destination - predictions 2013"); text(0.1, 0, paste("AUC:", round(ROC_2013_CF_with_1314_coef$auc, 2)))
# dev.off()
# png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR hyd destinations 2014 with 1314 coef.png")
# plot(ROC_2014_CF_with_1314_coef, main="hyd destination - predictions 2014"); text(0.1, 0, paste("AUC:", round(ROC_2014_CF_with_1314_coef$auc, 2)))
# dev.off()
# png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR hyd destinations 2016 with 1314 coef.png")
# plot(ROC_2016_CF_with_1314_coef, main="hyd destination - predictions 2016"); text(0.1, 0, paste("AUC:", round(ROC_2016_CF_with_1314_coef$auc, 2)))
# dev.off()
# png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR hyd destinations 2017 with 1314 coef.png")
# plot(ROC_2017_CF_with_1314_coef, main="hyd destination - predictions 2017"); text(0.1, 0, paste("AUC:", round(ROC_2017_CF_with_1314_coef$auc, 2)))
# dev.off()
png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR LASSO hyd destinations 2016 with 1617 coef.png")
plot(ROC_2016_CF_with_1617_coef, main="hyd destination - predictions 2016"); text(0.1, 0, paste("AUC:", round(ROC_2016_CF_with_1617_coef$auc, 2)))
dev.off()
png("Colombia Data/local GWR PML result predicted prices/roc curves/roc curves CF by two-year (violence_all left-right)/roc curve GWR LASSO hyd destinations 2017 with 1617 coef.png")
plot(ROC_2017_CF_with_1617_coef, main="hyd destination - predictions 2017"); text(0.1, 0, paste("AUC:", round(ROC_2017_CF_with_1617_coef$auc, 2)))
dev.off()
