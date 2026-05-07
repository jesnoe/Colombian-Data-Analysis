# setwd("C:/Users/User/Documents/R")
library(readxl)
library(writexl)
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
library(distances)

{
binary_vars <- c("y", "airport", "armed_group", "ferry", "police", "military")
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
empty_map <- ggplot(map_df, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group),
               color = "black",
               fill="white",
               linewidth = 0.1) + 
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

indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary")

dep_var_ <- "hyd_destination"; price <- F
reg_data_year1 <- regression_data_CF_2016
reg_data_year2 <- regression_data_CF_2017
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

coord_unique <- left_join(regression_data_CF_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix

gwr_data1 <- list(norm = reg_data_year1, coord = coord_unique, dist = gwr_data_dist)
gwr_data2 <- list(norm = reg_data_year2, coord = coord_unique, dist = gwr_data_dist)

# base_gwr_data <- read.csv("Colombia Data/base gwr data.csv") %>% as_tibble
# hyd_gwr_data <- read.csv("Colombia Data/hyd gwr data.csv") %>% as_tibble
PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 2016-2017 data no price CF (05-01-2026).csv") %>% as_tibble
# PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble
# PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble 
# PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble 

indep_vars_ <- names(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:2)]
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

local_GWR_PML_sensitivity <- function(id_i, gwr_PML_data_1, gwr_PML_data_2, indep_vars, weight_=NULL, sig_level_=0.1) {
  bwd_range <- seq(0.5, 4, by=0.1)
  local_GWR_coefs_PML_result <- list()
  AUC_i <- list()
  for (n_drop_ in 12:7) {
    for (n_y_ in 10:5) {
      param_name <- paste0("param_", n_drop_, "_", n_y_)
      AUC_i[[param_name]] <- c()
      local_GWR_coefs_PML_result[[param_name]] <- list()
    }
  }
  
  for (j in 1:length(bwd_range)) {
    bw_ij <- bwd_range[j]
    bw_name <- paste0("bw_", bw_ij)
    
    neighbor_ij_year1 <- neighbor_id(id_i, bw_ij, F, gwr_data_=gwr_PML_data_1) %>% 
      filter(id != id_i) #### leave a focal point out
    neighbor_ij_year2 <- neighbor_id(id_i, bw_ij, F, gwr_data_=gwr_PML_data_2) %>% filter(id != id_i)
    neighbor_ij <- bind_rows(neighbor_ij_year1, neighbor_ij_year2)
    n_0_1 <- neighbor_ij$y %>% table
    
    for (n_drop_ in 12:7) {
      for (n_y_ in 10:5) {
        neighbor_ij_param <- neighbor_ij
        n_unique_vals <- neighbor_ij_param %>% select(-id) %>% apply(2, function(x) length(table(x)))
        param_name <- paste0("param_", n_drop_, "_", n_y_)
        local_GWR_coefs_PML_result[[param_name]][[bw_name]] <- NA
        
        # restrict too unbalanced responses
        if (sum(n_0_1 < 8) > 0 | length(n_0_1) < 2) {
          AUC_i[[param_name]] <- c(AUC_i[[param_name]], NA)
          local_GWR_coefs_PML_result[[paste0("id_", id_i)]][[paste0("bw_", bw_ij)]] <- NA
          next
        }
        
        n_unique_vals <- neighbor_ij_param %>% select(-id) %>% apply(2, function(x) length(table(x)))
        
        # variable drop
        if (n_unique_vals[["coca_area"]] < n_drop_) {
          neighbor_ij_param$coca_area <- NULL
          n_unique_vals[["coca_area"]] <- n_drop_
        }
        if (n_unique_vals[["seizures"]] < n_drop_) {
          neighbor_ij_param$seizures <- NULL
          n_unique_vals[["seizures"]] <- n_drop_
        }
        if (n_unique_vals[["river_length"]] < n_drop_) {
          neighbor_ij_param$river_length <- NULL
          n_unique_vals[["river_length"]] <- n_drop_
        }
        if (n_unique_vals[["road_length"]] < n_drop_) {
          neighbor_ij_param$road_length <- NULL
          n_unique_vals[["road_length"]] <- n_drop_
        }
        
        if (neighbor_ij_param$airport %>% table %>% min < n_drop_ | n_unique_vals[["airport"]] < 2) neighbor_ij_param$airport <- NULL
        # if (neighbor_ij_param$armed_group %>% table %>% min < n_drop_ | n_unique_vals[["armed_group"]] < 2) neighbor_ij_param$armed_group <- NULL
        if (neighbor_ij_param$ferry %>% table %>% min < n_drop_ | n_unique_vals[["ferry"]] < 2) neighbor_ij_param$ferry <- NULL
        if (neighbor_ij_param$police %>% table %>% min < n_drop_ | n_unique_vals[["police"]] < 2) neighbor_ij_param$police <- NULL
        if (neighbor_ij_param$military %>% table %>% min < n_drop_ | n_unique_vals[["military"]] < 2) neighbor_ij_param$military <- NULL
        if (neighbor_ij_param$lab_reported %>% table %>% min < n_drop_ | n_unique_vals[["lab_reported"]] < 2) neighbor_ij_param$lab_reported <- NULL
        if (neighbor_ij_param$left_wing %>% table %>% min < n_drop_ | n_unique_vals[["left_wing"]] < 2) neighbor_ij_param$left_wing <- NULL
        if (neighbor_ij_param$right_paramilitary %>% table %>% min < n_drop_ | n_unique_vals[["right_paramilitary"]] < 2) neighbor_ij_param$right_paramilitary <- NULL
        if (is.null(neighbor_ij_param$lab_reported)) neighbor_ij_param$lab_residual <- NULL
        # variable drop end
        
        if (!is.null(weight_)) {
          weight_i <- ifelse(neighbor_ij_param$y == 1, weight_[1], weight_[2])
        }else{
          weight_i <- NULL
        }
        
        result_i <- tryCatch(
          {
            if (("left_wing" %in% names(neighbor_ij_param)) & ("right_paramilitary" %in% names(neighbor_ij_param))) {
              left_right <- (neighbor_ij_param$left_wing * neighbor_ij_param$right_paramilitary) %>% table
              if (left_right %>% min < n_drop_ | length(left_right) < 2) {
                PML_result_ij <- logistf(y~., neighbor_ij_param %>% select(-id), weights=weight_i, alpha=sig_level_)
              }else{
                PML_result_ij <- logistf(y~.+left_wing*right_paramilitary, neighbor_ij_param %>% select(-id), weights=weight_i, alpha=sig_level_)
              }
            }else{
              PML_result_ij <- logistf(y~., neighbor_ij_param %>% select(-id), weights=weight_i, alpha=sig_level_)
            }
            
            PML_result_ij_y <- PML_result_ij$model$y %>% as.factor
            PML_result_ij_pi_hat_tbl <- tibble(y=PML_result_ij_y, pi_hat=PML_result_ij$predict)
            PML_result_ij_ROC <- ROC_pred(PML_result_ij_pi_hat_tbl)
            AUC_ij <- PML_result_ij_ROC$auc # AUC
            
            AUC_i[[param_name]] <- c(AUC_i[[param_name]], AUC_ij)
            local_GWR_coefs_PML_result[[param_name]][[bw_name]] <- PML_result_ij
          },
          error = function(e) {
            return(e)
          }
        )
        
        if (inherits(result_i, "error")) {
          AUC_i[[param_name]] <- c(AUC_i[[param_name]], NA)
        }
      }
    }
  }
  
  # best bw coef
  result <- list()
  for (n_drop_ in 12:7) {
    for (n_y_ in 10:5) {
      param_name <- paste0("param_", n_drop_, "_", n_y_)
      AUC_i_param <- AUC_i[[param_name]]
      
      best_bw <- ifelse(all(is.na(AUC_i_param)), NA, bwd_range[which.max(AUC_i_param)])
      coef_table <- tibble(id = id_i, bw=best_bw, n_y=n_y_, n_drop=n_drop_)
      coef_table <- bind_rows(coef_table, coef_table)
      coef_mat <- matrix(NA, 1, length(indep_vars)+1)
      if (is.na(best_bw)) {
        colnames(coef_mat) <- indep_vars
        coef_table <- bind_cols(coef_table, coef_mat)
        coef_table$pi_hat <- NA
        coef_table$y <- NA
        result[[param_name]] <- coef_table
        next
      }
      
      indep_vars_df <- data.frame(var_name=c(indep_vars, "left_wing:right_paramilitary"))
      if (!is.na(best_bw)) {
        local_GWR_model_i <- local_GWR_coefs_PML_result[[param_name]][[paste0("bw_", best_bw)]]
        coef_i <- coef(local_GWR_model_i)
        coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i, p_value=local_GWR_model_i$prob)
        coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
        coef_mat[1,] <- coef_i_df$coef
      }
      
      colnames(coef_mat) <- c(indep_vars, "left_right")
      coef_table <- bind_cols(coef_table, coef_mat)
      
      model_vars_i <- (coef_i %>% names)[-1]
      
      neighbor_i <- bind_rows(neighbor_id(id_i, best_bw, scale_11_=F, gwr_PML_data_1), neighbor_id(id_i, best_bw, scale_11_=F, gwr_PML_data_2))
      var_names_i <- names(neighbor_i)[names(neighbor_i) %in% model_vars_i]
      data_pred_i <- neighbor_i %>% filter(id == id_i) %>% select(y, all_of(var_names_i))
      pi_hat_i <- predict(local_GWR_model_i, data_pred_i, type="response")
      coef_table$pi_hat <- pi_hat_i
      coef_table$y <- data_pred_i$y
      result[[param_name]] <- coef_table
    }
  }
  rm(local_GWR_coefs_PML_result)
  return(bind_rows(result))
}



local_GWR_PML_sensitivity_hyd_dest <- list()
start_time <- Sys.time()
for (j in 1:nrow(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest)) {
  id_j <- PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest$id[j]
  local_GWR_PML_sensitivity_hyd_dest[[paste0("bw_", id_j)]] <- local_GWR_PML_sensitivity(id_j, gwr_data1, gwr_data2, indep_vars_)
  if (j %% 100 == 0) print(paste0(j, "th municipio complete: ", Sys.time()))
}
end_time <- Sys.time()
end_time - start_time # 2.830059 days
local_GWR_PML_sensitivity_hyd_dest_tbl <- bind_rows(local_GWR_PML_sensitivity_hyd_dest)
local_GWR_PML_sensitivity_hyd_dest_tbl$year <- rep(c(2016, 2017), 40320)
# y_tbl <- bind_rows(regression_data_CF_2016 %>% select(id, hyd_destination) %>% rename(y = hyd_destination),
#                    regression_data_CF_2017 %>% select(id, hyd_destination) %>% rename(y = hyd_destination))
# y_tbl$year <- c(rep(2016, nrow(regression_data_CF_2016)), rep(2017, nrow(regression_data_CF_2017)))
local_GWR_PML_sensitivity_hyd_dest_tbl <- left_join(local_GWR_PML_sensitivity_hyd_dest_tbl, y_tbl, by=c("id", "year")) %>% relocate(id:n_drop, y)
# write_xlsx(local_GWR_PML_sensitivity_hyd_dest_tbl %>% relocate(id, bw, year), "Colombia Data/local GWR PML result predicted prices/sensitivity analysis hyd destination 2016-2017 (violence left-right).xlsx")


local_GWR_PML_sensitivity_hyd_dest_tbl <- read_xlsx("Colombia Data/local GWR PML result predicted prices/sensitivity analysis hyd destination 2016-2017 (violence left-right).xlsx")
sensitivity_summary <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% group_by(id, year) %>%
  summarize(y=y[1], n_params=n(), bw_sd=sd(bw, na.rm=T), across(Intercept:pi_hat, sd, na.rm = TRUE, .names = "{.col}_sd"))
sensitivity_summary_coef <- sensitivity_summary %>% filter(year == 2016) %>% ungroup %>% select(-year, -pi_hat_sd)
sensitivity_summary_pi_hat <- sensitivity_summary %>% pivot_wider(id_cols = id, names_from = year, values_from = pi_hat_sd, names_glue = "pi_hat_sd_{year}")

AUC_2016 <- c()
AUC_2017 <- c()
AUC_param <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% select(n_y, n_drop) %>% unique
for (i in 1:nrow(AUC_param)) {
  n_y_i <- AUC_param$n_y[i]
  n_drop_i <- AUC_param$n_drop[i]
  pi_hat_2016 <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% filter(year ==2016 & n_y == n_y_i & n_drop == n_drop_i) %>% select(y, pi_hat) %>% mutate(y=as.factor(y))
  pi_hat_2017 <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% filter(year ==2017 & n_y == n_y_i & n_drop == n_drop_i) %>% select(y, pi_hat) %>% mutate(y=as.factor(y))
  
  AUC_2016 <- c(AUC_2016, ROC_pred(pi_hat_2016)$auc)
  AUC_2017 <- c(AUC_2017, ROC_pred(pi_hat_2017)$auc)
}
AUC_param$AUC_2016 <- AUC_2016
AUC_param$AUC_2017 <- AUC_2017

AUC_param %>% print(n=36)

# coef, pi_hat standard deviation maps
sensitivity_summary_tbl <- sensitivity_summary_coef
for (i in c(4, 6:length(sensitivity_summary_tbl))) {
  var_name <- names(sensitivity_summary_tbl)[i]
  map_data_i <- data.frame(id=sensitivity_summary_tbl$id,
                           coef=sensitivity_summary_tbl[[var_name]],
                           rounded_coef=sensitivity_summary_tbl[[var_name]] %>% round(3))
  min_coef <- min(map_data_i$coef, na.rm=T)
  max_coef <- max(map_data_i$coef, na.rm=T)
  sensitivity_map_coords <- map_df %>% left_join(map_data_i, by="id")
  
  sensitivity_map <- ggplot(sensitivity_map_coords, aes(x=long, y=lat)) +
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
  
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/sensitivity maps (%s)/coef sensitivity map (%s).png",
                 "hyd_destination", var_name),
         sensitivity_map, scale=1)
}


sensitivity_summary %>% filter(seizures_sd > 40)
local_GWR_PML_sensitivity_hyd_dest_tbl %>% filter(year == 2016 & id %in% c(15533, 85225)) %>% select(-year) %>% print(n=144)


sum(sensitivity_summary$pred_var > 0, na.rm = T) / nrow(sensitivity_summary %>% filter(!is.na(pred_var))) # 153/1110 = 0.1378
sensitivity_summary$bw_var %>% summary
sensitivity_summary %>% arrange(desc(pred_var))
sensitivity_summary %>% arrange(desc(bw_var))
local_GWR_PML_sensitivity_hyd_dest_tbl %>% filter(id %in% (sensitivity_summary %>% filter(is.na(pred_var)) %>% pull(id)))



sample(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest$id, 5)
sensitivity_id_5002 <- local_GWR_PML_id(5002, gwr_data, indep_vars_)
sensitivity_id_91263 <- local_GWR_PML_sensitivity(91263)
start.time <- Sys.time()
sensitivity_id_41615 <- local_GWR_PML_id(41615, gwr_data, indep_vars_)
end.time <- Sys.time()
end.time - start.time # 5.586761 mins
sensitivity_id_52258 <- local_GWR_PML_sensitivity(52258)
sensitivity_id_66088 <- local_GWR_PML_sensitivity(66088)
sensitivity_id_15842 <- local_GWR_PML_sensitivity(15842)
sensitivity_id_68179 <- local_GWR_PML_sensitivity(68179)
sensitivity_id_25260 <- local_GWR_PML_sensitivity(25260)
sensitivity_id_23417 <- local_GWR_PML_sensitivity(23417)
sensitivity_id_68573 <- local_GWR_PML_sensitivity(68573)

sensitivity_id_5002
sensitivity_id_91263
sensitivity_id_41615
sensitivity_id_52258
sensitivity_id_66088
sensitivity_id_15842
sensitivity_id_68179
sensitivity_id_25260
sensitivity_id_23417
sensitivity_id_68573

id_i<-5002; gwr_PML_data_<-gwr_data; indep_vars<-indep_vars_; n_y=8; n_drop=10; weight_=NULL

PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id %in% c(5002, 91263, 41615, 52258))
sensitivity_id_91263[1,]
sensitivity_id_5002[1,]
sensitivity_id_41615[1,]
sensitivity_id_52258[1,]

logistf(y~., local_GWR_coefs_PML_var_drop_log_seizure_scaled_loo$id_91263$bw_5$model %>% as_tibble)
logistf(y~., neighbor_id(91263, 5, scale_11_ = F, gwr_data_=gwr_data) %>% filter(id != 91263) %>% select(-id))
neighbor_id(91263, 5, scale_11_ = F, gwr_data_=gwr_data) %>% filter(id != 91263) %>% select(-id) %>% view


## interpretation
# setwd("C:/Users/User/Documents/R")
library(readxl)
library(writexl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)ff
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
library(distances)
{
  binary_vars <- c("y", "airport", "armed_group", "ferry", "police", "military")
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
  empty_map <- ggplot(map_df, aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group),
                 color = "black",
                 fill="white",
                 linewidth = 0.1) + 
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
  
  base_gwr_data <- read.csv("Colombia Data/base gwr data.csv") %>% as_tibble
  hyd_gwr_data <- read.csv("Colombia Data/hyd gwr data.csv") %>% as_tibble
  PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
  PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble
  PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
  PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble
  indep_vars_ <- names(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:2)]
}


coca_area_significant_id <- PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(coca_area <= 0.1) %>% pull(id)
river_length_significant_id <- PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(river_length <= 0.1) %>% pull(id)

area_of_interest_list <- function(coefs, pvals, var, threshold) {
  significant_id <- pvals %>% filter({{var}} <= 0.1) %>% pull(id)
  
  result <- coefs %>% filter(id %in% significant_id & {{var}} >= threshold) %>%
    select(id, bw, {{var}}) %>% rename(variable_coef = {{var}}) %>%
    left_join(pvals %>% rename(p_value={{var}}) %>% select(id, p_value), by="id") %>%
    left_join(municipio_centroid %>% select(id:depto), by="id") %>%
    left_join(hyd_gwr_data %>% select(id, {{var}}), by="id") %>% relocate(id, municipio, depto, bw, {{var}})
  
  return(result)
}

coca_area_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, coca_area, 1)

river_length_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, river_length, 0)
river_length_list_hyd_source <- area_of_interest_list(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_source, PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_source, river_length, 0)

armed_group_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, armed_group, 2)
police_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest, police, -Inf)


coca_area_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/coca_area_list_hyd_destination.xlsx")
river_length_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/river_length_list_hyd_destination.xlsx")
river_length_list_hyd_source %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/river_length_list_hyd_source.xlsx")
armed_group_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/armed_group_list_hyd_dest.xlsx")
police_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/police_list_hyd_destination.xlsx")
