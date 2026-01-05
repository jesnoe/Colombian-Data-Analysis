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

base_gwr_data <- read.csv("Colombia Data/base gwr data.csv") %>% as_tibble
hyd_gwr_data <- read.csv("Colombia Data/hyd gwr data.csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble 
PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_base_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs base_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble 

indep_vars_ <- names(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:2)]
}


local_GWR_PML_sensitivity <- function(id_i, gwr_PML_data_, indep_vars, weight_=NULL) {
  bwd_range <- seq(0.5, 5, by=0.1)
  local_GWR_coefs_PML_result <- list()
  F1_i <- list()
  for (n_drop_ in 10:7) {
    for (n_y_ in 8:5) {
      param_name <- paste0("param_", n_drop_, "_", n_y_)
      F1_i[[param_name]] <- c()
      local_GWR_coefs_PML_result[[param_name]] <- list()
    }
  }
  
  for (j in 1:length(bwd_range)) {
    bw_ij <- bwd_range[j]
    bw_name <- paste0("bw_", bw_ij)
    
    neighbor_ij_ <- neighbor_id(id_i, bw_ij, scale_11_ = F, gwr_data_=gwr_PML_data_) %>% 
      filter(id != id_i) #### leave a focal point out
    n_0_1 <- neighbor_ij_$y %>% table
    
    for (n_drop_ in 10:7) {
      for (n_y_ in 8:5) {
        neighbor_ij <- neighbor_ij_
        n_unique_vals <- neighbor_ij %>% select(-id) %>% apply(2, function(x) length(table(x)))
        param_name <- paste0("param_", n_drop_, "_", n_y_)
        local_GWR_coefs_PML_result[[param_name]][[bw_name]] <- NA
        
        # restrict too unbalanced responses
        if (sum(n_0_1 < n_y_) > 0 | length(n_0_1) < 2) {
          F1_i[[param_name]] <- c(F1_i[[param_name]], NA)
          next
        }
        
        if (n_unique_vals[["coca_area"]] < n_drop_) {
          neighbor_ij$coca_area <- NULL
          n_unique_vals[["coca_area"]] <- n_drop_
        }
        if (n_unique_vals[["seizures"]] < n_drop_) {
          neighbor_ij$seizures <- NULL
          n_unique_vals[["seizures"]] <- n_drop_
        }
        if (n_unique_vals[["price_avg"]] < n_drop_) {
          neighbor_ij$price_avg <- NULL
          n_unique_vals[["price_avg"]] <- n_drop_
        }
        if (n_unique_vals[["river_length"]] < n_drop_) {
          neighbor_ij$river_length <- NULL
          n_unique_vals[["river_length"]] <- n_drop_
        }
        if (n_unique_vals[["road_length"]] < n_drop_) {
          neighbor_ij$road_length <- NULL
          n_unique_vals[["road_length"]] <- n_drop_
        }
        if (n_unique_vals[["lab_prob"]] < n_drop_) {
          neighbor_ij$lab_prob <- NULL
          n_unique_vals[["lab_prob"]] <- n_drop_
        }
        
        if (neighbor_ij$airport %>% table %>% min < n_drop_ | n_unique_vals[["airport"]] < 2) neighbor_ij$airport <- NULL
        if (neighbor_ij$armed_group %>% table %>% min < n_drop_ | n_unique_vals[["armed_group"]] < 2) neighbor_ij$armed_group <- NULL
        if (neighbor_ij$ferry %>% table %>% min < n_drop_ | n_unique_vals[["ferry"]] < 2) neighbor_ij$ferry <- NULL
        if (neighbor_ij$police %>% table %>% min < n_drop_ | n_unique_vals[["police"]] < 2) neighbor_ij$police <- NULL
        if (neighbor_ij$military %>% table %>% min < n_drop_ | n_unique_vals[["military"]] < 2) neighbor_ij$military <- NULL
        
        if (!is.null(weight_)) {
          weight_i <- ifelse(neighbor_ij$y == 1, weight_[1], weight_[2])
        }else{
          weight_i <- NULL
        }
        
        result_i <- tryCatch(
          {
            PML_result_ij <- logistf(y~., neighbor_ij %>% select(-id), weights=weight_i, alpha=0.1)
            
            PML_result_ij_y <- PML_result_ij$model$y
            PML_result_ij_pred <- ifelse(PML_result_ij$predict < 0.5, 0, 1) %>% factor(levels = c("0", "1"))
            PML_result_ij_CM <- confusionMatrix(PML_result_ij_pred, PML_result_ij_y, positive = "1")
            F1_ij <- PML_result_ij_CM$byClass[7] # F1 score
            F1_i[[param_name]] <- c(F1_i[[param_name]], F1_ij)
            local_GWR_coefs_PML_result[[param_name]][[bw_name]] <- PML_result_ij
          },
          error = function(e) {
            return(e)
          }
        )
        
        if (inherits(result_i, "error")) {
          F1_i[[param_name]] <- c(F1_i[[param_name]], NA)
        }
      }
    }
  }
  
  # best bw coef
  result <- list()
  for (n_drop_ in 10:7) {
    for (n_y_ in 8:5) {
      param_name <- paste0("param_", n_drop_, "_", n_y_)
      F1_i_param <- F1_i[[param_name]]
      
      best_bw <- ifelse(all(is.na(F1_i_param)), NA, bwd_range[which.max(F1_i_param)])
      coef_table <- tibble(id = id_i, bw=best_bw, n_y=n_y_, n_drop=n_drop_)
      coef_mat <- matrix(NA, 1, length(indep_vars))
      if (is.na(best_bw)) {
        colnames(coef_mat) <- indep_vars
        coef_table <- bind_cols(coef_table, coef_mat)
        coef_table$pi_hat <- NA
        coef_table$y <- NA
        coef_table$y_pred <- NA
        result[[param_name]] <- coef_table
        next
      }
      
      indep_vars_df <- data.frame(var_name=indep_vars)
      if (!is.na(best_bw)) {
        local_GWR_model_i <- local_GWR_coefs_PML_result[[param_name]][[paste0("bw_", best_bw)]]
        coef_i <- coef(local_GWR_model_i)
        coef_i_df <- data.frame(var_name=c("Intercept", names(coef_i)[-1]), coef=coef_i, p_value=local_GWR_model_i$prob)
        coef_i_df <- left_join(indep_vars_df, coef_i_df, by="var_name")
        coef_mat[1,] <- coef_i_df$coef
      }
      
      colnames(coef_mat) <- indep_vars
      coef_table <- bind_cols(coef_table, coef_mat)
      
      model_vars_i <- (coef_i %>% names)[-1]
      
      neighbor_i <- neighbor_id(id_i, best_bw, scale_11_=F, gwr_PML_data_)
      var_names_i <- names(neighbor_i)[names(neighbor_i) %in% model_vars_i]
      data_pred_i <- neighbor_i %>% filter(id == id_i) %>% select(all_of(var_names_i)) %>% relocate(model_vars_i)
      pi_hat_i <- predict(local_GWR_model_i, data_pred_i, type="response")
      coef_table$pi_hat <- pi_hat_i
      coef_table$y <- data_pred_i$y
      coef_table$y_pred <- ifelse(pi_hat_i< 0.5, 0, 1)
      result[[param_name]] <- coef_table
    }
  }
  
  return(bind_rows(result))
}

local_GWR_PML_sensitivity_hyd_dest <- list()
start_time <- Sys.time()
for (j in 1:nrow(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)) {
  id_j <- PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$id[j]
  local_GWR_PML_sensitivity_hyd_dest[[paste0("bw_", id_j)]] <- local_GWR_PML_sensitivity(id_j, gwr_data, indep_vars_)
  if (j %% 100 == 0) print(paste0(j, "th municipio complete: ", Sys.time()))
}
end_time <- Sys.time()
end_time - start_time # 1.20206 days
local_GWR_PML_sensitivity_hyd_dest_tbl <- bind_rows(local_GWR_PML_sensitivity_hyd_dest)
write_xlsx(local_GWR_PML_sensitivity_hyd_dest_tbl, "Colombia Data/local GWR PML result predicted prices/sensitivity analysis hyd destination.xlsx")

local_GWR_PML_sensitivity_hyd_dest_tbl <- left_join(local_GWR_PML_sensitivity_hyd_dest_tbl %>% select(-y), gwr_data$norm %>% select(id, y), by="id")
sensitivity_summary <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% group_by(id) %>% summarize(n_params=n(), correct_pred=sum(y_pred == y), pred_var=var(y_pred))
sensitivity_summary
sensitivity_summary$pred_var %>% summary
local_GWR_PML_sensitivity_hyd_dest_tbl %>% filter(id %in% (sensitivity_summary %>% filter(is.na(pred_var)) %>% pull(id)))



sample(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest$id, 5)
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

PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(id %in% c(5002, 91263, 41615, 52258))
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
  
  base_gwr_data <- read.csv("Colombia Data/base gwr data.csv") %>% as_tibble
  hyd_gwr_data <- read.csv("Colombia Data/hyd gwr data.csv") %>% as_tibble
  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
  PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble
  PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 (12-09-2025).csv") %>% as_tibble
  PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_source <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_source leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 weight 7-3 (12-09-2025).csv") %>% as_tibble
  indep_vars_ <- names(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:2)]
}


coca_area_significant_id <- PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(coca_area <= 0.1) %>% pull(id)
river_length_significant_id <- PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest %>% filter(river_length <= 0.1) %>% pull(id)

area_of_interest_list <- function(coefs, pvals, var, threshold) {
  significant_id <- pvals %>% filter({{var}} <= 0.1) %>% pull(id)
  
  result <- coefs %>% filter(id %in% significant_id & {{var}} >= threshold) %>%
    select(id, bw, {{var}}) %>% rename(variable_coef = {{var}}) %>%
    left_join(pvals %>% rename(p_value={{var}}) %>% select(id, p_value), by="id") %>%
    left_join(municipio_centroid %>% select(id:depto), by="id") %>%
    left_join(hyd_gwr_data %>% select(id, {{var}}), by="id") %>% relocate(id, municipio, depto, bw, {{var}})
  
  return(result)
}

coca_area_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, coca_area, 1)

river_length_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, river_length, 0)
river_length_list_hyd_source <- area_of_interest_list(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_source, PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_source, river_length, 0)

armed_group_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, armed_group, 2)
police_list_hyd_dest <- area_of_interest_list(PML_gwr_coefs_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, PML_gwr_p_vals_F1_var_drop_log_seizure_coca_10_loo_hyd_dest, police, -Inf)


coca_area_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/coca_area_list_hyd_destination.xlsx")
river_length_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/river_length_list_hyd_destination.xlsx")
river_length_list_hyd_source %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/river_length_list_hyd_source.xlsx")
armed_group_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/armed_group_list_hyd_dest.xlsx")
police_list_hyd_dest %>% write_xlsx("Colombia Data/local GWR PML result predicted prices/areas of interest/police_list_hyd_destination.xlsx")
