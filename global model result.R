#### seizures are last year data t-1
# setwd("/Users/R")
# setwd("C:/Users/User/Documents/R")
indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_reported", "lab_residual", "FARC", "ELN")
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
  
  depto_map <- suppressMessages(fortify(departamentos)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(id != 88) %>% 
    left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
  
  violence_AAMM <- read.csv("Colombia Data/violence with id (AAMM).csv") %>% as_tibble
  violence_etc <- read.csv("Colombia Data/violence with id (etc).csv") %>% as_tibble
  violence_all <- bind_rows(violence_AAMM, violence_etc)
  conflict <- read.csv("Colombia Data/Conflict events.csv") %>% as_tibble
  violence_combined <- bind_rows(violence_all %>% select(id, year, FARC, ELN, AUC) %>%
                                   mutate(FARC = ifelse(FARC == "yes", 1 , 0),
                                          ELN = ifelse(ELN == "yes", 1 , 0),
                                          AUC = ifelse(AUC == "yes", 1 , 0)),
                                 conflict %>% mutate(FARC = ifelse(grepl("FARC", dyad_name), 1, 0)) %>% select(id, year, FARC)) %>% 
    group_by(id) %>% # removed year under the assumption that paramilitary and guerrilla groups do not relocate that much
    summarize(FARC = ifelse(any(FARC == 1), 1, 0),
              ELN = ifelse(any(ELN == 1), 1, 0),
              AUC = ifelse(any(AUC == 1), 1, 0)) %>% ungroup
  
  regression_data_CF_2013 <- read.csv("Colombia Data/regression data all municipios CF 2013.csv") %>% as_tibble
  regression_data_CF_2014 <- read.csv("Colombia Data/regression data all municipios CF 2014.csv") %>% as_tibble
  regression_data_CF_2016 <- read.csv("Colombia Data/regression data all municipios CF 2016.csv") %>% as_tibble
  regression_data_CF_2017 <- read.csv("Colombia Data/regression data all municipios CF 2017.csv") %>% as_tibble
  
  regression_data_CF_2013 <- regression_data_CF_2013 %>% left_join(violence_combined, by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  regression_data_CF_2014 <- regression_data_CF_2014 %>% left_join(violence_combined, by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  regression_data_CF_2016 <- regression_data_CF_2016 %>% left_join(violence_combined, by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  regression_data_CF_2017 <- regression_data_CF_2017 %>% left_join(violence_combined, by = "id") %>% 
    mutate(FARC = ifelse(is.na(FARC), 0, FARC),
           ELN = ifelse(is.na(ELN), 0, ELN),
           AUC = ifelse(is.na(AUC), 0, AUC),
           armed_group = ifelse(AUC == 1, 1, armed_group)) %>% select(-AUC)
  
  coord_unique <- left_join(regression_data_CF_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  regression_data_CF_2013$hyd_seizures <- round(regression_data_CF_2013$hyd_seizures, 2)
  regression_data_CF_2014$hyd_seizures <- round(regression_data_CF_2014$hyd_seizures, 2)
  regression_data_CF_2016$hyd_seizures <- round(regression_data_CF_2016$hyd_seizures, 2)
  regression_data_CF_2017$hyd_seizures <- round(regression_data_CF_2017$hyd_seizures, 2)
  
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
  
  # PML_gwr_coefs_AUC_CF_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_AUC all var drop 10 2016 data CF (04-13-2026).csv") %>% as_tibble
}

ROC_pred <- function(GWR_pred) {
  result <- roc(GWR_pred$y, GWR_pred$pi_hat, positive = "1", quiet = T)
  return(result)
}

global_reg_data_year <- function(dep_var_, reg_data_year, year_, price=F) {
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
  
  if (!price) reg_data_year <- reg_data_year %>% select(-price_avg)
  title_for_price <- ifelse(price, "with price", "no price")
  
  global_reg_year <- glm(y~., family = binomial, data=reg_data_year %>% select(-id))
  
  return(global_reg_year)
}

global_reg_2016 <- global_reg_data_year("hyd_destination", regression_data_CF_2016, 2016)
global_reg_2017 <- global_reg_data_year("hyd_destination", regression_data_CF_2017, 2017)

global_reg_2016_ROC <- roc(global_reg_2016$data$y, global_reg_2016$fitted.values, positive = "1", quiet = T)
global_reg_2017_ROC <- roc(global_reg_2017$data$y, global_reg_2017$fitted.values, positive = "1", quiet = T)

png("Colombia Data/local GWR PML result predicted prices/roc curves/global model roc curves CF by year (violence_all)/roc curve global hyd destinations 2016.png")
plot(global_reg_2016_ROC, main="hyd destination - predictions 2016"); text(0.1, 0, paste("AUC:", round(global_reg_2016_ROC$auc, 2)))
dev.off()
png("Colombia Data/local GWR PML result predicted prices/roc curves/global model roc curves CF by year (violence_all)/roc curve global hyd destinations 2017.png")
plot(global_reg_2017_ROC, main="hyd destination - predictions 2017"); text(0.1, 0, paste("AUC:", round(global_reg_2017_ROC$auc, 2)))
dev.off()
