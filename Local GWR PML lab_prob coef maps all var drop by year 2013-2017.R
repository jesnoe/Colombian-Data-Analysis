#### seizures are last year data t-1
## bandwidth range: 0~4
indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "armed_group", "lab_prob")
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
}

PML_gwr_coefs_F1_lab_prob_2013 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2013 data no price lab_prob (02-04-2026).csv") %>% as_tibble
PML_gwr_coefs_F1_lab_prob_2014 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2014 data no price lab_prob (02-04-2026).csv") %>% as_tibble
PML_gwr_coefs_F1_lab_prob_2016 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2016 data no price lab_prob (02-04-2026).csv") %>% as_tibble
PML_gwr_coefs_F1_lab_prob_2017 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination leave-one-out PML_log_seizure_coca_bw_F1 all var drop 10 2017 data no price lab_prob (02-04-2026).csv") %>% as_tibble

PML_gwr_coefs_F1_lab_prob_all_years <- bind_rows(PML_gwr_coefs_F1_lab_prob_2013, PML_gwr_coefs_F1_lab_prob_2014, PML_gwr_coefs_F1_lab_prob_2016, PML_gwr_coefs_F1_lab_prob_2017)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$coca_area)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$seizures)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$river_length)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$road_length)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$population)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$airport)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$ferry)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$police)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$military)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$armed_group)
# hist(PML_gwr_coefs_F1_lab_prob_all_years$lab_prob)

price_ <- F
if (price_) {
  indep_vars_in <- indep_vars
  title_for_price <- "with price"
}else{
  indep_vars_in <- indep_vars[-which(indep_vars == "price_avg")]
  title_for_price <- "no price"
} 
indep_vars_in <- c("Intercept", indep_vars_in)

min_max_table_ <- tibble(
  var_name = indep_vars_in[-1],
  max_coef = PML_gwr_coefs_F1_lab_prob_all_years %>% select(-(id:Intercept)) %>% apply(2, function(x) max(x, na.rm = T)),
  min_coef = PML_gwr_coefs_F1_lab_prob_all_years %>% select(-(id:Intercept)) %>% apply(2, function(x) min(x, na.rm = T)),
  mid_coef = c(5, 10, 10, 10, 30, 10, 5, 15, 10, 20, 100)
)

# coef map by F1 scores common coef range for each year
local_gwr_PML_coef_map_by_F1_common_range <- function(min_max_table, dep_var, alpha=0.1, n_drop, date_, year_, indep_vars_, price) {
  # dep_var="hyd_destination"; alpha=0.1; n_drop=10; date_="02-04-2026"; indep_vars_=indep_vars_in; price=F; year_=2013; min_max_table=min_max_table_
  years <- c(2013, 2014, 2016, 2017)
  title_for_price <- ifelse(price, "with price", "no price")
  
  for (year_ in years) {
    coef_table <- read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs %s leave-one-out PML_log_seizure_coca_bw_F1 all var drop %i %i data %s lab_prob (%s).csv",
                                   dep_var, n_drop, year_, title_for_price, date_)) %>% as_tibble
    pval_table <- read.csv(sprintf("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value %s leave-one-out PML_log_seizure_coca_bw_F1 all var drop %i %i data %s lab_prob (%s).csv",
                                   dep_var, n_drop, year_, title_for_price, date_)) %>% as_tibble
    
    for (i in 4:length(coef_table)) {
      var_name <- names(coef_table)[i]
      gwr_coefs_i <- data.frame(id=coef_table$id,
                                coef=coef_table[[var_name]],
                                rounded_coef=coef_table[[var_name]] %>% round(3),
                                p_value=pval_table[[var_name]])
      min_max_table_idx <- which(min_max_table$var_name == var_name)
      min_coef <- min_max_table$min_coef[min_max_table_idx]
      mid_coef <- min_max_table$mid_coef[min_max_table_idx]
      max_coef <- min_max_table$max_coef[min_max_table_idx]
      coef_map_coords <- map_df %>%
        left_join(gwr_coefs_i, by="id")
      
      gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) +
        geom_polygon(aes(group=group, fill=coef),
                     color = "black",
                     linewidth = 0.1) +
        geom_point(aes(x=long, y=lat), data=municipio_centroid %>% filter(id %in% (gwr_coefs_i %>% filter(p_value <= alpha) %>% pull(id))), size=0.7) + # add significant locations
        expand_limits(x = depto_map$long, y = depto_map$lat) +
        coord_quickmap() +
        scale_fill_gradientn(colors = c("blue","skyblue", "palegreen","grey40", "yellow", "orange", "red"),
                             values = scales::rescale(c(min_coef, -mid_coef, -mid_coef/4, 0 , mid_coef/4, mid_coef, max_coef), from = c(min_coef, max_coef)),
                             limits = c(min_coef, max_coef),
                             breaks = seq(5*(1 + floor(min_coef) %/% 5), 5*(ceiling(max_coef) %/% 5), length.out = 6),
                             labels = seq(5*(1 + floor(min_coef) %/% 5), 5*(ceiling(max_coef) %/% 5), length.out = 6),
                             na.value = "white") +
        labs(fill=var_name, x="", y="", title=dep_var) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
      
      ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps lab_prob by year/%s common range (%i)/local GWR PML coef by drop %s %s all var drop %i %i data %s lab_prob (%s).png",
                     dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price, date_),
             gwr_coef_map, scale=1)
      
      # ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s weight 7-3 common range (%i)/local GWR PML coef by drop %s %s all var drop %i weight 7-3 %i data %s lab_prob (%s).png",
      #                dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price, date_),
      #        gwr_coef_map, scale=1)
    }
  }
}

local_gwr_PML_coef_map_by_F1_common_range(min_max_table_, "hyd_destination", n_drop=10, date_="02-04-2026", indep_vars_=indep_vars_in, price=F)
