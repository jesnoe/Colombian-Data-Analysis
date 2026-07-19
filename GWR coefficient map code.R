# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(ggpattern)
library(gridExtra)
library(lubridate)
library(colmaps) # https://github.com/nebulae-co/colmaps
library(sf)
library(sp)
library(caret)
library(pracma)
library(GWmodel)
library(pROC)
library(glmnet)
library(reshape2)
library(regclass)
library(logistf)
library(knitr)
{
  map <- municipios
  map_df <- suppressMessages(fortify(map)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(!(id %in% c(88001, 88564)))
  
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id) %>% 
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
}

PML_gwr_coefs_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price CF (05-08-2026).csv") %>% as_tibble
PML_gwr_pvals_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price CF (05-08-2026).csv") %>% as_tibble
indep_vars <- names(PML_gwr_coefs_AUC_CF_1617)[-(1:2)]

## run this line to test the function below
# coef_table = PML_gwr_coefs_AUC_CF_1617; pval_table = PML_gwr_pvals_AUC_CF_1617; dep_var = "hyd_destination"; alpha=0.1; n_drop = 10; date_ = today(); year_=1617; indep_vars_=indep_vars

local_gwr_PML_coef_map_by_AUC <- function(coef_table, pval_table, dep_var, alpha=0.1, n_drop, date_, year_, indep_vars_) {
  indep_vars_ <- c(indep_vars_, "left_wing:right_paramilitary")
  id_excluded <- coef_table %>% filter(is.na(bw)) %>% pull(id)
  
  for (i in c(2, 4:length(coef_table))) {
    var_name <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              excluded=coef_table$id %in% id_excluded,
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
        geom_polygon_pattern(aes(group=group, fill=coef, pattern=excluded),
                             color = "black",
                             linewidth = 0.1,
                             pattern_fill = NA,         # Makes the pattern background transparent
                             pattern_density = 0.1,     # Keeps the stripe lines thin
                             pattern_spacing = 0.02) +
        expand_limits(x = map_df$long, y = map_df$lat) +
        coord_quickmap() +
        scale_fill_viridis_c(na.value = "white") +
        scale_pattern_manual(values = c("TRUE" = "stripe", "FALSE" = "none"), 
                             guide = "none") + # Hides the pattern legend
        labs(fill=var_name, x=NULL, y=NULL, title=NULL) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
    }else{
      gwr_coef_map <- ggplot(coef_map_coords, aes(x=long, y=lat)) +
        geom_polygon_pattern(aes(group=group, fill=coef, pattern=excluded),
                             color = "black",
                             linewidth = 0.1,
                             pattern_fill = NA,         # Makes the pattern background transparent
                             pattern_density = 0.1,     # Keeps the stripe lines thin
                             pattern_spacing = 0.02) +
        geom_point(aes(x=long, y=lat), data=municipio_centroid %>% filter(id %in% (gwr_coefs_i %>% filter(p_value <= alpha) %>% pull(id))), size=0.7) + # add significant locations
        expand_limits(x = map_df$long, y = map_df$lat) +
        coord_quickmap() +
        scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","#C00000"),
                             values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                             na.value = "white") +
        scale_pattern_manual(values = c("TRUE" = "stripe", "FALSE" = "none"), 
                             guide = "none") + # Hides the pattern legend
        labs(fill=var_name, x=NULL, y=NULL, title=NULL) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              line = element_blank()
        )
    }
    
    ggsave(sprintf("coef maps/%s (%i)/local GWR PML coef by AUC violence_all left-right %s %s all var drop n_drop=%i %i data CF.png",
                   dep_var, year_, var_name, dep_var, n_drop, year_),
           gwr_coef_map, scale=1)
  }
}

local_gwr_PML_coef_map_by_AUC(PML_gwr_coefs_AUC_CF_1617, PML_gwr_pvals_AUC_CF_1617, "hyd_destination", n_drop=10, date_=today(), year_=1617, indep_vars_=indep_vars)
