# indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary")
# indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_residual", "left_wing", "right_paramilitary")
indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_prob", "left_wing", "right_paramilitary")
max_bwd <- 4
# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(ggpattern)
library(ggspatial)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(colmaps)
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
########## used bandwidth range 0.5~3.0 due to the time limit
{
  binary_vars <- c("y", "airport", "armed_group", "ferry", "police", "military")
  municipios_ES <- municipios@data %>% mutate(id = as.numeric(id), id_depto = as.numeric(id_depto))
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
  
  depto_map <- suppressMessages(fortify(departamentos)) %>% 
    mutate(id=as.numeric(id)) %>% 
    filter(id != 88) %>% 
    left_join(municipios_ES %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
  
  depto_centroid <- depto_map %>% 
    filter(!(id %in% c(88))) %>% 
    group_by(id, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  
  # indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_reported", "lab_residual", "left_wing", "right_paramilitary")
  # indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_residual", "left_wing", "right_paramilitary")
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_prob", "left_wing", "right_paramilitary")
  # row1 <- read_xlsx("Colombia Data/local GWR PML result predicted prices/sensitivity analysis hyd destination 2016-2017 combined (violence left-right).xlsx") %>% head(1)
  
  dep_var_ <- "hyd_destination"; price <- F
  
  
  # hyd_lab_glm_year <- glm(hyd_lab~.+left_wing*right_paramilitary, family=binomial(link="probit"),
  #                         data=regression_data_CF_1617 %>% select(hyd_lab, coca_area, hyd_seizures, river_length:right_paramilitary))
  # summary(hyd_lab_glm_year)
  # z_vals <- predict(hyd_lab_glm_year, type="link")
  # Inv_Mill_1 <- dnorm(z_vals) / pnorm(z_vals)
  # Inv_Mill_0 <- -dnorm(z_vals) / (1-pnorm(z_vals))
  # 
  # hyd_lab_res <- numeric(nrow(regression_data_CF_1617))
  # pos_index <- which(regression_data_CF_1617$hyd_lab == "1")
  # neg_index <- which(regression_data_CF_1617$hyd_lab == "0")
  # hyd_lab_res[pos_index] <- Inv_Mill_1[pos_index]
  # hyd_lab_res[neg_index] <- Inv_Mill_0[neg_index]
  # regression_data_CF_1617$hyd_lab_res <- hyd_lab_res
  # 
  # reg_data_year1 <- regression_data_CF_1617
  # dep_var_index <- which(names(reg_data_year1) == dep_var_)
  # names(reg_data_year1)[dep_var_index] <- "y"
  # 
  # if (grepl("hyd", dep_var_)) {
  #   reg_data_year1 <- reg_data_year1 %>% 
  #     select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
  #     rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
  #     select(id, y, all_of(indep_vars))
  # }else{
  #   reg_data_year1 <- reg_data_year1 %>% 
  #     select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
  #     rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
  #     select(id, y, all_of(indep_vars))
  # }
  # 
  # if (!price) {
  #   reg_data_year1 <- reg_data_year1 %>% select(-price_avg)
  # }
  # title_for_price <- ifelse(price, "with price", "no price")
  
  regression_data_lab_prob_2013 <- read.csv("Colombia Data/regression data all municipios lab_prob 2013.csv") %>% as_tibble
  regression_data_lab_prob_2014 <- read.csv("Colombia Data/regression data all municipios lab_prob 2014.csv") %>% as_tibble
  regression_data_lab_prob_2016 <- read.csv("Colombia Data/regression data all municipios lab_prob 2016.csv") %>% as_tibble
  regression_data_lab_prob_2017 <- read.csv("Colombia Data/regression data all municipios lab_prob 2017.csv") %>% as_tibble
  regression_data_lab_prob_1617 <- read.csv("Colombia Data/regression data all municipios lab_prob 1617.csv") %>% as_tibble
  
  
  coord_unique <- left_join(regression_data_lab_prob_2013 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  
  regression_data_lab_prob_2013$hyd_seizures <- round(regression_data_lab_prob_2013$hyd_seizures, 2)
  regression_data_lab_prob_2014$hyd_seizures <- round(regression_data_lab_prob_2014$hyd_seizures, 2)
  regression_data_lab_prob_2016$hyd_seizures <- round(regression_data_lab_prob_2016$hyd_seizures, 2)
  regression_data_lab_prob_2017$hyd_seizures <- round(regression_data_lab_prob_2017$hyd_seizures, 2)
  regression_data_lab_prob_1617$hyd_seizures <- round(regression_data_lab_prob_1617$hyd_seizures, 2)
  
  collapsed_armed_group <- tibble(id = regression_data_lab_prob_2013$id,
                                  armed_group_2013 = regression_data_lab_prob_2013$armed_group,
                                  armed_group_2014 = regression_data_lab_prob_2014$armed_group,
                                  armed_group_2016 = regression_data_lab_prob_2016$armed_group,
                                  armed_group_2017 = regression_data_lab_prob_2017$armed_group) %>% 
    apply(1, function(x) ifelse(sum(x[2:5]) > 0, 1, 0))
  
  regression_data_lab_prob_2013$armed_group <- collapsed_armed_group
  regression_data_lab_prob_2014$armed_group <- collapsed_armed_group
  regression_data_lab_prob_2016$armed_group <- collapsed_armed_group
  regression_data_lab_prob_2017$armed_group <- collapsed_armed_group
  
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
    right_join(regression_data_lab_prob_2016 %>% select(id, armed_group) %>% rename(paramilitary = armed_group), by="id") %>% 
    mutate(left_wing = if_any(c(Guerrilla:ELN, Front), ~ . == 1) %>% as.numeric,
           right_paramilitary = if_any(c(AUC, Bloque, paramilitary), ~ . == 1) %>%  as.numeric)
  violence_combined[is.na(violence_combined)] <- 0
  
  regression_data_lab_prob_1617 <- regression_data_lab_prob_1617 %>% select(-armed_group) %>% left_join(violence_combined %>% select(id, left_wing, right_paramilitary), by = "id")
  
  reg_data_year1 <- regression_data_lab_prob_1617
  dep_var_index <- which(names(reg_data_year1) == dep_var_)
  names(reg_data_year1)[dep_var_index] <- "y"
  
  if (grepl("hyd", dep_var_)) {
    reg_data_year1 <- reg_data_year1 %>% 
      select(-PPI_lab_prob, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_prob=hyd_lab_prob, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year1 <- reg_data_year1 %>% 
      select(-hyd_lab_prob, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_prob=PPI_lab_prob, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  
  if (!price) reg_data_year1 <- reg_data_year1 %>% select(-price_avg)
  title_for_price <- ifelse(price, "with price", "no price")
  # write.csv(reg_data_year1, "Colombia Data/map data all municipios lab_prob 1617 (not normalized).csv", row.names = F)
  
  coord_unique <- left_join(regression_data_lab_prob_1617 %>% select(id), municipio_centroid %>% ungroup %>% select(id, long, lat), by="id") 
  gwr_data_dist <- dist(coord_unique %>% select(-id), diag=T, upper=T) %>% as.matrix
  gwr_data1 <- list(norm = reg_data_year1, coord = coord_unique, dist = gwr_data_dist)
  
  # indep_vars_ <- names(PML_gwr_coefs_AUC_var_drop_log_seizure_coca_10_loo_hyd_dest)[-(1:2)]

ROC_pred <- function(GWR_pred) {
  result <- roc(GWR_pred$y, GWR_pred$pi_hat, positive = "1", quiet = T)
  return(result)
}

lon_label <- function(x) {
  deg <- floor(abs(x))
  min <- round((abs(x) - deg) * 60)
  
  ifelse(
    min == 0,
    paste0(deg, "\u00B0", ifelse(x < 0, "W", "E")),
    paste0(deg, "\u00B0", min, "\u2032", ifelse(x < 0, "W", "E"))
  )
}

lat_label <- function(x) {
  deg <- floor(abs(x))
  min <- round((abs(x) - deg) * 60)
  
  direction <- ifelse(x < 0, "S", "N")
  
  ifelse(
    min == 0,
    paste0(deg, "\u00B0", direction),
    paste0(deg, "\u00B0", min, "\u2032", direction)
  )
}

map_geospatial <- function(ggplot_map) {
  result <- ggplot_map +
    # North arrow
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      pad_x = unit(1.0, "cm"),
      pad_y = unit(1.0, "cm"),
      height = unit(1.3, "cm"),
      width = unit(1.3, "cm"),
      style = north_arrow_fancy_orienteering
    ) +
    
    # Scale bar
    annotation_scale(
      location = "br",
      width_hint = 0.30,
      unit_category = "metric",
      pad_x = unit(2.3, "cm"),
      pad_y = unit(0.3, "cm"),
      text_cex = 0.7,
      line_width = 0.5
    ) +
    
    scale_x_continuous(
      labels = lon_label,
      position = "top",
      sec.axis = dup_axis(labels = lon_label)
    ) +
    
    scale_y_continuous(
      labels = lat_label,
      sec.axis = dup_axis(labels = lat_label)
    ) +
    
    # coord_sf allows ggspatial to calculate the scale bar correctly
    coord_sf(
      xlim = x_limits,
      ylim = y_limits,
      default_crs = sf::st_crs(4326),
      crs = sf::st_crs(4326),
      expand = FALSE
    ) +
    
    labs(
      x = NULL,
      y = NULL,
      title = NULL
    ) +
    theme_bw() +
    theme(
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # panel.border = element_blank(),
      # axis.text = element_blank(),
      line = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.025, 0.025),
      legend.justification = c(0, 0)
    )
  
  return(result)
}

x_limits <- range(depto_map$long, na.rm = TRUE) + c(-1.7, 0.8)
y_limits <- range(depto_map$lat,  na.rm = TRUE) + c(-1.2, 0.8)

}

# depto map to assist result & discussion section
depto_list <- c("Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas", "Cauca", "Caqueta", "Cesar", "Choco", "Cordoba", "Cundinamarca",
                "La Guajira", "Magdalena", "Narino", "Norte de Santander", "Risaralda", "Santander", "Sucre", "Tolima", "Valle del Cauca")
depto_color_key <- municipios_capital %>% filter(depto %in% depto_list) %>% select(id_depto, depto) %>% distinct %>% arrange(id_depto) %>% rename(id = id_depto) %>% mutate(id = as.numeric(id))

highlight_colors <- c(
  "1" = "#8dd3c7",
  "2" = "#ffffb3",
  "3" = "#bebada",
  "4" = "#fb8072",
  "5" = "#80b1d3"
)

depto_color_key$color_group <- rep(seq_along(highlight_colors), length.out = length(depto_list))
depto_color_key$color_group[which(depto_color_key$depto == "Boyaca")] <- 2
depto_color_key$color_group[which(depto_color_key$depto == "Caldas")] <- 4
depto_color_key$color_group[which(depto_color_key$depto == "Risaralda")] <- 3
depto_color_key$color_group[which(depto_color_key$depto == "Sucre")] <- 3
depto_color_key$color_group[which(depto_color_key$depto == "Cesar")] <- 5
depto_color_key$color_group[which(depto_color_key$depto == "Narino")] <- 4
depto_color_key$color_group[which(depto_color_key$depto == "Magdalena")] <- 1
depto_color_key$color_group[which(depto_color_key$depto == "Norte de Santander")] <- 4

# Add department names and color groups to polygon data
depto_plot_data <- depto_map %>% 
  left_join(depto_color_key %>% select(-depto), by = "id" )%>%
  mutate(id = as.numeric(id),
         color_group = as.factor(color_group))


# Label positions
depto_labels <- depto_centroid %>%
  filter(id %in% id_depto_list) %>%
  bind_rows(
    municipio_centroid %>% ungroup %>% 
      select(-municipio, -depto) %>% 
      filter(id %in% c(27450, 50226)) %>%
      left_join(municipios_ES %>% select(id, municipio) %>% mutate(id = as.numeric(id)), by="id") %>% 
      rename(depto = municipio)
    ) %>% 
  mutate(
    depto = case_when(
      depto == "Norte De Santander" ~ "Norte de\nSantander",
      depto == "Valle Del Cauca"    ~ "Valle del\nCauca",
      depto == "La Guajira"         ~ "La Guajira",
      depto == "Bogotá, D. C."      ~ "Bogotá, D.C.",
      TRUE                          ~ depto
    ),
    long = case_when(
      depto == "Antioquia"    ~ long+0.70,
      depto == "Bolívar"      ~ long+0.60,
      depto == "Boyacá"       ~ long+0.20,
      depto == "Cesar"        ~ long+0.20,
      depto == "Chocó"        ~ long+0.20,
      depto == "Cundinamarca" ~ long-0.50,
      depto == "Magdalena"    ~ long-0.20,
      depto == "Risaralda"    ~ long-0.15,
      depto == "Sucre"        ~ long-0.10,
      TRUE                    ~ long
    ),
    lat = case_when(
      depto == "Antioquia"    ~ lat-0.30,
      depto == "Bolívar"      ~ lat-1.00,
      depto == "Boyacá"       ~ lat-0.40,
      depto == "Caquetá"      ~ lat-0.60,
      depto == "Caldas"       ~ lat-0.10,
      depto == "Cauca"        ~ lat+0.30,
      depto == "Cesar"        ~ lat+0.60,
      depto == "Chocó"        ~ lat-0.50,
      depto == "Cundinamarca" ~ lat+0.40,
      depto == "Magdalena"    ~ lat+0.35,
      depto == "Nariño"       ~ lat+0.10,
      TRUE                    ~ lat
    )
  ) %>% 
  mutate(
    nudge_x = case_when(
      depto == "Risaralda"    ~  -0.5,
      TRUE                    ~  0
    ),
    
    nudge_y = case_when(
      depto == "Atlántico"    ~  0.5,
      depto == "Risaralda"    ~  0.3,
      depto == "Bogotá, D.C." ~  0.30,
      TRUE                    ~  0
    )
  )

# Municipios shown with red points
municipio_points <- municipio_centroid %>%
  filter(id %in% c(11001, 27450, 50226))

depto_location_map <- ggplot(
  depto_plot_data,
  aes(x = long, y = lat)
) +
  geom_polygon(
    aes(
      group = group,
      fill = color_group
    ),
    color = "grey55",
    linewidth = 0.18
  ) +
  
  # Five supplied colors; unselected departments remain light gray
  scale_fill_manual(
    values = highlight_colors,
    na.value = "grey96",
    guide = "none"
  ) +
  
  # Department labels
  geom_text_repel(
    data = depto_labels,
    aes(
      x = long,
      y = lat,
      label = depto
    ),
    inherit.aes = FALSE,
    nudge_x = depto_labels$nudge_x,
    nudge_y = depto_labels$nudge_y,
    size = 3,
    lineheight = 0.85,
    fontface = "plain",
    color = "grey10",
    box.padding = 0.12,
    point.padding = 0.05,
    min.segment.length = 0.10,
    segment.color = "grey45",
    segment.linewidth = 0.25,
    max.overlaps = Inf,
    seed = 123
  ) +
  
  # Mentioned municipios
  geom_point(
    data = municipio_points,
    aes(x = long, y = lat),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.5,
    stroke = 0.5,
    color = "white",
    fill = "#d73027"
  )

depto_location_map <- map_geospatial(depto_location_map)
ggsave(
  "Colombia Data/local GWR PML result predicted prices/colombia_locations.png",
  depto_location_map,
  width = 6.5,
  height = 8,
  units = "in"
)

# data map
# hyd_destination_annual <- bind_rows(regression_data_CF_2013 %>% select(id, hyd_destination) %>% mutate(year = 2013, hyd_destination = as.factor(hyd_destination)),
#                                     regression_data_CF_2014 %>% select(id, hyd_destination) %>% mutate(year = 2014, hyd_destination = as.factor(hyd_destination)),
#                                     regression_data_CF_2016 %>% select(id, hyd_destination) %>% mutate(year = 2016, hyd_destination = as.factor(hyd_destination)),
#                                     regression_data_CF_2017 %>% select(id, hyd_destination) %>% mutate(year = 2017, hyd_destination = as.factor(hyd_destination)))
# for (year_ in c(2013, 2014, 2016, 2017)) { # area maps
#   hyd_destination_year <- hyd_destination_annual %>% filter(year == year_)
#   hyd_destination_year_map <- left_join(map_df, hyd_destination_year %>% select(id, hyd_destination), by="id")
#   
#   hyd_destination_map_ggplot <- hyd_destination_year_map %>% ggplot() +
#     geom_polygon(aes(x=long, y=lat, group=group, fill=hyd_destination),
#                  color = "black",
#                  linewidth = 0.1) +
#     expand_limits(x = map_df$long, y = map_df$lat) +
#     coord_quickmap() +
#     scale_fill_manual(values = c("0"="white", "1"="#C00000"), na.value = "white") +
#     labs(fill="destination", x=NULL, y=NULL) +
#     theme_bw() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           axis.text = element_blank(),
#           line = element_blank(),
#           plot.margin = unit(c(0, 0, 0, 0), "cm"),
#           legend.position.inside = c(0.025, 0.025))
#   
#   ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/hyd_destination maps/hyd_destination map (%i).png", year_),
#          hyd_destination_map_ggplot, scale=1)
# }
# 
# data_map <- function(reg_data_year) {
#   binary_vars <- c("airport", "ferry", "police", "military", "lab_reported", "left_wing", "right_paramilitary")
#   for (i in 3:ncol(reg_data_year)) {
#     var_name_ <- names(reg_data_year)[i]
#     gwr_data_i <- data.frame(id=reg_data_year$id,
#                              obs=reg_data_year[[var_name_]])
#     data_map_coords <- map_df %>%
#       left_join(gwr_data_i, by="id")
#     if (var_name_ %in% binary_vars) {
#       ggplot(data_map_coords, aes(x=long, y=lat)) +
#         geom_polygon(aes(group=group, fill=as.factor(obs)),
#                      color = "black",
#                      linewidth = 0.1) +
#         expand_limits(x = map_df$long, y = map_df$lat) +
#         coord_quickmap(expand = FALSE) +
#         scale_fill_manual(values = c("0"="white", "1"="#C00000"), na.value = "white") +
#         labs(fill=var_name_, x=NULL, y=NULL) +
#         theme_bw() +
#         theme(panel.grid.major = element_blank(),
#               panel.grid.minor = element_blank(),
#               panel.border = element_blank(),
#               axis.text = element_blank(),
#               line = element_blank(),
#               legend.position.inside = c(0.025, 0.025)
#         ) -> data_map_i
#     }else{
#       ggplot(data_map_coords, aes(x=long, y=lat)) +
#         geom_polygon(aes(group=group, fill=obs),
#                      color = "black",
#                      linewidth = 0.1) +
#         expand_limits(x = map_df$long, y = map_df$lat) +
#         coord_quickmap(expand = FALSE) +
#         scale_fill_viridis_c(na.value = "white") +
#         labs(fill=var_name_, x="", y="") +
#         theme_bw() +
#         theme(panel.grid.major = element_blank(),
#               panel.grid.minor = element_blank(),
#               panel.border = element_blank(),
#               axis.text = element_blank(),
#               line = element_blank()
#         ) -> data_map_i
#       # next
#     }
#     
#     ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/data maps for paper/local GWR data %s.png", var_name_),
#            data_map_i, units = "in", width = 7.5, height = 7)
#   }
# }
# 
# data_map(reg_data_year1)

regression_data_CF_1617_x_norm <- read.csv("Colombia Data/regression data all municipios CF 1617.csv") %>% as_tibble
regression_data_lab_prob_1617_x_norm <- read.csv("Colombia Data/regression data all municipios lab_prob 1617 (not normalized).csv") %>% as_tibble %>% rename(seizures = hyd_seizures)
# var_names <- c("coca_area", "seizures", "river_length", "road_length", "population")
var_names <- c(indep_vars[-1], "lab_reported")
regression_data_lab_prob_1617_x_norm <- regression_data_lab_prob_1617_x_norm %>% rename(lab_prob = hyd_lab_prob) %>% 
  left_join(regression_data_lab_prob_1617 %>% select(id, left_wing, right_paramilitary), by="id") %>% 
  left_join(regression_data_CF_1617_x_norm %>% select(id, hyd_lab), by="id") %>% rename(lab_reported = hyd_lab) %>% 
  select(id, all_of(var_names))

binary_vars <- c("airport", "ferry", "police", "military", "lab_reported", "left_wing", "right_paramilitary")

for (i in 1:length(var_names)) {
  var_name_ <- var_names[i]
  gwr_data_i <- data.frame(id=regression_data_lab_prob_1617_x_norm$id,
                           obs=regression_data_lab_prob_1617_x_norm[[var_name_]])
  data_map_coords <- map_df %>%
    left_join(gwr_data_i, by="id")
  
  if (var_name_ %in% binary_vars) {
    ggplot(data_map_coords, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group, fill=as.factor(obs)),
                   color = "black",
                   linewidth = 0.1) +
      expand_limits(x = map_df$long, y = map_df$lat) +
      coord_quickmap(expand = FALSE) +
      scale_fill_manual(values = c("0"="white", "1"="#C00000"), na.value = "white") +
      labs(fill=var_name_, x=NULL, y=NULL) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank()
      ) -> data_map_i
  }else{
    ggplot(data_map_coords, aes(x=long, y=lat)) +
      geom_polygon(aes(group=group, fill=obs),
                   color = "black",
                   linewidth = 0.1) +
      expand_limits(x = map_df$long, y = map_df$lat) +
      coord_quickmap(expand = FALSE) +
      scale_fill_viridis_c(na.value = "white") +
      labs(fill=var_name_, x="", y="") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank()
      ) -> data_map_i
  }
  data_map_i <- map_geospatial(data_map_i)
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/data maps for paper/local GWR data %s.png", var_name_),
         data_map_i, units = "in", width = 5, height = 7)
}

hyd_destination_1617_map_coords <- map_df %>% left_join(reg_data_year1 %>% select(id, y) %>% mutate(y = as.factor(y)), by="id")
ggplot(hyd_destination_1617_map_coords, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=y),
               color = "black",
               linewidth = 0.1) +
  expand_limits(x = depto_map$long, y = depto_map$lat) +
  coord_quickmap() +
  scale_fill_manual(values = c("0"="white", "1"="#C00000"), na.value = "white") +
  labs(fill="y", x="", y="") +
  theme_bw() -> hyd_destination_1617_map
hyd_destination_1617_map <- map_geospatial(hyd_destination_1617_map)
ggsave("Colombia Data/local GWR PML result predicted prices/hyd_destination maps/hyd_destination map (1617).png", hyd_destination_1617_map, units = "in", width = 5, height = 7)

# Sensitivity analysis
# local_GWR_PML_sensitivity_hyd_dest_tbl <- read_xlsx("Colombia Data/local GWR PML result predicted prices/sensitivity analysis hyd destination 2016-2017 combined (violence left-right).xlsx")
local_GWR_PML_sensitivity_hyd_dest_tbl <- read_xlsx("Colombia Data/local GWR PML result predicted prices/sensitivity analysis hyd destination 2016-2017 combined lab_prob (violence left-right).xlsx")
sensitivity_summary <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% group_by(id) %>%
  summarize(y=y[1], n_params=n(), bw_sd=sd(bw, na.rm=T), across(coca_area:pi_hat,  \(x) sd(x, na.rm = TRUE)))
sensitivity_summary_coef <- sensitivity_summary %>% ungroup %>% select(-pi_hat)

coef_sensitivity_summary <- sensitivity_summary_coef %>%
  summarise(across(coca_area:left_right,
                   list(
                     Min = ~min(., na.rm = TRUE),
                     Q1 = ~quantile(., 0.25, na.rm = TRUE),
                     Median = ~median(., na.rm = TRUE),
                     Mean = ~mean(., na.rm = TRUE),
                     Q3 = ~quantile(., 0.75, na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE)
                   )
  ))

coef_sensitivity_table <- coef_sensitivity_summary %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "(.+)_(Min|Q1|Median|Mean|Q3|Max)")

final_table <- coef_sensitivity_table %>%
  select(Variable, Min, Q1, Median, Mean, Q3, Max)

final_table$n_large_sd <- sensitivity_summary_coef %>% select(coca_area:left_right) %>% apply(2, function(x) sum(abs(x) > 20, na.rm=T))

kable(
  final_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  caption = "Local GWR PML sensitivity analysis for $n_y$ and $n_{large\\_sd}$"
)

AUC_1617 <- c()
AUC_param <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% select(n_y, n_drop) %>% unique
for (i in 1:nrow(AUC_param)) {
  n_y_i <- AUC_param$n_y[i]
  n_drop_i <- AUC_param$n_drop[i]
  pi_hat_1617 <- local_GWR_PML_sensitivity_hyd_dest_tbl %>% filter(n_y == n_y_i & n_drop == n_drop_i) %>% select(y, pi_hat) %>% mutate(y=as.factor(y))
  
  AUC_1617 <- c(AUC_1617, ROC_pred(pi_hat_1617)$auc)
}
AUC_param$AUC <- AUC_1617
AUC_param$n_y <- as.factor(AUC_param$n_y)

AUC_param %>% print(n=48)
AUC_plot <- AUC_param %>% ggplot + ylim(0.80, 0.90) +
  geom_point(aes(x=n_drop, y=AUC, group=n_y, color=n_y)) +
  geom_line(aes(x=n_drop, y=AUC, group=n_y, color=n_y))

ggsave("Colombia Data/local GWR PML result predicted prices/sensitivity analysis AUC.png", AUC_plot, scale=1)


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
    labs(fill=var_name, x="", y="") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/sensitivity maps (%s)/2016-2017 combined/coef sensitivity map (%s).png",
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


# PML GWR coef map by AUC scores
local_gwr_PML_coef_map_by_AUC <- function(coef_table, pval_table, dep_var, alpha=0.1, n_drop, year_, indep_vars_, price) {
  # coef_table = PML_gwr_coefs_AUC_lab_prob_1617; pval_table = PML_gwr_pvals_AUC_lab_prob_1617; dep_var = "hyd_destination"; alpha=0.1; n_drop = 10; year_ = 1617; indep_vars_ = indep_vars_in; price=F
  indep_vars_ <- c(indep_vars_, "left_wing:right_paramilitary")
  title_for_price <- ifelse(price, "with price", "no price")
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
        expand_limits(x = depto_map$long, y = depto_map$lat) +
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
        expand_limits(x = depto_map$long, y = depto_map$lat) +
        coord_quickmap() +
        scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","#C00000"),
                             values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                             na.value = "white") +
        scale_pattern_manual(values = c("TRUE" = "stripe", "FALSE" = "none"), 
                             guide = "none"
                             ) +
        labs(fill=var_name, x=NULL, y=NULL, title=NULL)
      
      gwr_coef_map <- map_geospatial(gwr_coef_map)
    }
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%i)/local GWR PML coef by AUC violence_all left-right %s %s all var drop n_drop=%i %i data %s.png",
                   dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price),
           gwr_coef_map, units = "in", width = 5, height = 7)
  }
}

## coef map by AUC var drop 
local_gwr_PML_coef_map_by_AUC_year <- function(PML_gwr_coefs, PML_gwr_pvals, dep_var_, year_, price_=F) {
  if (price_) {
    indep_vars_in <- indep_vars
    title_for_price <- "with price"
  }else{
    indep_vars_in <- indep_vars[-which(indep_vars == "price_avg")]
    title_for_price <- "no price"
  } 
  indep_vars_in <- c("Intercept", indep_vars_in)
  
  local_gwr_PML_coef_map_by_AUC(PML_gwr_coefs, PML_gwr_pvals, dep_var = dep_var_, indep_vars_ = indep_vars_in, n_drop=10, year_=year_, price=price_)
}

# PML_gwr_coefs_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_residual (07-15-2026).csv") %>% as_tibble
# PML_gwr_pvals_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_residual (07-15-2026).csv") %>% as_tibble
# local_gwr_PML_coef_map_by_AUC_year(PML_gwr_coefs_AUC_CF_1617, PML_gwr_pvals_AUC_CF_1617, "hyd_destination", year_=1617)

indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_prob", "left_wing", "right_paramilitary")
PML_gwr_coefs_AUC_lab_prob_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML coefs hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_prob (07-15-2026).csv") %>% as_tibble
PML_gwr_pvals_AUC_lab_prob_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR PML p-value hyd_destination violence_all left-right all var drop by AUC n_drop=10 1617 data no price lab_prob (07-15-2026).csv") %>% as_tibble
# local_gwr_PML_coef_map_by_AUC_year(PML_gwr_coefs_AUC_lab_prob_1617, PML_gwr_pvals_AUC_lab_prob_1617, "hyd_destination", year_=1617)

# GWR_predict_1617_CF_with_1617_coef %>% arrange(desc(pi_hat))

# Region-specific factors
sig_level <- 0.1
n_sig_depto <- PML_gwr_coefs_AUC_lab_prob_1617 %>% 
  left_join(municipios_capital %>% select(id, depto), by = "id") %>% 
  group_by(depto) %>% 
  summarize(across(coca_area:left_right, ~ sum(.x <= sig_level, na.rm = T))) %>% 
  left_join(municipios_capital %>% select(id_depto, depto) %>% unique, by = "depto") %>%
  relocate(id_depto, depto)

n_sig_depto
for (i in 3:ncol(n_sig_depto)) {
  n_sig_map_coords <- depto_map %>% left_join(n_sig_depto[,c(1,i)] %>% rename(id = id_depto) %>% mutate(id = as.integer(id)), by="id")
  var_name <- names(n_sig_depto)[i]
  names(n_sig_map_coords)[ncol(n_sig_map_coords)] <- "n_sig"
  
  n_sig_map <- ggplot(n_sig_map_coords, aes(x=long, y=lat)) +
    geom_polygon(aes(group=group, fill=n_sig),
                         color = "black",
                         linewidth = 0.1) +
    expand_limits(x = depto_map$long, y = depto_map$lat) +
    coord_quickmap() +
    scale_fill_viridis_c(na.value = "white") +
    labs(fill=var_name, x=NULL, y=NULL, title=NULL) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank()
    )
  ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/n_sig maps/local GWR PML n_sig %s.png", var_name), n_sig_map, scale=1)
}

# For Case Study (predicted probability: 23672=1, 54206 = 0.931)
PML_gwr_coefs_AUC_lab_prob_1617 %>% filter(id %in% c(54206, 23672))
PML_gwr_pvals_AUC_lab_prob_1617 %>% filter(id %in% c(54206, 23672))
regression_data_lab_prob_1617 %>% filter(id %in% c(54206, 23672)) %>% select(-c(base_source:PPI_lab_prob, base_avg, hyd_avg, base_seizures), hyd_destination)
municipio_centroid %>% filter(id %in% c(54206, 23672))

# For Potential Destinations (predicted probability: 27450=0.998, 50226 = 0.989)
PML_gwr_coefs_AUC_lab_prob_1617 %>% filter(id %in% c(27450, 50226))
PML_gwr_pvals_AUC_lab_prob_1617 %>% filter(id %in% c(27450, 50226))
regression_data_lab_prob_1617 %>% filter(id %in% c(27450, 50226)) %>% select(-c(base_source:PPI_lab_prob, base_avg, hyd_avg, base_seizures), hyd_destination)
municipio_centroid %>% filter(id %in% c(27450, 50226))


PML_gwr_pvals_AUC_CF_1617 %>% select(Intercept:left_right) %>% apply(2, function(x) sum(x <= 0.1, na.rm=T))

local_gwr_PML_coef_map_by_AUC_year(PML_gwr_coefs_AUC_CF_1617, PML_gwr_pvals_AUC_CF_1617, "hyd_destination", 1617, price_=F)
regression_data_CF_1617

global_reg_year_CF <- function(reg_data_year, dep_var, no_price) {
  indep_vars <- c("price_avg", "coca_area", "seizures", "river_length", "road_length", "population", "airport", "ferry", "police", "military", "lab_prob", "left_wing", "right_paramilitary")
  dep_var_index <- which(names(reg_data_year) == dep_var)
  names(reg_data_year)[dep_var_index] <- "y"
  if (grepl("hyd", dep_var_)) {
    reg_data_year_pred <- reg_data_year %>% 
      select(-PPI_lab_prob, -base_avg, -base_seizures) %>%
      rename(price_avg=hyd_avg, lab_prob=hyd_lab_prob, seizures=hyd_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }else{
    reg_data_year_pred <- reg_data_year %>% 
      select(-hyd_lab_prob, -hyd_avg, -hyd_seizures) %>%
      rename(price_avg=base_avg, lab_prob=PPI_lab_prob, seizures=base_seizures) %>% 
      select(id, y, all_of(indep_vars))
  }
  # if (grepl("hyd", dep_var)) {
  #   reg_data_year_pred <- reg_data_year %>% 
  #     select(-PPI_lab, -PPI_lab_res, -base_avg, -base_seizures) %>%
  #     rename(price_avg=hyd_avg, lab_reported=hyd_lab, lab_residual=hyd_lab_res, seizures=hyd_seizures) %>% 
  #     select(id, y, all_of(indep_vars))
  # }else{
  #   reg_data_year_pred <- reg_data_year %>% 
  #     select(-hyd_lab, -hyd_lab_res, -hyd_avg, -hyd_seizures) %>%
  #     rename(price_avg=base_avg, lab_reported=PPI_lab, lab_residual=PPI_lab_res, seizures=base_seizures) %>% 
  #     select(id, y, all_of(indep_vars))
  # }
  
  if (no_price) reg_data_year_pred$price_avg <- NULL
  
  global_reg_model_CF_year <- glm(y~.+left_wing*right_paramilitary, data=reg_data_year_pred %>% select(-id), family=binomial)
  return(global_reg_model_CF_year)
}
# global_reg_coefs_CF_1617 <- global_reg_year_CF(regression_data_CF_1617, "hyd_destination", no_price=T) %>% coef
global_reg_coefs_CF_1617 <- global_reg_year_CF(regression_data_lab_prob_1617, "hyd_destination", no_price=T) %>% coef
names(global_reg_coefs_CF_1617)[1] <- "Intercept"
names(global_reg_coefs_CF_1617)[length(global_reg_coefs_CF_1617)] <- "left_right"

PML_gwr_coefs_AUC_CF_1617 <- PML_gwr_coefs_AUC_lab_prob_1617
PML_gwr_pvals_AUC_CF_1617 <- PML_gwr_pvals_AUC_lab_prob_1617
gwr_summary <- PML_gwr_coefs_AUC_CF_1617 %>%
  summarise(across(Intercept:left_right,
                   list(
                     Min = ~min(., na.rm = TRUE),
                     Q1 = ~quantile(., 0.25, na.rm = TRUE),
                     Median = ~median(., na.rm = TRUE),
                     Mean = ~mean(., na.rm = TRUE),
                     Q3 = ~quantile(., 0.75, na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE)
                   )
  ))

gwr_table <- gwr_summary %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "(.+)_(Min|Q1|Median|Mean|Q3|Max)")
gwr_table$global <- global_reg_coefs_CF_1617[ match(gwr_table$Variable, names(global_reg_coefs_CF_1617)) ]

gwr_table <- gwr_table %>%
  select(Variable, global, Min, Q1, Median, Mean, Q3, Max)
gwr_table$n_sig <- PML_gwr_pvals_AUC_CF_1617 %>% select(-(id:bw)) %>% apply(2, function(x) sum(x <= 0.1, na.rm=T))

footer <- data.frame(
  Variable = "AUC",
  global = 0.692,
  Min = 0.824,
  Q1 = NA,
  Median = NA,
  Mean = NA,
  Q3 = NA,
  Max = NA,
  n_sig = NA
)

final_table <- bind_rows(gwr_table, footer)

kable(
  final_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  caption = "global and GWR Results"
)

PML_gwr_coefs_AUC_CF_1617 %>% summary

# final_table %>%
#   kbl(
#     format = "latex",
#     booktabs = TRUE,
#     digits = 3,
#     escape = FALSE,
#     caption = "global and GWR Results"
#   ) %>%
#   add_header_above(c(
#     " " = 2,
#     "GWR Spatially Varying Coefficients (SVCs)" = 6
#   )) %>%
#   kable_styling(
#     latex_options = c("hold_position")
#   )


#### For LASSO
# coef map by AUC scores
local_gwr_LASSO_coef_map_by_AUC <- function(coef_table, PML_coef_table, dep_var, alpha=0.1, n_drop, date_, year_, indep_vars_, price) {
  indep_vars_ <- c(indep_vars_, "left_wing:right_paramilitary")
  title_for_price <- ifelse(price, "with price", "no price")
  id_excluded <- PML_coef_table %>% filter(is.na(bw)) %>% pull(id)
  
  for (i in c(2, 4:length(coef_table))) {
    var_name <- names(coef_table)[i]
    gwr_coefs_i <- data.frame(id=coef_table$id,
                              excluded=coef_table$id %in% id_excluded,
                              coef=coef_table[[var_name]],
                              rounded_coef=coef_table[[var_name]] %>% round(3))
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
        expand_limits(x = depto_map$long, y = depto_map$lat) +
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
        expand_limits(x = depto_map$long, y = depto_map$lat) +
        coord_quickmap() +
        scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","#C00000"),
                             values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, max_coef/abs(min_coef))),
                             na.value = "white") +
        labs(fill=var_name, x=NULL, y=NULL, title=NULL) +
        scale_pattern_manual(values = c("TRUE" = "stripe", "FALSE" = "none"), 
                             guide = "none")  +
        labs(fill=var_name, x=NULL, y=NULL, title=NULL)
        
        gwr_coef_map <- map_geospatial(gwr_coef_map)
    }
    
    ggsave(sprintf("Colombia Data/local GWR PML result predicted prices/coef maps/%s (%s)/local GWR lasso coef by AUC violence_all left-right %s %s %i %s data combined %s.png",
                   dep_var, year_, var_name, dep_var, n_drop, year_, title_for_price),
           gwr_coef_map, units = "in", width = 5, height = 7)
    
  }
}

## coef map by AUC var drop 
local_gwr_LASSO_coef_map_by_AUC_year <- function(LASSO_gwr_coefs, PML_gwr_coefs, dep_var_, year_, price_=F) {
  if (price_) {
    indep_vars_in <- indep_vars
    title_for_price <- "with price"
  }else{
    indep_vars_in <- indep_vars[-which(indep_vars == "price_avg")]
    title_for_price <- "no price"
  } 
  indep_vars_in <- c("Intercept", indep_vars_in)
  
  local_gwr_LASSO_coef_map_by_AUC(LASSO_gwr_coefs, PML_gwr_coefs, dep_var = dep_var_, indep_vars_ = indep_vars_in, n_drop=10, date_="", year_=year_, price=price_)
}

# LASSO_gwr_coefs_AUC_CF_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs hyd_destination violence_all left-right by AUC n_drop=10 2016-2017 data combined no price lab_residual.csv") %>% as_tibble
LASSO_gwr_coefs_AUC_lab_prob_1617 <- read.csv("Colombia Data/local GWR PML result predicted prices/local GWR LASSO coefs hyd_destination violence_all left-right by AUC n_drop=10 2016-2017 data combined no price lab_prob.csv") %>% as_tibble

excluded_index <- which(is.na(PML_gwr_coefs_AUC_lab_prob_1617$bw))
LASSO_gwr_coefs_AUC_lab_prob_1617[excluded_index, -1] <- NA
local_gwr_LASSO_coef_map_by_AUC_year(LASSO_gwr_coefs_AUC_lab_prob_1617, PML_gwr_coefs_AUC_lab_prob_1617, "hyd_destination", "2016-2017")

LASSO_gwr_coefs_AUC_CF_1617 %>%
  reframe(across(coca_area:left_right,
                 ~ sort(., decreasing = TRUE)[1:10]
  ))

LASSO_gwr_coefs_AUC_CF_1617 %>%
  reframe(across(coca_area:left_right,
                 ~ sort(.)[1:10]
  ))


gwr_summary <- LASSO_gwr_coefs_AUC_CF_1617 %>%
  summarise(across(Intercept:left_right,
                   list(
                     Min = ~min(., na.rm = TRUE),
                     Q1 = ~quantile(., 0.25, na.rm = TRUE),
                     Median = ~median(., na.rm = TRUE),
                     Mean = ~mean(., na.rm = TRUE),
                     Q3 = ~quantile(., 0.75, na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE)
                   )
  ))

gwr_table <- gwr_summary %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_pattern = "(.+)_(Min|Q1|Median|Mean|Q3|Max)")

gwr_table <- gwr_table %>%
  select(Variable, Min, Q1, Median, Mean, Q3, Max)

footer <- data.frame(
  Variable = "AUC",
  Min = 0.84,
  Q1 = NA,
  Median = NA,
  Mean = NA,
  Q3 = NA,
  Max = NA
)

final_table <- bind_rows(gwr_table, footer)
# final_table$n_large_coef <- c(LASSO_gwr_coefs_AUC_CF_1617 %>% select(Intercept:left_right) %>% apply(2, function(x) sum(abs(x) > 1000, na.rm=T)), NA)

kable(
  final_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  escape = FALSE,
  caption = "Local GWR LASSO Results"
)

