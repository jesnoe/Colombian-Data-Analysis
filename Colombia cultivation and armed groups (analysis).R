# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

# {
# eradication_aerial <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Aerial (Ha).xlsx")
# eradication_manual <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Manual (Ha).xlsx")
# coca_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca leaves (kg).xlsx")
# eradication_aerial <- eradication_aerial %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
# eradication_manual <- eradication_manual %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
# coca_seizures <- coca_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
# 
# eradication_aerial <- left_join(municipios_id, eradication_aerial, by="id")
# eradication_manual <- left_join(municipios_id, eradication_manual, by="id")
# coca_seizures <- left_join(municipios_id, coca_seizures, by="id")
# }

armed_groups_combined <- list()
years <- (2008:2016)[-c(2,8)]
for (year in years) {
   armed_groups_combined_year <- read.csv(paste("Colombia Data/Armed Groups (Combined)/Colombia-Armed groups-Paramilitar", year ,"(combined).csv")) %>%
    as_tibble
   # if (year < 2016) {
   #   eradication_aerial_index <- grep(year, names(eradication_aerial))
   #   eradication_aerial_name <- paste("errad_aerial", year, sep="_")
   #   names(eradication_aerial)[eradication_aerial_index] <- eradication_aerial_name
   #   armed_groups_combined_year <- full_join(armed_groups_combined_year, eradication_aerial[,c(1,eradication_aerial_index)], by="id")
   # }
   # 
   # eradication_manual_index <- grep(year, names(eradication_manual))
   # eradication_manual_name <- paste("errad_manual", year, sep="_")
   # names(eradication_manual)[eradication_manual_index] <- eradication_manual_name
   # armed_groups_combined_year <- full_join(armed_groups_combined_year, eradication_manual[,c(1,eradication_manual_index)], by="id")
   # 
   # coca_seizures_index <- grep(year, names(coca_seizures))
   # coca_seizures_name <- paste("coca_seizure", year, sep="_")
   # names(coca_seizures)[coca_seizures_index] <- coca_seizures_name
   # armed_groups_combined_year <- full_join(armed_groups_combined_year, coca_seizures[,c(1,coca_seizures_index)], by="id")
   
   armed_groups_combined[[paste0("y", year)]] <- armed_groups_combined_year
}
armed_groups_combined$y2008

# for (year in names(armed_groups_combined)) {
#   armed_groups_combined[[year]] %>%
#     write.csv(paste("Colombia Data/Armed Groups (Combined)/Colombia-Armed groups-Paramilitar", substr(year,2,5) ,"(combined).csv"), row.names=F)
# }


armed_group_names <- c()
for (data_year in armed_groups_combined) {
  armed_group_names <- c(armed_group_names, colnames(data_year[-(1:9)]))
}
table(armed_group_names) %>% sort

for (year_data in armed_groups_combined) {
  print(max(year_data$n_armed_groups, na.rm=T))
}

for (year_data in armed_groups_combined) {
  print(max(year_data[,7], na.rm=T))
}

for (year_data in armed_groups_combined) {
  print(max(year_data[,8], na.rm=T))
}

for (year_data in armed_groups_combined) {
  print(max(year_data[,9], na.rm=T))
}

for (year_data in armed_groups_combined) {
  if (grepl(2016, names(year_data)[7])) next
  print(max(year_data[,grep("aerial", names(year_data))], na.rm=T) %>% log)
  print(min(year_data[,grep("aerial", names(year_data))], na.rm=T) %>% log)
}# min/max = -1.237874/9.280468

for (year_data in armed_groups_combined) {
  print(max(year_data[,grep("manual", names(year_data))], na.rm=T) %>% log)
  print(min(year_data[,grep("manual", names(year_data))], na.rm=T) %>% log)
}# min/max = -3.218876/8.503613

for (year_data in armed_groups_combined) {
  print(max(year_data[,grep("seizure", names(year_data))], na.rm=T) %>% log)
  print(min(year_data[,grep("seizure", names(year_data))], na.rm=T) %>% log)
}# min/max = -3.863233/12.90072

map <- municipios
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(!(id %in% c(88001, 88564))) # excludes islands in Caribbean
palette <- colorRampPalette(c("grey60", "#b30000"))

n_armed_groups_maps <- list()
cultivation_maps <- list()
log_cultivation_maps <- list()
labs_HCl_maps <- list()
labs_PPI_maps <- list()
log_errad_aerial_maps <- list()
log_errad_manual_maps <- list()
log_coca_seizure_maps <- list()
for (year in names(armed_groups_combined)) {
  data_year <- armed_groups_combined[[year]]
  year_num <- substr(year,2,5)
  data_year[[paste("log_cultivation", year_num, sep="_")]] <- log(data_year[[paste("cultivation", year_num, sep="_")]])
  data_year[[paste("log_errad_manual", year_num, sep="_")]] <- log(data_year[[paste("errad_manual", year_num, sep="_")]])
  data_year[[paste("log_coca_seizure", year_num, sep="_")]] <- log(data_year[[paste("coca_seizure", year_num, sep="_")]])
  if (year_num < 2016) {
    data_year[[paste("log_errad_aerial", year_num, sep="_")]] <- log(data_year[[paste("errad_aerial", year_num, sep="_")]])
  }
  n_armed_groups_year <- ggplot(data_year, aes_string(map_id = "id")) + 
    geom_map(aes_string(fill = "n_armed_groups"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_gradientn(colors = palette(7), na.value = "white", limits=c(0,6)) +
    labs(fill = "", x="", y="", title=year_num) +
    theme_bw() +
    theme(legend.key.size = unit(.2, 'cm'),
          legend.text = element_text(size=4),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
    
  cultivation_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("cultivation", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(100), na.value = "white", limits=c(0,23148))
  
  log_cultivation_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("log_cultivation", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(100), na.value = "white", limits=c(0,11)) # min/max of log cultivation are 0/10.04966
  
  labs_HCl_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("labs_HCl", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(20), na.value = "white", limits=c(0,42))
  
  labs_PPI_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("labs_PPI", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(20), na.value = "white", limits=c(0,389))
  
  log_errad_manual_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("log_errad_manual", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(100), na.value = "white", limits=c(-4,9))
  
  log_coca_seizure_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("log_coca_seizure", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(100), na.value = "white", limits=c(-4,13))
  
  n_armed_groups_maps[[year]] <- n_armed_groups_year
  cultivation_maps[[year]] <- cultivation_year
  log_cultivation_maps[[year]] <- log_cultivation_year
  labs_HCl_maps[[year]] <- labs_HCl_year
  labs_PPI_maps[[year]] <-labs_PPI_year
  log_errad_manual_maps[[year]] <- log_errad_manual_year
  log_coca_seizure_maps[[year]] <- log_coca_seizure_year
  
  if (year_num < 2016) {
    log_errad_aerial_year <- n_armed_groups_year +
      geom_map(aes_string(fill = paste("log_errad_aerial", year_num, sep="_")),
               map = map_df,
               color = "black",
               linewidth = 0.1) +
      scale_fill_gradientn(colors = palette(100), na.value = "white", limits=c(-4,10))
    log_errad_aerial_maps[[year]] <- log_errad_aerial_year
  }
}

n_armed_groups_maps_2008_2012 <- grid.arrange(n_armed_groups_maps[[1]],
                                              n_armed_groups_maps[[2]],
                                              n_armed_groups_maps[[3]],
                                              n_armed_groups_maps[[4]],
                                              ncol=2)
n_armed_groups_maps_2013_2016 <- grid.arrange(n_armed_groups_maps[[5]],
                                              n_armed_groups_maps[[6]],
                                              n_armed_groups_maps[[7]],
                                              ncol=2)
cultivation_maps_2008_2012 <- grid.arrange(cultivation_maps[[1]],
                                           cultivation_maps[[2]],
                                           cultivation_maps[[3]],
                                           cultivation_maps[[4]],
                                           ncol=2)
cultivation_maps_2013_2016 <- grid.arrange(cultivation_maps[[5]],
                                           cultivation_maps[[6]],
                                           cultivation_maps[[7]],
                                           ncol=2)
log_cultivation_maps_2008_2012 <- grid.arrange(log_cultivation_maps[[1]],
                                               log_cultivation_maps[[2]],
                                               log_cultivation_maps[[3]],
                                               log_cultivation_maps[[4]],
                                               ncol=2)
log_cultivation_maps_2013_2016 <- grid.arrange(log_cultivation_maps[[5]],
                                               log_cultivation_maps[[6]],
                                               log_cultivation_maps[[7]],
                                               ncol=2)
labs_HCl_maps_2008_2012 <- grid.arrange(labs_HCl_maps[[1]],
                                        labs_HCl_maps[[2]],
                                        labs_HCl_maps[[3]],
                                        labs_HCl_maps[[4]],
                                        ncol=2)
labs_HCl_maps_2013_2016 <- grid.arrange(labs_HCl_maps[[5]],
                                        labs_HCl_maps[[6]],
                                        labs_HCl_maps[[7]],
                                        ncol=2)
labs_PPI_maps_2008_2012 <- grid.arrange(labs_PPI_maps[[1]],
                                        labs_PPI_maps[[2]],
                                        labs_PPI_maps[[3]],
                                        labs_PPI_maps[[4]],
                                        ncol=2)
labs_PPI_maps_2013_2016 <- grid.arrange(labs_PPI_maps[[5]],
                                        labs_PPI_maps[[6]],
                                        labs_PPI_maps[[7]],
                                        ncol=2)
log_errad_aerial_maps_2008_2012 <- grid.arrange(log_errad_aerial_maps[[1]],
                                                log_errad_aerial_maps[[2]],
                                                log_errad_aerial_maps[[3]],
                                                log_errad_aerial_maps[[4]],
                                               ncol=2)
log_errad_aerial_maps_2013_2015 <- grid.arrange(log_errad_aerial_maps[[5]],
                                                log_errad_aerial_maps[[6]],
                                               ncol=2)
log_errad_manual_maps_2008_2012 <- grid.arrange(log_errad_manual_maps[[1]],
                                                log_errad_manual_maps[[2]],
                                                log_errad_manual_maps[[3]],
                                                log_errad_manual_maps[[4]],
                                                ncol=2)
log_errad_manual_maps_2013_2016 <- grid.arrange(log_errad_manual_maps[[5]],
                                                log_errad_manual_maps[[6]],
                                                log_errad_manual_maps[[7]],
                                                ncol=2)
log_coca_seizure_maps_2008_2012 <- grid.arrange(log_coca_seizure_maps[[1]],
                                                log_coca_seizure_maps[[2]],
                                                log_coca_seizure_maps[[3]],
                                                log_coca_seizure_maps[[4]],
                                                ncol=2)
log_coca_seizure_maps_2013_2016 <- grid.arrange(log_coca_seizure_maps[[5]],
                                                log_coca_seizure_maps[[6]],
                                                log_coca_seizure_maps[[7]],
                                                ncol=2)

# ggsave("num of armed groups map (2008-2012).png", n_armed_groups_maps_2008_2012, scale=1)
# ggsave("num of armed groups map (2013-2016).png", n_armed_groups_maps_2013_2016, scale=1)
# ggsave("cultivation map (2008-2012).png", cultivation_maps_2008_2012, scale=1)
# ggsave("cultivation map (2013-2016).png", cultivation_maps_2013_2016, scale=1)
# ggsave("log_cultivation map (2008-2012).png", log_cultivation_maps_2008_2012, scale=1)
# ggsave("log_cultivation map (2013-2016).png", log_cultivation_maps_2013_2016, scale=1)
# ggsave("labs HCl map (2008-2012).png", labs_HCl_maps_2008_2012, scale=1)
# ggsave("labs HCl map (2013-2016).png", labs_HCl_maps_2013_2016, scale=1)
# ggsave("labs PPI map (2008-2012).png", labs_PPI_maps_2008_2012, scale=1)
# ggsave("labs PPI map (2013-2016).png", labs_PPI_maps_2013_2016, scale=1)
# ggsave("log_errad_aerial map (2008-2012).png", log_errad_aerial_maps_2008_2012, scale=1)
# ggsave("log_errad_aerial map (2013-2015).png", log_errad_aerial_maps_2013_2015, scale=1)
# ggsave("log_errad_manual map (2008-2012).png", log_errad_manual_maps_2008_2012, scale=1)
# ggsave("log_errad_manual map (2013-2016).png", log_errad_manual_maps_2013_2016, scale=1)
# ggsave("log_coca_seizure map (2008-2012).png", log_coca_seizure_maps_2008_2012, scale=1)
# ggsave("log_coca_seizure map (2013-2016).png", log_coca_seizure_maps_2013_2016, scale=1)

n_armed_groups_maps <- list()
cultivation_maps <- list()
labs_HCl_maps <- list()
labs_PPI_maps <- list()
for (year in names(armed_groups_combined)) {
  data_year <- armed_groups_combined[[year]]
  year_num <- substr(year,2,5)
  
  n_armed_groups_year <- ggplot(data_year, aes_string(map_id = "id")) + 
    geom_map(aes_string(fill = "n_armed_groups"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_gradientn(colors = palette(7), na.value = "white") +
    labs(fill = "", x="", y="", title=year_num) +
    theme_bw() +
    theme(legend.key.size = unit(.2, 'cm'),
          legend.text = element_text(size=4),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  
  cultivation_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("cultivation", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(100), na.value = "white")
  
  labs_HCl_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("labs_HCl", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(20), na.value = "white")
  
  labs_PPI_year <- n_armed_groups_year +
    geom_map(aes_string(fill = paste("labs_PPI", year_num, sep="_")),
             map = map_df,
             color = "black",
             linewidth = 0.1) +
    scale_fill_gradientn(colors = palette(20), na.value = "white")
  
  n_armed_groups_maps[[year]] <- n_armed_groups_year
  cultivation_maps[[year]] <- cultivation_year
  labs_HCl_maps[[year]] <- labs_HCl_year
  labs_PPI_maps[[year]] <-labs_PPI_year
}

n_armed_groups_maps_2008_2012 <- grid.arrange(n_armed_groups_maps[[1]],
                                              n_armed_groups_maps[[2]],
                                              n_armed_groups_maps[[3]],
                                              n_armed_groups_maps[[4]],
                                              ncol=2)
n_armed_groups_maps_2013_2016 <- grid.arrange(n_armed_groups_maps[[5]],
                                              n_armed_groups_maps[[6]],
                                              n_armed_groups_maps[[7]],
                                              ncol=2)
cultivation_maps_2008_2012 <- grid.arrange(cultivation_maps[[1]],
                                           cultivation_maps[[2]],
                                           cultivation_maps[[3]],
                                           cultivation_maps[[4]],
                                           ncol=2)
cultivation_maps_2013_2016 <- grid.arrange(cultivation_maps[[5]],
                                           cultivation_maps[[6]],
                                           cultivation_maps[[7]],
                                           ncol=2)
labs_HCl_maps_2008_2012 <- grid.arrange(labs_HCl_maps[[1]],
                                        labs_HCl_maps[[2]],
                                        labs_HCl_maps[[3]],
                                        labs_HCl_maps[[4]],
                                        ncol=2)
labs_HCl_maps_2013_2016 <- grid.arrange(labs_HCl_maps[[5]],
                                        labs_HCl_maps[[6]],
                                        labs_HCl_maps[[7]],
                                        ncol=2)
labs_PPI_maps_2008_2012 <- grid.arrange(labs_PPI_maps[[1]],
                                        labs_PPI_maps[[2]],
                                        labs_PPI_maps[[3]],
                                        labs_PPI_maps[[4]],
                                        ncol=2)
labs_PPI_maps_2013_2016 <- grid.arrange(labs_PPI_maps[[5]],
                                        labs_PPI_maps[[6]],
                                        labs_PPI_maps[[7]],
                                        ncol=2)

# ggsave("num of armed groups map separate ranges (2008-2012).png", n_armed_groups_maps_2008_2012, scale=1)
# ggsave("num of armed groups map separate ranges (2013-2016).png", n_armed_groups_maps_2013_2016, scale=1)
# ggsave("cultivation map separate ranges (2008-2012).png", cultivation_maps_2008_2012, scale=1)
# ggsave("cultivation map separate ranges (2013-2016).png", cultivation_maps_2013_2016, scale=1)
# ggsave("labs HCl map separate ranges (2008-2012).png", labs_HCl_maps_2008_2012, scale=1)
# ggsave("labs HCl map separate ranges (2013-2016).png", labs_HCl_maps_2013_2016, scale=1)
# ggsave("labs PPI map separate ranges (2008-2012).png", labs_PPI_maps_2008_2012, scale=1)
# ggsave("labs PPI map separate ranges (2013-2016).png", labs_PPI_maps_2013_2016, scale=1)