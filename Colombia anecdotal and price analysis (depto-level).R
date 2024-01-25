# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(fpp2)
library(sf)

base_to_base <- read.csv("Colombia Data/Anecdotal base to base with id.csv") %>% as_tibble
price_2013_2015 <- read.csv("Colombia Data/Colombia Price Data 2013-2015 edited.csv") %>% as_tibble
price_2016_2021 <- read.csv("Colombia Data/Colombia Price Data 2016-2021 edited.csv") %>% as_tibble

price_2013 <- price_2013_2015 %>% filter(year == 2013)
price_2013_2021 <- rbind(price_2013_2015, price_2016_2021 %>% select(-seeds, -leaves))
price_2014_2021 <- rbind(price_2013_2015 %>% filter(year > 2013), price_2016_2021 %>% select(-seeds, -leaves)) %>%
  mutate(month=as.numeric(month)) %>% arrange(id, year, month)

price_2013_2021 %>% nrow # 4477
price_2013_2021 %>% filter(!is.na(base_avg)) %>% nrow # 1961
price_2013_2021 %>% filter(!is.na(paste_avg)) %>% nrow # 902
price_2013_2021 %>% filter(!is.na(hyd_avg)) %>% nrow # 3581

municipio_prices_summary <- rbind(price_2013_2021$paste_avg %>% summary,
                                  price_2013_2021$base_avg %>% summary,
                                  price_2013_2021$hyd_avg %>% summary) %>% as.data.frame
municipio_prices_summary$Min. <- as.integer(municipio_prices_summary$Min.)
rownames(municipio_prices_summary) <- c("paste", "base", "hyd")
municipio_prices_summary

# Price variance
price_2013_2021 %>% filter(paste_avg < 10000)
price_2013_2021 %>% filter(base_avg < 10000) %>% select(month, year, municipio, depto, base_avg, base_wholesale, base_retail) %>% view
price_2013_2021 %>% filter(hyd_avg < 10000) %>% select(month, year, municipio, depto, hyd_avg, hyd_wholesale, hyd_retail) %>% view

price_2013_2021_summary <- price_2013_2021 %>% 
  group_by(id_depto, region, month, year, depto) %>% 
  summarise(
    mean_paste_avg=mean(paste_avg, na.rm=T),
    med_paste_avg=median(paste_avg, na.rm=T),
    sd_paste_avg=sd(paste_avg, na.rm=T),
    mean_base_avg=mean(base_avg, na.rm=T),
    med_base_avg=median(base_avg, na.rm=T),
    sd_base_avg=sd(base_avg, na.rm=T),
    mean_hyd_avg=mean(hyd_avg, na.rm=T),
    med_hyd_avg=median(hyd_avg, na.rm=T),
    sd_hyd_avg=sd(hyd_avg, na.rm=T),
  ) %>% arrange(id_depto, year, month)

price_2013_2021_summary
price_2013_2021_summary %>% filter(year < 2017)
price_2013_2021_summary %>% ggplot() +
  geom_histogram(aes(x=sd_paste_avg), bins=50) +
  labs(title="Paste", x="sd")
price_2013_2021_summary %>% ggplot() +
  geom_histogram(aes(x=sd_base_avg), bins=50) +
  labs(title="Base", x="sd")
price_2013_2021_summary %>% ggplot() +
  geom_histogram(aes(x=sd_hyd_avg), bins=50) +
  labs(title="Hydrochloride", x="sd")

price_2013_2021_summary %>% filter(sd_base_avg > 1000000) %>% select(month, year, depto, sd_base_avg) %>% view
price_2013_2021 %>% filter(id_depto==5, year == 2017) %>% select(month, year, municipio, depto, base_avg, base_wholesale, base_retail) %>% view


# Price existence map
map <- departamentos
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean

for (year_i in 2013:2021) {
  price_2013_2021_map <- price_2013_2021_summary %>%
    filter(year == year_i) %>% 
    group_by(id_depto, year) %>% 
    summarise(paste_avg=ifelse(sum(!is.na(mean_paste_avg)) > 0, T, NA),
           base_avg=ifelse(sum(!is.na(mean_base_avg)) > 0, T, NA),
           hyd_avg=ifelse(sum(!is.na(mean_hyd_avg)) > 0, T, NA),) %>% 
    select(id_depto, base_avg, paste_avg, hyd_avg) %>% 
    as.data.frame
  map_df_i <- left_join(map_df, price_2013_2021_map, by="id_depto")
  
  paste_price_map_i <- ggplot(map_df_i, aes_string(map_id = "id")) + 
    geom_map(aes_string(fill = "paste_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_manual(values="orange",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Paste Avg Price", year_i)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  base_price_map_i <- paste_price_map_i +
    geom_map(aes_string(fill = "base_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    scale_fill_manual(values="brown",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Base Avg Price", year_i))
  
  hyd_price_map_i <- paste_price_map_i +
    geom_map(aes_string(fill = "hyd_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    scale_fill_manual(values="red",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Hydrochloride Avg Price", year_i))
  
  combined_map <- grid.arrange(paste_price_map_i, base_price_map_i, hyd_price_map_i, ncol=3)
  ggsave(paste0("Colombia Data/Figs/Price map/department-level price map (", year_i, ").png"), combined_map, width=40, height=15, unit="cm")
  # ggsave(paste0("Colombia Data/Figs/Price map/base avg price map (", month_year_i, ").png"), base_price_map_i, scale=1)
  # ggsave(paste0("Colombia Data/Figs/Price map/paste avg price map (", month_year_i, ").png"), paste_price_map_i, scale=1)
  # ggsave(paste0("Colombia Data/Figs/Price map/hyd avg price map (", month_year_i, ").png"), hyd_price_map_i, scale=1)
}
