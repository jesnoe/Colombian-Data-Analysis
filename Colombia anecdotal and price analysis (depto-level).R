# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(fpp2)
library(MTS)

base_to_base <- read.csv("Colombia Data/Anecdotal base to base with id.csv") %>% as_tibble
price_2013_2015 <- read.csv("Colombia Data/Colombia Price Data 2013-2015 edited.csv") %>% as_tibble
price_2016_2021 <- read.csv("Colombia Data/Colombia Price Data 2016-2021 edited.csv") %>% as_tibble

price_2013 <- price_2013_2015 %>% filter(year == 2013)
price_2013_2021 <- rbind(price_2013_2015, price_2016_2021 %>% select(-seeds, -leaves))
price_2014_2021 <- rbind(price_2013_2015 %>% filter(year > 2013), price_2016_2021 %>% select(-seeds, -leaves)) %>%
  mutate(month=as.numeric(month)) %>% arrange(id, year, month)

price_2013_2021 %>% nrow # 4477
price_2013_2021 %>% filter(!is.na(paste_avg)) %>% nrow # 902
price_2013_2021 %>% filter(!is.na(base_avg)) %>% nrow # 1961
price_2013_2021 %>% filter(!is.na(hyd_avg)) %>% nrow # 3581

municipio_prices_summary <- rbind(price_2013_2021$paste_avg %>% summary,
                                  price_2013_2021$base_avg %>% summary,
                                  price_2013_2021$hyd_avg %>% summary) %>% as.data.frame
municipio_prices_summary$Min. <- as.integer(municipio_prices_summary$Min.)
rownames(municipio_prices_summary) <- c("paste", "base", "hyd")
municipio_prices_summary$n_obs <- c(902, 1961, 3581)
municipio_prices_summary

# Price variance
price_2013_2021 %>% filter(paste_avg < 10000)
price_2013_2021 %>% filter(base_avg < 10000) %>% select(month, year, municipio, depto, base_avg, base_wholesale, base_retail) %>% view
price_2013_2021 %>% filter(hyd_avg < 10000) %>% select(month, year, municipio, depto, hyd_avg, hyd_wholesale, hyd_retail) %>% view

price_2013_2021_summary <- price_2013_2021 %>% filter(year < 2018) %>% 
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

price_2013_2021_summary %>% filter(sd_paste_avg > 1000000) %>% select(month, year, depto, sd_paste_avg) %>% view
price_2013_2021 %>% filter(id_depto==76, year == 2014, month == 6) %>% select(month, year, municipio, depto, paste_avg, paste_wholesale, paste_retail) %>% view

price_2013_2021_summary %>% filter(sd_base_avg > 1000000) %>% select(month, year, depto, sd_base_avg) %>% view
price_2013_2021 %>% filter(id_depto==5, year == 2017, month %in% 2:5) %>% select(month, year, municipio, depto, base_avg, base_wholesale, base_retail) %>% view

price_2013_2021_summary %>% filter(sd_hyd_avg > 3000000) %>% select(month, year, depto, sd_hyd_avg) %>% view
price_2013_2021 %>% filter(id_depto==5, year == 2020, month == 8) %>% select(month, year, municipio, depto, hyd_avg, hyd_wholesale, hyd_retail) %>% view
price_2013_2021 %>% filter(id_depto==15, year == 2016, month %in% c(3, 8, 9)) %>%
  select(month, year, municipio, depto, hyd_avg, hyd_wholesale, hyd_retail) %>% view


# Price existence map
map <- departamentos
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id_depto=as.numeric(id)) %>% 
  filter(id_depto != 88) # excludes islands in Caribbean

for (year_i in 2013:2021) { # annual map
  price_2013_2021_map <- price_2013_2021_summary %>%
    filter(year == year_i) %>% 
    group_by(id_depto, year) %>% 
    summarise(paste_avg=ifelse(sum(!is.na(mean_paste_avg)) > 0, T, NA),
           base_avg=ifelse(sum(!is.na(mean_base_avg)) > 0, T, NA),
           hyd_avg=ifelse(sum(!is.na(mean_hyd_avg)) > 0, T, NA)) %>% 
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


month_year <- price_2013_2021_summary %>% as.data.frame %>% select(month, year) %>% unique %>% arrange(year, month)

for (i in 1:nrow(month_year)) { # monthly map
  month_i <- month_year$month[i]
  year_i <- month_year$year[i]
  price_2013_2021_map <- price_2013_2021_summary %>%
    filter(month == month_i & year == year_i) %>% 
    mutate(paste_avg=ifelse(sum(!is.na(mean_paste_avg)) > 0, T, NA),
           base_avg=ifelse(sum(!is.na(mean_base_avg)) > 0, T, NA),
           hyd_avg=ifelse(sum(!is.na(mean_hyd_avg)) > 0, T, NA)) %>% 
    select(id_depto, base_avg, paste_avg, hyd_avg) %>% 
    as.data.frame
  map_df_i <- left_join(map_df, price_2013_2021_map, by="id_depto")
  month_year_i <- paste(year_i, month_i, sep="-")
  
  paste_price_map_i <- ggplot(map_df_i, aes_string(map_id = "id")) + 
    geom_map(aes_string(fill = "paste_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_manual(values="orange",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Paste", month_year_i)) +
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
    labs(fill = "", x="", y="", title=paste("Base", month_year_i))
  
  hyd_price_map_i <- base_price_map_i +
    geom_map(aes_string(fill = "hyd_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    scale_fill_manual(values="red",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Hydrochloride", month_year_i))
  
  combined_map <- grid.arrange(paste_price_map_i, base_price_map_i, hyd_price_map_i, ncol=3)
  ggsave(paste0("Colombia Data/Figs/Price map/department-level price map (", month_year_i, ").png"), combined_map, width=40, height=15, unit="cm")
}


### Time series plot
price_2013_2021_summary_ts <- price_2013_2021_summary %>%
  mutate(month=ifelse(month == "January to April", 10,
                      ifelse(month == "May to August", 11,
                             ifelse(month == "September to December", 12, month)
                             )
                      ) %>% as.numeric)
  
price_2013_2021_n_obs <- price_2013_2021 %>% 
  filter(year < 2018) %>% 
  group_by(id_depto, depto) %>%
  summarise(n_paste_avg=sum(paste_avg > 0, na.rm=T),
            n_base_avg=sum(base_avg > 0, na.rm=T),
            n_hyd_avg=sum(hyd_avg > 0, na.rm=T))
price_2013_2021_n_obs %>% select(depto, n_paste_avg) %>% arrange(desc(n_paste_avg)) %>% view
price_2013_2021_n_obs %>% select(depto, n_base_avg) %>% arrange(desc(n_base_avg)) %>% view
price_2013_2021_n_obs %>% select(depto, n_hyd_avg) %>% arrange(desc(n_hyd_avg)) %>% view

month_year_ts <- tibble(month=c(10:12, rep(1:12, 4)),
                        year=c(rep(2013, 3), rep(2014:2017, each=12)))

price_Magdalena <- price_2013_2021_summary_ts %>%
  filter(id_depto == 47) %>% 
  right_join(month_year_ts, by=c("month", "year")) %>% 
  arrange(year, month)
price_Antioquia <- price_2013_2021_summary_ts %>%
  filter(id_depto == 5) %>% 
  right_join(month_year_ts, by=c("month", "year")) %>% 
  arrange(year, month)
price_Valle_del_Cauca <- price_2013_2021_summary_ts %>%
  filter(id_depto == 25) %>% 
  right_join(month_year_ts, by=c("month", "year")) %>% 
  arrange(year, month)

mts <- function(dt) {
  result <- ts(dt[,c("mean_paste_avg", "sd_paste_avg", "mean_base_avg", "sd_base_avg", "mean_hyd_avg", "sd_hyd_avg")],
     start=c(2013, 10), frequency=12)
  return(result)
}
price_Magdalena_ts <- mts(price_Magdalena)
p1 <- autoplot(price_Magdalena_ts[,c("mean_paste_avg", "sd_paste_avg")],
               xlab="Year", ylab="Price", main="Magdalena")
p2 <- autoplot(price_Magdalena_ts[,c("mean_base_avg", "sd_base_avg")],
               xlab="Year", ylab="Price", main="Magdalena")
p3 <- autoplot(price_Magdalena_ts[,c("mean_hyd_avg", "sd_hyd_avg")],
               xlab="Year", ylab="Price", main="Magdalena")
grid.arrange(p1, p2, p3, ncol=3)

price_Antioquia_ts <- mts(price_Antioquia)
p1 <- autoplot(price_Antioquia_ts[,c("mean_paste_avg", "sd_paste_avg")],
               xlab="Year", ylab="Price", main="Antioquia")
p2 <- autoplot(price_Antioquia_ts[,c("mean_base_avg", "sd_base_avg")],
               xlab="Year", ylab="Price", main="Antioquia")
p3 <- autoplot(price_Antioquia_ts[,c("mean_hyd_avg", "sd_hyd_avg")],
               xlab="Year", ylab="Price", main="Antioquia")
grid.arrange(p1, p2, p3, ncol=3)

price_Valle_del_Cauca_ts <- mts(price_Valle_del_Cauca)
p1 <- autoplot(price_Valle_del_Cauca_ts[,c("mean_paste_avg", "sd_paste_avg")],
               xlab="Year", ylab="Price", main="Valle del Cauca")
p2 <- autoplot(price_Valle_del_Cauca_ts[,c("mean_base_avg", "sd_base_avg")],
               xlab="Year", ylab="Price", main="Valle del Cauca")
p3 <- autoplot(price_Valle_del_Cauca_ts[,c("mean_hyd_avg", "sd_hyd_avg")],
               xlab="Year", ylab="Price", main="Valle del Cauca")
grid.arrange(p1, p2, p3, ncol=3)