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
price_2013_2016 <- read.csv("Colombia Data/Colombia Price Data 2013-2016 edited.csv") %>% as_tibble

# base_to_base
# price_2013_2016 %>% 
#   group_by(id, municipio, depto) %>% 
#   summarise(non0_paste_avg = sum(paste_avg > 0),
#             non0_base_avg = sum(base_avg > 0),
#             non0_hyd_avg = sum(hyd_avg > 0))
# 
# price_2013_2016 %>% 
#   group_by(id, municipio, depto) %>% 
#   summarise(n_paste_avg = sum(!is.na(paste_avg)),
#             n_base_avg = sum(!is.na(base_avg)),
#             n_hyd_avg = sum(!is.na(hyd_avg)))
# 
# price_2013_2016 %>% select(month, year) %>% unique
# 
price_2013_2016 %>%
  group_by(id, municipio, depto, month, year) %>%
  filter(length(base_avg)>1) %>% as.data.frame

price_2013_2016 <- rbind(price_2013_2016 %>% filter(year == 2013),
                         price_2013_2016 %>% filter(year > 2013) %>% 
                      mutate(month=as.Date(paste0(1, month, "2000"), "%d%B%Y") %>% month) %>% 
                      arrange(year, month))

price_2013 <- price_2013_2016 %>% filter(year == 2013)

price_2013$month <- as.factor(price_2013$month)
price_2013 %>% 
  group_by(id, municipio, depto) %>% 
  summarise(n_paste_avg = sum(!is.na(paste_avg)),
            n_base_avg = sum(!is.na(base_avg)),
            n_hyd_avg = sum(!is.na(hyd_avg)))


## Price correlation for 2014~2016 prices
price_2014_2016 <- price_2013_2016 %>% filter(year > 2013)
price_2014_2016 <- price_2014_2016 %>% 
  arrange(id, year, month) %>% 
  relocate(id, month, year, depto, municipio)
price_2014_2016 %>% select(-paste_wholesale, -paste_retail, -base_wholesale, -base_retail, -hyd_wholesale, -hyd_retail)

base_ge4_id <- price_2014_2016 %>% 
  group_by(id, municipio, depto) %>% 
  summarise(id_depto=id_depto[1],
            non0_paste_avg = sum(paste_avg > 0),
            non0_base_avg = sum(base_avg > 0),
            non0_hyd_avg = sum(hyd_avg > 0)) %>%
  filter(non0_base_avg > 3) %>% 
  pull(id)

base_dyads <- tibble(source_id=base_ge4_id, destination_id=base_ge4_id) %>%
  complete(source_id, destination_id) %>% 
  filter(source_id != destination_id) %>% 
  rbind(base_to_base %>% select(source_id, destination_id)) %>% unique

get_corr <- function(dyads, price=price_2014_2016, cocaine) {
  
  result <- tibble(pearson_corr=0, spearman_corr=0, kendall_corr=0, direction=0, n_prices=0)
  for (i in 1:nrow(dyads)) {
    source_id <- dyads[[1]][i]
    destination_id <- dyads[[2]][i]
    source_price <- price_2014_2016 %>% filter(id == source_id)
    destination_price <- price_2014_2016 %>% filter(id == destination_id)
    coexisting_MY <- source_price[which(source_price[[cocaine]] > 0), c(2:3, which(names(source_price)==cocaine) )] %>% 
      inner_join(destination_price[which(destination_price[[cocaine]] > 0), c(2:3, which(names(destination_price)==cocaine))], by=c("month", "year"))
    source_prices_co <- coexisting_MY[[3]]
    destination_prices_co <- coexisting_MY[[4]]
    pearson_corr <- cor(source_prices_co, destination_prices_co)
    spearman_corr <- cor(source_prices_co, destination_prices_co, method="spearman")
    kendall_corr <- cor(source_prices_co, destination_prices_co, method="kendall")
    result <- result %>% rbind(c(pearson_corr,
                                 spearman_corr,
                                 kendall_corr,
                                 sign(mean(destination_prices_co)-mean(source_prices_co)),
                                 nrow(coexisting_MY)))
  }
  result <- result[-1,]
  return(result)
}

municipios_corr <- municipios@data %>% mutate(id=as.numeric(id), id_depto=as.numeric(id_depto))
source_locations <- left_join(base_dyads %>% rename(id=source_id), municipios_corr %>% select(id, municipio, depto), by="id") %>% 
  apply(1, function(x) paste(x[3], x[4], sep=", "))
destination_locations <- left_join(base_dyads %>% rename(id=destination_id), municipios_corr %>% select(id, municipio, depto), by="id") %>% 
  apply(1, function(x) paste(x[3], x[4], sep=", "))
base_dyads$source_location <- source_locations
base_dyads$destination_location <- destination_locations
base_corr <- get_corr(base_dyads, price=price_2014_2016, cocaine="base_avg")
base_dyads <- base_dyads %>% cbind(base_corr) %>% as_tibble

base_to_base %>% filter(is.na(destination_id))


base_to_base <- base_to_base %>% mutate(id_pair=paste(source_id, destination_id, sep=","))
base_dyads$anecdotal <- data.frame(id_pair=paste(base_dyads$source_id, base_dyads$destination_id, sep=",")) %>% 
  mutate(check=ifelse(id_pair %in% base_to_base$id_pair, 1, 0)) %>% pull(check)
base_dyads %>% arrange(desc(n_prices)) %>% view
# write.csv("Colombia Data/base price correlations.csv", row.names=F)
base_dyads %>% filter(source_id == destination_id)


base_dyads %>% filter(pearson_corr == 0)
base_dyads %>% filter(spearman_corr == 0)
base_dyads %>% filter(kendall_corr == 0)
zero_corr_id <- base_dyads %>% filter(pearson_corr == 0) %>% select(source_id, destination_id)
price_2014_2016 %>% filter(id %in% c(5001, 54250)) %>% select(id:municipio, base_avg) %>% as.data.frame

zero_corr_price <- list()
for (i in 1:nrow(zero_corr_id)) {
  source_id <- zero_corr_id[[1]][i]
  destination_id <- zero_corr_id[[2]][i]
  source_price <- price_2014_2016 %>% filter(id == source_id)
  destination_price <- price_2014_2016 %>% filter(id == destination_id)
  coexisting_MY <- source_price[which(source_price[["base_avg"]] > 0), c(2:3, which(names(source_price)=="base_avg") )] %>% 
    inner_join(destination_price[which(destination_price[["base_avg"]] > 0), c(2:3, which(names(destination_price)=="base_avg"))], by=c("month", "year"))
  zero_corr_price[[paste0("id", source_id, ",", destination_id)]] <- coexisting_MY
}
zero_corr_price


# price maps
map <- municipios
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(!(id %in% c(88001, 88564))) # excludes islands in Caribbean
palette <- colorRampPalette(c("grey60", "#b30000"))

month_year <- price_2013_2016 %>% select(month, year) %>% unique

for (i in 1:nrow(month_year)) {
  month_i <- month_year$month[i]
  year_i <- month_year$year[i]
  price_2013_2016_map <- price_2013_2016 %>%
    filter(month == month_i & year == year_i) %>% 
    mutate(base_avg=ifelse(base_avg > 0, T, NA),
           paste_avg=ifelse(paste_avg > 0, T, NA),
           hyd_avg=ifelse(hyd_avg > 0, T, NA),) %>% 
    select(id, base_avg, paste_avg, hyd_avg) %>% 
    as.data.frame
  map_df_i <- left_join(map_df, price_2013_2016_map, by="id")
  month_year_i <- paste(year_i, month_i, sep="-")
  
  base_price_map_i <- ggplot(map_df_i, aes_string(map_id = "id")) + 
    geom_map(aes_string(fill = "base_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    expand_limits(x = map_df$long, y = map_df$lat) + 
    coord_quickmap() +
    scale_fill_manual(values="orange",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Base Avg Price", month_year_i)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  paste_price_map_i <- base_price_map_i +
    geom_map(aes_string(fill = "paste_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    scale_fill_manual(values="brown",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Paste Avg Price", month_year_i))
  
  hyd_price_map_i <- base_price_map_i +
    geom_map(aes_string(fill = "hyd_avg"),
             map = map_df,
             color = "black",
             linewidth = 0.1) + 
    scale_fill_manual(values="red",na.value = "white") +
    labs(fill = "", x="", y="", title=paste("Hydrochloride Avg Price", month_year_i))
  
  combined_map <- grid.arrange(base_price_map_i, paste_price_map_i, hyd_price_map_i, ncol=3)
  ggsave(paste0("Colombia Data/Figs/Price map/price map (", month_year_i, ").png"), combined_map, width=40, height=15, unit="cm")
  # ggsave(paste0("Colombia Data/Figs/Price map/base avg price map (", month_year_i, ").png"), base_price_map_i, scale=1)
  # ggsave(paste0("Colombia Data/Figs/Price map/paste avg price map (", month_year_i, ").png"), paste_price_map_i, scale=1)
  # ggsave(paste0("Colombia Data/Figs/Price map/hyd avg price map (", month_year_i, ").png"), hyd_price_map_i, scale=1)
}

## time series plot
price_47001 <- price_2013_2016 %>% filter(id==47001)
price_47001 %>% ggplot() +
  geom_line(aes(x=1:33, y=base_avg, color="Base")) +
  geom_line(aes(x=1:33, y=paste_avg, color="Paste")) +
  geom_line(aes(x=1:33, y=hyd_avg, color="Hyd")) +
  labs(color="Price", x="", y="price", title="SANTA MARTA, Magdalena")

price_85001 <- price_2013_2016 %>% filter(id==85001)
price_85001 <- left_join(month_year, price_85001, by=c("month", "year"))
price_85001 %>% ggplot() +
  geom_line(aes(x=1:33, y=base_avg, color="Base")) +
  geom_line(aes(x=1:33, y=paste_avg, color="Paste")) +
  geom_line(aes(x=1:33, y=hyd_avg, color="Hyd")) +
  labs(color="Price", x="", y="price", title="YOPAL, Casanare")
