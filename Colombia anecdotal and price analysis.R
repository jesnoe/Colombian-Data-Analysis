# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

base_to_base <- read.csv("Colombia Data/Anecdotal base to base with id.csv") %>% as_tibble
price_2013_2016 <- read.csv("Colombia Data/Colombia Price Data 2013-2016 edited.csv") %>% as_tibble

base_to_base
price_2013_2016 %>% 
  group_by(id, municipio, depto) %>% 
  summarise(non0_paste_avg = sum(paste_avg > 0),
            non0_base_avg = sum(base_avg > 0),
            non0_hyd_avg = sum(hyd_avg > 0))

price_2013_2016 %>% 
  group_by(id, municipio, depto) %>% 
  summarise(n_paste_avg = sum(!is.na(paste_avg)),
            n_base_avg = sum(!is.na(base_avg)),
            n_hyd_avg = sum(!is.na(hyd_avg)))

price_2013_2016 %>% select(month, year) %>% unique

price_2013 <- price_2013_2016 %>% filter(year == 2013)

price_2013$month <- as.factor(price_2013$month)
price_2013 %>% 
  group_by(id, municipio, depto) %>% 
  summarise(n_paste_avg = sum(!is.na(paste_avg)),
            n_base_avg = sum(!is.na(base_avg)),
            n_hyd_avg = sum(!is.na(hyd_avg)))

price_2014_2016 <- price_2013_2016 %>% filter(year > 2013)
price_2014_2016 <- price_2014_2016 %>% 
  mutate(month=as.Date(paste0(1, month, "2000"), "%d%B%Y") %>% month) %>% 
  arrange(id) %>% 
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
  
  result <- tibble(price_corr=0, direction=0, n_prices=0)
  for (i in 1:nrow(dyads)) {
    source_id <- dyads[[1]][i]
    destination_id <- dyads[[2]][i]
    source_price <- price_2014_2016 %>% filter(id == source_id)
    destination_price <- price_2014_2016 %>% filter(id == destination_id)
    coexisting_MY <- source_price[which(source_price[[cocaine]] > 0), c(2:3, which(names(source_price)==cocaine) )] %>% 
      inner_join(destination_price[which(destination_price[[cocaine]] > 0), c(2:3, which(names(destination_price)==cocaine))], by=c("month", "year"))
    source_prices_co <- coexisting_MY[[3]]
    destination_prices_co <- coexisting_MY[[4]]
    price_corr <- cor(source_prices_co, destination_prices_co)
    result <- result %>% rbind(c(price_corr,
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
