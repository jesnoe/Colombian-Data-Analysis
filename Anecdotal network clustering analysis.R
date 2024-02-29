# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)

armed_groups_combined <- list()
years <- (2008:2016)[-c(2,8)]
for (year in years) {
  armed_groups_combined_year <- read.csv(paste("Colombia Data/Armed Groups (Combined)/Colombia-Armed groups-Paramilitar", year ,"(combined).csv")) %>%
    as_tibble
  armed_groups_combined[[paste0("y", year)]] <- armed_groups_combined_year
}
armed_groups_combined$y2008

n_armed_groups <- armed_groups_combined$y2008 %>% select(id, n_armed_groups) %>% rename(X2008=n_armed_groups)
for (year in years[-1]) {
  n_armed_groups <- full_join(n_armed_groups, armed_groups_combined[[paste0("y", year)]] %>% select(id, n_armed_groups), by="id")
  names(n_armed_groups)[ncol(n_armed_groups)] <- paste0("X", year)
}

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

cultivation <- read.csv("Colombia Data/Colombia Coca Cultivation 1999-2016 renamed (Ha).csv") %>% as_tibble
cultivation <- cultivation %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
labs_HCl <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (COCAINE HYDROCHLORIDE).csv") %>% as_tibble
labs_HCl <- labs_HCl %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
labs_PPI <- read.csv("Colombia Data/Colombia-Laboratories-1997-2022 renamed (PRIMARY PRODUCTION INFRASTRUCTURE).csv") %>% as_tibble
labs_PPI <- labs_PPI %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))

eradication_aerial <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Aerial (Ha).xlsx")
eradication_manual <- read_xlsx("Colombia Data/Colombia-Coca Eradication-1994-2021 Manual (Ha).xlsx")
coca_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca leaves (kg).xlsx")
base_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Coca paste and base (kg).xlsx")
HCl_seizures <- read_xlsx("Colombia Data/Colombia-Seizures-1999-2022 Cocaine (kg).xlsx")
eradication_aerial <- eradication_aerial %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
eradication_manual <- eradication_manual %>% rename(id="CODMPIO") %>% mutate(id=as.numeric(id))
coca_seizures <- coca_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
base_seizures <- base_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))
HCl_seizures <- HCl_seizures %>% rename(id="CodMpio") %>% mutate(id=as.numeric(id))

price_2013_2015 <- read.csv("Colombia Data/Colombia Price Data 2013-2015 edited.csv") %>% as_tibble
price_2016_2021 <- read.csv("Colombia Data/Colombia Price Data 2016-2021 edited.csv") %>% as_tibble
# eradication_aerial <- left_join(municipios_id, eradication_aerial, by="id")
# eradication_manual <- left_join(municipios_id, eradication_manual, by="id")
# coca_seizures <- left_join(municipios_id, coca_seizures, by="id")
}

population <- read_xlsx("Colombia Data/Census population by municipios (2018).xlsx") %>% 
  filter(!is.na(municipio)) %>% 
  select(-type)
population <- population %>% 
  mutate(id=substr(municipio, 1, 5) %>% as.numeric) %>%
  select(id, population) %>% 
  right_join(municipios_capital %>% mutate(id=as.numeric(id)), by="id") %>% 
  select(id, id_depto, municipio, depto, population)
# write.csv(population, "Colombia Data/Census population by municipios (2018).csv")

price_2016_2021 %>% 
  group_by(id) %>% 
  summarise(n_seeds=sum(!is.na(seeds)),
            n_leaves=sum(!is.na(leaves))) %>% 
  arrange(desc(n_seeds), desc(n_leaves))

price <- rbind(price_2013_2015, price_2016_2021 %>% select(-seeds, -leaves))

price %>% select(month, year) %>% unique %>% nrow # 86 months (except for 2013 with "Jan to Apr", "May to Aug", "Sep to Dec")
n_price <- price %>% 
  group_by(id) %>% 
  summarise(paste_avg=sum(!is.na(paste_avg)),
            paste_wholesale=sum(!is.na(paste_wholesale)),
            paste_retail=sum(!is.na(paste_retail)),
            base_avg=sum(!is.na(base_avg)),
            base_wholesale=sum(!is.na(base_wholesale)),
            base_retail=sum(!is.na(base_retail)),
            hyd_avg=sum(!is.na(hyd_avg)),
            hyd_wholesale=sum(!is.na(hyd_wholesale)),
            hyd_retail=sum(!is.na(hyd_retail)))
n_price[n_price == 0] <- NA
n_armed_groups <- n_armed_groups %>% 
  mutate(n_armed_groups=n_armed_groups %>% select(-id) %>% apply(1, function(x) sum(!is.na(x)))) %>% 
  select(id, n_armed_groups) %>% 
  arrange(id)

get_n_obs <- function(dat, data_name) {
  col_name <- paste0("n_", data_name)
  dat[[col_name]] <- dat[,-(1:4)] %>% apply(1, function(x) sum(!is.na(x)))
  return(dat[, c(grep("id", names(dat)), length(names(dat)))] %>% arrange(id))
}

n_cultivation <- get_n_obs(cultivation, "cultivation")
n_labs_HCl <- get_n_obs(labs_HCl, "labs_HCl")
n_labs_PPI <- get_n_obs(labs_PPI, "labs_PPI")
n_eradication_aerial <- get_n_obs(eradication_aerial, "eradication_aerial")
n_eradication_manual <- get_n_obs(eradication_manual, "eradication_manual")
n_coca_seizures <- get_n_obs(coca_seizures, "coca_seizures")
n_base_seizures <- get_n_obs(base_seizures, "base_seizures")
n_HCl_seizures <- get_n_obs(HCl_seizures, "HCl_seizures")

n_obs_list_municipios <- municipios_capital %>% mutate(id=as.numeric(id)) %>% arrange(id) %>% 
  left_join(n_cultivation, by="id") %>% 
  left_join(n_labs_HCl, by="id") %>% 
  left_join(n_labs_PPI, by="id") %>% 
  left_join(n_eradication_aerial, by="id") %>% 
  left_join(n_eradication_manual, by="id") %>% 
  left_join(n_coca_seizures, by="id") %>%
  left_join(n_base_seizures, by="id") %>% 
  left_join(n_HCl_seizures, by="id") %>% 
  left_join(n_armed_groups, by="id") %>% 
  left_join(n_price, by="id") %>% 
  mutate(non_missing_cols=n_obs_list_municipios %>% apply(1, function(x) sum(!is.na(x[5:22])))) %>% 
  mutate(non_missing_prices=n_obs_list_municipios %>% apply(1, function(x) sum(!is.na(x[14:22]), na.rm=T))) %>% 
  as_tibble
# write.csv(n_obs_list_municipios, "Colombia Data/num of observations (municipios).csv", row.names=F)

read.csv("Colombia Data/N of observations (municipios).csv") %>% as_tibble

n_obs_list_depto <- n_obs_list_municipios %>%
  group_by(id_depto) %>% 
  summarise(depto=depto[1],
            n_cultivation=max(n_cultivation, na.rm=T),
            n_labs_HCl=max(n_labs_HCl, na.rm=T),
            n_labs_PPI=max(n_labs_PPI, na.rm=T),
            n_eradication_aerial=max(n_eradication_aerial, na.rm=T),
            n_eradication_manual=max(n_eradication_manual, na.rm=T),
            n_coca_seizures=max(n_coca_seizures, na.rm=T),
            n_base_seizures=max(n_base_seizures, na.rm=T),
            n_HCl_seizures=max(n_HCl_seizures, na.rm=T),
            n_armed_groups=max(n_armed_groups, na.rm=T),
            paste_avg=max(paste_avg, na.rm=T),
            paste_wholesale=max(paste_wholesale, na.rm=T),
            paste_retail=max(paste_retail, na.rm=T),
            base_avg=max(base_avg, na.rm=T),
            base_wholesale=max(base_wholesale, na.rm=T),
            base_retail=max(base_retail, na.rm=T),
            hyd_avg=max(hyd_avg, na.rm=T),
            hyd_wholesale=max(hyd_wholesale, na.rm=T),
            hyd_retail=max(hyd_retail, na.rm=T),
            non_missing_cols=max(non_missing_cols, na.rm=T),
            non_missing_prices=max(non_missing_prices, na.rm=T)
  )

# write.csv(n_obs_list_depto, "Colombia Data/n of observations (department).csv", row.names=F)

# convert anecdotal data cities into municipios
base_to_base  <- read.csv("Colombia Data/Anecdotal base to base with id.csv") %>% as_tibble
HCl_to_HCl  <- read.csv("Colombia Data/Anecdotal HCl to HCl with coordinates.csv") %>% as_tibble
anecdotal_annual  <- read.csv("Colombia Data/Anecdotal annual.csv") %>% as_tibble
anecdotal_annual %>% select(SOURCE.MUNICIPIO.CITY, SOURCE.DEPARTAMENTO) %>% unique
anecdotal_annual %>% select(DESTINATION..CITY., DESTINATION.DEPARTAMENTO) %>% unique

map <- municipios
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(!(id %in% c(88001, 88564))) # excludes islands in Caribbean
map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")

for (i in 1:nrow(HCl_to_HCl)) {
  source_long_i <- HCl_to_HCl$source_long[i]
  source_lat_i <- HCl_to_HCl$source_lat[i]
  destination_long_i <- HCl_to_HCl$destine_long[i]
  destination_lat_i <- HCl_to_HCl$destine_lat[i]
  # mun_index <- which(abs(source_long_i - map_df$long) < 0.005)
  # candidate_mun <- map_df[mun_index,]
  # candidate_mun_minmax <- candidate_mun %>%
  #   group_by(id) %>%
  #   summarise(min_lat = min(lat), max_lat=max(lat), municipio=municipio[1], depto=depto[1])
  search_index <- c(0,0)
  for (id_i in (unique(municipios_capital$id) %>% sort)) {
    municipio_id <- map_df %>% filter(id == id_i)
    
    if (point.in.polygon(source_long_i, source_lat_i, municipio_id$long, municipio_id$lat)) {
      source_id <- id_i
      search_index[1] <- 1
    }
    if (point.in.polygon(destination_long_i, destination_lat_i, municipio_id$long, municipio_id$lat)) {
      destination_id <- id_i
      search_index[2] <- 1
    }
    
    if (sum(search_index) == 2) break
  }
  # candidate_index <- which(source_lat_i >= candidate_mun_minmax$min_lat & source_lat_i <= candidate_mun_minmax$max_lat)
  # matched_municipio <- candidate_mun_minmax[candidate_index[which.max(candidate_mun_minmax$min_lat[candidate_index])],]
  HCl_to_HCl$source_id[i] <- source_id
  HCl_to_HCl$source_municipio[i] <- municipios_capital %>% filter(id == source_id) %>% pull(municipio)
  HCl_to_HCl$destination_id[i] <- destination_id
  HCl_to_HCl$destination_municipio[i] <- municipios_capital %>% filter(id == destination_id) %>% pull(municipio)
}

HCl_to_HCl[which(is.na(HCl_to_HCl$source_id)),] %>% as.data.frame

# map checking
base_to_base_source_id <- base_to_base %>% filter(!is.na(source_id)) %>% pull(source_id) %>% unique
base_to_base_destination_id <- base_to_base %>% filter(!is.na(destination_id)) %>% pull(destination_id) %>% unique

par(mfrow=c(1,3))
cultivation %>% filter(id %in% base_to_base_source_id) %>% select(X1999:X2016) %>% flatten %>% unlist %>% 
  hist(breaks=50, main="Cultivation (Anecdotal Sources)", xlab="kg", ylab="", xlim=c(0, 25000))
cultivation %>% filter(id %in% base_to_base_destination_id) %>% select(X1999:X2016) %>% flatten %>% unlist %>% 
  hist(breaks=10, main="Cultivation (Anecdotal Destinations)", xlab="kg", ylab="", xlim=c(0, 25000))
cultivation %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X1999:X2016) %>% flatten %>% unlist %>% 
  hist(breaks=50, main="Cultivation (Non-anecdotal)", xlab="kg", ylab="", xlim=c(0, 25000))

cultivation %>% filter(id %in% base_to_base_source_id) %>% select(-CODDEPTO, -id) %>% view
cultivation %>% filter(id %in% base_to_base_destination_id) %>% select(-CODDEPTO, -id) %>% view
cultivation %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(-CODDEPTO, -id) %>% view

cultivation %>% filter(id %in% base_to_base_source_id) %>% select(X1999:X2016) %>% apply(2, function(x) mean(x, na.rm=T))
cultivation %>% filter(id %in% base_to_base_destination_id) %>% select(X1999:X2016) %>% apply(2, function(x) mean(x, na.rm=T))
cultivation %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X1999:X2016) %>% apply(2, function(x) mean(x, na.rm=T))

labs_PPI %>% filter(id %in% base_to_base_source_id) %>% select(X1997:X2022) %>% flatten %>% unlist %>% 
  hist(breaks=50, main="# of PPI labs (Anecdotal Sources)", xlab="", ylab="", xlim=c(0, 900))
labs_PPI %>% filter(id %in% base_to_base_destination_id) %>% select(X1997:X2022) %>% flatten %>% unlist %>% 
  hist(breaks=5, main="# of PPI labs (Anecdotal Destinations)", xlab="", ylab="", xlim=c(0, 900))
labs_PPI %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X1997:X2022) %>% flatten %>% unlist %>% 
  hist(breaks=30, main="# of PPI labs (Non-anecdotal)", xlab="", ylab="", xlim=c(0, 900))

labs_PPI %>% filter(id %in% base_to_base_source_id) %>% select(-CODDEPTO, -id) %>% view
labs_PPI %>% filter(id %in% base_to_base_destination_id) %>% select(-CODDEPTO, -id) %>% view
labs_PPI %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(-CODDEPTO, -id) %>% view

labs_PPI %>% filter(id %in% base_to_base_source_id) %>% select(X1997:X2022) %>% apply(2, function(x) mean(x, na.rm=T))
labs_PPI %>% filter(id %in% base_to_base_destination_id) %>% select(X1997:X2022) %>% apply(2, function(x) mean(x, na.rm=T))
labs_PPI %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X1997:X2022) %>% apply(2, function(x) mean(x, na.rm=T))

labs_HCl %>% filter(id %in% base_to_base_source_id) %>% select(X1997:X2022) %>% flatten %>% unlist %>% 
  hist(breaks=5, main="# of HCl labs (Anecdotal Sources)", xlab="", ylab="", xlim=c(0, 300))
labs_HCl %>% filter(id %in% base_to_base_destination_id) %>% select(X1997:X2022) %>% flatten %>% unlist %>% 
  hist(breaks=1, main="# of HCl labs (Anecdotal Destinations)", xlab="", ylab="", xlim=c(0, 300))
labs_HCl %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X1997:X2022) %>% flatten %>% unlist %>% 
  hist(breaks=30, main="# of HCl labs (Non-anecdotal)", xlab="", ylab="", xlim=c(0, 300))

labs_HCl %>% filter(id %in% base_to_base_source_id) %>% select(-CODDEPTO, -id) %>% view
labs_HCl %>% filter(id %in% base_to_base_destination_id) %>% select(-CODDEPTO, -id) %>% view
labs_HCl %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(-CODDEPTO, -id) %>% view

labs_HCl %>% filter(id %in% base_to_base_source_id) %>% select(X1997:X2022) %>% apply(2, function(x) mean(x, na.rm=T))
labs_HCl %>% filter(id %in% base_to_base_destination_id) %>% select(X1997:X2022) %>% apply(2, function(x) mean(x, na.rm=T))
labs_HCl %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X1997:X2022) %>% apply(2, function(x) mean(x, na.rm=T))

n_armed_groups %>% filter(id %in% base_to_base_source_id) %>% select(X2008:X2016) %>% flatten %>% unlist %>% 
  hist(main="# of Armed Groups (Anecdotal Sources)", xlab="", ylab="")
n_armed_groups %>% filter(id %in% base_to_base_destination_id) %>% select(X2008:X2016) %>% flatten %>% unlist %>% 
  hist(main="# of Armed Groups (Anecdotal Destinations)", xlab="", ylab="")
n_armed_groups %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X2008:X2016) %>% flatten %>% unlist %>% 
  hist(main="# of Armed Groups (Non-anecdotal)", xlab="", ylab="")

n_armed_groups %>% filter(id %in% base_to_base_source_id) %>% select(X2008:X2016) %>% apply(2, function(x) mean(x, na.rm=T))
n_armed_groups %>% filter(id %in% base_to_base_destination_id) %>% select(X2008:X2016) %>% apply(2, function(x) mean(x, na.rm=T))
n_armed_groups %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(X2008:X2016) %>% apply(2, function(x) mean(x, na.rm=T))

base_seizures %>% filter(id %in% base_to_base_source_id) %>% select(`1999`:`2022`) %>% flatten %>% unlist %>% 
  hist(50, main="# of Base Paste Seizures (Anecdotal Sources)", xlab="", ylab="")
base_seizures %>% filter(id %in% base_to_base_destination_id) %>% select(`1999`:`2022`) %>% flatten %>% unlist %>% 
  hist(50, main="# of Base Paste Seizures (Anecdotal Destinations)", xlab="", ylab="")
base_seizures %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(`1999`:`2022`) %>% flatten %>% unlist %>% 
  hist(50, main="# of Base Paste Seizures (Non-anecdotal)", xlab="", ylab="")
par(mfrow=c(1,1))

base_seizures %>% filter(id %in% base_to_base_source_id) %>% select(-CodDepto, -id) %>% view
base_seizures %>% filter(id %in% base_to_base_destination_id) %>% select(-CodDepto, -id) %>% view
base_seizures %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(-CodDepto, -id) %>% view

base_seizures %>% filter(id %in% base_to_base_source_id) %>% select(`1999`:`2022`) %>% apply(2, function(x) mean(x, na.rm=T))
base_seizures %>% filter(id %in% base_to_base_destination_id) %>% select(`1999`:`2022`) %>% apply(2, function(x) mean(x, na.rm=T))
base_seizures %>% filter(!(id %in% c(base_to_base_source_id, base_to_base_destination_id))) %>% select(`1999`:`2022`) %>% apply(2, function(x) mean(x, na.rm=T))


price_2013_2015 %>% filter(id %in% base_to_base_source_id) %>% select(month:hyd_retail) %>% view
price_2013_2015 %>% filter(id %in% base_to_base_source_id) %>% pull(id) %>% unique
price_2013_2015 %>% filter(id %in% base_to_base_source_id) %>%
  group_by(id) %>% 
  summarise(n_base_wholesale=sum(!is.na(base_wholesale)),
            n_hyd_wholesale=sum(!is.na(hyd_wholesale)))

price_2016_2021 %>% filter(id %in% base_to_base_source_id) %>% select(month:hyd_retail) %>% view
price_2016_2021 %>% filter(id %in% base_to_base_source_id) %>% pull(id) %>% unique


price_2016_2021 %>% filter(id %in% base_to_base_source_id) %>%
  group_by(id) %>% 
  summarise(n_base_wholesale=sum(!is.na(base_wholesale)),
            n_hyd_wholesale=sum(!is.na(hyd_wholesale)))
