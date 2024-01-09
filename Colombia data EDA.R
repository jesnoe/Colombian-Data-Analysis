# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(fpp2)
library(readxl)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)

## check misspelling in regional names and correct
cultivation <- read_xlsx("Colombia Data/Colombia Coca Cultivation 1999-2016 (Ha).xlsx")
eradication_aerial <- read_xlsx("Colombia Data/Colombia-Coca Erradication-1994-2021 Aerial (Ha).xlsx")
eradication_manual <- read_xlsx("Colombia Data/Colombia-Coca Erradication-1994-2021 Manual (Ha).xlsx")
price <- read_xlsx("Colombia Data/Colombia Price Data 2016-2021 for R.xlsx")

cultivation$n_obs <- cultivation %>% apply(1, function(x) sum(!is.na(x[5:22])))
eradication_aerial$n_obs <- eradication_aerial %>% apply(1, function(x) sum(!is.na(x[5:26])))
eradication_manual$n_obs <- eradication_manual %>% apply(1, function(x) sum(!is.na(x[5:29])))

2016-1999+1
nrow(cultivation)
cultivation$n_obs %>% hist(breaks=1:18, xlim=c(0,20), probability=T)

2021-1994+1
eradication_aerial$n_obs %>% hist(breaks=1:28, xlim=c(0,30), probability=T)
eradication_manual$n_obs %>% hist(breaks=1:28, xlim=c(0,30), probability=T)

cultivation %>%
  arrange(desc(n_obs), DEPARTAMENTO, MUNICIPIO) %>% 
  select(DEPARTAMENTO, MUNICIPIO, n_obs)
eradication_aerial %>%
  arrange(Departamento, MUNICIPIO) %>% 
  select(Departamento, MUNICIPIO, n_obs) %>% 
  as.data.frame
eradication_manual %>%
  arrange(desc(n_obs), Departamento, MUNICIPIO) %>% 
  select(Departamento, MUNICIPIO, n_obs)

cultivation %>% filter(DEPARTAMENTO == "ANTIOQUIA" & MUNICIPIO %in% (eradication_aerial %>% filter(Departamento == "ANTIOQUIA") %>% pull(MUNICIPIO) %>% unique))

cultivation %>% filter(DEPARTAMENTO == "ANTIOQUIA" & MUNICIPIO == "ANORÍ") -> cultivation_ANORÍ
eradication_aerial %>% filter(Departamento == "ANTIOQUIA" & MUNICIPIO == "ANORÍ") -> eradication_aerial_ANORÍ
cultivation_ANORÍ_ts <- ts(t(cultivation_ANORÍ[9:21]), start=2003)
eradication_aerial_ANORÍ_ts <- ts(t(eradication_aerial_ANORÍ[14:26]), start=2003)
cor(cultivation_ANORÍ_ts, eradication_aerial_ANORÍ_ts)

cor(t(cultivation[1:4, 5:21]), t(eradication_aerial[1:4, 10:26]), na)

price$month <- match(price$month, month.name)
price <- price %>% arrange(department, city, year, month)
price[, 2:4] %>% unique
price %>% apply(2, function(x) sum(is.na(x)))
price[,5:16] <- price[,5:16] %>% apply(2, function(x) ifelse(is.na(x), 0, x))

price %>%
  group_by(month, year, city, department) %>% 
  summarise(region = region[1],
            seeds = sum(seeds),
            leaves = sum(leaves),
            paste_wholesale = sum(paste_wholesale),
            paste_retail = sum(paste_retail),
            base_wholesale = sum(base_wholesale),
            base_retail = sum(base_retail),
            hyd_wholesale = sum(hyd_wholesale),
            hyd_retail = sum(hyd_retail)) -> price
