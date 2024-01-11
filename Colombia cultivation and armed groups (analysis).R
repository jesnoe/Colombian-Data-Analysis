# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

armed_groups_combined <- list()
years <- (2008:2016)[-c(2,8)]
for (year in years) {
  armed_groups_combined[[paste0("y", year)]] <- read.csv(paste("Colombia Data/Armed Groups (Combined)/Colombia-Armed groups-Paramilitar", year ,"(combined).csv")) %>%
    as_tibble
}
armed_groups_combined$y2008
