# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)
library(sp)

NA_SA <- st_read("Colombia Data/NA_SA_NetwordData.gdb")
NA_SA
st_coordinates(NA_SA$Shape[1])
NA_SA[,-ncol(NA_SA)] %>% summary
