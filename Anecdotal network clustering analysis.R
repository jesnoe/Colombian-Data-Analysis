# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(colmaps)
library(sf)

base_to_base  <- read.csv("Colombia Data/Anecdotal base to base with coordinates.csv") %>% as_tibble
HCl_to_HCl  <- read.csv("Colombia Data/Anecdotal HCl to HCl with coordinates.csv") %>% as_tibble

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

# eradication_aerial <- left_join(municipios_id, eradication_aerial, by="id")
# eradication_manual <- left_join(municipios_id, eradication_manual, by="id")
# coca_seizures <- left_join(municipios_id, coca_seizures, by="id")
}

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
