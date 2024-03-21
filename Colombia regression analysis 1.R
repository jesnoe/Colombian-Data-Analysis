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
  map <- municipios
  map_df <- suppressMessages(fortify(map)) %>% 
    mutate(id=as.numeric(id))
  map_df <- left_join(map_df, municipios_capital %>% select(id, municipio, depto) %>% unique, by="id")


base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual municipality only.csv") %>% as_tibble

population <- read.csv("Colombia Data/Census population by municipios (2018).csv") %>% as_tibble
population$log_population <- log(population$population)

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

  river_length_muni <- read.csv("Colombia Data/rivers.csv") %>% as_tibble
  road_length_muni <- read.csv("Colombia Data/roads without tertiary.csv") %>% as_tibble
  
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
}
{
data1 <- left_join(population, river_length_muni %>% select(-municipio, -depto), by="id")
data1$base_source <- ifelse(data1$id %in% unique(base_to_base$source_id), 1, 0) %>% as.factor
data1$base_destination <- ifelse(data1$id %in% unique(base_to_base$destination_id), 1, 0) %>% as.factor
data1$HCl_source <- ifelse(data1$id %in% unique(HCl_to_HCl$source_id), 1, 0) %>% as.factor
data1$HCl_destination <- ifelse(data1$id %in% unique(HCl_to_HCl$destination_id), 1, 0) %>% as.factor
data1$general_source <- ifelse(data1$id %in% unique(general$source_id), 1, 0) %>% as.factor
data1$general_destination <- ifelse(data1$id %in% unique(general$destination_id), 1, 0) %>% as.factor
model1_base_source <- glm(base_source~., family="binomial",  data=data1 %>% select(population:n_big_rivers, base_source)); summary(model1_base_source)

base_source_positive_index <- which(data1$base_source == 1)
base_source_unlabeled_index <- which(data1$base_source == 0)
base_destination_positive_index <- which(data1$base_destination == 1)
base_destination_unlabeled_index <- which(data1$base_destination == 0)
HCl_source_positive_index <- which(data1$HCl_source == 1)
HCl_source_unlabeled_index <- which(data1$HCl_source == 0)
HCl_destination_positive_index <- which(data1$HCl_destination == 1)
HCl_destination_unlabeled_index <- which(data1$HCl_destination == 0)
general_source_positive_index <- which(data1$general_source == 1)
general_source_unlabeled_index <- which(data1$general_source == 0)
general_destination_positive_index <- which(data1$general_destination == 1)
general_destination_unlabeled_index <- which(data1$general_destination == 0)
}
sample_positive_unlabeled <- function(positive, unlabeled) {
  positive_samples <- positive[sample(1:nrow(positive), 50),]
  unlabeled_samples <- unlabeled[sample(1:nrow(unlabeled), 50),]
  result <- rbind(positive_samples, unlabeled_samples)
  return(result)
}

shuffle <- function(positive, unlabeled) {
  result <- tibble(positive=sample(positive, 50),
                   unlabeled=sample(unlabeled, 50))
  return(result)
}

{
set.seed(100)
data1_base_source1_index <- shuffle(base_source_positive_index, base_source_unlabeled_index)
data1_base_source1 <- data1[c(data1_base_source1_index$positive, data1_base_source1_index$unlabeled), ]
data1_base_source2_index <- shuffle(base_source_positive_index, base_source_unlabeled_index)
data1_base_source2 <- data1[c(data1_base_source1_index$positive, data1_base_source1_index$unlabeled), ]
data1_base_source3_index <- shuffle(base_source_positive_index, base_source_unlabeled_index)
data1_base_source3 <- data1[c(data1_base_source1_index$positive, data1_base_source1_index$unlabeled), ]
data1_base_destination1_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index)
data1_base_destination1 <- data1[c(data1_base_destination1_index$positive, data1_base_destination1_index$unlabeled), ]
data1_base_destination2_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index)
data1_base_destination2 <- data1[c(data1_base_destination1_index$positive, data1_base_destination1_index$unlabeled), ]
data1_base_destination3_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index)
data1_base_destination3 <- data1[c(data1_base_destination1_index$positive, data1_base_destination1_index$unlabeled), ]

data1_HCl_source1_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index)
data1_HCl_source1 <- data1[c(data1_HCl_source1_index$positive, data1_HCl_source1_index$unlabeled), ]
data1_HCl_source2_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index)
data1_HCl_source2 <- data1[c(data1_HCl_source1_index$positive, data1_HCl_source1_index$unlabeled), ]
data1_HCl_source3_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index)
data1_HCl_source3 <- data1[c(data1_HCl_source1_index$positive, data1_HCl_source1_index$unlabeled), ]
data1_HCl_destination1_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index)
data1_HCl_destination1 <- data1[c(data1_HCl_destination1_index$positive, data1_HCl_destination1_index$unlabeled), ]
data1_HCl_destination2_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index)
data1_HCl_destination2 <- data1[c(data1_HCl_destination1_index$positive, data1_HCl_destination1_index$unlabeled), ]
data1_HCl_destination3_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index)
data1_HCl_destination3 <- data1[c(data1_HCl_destination1_index$positive, data1_HCl_destination1_index$unlabeled), ]

data1_general_destination1_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index)
data1_general_destination1 <- data1[c(data1_general_destination1_index$positive, data1_general_destination1_index$unlabeled), ]
data1_general_destination2_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index)
data1_general_destination2 <- data1[c(data1_general_destination1_index$positive, data1_general_destination1_index$unlabeled), ]
data1_general_destination3_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index)
data1_general_destination3 <- data1[c(data1_general_destination1_index$positive, data1_general_destination1_index$unlabeled), ]
}

## regression with data1 (population and rivers)
  # base source/destination
data1_base_source_glm <- glm(base_source~., family="binomial",  data=data1 %>% select(population:n_big_rivers, base_source))
summary(data1_base_source_glm)
data1_base_source1_glm <- glm(base_source~., family="binomial",  data=data1_base_source1 %>% select(population:n_big_rivers, base_source))
summary(data1_base_source1_glm)
data1_base_source2_glm <- glm(base_source~., family="binomial",  data=data1_base_source2 %>% select(population:n_big_rivers, base_source))
summary(data1_base_source2_glm)
data1_base_source3_glm <- glm(base_source~., family="binomial",  data=data1_base_source3 %>% select(population:n_big_rivers, base_source))
summary(data1_base_source3_glm)

data1_base_destination_glm <- glm(base_destination~., family="binomial",  data=data1 %>% select(population:n_big_rivers, base_destination))
summary(data1_base_destination_glm)
data1_base_destination1_glm <- glm(base_destination~., family="binomial",  data=data1_base_destination1 %>% select(population:n_big_rivers, base_destination))
summary(data1_base_destination1_glm)
data1_base_destination2_glm <- glm(base_destination~., family="binomial",  data=data1_base_destination2 %>% select(population:n_big_rivers, base_destination))
summary(data1_base_destination2_glm)
data1_base_destination3_glm <- glm(base_destination~., family="binomial",  data=data1_base_destination3 %>% select(population:n_big_rivers, base_destination))
summary(data1_base_destination3_glm)

F1_PU <- function(glm_model, test_data, positive) {
  n_data <- nrow(test_data)
  positive <- as.numeric(positive)
  n_positive <- sum(positive, na.rm=T)
  y_hat <- ifelse(predict(glm_model, test_data) > .5, 1, 0)
  recall <- sum(positive*y_hat, na.rm=T)/n_positive
  result <- recall^2/(sum(y_hat/n_data, na.rm=T))
  return(result)
}
test_data_source1 <- data1[c(base_source_positive_index[!(base_source_positive_index %in% data1_base_source1_index$positive)],
                     sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data1_base_source1_index$unlabeled)], 50)), ]
F1_PU(data1_base_source1_glm, test_data_source1, test_data_source1$base_source)
test_data_source2 <- data1[c(base_source_positive_index[!(base_source_positive_index %in% data1_base_source2_index$positive)],
                     sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data1_base_source2_index$unlabeled)], 50)), ]
F1_PU(data1_base_source2_glm, test_data_source2, test_data_source2$base_source)
test_data_source3 <- data1[c(base_source_positive_index[!(base_source_positive_index %in% data1_base_source3_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data1_base_source3_index$unlabeled)], 50)), ]
F1_PU(data1_base_source3_glm, test_data_source3, test_data_source3$base_source)

test_data_destination1 <- data1[c(base_destination_positive_index[!(base_destination_positive_index %in% data1_base_destination1_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data1_base_destination1_index$unlabeled)], 50)), ]
F1_PU(data1_base_destination1_glm, test_data_destination1, test_data_destination1$base_destination)
test_data_destination2 <- data1[c(base_destination_positive_index[!(base_destination_positive_index %in% data1_base_destination2_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data1_base_destination2_index$unlabeled)], 50)), ]
F1_PU(data1_base_destination2_glm, test_data_destination2, test_data_destination2$base_destination)
test_data_destination3 <- data1[c(base_destination_positive_index[!(base_destination_positive_index %in% data1_base_destination3_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data1_base_destination3_index$unlabeled)], 50)), ]
F1_PU(data1_base_destination3_glm, test_data_destination3, test_data_destination3$base_destination)

  # HCl source/destination
data1_HCl_source_glm <- glm(HCl_source~., family="binomial",  data=data1 %>% select(population:n_big_rivers, HCl_source))
summary(data1_HCl_source_glm)
data1_HCl_source1_glm <- glm(HCl_source~., family="binomial",  data=data1_HCl_source1 %>% select(population:n_big_rivers, HCl_source))
summary(data1_HCl_source1_glm)
data1_HCl_source2_glm <- glm(HCl_source~., family="binomial",  data=data1_HCl_source2 %>% select(population:n_big_rivers, HCl_source))
summary(data1_HCl_source2_glm)
data1_HCl_source3_glm <- glm(HCl_source~., family="binomial",  data=data1_HCl_source3 %>% select(population:n_big_rivers, HCl_source))
summary(data1_HCl_source3_glm)

data1_HCl_destination_glm <- glm(HCl_destination~., family="binomial",  data=data1 %>% select(population:n_big_rivers, HCl_destination))
summary(data1_HCl_destination_glm)
data1_HCl_destination1_glm <- glm(HCl_destination~., family="binomial",  data=data1_HCl_destination1 %>% select(population:n_big_rivers, HCl_destination))
summary(data1_HCl_destination1_glm)
data1_HCl_destination2_glm <- glm(HCl_destination~., family="binomial",  data=data1_HCl_destination2 %>% select(population:n_big_rivers, HCl_destination))
summary(data1_HCl_destination2_glm)
data1_HCl_destination3_glm <- glm(HCl_destination~., family="binomial",  data=data1_HCl_destination3 %>% select(population:n_big_rivers, HCl_destination))
summary(data1_HCl_destination3_glm)

test_data_source1 <- data1[c(HCl_source_positive_index[!(HCl_source_positive_index %in% data1_HCl_source1_index$positive)],
                             sample(HCl_source_unlabeled_index[!(HCl_source_unlabeled_index %in% data1_HCl_source1_index$unlabeled)], 50)), ]
F1_PU(data1_HCl_source1_glm, test_data_source1, test_data_source1$HCl_source)
test_data_source2 <- data1[c(HCl_source_positive_index[!(HCl_source_positive_index %in% data1_HCl_source2_index$positive)],
                             sample(HCl_source_unlabeled_index[!(HCl_source_unlabeled_index %in% data1_HCl_source2_index$unlabeled)], 50)), ]
F1_PU(data1_HCl_source2_glm, test_data_source2, test_data_source2$HCl_source)
test_data_source3 <- data1[c(HCl_source_positive_index[!(HCl_source_positive_index %in% data1_HCl_source3_index$positive)],
                             sample(HCl_source_unlabeled_index[!(HCl_source_unlabeled_index %in% data1_HCl_source3_index$unlabeled)], 50)), ]
F1_PU(data1_HCl_source3_glm, test_data_source3, test_data_source3$HCl_source)

test_data_destination1 <- data1[c(HCl_destination_positive_index[!(HCl_destination_positive_index %in% data1_HCl_destination1_index$positive)],
                                  sample(HCl_destination_unlabeled_index[!(HCl_destination_unlabeled_index %in% data1_HCl_destination1_index$unlabeled)], 50)), ]
F1_PU(data1_HCl_destination1_glm, test_data_destination1, test_data_destination1$HCl_destination)
test_data_destination2 <- data1[c(HCl_destination_positive_index[!(HCl_destination_positive_index %in% data1_HCl_destination2_index$positive)],
                                  sample(HCl_destination_unlabeled_index[!(HCl_destination_unlabeled_index %in% data1_HCl_destination2_index$unlabeled)], 50)), ]
F1_PU(data1_HCl_destination2_glm, test_data_destination2, test_data_destination2$HCl_destination)
test_data_destination3 <- data1[c(HCl_destination_positive_index[!(HCl_destination_positive_index %in% data1_HCl_destination3_index$positive)],
                                  sample(HCl_destination_unlabeled_index[!(HCl_destination_unlabeled_index %in% data1_HCl_destination3_index$unlabeled)], 50)), ]
F1_PU(data1_HCl_destination3_glm, test_data_destination3, test_data_destination3$HCl_destination)

# general source/destination
data1_general_source_glm <- glm(general_source~., family="binomial",  data=data1 %>% select(population:n_big_rivers, general_source))
summary(data1_general_source_glm)

data1_general_destination_glm <- glm(general_destination~., family="binomial",  data=data1 %>% select(population:n_big_rivers, general_destination))
summary(data1_general_destination_glm)
data1_general_destination1_glm <- glm(general_destination~., family="binomial",  data=data1_general_destination1 %>% select(population:n_big_rivers, general_destination))
summary(data1_general_destination1_glm)
data1_general_destination2_glm <- glm(general_destination~., family="binomial",  data=data1_general_destination2 %>% select(population:n_big_rivers, general_destination))
summary(data1_general_destination2_glm)
data1_general_destination3_glm <- glm(general_destination~., family="binomial",  data=data1_general_destination3 %>% select(population:n_big_rivers, general_destination))
summary(data1_general_destination3_glm)

test_data_destination1 <- data1[c(general_destination_positive_index[!(general_destination_positive_index %in% data1_general_destination1_index$positive)],
                                  sample(general_destination_unlabeled_index[!(general_destination_unlabeled_index %in% data1_general_destination1_index$unlabeled)], 50)), ]
F1_PU(data1_general_destination1_glm, test_data_destination1, test_data_destination1$general_destination)
test_data_destination2 <- data1[c(general_destination_positive_index[!(general_destination_positive_index %in% data1_general_destination2_index$positive)],
                                  sample(general_destination_unlabeled_index[!(general_destination_unlabeled_index %in% data1_general_destination2_index$unlabeled)], 50)), ]
F1_PU(data1_general_destination2_glm, test_data_destination2, test_data_destination2$general_destination)
test_data_destination3 <- data1[c(general_destination_positive_index[!(general_destination_positive_index %in% data1_general_destination3_index$positive)],
                                  sample(general_destination_unlabeled_index[!(general_destination_unlabeled_index %in% data1_general_destination3_index$unlabeled)], 50)), ]
F1_PU(data1_general_destination3_glm, test_data_destination3, test_data_destination3$general_destination)


## regression with data2 (data1 + cultivation, 1999~2016)
labs_PPI$lab_PPI_exist <- apply(labs_PPI, 1, function(x) return(sum(!is.na(x[5:30]))>0))
labs_HCl$lab_HCl_exist <- apply(labs_HCl, 1, function(x) return(sum(!is.na(x[5:30]))>0))
coca_seizures$coca_seizures <- apply(coca_seizures, 1, function(x) return(sum(!is.na(x[5:28]))>0))
data2 <- left_join(data1, labs_PPI %>% select(id, lab_PPI_exist), by="id") %>% 
  left_join(labs_HCl %>% select(id, lab_HCl_exist), by="id") %>% 
  left_join(coca_seizures %>% select(id, coca_seizures), by="id")
data2$lab_PPI_exist <- ifelse(is.na(data2$lab_PPI_exist), F, T)
data2$lab_HCl_exist <- ifelse(is.na(data2$lab_HCl_exist), F, T)
data2$coca_seizures <- ifelse(is.na(data2$coca_seizures), F, T)

{
set.seed(5478)
data2_base_source1_index <- shuffle(base_source_positive_index, base_source_unlabeled_index)
data2_base_source1 <- data2[c(data2_base_source1_index$positive, data2_base_source1_index$unlabeled), ]
data2_base_source2_index <- shuffle(base_source_positive_index, base_source_unlabeled_index)
data2_base_source2 <- data2[c(data2_base_source1_index$positive, data2_base_source1_index$unlabeled), ]
data2_base_source3_index <- shuffle(base_source_positive_index, base_source_unlabeled_index)
data2_base_source3 <- data2[c(data2_base_source1_index$positive, data2_base_source1_index$unlabeled), ]
data2_base_destination1_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index)
data2_base_destination1 <- data2[c(data2_base_destination1_index$positive, data2_base_destination1_index$unlabeled), ]
data2_base_destination2_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index)
data2_base_destination2 <- data2[c(data2_base_destination1_index$positive, data2_base_destination1_index$unlabeled), ]
data2_base_destination3_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index)
data2_base_destination3 <- data2[c(data2_base_destination1_index$positive, data2_base_destination1_index$unlabeled), ]

data2_HCl_source1_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index)
data2_HCl_source1 <- data2[c(data2_HCl_source1_index$positive, data2_HCl_source1_index$unlabeled), ]
data2_HCl_source2_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index)
data2_HCl_source2 <- data2[c(data2_HCl_source1_index$positive, data2_HCl_source1_index$unlabeled), ]
data2_HCl_source3_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index)
data2_HCl_source3 <- data2[c(data2_HCl_source1_index$positive, data2_HCl_source1_index$unlabeled), ]
data2_HCl_destination1_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index)
data2_HCl_destination1 <- data2[c(data2_HCl_destination1_index$positive, data2_HCl_destination1_index$unlabeled), ]
data2_HCl_destination2_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index)
data2_HCl_destination2 <- data2[c(data2_HCl_destination1_index$positive, data2_HCl_destination1_index$unlabeled), ]
data2_HCl_destination3_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index)
data2_HCl_destination3 <- data2[c(data2_HCl_destination1_index$positive, data2_HCl_destination1_index$unlabeled), ]

data2_general_destination1_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index)
data2_general_destination1 <- data2[c(data2_general_destination1_index$positive, data2_general_destination1_index$unlabeled), ]
data2_general_destination2_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index)
data2_general_destination2 <- data2[c(data2_general_destination1_index$positive, data2_general_destination1_index$unlabeled), ]
data2_general_destination3_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index)
data2_general_destination3 <- data2[c(data2_general_destination1_index$positive, data2_general_destination1_index$unlabeled), ]
}


  # base source
data2_base_source_glm <- glm(base_source~., family="binomial",  data=data2 %>%
                               select(population:n_big_rivers, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source_glm)
data2_base_source1_glm <- glm(base_source~., family="binomial",  data=data2_base_source1 %>% 
                                select(population:n_big_rivers, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source1_glm)
data2_base_source2_glm <- glm(base_source~., family="binomial",  data=data2_base_source2 %>%
                                select(population:n_big_rivers, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source2_glm)
data2_base_source3_glm <- glm(base_source~., family="binomial",  data=data2_base_source3 %>% 
                                select(population:n_big_rivers, base_source, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_source3_glm)

data2_base_destination_glm <- glm(base_destination~., family="binomial",  data=data2 %>%
                                    select(population:n_big_rivers, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination_glm)
data2_base_destination1_glm <- glm(base_destination~., family="binomial",  data=data2_base_destination1 %>%
                                     select(population:n_big_rivers, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination1_glm)
data2_base_destination2_glm <- glm(base_destination~., family="binomial",  data=data2_base_destination2 %>% 
                                     select(population:n_big_rivers, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination2_glm)
data2_base_destination3_glm <- glm(base_destination~., family="binomial",  data=data2_base_destination3 %>% 
                                     select(population:n_big_rivers, base_destination, lab_PPI_exist, lab_HCl_exist, coca_seizures))
summary(data2_base_destination3_glm)

test_data_source1 <- data2[c(base_source_positive_index[!(base_source_positive_index %in% data2_base_source1_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data2_base_source1_index$unlabeled)], 50)), ]
F1_PU(data2_base_source1_glm, test_data_source1, test_data_source1$base_source)
test_data_source2 <- data2[c(base_source_positive_index[!(base_source_positive_index %in% data2_base_source2_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data2_base_source2_index$unlabeled)], 50)), ]
F1_PU(data2_base_source2_glm, test_data_source2, test_data_source2$base_source)
test_data_source3 <- data2[c(base_source_positive_index[!(base_source_positive_index %in% data2_base_source3_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data2_base_source3_index$unlabeled)], 50)), ]
F1_PU(data2_base_source3_glm, test_data_source3, test_data_source3$base_source)

test_data_destination1 <- data2[c(base_destination_positive_index[!(base_destination_positive_index %in% data2_base_destination1_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data2_base_destination1_index$unlabeled)], 50)), ]
F1_PU(data2_base_destination1_glm, test_data_destination1, test_data_destination1$base_destination)
test_data_destination2 <- data2[c(base_destination_positive_index[!(base_destination_positive_index %in% data2_base_destination2_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data2_base_destination2_index$unlabeled)], 50)), ]
F1_PU(data2_base_destination2_glm, test_data_destination2, test_data_destination2$base_destination)
test_data_destination3 <- data2[c(base_destination_positive_index[!(base_destination_positive_index %in% data2_base_destination3_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data2_base_destination3_index$unlabeled)], 50)), ]
F1_PU(data2_base_destination3_glm, test_data_destination3, test_data_destination3$base_destination)

  # HCl source
data2_HCl_source_glm <- glm(HCl_source~., family="binomial",  data=data2 %>% select(population:n_big_rivers, HCl_source, X1999:X2016))
summary(data2_HCl_source_glm)
data2_HCl_source1_glm <- glm(HCl_source~., family="binomial",  data=data2_HCl_source1 %>% select(population:n_big_rivers, HCl_source, X1999:X2016))
summary(data2_HCl_source1_glm)
data2_HCl_source2_glm <- glm(HCl_source~., family="binomial",  data=data2_HCl_source2 %>% select(population:n_big_rivers, HCl_source, X1999:X2016))
summary(data2_HCl_source2_glm)
data2_HCl_source3_glm <- glm(HCl_source~., family="binomial",  data=data2_HCl_source3 %>% select(population:n_big_rivers, HCl_source, X1999:X2016))
summary(data2_HCl_source3_glm)

data2_HCl_destination_glm <- glm(HCl_destination~., family="binomial",  data=data2 %>% select(population:n_big_rivers, HCl_destination, X1999:X2016))
summary(data2_HCl_destination_glm)
data2_HCl_destination1_glm <- glm(HCl_destination~., family="binomial",  data=data2_HCl_destination1 %>% select(population:n_big_rivers, HCl_destination, X1999:X2016))
summary(data2_HCl_destination1_glm)
data2_HCl_destination2_glm <- glm(HCl_destination~., family="binomial",  data=data2_HCl_destination2 %>% select(population:n_big_rivers, HCl_destination, X1999:X2016))
summary(data2_HCl_destination2_glm)
data2_HCl_destination3_glm <- glm(HCl_destination~., family="binomial",  data=data2_HCl_destination3 %>% select(population:n_big_rivers, HCl_destination, X1999:X2016))
summary(data2_HCl_destination3_glm)

  # general source
data2_general_source_glm <- glm(general_source~., family="binomial",  data=data2 %>% select(population:n_big_rivers, general_source, X1999:X2016))
summary(data2_general_source_glm)
data2_general_source1_glm <- glm(general_source~., family="binomial",  data=data2_general_source1 %>% select(population:n_big_rivers, general_source, X1999:X2016))
summary(data2_general_source1_glm)
data2_general_source2_glm <- glm(general_source~., family="binomial",  data=data2_general_source2 %>% select(population:n_big_rivers, general_source, X1999:X2016))
summary(data2_general_source2_glm)
data2_general_source3_glm <- glm(general_source~., family="binomial",  data=data2_general_source3 %>% select(population:n_big_rivers, general_source, X1999:X2016))
summary(data2_general_source3_glm)

data2_general_destination_glm <- glm(general_destination~., family="binomial",  data=data2 %>% select(population:n_big_rivers, general_destination, X1999:X2016))
summary(data2_general_destination_glm)
data2_general_destination1_glm <- glm(general_destination~., family="binomial",  data=data2_general_destination1 %>% select(population:n_big_rivers, general_destination, X1999:X2016))
summary(data2_general_destination1_glm)
data2_general_destination2_glm <- glm(general_destination~., family="binomial",  data=data2_general_destination2 %>% select(population:n_big_rivers, general_destination, X1999:X2016))
summary(data2_general_destination2_glm)
data2_general_destination3_glm <- glm(general_destination~., family="binomial",  data=data2_general_destination3 %>% select(population:n_big_rivers, general_destination, X1999:X2016))
summary(data2_general_destination3_glm)