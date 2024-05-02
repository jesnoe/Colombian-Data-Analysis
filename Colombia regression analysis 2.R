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
library(caret)
library(randomForest)

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
municipios_capital <- municipios_capital %>% filter(!(id %in% c(88001, 88564))) %>% as_tibble
map <- municipios
map_df <- suppressMessages(fortify(map)) %>% 
  mutate(id=as.numeric(id))
map_df <- left_join(map_df, municipios_capital %>% unique, by="id")

base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual with municipality.csv") %>% as_tibble
labs_PPI_reg_data <- read.csv("Colombia Data/labs_PPI_reg_data.csv") %>% as_tibble
labs_HCl_reg_data <- read.csv("Colombia Data/labs_HCl_reg_data.csv") %>% as_tibble
}

ex_year <- 2016
anecdotal_year <- anecdotal_annual %>%
  filter(YEAR == ex_year)
labs_PPI_2steps_year <- labs_PPI_reg_data %>% 
  filter(year == ex_year) %>% 
  select(-MUNICIPIO, -DEPARTAMENTO, -paste_avg, -hyd_avg, -paste_price_distance, -hyd_price_distance,
         -n_armed_groups, -n_rivers, -n_big_rivers, -n_roads, -HCl_seizures) %>% 
  mutate(n_labs=ifelse(is.na(n_labs), 0, n_labs),
         coca_area=ifelse(is.na(coca_area), 0, coca_area),
         erad_aerial=ifelse(is.na(erad_aerial), 0, erad_aerial),
         erad_manual=ifelse(is.na(erad_manual), 0, erad_manual),
         coca_seizures=ifelse(is.na(coca_seizures), 0, coca_seizures),
         base_seizures=ifelse(is.na(base_seizures), 0, base_seizures)) %>% 
  left_join(labs_HCl_reg_data %>%
              filter(year == ex_year) %>%
              mutate(n_HCl_labs=n_labs) %>%
              select(id, n_HCl_labs), by="id") %>% 
  mutate(n_HCl_labs=ifelse(is.na(n_HCl_labs), 0, n_HCl_labs)) %>% 
  left_join(armed_groups[[paste0("y", ex_year)]] %>% select(-(municipio:n_armed_groups)), by="id")
labs_PPI_2steps_year[is.na(labs_PPI_2steps_year)] <- 0

anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id) %>% unique %>% length # 64
sum(!(anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id) %>% unique %in% labs_PPI_2steps_year$id)) # 14 municipals in anecdotal data lack enough attributes (excluded)


{
data_year <- labs_PPI_2steps_year %>% left_join(population %>% select(id, population), by="id")
  
names(data_year) <- gsub(" ", "_", names(data_year)) %>% stri_trans_general("Latin-ASCII") 
names(data_year)[15:37] <- c("Los_Rastrojos", "Clan_del_Golfo", "Las_Aguilas_Negras", "Los_Paisas",
                             "Ejercito_Revolucionario_Popular_Antisubversivo_de_Colombia", "Los_Pachenga_O_AUTODEFENSAS_CONQUISTADORAS_DE_LA_SIERRA_NEVADA",
                             "Los_Caparros_FRENTE_VIRGILIO_PERALTA_ARENAS", "Los_Pachelly", "Los_Puntilleros", "La_Constru", "Los_Contadores",
                             "La_Oficina_de_Envigado_DEL_VALLE_DE_ABURRA_U_OVA", "Los_Pelusos", "Libertadores_del_Nordeste_NUEVO_RENACER_O_GUEROS", "La_Cordillera",
                             "La_Empresa", "La_Local", "Los_Caquetenos", "Los_Costenos", "Nuevo_Orden", "HEROES_DEL_CENTRAL_BOLIVAR_BAJO_CAUCA",
                             "Grupos_no_identificados", "Autodefensas_Unidas_de_Colombia")

data_year$base_source_all <- ifelse(data_year$id %in% (base_to_base %>% pull(source_id) %>% unique), 1, 0) %>% as.factor
data_year$base_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$base_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "BASE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$HCl_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$HCl_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "COCAINE") %>% pull(destination_id)), 1, 0) %>% as.factor
data_year$general_source <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(source_id)), 1, 0) %>% as.factor
data_year$general_destination <- ifelse(data_year$id %in% (anecdotal_year %>% filter(PROCESS == "GENERAL") %>% pull(destination_id)), 1, 0) %>% as.factor

base_source_positive_index <- which(data_year$base_source == 1)
base_source_unlabeled_index <- which(data_year$base_source == 0)
base_destination_positive_index <- which(data_year$base_destination == 1)
base_destination_unlabeled_index <- which(data_year$base_destination == 0)
HCl_source_positive_index <- which(data_year$HCl_source == 1)
HCl_source_unlabeled_index <- which(data_year$HCl_source == 0)
HCl_destination_positive_index <- which(data_year$HCl_destination == 1)
HCl_destination_unlabeled_index <- which(data_year$HCl_destination == 0)
general_source_positive_index <- which(data_year$general_source == 1)
general_source_unlabeled_index <- which(data_year$general_source == 0)
general_destination_positive_index <- which(data_year$general_destination == 1)
general_destination_unlabeled_index <- which(data_year$general_destination == 0)
}
sample_positive_unlabeled <- function(positive, unlabeled) {
  positive_samples <- positive[sample(1:nrow(positive), 50),]
  unlabeled_samples <- unlabeled[sample(1:nrow(unlabeled), 50),]
  result <- rbind(positive_samples, unlabeled_samples)
  return(result)
}

shuffle <- function(positive, unlabeled, N) { # N: number of positive and unlabeled samples
  result <- tibble(positive=sample(positive, N),
                   unlabeled=sample(unlabeled, N))
  return(result)
}

{
set.seed(100)
data_year_base_source1_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
data_year_base_source1 <- data_year[c(data_year_base_source1_index$positive, data_year_base_source1_index$unlabeled), ]
data_year_base_source2_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
data_year_base_source2 <- data_year[c(data_year_base_source1_index$positive, data_year_base_source1_index$unlabeled), ]
data_year_base_source3_index <- shuffle(base_source_positive_index, base_source_unlabeled_index, N=40)
data_year_base_source3 <- data_year[c(data_year_base_source1_index$positive, data_year_base_source1_index$unlabeled), ]
data_year_base_destination1_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
data_year_base_destination1 <- data_year[c(data_year_base_destination1_index$positive, data_year_base_destination1_index$unlabeled), ]
data_year_base_destination2_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
data_year_base_destination2 <- data_year[c(data_year_base_destination1_index$positive, data_year_base_destination1_index$unlabeled), ]
data_year_base_destination3_index <- shuffle(base_destination_positive_index, base_destination_unlabeled_index, N=40)
data_year_base_destination3 <- data_year[c(data_year_base_destination1_index$positive, data_year_base_destination1_index$unlabeled), ]

data_year_HCl_source1_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
data_year_HCl_source1 <- data_year[c(data_year_HCl_source1_index$positive, data_year_HCl_source1_index$unlabeled), ]
data_year_HCl_source2_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
data_year_HCl_source2 <- data_year[c(data_year_HCl_source1_index$positive, data_year_HCl_source1_index$unlabeled), ]
data_year_HCl_source3_index <- shuffle(HCl_source_positive_index, HCl_source_unlabeled_index, N=40)
data_year_HCl_source3 <- data_year[c(data_year_HCl_source1_index$positive, data_year_HCl_source1_index$unlabeled), ]
data_year_HCl_destination1_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
data_year_HCl_destination1 <- data_year[c(data_year_HCl_destination1_index$positive, data_year_HCl_destination1_index$unlabeled), ]
data_year_HCl_destination2_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
data_year_HCl_destination2 <- data_year[c(data_year_HCl_destination1_index$positive, data_year_HCl_destination1_index$unlabeled), ]
data_year_HCl_destination3_index <- shuffle(HCl_destination_positive_index, HCl_destination_unlabeled_index, N=40)
data_year_HCl_destination3 <- data_year[c(data_year_HCl_destination1_index$positive, data_year_HCl_destination1_index$unlabeled), ]

data_year_general_destination1_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
data_year_general_destination1 <- data_year[c(data_year_general_destination1_index$positive, data_year_general_destination1_index$unlabeled), ]
data_year_general_destination2_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
data_year_general_destination2 <- data_year[c(data_year_general_destination1_index$positive, data_year_general_destination1_index$unlabeled), ]
data_year_general_destination3_index <- shuffle(general_destination_positive_index, general_destination_unlabeled_index, N=40)
data_year_general_destination3 <- data_year[c(data_year_general_destination1_index$positive, data_year_general_destination1_index$unlabeled), ]
}

## regression with data_year (population and rivers)
  # base source/destination
base_source_glm <- glm(base_source~., family="binomial",
                       data=data_year %>%
                         # mutate(n_labs=ifelse(n_labs > 0, 1, 0)) %>% 
                         select(n_labs:population, base_source))
summary(base_source_glm)

base_source_glm2 <- glm(base_source~., family="binomial",
                        data=data_year %>%
                          select(n_labs:base_seizures, -erad_aerial, coca_group, population, base_source))
summary(base_source_glm2)

base_source_glm_reduced <- glm(base_source~., family="binomial",
                        data=data_year %>%
                          mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                          select(n_labs:coca_seizures, -base_price_distance, -erad_aerial, -erad_manual, coca_group, population, base_source))
summary(base_source_glm_reduced)

base_source_year <- data_year %>%
  select(id, n_labs:population, base_source) %>% 
  mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
  mutate(posterior_glm=base_source_glm$fitted.values) %>% 
  mutate(posterior_glm2=base_source_glm2$fitted.values) %>% 
  mutate(posterior_glm_r=base_source_glm_reduced$fitted.values)

threshold <- .1
base_source_year %>% filter(posterior_glm < threshold) %>% select(base_source, posterior_glm) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_glm >= threshold) %>% select(base_source, posterior_glm) %>% pull(base_source) %>% table

base_source_year %>% filter(posterior_glm2 < threshold) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_glm2 >= threshold) %>% pull(base_source) %>% table # lower true 1

base_source_year %>% filter(posterior_glm_r < threshold) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_glm_r >= threshold) %>% pull(base_source) %>% table # lower true 1

base_source_year %>% 
  filter(posterior_glm < threshold & base_source == 1) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm2)

base_source_year %>% 
  filter(posterior_glm2 < threshold & base_source == 1) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm2)

base_source_year %>% 
  filter(posterior_glm > threshold) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm)

base_source_year %>% 
  filter(posterior_glm2 > threshold) %>%
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm2)

base_source_year %>% filter(posterior_glm < threshold & base_source == 1) %>% select(base_source, posterior_glm) %>% arrange(posterior_glm)

set.seed(291038)
base_source_rf <- randomForest(base_source~.,
                               data=data_year %>%
                                 select(n_labs:population, base_source),
                               type="classification", ntree=50, mtry=20)

base_source_rf2 <- randomForest(base_source~.,
                                data=data_year %>%
                                  mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                                  select(n_labs:base_seizures, coca_group, population, base_source),
                                type="classification", ntree=50, mtry=10)
base_source_year$posterior_rf <- base_source_rf$votes[,2]
base_source_year$posterior_rf2 <- base_source_rf2$votes[,2]

threshold <- .1
base_source_year %>% filter(posterior_rf < threshold) %>% select(base_source, posterior_rf) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_rf >= threshold) %>% select(base_source, posterior_rf) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_rf < threshold & base_source == 1) %>% select(base_source, posterior_rf) %>% arrange(posterior_rf)

base_source_year %>% filter(posterior_rf2 < threshold) %>% select(base_source, posterior_rf2) %>% pull(base_source) %>% table # worse than individual groups
base_source_year %>% filter(posterior_rf2 >= threshold) %>% select(base_source, posterior_rf2) %>% pull(base_source) %>% table
base_source_year %>% filter(posterior_rf2 < threshold & base_source == 1) %>% select(base_source, posterior_rf2) %>% arrange(posterior_rf2)

base_source_year <- base_source_year %>% 
  mutate(y_glm=ifelse(posterior_glm < threshold, 0, 1),
         y_glm2=ifelse(posterior_glm2 < threshold, 0, 1),
         y_rf=ifelse(posterior_rf < threshold, 0, 1),
         y_rf2=ifelse(posterior_rf2 < threshold, 0, 1))

base_source_year %>%
  filter(y_glm != y_rf2) %>% 
  select(base_source, y_glm, y_rf2, posterior_glm, posterior_rf2) %>% 
  print(n=20)

base_source_year %>%
  filter(y_glm2 != y_rf2) %>% 
  select(base_source, posterior_glm2, posterior_rf2)


base_source_year %>%
  filter(y_glm2 != y_rf2) %>% 
  select(n_labs:base_seizures, coca_group, population, base_source, -erad_aerial, -erad_manual) %>% view

base_source_year$base_avg %>% summary

unmatched_index <- which(base_source_year$y_glm2 != base_source_year$y_rf2)
data_year_unmatched <- data_year[unmatched_index,]
base_source_unmatched_glm <- glm(base_source~., family="binomial",
                                 data=data_year_unmatched %>%
                                   mutate(coca_group=(Clan_del_Golfo + Los_Pelusos)) %>%
                                   select(n_labs:base_seizures, -erad_aerial, coca_group, population, base_source))
summary(base_source_glm2)
summary(base_source_unmatched_glm)

base_source_unmatched_rf <- randomForest(base_source~.,
                                         data=data_year_unmatched %>%
                                           mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                                           select(n_labs:base_seizures, coca_group, population, base_source),
                                         type="classification", ntree=50, mtry=10)

sum(data_year_unmatched$base_source == 1) # 15 sources in unmatched data

base_source_year_unmatched <- data_year_unmatched %>%
  select(id, n_labs:population, base_source) %>% 
  mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
  mutate(posterior_glm=base_source_unmatched_glm$fitted.values)
base_source_year_unmatched$posterior_rf <- base_source_unmatched_rf$votes[,2]

threshold <- .1
base_source_year_unmatched <- base_source_year_unmatched %>% 
  mutate(y_glm=ifelse(posterior_glm < threshold, 0, 1),
         y_rf=ifelse(posterior_rf < threshold, 0, 1))

base_source_year_unmatched %>% filter(posterior_glm < threshold) %>% select(base_source, posterior_glm) %>% pull(base_source) %>% table
base_source_year_unmatched %>% filter(posterior_glm >= threshold) %>% select(base_source, posterior_glm) %>% pull(base_source) %>% table

base_source_year_unmatched %>% filter(posterior_rf < threshold) %>% select(base_source, posterior_rf) %>% pull(base_source) %>% table
base_source_year_unmatched %>% filter(posterior_rf >= threshold) %>% select(base_source, posterior_rf) %>% pull(base_source) %>% table

base_source_year_unmatched %>%
  filter(y_glm != y_rf) %>% 
  select(base_source, y_glm, y_rf, posterior_glm, posterior_rf) %>% 
  print(n=20)

base_source_year_unmatched %>%
  filter(y_glm == y_rf) %>% 
  select(base_source, y_glm) %>% 
  table # only 4 more matched result


## kmeans clustering
set.seed(50)
base_source_kmeans <- kmeans(data_year %>%
                               mutate(coca_group=(Los_Rastrojos + Clan_del_Golfo + Los_Pelusos)) %>%
                               select(n_labs:base_seizures, -erad_aerial, coca_group, population) %>% 
                               scale,
                             4)

for (i in 1:length(base_source_kmeans$size)) {
  print(paste("cluster", i, ":", sum(base_source_year$base_source == 1 & as.numeric(base_source_kmeans$cluster == i) == 1)))
}
base_source_kmeans$size

base_source_year$cluster <- as.numeric(base_source_kmeans$cluster == 3)
table(base_source_year$base_source)
table(base_source_year$cluster)

base_source_year %>% 
  filter(base_source == 1 & cluster == 0) %>% 
  select(id, n_labs:n_HCl_labs, -erad_aerial, coca_group, population, posterior_glm)


## PCA
base_source_year_pca <- prcomp(base_source_year %>%
                                 select(n_labs:coca_seizures, -base_price_distance, -erad_aerial, -erad_manual, coca_group, population) %>% scale)
sum(base_source_year_pca$sdev[1:2])/sum(base_source_year_pca$sdev) # 0.3817472

base_source_year_pca$x %>% 
  as.data.frame %>% 
  mutate(base_source=base_source_year$base_source) %>% 
  ggplot() +
  geom_point(aes(x=PC1, y=PC2, color=base_source)) +
  lims(x=c(-2, 3), y=c(-1, 3))

F1_PU <- function(glm_model, test_data, positive) {
  n_data <- nrow(test_data)
  positive <- as.numeric(positive)
  n_positive <- sum(positive, na.rm=T)
  y_hat <- ifelse(predict(glm_model, test_data) > threshold, 1, 0)
  recall <- sum(positive*y_hat, na.rm=T)/n_positive
  result <- recall^2/(sum(y_hat/n_data, na.rm=T))
  return(result)
}
test_data_source1 <- data_year[c(base_source_positive_index[!(base_source_positive_index %in% data_year_base_source1_index$positive)],
                     sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data_year_base_source1_index$unlabeled)], 50)), ]
F1_PU(data_year_base_source1_glm, test_data_source1, test_data_source1$base_source)
test_data_source2 <- data_year[c(base_source_positive_index[!(base_source_positive_index %in% data_year_base_source2_index$positive)],
                     sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data_year_base_source2_index$unlabeled)], 50)), ]
F1_PU(data_year_base_source2_glm, test_data_source2, test_data_source2$base_source)
test_data_source3 <- data_year[c(base_source_positive_index[!(base_source_positive_index %in% data_year_base_source3_index$positive)],
                             sample(base_source_unlabeled_index[!(base_source_unlabeled_index %in% data_year_base_source3_index$unlabeled)], 50)), ]
F1_PU(data_year_base_source3_glm, test_data_source3, test_data_source3$base_source)

test_data_destination1 <- data_year[c(base_destination_positive_index[!(base_destination_positive_index %in% data_year_base_destination1_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data_year_base_destination1_index$unlabeled)], 50)), ]
F1_PU(data_year_base_destination1_glm, test_data_destination1, test_data_destination1$base_destination)
test_data_destination2 <- data_year[c(base_destination_positive_index[!(base_destination_positive_index %in% data_year_base_destination2_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data_year_base_destination2_index$unlabeled)], 50)), ]
F1_PU(data_year_base_destination2_glm, test_data_destination2, test_data_destination2$base_destination)
test_data_destination3 <- data_year[c(base_destination_positive_index[!(base_destination_positive_index %in% data_year_base_destination3_index$positive)],
                                  sample(base_destination_unlabeled_index[!(base_destination_unlabeled_index %in% data_year_base_destination3_index$unlabeled)], 50)), ]
F1_PU(data_year_base_destination3_glm, test_data_destination3, test_data_destination3$base_destination)
