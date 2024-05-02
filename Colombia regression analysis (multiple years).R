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
    mutate(id=as.numeric(id)) %>% 
    filter(!(id %in% c(88001, 88564)))
  map_df <- left_join(map_df, municipios_capital %>% unique, by="id")
  
  base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
  HCl_to_HCl <- read.csv("Colombia Data/Anecdotal HCl to HCl municipality only.csv") %>% as_tibble
  general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
  anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual with municipality.csv") %>% as_tibble
  labs_PPI_reg_data <- read.csv("Colombia Data/labs_PPI_reg_data.csv") %>% as_tibble
  labs_HCl_reg_data <- read.csv("Colombia Data/labs_HCl_reg_data.csv") %>% as_tibble
}

regression_data_years <- read.csv("Colombia Data/regression data (05-01-2024).csv") %>% as_tibble

# regression without n_armed_groups
HCl_destination_glm_years <- glm(HCl_destination~., family="binomial",
                                 data=regression_data_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   select(year, n_labs:population, HCl_destination, -n_armed_groups, -base_avg, -base_price_distance,
                                          -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_destination_glm_years)
fit_test_years <- regression_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_labs:population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_destination_glm_years$fitted.values)

sum(fit_test_years$HCl_destination == 1) # 329

threshold <- .1
confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$HCl_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(HCl_destination) %>% as.factor,
                positive="1")

  # random forest
HCl_destination_rf_years <- randomForest(HCl_destination~.,
                                         data=regression_data_years %>%
                                           mutate(year=as.factor(year),
                                                  HCl_destination=as.factor(HCl_destination)) %>%
                                           select(year, n_labs:population, HCl_destination, -n_armed_groups, -base_avg, -base_price_distance,
                                                  -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures),
                                         type="classification", ntree=50, mtry=8)
fit_test_years$posterior_rf <- HCl_destination_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$HCl_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(HCl_destination) %>% as.factor,
                positive="1")

## regression with n_armed_groups
HCl_destination_glm_years <- glm(HCl_destination~., family="binomial",
                                 data=regression_data_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   select(year, n_labs:population, HCl_destination, -base_avg, -base_price_distance,
                                          -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_destination_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data

fit_test_years <- regression_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_labs:population, HCl_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_destination_glm_years$fitted.values)

confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$HCl_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(HCl_destination) %>% as.factor,
                positive="1")

  # random forest
HCl_destination_rf_years <- randomForest(HCl_destination~.,
                                         data=regression_data_years %>%
                                           mutate(year=as.factor(year),
                                                  HCl_destination=as.factor(HCl_destination)) %>%
                                           select(year, n_labs:population, HCl_destination, -base_avg, -base_price_distance,
                                                  -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures),
                                         type="classification", ntree=50, mtry=8)
fit_test_years$posterior_rf <- HCl_destination_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$HCl_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(HCl_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(HCl_destination) %>% as.factor,
                positive="1")

fit_test_years %>%
  filter(posterior_glm < threshold & posterior_rf < threshold & HCl_destination == 1) %>% 
  select(-erad_aerial, -erad_manual)

fit_test_years %>%
  filter(posterior_glm >= threshold & posterior_rf >= threshold & HCl_destination == 1) %>% 
  select(-erad_aerial, -erad_manual) %>% 
  summary

glm(HCl_destination~., family="binomial",
    data=regression_data_years %>%
      left_join(price_med %>% select(id, year, hyd_med), by=c("id", "year")) %>% 
      mutate(year=as.factor(year)) %>%
      select(year, n_labs:population, HCl_destination, hyd_med, -hyd_avg, -base_avg, -base_price_distance,
             -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures)) %>% 
  summary

southeast_deptos <- c("Meta", "Vichada", "Caqueta", "Guaviare", "Guainia", "Vaupes", "Amazonas")
southeast_id_depto <- municipios_capital %>% 
  select(-municipio) %>% 
  filter(depto %in% southeast_deptos) %>% 
  unique %>% 
  pull(id)

fit_test_years %>%
  mutate(id=regression_data_years$id) %>% 
  filter(posterior_glm < threshold & posterior_rf >= threshold & id %in% southeast_id_depto) %>% 
  select(-erad_aerial, -erad_manual) %>% summary

fit_test_years %>%
  mutate(id=regression_data_years$id) %>% 
  filter(posterior_glm >= threshold) %>% 
  select(-erad_aerial, -erad_manual) %>% summary

depto_map <- suppressMessages(fortify(departamentos)) %>% 
  mutate(id=as.numeric(id)) %>% 
  filter(id != 88) %>% 
  left_join(municipios_capital %>% mutate(id=as.numeric(id_depto)) %>% select(id, depto) %>% unique, by="id")
empty_map <- ggplot(depto_map, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill = ""),
               color = "black",
               linewidth = 0.1,
               fill="white") + 
  expand_limits(x = depto_map$long, y = depto_map$lat) + 
  coord_quickmap() +
  scale_fill_manual(values="", na.value = "") +
  labs(fill="", x="", y="", title="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        line = element_blank()
  )

HCl_destination_map_data <- 
  regression_data_years %>% 
  select(id, HCl_destination) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years$posterior_glm,
         posterior_rf=fit_test_years$posterior_rf)

HCl_to_HCl_destination_map <- empty_map +
  geom_point(data=HCl_destination_map_data %>% 
               filter(HCl_destination == 1),
             aes(x=long, 
                 y=lat),
             color="black",
             size=0.1) +
  labs(title="Anecdotal HCl Destinations")

HCl_regression_destination_map <- empty_map +
  geom_point(data=HCl_destination_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (HCl Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

HCl_rf_destination_map <- empty_map +
  geom_point(data=HCl_destination_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (HCl Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/HCl destinations anecdotal (2013-2016).png", HCl_to_HCl_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/HCl destinations regression (2013-2016).png", HCl_regression_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/HCl destinations rf (2013-2016).png", HCl_rf_destination_map, scale=1)


## HCl source regression
HCl_source_glm_years <- glm(HCl_source~., family="binomial",
                                 data=regression_data_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   select(year, n_labs:population, HCl_source, -base_avg, -base_price_distance,
                                          -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(HCl_source_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data

fit_test_years <- regression_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_labs:population, HCl_source, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=HCl_source_glm_years$fitted.values)

confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$HCl_source %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(HCl_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(HCl_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(HCl_source) %>% as.factor,
                positive="1")

# random forest
HCl_source_rf_years <- randomForest(HCl_source~.,
                                         data=regression_data_years %>%
                                           mutate(year=as.factor(year),
                                                  HCl_source=as.factor(HCl_source)) %>%
                                           select(year, n_labs:population, HCl_source, -base_avg, -base_price_distance,
                                                  -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures),
                                         type="classification", ntree=50, mtry=8)
fit_test_years$posterior_rf <- HCl_source_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$HCl_source %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(HCl_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(HCl_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(HCl_source) %>% as.factor,
                positive="1")

HCl_source_map_data <- 
  regression_data_years %>% 
  select(id, HCl_source) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years$posterior_glm,
         posterior_rf=fit_test_years$posterior_rf)

HCl_to_HCl_source_map <- empty_map +
  geom_point(data=HCl_source_map_data %>% 
               filter(HCl_source == 1),
             aes(x=long, 
                 y=lat),
             color="black",
             size=0.1) +
  labs(title="Anecdotal HCl sources")

HCl_regression_source_map <- empty_map +
  geom_point(data=HCl_source_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (HCl sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

HCl_rf_source_map <- empty_map +
  geom_point(data=HCl_source_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (HCl sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/HCl sources anecdotal (2013-2016).png", HCl_to_HCl_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/HCl sources regression (2013-2016).png", HCl_regression_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/HCl sources rf (2013-2016).png", HCl_rf_source_map, scale=1)

## Base destination regression
base_destination_glm_years <- glm(base_destination~., family="binomial",
                                  data=regression_data_years %>% 
                                    filter(year != 2014) %>%
                                    mutate(year=as.factor(year)) %>%
                                    select(year, n_labs:population, base_destination, -hyd_avg, -hyd_price_distance,
                                           -paste_avg, -paste_price_distance, -HCl_seizures))
summary(base_destination_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data

fit_test_years <- regression_data_years %>%
  filter(year != 2014) %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_labs:population, base_destination, -hyd_avg, -hyd_price_distance, -paste_avg, -paste_price_distance, -HCl_seizures) %>% 
  mutate(posterior_glm=base_destination_glm_years$fitted.values)

confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$base_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(base_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(base_destination) %>% as.factor,
                positive="1")

  # random forest
base_destination_rf_years <- randomForest(base_destination~.,
                                          data=regression_data_years %>% 
                                            filter(year != 2014) %>%
                                            mutate(year=as.factor(year),
                                                   base_destination=as.factor(base_destination)) %>%
                                            select(year, n_labs:population, base_destination, -hyd_avg, -hyd_price_distance,
                                                   -paste_avg, -paste_price_distance, -HCl_seizures),
                                          type="classification", ntree=50, mtry=8)
fit_test_years$posterior_rf <- base_destination_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$base_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(base_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(base_destination) %>% as.factor,
                positive="1")

base_destination_map_data <- 
  regression_data_years %>% 
  filter(year != 2014) %>%
  select(id, base_destination) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years$posterior_glm,
         posterior_rf=fit_test_years$posterior_rf)

base_to_base_destination_map <- empty_map +
  geom_point(data=base_destination_map_data %>% 
               filter(base_destination == 1),
             aes(x=long, 
                 y=lat),
             color="black",
             size=0.1) +
  labs(title="Anecdotal base destinations")

base_regression_destination_map <- empty_map +
  geom_point(data=base_destination_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (base destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

base_rf_destination_map <- empty_map +
  geom_point(data=base_destination_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (base destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/base destinations anecdotal (2013-2016).png", base_to_base_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base destinations regression (2013-2016).png", base_regression_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base destinations rf (2013-2016).png", base_rf_destination_map, scale=1)


## base source regression
base_source_glm_years <- glm(base_source~., family="binomial",
                             data=regression_data_years %>%
                               filter(year != 2014) %>%
                               mutate(year=as.factor(year)) %>%
                               select(year, n_labs:population, base_source, -hyd_avg, -hyd_price_distance,
                                      -paste_avg, -paste_price_distance, -HCl_seizures))
summary(base_source_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data

fit_test_years <- regression_data_years %>%
  filter(year != 2014) %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_labs:population, base_source, -hyd_avg, -hyd_price_distance, -paste_avg, -paste_price_distance, -HCl_seizures) %>% 
  mutate(posterior_glm=base_source_glm_years$fitted.values)

confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$base_source %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(base_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(base_source) %>% as.factor,
                positive="1")

  # random forest
base_source_rf_years <- randomForest(base_source~.,
                                     data=regression_data_years %>%
                                       filter(year != 2014) %>%
                                       mutate(year=as.factor(year),
                                              base_source=as.factor(base_source)) %>%
                                       select(year, n_labs:population, base_source, -hyd_avg, -hyd_price_distance,
                                              -paste_avg, -paste_price_distance, -HCl_seizures),
                                     type="classification", ntree=50, mtry=8)
fit_test_years$posterior_rf <- base_source_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$base_source %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(base_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(base_source) %>% as.factor,
                positive="1")

base_source_map_data <- 
  regression_data_years %>% 
  filter(year != 2014) %>%
  select(id, base_source) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years$posterior_glm,
         posterior_rf=fit_test_years$posterior_rf)

base_to_base_source_map <- empty_map +
  geom_point(data=base_source_map_data %>% 
               filter(base_source == 1),
             aes(x=long, 
                 y=lat),
             color="black",
             size=0.1) +
  labs(title="Anecdotal base sources")

base_regression_source_map <- empty_map +
  geom_point(data=base_source_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (base sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

base_rf_source_map <- empty_map +
  geom_point(data=base_source_map_data,
             aes(x=long, 
                 y=lat,
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (base sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/base sources anecdotal (2013-2016).png", base_to_base_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base sources regression (2013-2016).png", base_regression_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base sources rf (2013-2016).png", base_rf_source_map, scale=1)