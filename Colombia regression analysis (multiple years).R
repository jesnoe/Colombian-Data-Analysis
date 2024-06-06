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
  
  municipio_centroid <- map_df %>% 
    filter(!(id %in% c(88001, 88564))) %>% 
    group_by(id, municipio, depto) %>% 
    summarize(long=mean(long),
              lat=mean(lat))
  
  base_to_base <- read.csv("Colombia Data/Anecdotal base to base municipality only.csv") %>% as_tibble
  hyd_to_hyd <- read.csv("Colombia Data/Anecdotal hyd to hyd municipality only.csv") %>% as_tibble
  general <- read.csv("Colombia Data/Anecdotal general municipality only.csv") %>% as_tibble
  anecdotal_annual <- read.csv("Colombia Data/Anecdotal annual with municipality.csv") %>% as_tibble
  labs_PPI_reg_data <- read.csv("Colombia Data/labs_PPI_reg_data.csv") %>% as_tibble
  labs_hyd_reg_data <- read.csv("Colombia Data/labs_hyd_reg_data.csv") %>% as_tibble
  
  airports <- read.csv("Colombia Data/airports.csv") %>% as_tibble
}

regression_data_years <- read.csv("Colombia Data/regression data (05-15-2024).csv") %>% as_tibble %>% 
  left_join(airports, by="id") %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1],
         airport=ifelse(n_airports > 0, 1, 0))

ever_anecdotal <- regression_data_years %>% 
  group_by(id) %>% 
  summarise(base_source=ifelse(sum(base_source)>0, 1, 0),
            base_destination=ifelse(sum(base_destination)>0, 1, 0),
            hyd_source=ifelse(sum(hyd_source)>0, 1, 0),
            hyd_destination=ifelse(sum(hyd_destination)>0, 1, 0))

ever_anecdotal_data_years <- regression_data_years %>% 
  select(-(base_source_all:general_destination)) %>% 
  left_join(ever_anecdotal, by="id")

## regression with n_armed_groups
hyd_destination_glm_years <- glm(hyd_destination~., family="binomial",
                                 data=ever_anecdotal_data_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   select(year, n_PPI_labs:population, n_airports, hyd_destination, -base_avg, -base_price_distance,
                                          -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group))
summary(hyd_destination_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data
hyd_destination_glm_years$significance <- ifelse(summary(hyd_destination_glm_years)$coefficients[,4] <= 0.05, 1, 0)


tibble(var=names(hyd_destination_glm_years$coefficients), coefficients=hyd_destination_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients)) +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90))

tibble(var=names(hyd_destination_glm_years$coefficients), coefficients=hyd_destination_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=sign(coefficients))) +
  labs(x="", y="sign of coefficients") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     breaks = c(-1,0, 1)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.1))

fit_test_years <- ever_anecdotal_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, hyd_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=hyd_destination_glm_years$fitted.values)

threshold <- .5
confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$hyd_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(hyd_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(hyd_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(hyd_destination) %>% as.factor,
                positive="1")

  # random forest
set.seed(100)
hyd_destination_rf_years <- randomForest(hyd_destination~.,
                                         data=ever_anecdotal_data_years %>%
                                           mutate(year=as.factor(year),
                                                  hyd_destination=as.factor(hyd_destination)) %>%
                                           select(year, n_PPI_labs:population, hyd_destination, -base_avg, -base_price_distance,
                                                  -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group),
                                         type="classification", ntree=50, mtry=8)
fit_test_years$posterior_rf <- hyd_destination_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$hyd_destination %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(hyd_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(hyd_destination) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(hyd_destination) %>% as.factor,
                positive="1")

fit_test_years %>%
  filter(posterior_glm < threshold & posterior_rf < threshold & hyd_destination == 1) %>% 
  select(-erad_aerial, -erad_manual)

fit_test_years %>%
  filter(posterior_glm >= threshold & posterior_rf >= threshold & hyd_destination == 1) %>% 
  select(-erad_aerial, -erad_manual) %>% 
  summary

glm(hyd_destination~., family="binomial",
    data=regression_data_years %>%
      left_join(price_med %>% select(id, year, hyd_med), by=c("id", "year")) %>% 
      mutate(year=as.factor(year)) %>%
      select(year, n_PPI_labs:population, hyd_destination, hyd_med, -hyd_avg, -base_avg, -base_price_distance,
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

hyd_destination_map_data <- 
  ever_anecdotal_data_years %>% 
  select(id, hyd_destination) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years$posterior_glm,
         posterior_rf=fit_test_years$posterior_rf)

hyd_to_hyd_destination_map <- empty_map +
  geom_point(data=hyd_destination_map_data %>% 
               filter(hyd_destination == 1),
             aes(x=long, 
                 y=lat),
             color="black",
             size=0.1) +
  labs(title="Anecdotal hyd Destinations")

hyd_regression_destination_map <- empty_map +
  geom_point(data=hyd_destination_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_glm < threshold, 2*(threshold-posterior_glm), 2*(posterior_glm-threshold)),
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (hyd Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") +
  theme(legend.position="right")

hyd_rf_destination_map <- empty_map +
  geom_point(data=hyd_destination_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_rf < threshold, 2*(threshold-posterior_rf), 2*(posterior_rf-threshold)),
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (hyd Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") + 
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/hyd destinations anecdotal (2013-2016).png", hyd_to_hyd_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd destinations regression without coords (2013-2016).png", hyd_regression_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd destinations rf without coords (2013-2016).png", hyd_rf_destination_map, scale=1)


## hyd source regression
hyd_source_glm_years <- glm(hyd_source~., family="binomial",
                                 data=ever_anecdotal_data_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   select(year, n_PPI_labs:population, airports, hyd_source, -base_avg, -base_price_distance,
                                          -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group))
summary(hyd_source_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data
hyd_source_glm_years$significance <- ifelse(summary(hyd_source_glm_years)$coefficients[,4] <= 0.05, 1, 0)
names(hyd_source_glm_years$coefficients) <- ifelse(summary(hyd_destination_glm_years)$coefficients[,4] <= 0.05,
                                                   paste0(names(hyd_source_glm_years$coefficients), "*"),
                                                   paste0(names(hyd_source_glm_years$coefficients), "-"))
names(hyd_source_glm_years$coefficients) <- ifelse(summary(hyd_source_glm_years)$coefficients[,4] <= 0.05,
                                                   paste0(names(hyd_source_glm_years$coefficients), "*"),
                                                   paste0(names(hyd_source_glm_years$coefficients), "-"))

tibble(var=names(hyd_source_glm_years$coefficients), coefficients=hyd_source_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients)) +
  scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90))

tibble(var=names(hyd_source_glm_years$coefficients), coefficients=hyd_source_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=sign(coefficients))) +
  labs(x="", y="sign of coefficients") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     breaks = c(-1,0, 1)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.1))

fit_test_years <- ever_anecdotal_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, hyd_source, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=hyd_source_glm_years$fitted.values)

confusionMatrix(ifelse(fit_test_years$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years$hyd_source %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(hyd_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(hyd_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_glm) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(hyd_source) %>% as.factor,
                positive="1")

# random forest
set.seed(100)
hyd_source_rf_years <- randomForest(hyd_source~.,
                                         data=ever_anecdotal_data_years %>%
                                           mutate(year=as.factor(year),
                                                  hyd_source=as.factor(hyd_source)) %>%
                                           select(year, n_PPI_labs:population, hyd_source, -base_avg, -base_price_distance,
                                                  -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group),
                                         type="classification", ntree=50, mtry=8)
hyd_source_rf_years$importance
fit_test_years$posterior_rf <- hyd_source_rf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years$hyd_source %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2013) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2013) %>% pull(hyd_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2014) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2014) %>% pull(hyd_source) %>% as.factor,
                positive="1")
confusionMatrix(ifelse(fit_test_years %>% filter(year==2016) %>% pull(posterior_rf) < threshold, 0, 1) %>% as.factor,
                fit_test_years %>% filter(year==2016) %>% pull(hyd_source) %>% as.factor,
                positive="1")

hyd_source_map_data <- 
  ever_anecdotal_data_years %>% 
  select(id, hyd_source) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years$posterior_glm,
         posterior_rf=fit_test_years$posterior_rf)

hyd_to_hyd_source_map <- empty_map +
  geom_point(data=hyd_source_map_data %>% 
               filter(hyd_source == 1),
             aes(x=long, 
                 y=lat),
             color="black",
             size=0.1) +
  labs(title="Anecdotal hyd sources")

hyd_regression_source_map <- empty_map +
  geom_point(data=hyd_source_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_glm < threshold, 2*(threshold-posterior_glm), 2*(posterior_glm-threshold)),
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (hyd sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") +
  theme(legend.position="right")

hyd_rf_source_map <- empty_map +
  geom_point(data=hyd_source_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_rf < threshold, 2*(threshold-posterior_rf), 2*(posterior_rf-threshold)),
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (hyd sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") +
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/hyd sources anecdotal (2013-2016).png", hyd_to_hyd_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd sources regression without coords (2013-2016).png", hyd_regression_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd sources rf without coords (2013-2016).png", hyd_rf_source_map, scale=1)

## Base destination regression
base_destination_glm_years <- glm(base_destination~., family="binomial",
                                  data=regression_data_years %>% 
                                    filter(year != 2014) %>%
                                    mutate(year=as.factor(year)) %>%
                                    select(year, n_PPI_labs:population, base_destination, airport, -hyd_avg, -hyd_price_distance,
                                           -paste_avg, -paste_price_distance, -hyd_seizures))
summary(base_destination_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data
base_destination_glm_years$significance <- ifelse(summary(base_destination_glm_years)$coefficients[,4] <= 0.05, 1, 0)
names(base_destination_glm_years$coefficients) <- ifelse(summary(base_destination_glm_years)$coefficients[,4] <= 0.05,
                                                         paste0(names(base_destination_glm_years$coefficients), "*"),
                                                         paste0(names(base_destination_glm_years$coefficients), "-"))

tibble(var=names(base_destination_glm_years$coefficients), coefficients=base_destination_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients)) +
  scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90))

tibble(var=names(base_destination_glm_years$coefficients), coefficients=base_destination_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=sign(coefficients))) +
  labs(x="", y="sign of coefficients") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     breaks = c(-1,0, 1)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.1))

fit_test_years <- regression_data_years %>%
  filter(year != 2014) %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, base_destination, -hyd_avg, -hyd_price_distance, -paste_avg, -paste_price_distance, -hyd_seizures) %>% 
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
set.seed(100)
base_destination_rf_years <- randomForest(base_destination~.,
                                          data=regression_data_years %>% 
                                            filter(year != 2014) %>%
                                            mutate(year=as.factor(year),
                                                   base_destination=as.factor(base_destination)) %>%
                                            select(year, n_PPI_labs:population, base_destination, -hyd_avg, -hyd_price_distance,
                                                   -paste_avg, -paste_price_distance, -hyd_seizures),
                                          type="classification", ntree=50, mtry=8)
base_destination_rf_years$importance
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
                 alpha=ifelse(posterior_glm < threshold, 1, posterior_glm),  color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (base destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") + 
  theme(legend.position="right")

base_rf_destination_map <- empty_map +
  geom_point(data=base_destination_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_glm < threshold, 1, posterior_glm),  color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (base destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") + 
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/base destinations anecdotal (2013-2016).png", base_to_base_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base destinations regression without coords (2013-2016).png", base_regression_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base destinations rf without coords (2013-2016).png", base_rf_destination_map, scale=1)


## base source regression
base_source_glm_years <- glm(base_source~., family="binomial",
                             data=regression_data_years %>%
                               filter(year != 2014) %>%
                               mutate(year=as.factor(year)) %>%
                               select(year, n_PPI_labs:population, base_source, airport, -hyd_avg, -hyd_price_distance,
                                      -paste_avg, -paste_price_distance, -hyd_seizures))
summary(base_source_glm_years) # n_armed_groups was insignificant for the results with individual year data, but significant with multiple years data
base_source_glm_years$significance <- ifelse(summary(base_source_glm_years)$coefficients[,4] <= 0.05, 1, 0)
names(base_source_glm_years$coefficients) <- ifelse(summary(base_source_glm_years)$coefficients[,4] <= 0.05,
                                                    paste0(names(base_source_glm_years$coefficients), "*"),
                                                    paste0(names(base_source_glm_years$coefficients), "-"))
names(base_source_glm_years$coefficients) <- ifelse(summary(base_destination_glm_years)$coefficients[,4] <= 0.05,
                                                    paste0(names(base_source_glm_years$coefficients), "*"),
                                                    paste0(names(base_source_glm_years$coefficients), "-"))

tibble(var=names(base_source_glm_years$coefficients), coefficients=base_source_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients)) +
  scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90))

tibble(var=names(base_source_glm_years$coefficients), coefficients=base_source_glm_years$coefficients) %>% 
  ggplot() +
  geom_col(aes(x=var, y=sign(coefficients))) +
  labs(x="", y="sign of coefficients") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     breaks = c(-1,0, 1)) +
  theme(axis.text.x = element_text(angle=90))

fit_test_years <- regression_data_years %>%
  filter(year != 2014) %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, base_source, -hyd_avg, -hyd_price_distance, -paste_avg, -paste_price_distance, -hyd_seizures) %>% 
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
set.seed(100)
base_source_rf_years <- randomForest(base_source~.,
                                     data=regression_data_years %>%
                                       filter(year != 2014) %>%
                                       mutate(year=as.factor(year),
                                              base_source=as.factor(base_source)) %>%
                                       select(year, n_PPI_labs:population, base_source, -hyd_avg, -hyd_price_distance,
                                              -paste_avg, -paste_price_distance, -hyd_seizures),
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
                 alpha=ifelse(posterior_glm < threshold, 1, posterior_glm),
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (base sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") + 
  theme(legend.position="right")

base_rf_source_map <- empty_map +
  geom_point(data=base_source_map_data,
             aes(x=long, 
                 y=lat,
                 alpha=ifelse(posterior_glm < threshold, 1, posterior_glm),
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (base sources)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") + 
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/base sources anecdotal (2013-2016).png", base_to_base_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base sources regression without coords (2013-2016).png", base_regression_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/base sources rf without coords (2013-2016).png", base_rf_source_map, scale=1)

## coefficients bar chart
base_reg_coefs <- tibble(var=rep(names(base_source_glm_years$coefficients[-1]), 2),
                         coefficients=c(base_source_glm_years$coefficients[-1], base_destination_glm_years$coefficients[-1]),
                         response=c(rep("base_source", 19), rep("base_destination", 19))) %>% 
  mutate(var=as.factor(var),
         response=as.factor(response),
         # response=factor(response, levels=c("base_source", "base_destination")),
         significance=c(base_source_glm_years$significance[-1], base_destination_glm_years$significance[-1]))

hyd_reg_coefs <- tibble(var=rep(names(hyd_source_glm_years$coefficients[-1]),2),
                        coefficients=c(hyd_source_glm_years$coefficients[-1], hyd_destination_glm_years$coefficients[-1]),
                        response=c(rep("hyd_source", 18), rep("hyd_destination", 18))) %>% 
  mutate(var=as.factor(var),
         response=as.factor(response),
         # response=factor(response, levels=c("hyd_source", "hyd_destination")),
         significance=c(hyd_source_glm_years$significance[-1], hyd_destination_glm_years$significance[-1]))

base_reg_coefs %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients, group=response, fill=response),
           position="dodge") +
  # scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2),
        legend.position = "bottom")

hyd_reg_coefs %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients, group=response, fill=response),
           position="dodge") +
  # scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2),
        legend.position = "bottom")

## importance bar chart
base_rf_importances <- tibble(var=rep(rownames(base_source_rf_years$importance),2),
                              importance=c(base_source_rf_years$importance, base_destination_rf_years$importance),
                              response=c(rep("base_source", 19), rep("base_destination", 19))) %>% 
  mutate(var=as.factor(var),
         response=as.factor(response))
         # response=factor(response, levels=c("base_source", "base_destination")))

hyd_rf_importances <- tibble(var=rep(rownames(hyd_source_rf_years$importance),2),
                             importance=c(hyd_source_rf_years$importance, hyd_destination_rf_years$importance),
                             response=c(rep("hyd_source", 17), rep("hyd_destination", 17))) %>% 
  mutate(var=as.factor(var),
         response=as.factor(response))
         # response=factor(response, levels=c("hyd_source", "hyd_destination")))

base_rf_importances %>% 
  ggplot() +
  geom_col(aes(x=var, y=importance, group=response, fill=response),
           position="dodge") +
  # scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2),
        legend.position = "bottom")

hyd_rf_importances %>% 
  ggplot() +
  geom_col(aes(x=var, y=importance, group=response, fill=response),
           position="dodge") +
  # scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2),
        legend.position = "bottom")

# PU F1 score comparison
F1_PU <- function(prediction, positive) {
  n_data <- length(prediction)
  positive <- as.numeric(positive)
  n_positive <- sum(positive, na.rm=T)
  recall <- sum(positive*prediction, na.rm=T)/n_positive
  result <- recall^2/(sum(prediction/n_data, na.rm=T))
  return(result)
}

hyd_destination_combined <- regression_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, hyd_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=hyd_destination_glm_years$fitted.values,
         posterior_rf=hyd_destination_rf_years$votes[,2])
hyd_destination_combined <- hyd_destination_combined %>% 
  mutate(hyd_destination=ifelse(posterior_glm >= 0.2 & posterior_rf >= 0.2, 1, hyd_destination))
sum(hyd_destination_combined$hyd_destination)

hyd_destination_combined_glm <- glm(hyd_destination~., family="binomial",
                                    data=hyd_destination_combined %>%
                                      select(-posterior_glm, -posterior_rf))
hyd_destination_combined_rf <- randomForest(hyd_destination~.,
                                            data=hyd_destination_combined %>%
                                              mutate(hyd_destination=as.factor(hyd_destination)) %>%
                                              select(-posterior_glm, -posterior_rf),
                                            type="classification", ntree=50, mtry=8)

F1_PU(ifelse(hyd_destination_glm_years$fitted.values < threshold, 0, 1), regression_data_years$hyd_destination) # 1.105881
F1_PU(as.numeric(hyd_destination_rf_years$predicted) - 1, regression_data_years$hyd_destination) # 1.65043
F1_PU(ifelse(hyd_destination_combined_glm$fitted.values < threshold, 0, 1), regression_data_years$hyd_destination) # 1.17615
F1_PU(as.numeric(hyd_destination_combined_rf$predicted) - 1, regression_data_years$hyd_destination) # 1.754631
