library(pracma)
library(GWmodel)
regression_data_years <- read.csv("Colombia Data/regression data (05-15-2024).csv") %>% as_tibble %>% 
  mutate(base_avg=scale(base_avg)[,1],
         paste_avg=scale(paste_avg)[,1],
         hyd_avg=scale(hyd_avg)[,1])

ever_anecdotal <- regression_data_years %>% 
  group_by(id) %>% 
  summarise(base_source=ifelse(sum(base_source)>0, 1, 0),
            base_destination=ifelse(sum(base_destination)>0, 1, 0),
            hyd_source=ifelse(sum(hyd_source)>0, 1, 0),
            hyd_destination=ifelse(sum(hyd_destination)>0, 1, 0))

ever_anecdotal_data_years <- regression_data_years %>% 
  select(-(base_source_all:general_destination)) %>% 
  left_join(ever_anecdotal, by="id")
ever_anecdotal %>% select(-id) %>% apply(2, sum) # 541 municipalities
ever_anecdotal_data_years %>% select(base_source:hyd_destination) %>% apply(2, sum) # 1,623 rows

# filter out departments without anecdotal evidence
anecdotal_id_depto <- ever_anecdotal_data_years %>% 
  left_join(municipios_capital %>% select(id, id_depto), by="id") %>% 
  group_by(id_depto) %>% 
  summarise(hyd_source=sum(hyd_source),
            hyd_destination=sum(hyd_destination)) %>% 
  mutate(anecdotal=hyd_source+hyd_destination>0) %>% 
  filter(anecdotal) %>% pull(id_depto)
ever_anecdotal_data_years <- ever_anecdotal_data_years %>% 
  left_join(municipios_capital %>% select(id, id_depto), by="id") %>% 
  filter(id_depto %in% anecdotal_id_depto)

hyd_destination_gwr_years <- glm(hyd_destination~.+long*lat, family="binomial",
                                 data=ever_anecdotal_data_years %>%
                                   mutate(year=as.factor(year)) %>%
                                   left_join(municipio_centroid %>% select(id, long, lat), by="id") %>% 
                                   select(year, n_PPI_labs:population, hyd_destination, long, lat, -municipio, -base_group,
                                          -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(hyd_destination_gwr_years)

fit_test_years_gw <- ever_anecdotal_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, hyd_destination, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=hyd_destination_gwr_years$fitted.values)

threshold <- .5
confusionMatrix(ifelse(fit_test_years_gw$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years_gw$hyd_destination %>% as.factor,
                positive="1")

set.seed(100)
hyd_destination_grf_years <- randomForest(hyd_destination~.+long*lat,
                                          data=ever_anecdotal_data_years %>%
                                            mutate(year=as.factor(year),
                                                   hyd_destination=as.factor(hyd_destination)) %>%
                                            left_join(municipio_centroid %>% select(id, long, lat), by="id") %>% 
                                            select(year, n_PPI_labs:population, hyd_destination, long, lat, -municipio, -base_group,
                                                  -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures),
                                          type="classification", ntree=50, mtry=10)
fit_test_years_gw$posterior_rf <- hyd_destination_grf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years_gw$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years_gw$hyd_destination %>% as.factor,
                positive="1")

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
  mutate(posterior_glm=fit_test_years_gw$posterior_glm,
         posterior_rf=fit_test_years_gw$posterior_rf)

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

# ggsave("Colombia Data/Figs/prediction map/hyd destinations regression with coords (2013-2016).png", hyd_regression_destination_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd destinations rf with coords (2013-2016).png", hyd_rf_destination_map, scale=1)

hyd_reg_destination_new_pred_map <- empty_map +
  geom_point(data=hyd_destination_map_data %>% filter(hyd_destination==0 & posterior_glm >= threshold),
             aes(x=long, 
                 y=lat,
                 alpha=2*(posterior_glm-threshold),
                 color=ifelse(posterior_glm < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="Regression Predictions (hyd Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") +
  theme(legend.position="right")

hyd_rf_destination_new_pred_map <- empty_map +
  geom_point(data=hyd_destination_map_data %>% filter(hyd_destination==0 & posterior_rf >= threshold),
             aes(x=long, 
                 y=lat,
                 alpha=2*(posterior_rf-threshold),
                 color=ifelse(posterior_rf < threshold, "y_hat=0", "y_hat=1")),
             size=0.1) +
  labs(title="RF Predictions (hyd Destinations)", color="") +
  scale_color_manual(values=c("y_hat=0" = "blue",
                              "y_hat=1" = "red")) +
  guides(alpha="none") + 
  theme(legend.position="right")

# ggsave("Colombia Data/Figs/prediction map/hyd destinations reg new pred with coords (2013-2016).png", hyd_reg_destination_new_pred_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd destinations rf new pred with coords (2013-2016).png", hyd_rf_destination_new_pred_map, scale=1)

  # hyd source
hyd_source_gwr_years <- glm(hyd_source~.+long*lat, family="binomial",
                            data=ever_anecdotal_data_years %>%
                              mutate(year=as.factor(year)) %>%
                              left_join(municipio_centroid %>% select(id, long, lat), by="id") %>% 
                              select(year, n_PPI_labs:population, hyd_source, long, lat, -municipio, -base_group,
                                     -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures))
summary(hyd_source_gwr_years)
names(hyd_source_gwr_years$coefficients) <- ifelse(summary(hyd_destination_gwr_years)$coefficients[,4] <= 0.05,
                                                   paste0(names(hyd_source_gwr_years$coefficients), "*"),
                                                   paste0(names(hyd_source_gwr_years$coefficients), "-"))
names(hyd_source_gwr_years$coefficients) <- ifelse(summary(hyd_source_gwr_years)$coefficients[,4] <= 0.05,
                                                   paste0(names(hyd_source_gwr_years$coefficients), "*"),
                                                   paste0(names(hyd_source_gwr_years$coefficients), "-"))

fit_test_years_gw <- ever_anecdotal_data_years %>%
  mutate(year=as.factor(year)) %>%
  select(year, n_PPI_labs:population, hyd_source, -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures) %>% 
  mutate(posterior_glm=hyd_source_gwr_years$fitted.values)

confusionMatrix(ifelse(fit_test_years_gw$posterior_glm < threshold, 0, 1) %>% as.factor,
                fit_test_years_gw$hyd_source %>% as.factor,
                positive="1")

set.seed(100)
hyd_source_grf_years <- randomForest(hyd_source~.+long*lat,
                                     data=ever_anecdotal_data_years %>%
                                       mutate(year=as.factor(year),
                                              hyd_source=as.factor(hyd_source)) %>%
                                       left_join(municipio_centroid %>% select(id, long, lat), by="id") %>% 
                                       select(year, n_PPI_labs:population, hyd_source, long, lat, -municipio, -base_group,
                                              -base_avg, -base_price_distance, -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures),
                                     type="classification", ntree=50, mtry=10)
fit_test_years_gw$posterior_rf <- hyd_source_grf_years$votes[,2]

confusionMatrix(ifelse(fit_test_years_gw$posterior_rf < threshold, 0, 1) %>% as.factor,
                fit_test_years_gw$hyd_source %>% as.factor,
                positive="1")

hyd_source_map_data <- 
  ever_anecdotal_data_years %>% 
  select(id, hyd_source) %>% 
  left_join(municipio_centroid %>% select(id, long, lat)) %>% 
  mutate(posterior_glm=fit_test_years_gw$posterior_glm,
         posterior_rf=fit_test_years_gw$posterior_rf)

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

# ggsave("Colombia Data/Figs/prediction map/hyd sources regression with coords (2013-2016).png", hyd_regression_source_map, scale=1)
# ggsave("Colombia Data/Figs/prediction map/hyd sources rf with coords (2013-2016).png", hyd_rf_source_map, scale=1)

  # coef/importance bar plot
hyd_reg_coefs <- tibble(var=rep(names(hyd_source_gwr_years$coefficients[-1]),2),
                        coefficients=c(hyd_source_gwr_years$coefficients[-1], hyd_destination_gwr_years$coefficients[-1]),
                        response=c(rep("hyd_source", 21), rep("hyd_destination", 21))) %>% 
  mutate(var=as.factor(var),
         response=as.factor(response),
         # response=factor(response, levels=c("hyd_source", "hyd_destination")),
         significance=c(hyd_source_gwr_years$significance[-1], hyd_destination_gwr_years$significance[-1]))

hyd_reg_coefs %>% 
  ggplot() +
  geom_col(aes(x=var, y=coefficients, group=response, fill=response),
           position="dodge") +
  # scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2),
        legend.position = "bottom")

  # importance bar chart
hyd_rf_importances <- tibble(var=rep(rownames(hyd_source_grf_years$importance),2),
                             importance=c(hyd_source_grf_years$importance, hyd_destination_grf_years$importance),
                             response=c(rep("hyd_source", 19), rep("hyd_destination", 19))) %>% 
  mutate(var=as.factor(var),
         response=as.factor(response))

hyd_rf_importances %>% 
  ggplot() +
  geom_col(aes(x=var, y=importance, group=response, fill=response),
           position="dodge") +
  # scale_x_discrete(breaks=NULL) +
  labs(x="") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2),
        legend.position = "bottom")



## ggwr.basic experiment
gwr_hyd_destination_coord <- left_join(ever_anecdotal_data_years %>% 
                                         filter(year==2016) %>%
                                         select(id, year, n_PPI_labs:population, hyd_destination, -base_avg, -base_price_distance,
                                                -paste_avg, -paste_price_distance, -coca_seizures, -base_seizures, -base_group, -erad_aerial),
                                       municipio_centroid %>% select(id, long, lat), by="id") %>% select(-id, -year, -municipio)

gwr_hyd_destination_sp <- SpatialPointsDataFrame(gwr_hyd_destination_coord %>% select(long, lat),
                                                 gwr_hyd_destination_coord %>% select(-long, -lat))

bw_var <- bw.ggwr(hyd_destination~.,
                  family ="binomial",
                  dMat=gw.dist(dp.locat=coordinates(gwr_hyd_destination_sp)),
                  adaptive = F,
                  approach = "AIC",
                  data=gwr_hyd_destination_sp)
                  # data=gwr_hyd_destination_sp[, c(ncol(gwr_hyd_destination_sp), 1:2)])

hyd_destination_gwr <- ggwr.basic(hyd_destination~.,
                                  family ="binomial",
                                  bw=2.256702,
                                  data=gwr_hyd_destination_sp[, c(ncol(gwr_hyd_destination_sp), 1:2)])

formula=hyd_destination~.; data=gwr_hyd_destination_sp; family="binomial"; approach="AIC";
kernel="bisquare"; adaptive=FALSE; p=2; theta=0; longlat=F;
y <- data$hyd_destination
x <- as.matrix(data %>% select(-hyd_destination, -base_group, -long, -lat))

function (formula, data, family = "poisson", approach = "CV", 
          kernel = "bisquare", adaptive = FALSE, p = 2, theta = 0, 
          longlat = F, dMat) 
{
  if (is(data, "Spatial")) {
    dp.locat <- coordinates(data)
    data <- as(data, "data.frame")
  }
  else {
    stop("Given regression data must be Spatial*DataFrame")
  }
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.extract(mf, "response")
  x <- model.matrix(mt, mf)
  dp.n <- nrow(data)
  if (dp.n > 1500) {
    cat("Take a cup of tea and have a break, it will take a few minutes.\n")
    cat("          -----A kind suggestion from GWmodel development group\n")
  }
  if (missing(dMat)) 
    DM.given <- F
  else {
    DM.given <- T
    dim.dMat <- dim(dMat)
    if (dim.dMat[1] != dp.n || dim.dMat[2] != dp.n) 
      stop("Dimensions of dMat are not correct")
  }
  if (adaptive) {
    upper <- dp.n
    lower <- 20
  }
  else {
    if (DM.given) {
      upper <- range(dMat)[2]
      lower <- upper/5000
    }
    else {
      dMat <- NULL
      if (p == 2) {
        b.box <- bbox(dp.locat)
        upper <- sqrt((b.box[1, 2] - b.box[1, 1])^2 + 
                        (b.box[2, 2] - b.box[2, 1])^2)
        lower <- upper/5000
      }
      else {
        upper <- 0
        for (i in 1:dp.n) {
          dist.vi <- gw.dist(dp.locat = dp.locat, focus = i, 
                             p = p, theta = theta, longlat = longlat)
          upper <- max(upper, range(dist.vi)[2])
        }
        lower <- upper/5000
      }
    }
  }
  bw <- NA
  if (approach == "cv" || approach == "CV") 
    bw <- gold(ggwr.cv, lower, upper, adapt.bw = adaptive, 
               x, y, family = family, kernel, adaptive, dp.locat, 
               p, theta, longlat, dMat)
  else if (approach == "aic" || approach == "AIC" || approach == 
           "AICc") 
    bw <- gold(ggwr.aic, lower, upper, adapt.bw = adaptive, 
               x, y, family = family, kernel, adaptive, dp.locat, 
               p, theta, longlat, dMat)
  bw
}

xL=lower; xU=upper; adapt.bw=F

gold <- function (fun, xL, xU, adapt.bw = F, ...) 
{
  eps = 1e-04
  R <- (sqrt(5) - 1)/2
  iter <- 1
  d <- R * (xU - xL)
  if (adapt.bw) {
    x1 <- floor(xL + d)
    x2 <- round(xU - d)
  }
  else {
    x1 <- xL + d
    x2 <- xU - d
  }
  f1 <- eval(ggwr.aic(x1, ...))
  f2 <- eval(ggwr.aic(x2, ...))
  d1 <- f2 - f1
  if (f1 < f2) 
    xopt <- x1
  else xopt <- x2
  ea <- 100
  while ((abs(d) > eps) && (abs(d1) > eps)) {
    d <- R * d
    if (f1 < f2) {
      xL <- x2
      x2 <- x1
      if (adapt.bw) 
        x1 <- round(xL + d)
      else x1 <- xL + d
      f2 <- f1
      f1 <- eval(ggwr.aic(x1, ...))
    }
    else {
      xU <- x1
      x1 <- x2
      if (adapt.bw) 
        x2 <- floor(xU - d)
      else x2 <- xU - d
      f1 <- f2
      f2 <- eval(ggwr.aic(x2, ...))
    }
    iter <- iter + 1
    if (f1 < f2) 
      xopt <- x1
    else xopt <- x2
    d1 <- f2 - f1
  }
  xopt
}