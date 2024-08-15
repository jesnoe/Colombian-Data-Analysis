municipios_capital %>% filter(id %in% c(5400, 5642, 94888))

gwr_result_list$id_5400$bw_0.6$data$hyd_destination
gwr_result_list$id_5642$bw_0.7$data$hyd_destination
gwr_result_list$id_94888$bw_4$data$hyd_destination

gwr_result_list$id_5400$bw_0.6 %>% summary
gwr_result_list$id_5642$bw_0.7 %>% summary
gwr_result_list$id_94888$bw_4 %>% summary

# gwr_result_list$id_5400$bw_0.6$data %>% write.csv("Colombia Data/local gwr data id=5400 bw=0.6.csv", row.names=F)
# gwr_result_list$id_5642$bw_0.7$data %>% write.csv("Colombia Data/local gwr data id=5642 bw=0.7.csv", row.names=F)
# gwr_result_list$id_94888$bw_4$data %>% write.csv("Colombia Data/local gwr data id=94888 bw=4.csv", row.names=F)

glm(hyd_destination~.,
    data=gwr_result_list$id_5400$bw_0.6$data %>% select(-coca_area, -n_hyd_labs, -hyd_group),
    family="binomial") %>% summary
glm(hyd_destination~.,
    data=gwr_result_list$id_5642$bw_0.7$data %>% select(-coca_area, -hyd_avg, -n_hyd_labs, -erad_manual, -n_armed_groups),
    family="binomial") %>% summary
glm(hyd_destination~.,
    data=gwr_result_list$id_94888$bw_4$data %>% select(-coca_area, -hyd_avg, -n_hyd_labs, -erad_manual, -n_armed_groups),
    family="binomial") %>% summary

cor(gwr_result_list$id_5642$bw_0.7$data %>% select(-hyd_destination))
cor(gwr_result_list$id_5642$bw_0.7$data %>% select(-coca_area, -hyd_avg, -erad_manual, -population, -hyd_group, -hyd_destination))
  
# library(MASS)
gwr_model_5400 <- glm(hyd_destination~.,
                      data=gwr_result_list$id_5400$bw_0.6$data,
                      family=binomial)
gwr_model_5642 <- glm(hyd_destination~.,
                      data=gwr_result_list$id_5642$bw_0.7$data,
                      family=binomial)
gwr_model_94888 <- glm(hyd_destination~.,
                      data=gwr_result_list$id_94888$bw_4$data,
                      family=binomial)
# [gwr_result_list$id_5400$bw_0.6$coefficients]
id_5400_step_result <- MASS::stepAIC(gwr_model_5400, trace=F, direction="both")
id_5400_step_result %>% summary

id_5642_step_result <- MASS::stepAIC(gwr_model_5642, trace=F, direction="both")
id_5642_step_result %>% summary

id_94888_step_result <- MASS::stepAIC(gwr_model_94888, trace=F, direction="both")
id_94888_step_result %>% summary
