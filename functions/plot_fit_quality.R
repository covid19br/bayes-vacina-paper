library(tidyverse)
library(ggridges)
source("theme.publication.R")
estados <- read_csv("DATA/dist_etaria.csv")
estados <- colnames(estados)[-1]
# estados <- "Brasil"
outcome <- c("ob.covid","covid")
# outcome <- "ob.covid"
all_data <- c()
for(estado in estados){
  df <- read_csv(paste0("output/bruto/counterfactual_bruto_shift",estado,"_covid.csv")) %>%
    mutate(data='sragcovid')
  df <- read_csv(paste0("output/bruto/counterfactual_bruto_shift",estado,"_ob.covid.csv")) %>%
    mutate(data='deathcovid') %>%
    bind_rows(df)
  dt.ini <- '2021-01-14'
  dt.max <- max(df$week)
  dados <- df %>% filter(agegroup %in% c("50-59","60-69","70-79","80+"), type == "fit")
  

  dados2 <- dados %>%group_by(sample, agegroup, data) %>% summarize(cases = sum(cases), predict = sum(predict))
  dados3 <- dados2 %>% group_by(agegroup, data) %>% summarize(cases = max(cases),
                                                              predict.mean = mean(predict),
                                                              predict.q025 = quantile(predict, 0.025),
                                                              predict.q50 = quantile(predict, 0.50),
                                                              predict.q975 = quantile(predict, 0.975)) %>% mutate(estado = estado)
  all_data <- rbind(all_data, dados3)
  }
all_data %>% mutate(data = if_else(data == "sragcovid","COVID-19 confirmed SARI cases","COVID-19 confirmed SARI deaths")) %>% 
  ggplot(aes(x = cases, y = predict.mean,color = estado,linetype = agegroup, shape = agegroup)) + 
  facet_wrap(~data, scales = "free") + geom_abline(slope = 1, intercept = 0)+ geom_point() + 
  scale_color_viridis_d(name = "State") +
  scale_linetype(name = "Age group")+ labs(x = "Observed values", y = "Estimated values")+
  scale_shape(name = "Age group")+
  geom_errorbar(aes(ymin = predict.q025, ymax = predict.q975)) +theme_Publication(base_size = 16)+
  theme(plot.margin = margin(10, 10,10,10,"pt"))
ggsave("Plots/plot_fitting_quality.png",width = 12, height = 6, units = "in",dpi = 600)
  