library(tidyverse)
library(ggridges)
source("theme.publication.R")
estados <- read_csv("DATA/dist_etaria.csv")
estados <- colnames(estados)[-1]
# estados <- "Brasil"
outcome <- c("ob.covid","covid")
cor_nova <- c("#DE4B07","#A299C8","#4B2887")

gera_distr_brasil_en <- function(){
  df <- read_csv(paste0("output/bruto/counterfactual_bruto_shift_","Brasil","covid.csv")) %>%
    mutate(data='sragcovid')
  df <- read_csv(paste0("output/bruto/counterfactual_bruto_shift_","Brasil","ob.covid.csv")) %>%
    mutate(data='deathcovid') %>%
    bind_rows(df)
  dt.ini <- '2021-01-14'
  dt.max <- max(df$week)
  dados <- df %>% filter(agegroup %in% c("50-59","60-69","70-79","80+")) %>% filter(week >= dt.ini)
  
  dados2<- dados %>% pivot_wider(names_from = "type",values_from = "predict")
  dados2 <- dados2 %>% mutate(diff = pred - fit, diff_1month  = pred - `1month`,diff_2month = pred - `2month` )
  dados3 <- dados2 %>% group_by(sample,agegroup,data) %>% summarize(diff_cum = sum(diff),
                                                                    diff_cum_1month = sum(diff_1month),
                                                                    diff_cum_2month = sum(diff_2month))
  dados3 <- dados3 %>% pivot_longer(c(diff_cum,diff_cum_1month,diff_cum_2month),names_to = 'diff_type',values_to = "diff_cum")
  dados3 %>% filter(data == "sragcovid")%>% 
    mutate(diff_type=factor(diff_type,
                                                               levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
                                                               labels=c('Realized', '4 weeks earlier','8 weeks earlier')))%>%
    # mutate(diff_type=factor(diff_type,
    #                         levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
    #                         labels=c('Realizada', '4 semanas mais cedo','8 semanas mais cedo')))%>%
    ggplot(aes(x=diff_cum, y = agegroup, fill=diff_type)) +
    geom_density_ridges(alpha = 0.8)+
    # scale_color_manual(values=colorblind_pal()(3)[2:3], name='') +
    scale_fill_manual(values=cor_nova, name='Vaccination')+
    # scale_fill_manual(values=cor_nova, name='Vacinação')+
    labs(x='Estimate of potentially averted\nCOVID-19 hospitalisations', y='Age group') +
    # labs(x='Estimativa de casos de SRAG\npor COVID-19 potencialmente evitados', y='Faixa etária') +
    
    theme_Publication(base_size=18, base_family = 'Roboto')+
    theme(legend.position='bottom',
          legend.direction = 'horizontal') + theme(plot.margin = margin(6, 12,6,6,"pt"))
  ggsave(paste0('Plots/plot_casos_evitados_brasil_en.png'),height = 6, width = 8, units = 'in',dpi = 600)
  
  
  
  dados3 %>% filter(data == "deathcovid")%>% 
    mutate(diff_type=factor(diff_type,
          levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
          labels=c('Realized', '4 weeks earlier','8 weeks earlier')))%>%
    # mutate(diff_type=factor(diff_type,
    #                         levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
    #                         labels=c('Realizada', '4 semanas mais cedo','8 semanas mais cedo')))%>%
    ggplot(aes(x=diff_cum, y = agegroup, fill=diff_type)) +
    geom_density_ridges(alpha = 0.8)+
    # scale_color_manual(values=colorblind_pal()(3)[2:3], name='') +
    scale_fill_manual(values=cor_nova, name='Vaccination')+
    labs(x='Estimate of potentially averted\nCOVID-19 deaths', y='Age group') +
    # labs(x='Estimativa de óbitos por SRAG confirmados\npara COVID-19 potencialmente evitados', y='Faixa etária') +
    
    theme_Publication(base_size=18, base_family = 'Roboto')+
    theme(legend.position='bottom',
          legend.direction = 'horizontal') + theme(plot.margin = margin(6, 12,6,6,"pt"))
  ggsave(paste0('Plots/plot_obitos_evitados_brasil_en.png'),height = 6, width = 8, units = 'in',dpi = 600)
}
gera_distr_estados_en <- function(){
  for(estado in estados){
    df <- read_csv(paste0("output/bruto/counterfactual_bruto_shift",estado,"_covid.csv")) %>%
      mutate(data='sragcovid')
    df <- read_csv(paste0("output/bruto/counterfactual_bruto_shift",estado,"_ob.covid.csv")) %>%
      mutate(data='deathcovid') %>%
      bind_rows(df)
    dt.ini <- '2021-01-14'
    dt.max <- max(df$week)
    dados <- df %>% filter(agegroup %in% c("50-59","60-69","70-79","80+")) %>% filter(week >= dt.ini)
    
    dados2<- dados %>% pivot_wider(names_from = "type",values_from = "predict")
    dados2 <- dados2 %>% mutate(diff = pred - fit, diff_1month  = pred - `1month`,diff_2month = pred - `2month` )
    dados3 <- dados2 %>% group_by(sample,agegroup,data) %>% summarize(diff_cum = sum(diff),
                                                                      diff_cum_1month = sum(diff_1month),
                                                                      diff_cum_2month = sum(diff_2month))
    dados3 <- dados3 %>% pivot_longer(c(diff_cum,diff_cum_1month,diff_cum_2month),names_to = 'diff_type',values_to = "diff_cum")
    dados3 %>% filter(data == "sragcovid")%>% 
      mutate(diff_type=factor(diff_type,
                                                                 levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
                                                                 labels=c('Realized', '4 weeks earlier','8 weeks earlier')))%>%
      # mutate(diff_type=factor(diff_type,
      #                         levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
      #                         labels=c('Realizada', '4 semanas mais cedo','8 semanas mais cedo')))%>%
      ggplot(aes(x=diff_cum, y = agegroup, fill=diff_type)) +
      geom_density_ridges(alpha = 0.8)+
      # scale_color_manual(values=colorblind_pal()(3)[2:3], name='') +
      scale_fill_manual(values=cor_nova, name='Vaccination')+
      # scale_fill_manual(values=cor_nova, name='Vacinação')+
      labs(x='Estimate of potentially averted\nCOVID-19 hospitalisations', y='Age group') +
      # labs(x='Estimativa de casos de SRAG\npor COVID-19 potencialmente evitados', y='Faixa etária') +
      theme_Publication(base_size=18, base_family = 'Roboto')+ggtitle(estado)+
      theme(legend.position='bottom',
            legend.direction = 'horizontal') + theme(plot.margin = margin(6, 12,6,6,"pt"))
    ggsave(paste0('Plots/plot_casos_evitados_',estado,'.png'),height = 6, width = 8, units = 'in',dpi = 600)
    dados3 %>% filter(data == "deathcovid")%>% 
      mutate(diff_type=factor(diff_type,
            levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
            labels=c('Realized', '4 weeks earlier','8 weeks earlier')))%>%
      # mutate(diff_type=factor(diff_type,
      #                         levels=c('diff_cum', 'diff_cum_1month',"diff_cum_2month"),
      #                         labels=c('Realizada', '4 semanas mais cedo','8 semanas mais cedo')))%>%
      ggplot(aes(x=diff_cum, y = agegroup, fill=diff_type)) +
      geom_density_ridges(alpha = 0.8)+
      # scale_color_manual(values=colorblind_pal()(3)[2:3], name='') +
      scale_fill_manual(values=cor_nova, name='Vaccination')+
      labs(x='Estimate of potentially averted\nCOVID-19 deaths', y='Age group') +
      # labs(x='Estimativa de óbitos por SRAG confirmados\npara COVID-19 potencialmente evitados', y='Faixa etária') +
      
      theme_Publication(base_size=18, base_family = 'Roboto')+ggtitle(estado)+
      theme(legend.position='bottom',
            legend.direction = 'horizontal') + theme(plot.margin = margin(6, 12,6,6,"pt"))
    ggsave(paste0('Plots/plot_obitos_evitados_',estado,'.png'),height = 6, width = 8, units = 'in',dpi = 600)
    
  }
}
  # outcome <- "ob.covid"



# dados3 %>%
#   ggplot(aes(x=diff_cum, y = agegroup, fill=agegroup)) +
# geom_density_ridges()+
#   # scale_color_manual(values=colorblind_pal()(3)[2:3], name='') +
#   scale_fill_manual(values=colorblind_pal()(5)[2:5], name='Faixa etária')+
#   labs(x='Estimativa de Óbitos de SRAG por COVID-19 evitados', y='Faixa etária') +
#   theme_Publication(base_size=16, base_family = 'Roboto') +
#     theme(legend.position='none',
#         legend.direction = 'horizontal') +ggtitle("Brasil")
# ggsave(paste0('Plots/srag.ob.covid.evitados.brasil.',dt.ini, '.', dt.max, '.png'))
# 
#   # facet_wrap(~agegroup, scale='free_y')+ggtitle("Brasil")



