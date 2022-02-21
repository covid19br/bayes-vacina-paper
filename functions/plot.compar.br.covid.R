require(tidyverse)
library(scales)
source('theme.publication.R')
Sys.setlocale('LC_TIME', locale='pt_BR.UTF-8')


gera_plot_brasil_en <- function(){
  Sys.setlocale('LC_TIME', locale='en_US.UTF-8')
  
  #####prepara dado
  df <- read_csv('output/counterfactual_shift_Brasilcovid.csv') %>%
    mutate(data='sragcovid')
  df <- read_csv('output/counterfactual_shift_Brasilob.covid.csv') %>%
    mutate(data='deathcovid') %>%
    bind_rows(df)
  df_cases_hosp <- df %>% filter(data == "sragcovid",type == "1month") %>% select(week, cases,agegroup)
  df_cases_death <- df %>% filter(data == "deathcovid",type == "1month") %>% select(week, cases,agegroup)
  
  dt.ini <- '2021-01-01'
  dt.max <- max(df$week)
  color_laranja <- c("#fdbe85","#fd8d3c","#e6550d","#a63603")
  cor_nova <- c("#DE4B07","#A299C8","#4B2887")
  cor_nova <- c("#DE4B07","#FCAA51","#A299C8","#4B2887")
  
  ######plot contrafato
  df %>%
    filter(!agegroup %in% c('30-39', '40-49'),
           week >= dt.ini,
           data=='sragcovid') %>% 
    # filter(type %in% c("fit","1month","2month"))%>%
    filter(type %in% c("fit","pred"))%>%
    
    mutate(type=factor(type,
                       levels=c('fit', 'pred','1month','2month'),
                       labels=c('Fitted Model', 'Projection without vaccination',
                                '4 weeks earlier', '8 weeks earlier'))) %>%
    # mutate(type=factor(type,
    # levels=c('fit', 'pred','1month','2month'),
    # labels=c('Realizada', 'Projeção sem vacinação',
    # '4 semanas mais cedo', '8 semanas mais cedo'))) %>%
    ggplot(aes(x=week, fill=type, color=type)) +
    
    geom_ribbon(aes(ymin=q025, ymax=q975), alpha=.6, size=0) +
    geom_ribbon(aes(ymin=q25, ymax=q75),alpha = 0.4,  size=0) +
    geom_line(aes(y=q50)) +
    geom_point(data = df_cases_hosp,aes(x = week, y=cases), color='black', inherit.aes = F) +
    # scale_color_manual(values=cor_nova, name='Vacinação') +
    # scale_fill_manual(values=cor_nova, name='Vacinação') +
    scale_color_manual(values=cor_nova, name='Vaccination') +
    scale_fill_manual(values=cor_nova, name='Vaccination') +
    # scale_pattern_manual(values=c('stripe', 'weave', 'wave')) +
    # scale_pattern_angle_manual(values=c(0, 45, 90), name = 'Vacinação')+
    labs(x='Epidemiological week of onset of symptoms', y='COVID-19 hospitalisations') +
    # labs(x='Semana epidemiológica de primeiros sintomas', y='Casos de SRAG\nconfirmados para COVID-19') +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='none',
          legend.direction = 'horizontal') + scale_x_date(date_breaks = "1 month",
                                                          label = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"),
                                                          limits = as.Date(c("2021-01-01","2021-09-01")))+
    facet_wrap(~agegroup, scale='free_y') +theme(plot.margin = margin(t = 50,b = 15,r = 5,l = 5,"pt"))
  ggsave(paste0('Plots/plot_casos_contrafato_brasil_en.png'),width = 12, height = 4, units = "in",dpi = 600)
  
  #### plot shift
  df %>%
    filter(!agegroup %in% c('30-39', '40-49'),
           week >= dt.ini,
           data=='deathcovid') %>% 
    filter(type %in% c("fit","pred"))%>%
    mutate(type=factor(type,
                       levels=c('fit', 'pred','1month','2month'),
                       labels=c('Fitted model', 'Projection without vaccination',
                                '4 weeks earlier', '8 weeks earlier'))) %>%
    # mutate(type=factor(type,
    #                    levels=c('fit', 'pred','1month','2month'),
    #                    labels=c('Realizada', 'Projeção sem vacinação',
    #                             '4 semanas mais cedo', '8 semanas mais cedo'))) %>%
    ggplot(aes(x=week, fill=type, color=type)) +
    geom_ribbon(aes(ymin=q025, ymax=q975), alpha=.6, size=0) +
    geom_ribbon(aes(ymin=q25, ymax=q75), alpha=.4, size=0) +
    geom_line(aes(y=q50)) +
    geom_point(data = df_cases_death,aes(x = week, y=cases), color='black', inherit.aes = F) +
    # scale_color_manual(values=colorblind_pal()(5)[2:5], name='Vacinação') +
    # scale_fill_manual(values=colorblind_pal()(5)[2:5], name='Vacinação') +
    scale_color_manual(values=cor_nova, name='Vaccination') +
    scale_fill_manual(values=cor_nova, name='Vaccination') +
    labs(x='Epidemiological week of death', y='COVID-19 deaths') +
    # labs(x='Semana epidemiológica do óbito', y='Óbitos por SRAG\nconfirmados para COVID-19') +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='bottom',
          legend.direction = 'horizontal') + scale_x_date(date_breaks = "1 month",
                                                          label = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"),
                                                          limits = as.Date(c("2021-01-01","2021-09-01")))+
    facet_wrap(~agegroup, scale='free_y') +theme(plot.margin = margin(t = 5,b = 5,r = 5,l = 5,"pt"))
  ggsave(paste0('Plots/plot_obitos_contrafato_brasil_en.png'),width = 12, height = 4, units = "in",dpi = 600)
}
gera_plot_brasil_shift_en <- function(){
  Sys.setlocale('LC_TIME', locale='en_US.UTF-8')
  
  #####prepara dado
  df <- read_csv('output/counterfactual_shift_Brasilcovid.csv') %>%
    mutate(data='sragcovid')
  df <- read_csv('output/counterfactual_shift_Brasilob.covid.csv') %>%
    mutate(data='deathcovid') %>%
    bind_rows(df)
  df_cases_hosp <- df %>% filter(data == "sragcovid",type == "1month") %>% select(week, cases,agegroup)
  df_cases_death <- df %>% filter(data == "deathcovid",type == "1month") %>% select(week, cases,agegroup)
  
  dt.ini <- '2021-01-01'
  dt.max <- max(df$week)
  color_laranja <- c("#fdbe85","#fd8d3c","#e6550d","#a63603")
  cor_nova <- c("#DE4B07","#A299C8","#4B2887")
  ######plot contrafato
  df %>%
    filter(!agegroup %in% c('30-39', '40-49'),
           week >= dt.ini,
           data=='sragcovid') %>% 
    # filter(type %in% c("fit","1month","2month"))%>%
    filter(type %in% c("fit","1month","2month"))%>%
    
    mutate(type=factor(type,
                       levels=c('fit', 'pred','1month','2month'),
                       labels=c('Fitted Model', 'Projection without vaccination',
                                '4 weeks earlier', '8 weeks earlier'))) %>%
    # mutate(type=factor(type,
    # levels=c('fit', 'pred','1month','2month'),
    # labels=c('Realizada', 'Projeção sem vacinação',
    # '4 semanas mais cedo', '8 semanas mais cedo'))) %>%
    ggplot(aes(x=week, fill=type, color=type)) +
    
    geom_ribbon(aes(ymin=q025, ymax=q975), alpha=.6, size=0) +
    geom_ribbon(aes(ymin=q25, ymax=q75),alpha = 0.4,  size=0) +
    geom_line(aes(y=q50)) +
    geom_point(data = df_cases_hosp,aes(x = week, y=cases), color='black', inherit.aes = F) +
    # scale_color_manual(values=cor_nova, name='Vacinação') +
    # scale_fill_manual(values=cor_nova, name='Vacinação') +
    scale_color_manual(values=cor_nova, name='Vaccination') +
    scale_fill_manual(values=cor_nova, name='Vaccination') +
    # scale_pattern_manual(values=c('stripe', 'weave', 'wave')) +
    # scale_pattern_angle_manual(values=c(0, 45, 90), name = 'Vacinação')+
    labs(x='Epidemiological week of onset of symptoms', y='COVID-19 hospitalisations') +
    # labs(x='Semana epidemiológica de primeiros sintomas', y='Casos de SRAG\nconfirmados para COVID-19') +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='none',
          legend.direction = 'horizontal') + scale_x_date(date_breaks = "1 month",
                                                          label = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"),
                                                          limits = as.Date(c("2021-01-01","2021-09-01")))+
    facet_wrap(~agegroup, scale='free_y') +theme(plot.margin = margin(t = 50,b = 15,r = 5,l = 5,"pt"))
  ggsave(paste0('Plots/plot_casos_shift_brasil_en.png'),width = 12, height = 4, units = "in",dpi = 600)
  
  #### plot shift
  df %>%
    filter(!agegroup %in% c('30-39', '40-49'),
           week >= dt.ini,
           data=='deathcovid') %>% 
    filter(type %in% c("fit","1month",'2month'))%>%
    mutate(type=factor(type,
                       levels=c('fit', 'pred','1month','2month'),
                       labels=c('Fitted model', 'Projection without vaccination',
                                '4 weeks earlier', '8 weeks earlier'))) %>%
    # mutate(type=factor(type,
    #                    levels=c('fit', 'pred','1month','2month'),
    #                    labels=c('Realizada', 'Projeção sem vacinação',
    #                             '4 semanas mais cedo', '8 semanas mais cedo'))) %>%
    ggplot(aes(x=week, fill=type, color=type)) +
    geom_ribbon(aes(ymin=q025, ymax=q975), alpha=.6, size=0) +
    geom_ribbon(aes(ymin=q25, ymax=q75), alpha=.4, size=0) +
    geom_line(aes(y=q50)) +
    geom_point(data = df_cases_death,aes(x = week, y=cases), color='black', inherit.aes = F) +
    # scale_color_manual(values=colorblind_pal()(5)[2:5], name='Vacinação') +
    # scale_fill_manual(values=colorblind_pal()(5)[2:5], name='Vacinação') +
    scale_color_manual(values=cor_nova, name='Vaccination') +
    scale_fill_manual(values=cor_nova, name='Vaccination') +
    labs(x='Epidemiological week of death', y='COVID-19 deaths') +
    # labs(x='Semana epidemiológica do óbito', y='Óbitos por SRAG\nconfirmados para COVID-19') +
    theme_Publication(base_size=16, base_family = 'Roboto') +
    theme(legend.position='bottom',
          legend.direction = 'horizontal') + scale_x_date(date_breaks = "1 month",
                                                          label = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"),
                                                          limits = as.Date(c("2021-01-01","2021-09-01")))+
    facet_wrap(~agegroup, scale='free_y') +theme(plot.margin = margin(t = 5,b = 5,r = 5,l = 5,"pt"))
  ggsave(paste0('Plots/plot_obitos_shift_brasil_en.png'),width = 12, height = 4, units = "in",dpi = 600)
}





