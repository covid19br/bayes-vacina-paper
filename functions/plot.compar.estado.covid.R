require(tidyverse)
source('theme.publication.R')
Sys.setlocale('LC_TIME', locale='en_US.UTF-8')
# args = commandArgs(trailingOnly=TRUE)
estados <- read_csv("DATA/dist_etaria.csv")
estados <- colnames(estados)[-1]
# state <- args[1]
# estado <- args[1]
cor_nova <- c("#FCAA51","#DE4B07","#A299C8","#4B2887")

for(estado in estados){
  df <- read_csv(paste0('output/counterfactual_shift',estado,'_covid.csv')) %>%
    mutate(data='sragcovid')
  df <- read_csv(paste0('output/counterfactual_shift',estado,'_ob.covid.csv')) %>%
    mutate(data='deathcovid') %>%
    bind_rows(df)
  
  dt.ini <- '2021-01-01'
  dt.max <- max(df$week)
  df_cases_hosp <- df %>% filter(data == "sragcovid",type == "1month") %>% select(week, cases,agegroup)
  df_cases_death <- df %>% filter(data == "deathcovid",type == "1month") %>% select(week, cases,agegroup)
  df %>%
    filter(!agegroup %in% c('30-39', '40-49'),
           week >= dt.ini,
           data=='sragcovid') %>%
    mutate(type=factor(type,
                       levels=c('pred', 'fit','1month','2month'),
                       labels=c('Projection without vaccination','Fitted model',
                                '4 weeks earlier', '8 weeks earlier'))) %>%
    ggplot(aes(x=week, fill=type, color=type)) +
    geom_ribbon(aes(ymin=q025, ymax=q975), alpha=.6, size=0) +
    geom_ribbon(aes(ymin=q25, ymax=q75), alpha=.4, size=0) +
    geom_line(aes(y=q50)) +
    geom_point(data = df_cases_hosp,aes(x = week, y=cases), color='black', inherit.aes = F) +
    scale_color_manual(values=cor_nova, name='') +
    scale_fill_manual(values=cor_nova, name='') +
    labs(x='Epidemiological week of onset of symptoms', y='COVID-19 hospitalisations') +
    theme_Publication(base_size=16, base_family = 'Roboto') + ggtitle(estado)+
    theme(legend.position='none',
          legend.direction = 'horizontal') + scale_x_date(date_breaks = "1 month",
                                                          label = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"),
                                                          limits = as.Date(c("2021-01-01","2021-09-01")))+
    facet_wrap(~agegroup, scale='free_y') +theme(plot.margin = margin(t = 5,b = 50,r = 5,l = 5,"pt"))
  ggsave(paste0('Plots/srag.covid.',estado,'.',dt.ini, '.', dt.max, '.png'),width = 12, height = 4, units = "in",dpi = 600)
  
  df %>%
    filter(!agegroup %in% c('30-39', '40-49'),
           week >= dt.ini,
           data=='deathcovid') %>%
    mutate(type=factor(type,
                       levels=c('pred', 'fit','1month','2month'),
                       labels=c('Projection without vaccination','Fitted model',
                                '4 weeks earlier', '8 weeks earlier'))) %>%
    ggplot(aes(x=week, fill=type, color=type)) +
    geom_ribbon(aes(ymin=q025, ymax=q975), alpha=.6, size=0) +
    geom_ribbon(aes(ymin=q25, ymax=q75), alpha=.4, size=0) +
    geom_line(aes(y=q50)) +
    geom_point(data = df_cases_death,aes(x = week, y=cases), color='black', inherit.aes = F) +
    scale_color_manual(values=cor_nova, name='') +
    scale_fill_manual(values=cor_nova, name='') +
    labs(x='Epidemiological week of death', y='COVID-19 deaths') +
    theme_Publication(base_size=16, base_family = 'Roboto') +ggtitle(estado)+
    theme(legend.position='bottom',
          legend.direction = 'horizontal') + scale_x_date(date_breaks = "1 month",
                                                          label = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"),
                                                          limits = as.Date(c("2021-01-01","2021-09-01")))+
    facet_wrap(~agegroup, scale='free_y') +theme(plot.margin = margin(t = 5,b = 0,r = 5,l = 5,"pt"))
  ggsave(paste0('Plots/srag.ob.covid.',estado,'.',dt.ini, '.', dt.max, '.png'),width = 12, height = 4, units = "in",dpi = 600)
}
