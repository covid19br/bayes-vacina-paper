library(tidyverse)
library(zoo)
library(lubridate)
source("theme.publication.R")

gera_dado_cobertura <- function(state){
  dist_etaria <- read_csv("DATA/dist_etaria.csv")
  dist_etaria <- dist_etaria %>% select(agegroup,state)
  colnames(dist_etaria)[2] <- "pop"
  
  
  dados_vac <- read_csv(paste0("../dados-vacinas/doses_estados/doses_aplicadas_",state,".csv"))
  dados_vac <- dados_vac[!is.na(dados_vac$data),]
  dados_vac <- dados_vac[!is.na(dados_vac$agegroup),]
  dados_vac <- dados_vac %>% filter(doses %in% c("D2","DU")) %>%
    mutate(agegroup = ifelse(agegroup >= 9,9,agegroup))%>%
    group_by(vacina,agegroup,data) %>% summarise(n = sum(n)) %>%
    pivot_wider(names_from = vacina,values_from = n) %>%
    replace(is.na(.),0) %>% ungroup() %>%
    complete(data,agegroup = 1:9,fill = list(AZ = 0, Coronavac = 0, Pfizer = 0, Janssen = 0)) %>%
    arrange(data, agegroup) %>%
    group_by(agegroup) %>%
    mutate(AZ = cumsum(AZ),
           Coronavac = cumsum(Coronavac),
           Janssen = cumsum(Janssen),
           Pfizer = cumsum(Pfizer)) %>%
    group_by(agegroup) %>%
    mutate(AZ = zoo::rollapply(AZ,14,mean,align = 'right',fill = 0),
           Coronavac = zoo::rollapply(Coronavac,14,mean,align = 'right',fill = 0),
           Pfizer = zoo::rollapply(Pfizer,14,mean,align = 'right',fill = 0),
           Janssen = zoo::rollapply(Janssen,14,mean,align = 'right',fill = 0)) %>%
    ungroup() %>% arrange(data,agegroup) %>%
    mutate(week = floor_date(data,unit = "week")) %>%
    group_by(week,agegroup) %>%
    summarise(AZ = min(AZ),Coronavac = min(Coronavac),Pfizer = min(Pfizer),Janssen = min(Janssen))%>%
    mutate(week = as.Date(as.numeric(week)+14)) #####shifting values by 14 weeks to complete vaccination
  dados_vac <- left_join(dados_vac, dist_etaria)
  dados_vac %>% mutate(AZ = AZ/pop, Coronavac = Coronavac/pop, Janssen = Janssen/pop, Pfizer = Pfizer/pop) -> dados_vac
  dados_vac %>% group_by(week, agegroup) %>% summarize(cov = AZ+Coronavac+Pfizer+Janssen) -> dados_vac
  dados_vac %>% mutate(q25 = cov > 0.25, q50 = cov > 0.5, q75 = cov > 0.75) -> dados_vac
  dados_vac %>% filter(week <= as.Date("2021-08-29")) -> dados_vac
  all_df <- c()
  for(age in 1:9){
    df <- dados_vac %>% filter(agegroup == age)
    week25 <- df[df$q25,]
    week25 <- min(week25$week, na.rm = T)
    week50 <- df[df$q50,]
    week50 <- min(week50$week, na.rm = T)
    week75 <- df[df$q75,]
    week75 <- min(week75$week, na.rm = T)
    # week50 <- df[df$q50,]$week
    # week50 <- min(week50)
    # week75 <- df[df$q75,]$week
    # week75 <- min(week75)
    res <- data.frame(agegroup = age, week25, week50, week75)
    all_df <- rbind(all_df, res)

  }
  all_df$state <- state
  return(all_df)
}

