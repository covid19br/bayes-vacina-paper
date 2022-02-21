library(MASS)
library(coda)
library(zoo)
library(tidyverse)
library(lubridate)
library(plotly)
library(scales)
library(ggthemes)
library(beepr)
library(INLA)
library(stringr)
library(data.table)

# ###############################################################################
args = commandArgs(trailingOnly=TRUE)
state <- args[1]
outcome <- args[2]
# state = "SP"
# outcome = "covid"
###############################################################################
#######prepara dist etaria
age_reference <- 3
agegroups <- 7:9
dist_etaria <- read_csv("DATA/dist_etaria.csv")
dist_etaria <- dist_etaria %>% select(agegroup,state)
#######sivep
sivep <- read_csv("DATA/sum_dados_2021_10_18.csv.xz")
sivep <- sivep %>% 
  filter(sg_uf == state) %>% 
  select(c(dt_evento,age_class,covid,srag,ob.srag,ob.covid)) %>%
  complete(dt_evento,age_class,fill = list(covid = 0,srag = 0,ob.srag = 0,ob.covid = 0)) %>%
  group_by(dt_evento,age_class) %>%
  summarise(srag = sum(srag),covid = sum(covid),ob.covid = sum(ob.covid),ob.srag = sum(ob.srag)) %>%
  group_by(age_class) %>%
  mutate(srag = zoo::rollapply(srag,14,mean,align = 'right',fill = 0),
         covid = zoo::rollapply(covid,14,mean,align = 'right',fill = 0),
         ob.covid = zoo::rollapply(ob.covid,14,mean,align = 'right',fill = 0),
         ob.srag = zoo::rollapply(ob.srag,14,mean,align = 'right',fill = 0)) %>%
  ungroup() %>%
  mutate(week = floor_date(dt_evento,unit = "week")) %>%
  group_by(week,age_class) %>%
  summarise(srag = sum(srag),covid = sum(covid),ob.covid = sum(ob.covid),ob.srag = sum(ob.srag)) %>% 
  mutate(agegroup = as.numeric(str_extract(age_class,"\\d"))) %>% select(-age_class)

###### ajeita cobertura vacinal em semana epidemiologica

dados_vac <- read_csv(paste0("DATA/dados-vacinas/doses_estados/doses_aplicadas_",state,".csv"))
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
dados_vac2 <- dados_vac
dados_vac2$week  <- dados_vac2$week - 28
dados_vac3 <- dados_vac2
dados_vac3$week  <- dados_vac3$week - 28

colnames(dados_vac2)[3:6] <- paste0(colnames(dados_vac2)[3:6],'.2')
colnames(dados_vac3)[3:6] <- paste0(colnames(dados_vac3)[3:6],'.3')

dados_vac <- dados_vac %>% right_join(dados_vac2) %>% right_join(dados_vac3) %>% arrange(week)
dados_vac <- dados_vac %>% replace(is.na(.),0)

dados <- left_join(sivep,dados_vac)
dados <- dados %>% filter(week <= as.Date("2021-08-29"),week >= as.Date("2020-02-01"))

dados <- dados %>% replace(is.na(.),0)
colnames(dist_etaria)[2] <- "pop"
dados <- left_join(dados,dist_etaria)
# dados <- left_join(dados,dados_vac2)
dados <- dados %>% filter(agegroup > 0) %>% mutate(cobertura = (AZ + Coronavac+ Pfizer + Janssen)/pop,
                          AZ = AZ/pop,
                          Coronavac = Coronavac/pop,
                          Pfizer = Pfizer/pop,
                          Janssen = Janssen/pop,
                          AZ.2 = AZ.2/pop,
                          Coronavac.2 = Coronavac.2/pop,
                          Pfizer.2 = Pfizer.2/pop,
                          Janssen.2 = Janssen.2/pop,
                          AZ.3 = AZ.3/pop,
                          Coronavac.3 = Coronavac.3/pop,
                          Pfizer.3 = Pfizer.3/pop,
                          Janssen.3 = Janssen.3/pop)
# dados <- dados %>% mutate(cobertura = (AZ + Coronavac+ Pfizer + Janssen))
# dados_total <- left_join(dados_vac,dist_etaria)
# dados_total <- dados_total %>% mutate(cov = AZ + Coronavac + Pfizer + Janssen) %>%
  # select(-c(AZ,Coronavac,Pfizer,Janssen)) %>%
  # group_by(week) %>% summarize(cov = sum(cov)/sum(pop))
# dados <- left_join(dados, dados_total)
# dados$cov[is.na(dados$cov)] <- 0
###############################################################################
model_string <- "model <- outcome_old ~ 1 + f(week_young,model = 'ar1',hyper = list(prec = list(param = c(2,100)))) + outcome_young + outcome_young:Coronavac_old + outcome_young:Pfizer_old + outcome_young:AZ_old"
# model_string <- "model <- outcome_old ~ 1 + outcome_young + outcome_young*(cobertura_old) + cov_old"

model_string <- gsub("outcome",outcome,model_string)
eval(parse(text = model_string))
n_time <- length(unique(dados$week))
weeks <- unique(dados$week)
##############################################################################
sample_to_vector <- function(x, n.time = n_time, shift = F){
  values <- c()
  values <- c(values, rnorm(n = n.time,
                            mean = x$latent[1:n.time],
                            sd = 1/sqrt(x$hyperpar[1])))
  values <- c(values, rnorm(n = n.time,
                            mean = x$latent[(n.time + 1 ):(2*n.time)],
                            sd = 1/sqrt(x$hyperpar[1])))
  if(shift){
    values <- c(values, rnorm(n = n.time,
                              mean = x$latent[(2*n.time + 1 ):(3*n.time)],
                              sd = 1/sqrt(x$hyperpar[1])))
    values <- c(values, rnorm(n = n.time,
                              mean = x$latent[(3*n.time + 1 ):(4*n.time)],
                              sd = 1/sqrt(x$hyperpar[1])))
  }
  return(values)
}
gg.age <- function(x, agegroup,week = weeks,cases = NA, shift = T){
  n.time <- length(week)
  if(agegroup == 9){
    agegroup = "80+"
  } else{
    agegroup = paste0((agegroup-1)*10,"-",agegroup*10-1)
  }
  data.aux <- data.frame(predict = x)
  data.aux$agegroup <- agegroup
  if(shift){
    data.aux$type <- rep(c("fit","pred","1month","2month"),each = n.time)
    data.aux$week <- rep(week,4)
  }else{
    data.aux$type <- rep(c("fit","pred"),each = n.time)
    data.aux$week <- rep(week,2)
    
  }
  data.aux$cases <- cases 
  return(data.aux)
}
############################################################################
shift = T
all_pred <- c()
for(agegroup_ in agegroups){
  data_young <- dados %>% filter(agegroup == age_reference) %>% select(c(as.character(outcome)))
  data_old <- dados %>% filter(agegroup == agegroup_)%>% select(c(as.character(outcome),
                                                                  AZ,Coronavac,Pfizer,Janssen,
                                                                  AZ.2,Coronavac.2,Pfizer.2,Janssen.2,
                                                                  AZ.3,Coronavac.3,Pfizer.3,Janssen.3))
  # data_young$AZ <- data_young$AZ + runif(n_time,min = 0,max = 1e-9)
  # data_young$Coronavac <- data_young$Coronavac + runif(n_time,min = 0,max = 1e-9)
  # data_young$Pfizer <- data_young$Pfizer + runif(n_time,min = 0,max = 1e-9)
  # data_young$Janssen <- data_young$Janssen + runif(n_time,min = 0,max = 1e-9)
  # data_old$cov <- data_old$cov + runif(n_time,min = 0,max = 1e-9)
  # print(data_old$cov)
  cases_old <- as.numeric(data_old[[outcome]])
  colnames(data_young) <- paste0(colnames(data_young),"_young")
  colnames(data_old) <- paste0(colnames(data_old),"_old")
  dataset <- cbind(data_young,data_old)
  dataset$week_young<- 1:length(dataset$week_young)
  dataset2 <- dataset
  dataset2 <- dataset2 %>% mutate(AZ_old = 0,
                                  Pfizer_old = 0,
                                  Coronavac_old = 0,
                                  Janssen_old = 0)
  dataset2[,paste0(outcome,'_old')] <- NA
  if(shift){
  dataset3 <- dataset
  dataset4 <- dataset
  dataset <- rbind(dataset,dataset2)
  dataset3 <- dataset3 %>% mutate(AZ_old = AZ.2_old,
                                  Pfizer_old = Pfizer.2_old,
                                  Coronavac_old = Coronavac.2_old,
                                  Janssen_old = Janssen.2_old)
  dataset4 <- dataset4 %>% mutate(AZ_old = AZ.3_old,
                                  Pfizer_old = Pfizer.3_old,
                                  Coronavac_old = Coronavac.3_old,
                                  Janssen_old = Janssen.3_old)

  dataset3[,paste0(outcome,'_old')] <- NA
  dataset4[,paste0(outcome,'_old')] <- NA
  dataset <- rbind(dataset,dataset3)
  dataset <- rbind(dataset,dataset4)
  } else{
    dataset <- rbind(dataset,dataset2)
  }
  output <- inla(formula = model, family = "gaussian",data = dataset, 
                 control.compute = list(config = TRUE,dic = T), 
                 control.predictor = list(compute = T)
  )
  samples <- inla.posterior.sample(n = 1000, output)
  vector.samples0 <- lapply(X = samples, FUN = sample_to_vector,shift = T)
  tibble.samples.0 <- lapply( X = vector.samples0,
                              FUN = gg.age,agegroup = agegroup_, cases = cases_old,shift = T)
  pred <- bind_rows(tibble.samples.0, .id = "sample")
  all_pred <- rbind(all_pred,pred)
  
}
pred_summary <-all_pred %>% group_by(agegroup,type,week) %>%
  summarize(mean = mean(predict),
            q025 = quantile(predict,0.025),
            q25 = quantile(predict,0.25),
            q50 = quantile(predict,0.5),
            q75 = quantile(predict,0.75),
            q975 = quantile(predict,0.975),
            cases = mean(cases),.groups ="drop") %>% arrange(agegroup,type,week)
write_csv(all_pred,paste0("output/bruto/counterfactual_bruto_shift",state,"_",outcome,".csv"))
write_csv(pred_summary,paste0("output/counterfactual_shift",state,"_",outcome,".csv"))
