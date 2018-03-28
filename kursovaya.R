library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
setwd ("D:/InfTeh") 
dx = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),comment =c("["))
##удаляем лишние столбцы
dx = select(dx, -(roll)) 
dx = dx[,c(--1,-2,-3,-30,-35,-70,-88:-99)] 
names(dx) 
##выбираем летний сезон и ночное время суток
dx<-dx[dx$DOY>152 & dx$DOY<244 & dx$daytime == FALSE, c(1:ncol(dx))] 
dx = dx %>% mutate_if(is.character, factor) 
names(dx) = str_replace_all(names(dx), "[!]","_emph_") 
names(dx) = names(dx) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(dx) 
##корреляция
sapply(dx,is.numeric) 
dx_numeric = dx[,sapply(dx,is.numeric) ] 
dx_nonnumeric = dx[,!sapply(dx,is.numeric) ] 
cor_td = cor(dx_numeric) 
cor_td 
cor_td = cor(drop_na(dx_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula
row_numbers = 1:length(dx$date) 
teach = sample(row_numbers, floor(length(dx$date)*.7)) 
test = row_numbers[-teach] 
teaching_dx_unq = dx[teach,] 
testing_dx_unq = dx[test,] 

model1 = lm(h2o_flux ~ (Tau + rand_err_Tau + LE + qc_LE + rand_err_LE + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es + RH + VPD + max_speed + TKE + un_Tau + un_LE + un_h2o_flux + w_var + h2o_var)^2, data = dx)
model1
coef(model1) 
resid(model1) 
confint(model1) 
summary(model1)
anova(model1)
model2=lm(h2o_flux ~ (LE + RH + VPD + un_LE + un_h2o_flux + h2o_var) ^2, data = dx)
model2
anova(model2)
summary(model2)
model3=lm(h2o_flux ~ LE + RH + un_LE + un_h2o_flux + LE:RH +LE:VPD + LE:h2o_var + RH:VPD+ VPD:un_LE + VPD:un_h2o_flux + un_LE:un_h2o_flux + un_LE:h2o_var + un_h2o_flux:h2o_var, data = dx)                 
model3     
anova(model3)
summary(model3)
model4=lm(h2o_flux ~ LE + RH + un_LE + un_h2o_flux + LE:RH +LE:VPD + RH:VPD + VPD:un_LE +VPD:un_h2o_flux, data = dx)  
anova(model4)
summary(model4)
plot(model4)