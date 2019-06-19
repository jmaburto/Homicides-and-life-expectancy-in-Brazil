# Results at the national level
rm(list=ls(all=TRUE))

library(data.table)
library(reshape2)
library(DemoTools)

setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Homicides-and-life-expectancy-in-Brazil/")

source('R/1_Get Data.R')
source('R/Functions_Brazil.R')

#Get life expectancy for every state
mx.total          <- Data[, list(mx = sum(mx)), by = list(year,region,state,state_code,sex,age.group)]

#mx <- mx.total[year == 2000 & state == 'Acre' & sex == 'female']
#LTabr(nMx = mx,Age = seq(0,90,5),radix = 1,Sex = 'f')
#LifeExpectancy(mx = mx$mx,Sex = 'f')
#LT.Brazil.states  <- mx.total[,LTabr(nMx = mx,Age = age.group,radix = 1,Sex = ifelse(sex == 'female','f','m')), 
                              #by = list(year,region,state,state_code,sex)]


e0.Brazil.states  <-  mx.total[,LifeExpectancy(mx = mx,Age = age.group,Sex = ifelse(sex == 'female','f','m')), 
                               by = list(year,region,state,state_code,sex)]

e0.Brazil.states  <- e0.Brazil.states[order(region,state_code,sex,year),]

save(e0.Brazil.states, file = 'Data/Check_LifeExpectancy_Brazil190619.RData')
#chek life expectancy levels
#write.csv(e0.Brazil.states,file = 'Data/Check_LifeExpectancy_Brazil.csv')
 