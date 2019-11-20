# Results at the national level
rm(list=ls(all=TRUE))

library(data.table)
library(reshape2)
library(DemoTools)

setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Homicides-and-life-expectancy-in-Brazil/")

source('R/1_Get Data.R')

source('R/Robustness check for age below 75/Functions_Brazil_75.R')

#Get life expectancy for every state
mx.total          <- Data[, list(mx = sum(mx)), by = list(year,region,state,state_code,sex,age.group)]

#mx <- mx.total[year == 2000 & state == 'Acre' & sex == 'female']$mx
#LTabr(nMx = mx,Age = seq(0,90,5),radix = 1,Sex = 'f')
#LifeExpectancy(mx = mx$mx,Sex = 'f')
#LT.Brazil.states  <- mx.total[,LTabr(nMx = mx,Age = age.group,radix = 1,Sex = ifelse(sex == 'female','f','m')), 
                              #by = list(year,region,state,state_code,sex)]


e75.Brazil.states  <-  mx.total[,LifeExpectancy_75(mx = mx,Age = age.group,Sex = ifelse(sex == 'female','f','m')), 
                               by = list(year,region,state,state_code,sex)]

e75.Brazil.states  <- e75.Brazil.states[order(region,state_code,sex,year),]

names(e75.Brazil.states)[6] <- 'life.expectancy'

save(e75.Brazil.states, file = 'Data/Check_LifeExpectancy75_Brazil03Nov2019.RData')
#chek life expectancy levels
#write.csv(e0.Brazil.states,file = 'Data/Check_LifeExpectancy_Brazil05Aug2019.csv')
 
