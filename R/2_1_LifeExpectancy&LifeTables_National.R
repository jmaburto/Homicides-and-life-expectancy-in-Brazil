# Results at the national level
rm(list=ls(all=TRUE))

library(data.table)
library(reshape2)
library(DemoTools)

Data <- data.table(read.csv(file = 'Data/Homicide Database Brazil July302019.csv', sep = '',header = T,stringsAsFactors = F ))
Data[,new.deaths:= rate_corr*pop]
Data <- Data[Data$cause == 'total', c(4,5,6,7,8,13)]
names(Data)[3] <- 'age.group'
names(Data)[6] <- 'deaths'
names(Data)[2] <- 'sex'

source('R/Functions_Brazil.R')

Brazil.national <- Data[,list(Dx= sum(deaths),Nx = sum(pop)), by = list(sex,year,age.group)]

Brazil.national[,mx:=Dx/Nx]

Brazil.national <- Brazil.national[order(sex,year,age.group)]


e0.Brazil.national  <-  Brazil.national[,LifeExpectancy(mx = mx,Age = age.group,Sex = ifelse(sex == 'female','f','m')), 
                               by = list(year,sex)]

