# Results at the national level
rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(DemoTools)
library(DemoDecomp)
library(parallelsugar)
#library(parallelsugar)
#install.packages('snow')

setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Homicides-and-life-expectancy-in-Brazil/")

#get data
source('R/1_Get Data.R')
source('R/Robustness check for age below 75/Functions_Brazil_75.R')

Data      <- Data[order(region,state,year,sex,cause,age.group),]
Data$sex2 <- Data$sex

#unique(Data$cause)
#an example
#age.group   <- sort(unique(Data[year == 2000 & state == 'Acre' & sex == 'female',]$age.group))
#mxcvec      <- Data[year == 2000 & state == 'Acre' & sex == 'female',]$mx
#e0frommxc(mxcvec = mxcvec,sex = 'f')
  
#get decompositin of changes in life expectancy by age and cause of death

# subset data to make example for only one state
#m<- .SD <- Data2 <- Data[year %in% 2000:2003 & state == 'Acre' & sex == 'female',]
#x <- 2001
#Decomposition.results <- Data2[, COD_Decomp_fun (.SD), by = list(region,state_code,state,sex)]

Decomposition.results.75 <- Data[, COD_Decomp_fun (.SD), by = list(region,state_code,state,sex)]


save(Decomposition.results.75,file = 'Data/Decomposition_Results_75.RData')


