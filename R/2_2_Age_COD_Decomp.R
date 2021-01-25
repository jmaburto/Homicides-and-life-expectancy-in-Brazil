# Results at the national level
rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(DemoTools)
library(DemoDecomp)
library(parallelsugar)

#get data
source('R/1_Get Data.R')
source('R/Functions_Brazil.R')

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

Decomposition.results <- Data[, COD_Decomp_fun (.SD), by = list(region,state_code,state,sex)]

save(Decomposition.results,file = 'Data/Decomposition_Results.RData')


