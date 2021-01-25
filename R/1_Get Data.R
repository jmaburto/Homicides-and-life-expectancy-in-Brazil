#################################################################################
# Program to convert to get age specific mortality rates fro brazil and its #####
# states                            #############################################
# Data comes from National Institute of Statistics ##############################
# Corrected by Bernardo and Julia ###############################################                  
#################################################################################

rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)

#source('R/Funcs_1.R')
Data <- data.table(read.csv(file = 'Data/Homicide Database Brazil July302019.csv', sep = '',header = T,stringsAsFactors = F ))
#Total.mx.check <- Data[Data$cause == 'total' & Data$state_code == 12 &Data$sex == 'female' & Data$grupid == 0 & Data$year %in% 2000:2015, c(1:9,12)] 
Data <- Data[Data$cause != 'total', c(1:8,13,12)]
names(Data)[6] <- 'age.group'
names(Data)[10] <- 'mx'
names(Data)[9] <- 'deaths'
names(Data)[5] <- 'sex'

Data <- Data[order(region,state,sex,year,cause,age.group),]


#check totals
#mx.total          <- Data[, list(mx = sum(mx)), by = list(year,region,state,state_code,sex,age.group)]
#mx.total[mx.total$age.group == 0 & mx.total$state_code == 12 &mx.total$sex == 'female',]
#Total.mx.check$rate_corr
#mx.total[mx.total$age.group == 0 & mx.total$state_code == 12 &mx.total$sex == 'female',]$mx

gdata::keep(Data, sure = T)

