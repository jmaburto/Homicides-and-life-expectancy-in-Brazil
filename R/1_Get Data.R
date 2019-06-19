#################################################################################
# Program to convert to get age specific mortality rates fro brazil and its #####
# states                            #############################################
# Data comes from National Institute of Statistics ##############################
# Corrected by Bernardo and Julia ###############################################                  
#################################################################################

rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Homicides-and-life-expectancy-in-Brazil/")


#source('R/Funcs_1.R')
Data <- data.table(read.csv(file = 'Data/Homicide Brazil 18062019.csv', sep = '',header = T,stringsAsFactors = F ))
#Total.mx.check <- Data[Data$cause == 'total' & Data$state_code == 12 &Data$sex == 'female' & Data$grupid == 0 & Data$year %in% 2000:2015, c(1:9,14)] 
Data <- Data[Data$cause != 'total', c(1:9,12)]
names(Data)[6] <- 'age.group'
names(Data)[10] <- 'mx'

Data <- Data[order(region,state,sex,year,cause,age.group),]


#check totals
#mx.total          <- Data[, list(mx = sum(mx)), by = list(year,region,state,state_code,sex,age.group)]
#mx.total[mx.total$age.group == 0 & mx.total$state_code == 12 &mx.total$sex == 'female',]
#Total.mx.check$rate_corr2
#mx.total[mx.total$age.group == 0 & mx.total$state_code == 12 &mx.total$sex == 'female',]$mx

#gdata::keep(Data, sure = T)


# 
# unique(Data$cause)
# unique(Data$year)
# 







