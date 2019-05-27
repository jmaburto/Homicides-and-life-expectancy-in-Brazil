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

source('R/Funcs_1.R')

Data <- data.table(read.csv(file = 'Data/Homicide Database Brazil April29 2019.csv', sep = ';',header = T,stringsAsFactors = F ))

unique(Data$cause)
unique(Data$year)
unique(Data$grupid)

#First I need to code numerically region, sex,race, age and cause.
Deaths$Year   <- Deaths$year
Deaths$State  <- vec.state[as.character(Deaths$state)]
Deaths$Region <- vec.region[as.character(Deaths$region)]
Deaths$Sex    <- vec.sex[as.character(Deaths$sex)]
Deaths$Race   <- vec.race[as.character(Deaths$race)]
Deaths$Age    <- vec.age[as.character(Deaths$grupid)]
Deaths$Cause  <- vec.cause[as.character(Deaths$cause)]
Deaths        <- data.table(Deaths[,c('Year','Region','State','Race','Sex','Cause','Age','deaths')])
Deaths        <- Deaths[order(Year,Region,State,Race,Sex,Cause,Age),]

# check         <- apply(Deaths,2,unique)[1:7]
# prod(unlist(lapply(check,length)))
# dim(Deaths)
# check2 <- Deaths[Deaths$Age==1,]
# x      <- check2[,deaths[1]/deaths[length(deaths)],by = list(Year,Region,State,Race,Sex)]

cast.Deaths   <- dcast.data.table(Deaths,Year+Region+State+Race+Sex+Cause ~ Age,fill = NA,value.var = 'deaths')
melt.Deaths   <- melt.data.table(cast.Deaths,id.vars = c('Year','Region','State','Race','Sex','Cause'),variable.name = 'Age',value.name = 'deaths',variable.factor = F)

cast.Deaths   <- dcast.data.table(melt.Deaths,Age+Region+State+Race+Sex+Cause ~ Year ,fill = NA,value.var = 'deaths')
melt.Deaths   <- melt.data.table(cast.Deaths,id.vars = c('Age','Region','State','Race','Sex','Cause'),variable.name = 'Year',value.name = 'deaths',variable.factor = F)

cast.Deaths   <- dcast.data.table(melt.Deaths,Year+Age+State+Race+Sex+Cause ~ Region ,fill = NA,value.var = 'deaths')
melt.Deaths   <- melt.data.table(cast.Deaths,id.vars = c('Age','Year','State','Race','Sex','Cause'),variable.name = 'Region',value.name = 'deaths',variable.factor = F)

cast.Deaths   <- dcast.data.table(melt.Deaths,Year+Age+Region+Race+Sex+Cause ~ State ,fill = NA,value.var = 'deaths')
melt.Deaths   <- melt.data.table(cast.Deaths,id.vars = c('Age','Year','Region','Race','Sex','Cause'),variable.name = 'State',value.name = 'deaths',variable.factor = F)

cast.Deaths   <- dcast.data.table(melt.Deaths,Year+Age+Region+State+Sex+Cause ~ Race ,fill = NA,value.var = 'deaths')
melt.Deaths   <- melt.data.table(cast.Deaths,id.vars = c('Age','Year','Region','State','Sex','Cause'),variable.name = 'Race',value.name = 'deaths',variable.factor = F)

Deaths        <- melt.Deaths[,c('Year','Region','State','Race','Sex','Cause','Age','deaths')]

Deaths        <- Deaths[order(Year,Region,State,Race,Sex,Cause,Age),]
Deaths$Year   <- as.numeric(Deaths$Year)
Deaths$Region <- as.numeric(Deaths$Region)
Deaths$State  <- as.numeric(Deaths$State)
Deaths$Race   <- as.numeric(Deaths$Race)
Deaths$Age    <- as.numeric(Deaths$Age)

save(Deaths,file = 'R/MortalityBrazilData.RData')

Deaths[colnames(Deaths)] <- sapply(Deaths[colnames(Deaths)],as.numeric)

#I need to create a rest category for mortality
colSums(cast.Deaths)







z      <- read.csv(paste0(DataPath,'/pop by race 0409.txt'))
Pop    <- data.table(read.csv('Data/Population_Brazil.csv',sep = ';', header = T,stringsAsFactors = F))
Remain <- data.table(read.csv('Data/AM_Remaining_Brazil.csv',sep = ';', header = T,stringsAsFactors = F))


#order data
colnames(Deaths) <- varnames
colnames(Pop)    <- c(varnames[c(1,3:8)],'Pop')
colnames(Remain) <- c('Sex',varnames[c(1:5,7:9)])

Deaths           <- Deaths[order(Year,Sex,State.code,Cause,Age.group),]
Pop              <- Pop[order(Year,Sex,State.code,Age.group),]
Remain           <- Remain[order(Year,Sex,State.code,Age.group),]
Remain <-        Remain[,  c("Year","Cause","State.code","State","Region","Sex","Age.group","Age.group.label","Deaths") ]



#Calculate proportions of causes of death by age (remember to take care of 0 when applying to rates)
Dxs <- Dxs[,Dxs.prop := Dxs/sum(Dxs), by = list(year,sex,state,age)]
Dxs[is.na(Dxs.prop),]$Dxs.prop <- 0


#Get data for deaths and population
load('Data/Deaths_CONAPO.rdata')
load('Data/Population_CONAPO.rdata')
Nx     <- (basepryentMX)
Dx     <- (defspry)
gdata:: keep(Dxs,Nx,Dx,sure = T)
source('R/Functions.R')

# Get homogeneous datasets
# rename variables
names(Nx) <- c('row','year','state.name','state','cvegeo','sex2','age','Nx')
names(Dx) <- c('row','year','state.name','state','cvegeo','sex2','age','Dx')
# Get a variable name as integer for sex
Nx$sex <- 2 # for females
Dx$sex <- 2 # for females
Nx$sex[Nx$sex2=='Hombres'] <- 1 # for males
Dx$sex[Dx$sex2=='Hombres'] <- 1 # for males
# Get the same order in all of them 
Dx <- data.table(Dx)
Nx <- data.table(Nx)
Dx <- Dx[,c('year','sex','state','age','Dx')]
Nx <- Nx[,c('year','sex','state','age','Nx')]
Dx <- Dx[year >= 1995 & year <= 2015,]
Nx <- Nx[year >= 1995 & year <= 2015,]
# order all datasets accordingly
Dx  <- Dx[order(year,sex,state,age),]
Nx  <- Nx[order(year,sex,state,age),]
Dxs <- Dxs[order(year,sex,state,Cause,age),]
# merge population to Dx 
DxNx<- merge(Dx,Nx,all = T)
# estimate age.specific mortality rates
DxNx                <- DxNx[,mx :=Dx/Nx]
# maybe a good idea to fit a Kannisto model for the last ages
sort(unique(DxNx[is.na(mx),]$age))
sort(unique(DxNx[is.infinite(mx),]$age))
sort(unique(DxNx[mx > 1,]$age))

# example
# mx  <- DxNx[year==1995 & sex == 1 & state == 0,]$mx
# LT1 <- LifeTable(mx,sex='m')
# 
# library(latticeExtra)
# f1 <- xyplot(log(mx) ~ 0:109, xlim= c(0,120), ylim = c(-9,0))
# f1
# fit_kan <- Kannisto(mx = mx[81:95], x = 80:94)
# mx2 <- predict.Kannisto(fit_kan,94:109)
# f3 <- xyplot(log(mx2) ~ 80:109, type = 'l')
# f1+f3
# mx[95:110] <- mx2
# xyplot(mx ~ 0:109, xlim= c(0,120))

#now fit a Kannisto model to all data
DxNx <- DxNx[order(year,sex,state,age),]
DxNx <- DxNx[,mxn := my.kannisto.fun(mx,x=80:94), by = list(year,sex,state)]
sort(unique(DxNx[is.na(mxn),]$age))
sort(unique(DxNx[is.infinite(mxn),]$age))
sort(unique(DxNx[mxn > 1,]$age))
DxNx <- DxNx[,-c(7)]
colnames(DxNx) <- c(colnames(DxNx)[1:6],'mx')

#DxNx[is.na(mx),]$mx <- 0
#DxNx[is.infinite(mx),]$mx <- 0
#DxNx[mx > 1,]$mx <- 1

#plot(0:109, dx)

# Now transform Dxs to have a similar shape as DxNx
Dxs.cast <- dcast(Dxs,year+sex+state+age ~ Cause,value.var = 'Dxs.prop')

# merge all data together and send the rest of causes when 0 to the cause 16
DxNxCOD           <- merge(DxNx,Dxs.cast,by = c('year','sex','state','age'), all = T)
colnames(DxNxCOD) <- c(colnames(DxNxCOD)[1:7], paste0('Cause_',c(1:14,16)))
DxNxCOD$Cause_16  <- (1 - rowSums(DxNxCOD[,8:21]))

# get mx by causes of death, check for NA
mx.COD       <- DxNxCOD[,8:22]*DxNxCOD$mx 
DT.mxCOD     <- cbind(DxNxCOD[,1:7],mx.COD)
gdata::keep(DT.mxCOD,sure=T)





