rm(list=ls(all=TRUE))
library(data.table)
library(ggthemes)
library(ggplot2)


# period 1 
initial.year1 <- 2000
final.year1   <- 2007

# period 2
initial.year2 <- 2007
final.year2   <- 2015


# load data with decomposition results
load('Data/Decomposition_Results.RData')

ethnicity <- data.table(read.table(file = 'Data/Prop_ethnicity.csv',header = T,sep = ';',stringsAsFactors = F))
ethnicity <- ethnicity[,c(1,3:7)]
names(ethnicity)[1] <- 'state_code'

ethnicity$white <-  as.numeric(gsub("," ,".", ethnicity$white))
ethnicity$black <-  as.numeric(gsub("," ,".", ethnicity$black))
ethnicity$asian <-  as.numeric(gsub("," ,".", ethnicity$asian))
ethnicity$mixed <-  as.numeric(gsub("," ,".", ethnicity$mixed))
ethnicity$native <-  as.numeric(gsub("," ,".", ethnicity$native))

#Fig 2. Contribution of homicides
Cause.contribution1 <- Decomposition.results[year %in% (initial.year1+1):final.year1
                                             ,list(Period = '2000-07',
                                                   contribution = sum(contribution)),
                                             by = list(region,state_code,state,sex,cause)]

Cause.contribution2 <- Decomposition.results[year %in% (initial.year2+1):final.year2
                                             ,list(Period = '2007-15',
                                                   contribution = sum(contribution)),
                                             by = list(region,state_code,state,sex,cause)]

Cause.contribution <- rbind(Cause.contribution1,Cause.contribution2)

Cause.contribution$cause <- as.character(Cause.contribution$cause)

Cause.contribution$region2   <- factor(Cause.contribution$region,levels(factor(Cause.contribution$region))[c(2,3,1,5,4)])

Cause.contribution$sex2      <- ifelse(Cause.contribution$sex == 'male', 'Male', 'Female')

Cause.contribution <- as.data.table(Cause.contribution)

ref.order <- Cause.contribution[cause == 'homicide' & sex == 'male' & Period == '2007-15', c(2,7)]
names(ref.order)[2] <- 'ref.order'

Cause.contribution <- merge(Cause.contribution, ref.order, by = 'state_code')

Cause.contribution$state     <- reorder(Cause.contribution$state,Cause.contribution$ref.order)

Cause.contribution <- merge(Cause.contribution,ethnicity, by = 'state_code')

gdata::keep(Cause.contribution,Decomposition.results,sure = T)

  ggplot(Cause.contribution[cause == 'homicide' & Period == '2007-15'], aes(contribution, black, sex2)) +
  geom_point()+
  facet_grid(sex2 ~ ., scales = "free", space = "free") 
  
  
  
  Cause.contribution[Period == '2000-07' & sex == 'male' & cause == 'homicide',c(2,3,10:15)]