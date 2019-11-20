rm(list=ls(all=TRUE))
library(data.table)
library(ggthemes)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Homicides-and-life-expectancy-in-Brazil/")

# period 1 
initial.year1 <- 2000
final.year1   <- 2007

# period 2
initial.year2 <- 2007
final.year2   <- 2015


# load data with decomposition results
load('Data/Decomposition_Results.RData')

#Fig 1. Levels of life expectancy by state
#total change in life expectancy
e0.change1 <- Decomposition.results[year %in% (initial.year1+1):final.year1
                                           ,list(period = '2000-07',
                                                 e0.change = sum(contribution)),
                                           by = list(region,state_code,state,sex)]
e0.change1 <- e0.change1[order(sex,region,state_code)]

e0.change2 <- Decomposition.results[year %in% (initial.year2+1):final.year2
                                           ,list(period = '2007-15',
                                                 e0.change = sum(contribution)),
                                           by = list(region,state_code,state,sex)]

e0.change2          <- e0.change2[order(sex,region,state_code)]

e0.change           <- data.table(rbind(e0.change1,e0.change2))

load('Data/reforder.RData')

e0.change <- merge(e0.change,ref.order, by = 'state_code')

e0.change$state     <- reorder(e0.change$state,e0.change$ref.order)

e0.change$region2   <- factor(e0.change$region,levels(factor(e0.change$region))[c(2,3,1,5,4)])

e0.change$sex2      <- ifelse(e0.change$sex == 'male', 'Male', 'Female')

e0.change$Period    <- e0.change$period

gdata::keep(Decomposition.results,e0.change, sure = T)

Fig1 <-  ggplot(e0.change, aes(e0.change, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-3,6))+
  xlab('Change in life expectancty at birth (years)')+
  geom_point(data = e0.change, aes(e0.change, state,col=Period, shape=Period),size = 3) +
  facet_grid(region2 ~ sex2, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
Fig1

pdf(file="R/Figures/Figure_1.pdf",width=10,height=9,useDingbats = F)
Fig1
dev.off()

### Some results for the paper
load('Data/Check_LifeExpectancy_Brazil05Aug2019.RData')
e0.Brazil.states <- e0.Brazil.states[order(sex,state_code)]

total.change <- cbind(e0.Brazil.states[year == 2000],e0.Brazil.states[year == 2015]$life.expectancy)
total.change[,total.change:= V2 - life.expectancy]



