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

gdata::keep(Cause.contribution,Decomposition.results,sure = T)

`%notin%` <- Negate(`%in%`)

unique(Cause.contribution$cause)

fig.data <- Cause.contribution[cause %notin% c('homicide',"ischemic heart diseases","remaining avoidable"),]

FigS1 <-  ggplot(fig.data, 
                 aes(contribution, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,2.5))+
  xlab('Contribution to change in life expectancty at birth (years)')+
  geom_point(data = fig.data[sex == 'male'], aes(contribution, state,col=Period, shape=Period),size = 3) +
  facet_grid(region2 ~ cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
FigS1

pdf(file="R/Figures/Figure_S1_males.pdf",width=17,height=9,useDingbats = F)
FigS1
dev.off()

FigS2 <-  ggplot(fig.data, 
                 aes(contribution, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,2.5))+
  xlab('Contribution to change in life expectancty at birth (years)')+
  geom_point(data = fig.data[sex == 'female'], aes(contribution, state,col=Period, shape=Period),size = 3) +
  facet_grid(region2 ~ cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
FigS2

pdf(file="R/Figures/Figure_S2_females.pdf",width=17,height=9,useDingbats = F)
FigS2
dev.off()



