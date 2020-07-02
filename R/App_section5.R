rm(list=ls(all=TRUE))
library(data.table)
library(ggthemes)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# period 2
initial.year2 <- 2004
final.year2   <- 2015

# load data with decomposition results
load('Data/Decomposition_Results.RData')

Cause.contribution <- Decomposition.results[year %in% (initial.year2+1):final.year2
                                             ,list(Period = '2004-15',
                                                   contribution = sum(contribution)),
                                             by = list(region,state_code,state,sex,cause)]


Cause.contribution$region2   <- factor(Cause.contribution$region,levels(factor(Cause.contribution$region))[c(2,3,1,5,4)])

Cause.contribution$sex2      <- ifelse(Cause.contribution$sex == 'male', 'Male', 'Female')

Cause.contribution <- as.data.table(Cause.contribution)

ref.order <- Cause.contribution[cause == 'homicide' & sex == 'male' & Period == '2004-15', c(2,7)]
names(ref.order)[2] <- 'ref.order'

Cause.contribution <- merge(Cause.contribution, ref.order, by = 'state_code')

Cause.contribution$state     <- reorder(Cause.contribution$state,Cause.contribution$ref.order)

gdata::keep(Cause.contribution,Decomposition.results,sure = T)

Fig2 <-  ggplot(Cause.contribution[cause == 'homicide'], aes(contribution, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,1.5))+
  xlab('Contribution to change in life expectancty at birth (years)')+
  geom_point(data = Cause.contribution[cause == 'homicide'], aes(contribution, state,col=sex, shape=sex),size = 3) +
  facet_grid( region2 ~., scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
Fig2

pdf(file="BMJ submission/Figure_Section5_Homicides.pdf",width=6,height=9,useDingbats = F)
Fig2
dev.off()


