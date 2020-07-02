rm(list=ls(all=TRUE))
library(data.table)
library(ggthemes)
library(ggplot2)
library(patchwork)

load('Data/Decomposition_Results.RData')

# period 2000-05
initial.year2 <- 2000
final.year2   <- 2005

# load data with decomposition results

Cause.contribution <- Decomposition.results[year %in% (initial.year2+1):final.year2
                                             ,list(Period = '1',
                                                   contribution = sum(contribution)),
                                             by = list(region,state_code,state,sex,cause)]


Cause.contribution$region2   <- factor(Cause.contribution$region,levels(factor(Cause.contribution$region))[c(2,3,1,5,4)])

Cause.contribution$sex2      <- ifelse(Cause.contribution$sex == 'male', 'Male', 'Female')

Cause.contribution <- as.data.table(Cause.contribution)

ref.order <- Cause.contribution[cause == 'homicide' & sex == 'male' & Period == '1', c(2,7)]
names(ref.order)[2] <- 'ref.order'

Cause.contribution <- merge(Cause.contribution, ref.order, by = 'state_code')

Cause.contribution$state     <- reorder(Cause.contribution$state,Cause.contribution$ref.order)


Fig1 <-  ggplot(Cause.contribution[cause == 'homicide'], aes(contribution, state, sex2)) +
  ggtitle('2000-05')+
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,1.5))+
  xlab(' ')+
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


# period 2000-05
initial.year2 <- 2005
final.year2   <- 2010

# load data with decomposition results

Cause.contribution <- Decomposition.results[year %in% (initial.year2+1):final.year2
                                            ,list(Period = '1',
                                                  contribution = sum(contribution)),
                                            by = list(region,state_code,state,sex,cause)]


Cause.contribution$region2   <- factor(Cause.contribution$region,levels(factor(Cause.contribution$region))[c(2,3,1,5,4)])

Cause.contribution$sex2      <- ifelse(Cause.contribution$sex == 'male', 'Male', 'Female')

Cause.contribution <- as.data.table(Cause.contribution)

Cause.contribution <- merge(Cause.contribution, ref.order, by = 'state_code')

Cause.contribution$state     <- reorder(Cause.contribution$state,Cause.contribution$ref.order)



Fig2 <-  ggplot(Cause.contribution[cause == 'homicide'], aes(contribution, state, sex2)) +
  ggtitle('2005-10')+
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,1.5))+
  xlab('Contribution to change in life expectancty at birth (years)')+
  geom_point(data = Cause.contribution[cause == 'homicide'], aes(contribution, state,col=sex, shape=sex),size = 3,show.legend = F) +
  facet_grid( region2 ~., scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')


Fig2

# period 2000-05
initial.year2 <- 2010
final.year2   <- 2015

# load data with decomposition results

Cause.contribution <- Decomposition.results[year %in% (initial.year2+1):final.year2
                                            ,list(Period = '1',
                                                  contribution = sum(contribution)),
                                            by = list(region,state_code,state,sex,cause)]


Cause.contribution$region2   <- factor(Cause.contribution$region,levels(factor(Cause.contribution$region))[c(2,3,1,5,4)])

Cause.contribution$sex2      <- ifelse(Cause.contribution$sex == 'male', 'Male', 'Female')

Cause.contribution <- as.data.table(Cause.contribution)

Cause.contribution <- merge(Cause.contribution, ref.order, by = 'state_code')

Cause.contribution$state     <- reorder(Cause.contribution$state,Cause.contribution$ref.order)


Fig3 <-  ggplot(Cause.contribution[cause == 'homicide'], aes(contribution, state, sex2)) +
  ggtitle('2010-15')+
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,1.5))+
  xlab('')+
  geom_point(data = Cause.contribution[cause == 'homicide'], aes(contribution, state,col=sex, shape=sex),size = 3,show.legend = F) +
  facet_grid( region2 ~., scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')


Fig3




pdf(file="BMJ submission/Figure_Section3_Homicides.pdf",width=13,height=9,useDingbats = F)
Fig1|Fig2|Fig3
dev.off()


