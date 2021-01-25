rm(list=ls(all=TRUE))
library(data.table)
library(ggthemes)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

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
save(ref.order, file = 'Data/reforder.RData')

Cause.contribution <- merge(Cause.contribution, ref.order, by = 'state_code')

Cause.contribution$state     <- reorder(Cause.contribution$state,Cause.contribution$ref.order)

gdata::keep(Cause.contribution,Decomposition.results,sure = T)

Fig2 <-  ggplot(Cause.contribution[cause == 'homicide'], aes(contribution, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,2.5))+
  xlab('Contribution to change in life expectancy at birth as a result of homicides (years)')+
  geom_point(data = Cause.contribution[cause == 'homicide'], aes(contribution, state,col=Period, shape=Period),size = 3) +
  facet_grid(region2 ~ sex2, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
Fig2

pdf(file="R/Figures/Figure_2_Homicides.pdf",width=10,height=9,useDingbats = F)
Fig2
dev.off()


##### Fig 3. Ischemic Heart Diseases

Fig3 <-  ggplot(Cause.contribution[cause == 'ischemic heart diseases'], aes(contribution, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,2.5))+
  xlab('Contribution to change in life expectancy at birth as a result of IHD (years)')+
  geom_point(data = Cause.contribution[cause == 'ischemic heart diseases'], aes(contribution, state,col=Period, shape=Period),size = 3) +
  facet_grid(region2 ~ sex2, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
Fig3

pdf(file="R/Figures/Figure_3_IHD.pdf",width=10,height=9,useDingbats = F)
Fig3
dev.off()


##### Fig 4. Amenable  to medical service

Fig4 <-  ggplot(Cause.contribution[cause == 'remaining avoidable'], aes(contribution, state, sex2)) +
  geom_vline(xintercept = 0,lty = 2)+
  xlim(c(-2,2.5))+
  xlab('Contribution to change in life expectancy at birth as a result of remaining avoidable/amenable causes (years)')+
  geom_point(data = Cause.contribution[cause == 'remaining avoidable'], aes(contribution, state,col=Period, shape=Period),size = 3) +
  facet_grid(region2 ~ sex2, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual( values=c('magenta','blue'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
Fig4

pdf(file="R/Figures/Figure_4_Amenable.pdf",width=10,height=9,useDingbats = F)
Fig4
dev.FF

### subset data for a map
Homicide.contribution <- Cause.contribution[cause == 'homicide',c(1:7)]
save(Homicide.contribution,file ='Data/Map_Homicide_data.RData')

load('Data/Map_Homicide_data.RData')

write.csv(Homicide.contribution,file ='Data/Map_Homicide_data.csv',)


