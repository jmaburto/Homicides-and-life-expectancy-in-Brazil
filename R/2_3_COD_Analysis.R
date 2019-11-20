rm(list=ls(all=TRUE))
library(data.table)
library(ggthemes)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)


setwd("C:/Users/jmaburto.SAM/Documents/GitHub/Homicides-and-life-expectancy-in-Brazil/")

load('Data/Decomposition_Results.RData')


# Get Cause-specific components by cause from 2000-2015
initial.year <- 2000
final.year   <- 2015

Cause.contribution <- Decomposition.results[year %in% (initial.year+1):final.year
                                            ,list(contribution = sum(contribution)),
                                            by = list(region,state_code,state,sex,cause)]

Cause.contribution$cause <- as.character(Cause.contribution$cause)

#causes of death
causes.available <- c("alcoholic liver disease","diabetes","hiv","homicide","ischemic heart diseases","lung cancer",
                      "others causes","remaining avoidable", "road traffic accidents","self inflicted injuries")

### Figure 1, cause of death males
range(Cause.contribution[ Cause.contribution$sex == 'female',]$contribution)


pdf(file="R/Figures/Cause_contrib_Females_2000-15.pdf",width=10,height=9,useDingbats = F)

for (i in 1:length(causes.available)) {

Fig.contribution <- Cause.contribution[Cause.contribution$cause == causes.available[i] & Cause.contribution$sex == 'female',]


changes.COD <- ggplot(Fig.contribution, aes(contribution, state)) +
  ggtitle(paste0(causes.available[i],' contribution to life expectancy in ',initial.year,'-',final.year))+
  geom_vline(xintercept = 0)+
  xlim(c(-.6,4.8))+
  ylab('Contribution in years')+
  geom_point(data = Fig.contribution, aes(contribution, state),size = 3) +
  facet_grid(region ~ cause, scales = "free", space = "free") +
  theme_light()+
  #scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

grid.arrange(changes.COD,ncol=1)

changes.COD <- NULL

print(i)

}
dev.off()

range(Cause.contribution[ Cause.contribution$sex == 'male',]$contribution)

pdf(file="R/Figures/Cause_contrib_Males_2000-15.pdf",width=10,height=9,useDingbats = F)

for (i in 1:length(causes.available)) {
  
  Fig.contribution <- Cause.contribution[Cause.contribution$cause == causes.available[i] & Cause.contribution$sex == 'male',]
  
  changes.COD <- ggplot(Fig.contribution, aes(contribution, state)) +
    ggtitle(paste0(causes.available[i],' contribution to life expectancy in ',initial.year,'-',final.year))+
    geom_vline(xintercept = 0)+
    xlim(c(-1.7,5.3))+
    ylab('Contribution in years')+
    geom_point(data = Fig.contribution, aes(contribution, state),size = 3) +
    facet_grid(region ~ cause, scales = "free", space = "free") +
    theme_light()+
    #scale_color_manual(values=base2[c(1,6)])+
    theme(axis.title.y=element_blank())+
    theme(axis.title.x = element_text(size = 12, angle = 00))+
    theme(text = element_text(size=14),
          strip.text.x = element_text(size = 14, colour = "black"))+
    theme(strip.text.y = element_text(colour = "black"))+
    theme(legend.position = 'bottom')
  
  grid.arrange(changes.COD,ncol=1)
  
  changes.COD <- NULL
  
  print(i)
  
}
dev.off()




#total change in life expectancy


e0.change.decomp <- Decomposition.results[year %in% (initial.year+1):final.year
                                          ,list(e0.change = sum(contribution)),
                                          by = list(region,state_code,state,sex)]
e0.change.decomp$source <- 'Decomp'

load('Data/Check_LifeExpectancy_Brazil05Aug2019.RData')

e0.Brazil.states <- e0.Brazil.states[,c(2,4,3,5,6,1)]
names(e0.Brazil.states)[5] <- 'e0'

e0.change.julia <- e0.Brazil.states[,list(e0.change = e0[length(e0)] - e0[1]), by = list(region,state_code,state,sex)]

e0.change.julia$source <- 'Julia'

e0.data <- rbind(e0.change.decomp,e0.change.julia)
e0.data <- e0.data[order(region,state_code,state,sex,source),]
e0.data$cause <- 'total'

Check.Change.ex <- ggplot(e0.data, aes(e0.change, state)) +
  ggtitle(paste0(' change in life expectancy in ',initial.year,'-',final.year))+
  geom_vline(xintercept = 0)+
  #xlim(c(-3,3))+
  ylab('Contribution in years')+
  geom_point(data = e0.data, aes(e0.change, state,col=source, shape=source),size = 3) +
  facet_grid(region ~ cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=1:2)+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')
Check.Change.ex




Change.ex <- ggplot(e0.change.decomp, aes(e0.change, state)) +
  ggtitle(paste0(' change in life expectancy in ',initial.year,'-',final.year))+
  geom_vline(xintercept = 0)+
  #xlim(c(-3,3))+
  geom_point(data = e0.change.decomp, aes(e0.change, state,col=sex, shape=sex),size = 3) +
  ylab('Change in years')+
  facet_grid(region ~. , scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values= c('pink','magenta'))+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

Change.ex

pdf(file="R/Figures/Change_life_expectancy_2000-15.pdf",width=10,height=9,useDingbats = F)
Change.ex
dev.off()



