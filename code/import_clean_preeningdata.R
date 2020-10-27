#import the required packages. 
library(janitor)
library(tidyverse)
library(readxl)
library(forcats)
library(DataExplorer)
library(patchwork)
library(lme4)
library(effects)
library(ggpubr)


#Import data on all observations, tidy names, make the relevant variables factors
datafull <- read_xlsx("~/Work/Science Projects/Preening Finches/data/Data22_10_Timo.xlsx",
                  col_types = c("guess", "guess", "guess","guess","date", "numeric", "guess","date","date",
                                "guess","guess","numeric","numeric","numeric","guess", "guess","guess", "guess")) %>% 
  as_tibble()
datafull<-clean_names(datafull)

datafull <- datafull %>% mutate(
  point_id = factor(point_id), visit = factor(visit), preening = factor(preening), season=factor(season),
  species=factor(species),sex=factor(sex),wet=factor(wet),rain=factor(rain),weather=factor(weather))
 

#add row numbers
datafull$obsID <- seq.int(nrow(datafull))

datapreen <- datafull %>% filter(preening == 1 | preening ==2)

#all preening events, probability of preening with leaves (p_preening) as a function of daytime
datapreen %>% group_by(daytime) %>% summarise(wleaves=sum(preening==2),count=n()) %>% mutate(p_preening=(wleaves)/count) %>% ungroup()


#Collapse weather levels. all levels with some rain -> rain; all levels no rain -> no_rain

rain_new<-fct_collapse(datapreen$rain, rain = c("hr","lr","lr/nr","nr/lr"), no_rain = c("nr"))
datapreen$rain<-rain_new

sun_new<-fct_collapse(datapreen$weather, sun = c("s","c/s","s/c"), clouds = c("c","c/f"))
datapreen$sun<-sun_new

sex_new<-fct_collapse(datapreen$sex, M = c("M"), other = c("F","j"))
datapreen$sex<-sex_new

#Rename levels of seasons, wetness of leaves. 
season_new<-fct_collapse(datapreen$season, "Non-breeding" = c("0"), "Breeding" = c("1"))
datapreen$season<-season_new

leaves<-fct_collapse(datapreen$wet, dry = c("0"), wet = c("1"))
datapreen$leaves<-leaves

#keep the following relevant variables: point_id, preening, season, daytime, species, sex, wet, species, temperature
#potentially: visit

data1<-datapreen %>% select(point_id, visit, preening, season, time, daytime, species, sex, leaves, rain, sun,temperature,obsID) 
 glimpse(data1)
 
#exploratory plots
data1%>% plot_bar()
data1%>% plot_histogram()
data1%>% select(preening,season,daytime,sex,leaves,rain,sun) %>% plot_correlation

#probability of preening with leaves
data1 %>% group_by(daytime) %>% summarise(wleaves=sum(preening==2),count=n()) %>% mutate(p_preening=(wleaves)/count) %>% ungroup()

data1 %>% filter (season == 'Non-breeding') %>% group_by(daytime) %>% summarise(wleaves=sum(preening==2),count=n()) %>% mutate(p_preening=(wleaves)/count) %>% ungroup()

#plot for how preening depends on season, species. 
p1 <- ggplot(data1) +
  geom_bar(aes(x = preening)) 
p1+ facet_wrap(~ season)

p2<-ggplot(data1) + geom_col(aes(x = preening,y=sex,fill = sex)) + facet_wrap(~ season)

#preening with leaves (=2) is more frequent in non-breeding season, less frequent in breeding season.

#show barplots for preening with and without leaves, for sex, season and species. 

p1<-data1 %>% 
  filter(preening == 1) %>% 
  ggplot() + 
  geom_bar(aes(x = sex,fill = species)) + 
  facet_wrap( ~ season) +
  coord_cartesian(ylim = c(0, 60))+
  ggtitle("Preening without leaves") +
  theme(legend.position = "none")



p2 <- data1 %>%
  filter(preening == 2) %>%
  ggplot() +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap( ~ season) +
  coord_cartesian(ylim = c(0, 60))+
  ggtitle("Preening with leaves")


p3<-p1+p2
p3
ggsave("preeningfig3_newdata.pdf",width = 25, height = 11, units = "cm")


#same plot for how preening depends on species and season, without sex
p4<-data1 %>% 
  ggplot() + 
  geom_bar(aes(x = season,fill = species)) + 
  facet_wrap( ~ preening, labeller = labeller(preening =
                                                c("2" = "Preening with leaves",
                                                  "1" = "Preening without leaves"))) 
ggsave("preeningfig4.pdf", width = 18, height = 11, units = "cm")

#now a plot for the effect of daytime on preening

p5<-data1 %>% 
  ggplot() + 
  geom_bar(aes(x = daytime,fill = species)) + 
  facet_wrap( ~ preening, labeller = labeller(preening =
                                                c("2" = "Preening with leaves",
                                                  "1" = "Preening without leaves")))+
  ggtitle("Effect of daytime on preening")

ggsave("daytimefig.pdf",width = 25, height = 11, units = "cm")

#Preening with leaves as a function of daytime, separately for breeding and non-breeding season

p5a<-data1 %>% 
  filter(preening == 2) %>%
  ggplot() + 
  geom_bar(aes(x = daytime)) + 
  coord_cartesian(xlim=c(5.5,11.5),ylim = c(0, 70))+
  facet_wrap( ~ season, labeller = labeller(season =
                                                c("Breeding" = "Breeding season",
                                                  "Non-breeding" = "Non-breeding season")))+
ggtitle("Preening events with leaves for daytime and season")
  
ggsave("daytime_preeningwleaves_season.pdf",width = 25, height = 11, units = "cm")



p5b<-data1 %>% 
  filter(leaves=='wet') %>%
  ggplot() + 
  geom_bar(aes(x = daytime)) + 
  coord_cartesian(xlim=c(5.5,11.5),ylim = c(0, 70))+
  facet_wrap( ~ season, labeller = labeller(season =
                                              c("Breeding" = "Breeding season",
                                                "Non-breeding" = "Non-breeding season")))+
ggtitle("Wet leaves for daytime and season")

ggsave("daytime_wetleaves_season.pdf",width = 25, height = 11, units = "cm")



#same plot, but show season as color. Show both plots for wet and dry leaves together

p5c<-data1 %>% 
  filter(leaves=='wet') %>%
  ggplot() + 
  geom_bar(aes(x = daytime,fill=season))+
  ggtitle("Wet leaves")+
  coord_cartesian(xlim=c(5.5,11.5),ylim = c(0, 80))+
  theme(legend.position = "none")

p5d<-data1 %>% 
  filter(leaves=='dry') %>%
  ggplot() + 
  geom_bar(aes(x = daytime,fill=season))+
  ggtitle("Dry leaves")+
  coord_cartesian(xlim=c(5.5,11.5),ylim = c(0, 80))

p5c+p5d

ggsave("daytime_leaves_season.pdf",width = 25, height = 11, units = "cm")

#same plot, but for preening. show season as color. Show both plots for preening with and without leaves together

p5e<-data1 %>% 
  
  filter(preening == '2') %>%
  ggplot() + 
  geom_bar(aes(x = daytime,fill=season))+
  ggtitle("Preening with leaves")+
  coord_cartesian(xlim=c(5.5,11.5),ylim = c(0, 80))+
  theme(legend.position = "none")

p5f<-data1 %>% 
  filter(preening == '1') %>%
  ggplot() + 
  geom_bar(aes(x = daytime,fill=season))+
  ggtitle("Preening without leaves")+
  coord_cartesian(xlim=c(5.5,11.5),ylim = c(0, 80))
p5e+p5f

ggsave("daytime_preening_season.pdf",width = 25, height = 11, units = "cm")



#wetness of leaves

p6=data1 %>% 
  filter(preening==1)%>%
  ggplot() + 
  geom_bar(aes(x = leaves,fill = species)) + 
  facet_wrap( ~ season) +
  ggtitle("Preening without leaves")+
  coord_cartesian(ylim = c(0, 80))+ 
  theme(legend.position = "none")



p7=data1 %>% 
  filter(preening==2)%>%
  ggplot() + 
  geom_bar(aes(x = leaves,fill = species)) + 
  facet_wrap( ~ season) +
  ggtitle("Preening with leaves")+
  coord_cartesian(ylim = c(0, 80))

p6+p7

ggsave("wetnessofleaves.pdf",width = 25, height = 11, units = "cm")

#same plot, but without species as colors

p6a=data1 %>% 
  filter(preening==1)%>%
  ggplot() + 
  geom_bar(aes(x = leaves)) + 
  facet_wrap( ~ season) +
  ggtitle("Preening without leaves")+
  coord_cartesian(ylim = c(0, 80))+ 
  theme(legend.position = "none")



p7a=data1 %>% 
  filter(preening==2)%>%
  ggplot() + 
  geom_bar(aes(x = leaves)) + 
  facet_wrap( ~ season) +
  ggtitle("Preening with leaves")+
  coord_cartesian(ylim = c(0, 80))+ 
  theme(legend.position = "none")

  p6a+p7a 

ggsave("wetnessofleavesa.pdf",width = 25, height = 11, units = "cm")

#Preening with vs. without leaves as a function of wetness of leaves. 

data1 %>% 
  filter(preening ==2)%>% 
  ggplot() + 
  geom_bar(aes(x = leaves)) + 
  facet_wrap( ~ season) +
  ggtitle("Preening with leaves ")

data1 %>% 
  filter(preening ==1)%>% 
  ggplot() + 
  geom_bar(aes(x = leaves)) + 
  facet_wrap( ~ season) +
  ggtitle("Preening without leaves ")

#Wetness of leaves and preening with leaves, separately for seasons. 
  

#temperature

data1 %>% group_by(preening) %>% summarize(count=n(),meantemp=mean(temperature),stdtemp=sqrt(var(temperature))) %>% ungroup()

data1 %>% group_by(preening,leaves) %>% summarize(count=n(),meantemp=mean(temperature)) %>% ungroup()

data1 %>% group_by(season) %>% summarize(count=n(),meantemp=mean(temperature), std=sqrt(var(temperature))) %>% ungroup()

summary(data2$temperature)
#avg temperature is 15.4°C (std=1.12) in non-breeding season, 20.8°C (std=3.82) in breeding season. 

p8<-data1 %>% 
  ggplot() + 
  geom_histogram(aes(x = temperature,fill=season),col="grey",binwidth = 1.5) + 
  facet_wrap( ~ preening, labeller = labeller(preening =
                                                c("2" = "Preening with leaves",
                                                  "1" = "Preening without leaves")))+
  ggtitle("Effect of temperature on preening")

ggsave("tempertaturefig.pdf",width = 25, height = 11, units = "cm")



###fit the model
m2<- glmer(preening ~ season  + leaves+ sex + daytime + (1 | point_id), data = data1, family = binomial)
m3<- glmer(preening ~ season + leaves +  daytime +  (1 | point_id) , data = data1, family = binomial)
m4<- glmer(preening ~ season  +  daytime + (1 | point_id) , data = data1, family = binomial)
m5<- glmer(preening ~ season + (1 | point_id) , data = data1, family = binomial)
m6<- glmer(preening ~  (1 | point_id) , data = data1, family = binomial)


summary(m4)
summary(m3)
summary(m2)

anova(m2,m3,m4,m5,m6)


#effects of wet leaves and sex are not significant. Use m4 for the effect size estimates. 

#These are fixed effect, on an odds ratio scale. 
fixef(m4) %>% exp

plot(allEffects(m4))
plot(allEffects(m2))

#Find the 95%-confidence interval for the odds.
confint(m4,method="boot") %>%
  exp

#Interpretation model results, in terms of odds
#model results: Effect size estimates for season and daytime were similar with and without inclusion of sex and wetness of leaves as predictors. 
#the odds of preening w leaves decreased by a factor of 0.40 (95% CI:0.25-0.52) for each hour increase in daytime.
#the odds ratio of breeding season over non-breeding season was 0.10 (95% CI: 0.04-0.22), implying that the odds of preening with leaves were 90% smaller in the breeding season compared to the non-breeding season. 

#In terms of Probabilities

newdat1<-data1 %>% slice_head(n = 1)
newdat1$daytime=6
newdat2<-newdat1
newdat2$season='Non-breeding'
newdat_6=rbind(newdat1,newdat2)

predict(m4,newdat_6,type="response")

#Probability of preening with leaves at 6 in the morning was 0.36 in the breeding season and 0.85 in the non-breeding season.
#Probability of preening with leaves at 10 in the morning was 0.01 in the breeding season and 0.09 in the non-breeding season.


newdat1$daytime=10
newdat2<-newdat1
newdat2$season='Non-breeding'
newdat_10=rbind(newdat1,newdat2)

predict(m4,newdat_10,type="response")

