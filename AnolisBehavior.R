## Packages ----
library(arm)
library(rv)
library(tidyverse)
library(ggplot2)
library(jtools)
library(interactions)
library(patchwork)
library(MASS)
library(car)

## Data ----
data1<-read.csv("AnolisBehavior.csv", na.strings="")
# Stimulus = Anolis sagrei
# Focal = Anolis sagrei or Anolis distichus

data1[, c(12,22,25,28,31,33,
          35,37,40,42,43,44,
          47,50,53,55,57,59,
          61,63,64)]<-ifelse(data1[, c(12,22,25,28,31,33,
                                       35,37,40,42,43,44,
                                       47,50,53,55,57,59,
                                       61,63,64)]=="Y",1,0)

data1$Stimulus_ID<-factor(data1$Stimulus_ID)
data1$Stimulus_Sex<-factor(data1$Stimulus_Sex)
data1$F_morph<-factor(data1$F_morph)
data1$Focal_ID<-factor(data1$Focal_ID)
data1$Focal_Species<-factor(data1$Focal_Species)
data1$Focal_Species_Code<-factor(data1$Focal_Species_Code)
data1$Focal_Sex<-factor(data1$Focal_Sex)
data1$Focal_F_morph<-factor(data1$Focal_F_morph)
data1$Stim_F_morph<-factor(data1$Stim_F_morph)

data1.andi<-subset(data1, Focal_Species_Code=="Andi")
data1.ansa<-subset(data1, Focal_Species_Code=="Ansa")

## Summary ----
# All
aggregate(Focal_ID~Focal_Sex, length, data=data1) # 67 (14F & 53M) focal lizards with sex noted in the raw data
# The small number of females could be really limiting.

aggregate(Focal_ID~Focal_Species_Code, length, data=data1)
aggregate(Focal_ID~Focal_Species_Code+Focal_Sex, length, data=data1)

subset(data1, Focal_Sex=="M" & Stimulus_Sex=="M") # 35 matched males
subset(data1, Focal_Sex=="F" & Stimulus_Sex=="F") # 6 matched females
aggregate(Focal_ID~Focal_Sex, length, data=data1) # 28 focal lizards of unknown sex

aggregate(Focal_Bite~Focal_Species_Code, sum, data=data1)
# Andi         11
# Ansa         20
aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, sum, data=data1)
# Andi         F          1
# Ansa         F          3
# Andi         M          7
# Ansa         M         15
aggregate(Focal_Flee~Focal_Species_Code, sum, data=data1)
# Andi         33
# Ansa         19
aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, sum, data=data1)
# Andi         F          3
# Ansa         F          4
# Andi         M         15
# Ansa         M         11

sum(data1[35]==1) # 31 focal lizards attacked (bit)
sum(data1[33]==1) # 52 focal lizards fled
sum(data1[35]==0 & data1[33]==0) # 12 (13%) Didn't attack or flee (respond)- 7 Ansa & 5 Andi 
sum(data1[55]==1, na.rm=T) # Seems like some of the trials may have ended when the stimulus lizard fled (MJAnsaF12)
sum(data1[35]==0 & data1[33]==0 & data1[31]==1) # 7 focal lizards approached, but did not attack or flee
sum(data1[35]==0 & data1[33]==0 & data1[31]==0) # 5 focal lizards did not approach, attack, or flee
app.only<-subset(data1, Focal_Bite==0 & Focal_Flee==0 & Focal_Approach==1) # 1 1min observation
no.r<-subset(data1, Focal_Bite==0 & Focal_Flee==0 & Focal_Approach==0) # 2 15min observations, 1 10min observation, 2 observations with no end time recorded
# It looks like some of the trial times are a bit sketchy. Especially for the 12 that didn't "respond".
# I'm going to exclude the trials where 
aggregate(Trial.Time~factor(Focal_Flee), range, data=data1) # It's a bit odd that there would be a flee at 30 minutes (presumably the end of the trial)
plot(Trial.Time~factor(Focal_Flee), data=data1)
aggregate(Trial.Time~factor(Focal_Bite), range, data=data1)
plot(Trial.Time~factor(Focal_Bite), data=data1)

# 83 focal lizards responded to stimulus lizard
# In this case, 'respond' means the outcome ended in a bite or flee- I need to parse if displays occurred
(31/83)*100 # 37% attacked
(52/83)*100 # 63% fled

# Bite
sum(data1[25]==1 & data1[35]==1) # 16 headbobs/pushups resulted in focal biting
sum(data1[25]==0 & data1[35]==1) # 15 bit without headbobs/pushups
(16/31)*100 # 52% headbobbed and bit
sum(data1[28]==1 & data1[35]==1) # 14 dewlap extensions resulted in focal biting
sum(data1[28]==0 & data1[35]==1) # 17 extended dewlaps without biting
(14/31)*100 # 45% extended dewlaps and bit
a<-subset(data1, Focal_Bite==1)
mean(a$Focal_number_headbob_pushups, na.rm=T)

# Flee
sum(data1[25]==1 & data1[33]==1) # 31 headbobs/pushups resulted in focal fleeing
sum(data1[25]==0 & data1[33]==1) # 21 fled without headbobs/pushups
(21/52)*100 # 40% fled without headbobs
sum(data1[28]==1 & data1[33]==1, na.rm=T) # 21 fled after extending dewlaps
sum(data1[28]==0 & data1[33]==1, na.rm=T) # 30 fled without extending dewlaps
(30/51)*100 # 59% fled without extending dewlaps
# Extension was not recorded for GWAnsaF4 
b<-subset(data1, Focal_Flee==1)

## Lizards that responded
a$outcome<-factor("1")
b$outcome<-factor("0")
# This outcome column doesn't work correctly in logistic models because it doesn't like that it's a factor- Oops
responded<-rbind(a,b)
responded$Focal_number_headbob_pushups[is.na(responded$Focal_number_headbob_pushups)]<-0
aggregate(Focal_number_headbob_pushups~outcome, mean, data=responded)
responded$Focal_number_dewlap_extensions[is.na(responded$Focal_number_dewlap_extensions)]<-0
aggregate(Focal_number_dewlap_extensions~outcome, mean, data=responded)
responded$Focal_Sex<-factor(responded$Focal_Sex, levels=c(levels(responded$Focal_Sex), "Unkn")) # Add factor level to populate
responded$Focal_Sex[is.na(responded$Focal_Sex)]<-"Unkn"

plot(Focal_number_headbob_pushups~outcome, data=responded)
plot(Focal_number_dewlap_extensions~outcome, data=responded)

aggregate(Focal_ID~Stimulus_Sex+Focal_Species_Code, length, data=responded)
aggregate(Focal_ID~Focal_Sex, length, data=responded) # 59 focal lizards identified to sex responded
aggregate(Focal_Bite~Focal_Sex+Stimulus_Sex, length, data=responded)
aggregate(Focal_Bite~Focal_Sex+Stimulus_Sex, sum, data=responded)
# 100% of focal females bit stimulus females (n=4)
# 50% of focal males bit stimulus females (n=18)
# 0% of focal females bit stimulus males (n=7)
# 43% of focal males bit stimulus males (n=30)

c1<-aggregate(Focal_Bite~Focal_Sex+Stimulus_Sex+Focal_Species_Code, sum, data=responded)
c2<-aggregate(Focal_Bite~Focal_Sex+Stimulus_Sex+Focal_Species_Code, length, data=responded)
c<-cbind(c1, c2$Focal_Bite)
colnames(c)<-c("Focal.Sex","Stim.Sex","Species.Code","sum","length")
c$percentage<-(c$sum/c$length)*100

c.matched.m<-subset(c, Focal.Sex=="M" & Stim.Sex=="M")
c.matched.f<-subset(c, Focal.Sex=="F" & Stim.Sex=="F")
c.matched<-rbind(c.matched.m, c.matched.f)
c.mismatched.m<-subset(c, Focal.Sex=="M" & Stim.Sex=="F")
c.mismatched.f<-subset(c, Focal.Sex=="F" & Stim.Sex=="M")
c.mismatched<-rbind(c.mismatched.m, c.mismatched.f)

# Matched
p1<-ggplot(c.matched, aes(x=Species.Code, y=percentage, fill=Focal.Sex))+
  geom_bar(stat="identity", position="dodge", colour="black")+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="bottom")+
  scale_x_discrete(labels = c('Anolis distichus','Anolis sagrei'))+
  scale_fill_discrete(name = "Focal Sex")+
  ylim(0, 100)+
  geom_text(aes(label=length), position = position_dodge(width = .8), vjust = -0.5, size=5)+
  xlab("")+
  ylab("Frequency of Attack")+
  theme(plot.title=element_text(face="bold", size=15, hjust=0.5))+
  ggtitle("Matched Sexes")
#ggsave("Matched.png", width=7, height=7, plot=p1)

# Mismatched
p2<-ggplot(c.mismatched, aes(x=Species.Code, y=percentage, fill=Focal.Sex))+
  geom_bar(stat="identity", position="dodge", colour="black")+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="bottom")+
  scale_x_discrete(labels = c('Anolis distichus','Anolis sagrei'))+
  scale_fill_discrete(name = "Focal Sex")+
  ylim(0,100)+
  geom_text(aes(label=length), position = position_dodge(width = .8), vjust = -0.5, size=5)+
  xlab("")+
  ylab("Frequency of Attack")+
  theme(plot.title=element_text(face="bold", size=15, hjust=0.5))+
  ggtitle("Mismatched Sexes")
#ggsave("Mismatched.png", width=7, height=7, plot=p2)

# Both species together
d1<-aggregate(Focal_Bite~Focal_Sex+Stimulus_Sex, sum, data=responded)
d2<-aggregate(Focal_Bite~Focal_Sex+Stimulus_Sex, length, data=responded)
d<-cbind(d1, d2$Focal_Bite)
colnames(d)<-c("Focal.Sex","Stim.Sex","sum","length")
d$percentage<-(d$sum/d$length)*100

d.matched.m<-subset(d, Focal.Sex=="M" & Stim.Sex=="M")
d.matched.f<-subset(d, Focal.Sex=="F" & Stim.Sex=="F")
d.matched<-rbind(d.matched.m, d.matched.f)
d.matched$match<-"matched"
d.mismatched.m<-subset(d, Focal.Sex=="M" & Stim.Sex=="F")
d.mismatched.f<-subset(d, Focal.Sex=="F" & Stim.Sex=="M")
d.mismatched<-rbind(d.mismatched.m, d.mismatched.f)
d.mismatched$match<-"mismatched"
d.final<-rbind(d.matched, d.mismatched)

p3<-ggplot(d.final, aes(x=match, y=percentage, fill=Focal.Sex))+
  geom_bar(stat="identity", position="dodge", colour="black")+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  theme(panel.grid.major=element_line(colour="#FFFFFF"),panel.grid.minor=element_line(colour="#FFFFFF"))+
  theme(axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="bottom")+
  scale_x_discrete(labels = c('Matched Sexes','Mismatched Sexes'))+
  scale_fill_discrete(name = "Focal Sex")+
  ylim(0, 100)+
  geom_text(aes(label=length), position = position_dodge(width = .8), vjust = -0.5, size=5)+
  xlab("")+
  ylab("Frequency of Attack")+
  theme(plot.title=element_text(face="bold", size=15, hjust=0.5))+
  ggtitle("Pooled")
#ggsave("Pooled.png", width=7, height=7, plot=p3)

panel<-p1+p2
#ggsave("Panel.png", width=7, height=7, plot=panel)

# Remove unknown individuals
responded.kn<-subset(responded, Focal_Sex!="Unkn")
responded.kn<-droplevels(responded.kn)
responded.kn$matched<-factor(ifelse(responded.kn$Focal_Sex==responded.kn$Stimulus_Sex, "Yes", "No"))

table(responded.kn$Focal_Sex, responded.kn$outcome)
chisq.test(responded.kn$Focal_Sex, responded.kn$outcome, correct=F)

table(responded.kn$Focal_Species_Code, responded.kn$outcome)
chisq.test(responded.kn$Focal_Species_Code, responded.kn$outcome, correct=FALSE)

# By match
responded.kn.m<-subset(responded.kn, matched=="Yes")
responded.kn.mm<-subset(responded.kn, matched=="No")

# Matched
table(responded.kn.m$Focal_Sex, responded.kn.m$outcome)
chisq.test(responded.kn.m$Focal_Sex, responded.kn.m$outcome, correct=F)

# Mismatched
table(responded.kn.mm$Focal_Sex, responded.kn.mm$outcome)
chisq.test(responded.kn.mm$Focal_Sex, responded.kn.mm$outcome, correct=F)

## Andi
Andi.m<-subset(responded.kn.m, Focal_Species_Code=="Andi")
Andi.mm<-subset(responded.kn.mm, Focal_Species_Code=="Andi")

# Matched
table(Andi.m$Focal_Sex, Andi.m$outcome)
chisq.test(Andi.m$Focal_Sex, Andi.m$outcome, correct=F)

# Mismatched
table(Andi.mm$Focal_Sex, Andi.mm$outcome)
chisq.test(Andi.mm$Focal_Sex, Andi.mm$outcome, correct=F)

## Ansa
Ansa.m<-subset(responded.kn.m, Focal_Species_Code=="Ansa")
Ansa.mm<-subset(responded.kn.mm, Focal_Species_Code=="Ansa")

# Matched
table(Ansa.m$Focal_Sex, Ansa.m$outcome)
chisq.test(Ansa.m$Focal_Sex, Ansa.m$outcome, correct=F)

# Mismatched
table(Ansa.mm$Focal_Sex, Ansa.mm$outcome)
chisq.test(Ansa.mm$Focal_Sex, Ansa.mm$outcome, correct=F)

## Behavioral Analysis ----
# invlogit = conversion to probability
glm1<-glm(outcome~Focal_Species_Code*Stimulus_Sex, data=responded.kn, family=binomial(link="logit"))
summary(glm1)
betas1<-coef(glm1)

# only lizards that attacked
attacked.kn<-subset(responded.kn, outcome=="1") 
glm2<-glm(Focal_number_headbob_pushups~Focal_Species_Code*Stimulus_Sex, data=attacked.kn, family=poisson(link="log"))
summary(glm2)
predict(glm2, type="response")

hist(attacked.kn$Focal_Bite_Latency, breaks=10)
glm3<-glm(Focal_Bite_Latency~Focal_Species_Code*Stimulus_Sex, data=attacked.kn, family=quasipoisson) # quasipoisson produces more reasonable error estimates, I think
summary(glm3)
mean(attacked.kn$Focal_Bite_Latency)
var(attacked.kn$Focal_Bite_Latency)
aggregate(Focal_Bite_Latency~Focal_Species_Code+Stimulus_Sex, length, data=attacked.kn)
aggregate(Focal_Bite_Latency~Focal_Species_Code+Stimulus_Sex, mean, data=attacked.kn)
aggregate(Focal_Bite_Latency~Focal_Species_Code+Stimulus_Sex, median, data=attacked.kn)
aggregate(Focal_Bite_Latency~Focal_Species_Code+Stimulus_Sex, range, data=attacked.kn)
int.3<-cat_plot(glm3, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T) # cat_plot comes from interaction package
#ggsave("BiteLatency.png", width=7, height=7, plot=int.3)

hist(attacked.kn$Focal_number_headbob_pushups, breaks=10)
glm4<-glm(Focal_number_headbob_pushups~Focal_Species_Code*Stimulus_Sex, data=attacked.kn, family=quasipoisson(link="log"))
summary(glm4)
mean(attacked.kn$Focal_number_headbob_pushups)
var(attacked.kn$Focal_number_headbob_pushups)
aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, length, data=attacked.kn)
aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, mean, data=attacked.kn)
aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, median, data=attacked.kn)
aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, range, data=attacked.kn)
int.4<-cat_plot(glm4, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("HeadbobNumber.png", width=7, height=7, plot=p1)

# Try thinking about this in terms of matched vs. unmatched sexes- Doesn't really work because there are no unmatched females
# hist(attacked.kn$Focal_Bite_Latency)
# glm5<-glm(Focal_Bite_Latency~Focal_Species_Code*matched, data=attacked.kn, family=quasipoisson) # quasipoisson produces more reasonable errors
# summary(glm5)
# mean(attacked.kn$Focal_Bite_Latency)
# var(attacked.kn$Focal_Bite_Latency)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, length, data=attacked.kn)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, mean, data=attacked.kn)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, median, data=attacked.kn)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, range, data=attacked.kn)
# cat_plot(glm5, pred = Focal_Species_Code, modx = matched, plot.points=T) 
# 
# hist(attacked.kn$Focal_Bite_Latency)
# glm6<-glm(Focal_Bite_Latency~Focal_Sex*matched, data=attacked.kn, family=quasipoisson) # quasipoisson produces more reasonable errors
# summary(glm6)
# mean(attacked.kn$Focal_Bite_Latency)
# var(attacked.kn$Focal_Bite_Latency)
# aggregate(Focal_Bite_Latency~Focal_Sex+matched, length, data=attacked.kn)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, mean, data=attacked.kn)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, median, data=attacked.kn)
# aggregate(Focal_Bite_Latency~Focal_Species_Code+matched, range, data=attacked.kn)
# cat_plot(glm3, pred = Focal_Species_Code, modx = matched, plot.points=T) 
# 
# glm7<-glm(Focal_number_headbob_pushups~Focal_Species_Code*matched, data=attacked.kn, family=poisson(link="log"))
# summary(glm7)
# mean(attacked.kn$Focal_number_headbob_pushups)
# var(attacked.kn$Focal_number_headbob_pushups)
# aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, length, data=attacked.kn)
# aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, mean, data=attacked.kn)
# aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, median, data=attacked.kn)
# aggregate(Focal_number_headbob_pushups~Focal_Species_Code+Stimulus_Sex, range, data=attacked.kn)
# cat_plot(glm4, pred = Focal_Species_Code, modx = Stimulus_Sex)

hist(attacked.kn$Focal_number_dewlap_extensions)
glm8<-glm(Focal_number_dewlap_extensions~Focal_Species_Code*Stimulus_Sex, data=attacked.kn, family=poisson(link="log"))
summary(glm8)
mean(attacked.kn$Focal_number_dewlap_extensions)
var(attacked.kn$Focal_number_dewlap_extensions)
aggregate(Focal_number_dewlap_extensions~Focal_Species_Code+Stimulus_Sex, length, data=attacked.kn)
aggregate(Focal_number_dewlap_extensions~Focal_Species_Code+Stimulus_Sex, mean, data=attacked.kn)
aggregate(Focal_number_dewlap_extensions~Focal_Species_Code+Stimulus_Sex, median, data=attacked.kn)
aggregate(Focal_number_dewlap_extensions~Focal_Species_Code+Stimulus_Sex, range, data=attacked.kn)
int.8<-cat_plot(glm8, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("ExtensionsNumber.png", width=7, height=7, plot=p1)

attacked.kn$total.behaviors<-attacked.kn$Focal_number_headbob_pushups+attacked.kn$Focal_number_dewlap_extensions
hist(attacked.kn$total.behaviors, breaks=10)
glm9<-glm(total.behaviors~Focal_Species_Code*Stimulus_Sex, data=attacked.kn, family=quasipoisson(link="log"))
summary(glm9)
mean(attacked.kn$total.behaviors)
var(attacked.kn$total.behaviors)
aggregate(total.behaviors~Focal_Species_Code+Stimulus_Sex, length, data=attacked.kn)
aggregate(total.behaviors~Focal_Species_Code+Stimulus_Sex, mean, data=attacked.kn)
aggregate(total.behaviors~Focal_Species_Code+Stimulus_Sex, median, data=attacked.kn)
aggregate(total.behaviors~Focal_Species_Code+Stimulus_Sex, range, data=attacked.kn)
int.9<-cat_plot(glm8, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("ExtensionsNumber.png", width=7, height=7, plot=p1)

## Try negative binomial (from package MASS) ----
# It looks like negative binomial is the way to go
latency.nb<-glm.nb(Focal_Bite_Latency~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
summary(latency.nb)
latency.plot<-cat_plot(latency.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("BiteLatency.png", width=7, height=7, plot=latency.plot)

headbob.nb<-glm.nb(Focal_number_headbob_pushups~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
summary(headbob.nb)
headbob.plot<-cat_plot(headbob.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("HeadbobNumber.png", width=7, height=7, plot=headbob.plot)

dewlap.nb<-glm.nb(Focal_number_dewlap_extensions~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
summary(dewlap.nb)
dewlap.plot<-cat_plot(dewlap.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("ExtensionsNumber.png", width=7, height=7, plot=dewlap.plot)

total.nb<-glm.nb(total.behaviors~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
summary(total.nb)
total.plot<-cat_plot(total.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
#ggsave("TotalBehavior.png", width=7, height=7, plot=total.plot)

# It doesn't like numeric for poisson (makes sense)
attacked.kn$headbob.rate<-attacked.kn$Focal_number_headbob_pushups/attacked.kn$Focal_Bite_Latency
headbob.nb<-glm.nb(headbob.rate~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
summary(headbob.nb)
cat_plot(headbob.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)  

## Same analyses for matched sexes ----
# attacked.kn.m<-subset(attacked.kn, matched=="Yes")
# 
# latency.nb<-glm.nb(Focal_Bite_Latency~Focal_Species_Code*Stimulus_Sex, data=attacked.kn.m)
# summary(latency.nb)
# latency.plot<-cat_plot(latency.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
# #ggsave("BiteLatency.png", width=7, height=7, plot=latency.plot)
# 
# headbob.nb<-glm.nb(Focal_number_headbob_pushups~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
# summary(headbob.nb)
# headbob.plot<-cat_plot(headbob.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
# #ggsave("HeadbobNumber.png", width=7, height=7, plot=headbob.plot)
# 
# dewlap.nb<-glm.nb(Focal_number_dewlap_extensions~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
# summary(dewlap.nb)
# dewlap.plot<-cat_plot(dewlap.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
# #ggsave("ExtensionsNumber.png", width=7, height=7, plot=dewlap.plot)
# 
# total.nb<-glm.nb(total.behaviors~Focal_Species_Code*Stimulus_Sex, data=attacked.kn)
# summary(total.nb)
# total.plot<-cat_plot(total.nb, pred = Focal_Species_Code, modx = Stimulus_Sex, plot.points=T)
# #ggsave("TotalBehavior.png", width=7, height=7, plot=total.plot)
## New Analyses based on discussion with Christian 4/26 ----
  # Responded subset includes an unknown category for focal sex

responded$Focal_Dewlap_extension[is.na(responded$Focal_Dewlap_extension)]<-0

# New Models
  # Logistic Regressions

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

n.glm.global<-glm(Focal_Bite~Focal_Species_Code+Focal_Sex+Focal_Dewlap_extension+Focal_Headbob_Pushup, data=responded, family=binomial)
summary(n.glm.global)

n.glm1<-glm(Focal_Bite~Focal_Species_Code, data=responded, family=binomial(link=logit))
summary(n.glm1) # The higher probability of attack by Ansa makes sense since the stimulus were also Ansa
n.glm1.int<-logit2prob(coef(n.glm1)[1])
n.glm1.fac<-logit2prob(coef(n.glm1)[1]+(1*coef(n.glm1)[2]))

n.glm2<-glm(Focal_Bite~Focal_Sex, data=responded, family=binomial(link=logit))
summary(n.glm2) # Males were most likely to bite- Lots of error & no significance

n.glm3<-glm(Focal_Bite~Focal_Species_Code+Focal_Sex, data=responded, family=binomial(link=logit))
summary(n.glm3)

n.glm4<-glm(Focal_Bite~Focal_Species_Code+Stimulus_Sex, data=responded, family=binomial(link=logit))
summary(n.glm4)

n.glm5<-glm(Focal_Dewlap_extension~Focal_Species_Code, data=responded, family=binomial(link=logit))
summary(n.glm5) # It probably doesn't make much sense to include females in this analysis

responded.male<-subset(responded, Focal_Sex=="M")
responded.male$Focal_Sex<-droplevels(responded.male$Focal_Sex)

n.glm6<-glm(Focal_Dewlap_extension~Focal_Species_Code, data=responded.male, family=binomial(link=logit))
summary(n.glm6)

n.glm7<-glm(Focal_Headbob_Pushup~Focal_Species_Code, data=responded, family=binomial)
summary(n.glm7)

n.glm8<-glm(Focal_Bite~Focal_Dewlap_extension, data=responded, family=binomial)
summary(n.glm8)

n.glm9<-glm(Focal_Bite~Focal_Dewlap_extension+Focal_Species_Code, data=responded, family=binomial)
summary(n.glm9)

n.glm10<-glm(Focal_Bite~Focal_Headbob_Pushup, data=responded, family=binomial)
summary(n.glm10)

n.glm11<-glm(Focal_Bite~Focal_Headbob_Pushup+Focal_Species_Code, data=responded, family=binomial)
summary(n.glm11)
n.glm11.Anda<-logit2prob(coef(n.glm11)[1])
n.glm11.Anda.bob<-logit2prob(coef(n.glm11)[1]+(1*coef(n.glm11)[2]))
n.glm11.Ansa<-logit2prob(coef(n.glm11)[1]+(1*coef(n.glm11)[3]))
n.glm11.Ansa.bob<-logit2prob(coef(n.glm11)[1]+(1*coef(n.glm11)[2])+(1*coef(n.glm11)[3]))

n.glm12<-glm(Focal_Bite~Stimulus_SVL, data=responded, family=binomial)
summary(n.glm12)

n.glm13<-glm(Focal_H)

# Chi-square analyses
table(responded$Focal_Species_Code, responded$outcome)
chisq.test(responded$Focal_Species_Code, responded$outcome, correct=F)

table(responded.kn$Focal_Sex, responded.kn$Focal_Bite)
chisq.test(responded.kn$Focal_Sex, responded.kn$Focal_Bite, correct=F)

table(responded.male$Focal_Species_Code, responded.male$Focal_Dewlap_extension)
chisq.test(responded.male$Focal_Species_Code, responded.male$Focal_Dewlap_extension, correct=F) # How many actually ended up biting?

table(responded$Focal_Species_Code, responded$Focal_Headbob_Pushup)
chisq.test(responded$Focal_Species_Code, responded$Focal_Headbob_Pushup, correct=F)

table(responded$Focal_Dewlap_extension, responded$Focal_Bite)
chisq.test(responded$Focal_Dewlap_extension, responded$Focal_Bite, correct=F)

# Chi square comparing male attack frequency when matched vs. mismatched
responded.kn.male<-subset(responded.kn, Focal_Sex=="M")

table(responded.kn.male$matched, responded.kn.male$Focal_Bite)
chisq.test(responded.kn.male$matched, responded.kn.male$Focal_Bite, correct=F)

# Consider pooling females and unknowns
e<-subset(responded, Focal_Sex=="M")
e$pooled.sex<-"M"
f<-subset(responded, Focal_Sex!="M")
f$pooled.sex<-"PF"
pooled.res<-rbind(e,f)
pooled.res$pooled.sex<-factor(pooled.res$pooled.sex)

table(pooled.res$pooled.sex, pooled.res$Focal_Bite)
chisq.test(pooled.res$pooled.sex, pooled.res$Focal_Bite, correct=F)
# We pooled the lizards of unknown sex with females because females are often less conspicuous than males,
# meaning that lizards that were not identifiable to sex from a distance were most likely females. However,
# it is possible at least some of these unsexed individuals may have been immature males. 

p.glm1<-glm(Focal_Bite~Focal_Species_Code*pooled.sex, data=pooled.res, family=binomial(link=logit))
summary(p.glm1)

p.glm2<-glm(Focal_Bite~pooled.sex, data=pooled.res, family=binomial(link=logit))
summary(p.glm2)

# I should try calling pooled unknowns matched with stimulus females

# Breakdown of attacked vs. didn't attack
attacked<-subset(responded, outcome=="1")
fled<-subset(responded, outcome=="0")

aggregate(Focal_Bite~Focal_Species_Code, sum, data=attacked)
aggregate(Focal_Bite_Latency~Focal_Species_Code, mean, data=attacked)
mean(attacked$Focal_Bite_Latency)
var(attacked$Focal_Bite_Latency)
summary(glm(Focal_Bite_Latency~Focal_Species_Code, data=attacked, family=quasipoisson(link=log)))



