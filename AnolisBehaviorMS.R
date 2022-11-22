## Packages ----
library(arm)
library(rv)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(patchwork)
library(MASS)
library(car)
library(emmeans)

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

data1<-subset(data1, Focal_ID!="MJAnsaF12") # This trial seems to have ended when the stimulus lizard fled somehow.
# I changed the flee to N because the trial should have ended before the behavior was noted.
data1$Focal_Sex[is.na(data1$Focal_Sex)] <- "F"
data1$Focal_Dewlap_extension[is.na(data1$Focal_Dewlap_extension)] <- "0" # Change NAs to 0s because the behavior clearly didn't occur.
data1$Focal_Dewlap_extension<-as.numeric(data1$Focal_Dewlap_extension)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
## Summary Stuff ----
aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, length, data=data1)
aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, sum, data=data1)

## Graphs ----
## Figures after 7/15 meeting- IGNORE ALL THE ANALYSES BEFORE THIS ----

# Table 2
glm19<-glm(Focal_Headbob_Pushup~Focal_Species_Code*Focal_Sex, data=data1, family=binomial(link=logit))
summary(glm19)  
Anova(glm19)

  # Males only for glm20
glm20<-glm(Focal_Dewlap_extension~Focal_Species_Code, data=subset(data1, Focal_Sex=="M"), family=binomial(link=logit))
summary(glm20)
Anova(glm20)

glm21<-glm(Focal_Bite~Focal_Species_Code*Focal_Sex, data=data1, family=binomial(link=logit))
summary(glm21)
Anova(glm21)

glm22<-glm(Focal_Flee~Focal_Species_Code*Focal_Sex, data=data1, family=binomial(link=logit))
summary(glm22)
Anova(glm22)

# Figure 1- Frequency of attack and flee by species & latencies to attack and flee
glm1<-glm(Focal_Bite~Focal_Species_Code*Focal_Sex, data=data1, family=binomial(link=logit))
summary(glm1)
Anova(glm1)
p1.sum<-aggregate(Focal_Bite~Focal_Species_Code, sum, data=data1)
p1.length<-aggregate(Focal_Bite~Focal_Species_Code, length, data=data1)
p1.prop<-cbind(p1.sum, p1.length$Focal_Bite)
colnames(p1.prop)<-c("Species","Sum","Length")
p1.prop$Proportion<-(p1.prop$Sum/p1.prop$Length)*100

fig1a<-ggplot(p1.prop, aes(x=Species, y=Proportion))+
  geom_bar(stat="identity", position="dodge", colour="black")+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=28,face="bold"), axis.text.x=element_text(size=20,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_discrete(name = "Focal Sex")+
  xlab("")+
  ylab("Frequency of Attack (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="A", size=10)

glm2<-glm(Focal_Flee~Focal_Species_Code*Focal_Sex, data=data1, family=binomial(link=logit))
summary(glm2)
Anova(glm2)
p2.sum<-aggregate(Focal_Flee~Focal_Species_Code, sum, data=data1)
p2.length<-aggregate(Focal_Flee~Focal_Species_Code, length, data=data1)
p2.prop<-cbind(p2.sum, p2.length$Focal_Flee)
colnames(p2.prop)<-c("Species","Sum","Length")
p2.prop$Proportion<-(p2.prop$Sum/p2.prop$Length)*100

fig1b<-ggplot(p2.prop, aes(x=Species, y=Proportion))+
  geom_bar(stat="identity", position="dodge", colour="black")+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=28,face="bold"), axis.text.x=element_text(size=20,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_discrete(name = "Focal Sex")+
  xlab("")+
  ylab("Frequency of Flee (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="B", size=10)

lm3<-lm(Focal_Bite_Latency~Focal_Species_Code, data=data1)
Anova(lm3)
lm3a<-lm(log(Focal_Bite_Latency+1)~Focal_Species_Code, data=data1)
Anova(lm3a)

fig1c<-ggplot(data=data1, aes(x=Focal_Species_Code, y=Focal_Bite_Latency))+
  geom_boxplot(outlier.shape=NA, lwd=1)+
  geom_point(position=position_jitter(seed=2,width=0.15), size=5)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=28,face="bold"), axis.text.x=element_text(size=20,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400), breaks=c(0,100,200,300,400))+
  theme(legend.position="")+
  xlab("")+
  ylab("Focal Attack Latency (s)")+
  annotate("text", x=0.6, y=380, fontface="bold", label="C", size=10)

lm4<-lm(Focal_Flee_Latency~Focal_Species_Code, data=data1)
Anova(lm4)
lm4a<-glm(log(Focal_Flee_Latency+1)~Focal_Species_Code, data=data1)
Anova(lm4a)

fig1d<-ggplot(data=data1, aes(x=Focal_Species_Code, y=Focal_Flee_Latency))+
  geom_boxplot(outlier.shape=NA, lwd=1)+
  geom_point(position=position_jitter(seed=2,width=0.15), size=5)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=28,face="bold"), axis.text.x=element_text(size=20,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 600), breaks=c(0,150,300,450,600))+
  theme(legend.position="")+
  xlab("")+
  ylab("Focal Flee Latency (s)")+
  annotate("text", x=0.6, y=570, fontface="bold", label="D", size=10)

Fig1=plot_grid(fig1a, fig1b, fig1c, fig1d,
                   labels = "", nrow = 1, align="v")
ggsave("Fig1.jpeg", width=20, height=8, plot=Fig1)

StimM<-subset(data1, Stimulus_Sex=="M")
StimF<-subset(data1, Stimulus_Sex=="F")

Anova(glm(Focal_Bite~Focal_Species_Code+Focal_Sex, data=StimM, family=binomial(link=logit)))
p3.sum<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, sum, data=StimM)
p3.length<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, length, data=StimM)
p3.prop<-cbind(p3.sum, p3.length$Focal_Bite)
colnames(p3.prop)<-c("Species","Sex","Sum","Length")
p3.prop$Proportion<-(p3.prop$Sum/p3.prop$Length)*100

fig2a<-ggplot(p3.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Attack (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="A", size=8)

Anova(glm(Focal_Flee~Focal_Species_Code+Focal_Sex, data=StimM, family=binomial(link=logit)))
p5.sum<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, sum, data=StimM)
p5.length<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, length, data=StimM)
p5.prop<-cbind(p5.sum, p5.length$Focal_Flee)
colnames(p5.prop)<-c("Species","Sex","Sum","Length")
p5.prop$Proportion<-(p5.prop$Sum/p5.prop$Length)*100

fig2b<-ggplot(p5.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=18, face="bold"), legend.position = c(.85, .9))+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Flee (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="B", size=8)

StimMale<-plot_grid(fig2a, fig2b, nrow=1)
title_StimM <- ggdraw() +
  draw_label("Stimulus Male",
             size=28, fontface = "bold", hjust = 0.5)
StimMale.1<-plot_grid(title_StimM, StimMale, ncol = 1, rel_heights = c(0.2, 1))

Anova(glm(Focal_Bite~Focal_Species_Code+Focal_Sex, data=StimF, family=binomial(link=logit)))
p4.sum<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, sum, data=StimF)
p4.length<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, length, data=StimF)
p4.prop<-cbind(p4.sum, p4.length$Focal_Bite)
colnames(p4.prop)<-c("Species","Sex","Sum","Length")
p4.prop$Proportion<-(p4.prop$Sum/p4.prop$Length)*100

fig2c<-ggplot(p4.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Attack (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="C", size=8)

Anova(glm(Focal_Flee~Focal_Species_Code+Focal_Sex, data=StimF, family=binomial(link=logit)))
p6.sum<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, sum, data=StimF)
p6.length<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, length, data=StimF)
p6.prop<-cbind(p6.sum, p6.length$Focal_Flee)
colnames(p6.prop)<-c("Species","Sex","Sum","Length")
p6.prop$Proportion<-(p6.prop$Sum/p6.prop$Length)*100

fig2d<-ggplot(p6.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Flee (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="D", size=8)

StimFemale<-plot_grid(fig2c, fig2d, nrow=1)
title_StimF <- ggdraw() +
  draw_label("Stimulus Female",
             size=28, fontface = "bold", hjust = 0.5)
StimFemale.1<-plot_grid(title_StimF, StimFemale, ncol = 1, rel_heights = c(0.2, 1))

# Matched and mismatched
dat1.matched<-subset(data1, Stimulus_Sex==Focal_Sex)

Anova(glm(Focal_Bite~Focal_Species_Code+Focal_Sex, data=dat1.matched, family=binomial(link=logit)))
p7.sum<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, sum, data=dat1.matched)
p7.length<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, length, data=dat1.matched)
p7.prop<-cbind(p7.sum, p7.length$Focal_Bite)
colnames(p7.prop)<-c("Species","Sex","Sum","Length")
p7.prop$Proportion<-(p7.prop$Sum/p7.prop$Length)*100

fig2e<-ggplot(p7.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Attack (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="E", size=8)

Anova(glm(Focal_Flee~Focal_Species_Code+Focal_Sex, data=dat1.matched, family=binomial(link=logit)))
p8.sum<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, sum, data=dat1.matched)
p8.length<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, length, data=dat1.matched)
p8.prop<-cbind(p8.sum, p8.length$Focal_Flee)
colnames(p8.prop)<-c("Species","Sex","Sum","Length")
p8.prop$Proportion<-(p8.prop$Sum/p8.prop$Length)*100

fig2f<-ggplot(p8.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Flee (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="F", size=8)

Matched<-plot_grid(fig2e, fig2f, nrow=1)
title_Matched <- ggdraw() +
  draw_label("Matched Sexes",
             size=28, fontface = "bold", hjust = 0.5)
Matched.1<-plot_grid(title_Matched, Matched, ncol = 1, rel_heights = c(0.2, 1))

dat1.mis<-subset(data1, Stimulus_Sex!=Focal_Sex)
Anova(glm(Focal_Bite~Focal_Species_Code+Focal_Sex, data=dat1.mis, family=binomial(link=logit)))
p9.sum<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, sum, data=dat1.mis)
p9.length<-aggregate(Focal_Bite~Focal_Species_Code+Focal_Sex, length, data=dat1.mis)
p9.prop<-cbind(p9.sum, p9.length$Focal_Bite)
colnames(p9.prop)<-c("Species","Sex","Sum","Length")
p9.prop$Proportion<-(p9.prop$Sum/p9.prop$Length)*100

fig2g<-ggplot(p9.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Attack (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="G", size=8)

Anova(glm(Focal_Flee~Focal_Species_Code+Focal_Sex, data=dat1.mis, family=binomial(link=logit)))
p10.sum<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, sum, data=dat1.mis)
p10.length<-aggregate(Focal_Flee~Focal_Species_Code+Focal_Sex, length, data=dat1.mis)
p10.prop<-cbind(p10.sum, p10.length$Focal_Flee)
colnames(p10.prop)<-c("Species","Sex","Sum","Length")
p10.prop$Proportion<-(p10.prop$Sum/p10.prop$Length)*100

fig2h<-ggplot(p10.prop, aes(x=Species, y=Proportion, fill=Sex))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=21,face="bold"), axis.title=element_text(size=22,face="bold"), axis.text.x=element_text(size=21,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('Bark Anole','Brown Anole'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks=c(0,25,50,75,100))+ # Forces the y-axis to start at zero and end at 100
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name="Focal Sex")+
  xlab("")+
  ylab("Frequency of Flee (%)")+
  annotate("text", x=0.6, y=95, fontface="bold", label="H", size=8)

Mismatched<-plot_grid(fig2g, fig2h, nrow=1)
title_Mismatched <- ggdraw() +
  draw_label("Mismatched Sexes",
             size=28, fontface = "bold", hjust = 0.5)
Mismatched.1<-plot_grid(title_Mismatched, Mismatched, ncol = 1, rel_heights = c(0.2, 1))

Fig2<-plot_grid(StimMale.1, StimFemale.1,
                Matched.1, Mismatched.1,
               labels = "", ncol = 1, align="v")

ggsave("Fig2.jpeg", width=12, height=18, plot=Fig2)

# Behavioral Dependencies
# Headbobs & Pushups
data1.Ansa<-subset(data1, Focal_Species_Code=="Ansa")
data1.Andi<-subset(data1, Focal_Species_Code=="Andi")

glm3<-glm(Focal_Headbob_Pushup~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm3)
Probability3<-as.data.frame(c(logit2prob(coef(glm3)[1]),logit2prob(coef(glm3)[1]+(1*coef(glm3)[2]))))
colnames(Probability3)<-"Probability"
Probability3$Response<-c("No","Yes")
glm3.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm3.1)
glm3.sa<-glm(Focal_Headbob_Pushup~Stim_Headbob_Pushup, data=data1.Ansa, family=binomial(link=logit))
Anova(glm3.sa)
glm3.di<-glm(Focal_Headbob_Pushup~Stim_Headbob_Pushup, data=data1.Andi, family=binomial(link=logit))
Anova(glm3.di)
Probability3.sa<-as.data.frame(c(logit2prob(coef(glm3.sa)[1]),logit2prob(coef(glm3.sa)[1]+(1*coef(glm3.sa)[2]))))
colnames(Probability3.sa)<-"Probability"
Probability3.sa$Response<-c("No","Yes")
Probability3.sa$Species<-"Ansa"
Probability3.di<-as.data.frame(c(logit2prob(coef(glm3.di)[1]),logit2prob(coef(glm3.di)[1]+(1*coef(glm3.di)[2]))))
colnames(Probability3.di)<-"Probability"
Probability3.di$Response<-c("No","Yes")
Probability3.di$Species<-"Andi"
Probability3.sp<-rbind(Probability3.sa, Probability3.di)

fig3a<-ggplot(Probability3.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="A", size=7)+
  annotate("text", x=0.63, y=.87, label="Species, P=0.20", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, fontface="bold", label="Behavior, P<0.01", size=6, hjust=0)+
  coord_cartesian(clip = "off")

glm4<-glm(Focal_Headbob_Pushup~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm4)
table(data1$Focal_Headbob_Pushup, data1$Stim_Dewlap_extension)
Probability4<-as.data.frame(c(logit2prob(coef(glm4)[1]),logit2prob(coef(glm4)[1]+(1*coef(glm4)[2]))))
colnames(Probability4)<-"Probability"
Probability4$Response<-c("No","Yes")
glm4.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm4.1)
glm4.sa<-glm(Focal_Headbob_Pushup~Stim_Dewlap_extension, data=data1.Ansa, family=binomial(link=logit))
Anova(glm4.sa)
glm4.di<-glm(Focal_Headbob_Pushup~Stim_Dewlap_extension, data=data1.Andi, family=binomial(link=logit))
Anova(glm4.di)
Probability4.sa<-as.data.frame(c(logit2prob(coef(glm4.sa)[1]),logit2prob(coef(glm4.sa)[1]+(1*coef(glm4.sa)[2]))))
colnames(Probability4.sa)<-"Probability"
Probability4.sa$Response<-c("No","Yes")
Probability4.sa$Species<-"Ansa"
Probability4.di<-as.data.frame(c(logit2prob(coef(glm4.di)[1]),logit2prob(coef(glm4.di)[1]+(1*coef(glm4.di)[2]))))
colnames(Probability4.di)<-"Probability"
Probability4.di$Response<-c("No","Yes")
Probability4.di$Species<-"Andi"
Probability4.sp<-rbind(Probability4.sa, Probability4.di)
Prob4.yes<-subset(Probability4.sp, Response=="Yes")
Prob4.no<-subset(Probability4.sp, Response=="No")

fig3b<-ggplot(Probability4.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="B", size=7)+
  annotate("text", x=0.63, y=.87, label="Species, P=0.36", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, fontface="bold", label="Behavior, P=0.17", size=6, hjust=0)+
  coord_cartesian(clip = "off")
# 
# glm5<-glm(Focal_Headbob_Pushup~Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm5)
# table(data1$Focal_Headbob_Pushup, data1$Stim_Bite)
# Probability5<-as.data.frame(c(logit2prob(coef(glm5)[1]),logit2prob(coef(glm5)[1]+(1*coef(glm5)[2]))))
# colnames(Probability5)<-"Probability"
# Probability5$Response<-c("No","Yes")
# glm5.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm5.1)
# glm5.sa<-glm(Focal_Headbob_Pushup~Stim_Bite, data=data1.Ansa, family=binomial(link=logit))
# Anova(glm5.sa)
# summary(glm5.sa)
# glm5.di<-glm(Focal_Headbob_Pushup~Stim_Bite, data=data1.Andi, family=binomial(link=logit))
# Anova(glm5.di)
# Probability5.sa<-as.data.frame(c(logit2prob(coef(glm5.sa)[1]),logit2prob(coef(glm5.sa)[1]+(1*coef(glm5.sa)[2]))))
# colnames(Probability5.sa)<-"Probability"
# Probability5.sa$Response<-c("No","Yes")
# Probability5.sa$Species<-"Ansa"
# Probability5.di<-as.data.frame(c(logit2prob(coef(glm5.di)[1]),logit2prob(coef(glm5.di)[1]+(1*coef(glm5.di)[2]))))
# colnames(Probability5.di)<-"Probability"
# Probability5.di$Response<-c("No","Yes")
# Probability5.di$Species<-"Andi"
# Probability5.sp<-rbind(Probability5.sa, Probability5.di)
# 
# fig3c<-ggplot(Probability5.sp, aes(x=Response, y=Probability, group=Species))+
#   geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
#   theme_classic()+
#   theme(axis.title.x=element_blank())+
#   theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
#   theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
#   theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
#   scale_x_discrete(labels = c('No','Yes'))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
#   xlab("")+
#   ylab("")+
#   scale_fill_manual(values=c('#ABABAB','#000000'))+
#   annotate("text", x=0.52, y=.95, fontface="bold", label="C", size=7)+
#   annotate("text", x=0.63, y=.87, label="Species, P=0.28", size=6, hjust=0)+
#   annotate("text", x=0.63, y=.8, label="Behavior, P=0.40", size=6, hjust=0)+
#   coord_cartesian(clip = "off")

# Dewlaps
glm7<-glm(Focal_Dewlap_extension~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm7)
table(data1$Focal_Dewlap_extension, data1$Stim_Headbob_Pushup)
Probability7<-as.data.frame(c(logit2prob(coef(glm7)[1]),logit2prob(coef(glm7)[1]+(1*coef(glm7)[2]))))
colnames(Probability7)<-"Probability"
Probability7$Response<-c("No","Yes")
glm7.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm7.1)
glm7.sa<-glm(Focal_Dewlap_extension~Stim_Headbob_Pushup, data=data1.Ansa, family=binomial(link=logit))
Anova(glm7.sa)
glm7.di<-glm(Focal_Dewlap_extension~Stim_Headbob_Pushup, data=data1.Andi, family=binomial(link=logit))
Anova(glm7.di)
Probability7.sa<-as.data.frame(c(logit2prob(coef(glm7.sa)[1]),logit2prob(coef(glm7.sa)[1]+(1*coef(glm7.sa)[2]))))
colnames(Probability7.sa)<-"Probability"
Probability7.sa$Response<-c("No","Yes")
Probability7.sa$Species<-"Ansa"
Probability7.di<-as.data.frame(c(logit2prob(coef(glm7.di)[1]),logit2prob(coef(glm7.di)[1]+(1*coef(glm7.di)[2]))))
colnames(Probability7.di)<-"Probability"
Probability7.di$Response<-c("No","Yes")
Probability7.di$Species<-"Andi"
Probability7.sp<-rbind(Probability7.sa, Probability7.di)

fig3c<-ggplot(Probability7.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="C", size=7)+
  annotate("text", x=0.63, y=.87, label="Species, P=0.99", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, fontface="bold", label="Behavior, P=0.26", size=6, hjust=0)+
  coord_cartesian(clip = "off")

glm8<-glm(Focal_Dewlap_extension~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm8)
table(data1$Focal_Dewlap_extension, data1$Stim_Dewlap_extension)
Probability8<-as.data.frame(c(logit2prob(coef(glm8)[1]),logit2prob(coef(glm8)[1]+(1*coef(glm8)[2]))))
colnames(Probability8)<-"Probability"
Probability8$Response<-c("No","Yes")
glm8.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm8.1)
glm8.sa<-glm(Focal_Dewlap_extension~Stim_Dewlap_extension, data=data1.Ansa, family=binomial(link=logit))
Anova(glm8.sa)
glm8.di<-glm(Focal_Dewlap_extension~Stim_Dewlap_extension, data=data1.Andi, family=binomial(link=logit))
Anova(glm8.di)
Probability8.sa<-as.data.frame(c(logit2prob(coef(glm8.sa)[1]),logit2prob(coef(glm8.sa)[1]+(1*coef(glm8.sa)[2]))))
colnames(Probability8.sa)<-"Probability"
Probability8.sa$Response<-c("No","Yes")
Probability8.sa$Species<-"Ansa"
Probability8.di<-as.data.frame(c(logit2prob(coef(glm8.di)[1]),logit2prob(coef(glm8.di)[1]+(1*coef(glm8.di)[2]))))
colnames(Probability8.di)<-"Probability"
Probability8.di$Response<-c("No","Yes")
Probability8.di$Species<-"Andi"
Probability8.sp<-rbind(Probability8.sa, Probability8.di)

fig3d<-ggplot(Probability8.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="D", size=7)+
  annotate("text", x=0.63, y=.87, label="Species, P=0.63", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, fontface="bold", label="Behavior, P<0.01", size=6, hjust=0)+
  coord_cartesian(clip = "off")
# 
# glm9<-glm(Focal_Dewlap_extension~Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm9)
# table(data1$Focal_Dewlap_extension, data1$Stim_Bite)
# Probability9<-as.data.frame(c(logit2prob(coef(glm9)[1]),logit2prob(coef(glm9)[1]+(1*coef(glm9)[2]))))
# colnames(Probability9)<-"Probability"
# Probability9$Response<-c("No","Yes")
# glm9.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm9.1)
# glm9.sa<-glm(Focal_Dewlap_extension~Stim_Bite, data=data1.Ansa, family=binomial(link=logit))
# Anova(glm9.sa)
# glm9.di<-glm(Focal_Dewlap_extension~Stim_Bite, data=data1.Andi, family=binomial(link=logit))
# Anova(glm9.di)
# Probability9.sa<-as.data.frame(c(logit2prob(coef(glm9.sa)[1]),logit2prob(coef(glm9.sa)[1]+(1*coef(glm9.sa)[2]))))
# colnames(Probability9.sa)<-"Probability"
# Probability9.sa$Response<-c("No","Yes")
# Probability9.sa$Species<-"Ansa"
# Probability9.di<-as.data.frame(c(logit2prob(coef(glm9.di)[1]),logit2prob(coef(glm9.di)[1]+(1*coef(glm9.di)[2]))))
# colnames(Probability9.di)<-"Probability"
# Probability9.di$Response<-c("No","Yes")
# Probability9.di$Species<-"Andi"
# Probability9.sp<-rbind(Probability9.sa, Probability9.di)
# Prob9.yes<-subset(Probability9.sp, Response=="Yes")
# Prob9.no<-subset(Probability9.sp, Response=="No")
# 
# fig3f<-ggplot(Probability9.sp, aes(x=Response, y=Probability, group=Species))+
#   geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
#   theme_classic()+
#   theme(axis.title.x=element_blank())+
#   theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
#   theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
#   theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
#   scale_x_discrete(labels = c('No','Yes'))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
#   xlab("")+
#   ylab("")+
#   scale_fill_manual(values=c('#ABABAB','#000000'))+
#   annotate("text", x=0.52, y=.95, fontface="bold", label="F", size=7)+
#   annotate("text", x=0.63, y=.87, label="Species, P=0.93", size=6, hjust=0)+
#   annotate("text", x=0.63, y=.8, label="Behavior, P=0.79", size=6, hjust=0)+
#   coord_cartesian(clip = "off")

# Bite
glm11<-glm(Focal_Bite~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm11)
table(data1$Focal_Bite, data1$Stim_Headbob_Pushup)
Probability11<-as.data.frame(c(logit2prob(coef(glm11)[1]),logit2prob(coef(glm11)[1]+(1*coef(glm11)[2]))))
colnames(Probability11)<-"Probability"
Probability11$Response<-c("No","Yes")
glm11.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm11.1)
summary(glm11.1) # If I'm reading this correctly, Ansa was more likely to bite if the intruder headbobbed than distichus
glm11.sa<-glm(Focal_Bite~Stim_Headbob_Pushup, data=data1.Ansa, family=binomial(link=logit))
Anova(glm11.sa)
glm11.di<-glm(Focal_Bite~Stim_Headbob_Pushup, data=data1.Andi, family=binomial(link=logit))
Anova(glm11.di)
Probability11.sa<-as.data.frame(c(logit2prob(coef(glm11.sa)[1]),logit2prob(coef(glm11.sa)[1]+(1*coef(glm11.sa)[2]))))
colnames(Probability11.sa)<-"Probability"
Probability11.sa$Response<-c("No","Yes")
Probability11.sa$Species<-"Ansa"
Probability11.di<-as.data.frame(c(logit2prob(coef(glm11.di)[1]),logit2prob(coef(glm11.di)[1]+(1*coef(glm11.di)[2]))))
colnames(Probability11.di)<-"Probability"
Probability11.di$Response<-c("No","Yes")
Probability11.di$Species<-"Andi"
Probability11.sp<-rbind(Probability11.sa, Probability11.di)
Prob11.yes<-subset(Probability11.sp, Response=="Yes")
Prob11.no<-subset(Probability11.sp, Response=="No")

fig3e<-ggplot(Probability11.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="E", size=7)+
  annotate("text", x=0.63, y=.87, fontface="bold", label="Species, P=0.022", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, label="Behavior, P=0.56", size=6, hjust=0)+
  coord_cartesian(clip = "off")

glm12<-glm(Focal_Bite~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm12)
table(data1$Focal_Bite, data1$Stim_Dewlap_extension)
Probability12<-as.data.frame(c(logit2prob(coef(glm12)[1]),logit2prob(coef(glm12)[1]+(1*coef(glm12)[2]))))
colnames(Probability12)<-"Probability"
Probability12$Response<-c("No","Yes")
glm12.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm12.1)
summary(glm12.1) # If I'm reading this correctly, Ansa was more likely to bite if the intruder extended their dewlap than distichus
glm12.sa<-glm(Focal_Bite~Stim_Dewlap_extension, data=data1.Ansa, family=binomial(link=logit))
Anova(glm12.sa)
glm12.di<-glm(Focal_Bite~Stim_Dewlap_extension, data=data1.Andi, family=binomial(link=logit))
Anova(glm12.di)
Probability12.sa<-as.data.frame(c(logit2prob(coef(glm12.sa)[1]),logit2prob(coef(glm12.sa)[1]+(1*coef(glm12.sa)[2]))))
colnames(Probability12.sa)<-"Probability"
Probability12.sa$Response<-c("No","Yes")
Probability12.sa$Species<-"Ansa"
Probability12.di<-as.data.frame(c(logit2prob(coef(glm12.di)[1]),logit2prob(coef(glm12.di)[1]+(1*coef(glm12.di)[2]))))
colnames(Probability12.di)<-"Probability"
Probability12.di$Response<-c("No","Yes")
Probability12.di$Species<-"Andi"
Probability12.sp<-rbind(Probability12.sa, Probability12.di)
Prob12.yes<-subset(Probability12.sp, Response=="Yes")
Prob12.no<-subset(Probability12.sp, Response=="No")

fig3f<-ggplot(Probability12.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="F", size=7)+
  annotate("text", x=0.63, y=.87, fontface="bold", label="Species, P=0.022", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, label="Behavior, P=0.86", size=6, hjust=0)+
  coord_cartesian(clip = "off")
# 
# glm13<-glm(Focal_Bite~Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm13)
# table(data1$Focal_Bite, data1$Stim_Bite)
# Probability13<-as.data.frame(c(logit2prob(coef(glm13)[1]),logit2prob(coef(glm13)[1]+(1*coef(glm13)[2]))))
# colnames(Probability13)<-"Probability"
# Probability13$Response<-c("No","Yes")
# glm13.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm13.1)
# summary(glm13.1) # If I'm reading this correctly, Ansa was more likely to bite if the intruder bit than distichus
# glm13.sa<-glm(Focal_Bite~Stim_Bite, data=data1.Ansa, family=binomial(link=logit))
# Anova(glm13.sa)
# glm13.di<-glm(Focal_Bite~Stim_Bite, data=data1.Andi, family=binomial(link=logit))
# Anova(glm13.di)
# Probability13.sa<-as.data.frame(c(logit2prob(coef(glm13.sa)[1]),logit2prob(coef(glm13.sa)[1]+(1*coef(glm13.sa)[2]))))
# colnames(Probability13.sa)<-"Probability"
# Probability13.sa$Response<-c("No","Yes")
# Probability13.sa$Species<-"Ansa"
# Probability13.di<-as.data.frame(c(logit2prob(coef(glm13.di)[1]),logit2prob(coef(glm13.di)[1]+(1*coef(glm13.di)[2]))))
# colnames(Probability13.di)<-"Probability"
# Probability13.di$Response<-c("No","Yes")
# Probability13.di$Species<-"Andi"
# Probability13.sp<-rbind(Probability13.sa, Probability13.di)
# Prob13.yes<-subset(Probability13.sp, Response=="Yes")
# Prob13.no<-subset(Probability13.sp, Response=="No")
# 
# fig3i<-ggplot(Probability13.sp, aes(x=Response, y=Probability, group=Species))+
#   geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
#   theme_classic()+
#   theme(axis.title.x=element_blank())+
#   theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
#   theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
#   theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
#   scale_x_discrete(labels = c('No','Yes'))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
#   xlab("")+
#   ylab("")+
#   scale_fill_manual(values=c('#ABABAB','#000000'))+
#   annotate("text", x=0.52, y=.95, fontface="bold", label="I", size=7)+
#   annotate("text", x=0.63, y=.87, fontface="bold", label="Species, P=0.018", size=6, hjust=0)+
#   annotate("text", x=0.63, y=.8, label="Behavior, P=0.17", size=6, hjust=0)+
#   coord_cartesian(clip = "off")

# Flee
glm15<-glm(Focal_Flee~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm15)
table(data1$Focal_Flee, data1$Stim_Headbob_Pushup)
Probability15<-as.data.frame(c(logit2prob(coef(glm15)[1]),logit2prob(coef(glm15)[1]+(1*coef(glm15)[2]))))
colnames(Probability15)<-"Probability"
Probability15$Response<-c("No","Yes")
glm15.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm15.1)
summary(glm15.1) # It looks like Ansa were less likely to flee in response to a pushup than distichus were
glm15.sa<-glm(Focal_Flee~Stim_Headbob_Pushup, data=data1.Ansa, family=binomial(link=logit))
Anova(glm15.sa)
glm15.di<-glm(Focal_Flee~Stim_Headbob_Pushup, data=data1.Andi, family=binomial(link=logit))
Anova(glm15.di)
Probability15.sa<-as.data.frame(c(logit2prob(coef(glm15.sa)[1]),logit2prob(coef(glm15.sa)[1]+(1*coef(glm15.sa)[2]))))
colnames(Probability15.sa)<-"Probability"
Probability15.sa$Response<-c("No","Yes")
Probability15.sa$Species<-"Ansa"
Probability15.di<-as.data.frame(c(logit2prob(coef(glm15.di)[1]),logit2prob(coef(glm15.di)[1]+(1*coef(glm15.di)[2]))))
colnames(Probability15.di)<-"Probability"
Probability15.di$Response<-c("No","Yes")
Probability15.di$Species<-"Andi"
Probability15.sp<-rbind(Probability15.sa, Probability15.di)
Prob15.yes<-subset(Probability15.sp, Response=="Yes")
Prob15.no<-subset(Probability15.sp, Response=="No")

fig3g<-ggplot(Probability15.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="G", size=7)+
  annotate("text", x=0.63, y=.87, fontface="bold", label="Species, P=0.020", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, label="Behavior, P=0.15", size=6, hjust=0)+
  coord_cartesian(clip = "off")

glm16<-glm(Focal_Flee~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm16)
table(data1$Focal_Flee, data1$Stim_Dewlap_extension)
Probability16<-as.data.frame(c(logit2prob(coef(glm16)[1]),logit2prob(coef(glm16)[1]+(1*coef(glm16)[2]))))
colnames(Probability16)<-"Probability"
Probability16$Response<-c("No","Yes")
glm16.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm16.1)
summary(glm16.1) # It looks like Ansa were less likely to flee in response to a dewlap extension than distichus were
glm16.sa<-glm(Focal_Flee~Stim_Dewlap_extension, data=data1.Ansa, family=binomial(link=logit))
Anova(glm16.sa)
glm16.di<-glm(Focal_Flee~Stim_Dewlap_extension, data=data1.Andi, family=binomial(link=logit))
Anova(glm16.di)
Probability16.sa<-as.data.frame(c(logit2prob(coef(glm16.sa)[1]),logit2prob(coef(glm16.sa)[1]+(1*coef(glm16.sa)[2]))))
colnames(Probability16.sa)<-"Probability"
Probability16.sa$Response<-c("No","Yes")
Probability16.sa$Species<-"Ansa"
Probability16.di<-as.data.frame(c(logit2prob(coef(glm16.di)[1]),logit2prob(coef(glm16.di)[1]+(1*coef(glm16.di)[2]))))
colnames(Probability16.di)<-"Probability"
Probability16.di$Response<-c("No","Yes")
Probability16.di$Species<-"Andi"
Probability16.sp<-rbind(Probability16.sa, Probability16.di)
Prob16.yes<-subset(Probability16.sp, Response=="Yes")
Prob16.no<-subset(Probability16.sp, Response=="No")

fig3h<-ggplot(Probability16.sp, aes(x=Response, y=Probability, group=Species))+
  geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  annotate("text", x=0.5, y=.95, fontface="bold", label="H", size=7)+
  annotate("text", x=0.63, y=.87, fontface="bold", label="Species, P=0.028", size=6, hjust=0)+
  annotate("text", x=0.63, y=.8, label="Behavior, P=0.58", size=6, hjust=0)+
  coord_cartesian(clip = "off")
# 
# glm17<-glm(Focal_Flee~Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm17)
# table(data1$Focal_Flee, data1$Stim_Bite)
# Probability17<-as.data.frame(c(logit2prob(coef(glm17)[1]),logit2prob(coef(glm17)[1]+(1*coef(glm17)[2]))))
# colnames(Probability17)<-"Probability"
# Probability17$Response<-c("No","Yes")
# glm17.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
# Anova(glm17.1)
# summary(glm17.1) # It looks like Ansa were less likely to flee in response to a dewlap extension than distichus were
# glm17.sa<-glm(Focal_Flee~Stim_Bite, data=data1.Ansa, family=binomial(link=logit))
# Anova(glm17.sa)
# glm17.di<-glm(Focal_Flee~Stim_Bite, data=data1.Andi, family=binomial(link=logit))
# Anova(glm17.di)
# Probability17.sa<-as.data.frame(c(logit2prob(coef(glm17.sa)[1]),logit2prob(coef(glm17.sa)[1]+(1*coef(glm17.sa)[2]))))
# colnames(Probability17.sa)<-"Probability"
# Probability17.sa$Response<-c("No","Yes")
# Probability17.sa$Species<-"Ansa"
# Probability17.di<-as.data.frame(c(logit2prob(coef(glm17.di)[1]),logit2prob(coef(glm17.di)[1]+(1*coef(glm17.di)[2]))))
# colnames(Probability17.di)<-"Probability"
# Probability17.di$Response<-c("No","Yes")
# Probability17.di$Species<-"Andi"
# Probability17.sp<-rbind(Probability17.sa, Probability17.di)
# Prob17.yes<-subset(Probability17.sp, Response=="Yes")
# Prob17.no<-subset(Probability17.sp, Response=="No")
# 
# fig3l<-ggplot(Probability17.sp, aes(x=Response, y=Probability, group=Species))+
#   geom_col(aes(fill=Species), position=position_dodge(0.7), width=.7)+
#   theme_classic()+
#   theme(axis.title.x=element_blank())+
#   theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
#   theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
#   theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
#   scale_x_discrete(labels = c('No','Yes'))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
#   xlab("")+
#   ylab("")+
#   scale_fill_manual(values=c('#ABABAB','#000000'))+
#   annotate("text", x=0.52, y=.95, fontface="bold", label="L", size=7)+
#   annotate("text", x=0.63, y=.87, fontface="bold", label="Species, P=0.016", size=6, hjust=0)+
#   annotate("text", x=0.63, y=.8, fontface="bold", label="Behavior, P=0.019", size=6, hjust=0)+
#   coord_cartesian(clip = "off")

Fig3<-plot_grid(fig3a, fig3b,
                fig3c, fig3d,
                fig3e, fig3f,
                fig3g, fig3h,
                 ncol = 2, scale=0.99)+
  draw_label("Headbob/Pushup", size=28, fontface="bold", x=0.25, y=1, vjust=-0.5, angle= 0)+
  draw_label("Dewlap Extension", size=28, fontface="bold", x=0.75, y=1, vjust=-0.5, angle= 0)+
  draw_label("Flee", size=28, fontface="bold", x=-0.03, y=0.125, vjust= 1.5, angle=90)+
  draw_label("Attack", size=28, fontface="bold", x=-0.03, y=0.375, vjust= 1.5, angle=90)+
  draw_label("Dewlap Extension", size=28, fontface="bold", x=-0.03, y=0.625, vjust= 1.5, angle=90)+
  draw_label("Headbob/Pushup", size=28, fontface="bold", x=-0.03, y=0.875, vjust= 1.5, angle=90)+
  theme(plot.margin = margin(50, 5.5, 5.5, 50, "pt"))
# Add common axes
y.grob <- textGrob("Focal Lizard Behavior", 
                   gp=gpar(fontface="bold", fontsize=40), rot=90)
x.grob <- textGrob("Stimulus Lizard Behavior", 
                   gp=gpar(fontface="bold", fontsize=40))
Fig3.common<-grid.arrange(arrangeGrob(Fig3, left = y.grob, bottom = x.grob))
ggsave("Fig3.1.jpeg", width=20, height=16, plot=Fig3.common)




