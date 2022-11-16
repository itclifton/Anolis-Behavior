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
glm3<-glm(Focal_Headbob_Pushup~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
summary(glm3)
Anova(glm3)
glm3.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm3.1)
Probability3<-as.data.frame(c(logit2prob(coef(glm3)[1]),logit2prob(coef(glm3)[1]+(1*coef(glm3)[2]))))
colnames(Probability3)<-"Probability"
Probability3$Response<-c("No","Yes")

fig3a<-ggplot(Probability3, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5),
  plot.background = element_rect(colour = "red", fill=NA, size=4))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="A", size=7)

glm4<-glm(Focal_Headbob_Pushup~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm4)
summary(glm4)
glm4.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm4.1)
table(data1$Focal_Headbob_Pushup, data1$Stim_Dewlap_extension)
Probability4<-as.data.frame(c(logit2prob(coef(glm4)[1]),logit2prob(coef(glm4)[1]+(1*coef(glm4)[2]))))
colnames(Probability4)<-"Probability"
Probability4$Response<-c("No","Yes")

fig3b<-ggplot(Probability4, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5),
  plot.background = element_rect(colour = "red", fill=NA, size=4))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="B", size=7)

glm5<-glm(Focal_Headbob_Pushup~Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm5)
glm5.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm5.1)
table(data1$Focal_Headbob_Pushup, data1$Stim_Bite)
Probability5<-as.data.frame(c(logit2prob(coef(glm5)[1]),logit2prob(coef(glm5)[1]+(1*coef(glm5)[2]))))
colnames(Probability5)<-"Probability"
Probability5$Response<-c("No","Yes")

fig3c<-ggplot(Probability5, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="C", size=7)

glm6<-glm(Focal_Headbob_Pushup~Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm6)
glm6.1<-glm(Focal_Headbob_Pushup~Focal_Species_Code+Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm6.1)
table(data1$Focal_Headbob_Pushup, data1$Stim_Flee)
Probability6<-as.data.frame(c(logit2prob(coef(glm6)[1]),logit2prob(coef(glm6)[1]+(1*coef(glm6)[2]))))
colnames(Probability6)<-"Probability"
Probability6$Response<-c("No","Yes")

fig3d<-ggplot(Probability6, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="D", size=7)

# Dewlaps
glm7<-glm(Focal_Dewlap_extension~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm7)
glm7.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm7.1)
table(data1$Focal_Dewlap_extension, data1$Stim_Headbob_Pushup)
Probability7<-as.data.frame(c(logit2prob(coef(glm7)[1]),logit2prob(coef(glm7)[1]+(1*coef(glm7)[2]))))
colnames(Probability7)<-"Probability"
Probability7$Response<-c("No","Yes")

fig3e<-ggplot(Probability7, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none",
  plot.background = element_rect(colour = "red", fill=NA, size=4))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="E", size=7)

glm8<-glm(Focal_Dewlap_extension~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm8)
glm8.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm8.1)
table(data1$Focal_Dewlap_extension, data1$Stim_Dewlap_extension)
Probability8<-as.data.frame(c(logit2prob(coef(glm8)[1]),logit2prob(coef(glm8)[1]+(1*coef(glm8)[2]))))
colnames(Probability8)<-"Probability"
Probability8$Response<-c("No","Yes")

fig3f<-ggplot(Probability8, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none",
  plot.background = element_rect(colour = "red", fill=NA, size=4))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="F", size=7)

glm9<-glm(Focal_Dewlap_extension~Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm9)
glm9.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm9.1)
table(data1$Focal_Dewlap_extension, data1$Stim_Bite)
Probability9<-as.data.frame(c(logit2prob(coef(glm9)[1]),logit2prob(coef(glm9)[1]+(1*coef(glm9)[2]))))
colnames(Probability9)<-"Probability"
Probability9$Response<-c("No","Yes")

fig3g<-ggplot(Probability9, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="G", size=7)

glm10<-glm(Focal_Dewlap_extension~Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm10)
glm10.1<-glm(Focal_Dewlap_extension~Focal_Species_Code+Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm10.1)
table(data1$Focal_Dewlap_extension, data1$Stim_Flee)
Probability10<-as.data.frame(c(logit2prob(coef(glm10)[1]),logit2prob(coef(glm10)[1]+(1*coef(glm10)[2]))))
colnames(Probability10)<-"Probability"
Probability10$Response<-c("No","Yes")

fig3h<-ggplot(Probability10, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="H", size=7)

# Bite
glm11<-glm(Focal_Bite~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm11)
glm11.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm11.1)
summary(glm11.1) # If I'm reading this correctly, Ansa was more likely to bite if the intruder headbobbed than distichus
table(data1$Focal_Bite, data1$Stim_Headbob_Pushup)
Probability11<-as.data.frame(c(logit2prob(coef(glm11)[1]),logit2prob(coef(glm11)[1]+(1*coef(glm11)[2]))))
colnames(Probability11)<-"Probability"
Probability11$Response<-c("No","Yes")

fig3i<-ggplot(Probability11, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="I", size=7)

glm12<-glm(Focal_Bite~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm12)
glm12.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm12.1)
summary(glm12.1) # If I'm reading this correctly, Ansa was more likely to bite if the intruder extended their dewlap than distichus
table(data1$Focal_Bite, data1$Stim_Dewlap_extension)
Probability12<-as.data.frame(c(logit2prob(coef(glm12)[1]),logit2prob(coef(glm12)[1]+(1*coef(glm12)[2]))))
colnames(Probability12)<-"Probability"
Probability12$Response<-c("No","Yes")

fig3j<-ggplot(Probability12, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="J", size=7)

glm13<-glm(Focal_Bite~Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm13)
glm13.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm13.1)
summary(glm13.1) # If I'm reading this correctly, Ansa was more likely to bite if the intruder bit than distichus
table(data1$Focal_Bite, data1$Stim_Bite)
Probability13<-as.data.frame(c(logit2prob(coef(glm13)[1]),logit2prob(coef(glm13)[1]+(1*coef(glm13)[2]))))
colnames(Probability13)<-"Probability"
Probability13$Response<-c("No","Yes")

fig3k<-ggplot(Probability13, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="K", size=7)

glm14<-glm(Focal_Bite~Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm14)
glm14.1<-glm(Focal_Bite~Focal_Species_Code+Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm14.1)
table(data1$Focal_Bite, data1$Stim_Flee)
Probability14<-as.data.frame(c(logit2prob(coef(glm14)[1]),logit2prob(coef(glm14)[1]+(1*coef(glm14)[2]))))
colnames(Probability14)<-"Probability"
Probability14$Response<-c("No","Yes")

fig3l<-ggplot(Probability14, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="L", size=7)

# Flee
glm15<-glm(Focal_Flee~Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm15)
glm15.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Headbob_Pushup, data=data1, family=binomial(link=logit))
Anova(glm15.1)
summary(glm15.1) # It looks like Ansa were less likely to flee in response to a pushup than distichus were
table(data1$Focal_Flee, data1$Stim_Headbob_Pushup)
Probability15<-as.data.frame(c(logit2prob(coef(glm15)[1]),logit2prob(coef(glm15)[1]+(1*coef(glm15)[2]))))
colnames(Probability15)<-"Probability"
Probability15$Response<-c("No","Yes")

fig3m<-ggplot(Probability15, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="M", size=7)

glm16<-glm(Focal_Flee~Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm16)
glm16.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Dewlap_extension, data=data1, family=binomial(link=logit))
Anova(glm16.1)
summary(glm16.1) # It looks like Ansa were less likely to flee in response to a dewlap extension than distichus were
table(data1$Focal_Flee, data1$Stim_Dewlap_extension)
Probability16<-as.data.frame(c(logit2prob(coef(glm16)[1]),logit2prob(coef(glm16)[1]+(1*coef(glm16)[2]))))
colnames(Probability16)<-"Probability"
Probability16$Response<-c("No","Yes")

fig3n<-ggplot(Probability16, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none")+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="N", size=7)

glm17<-glm(Focal_Flee~Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm17)
glm17.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Bite, data=data1, family=binomial(link=logit))
Anova(glm17.1)
summary(glm17.1) # It looks like Ansa were less likely to flee in response to a dewlap extension than distichus were
table(data1$Focal_Flee, data1$Stim_Bite)
Probability17<-as.data.frame(c(logit2prob(coef(glm17)[1]),logit2prob(coef(glm17)[1]+(1*coef(glm17)[2]))))
colnames(Probability17)<-"Probability"
Probability17$Response<-c("No","Yes")

fig3o<-ggplot(Probability17, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none",
  plot.background = element_rect(colour = "red", fill=NA, size=4))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="O", size=7)

glm18<-glm(Focal_Flee~Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm18)
glm18.1<-glm(Focal_Flee~Focal_Species_Code+Stim_Flee, data=data1, family=binomial(link=logit))
Anova(glm18.1)
table(data1$Focal_Flee, data1$Stim_Flee)
Probability18<-as.data.frame(c(logit2prob(coef(glm18)[1]),logit2prob(coef(glm18)[1]+(1*coef(glm18)[2]))))
colnames(Probability18)<-"Probability"
Probability18$Response<-c("No","Yes")

fig3p<-ggplot(Probability18, aes(x=Response, y=Probability))+
  geom_col(position=position_dodge(0.7), colour="black", width=.7)+
  theme_classic()+ # New way of getting white background without adding a box around the plot- Replaces theme_bw()
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.4, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none",
  plot.background = element_rect(colour = "red", fill=NA, size=4))+
  scale_x_discrete(labels = c('No','Yes'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  annotate("text", x=0.6, y=.95, fontface="bold", label="P", size=7)

Fig3<-plot_grid(fig3a, fig3b, fig3c, fig3d,
                fig3e, fig3f, fig3g, fig3h,
                fig3i, fig3j, fig3k, fig3l,
                fig3m, fig3n, fig3o, fig3p,
                 ncol = 4, scale=0.99)+
  draw_label("Headbob or Pushup", size=28, fontface="bold", x=0.125, y=1, vjust=-0.5, angle= 0)+
  draw_label("Dewlap Extension", size=28, fontface="bold", x=0.375, y=1, vjust=-0.5, angle= 0)+
  draw_label("Attack", size=28, fontface="bold", x=0.625, y=1, vjust=-0.5, angle= 0)+
  draw_label("Flee", size=28, fontface="bold", x=0.875, y=1, vjust=-0.5, angle= 0)+
  draw_label("Flee", size=28, fontface="bold", x=-0.03, y=0.125, vjust= 1.5, angle=90)+
  draw_label("Attack", size=28, fontface="bold", x=-0.03, y=0.375, vjust= 1.5, angle=90)+
  draw_label("Dewlap Extension", size=28, fontface="bold", x=-0.03, y=0.625, vjust= 1.5, angle=90)+
  draw_label("Headbob or Pushup", size=28, fontface="bold", x=-0.03, y=0.875, vjust= 1.5, angle=90)+
  theme(plot.margin = margin(50, 5.5, 5.5, 50, "pt"))
# Add common axes
y.grob <- textGrob("Focal Lizard Behavior", 
                   gp=gpar(fontface="bold", fontsize=60), rot=90)
x.grob <- textGrob("Stimulus Lizard Behavior", 
                   gp=gpar(fontface="bold", fontsize=60))
Fig3.common<-grid.arrange(arrangeGrob(Fig3, left = y.grob, bottom = x.grob))
#ggsave("Fig3.jpeg", width=20, height=16, plot=Fig3.common)






