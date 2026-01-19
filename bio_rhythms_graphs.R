#correction graphs
#chapter2
rm(list=ls())
data<- read.csv("originalembryocort_qpcrdata.csv", sep=",")

head(data)

library(dplyr)
library(plyr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

groups <- c("Prenatal control/\nPostnatal control", "Prenatal control/\nPostnatal stress", "Prenatal CORT/\nPostnatal control", "Prenatal CORT/\nPostnatal stress")

my_per_title <- expression(paste("Relative ", italic("Per2"), " Expression"))

my_bmal_title <- expression(paste("Relative ", italic("Bmal1"), " Expression"))

# scale_x_discrete(labels=groups)+
#   labs(y=my_bmal_title)+
#   labs(y=my_per_title)

library(ggplot2)

cleanup=theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background=element_blank(),
              axis.line=element_line(color = "black"))


data$bmal = as.numeric(data$bmal)
data$per = as.numeric(data$per)
data$act= as.numeric(data$act)


data$time<-as.factor(data$time)
#data$sex<-as.factor(data$sex)

data$time<- factor(data$time, levels = c("9am", "2pm", "5pm", "zt1", "zt10"))
data$condition<-as.factor(data$condition)
data$age<-as.factor(data$age)
data$batch<-as.factor(data$batch)


pren<- data[ which(data$stage== 'prenatal'),]
post<- data[ which(data$stage== 'postnatal'),]


e11<- data[ which(data$age== 'e11'),]
e14<- data[ which(data$age== 'e14'),]
e17<- data[ which(data$age== 'e17'),]
pd10<- data[ which(data$age== 'pd10'),]
pd28<- data[ which(data$age== 'pd28'),]

#bmal="royalblue2"

#time


data_long <- gather(data, gene, measurement, bmal:per, factor_key=TRUE)
data_long


#time only
stage_names <- c(
  'e11'="Embryonic day 11",
  'e14'="Embryonic day 14",
  'e17'="Embryonic day 17",
  'pd10'="Post-natal day 10",
  'pd28'="Post-natal day 10",
  'bmal'= "Bmal1",
  'per'=  "Per2")


levels(stage_names) <- c("italic('Bmal1')", "italic('Per2')" )

ggplot(data_long, aes(y=measurement, x=time, fill=gene)) + 
  geom_boxplot()+
  scale_fill_manual(values = c("orange", "purple"))+
  cleanup+
  xlab("Time") +
  facet_grid(gene ~ age, scales="free", labeller = as_labeller(stage_names))+
 xlab("Time") + 
  ylab("Relative expression")+
 theme(strip.text.y.right  = element_text(face = "italic"))+
  scale_x_discrete(labels=c("zt1"="ZT1", "zt10"= "ZT10"))+
   theme(legend.position = "none")
  

#condition only

#use this is want fill=cond_age
# data_long <- as_tibble(data_long) 
# 
# data_long <- data_long %>% 
#   mutate(cond_age = paste(gene,condition,
#                            sep = "_")) 


levels(stage_names) <- c("italic('Bmal1')", "italic('Per2')" )

ggplot(data_long, aes(y=measurement, x=condition, fill=condition)) + 
  geom_boxplot()+
 scale_fill_manual(values = c("skyblue", "coral1"))+
  cleanup+
  xlab("Time") +
  facet_grid(gene ~ age, scales="free", labeller = as_labeller(stage_names))+
  xlab("Condition") + 
  ylab("Relative expression")+
  theme(strip.text.y.right  = element_text(face = "italic"))+
  scale_x_discrete(labels=c("control"="Control", "cort"= "Pre-natal CORT"))+
  theme(legend.position = "none")


#interactions


ggplot(data_long, aes(y=measurement, x=time)) + 
  geom_boxplot(aes(fill=condition))+
  scale_fill_manual(name="Condition", labels = c("Control", "Pre-natal CORT"),
                    values = c("skyblue", "coral1"))+
  cleanup+
  xlab("Time") +
  facet_grid(gene ~ age, scales="free", labeller = as_labeller(stage_names))+
  xlab("Time") + 
  ylab("Relative expression")+
  theme(strip.text.y.right  = element_text(face = "italic"))+
  scale_x_discrete(labels=c("zt1"="ZT1", "zt10"= "ZT10"))


