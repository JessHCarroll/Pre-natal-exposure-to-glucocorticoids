
rm(list=ls())
# preliminaries ####

## libraries ####
require(reshape2)
library(mgcv)
library(boot)
library(tidyverse)


theme_set(theme_bw())

## load data ####

#df_cort_wide<-read.csv("final_phd_paper_small.csv")

df_cort_wide<-read.csv("final_phd_paper_small_withpreen_final.csv")

df_cort_long <- df_cort_wide %>% 
  pivot_longer(cols = eatdrinkfor:rest,
               names_to = "behaviour",
               values_to = "secs_performed") %>% 
  mutate(secs_performed = secs_performed) 

#calcualte secs not performed.
df_cort_long <- df_cort_long %>%
  mutate(secs_not = 300 - secs_performed)

##flagged cages light turned of during recording, so less time samples. B3=96 SECS,C4=78 SECS, C7=84 SECS.
b3<- df_cort_long %>% filter(flag=='1' & cage== 'b3')%>%
  mutate(secs_not = 96 - secs_performed)

c4<- df_cort_long %>% filter(flag=='1' & cage== 'c4')%>%
  mutate(secs_not = 78 - secs_performed)

c7<- df_cort_long %>% filter(flag=='1' & cage== 'c7')%>%
  mutate(secs_not = 84 - secs_performed)

#remove original incorrect secs not from dataframe

df_cort_long <-df_cort_long  %>%
  filter(flag=='0')

#put in correct values for B3,C4 and C7

df_cort_long <-bind_rows(df_cort_long,b3,c4,c7)

#df_cort_long %>% filter(flag=='1')


df_cort_long$behaviour<-as.factor(df_cort_long$behaviour)
df_cort_long$sex<-as.factor(df_cort_long$sex)
df_cort_long$id<-as.factor(df_cort_long$id)
df_cort_long$treatment<-as.factor(df_cort_long$treatment)
df_cort_long$cage<-as.factor(df_cort_long$cage)

rest_data<-filter(df_cort_long, behaviour == "rest")

rest_data <- rest_data %>% 
  mutate(prop_behav = secs_performed / (secs_performed+secs_not))


df_rest <- as_tibble(rest_data) 

df_rest <- df_rest %>% 
  mutate(sex_treat = paste(sex,treatment,
                           sep = "_")) 


eat_data<-filter(df_cort_long, behaviour == "eatdrinkfor")

eat_data <- eat_data %>% 
  mutate(prop_behav = secs_performed / (secs_performed+secs_not))

df_eat <- as_tibble(eat_data) 

df_eat <- df_eat %>% 
  mutate(sex_treat = paste(sex,treatment,
                           sep = "_")) 


#####run models for eat

egam1<-gam(cbind(secs_performed,secs_not) ~
             s(zt, bs= "cc",k=8)+
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_eat,
           method = 'REML',
           family = "binomial")

egam2<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by=sex, bs= "cc",k=8)+
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_eat, 
           method = 'REML',
           family = "binomial")


egam3<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by=treatment, bs= "cc",k=6)+
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_eat, 
           method = 'REML',
           family = "binomial")

egam4<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by = factor(sex_treat), bs = "cc", k=8)+
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data = df_eat,
           method = 'REML',
           family = "binomial")

egam5<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by=treatment, bs= "cc", k = 6)+
             s(zt, by= sex, bs="cc", k = 6) +
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_eat, #df_cort_long
           method = 'REML',
           family = "binomial")

eall<-AIC(egam1,egam2,egam3, egam4,egam5)


eall[order(eall$AIC),]

eall['Delta AIC'] = eall['AIC']- min(eall['AIC'])
eall[order(eall$AIC),]


summary(egam4)

######################################################################

rgam1<-gam(cbind(secs_performed,secs_not) ~
             s(zt, bs="cc",
               k =10) +
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_rest,
           method = 'REML',
           family = "binomial")

rgam2<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by= sex, bs="cc",
               k =10) +
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_rest,
           method = 'REML',
           family = "binomial")

rgam3<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by=treatment, bs= "cc",
               k = 10)+
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_rest, 
           method = 'REML',
           family = "binomial")

rgam4<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by = factor(sex_treat), bs = "cc", k=10)+
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data = df_rest,
           method = 'REML',
           family = "binomial")

rgam5<-gam(cbind(secs_performed,secs_not) ~
             s(zt, by=treatment, bs= "cc", k= 10)+
             s(zt, by= sex, bs="cc", k= 10) +
             s(cage, bs = "re")+
             s(id, bs = "re"),
           data=df_rest, 
           method = 'REML',
           family = "binomial")




rall<-AIC(rgam1,rgam2,rgam3,rgam4, rgam5)

rall[order(rall$AIC),]

rall['Delta AIC'] = rall['AIC']- min(rall['AIC'])
rall[order(rall$AIC),]

summary(rgam4)


#############################################################################
#############################################################################


df_newdat_sextreat <- crossing(sex_treat = c("F_Control",
                                             "F_CORT",
                                             "M_Control",
                                             "M_CORT"),
                               zt = seq(0,24, length = 100),
                               cage = levels(factor(df_cort_long$cage)),
                               id = levels(factor(df_cort_long$id)))


egam_preds_matrix <- predict(egam4, 
                                newdata = df_newdat_sextreat,
                                se.fit = TRUE,
                                re.form = NA, exclude=c("s(cage)","s(id)"))


egam_preds_df <- bind_cols(df_newdat_sextreat,
                              egam_preds_matrix)


library(ggplot2)

egam_preds_df %>% 
  # filter(cage %in% c("b4") & id == "plusb2") %>% 
  ggplot(aes(x = zt, y = inv.logit(fit)))+
  geom_jitter(data = df_eat,
              aes(y = prop_behav),
              alpha = 0.3,
              colour = "gray40",
              width = 0.1,
              height = 0.01)+
  geom_ribbon(aes(ymin = inv.logit(fit - 1.96 * se.fit),
                  ymax = inv.logit(fit + 1.96 * se.fit)),
              alpha = 0.5,
              fill = "red" )+
  geom_line() +
  facet_grid(sex_treat ~.) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
  ggtitle("k=8")


####



df_newdat_sextreat <- crossing(sex_treat = c("F_Control",
                                             "F_CORT",
                                             "M_Control",
                                             "M_CORT"),
                               zt = seq(0,24, length = 100),
                               cage = levels(factor(df_cort_long$cage)),
                               id = levels(factor(df_cort_long$id)))


rgam_preds_matrix <- predict(rgam4, 
                             newdata = df_newdat_sextreat,
                             se.fit = TRUE,
                             re.form = NA, exclude=c("s(cage)","s(id)"))


rgam_preds_df <- bind_cols(df_newdat_sextreat,
                           rgam_preds_matrix)

rgam_preds_df %>% 
  # filter(cage %in% c("b4") & id == "plusb2") %>% 
  ggplot(aes(x = zt, y = inv.logit(fit)))+
  geom_jitter(data = df_rest,
              aes(y = prop_behav),
              alpha = 0.3,
              colour = "gray40",
              width = 0.1,
              height = 0.01)+
  geom_ribbon(aes(ymin = inv.logit(fit - 1.96 * se.fit),
                  ymax = inv.logit(fit + 1.96 * se.fit)),
              alpha = 0.5,
              fill = "red" )+
  geom_line() +
  facet_grid(sex_treat ~.) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
  ggtitle("k=8")



######################
###correct graphs####
labels <- c(F = "Female", M = "Male")
##int graph##################

egam_preds_df_plot <- egam_preds_df %>% 
  separate_wider_delim(sex_treat, "_", names = c("sex", "treatment"))

eat_plot<-egam_preds_df_plot %>% 
  ggplot(aes(x = zt, y = inv.logit(fit), color=treatment))+
  geom_jitter(data = df_eat,
              aes(y = prop_behav),
              alpha = 0.3,
              colour = "gray40",
              width = 0.1,
              height = 0.01)+
  geom_ribbon(aes(ymin = inv.logit(fit - 1.96 * se.fit),
                  ymax = inv.logit(fit + 1.96 * se.fit),
                  
                  fill = treatment ), alpha = 0.5, linetype = 0 )+
  scale_fill_manual(values = c("cornflowerblue",
                               "coral"), name="Treatment",labels = c("Control", "Pre-natal CORT"))+
  scale_color_manual(values = c("cornflowerblue",
                                "coral"), name="Treatment",labels = c("Control", "Pre-natal CORT"))+
  geom_line() +
  facet_grid(~ sex,labeller=labeller(sex = labels))+
  scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
  # scale_y_continuous("Proportion of time spent eating", breaks=c(0, 0.20, 0.40, 0.60, 0.80, 1.00), labels= c("0", "20", "40", "60", "80", "100"))+
  annotate('rect', xmin=12, xmax=24, ymin=0, ymax=1, alpha=.2, fill='black')+ 
  labs(x = "Zeitgeber time", y="Proportion of time spent eating")+
  theme_classic()


###rest


  
  
  df_newdat_sextreat <- crossing(sex_treat = c("F_Control",
                                               "F_CORT",
                                               "M_Control",
                                               "M_CORT"),
                                 zt = seq(0,24, length = 100),
                                 cage = levels(factor(df_cort_long$cage)),
                                 id = levels(factor(df_cort_long$id)))
  
  
  rgam_preds_matrix <- predict(rgam4, 
                               newdata = df_newdat_sextreat,
                               se.fit = TRUE,
                               re.form = NA, exclude=c("s(cage)","s(id)"))
  
  
  rgam_preds_df <- bind_cols(df_newdat_sextreat,
                             rgam_preds_matrix)
  
  
  library(ggplot2)
  
  rgam_preds_df %>% 
    # filter(cage %in% c("b4") & id == "plusb2") %>% 
    ggplot(aes(x = zt, y = inv.logit(fit)))+
    geom_jitter(data = df_rest,
                aes(y = prop_behav),
                alpha = 0.3,
                colour = "gray40",
                width = 0.1,
                height = 0.01)+
    geom_ribbon(aes(ymin = inv.logit(fit - 1.96 * se.fit),
                    ymax = inv.logit(fit + 1.96 * se.fit)),
                alpha = 0.5,
                fill = "red" )+
    geom_line() +
    facet_grid(sex_treat ~.) +
    scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
    ggtitle("k=8")
  
  
  
  
  ######################
  ###correct graphs####
  labels <- c(F = "Female", M = "Male")
  ##int graph##################
  
  rgam_preds_df_plot <- rgam_preds_df %>% 
    separate_wider_delim(sex_treat, "_", names = c("sex", "treatment"))
  
rest_plot<-  rgam_preds_df_plot %>% 
    ggplot(aes(x = zt, y = inv.logit(fit), color=treatment))+
    geom_jitter(data = rest_data,
                aes(y = prop_behav),
                alpha = 0.3,
                colour = "gray40",
                width = 0.1,
                height = 0.01)+
    geom_ribbon(aes(ymin = inv.logit(fit - 1.96 * se.fit),
                    ymax = inv.logit(fit + 1.96 * se.fit),
                    
                    fill = treatment ), alpha = 0.5,linetype = 0)+
    scale_fill_manual(values = c("cornflowerblue",
                                 "coral"), name="Treatment",labels = c("Control", "Pre-natal CORT"))+
    scale_color_manual(values = c("cornflowerblue",
                                  "coral"), name="Treatment",labels = c("Control", "Pre-natal CORT"))+
    geom_line() +
    facet_grid(~ sex,labeller=labeller(sex = labels))+
    scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
    annotate('rect', xmin=12, xmax=24, ymin=0, ymax=1, alpha=.2, fill='black')+ 
    labs(x = "Zeitgeber time", y="Proportion of time spent resting")+
    theme_classic()


rest_plot

eat_plot
