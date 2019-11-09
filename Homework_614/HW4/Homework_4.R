#windows
#due Nov.28
setwd('D:\\Mathemagic\\Fall2019\\CS614_Interactive_Data_Analysis\\Classwork\\Classwork_7_lecture_9')
rm(list=ls())

library('DAAG')
require(ggplot2)
library('lme4')

summary(ant111b)

df1=ant111b

#_____________________PROBLEM_1________________________
# On Slide 13 of the Lecture 9 slides, we performed a random intercept model on the ant111b data,
# with the only fixed effect being the intercept, i.e., average. Repeat this analysis eight times, where
# within each reproduction, you eliminate one of the sites. For each of these runs, calculate the variance
# partitioning coefficient, i.e. ????????????????Can you explain the results that you observe? The figure on Slide
# 6 may help your interpretation. What do you suppose would happen if instead of removing one site
# at random, you simply removed one plot at random from each of the 8 sites?
#(EACH TIME WE RUN WITH 7)
summary(df1)
summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'DBAN'),]))
vpc1=1.6437/(1.6437+0.8049)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'LFAN'),]))
vpc2=1.6709/(1.6709+0.7415)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'NSAN'),]))
vpc3=1.3647/(1.3647+0.7619)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'ORAN'),]))
vpc4=1.2086/(1.2806+0.7985)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'OVAN'),]))
vpc5=1.6653/(1.6653+0.6384)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'TEAN'),]))
vpc6=1.5720/(1.5720+0.7988)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'WEAN'),]))
vpc7=1.5814/(1.5814+0.7479)

summary(lmer(harvwt ~ 1+(1|site),data=df1[which(df1$site != 'WLAN'),]))
vpc8=1.5428/(1.5428+0.7744)

vpc=c(vpc1,vpc2,vpc3,vpc4,vpc5,vpc6,vpc7,vpc8)
vpc

# _____________________PROBLEM_2________________________
# The data set Guns from the MEMSS package reports the number of rounds fired per minute, by each
# of nine teams of gunners, each tested twice, once each with two different methods. Of the nine teams,
# three were comprised of men with slight builds, three with average builds, and three with heavy
# builds. Does the number of rounds fired significantly vary based on build or firing method? What
# proportion of the total variance is due to between-team versus within-team considerations, i.e. what
# is the variance portioning coefficient?
#install.packages('MEMSS')
library(MEMSS)
summary(Gun)
df2=Gun
summary(df2)
summary(aov(rounds~Error(Physique),data=df2))
summary(aov(rounds~Error(Method),data=df2))
summary(aov(rounds~Physique,data=df2)) #not sig
summary(aov(rounds~Method,data=df2))  #sig

summary(aov(rounds~Error(Team),data=df2))
summary(lmer(rounds ~ 1+(1|Team),data=df2))
sigW=4.662
sigB=0
vpc=sigB/(sigB+sigW)   #none of the variation is due to differences between team
# _____________________PROBLEM_3________________________
# The data set MathAchieve describes math achievement scores from various schools. The binary
# variables Minority and Sex and the continuous variable SES are obviously fixed-effects variables.
# However, the School variable can be chosen as either fixed or random effect. Discuss how different
# purposes for the study might result in you treating School as either a fixed or random effect. Carry
# out the random effect analysis and detail the results, in terms of the significance of the fixed effects
# and the contributions of variances. Which model better fits the data, a random-intercept or random
# slope model?
df3=MathAchieve
summary(df3)
m1=lmer(MathAch ~ Sex+Minority+SES+(1|School),data=df3)
summary(lm(MathAch ~ Sex+Minority+SES+School,data=df3))
m2=lmer(MathAch ~ Sex+Minority+SES+(MEANSES|School),data=df3)
anova(m1,m2)
#_____________________PROBLEM_4________________________
# Repeat the multiple baseline model that was outlined on Slide 49, but allow for change in slope as a
# result of the intervention in addition to change level. Do random slopes make sense? Summarize your
# results.
load('pfadata.RData')
df4=pfa_all
#random intercept, fixed slope (time is not a preditor variable)
m3=lmer(meancounts ~ IVflag+(1|HomeID),data=df4,REML=F)
summary(m3)
ranef(m3)

#random intercept+slope, fixed slope (time is not a preditor variable)
m4=lmer(meancounts ~ IVflag+(IVflag|HomeID),data=df4,REML=F)
summary(m4)
ranef(m4)
anova(m3,m4)
