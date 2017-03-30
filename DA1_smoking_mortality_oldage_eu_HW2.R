# CLEAR MEMORY
rm(list=ls())

install.packages ("readr")
install.packages ("dplyr")
install.packages ("ggplot2")
install.packages ("pastecs")
install.packages ("arm")
install.packages("DataCombine")
install.packages("descr")
install.packages("mfx")
library(arm)
library(descr)
library(pastecs)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(DataCombine)
library(stargazer)
library(mfx)
library(fBasics)

# CKECK  WORKING DIRECTORY
setwd("C:/Users/Keresztesi Luca/OneDrive/CEU 1st trimester/Data Analysis 2/HW2")
getwd()

# LOAD  DATA
share_original <- read.csv("mortality_oldage_eu.csv", na.strings = ".")
share_original <- subset(share_original, age>=50 & age<=80)
share_original$deceased <- as.numeric(share_original$deceased)
share_original$age <- as.numeric(share_original$age)
share_original$female <- as.numeric(share_original$female)
share_original <- subset(share_original, sports!="." & age!="." & eduyears_mod!="." & income10g!="." & female!="." & deceased!=".")
share <- share_original[c(1, 2, 4, 16, 19, 20, 21, 27)]

# Frequency tables
freq(share$deceased)
freq(share$sports)
basicStats(share[,3])
basicStats(share[,4])
basicStats(share[,5])
basicStats(share[,6])
basicStats(share[,7])
basicStats(share[,8])

CrossTable(share$sports, share$deceased)

#Converting the sports variable into binaries
share$sports_1 <- share$sports==1
share$sports_2 <- share$sports==2
share$sports_3 <- share$sports==3
share$sports_4 <- share$sports==4
share$sports_1 <- as.numeric(share$sports_1)
share$sports_2 <- as.numeric(share$sports_2)
share$sports_3 <- as.numeric(share$sports_3)
share$sports_4 <- as.numeric(share$sports_4)
summary(share$sports_1)
summary(share$sports_2)
summary(share$sports_3)
summary(share$sports_4)
CrossTable(share$deceased, share$sports)

# LPM
lpm1 <- lm(deceased ~ sports_1+sports_2+sports_3, data=share)
coeftest(lpm1, vcov=sandwich)

stargazer(list(lpm1), digits=3, out="sports_mortality_1.html")

# scatterplot and regression line with sports only
ggplot(data = share, aes(x=sports, y=deceased)) +
  geom_point(colour="orange") +
  geom_smooth(method="lm", colour="navy") 
# only  regression line with sports
ggplot(data = share, aes(x=sports, y=deceased)) +
  geom_smooth(method="lm", colour="navy") 

# CAUSAL EFFECT
share$sports_b <- share$sports_1 + share$sports_2 + share$sports_3
basicStats(share[,9])
CrossTable(share$deceased, share$sports_b)
lpm2 <- lm(deceased ~ sports_b, data=share)
coeftest(lpm2, vcov=sandwich)

# HANDLING CONFOUNDERS IN LPM

#AGE
# Functional form for age
ggplot(data = share, aes(x=age, y=deceased)) + 
  ggtitle("Mortality Rate and Age") + xlab("Age") + ylab("Mortality rate") +
  geom_smooth(method="lm", aes(colour="Linear")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(colour="Quadratic")) +
  scale_colour_manual(name="Legend",values=c("#00BFC4","#F8766D"))

# alternative nonparametric:  fraction deceased by single years of age
share$agey <- round(share$age)
byage <- aggregate(share$deceased, list(agey=share$agey), mean)
ggplot(data = share, aes(x=age, y=deceased)) +
  ggtitle("Mortality Rate and Age") + xlab("Age") + ylab("Mortality rate") +
  geom_line(data = byage, aes(x=agey, y=x, colour="Non-parametric"), size=2) +
  geom_smooth(method="lm", aes(colour="Linear")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(colour="Quadratic")) + 
  scale_colour_manual(name="Legend",values=c("#66CC00","#00BFC4","#F8766D"))

#EDUCATION
# Functional form for education
ggplot(data = share, aes(x=eduyears_mod, y=deceased)) +
  ggtitle("Mortality Rate and Education") + xlab("Education") + ylab("Mortality rate") +
  geom_smooth(method="lm", aes(colour="Linear")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#00BFC4"))

# alternative nonparametric:  fraction deceased by single years of education
byedu <- aggregate(share$deceased, list(edy=share$eduyears_mod), mean)
ggplot(data = share, aes(x=eduyears_mod, y=deceased)) +
  ggtitle("Mortality Rate and Education") + xlab("Education") + ylab("Mortality rate") +
  geom_line(data = byedu, aes(x=edy, y=x, colour="Non-parametric"), size=2) +
  geom_smooth(method="lm", aes(colour="Linear")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#66CC00","#00BFC4"))

#INCOME
# Functional form for income group
ggplot(data = share, aes(x=income10g, y=deceased)) +
  ggtitle("Mortality Rate and Income groups") + xlab("Income groups") + ylab("Mortality rate") +
  geom_smooth(method="lm", aes(colour="Linear")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#00BFC4"))

# alternative nonparametric:  fraction deceased by 10 income groups
byinc <- aggregate(share$deceased, list(inc=share$income10g), mean)
ggplot(data = share, aes(x=income10g, y=deceased)) +
  ggtitle("Mortality Rate and Income groups") + xlab("Income groups") + ylab("Mortality rate") +
  geom_line(data = byinc, aes(x=inc, y=x, colour="Non-parametric"), size=2) +
  geom_smooth(method="lm", aes(colour="Linear")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#66CC00","#00BFC4"))

#LPM with control variables
share$age_new<-share$age-50
share$income10g_new<-share$income10g-1
share$sports_1 <- share$sports_1 + share$sports_2

lpm3 <- lm(deceased ~ sports_1 + sports_3 + female + age_new + eduyears_mod + income10g_new, data=share)
lpm4 <- lm(deceased ~ sports_b + female + age_new + eduyears_mod + income10g_new, data=share)
lpm5 <- lm(deceased ~ sports_1 + sports_3 + female + poly(age_new,2) + poly(eduyears_mod,3) + poly(income10g_new,3), data=share)
lpm6 <- lm(deceased ~ sports_b + female + poly(age_new,2) + poly(eduyears_mod,3) + poly(income10g_new,3), data=share)
coeftest(lpm3, vcov=sandwich)
coeftest(lpm4, vcov=sandwich)
coeftest(lpm5, vcov=sandwich)
coeftest(lpm6, vcov=sandwich)

stargazer(list(lpm2, lpm3, lpm4, lpm5, lpm6), digits=3, out="smoking_mortality_2.html")

reg1 <- lm(sports_1 ~ age_new, data=share)
coeftest(reg1, vcov=sandwich)
summary(reg1)

reg2 <- lm(sports_3 ~ age_new, data=share)
coeftest(reg2, vcov=sandwich)
summary(reg2)

reg3 <- lm(sports_4 ~ age_new, data=share)
coeftest(reg3, vcov=sandwich)
summary(reg3)

# LOGIT
logitcoeffs_1 <- glm(deceased ~ sports_b + female + age_new +eduyears_mod + income10g_new, data=share, family='binomial')
logitmarg_1 <- logitmfx(formula = deceased ~ sports_b + female + age_new +eduyears_mod + income10g_new, data=share, atmean=FALSE)
summary(logitcoeffs_1)
print(logitmarg_1)

logitcoeffs_2 <- glm(deceased ~ sports_1 + sports_3 + female + age_new +eduyears_mod + income10g_new, data=share, family='binomial')
logitmarg_2 <- logitmfx(formula = deceased ~ sports_1 + sports_3 + female + age_new +eduyears_mod + income10g_new, data=share, atmean=FALSE)
summary(logitcoeffs_2)
print(logitmarg_2)

stargazer(list(lpm4, logitcoeffs_1, logitmarg_1$mfxest), digits=3, out="smoking_mortality_3.html")
stargazer(list(lpm3, logitcoeffs_2, logitmarg_2$mfxest), digits=3, out="smoking_mortality_4.html")

#PREDICTIONS
share$fit_lpm4 <- fitted.values(lpm4)
share$fit_logit1 <- fitted.values(logitcoeffs_2)

qplot(share$fit_lpm4)
qplot(share$fit_logit1)

lpme<-sum((share$fit_lpm4 - share$deceased)^2)
logite<-sum((share$fit_logit1 - share$deceased)^2)


ggplot (share) +
  ggtitle("Predicted Probabilities") + xlab("Age") + ylab("Predicted mortality rate") +
  geom_line(data = byage, aes(x=agey, y=x, colour="Non-parametric"), size=2) +
  geom_smooth(data=share, method="lm", aes(x=age, y=fit_lpm4, colour="Linear LPM")) +
  geom_smooth(data=share, method="lm", formula=y~poly(x,2), aes(x=age, y=fit_logit1, colour="Logit")) +
  geom_smooth(data=share, method="lm", formula=y~poly(x,3), aes(x=age, y=deceased, colour="LPM")) +
  scale_colour_manual(name="Legend",values=c("#66CC00","#FFFF33","#F8766D","#00BFC4"))

ggplot (share) +
  ggtitle("Predicted Probabilities") + xlab("Age") + ylab("Predicted mortality rate") +
  geom_line(data = byage, aes(x=agey, y=x, colour="Non-parametric"), size=2) +
  geom_smooth(data=share, method="lm", aes(x=age, y=fit_lpm4, colour="Linear LPM")) +
  geom_smooth(data=share, method="lm", aes(x=age, y=fit_logit1, colour="Logit")) +
  geom_smooth(data=share, method="glm", formula=y~poly(x,3), aes(x=age, y=fit_lpm4, colour="LPM")) +
  scale_colour_manual(name="Legend",values=c("#66CC00","#FFFF33","#F8766D","#00BFC4"))
