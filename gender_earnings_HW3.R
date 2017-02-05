install.packages ("pastecs")
install.packages("DataCombine")
install.packages("descr")

library(pastecs)
library(DataCombine)
library(descr)
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(segmented)
library(splines)
library(stargazer)
library(fBasics)

# CLEAR MEMORY
rm(list=ls())

# SET WORKING DIRECTORY
setwd("C:/Users/Keresztesi Luca/OneDrive/CEU 1st trimester/Data Analysis 2/HW3")
getwd()

# LOAD  DATA
cps_original <- read.csv("cps_morg2014.csv")

# SELECT OBSERVATIONS
cps_original <- subset(cps_original, cps_original$stfips == "CA" & cps_original$uhourse > 0 & cps_original$earnwke > 0 & cps_original$age <= 64 & cps_original$lfsr94 == "Employed-At Work")

# CREATE VARIABLES
cps_original$hours <- cps_original$uhourse
cps_original$female <- cps_original$sex==2
cps_original$earnhr <- cps_original$earnwke/cps_original$hours
cps_original$lnw <- log(cps_original$earnhr)

# CREATE SUBSET OF NEEDED VARIABLES
cps <- cps_original[c(1, 4, 7, 8, 10, 12, 13, 18, 20, 21, 22, 23, 24)]

# DISTRIBUTION OF EARNINGS & HOURS
basicStats(cps[,8])
qplot(cps$earnwke, geom="histogram", binwidth=100)
basicStats(cps[,10])
qplot(cps$hours, geom="histogram",binwidth=10)
basicStats(cps[,12])
qplot(cps$earnhr, geom="histogram", binwidth=10)
basicStats(cps[,13])
qplot(cps$lnw, geom="histogram", binwidth=0.4)

# EARNINGS PER HOUR AND HOURS
#ggplot(data = cps, aes(x=hours, y=lnw)) +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", formula=y~poly(x,3), colour="navy") 
  
# earnings per hour, hours worked, education and age, regressed on gender
# HOURS AND GENDER
reg_ho <- lm(hours ~ female, data=cps)
summary(reg_ho, vcov=sandwich)
hours_fem <- subset(cps, cps$female == 1)
freq(ordered(hours_fem$hours))
hours_mal <- subset(cps, cps$female == 0)
freq(ordered(hours_mal$hours))

ggplot(data = cps, aes(x=hours, y=lnw)) +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="navy")

# EDUCATION AND GENDER
cps$edy <- cps$ihigrdc

reg_ed <- lm(edy ~ female, data=cps)
summary(reg_ed, vcov=sandwich)
edu_fem <- subset(cps, cps$female == 1)
quantile(edu_fem$edy, c(.25, .50,  .75, .95))
edu_mal <- subset(cps, cps$female == 0)
quantile(edu_mal$edy, c(.25, .50,  .75, .95))

qplot(cps$edy, geom="histogram", binwidth=1)
freq(ordered(cps$edy))
ggplot(data = cps, aes(x=edy, y=lnw)) +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="navy") 

# AGE AND GENDER
reg_age <- lm(age ~ female, data=cps)
summary(reg_age, vcov=sandwich)
age_fem <- subset(cps, cps$female == 1)
quantile(age_fem$age, c(.25, .50,  .75, .95))
age_mal <- subset(cps, cps$female == 0)
quantile(age_mal$age, c(.25, .50,  .75, .95))

ggplot(data = cps, aes(x=age, y=lnw)) +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", colour="navy") 

# HOURLYS EARNINGS AND GENDER
reg_w <- lm(earnhr ~ female, data=cps)
summary(reg_w, vcov=sandwich)
w_fem <- subset(cps, cps$female == 1)
quantile(w_fem$earnhr, c(.25, .50,  .75, .95))
w_mal <- subset(cps, cps$female == 0)
quantile(w_mal$earnhr, c(.25, .50,  .75, .95))

#LINEAR REG OF LOG HOURLY WAGES ON GENDER
reg1 <- lm(lnw ~ female, data=cps)
summary(reg1, vcov=sandwich)
stargazer(list(reg1), digits=2, out="gender_earnings_1.html")

#LINEAR REG OF LOG HOURLY WAGES ON GENDER WITH CONTROL VARIABLES
reg2 <- lm(lnw ~ female + hours + edy + age, data=cps)
summary(reg2, vcov=sandwich)
stargazer(list(reg1, reg2), digits=2, out="gender_earnings_2.html")

#FINDING THE FUNCTIONAL FORMS
# alternative non-parametric: aggregate by hours
byhours <- aggregate(cps$lnw, list(h=cps$hours), mean)

ggplot(data = cps, aes(x=hours, y=lnw)) +
  ggtitle("Log hourly wages on Hours") + xlab("Hours worked") + ylab("Log Hourly wages") +
  geom_point(size=1.5,aes(colour=factor(female)))+
  geom_line(aes(x=h, y=x, colour="Alternative Non-parametric"), data=byhours, size=1) +
  geom_smooth(method="loess", aes(x=hours, y=lnw, colour="Non-parametric")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(x=hours, y=lnw, colour="Quadratic")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(x=hours, y=lnw, colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("darkgreen","indian red","lightcyan 4","darkslategray 2","gold 1","lightpink 1"))

# alternative non-parametric: aggregate by education
byedy <- aggregate(cps$lnw, list(edy=cps$edy), mean)

ggplot(data = cps, aes(x=edy, y=lnw)) +
  ggtitle("Log hourly wages on Education") + xlab("Years of education") + ylab("Log Hourly wages") +
  geom_point(size=1.5,aes(colour=factor(female)))+
  geom_line(aes(x=edy, y=x, colour="Alternative Non-parametric"), data=byedy, size=1) +
  geom_smooth(method="loess", aes(x=edy, y=lnw, colour="Non-parametric")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(x=edy, y=lnw, colour="Quadratic")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(x=edy, y=lnw, colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("darkgreen","indian red","lightcyan 4","darkslategray 2","gold 1","lightpink 1"))

# alternative non-parametric: aggregate by age
byage <- aggregate(cps$lnw, list(age=cps$age), mean)

ggplot(data = cps, aes(x=age, y=lnw)) +
  ggtitle("Log hourly wages on Age") + xlab("Age") + ylab("Log Hourly wages") +
  geom_point(size=1.5,aes(colour=factor(female)))+
  geom_line(aes(x=age, y=x, colour="Alternative Non-parametric"), data=byage, size=1) +
  geom_smooth(method="loess", aes(x=age, y=lnw, colour="Non-parametric")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(x=age, y=lnw, colour="Quadratic")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(x=age, y=lnw, colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("darkgreen","indian red","lightcyan 4","darkslategray 2","gold 1","lightpink 1"))

#finding the model with the best fit
reg3_222 <- lm(lnw ~ female + poly(hours,2) + poly(edy,2) + poly(age,2), data=cps)
reg3_322 <- lm(lnw ~ female + poly(hours,3) + poly(edy,2) + poly(age,2), data=cps)
reg3_232 <- lm(lnw ~ female + poly(hours,2) + poly(edy,3) + poly(age,2), data=cps)
reg3_223 <- lm(lnw ~ female + poly(hours,2) + poly(edy,2) + poly(age,3), data=cps)
reg3_332 <- lm(lnw ~ female + poly(hours,3) + poly(edy,3) + poly(age,2), data=cps)
reg3_323 <- lm(lnw ~ female + poly(hours,3) + poly(edy,2) + poly(age,3), data=cps)
reg3_233 <- lm(lnw ~ female + poly(hours,3) + poly(edy,2) + poly(age,3), data=cps)
reg3_333 <- lm(lnw ~ female + poly(hours,3) + poly(edy,3) + poly(age,3), data=cps)

AIC(reg2, reg3_222, reg3_322, reg3_232, reg3_223, reg3_332, reg3_323, reg3_233, reg3_333)

# REGRESSIONS CONDITIONAL ON HOURS, EDUCATION AND AGE IN CUBIC FORM
reg3 <- lm(lnw ~ female + poly(hours,3) + poly(edy,3) + poly(age,3), data=cps)
summary(reg3, vcov=sandwich)
stargazer(list(reg1, reg2, reg3), digits=2, out="gender_earnings_3.html")

#REGRESSION CONDITIONAL ON HOURS, EDUCATION AND AGE WITH INTERACTIONS
cps$edy_c<-as.factor(cps$edy)
reg4 <- lm(lnw ~ female + hours + edy_c + age + edy_c*hours + edy_c*age, data=cps)
summary(reg4, vcov=sandwich)

install.packages("knitr")
install.packages("gmodels")
library(knitr)
library(gmodels)

kable(ci(reg4))
stargazer(list(reg1, reg2, reg3, reg4), digits=2, out="gender_earnings_4.html")
AIC(reg1, reg2, reg3, reg4)
