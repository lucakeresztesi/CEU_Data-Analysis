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
setwd("C:/Users/Keresztesi Luca/OneDrive/CEU 1st trimester/Data Analysis 2/HW1")
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
cps <- cps_original[c(1, 4, 8, 13, 18, 20, 21, 22, 23, 24)]

# DISTRIBUTION OF EARNINGS & HOURS
basicStats(cps[,5])
qplot(cps$earnwke, geom="histogram", binwidth=100)
basicStats(cps[,7])
qplot(cps$hours, geom="histogram",binwidth=10)
basicStats(cps[,9])
qplot(cps$earnhr, geom="histogram", binwidth=10)
basicStats(cps[,10])
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
reg2 <- lm(lnw ~ female + hours, data=cps)
reg3 <- lm(lnw ~ female + hours + edy , data=cps)
reg4 <- lm(lnw ~ female + hours + edy + age, data=cps)
reg5 <- lm(lnw ~ female + poly(hours,3) + edy + age, data=cps)
reg6 <- lm(lnw ~ female + poly(hours,3) + edy + age + I(pmax(age-35,0)), data=cps)

summary(reg1, vcov=sandwich)
summary(reg2, vcov=sandwich)
summary(reg3, vcov=sandwich)
summary(reg4, vcov=sandwich)
summary(reg5, vcov=sandwich)

stargazer(list(reg1, reg2, reg3, reg4, reg5, reg6), digits=2, out="gender_earnings_1.html")

# REGRESSIONS WITH INTERACTIONS, EDUCATION AND AGE
cps$edy_female=cps$edy*cps$female
cps$age_female=cps$age*cps$female
cps$agesq=cps$age*cps$age
cps$agesp_female=cps$agesq*cps$female

reg1 <- lm(lnw ~ female + poly(hours,3) + edy + age, data=cps)
reg1 <- lm(lnw ~ female + poly(hours,3) + edy + poly(age,2), data=cps)
reg3 <- lm(lnw ~ female + poly(hours,3) + edy + age + edy_female + age_female, data=cps)
reg4 <- lm(lnw ~ female + poly(hours,3) + edy + poly(age,2) + edy_female + poly(age_female,2), data=cps)
summary(reg1, vcov=sandwich)
summary(reg2, vcov=sandwich)
summary(reg3, vcov=sandwich)
summary(reg4, vcov=sandwich)

stargazer(list(reg1,reg3), digits=2, out="gender_earnings_2.html")

cps$predlnw <- predict(reg3)

cps_f40ed5.5 <- subset(cps, female==1 & hours==40 & edy==5.5)
cps_m40ed5.5 <- subset(cps, female==0 & hours==40 & edy==5.5)
cps_f40ed12 <- subset(cps, female==1 & hours==40 & edy==12)
cps_m40ed12 <- subset(cps, female==0 & hours==40 & edy==12)
cps_f40ed14 <- subset(cps, female==1 & hours==40 & edy==14)
cps_m40ed14 <- subset(cps, female==0 & hours==40 & edy==14)
cps_f40ed16 <- subset(cps, female==1 & hours==40 & edy==16)
cps_m40ed16 <- subset(cps, female==0 & hours==40 & edy==16)
cps_f40ed18 <- subset(cps, female==1 & hours==40 & edy==18)
cps_m40ed18 <- subset(cps, female==0 & hours==40 & edy==18)

freq(cps$edy)

ggplot(data = cps, aes(x=age, y=predlnw)) + xlab("Age")+ylab("ln Earning Per Hour") + 
  ggtitle("Years spent in education: 5.5 and 12") +
  geom_line(data=cps_f40ed5.5, colour="#F8766D", size=3 ) +
  geom_line(data=cps_m40ed5.5, colour="#00BFC4", size=3) +
  geom_line(data=cps_f40ed12, colour="#F8766D", size=1 ) +
  geom_line(data=cps_m40ed12, colour="#00BFC4", size=1) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#00BFC4"))

ggplot(data = cps, aes(x=age, y=predlnw)) + xlab("Age")+ylab("ln Earning Per Hour") + 
  ggtitle("Years spent in education: 14 and 18") +
  geom_line(data=cps_f40ed14, colour="#F8766D", size=3 ) +
  geom_line(data=cps_m40ed14, colour="#00BFC4", size=3) +
  geom_line(data=cps_f40ed18, colour="#F8766D", size=1 ) +
  geom_line(data=cps_m40ed18, colour="#00BFC4", size=1) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#00BFC4"))

# POTENTIAL EXPERIENCE
cps$potexp <- cps$age - 6 - cps$edy
summary(cps$potexp)
cps$potexp <- cps$potexp*(cps$potexp>=0)
summary(cps$potexp)

ggplot(data = cps, aes(x=potexp, y=lnw)) +
  geom_smooth(method="loess", colour="black") +
  geom_smooth(method="lm", formula=y~poly(x,2), colour="navy") 

reg_potexp <- lm(potexp ~ female, data=cps)
summary(reg_potexp, vcov=sandwich)

# REGRESSIONS CONDITIONAL ON HOURS, EDUCATION AND POTENTIAL EXPERIENCE
reg1 <- lm(lnw ~ female, data=cps)
reg2 <- lm(lnw ~ female + hours, data=cps)
reg3 <- lm(lnw ~ female + hours + edy , data=cps)
reg4 <- lm(lnw ~ female + hours + edy + poly(potexp,2), data=cps)
reg5 <- lm(lnw ~ female + poly(hours,3) + edy + poly(potexp,2), data=cps)

summary(reg1, vcov=sandwich)
summary(reg2, vcov=sandwich)
summary(reg3, vcov=sandwich)
summary(reg4, vcov=sandwich)
summary(reg5, vcov=sandwich)

stargazer(list(reg1, reg2, reg3, reg4, reg5), digits=2, out="gender_earnings_3.html")

# REGRESSIONS WITH INTERACTIONS, EDUCATION AND POTENTIAL EXPERIENCE
cps$edy_female=cps$edy*cps$female
cps$potexp_female=cps$potexp*cps$female
cps$potexpsq=cps$potexp*cps$potexp
cps$potexpsq_female=cps$potexpsq*cps$female

reg1 <- lm(lnw ~ female + poly(hours,3) + edy + poly(potexp,2), data=cps)
reg2 <- lm(lnw ~ female + poly(hours,3) + edy + potexp + edy_female + potexp_female, data=cps)
reg3 <- lm(lnw ~ female + poly(hours,3) + edy + poly(potexp,2) + edy_female + poly(potexp_female,2), data=cps)
summary(reg1, vcov=sandwich)
summary(reg2, vcov=sandwich)
summary(reg3, vcov=sandwich)

stargazer(list(reg1, reg2, reg3), digits=2, out="gender_earnings_4.html")

cps$predlnw <- predict(reg3)

cps_f40ed5.5 <- subset(cps, female==1 & hours==40 & edy==5.5)
cps_m40ed5.5 <- subset(cps, female==0 & hours==40 & edy==5.5)
cps_f40ed12 <- subset(cps, female==1 & hours==40 & edy==12)
cps_m40ed12 <- subset(cps, female==0 & hours==40 & edy==12)
cps_f40ed14 <- subset(cps, female==1 & hours==40 & edy==14)
cps_m40ed14 <- subset(cps, female==0 & hours==40 & edy==14)
cps_f40ed16 <- subset(cps, female==1 & hours==40 & edy==16)
cps_m40ed16 <- subset(cps, female==0 & hours==40 & edy==16)
cps_f40ed18 <- subset(cps, female==1 & hours==40 & edy==18)
cps_m40ed18 <- subset(cps, female==0 & hours==40 & edy==18)

ggplot(data = cps, aes(x=potexp, y=predlnw)) + xlab("Potential Experience")+ylab("ln Earning Per Hour") + 
  ggtitle("Years spent in education: 5.5 and 12") +
  geom_smooth(data=cps_f40ed5.5, method="lm", formula=y~poly(x,2), colour="#F8766D", size=3) +
  geom_smooth(data=cps_m40ed5.5, method="lm", formula=y~poly(x,2), colour="#00BFC4", size=3) +
  geom_smooth(data=cps_f40ed12, method="lm", formula=y~poly(x,2), colour="#F8766D", size=1) +
  geom_smooth(data=cps_m40ed12, method="lm", formula=y~poly(x,2), colour="#00BFC4", size=1) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#00BFC4"))

ggplot(data = cps, aes(x=potexp, y=predlnw)) + xlab("Potential Experience")+ylab("ln Earning Per Hour") + 
ggtitle("Years spent in education: 14 and 18") +
  geom_smooth(data=cps_f40ed14, method="lm", formula=y~poly(x,2), colour="#F8766D", size=3) +
  geom_smooth(data=cps_m40ed14, method="lm", formula=y~poly(x,2), colour="#00BFC4", size=3) +
  geom_smooth(data=cps_f40ed18, method="lm", formula=y~poly(x,2), colour="#F8766D", size=1) +
  geom_smooth(data=cps_m40ed18, method="lm", formula=y~poly(x,2), colour="#00BFC4", size=1) +
  scale_colour_manual(name="Legend",values=c("#F8766D","#00BFC4"))

# employees that reported to work more than 10 hours and less than 70 

cps6<-subset(cps,(hours>10) & (hours<70))
cps6$edy_female=cps6$edy*cps6$female
cps6$age_female=cps6$age*cps6$female

reg1 <- lm(lnw ~ female + poly(hours,3) + edy + age, data=cps)
reg2 <- lm(lnw ~ female + poly(hours,3) + edy + age, data=cps6)
reg3 <- lm(lnw ~ female + poly(hours,3) + edy + age + I(pmax(age-35,0)), data=cps)
reg4 <- lm(lnw ~ female + poly(hours,3) + edy + age + I(pmax(age-35,0)), data=cps6)
reg5 <- lm(lnw ~ female + poly(hours,3) + edy + age + edy_female + age_female, data=cps)
reg6 <- lm(lnw ~ female + poly(hours,3) + edy + age + edy_female + age_female, data=cps6)

summary(reg1, vcov=sandwich)
summary(reg2, vcov=sandwich)
summary(reg3, vcov=sandwich)
summary(reg4, vcov=sandwich)
summary(reg5, vcov=sandwich)
summary(reg6, vcov=sandwich)

stargazer(list(reg1, reg2, reg3, reg4, reg5, reg6), digits=2, out="gender_earnings_5.html")

cps6$predlnw <- predict(reg3)


