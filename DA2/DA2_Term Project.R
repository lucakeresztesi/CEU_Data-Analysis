install.packages ("pastecs")
install.packages("DataCombine")
install.packages("descr")
install.packages("varhandle")
install.packages("knitr")
install.packages("gmodels")

library(knitr)
library(gmodels)
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
library(varhandle)

# CLEAR MEMORY
rm(list=ls())

# SET WORKING DIRECTORY
setwd("C:/Users/Keresztesi Luca/Box Sync/CEU 1st trimester/Data Analysis 2/Term Project")
getwd()

# LOAD  DATA
tr<- read.csv2("traffic_mortality_mod.csv", 
              header = TRUE,
              dec = ".",
              strip.white = TRUE,
              na.strings = "..",
              stringsAsFactors = FALSE)

# CLEAN AND CREATE VARIABLES
tr <- subset(tr, tr$traf !="NA" & tr$gdppc !="NA")
tr$gdppc <- tr$gdppc/1000
tr$lngdppc <- log(tr$gdppc)
tr$gdppc <- NULL

tr$africa <- tr$continent == "Africa"
tr$europe <- tr$continent == "Europe"
tr$northam <- tr$continent == "North-America"
tr$southam <- tr$continent == "South-America"
tr$oceania <- tr$continent == "Oceania"
tr$asia <- tr$continent == "Asia"

# DESCRIPTIVE STATS
basicStats(tr[,2])
qplot(tr$traf, geom="histogram", binwidth=5)
basicStats(tr[,3])
qplot(tr$urbgr, geom="histogram", binwidth=1)
basicStats(tr[,4])
qplot(tr$lifeexp, geom="histogram", binwidth=5)
basicStats(tr[,6])
qplot(tr$lngdppc, geom="histogram", binwidth=1)

# LOWESS NONPARAMETRIC REGRESSION
ggplot(data = tr, aes(x=urbgr, y=traf)) +
  ggtitle("Traffic mortality on Urban population growth") + xlab("Urban population growth") + ylab("Traffic mortality") +
  geom_point(size=1.5, aes(colour=factor(continent)))+
  geom_smooth(method="loess", colour="darkgreen")+
  scale_colour_manual(name="Legend",values=c("darkgreen","indian red","lightcyan 4","darkslategray 2","gold 1","lightpink 1"))

ggplot(data = tr, aes(x=urbgr, y=lngdppc)) +
  ggtitle("ln GDP per capita on Urban population growth") + xlab("Urban population growth") + ylab("ln GDP per capita") +
  geom_point(size=1.5, aes(colour=factor(continent)))+
  geom_smooth(method="loess", colour="deepskyblue4")+
  scale_colour_manual(name="Legend",values=c("darkgreen","indian red","lightcyan 4","darkslategray 2","gold 1","lightpink 1"))

# EXPLANATORY VARIABLES
# URBAN GROWTH AND TRAFFIC MORTALITY
reg_u <- lm(traf ~ urbgr, data=tr)
summary(reg_u, vcov=sandwich)

ggplot(data = tr, aes(x=urbgr, y=traf)) +
  ggtitle("Traffic mortality on Urban population growth") + xlab("Urban population growth") + ylab("Traffic mortality") +
  geom_point(size=1.5, colour="indian red")+
  geom_smooth(method="loess", aes(x=urbgr, y=traf, colour="Non-parametric")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(x=urbgr, y=traf, colour="Quadratic")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(x=urbgr, y=traf, colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("darkgreen", "darkslategray 2","gold 1"))

reg_u_2 <- lm(traf ~ poly(urbgr,2), data=tr)
summary(reg_u_2, vcov=sandwich)
reg_u_3 <- lm(traf ~ poly(urbgr,3), data=tr)
summary(reg_u_3, vcov=sandwich)
AIC(reg_u, reg_u_2, reg_u_3)

# LIFE EXPECTANCY AND TRAFFIC MORTALITY
reg_l <- lm(traf ~ lifeexp, data=tr)
summary(reg_l, vcov=sandwich)

ggplot(data = tr, aes(x=lifeexp, y=traf)) +
  ggtitle("Traffic mortality on Life expectancy") + xlab("Life expectancy") + ylab("Traffic mortality") +
  geom_point(size=1.5, colour="indian red")+
  geom_smooth(method="loess", aes(x=lifeexp, y=traf, colour="Non-parametric")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(x=lifeexp, y=traf, colour="Quadratic")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(x=lifeexp, y=traf, colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("darkgreen", "darkslategray 2","gold 1"))

reg_l_2 <- lm(traf ~ poly(lifeexp,2), data=tr)
summary(reg_l_2, vcov=sandwich)
reg_l_3 <- lm(traf ~ poly(lifeexp,3), data=tr)
summary(reg_l_3, vcov=sandwich)
AIC(reg_l, reg_l_2, reg_l_3)

# LN GDPPC AND TRAFFIC MORTALITY
reg_g <- lm(traf ~ lngdppc, data=tr)
summary(reg_g, vcov=sandwich)

ggplot(data = tr, aes(x=lngdppc, y=traf)) +
  ggtitle("Traffic mortality on Log GDP per capita") + xlab("Log GDP per capita") + ylab("Traffic mortality") +
  geom_point(size=1.5, colour="indian red")+
  geom_smooth(method="loess", aes(x=lngdppc, y=traf, colour="Non-parametric")) +
  geom_smooth(method="lm", formula=y~poly(x,2), aes(x=lngdppc, y=traf, colour="Quadratic")) +
  geom_smooth(method="lm", formula=y~poly(x,3), aes(x=lngdppc, y=traf, colour="Cubic")) +
  scale_colour_manual(name="Legend",values=c("darkgreen", "darkslategray 2","gold 1"))

reg_g_2 <- lm(traf ~ poly(lngdppc,2), data=tr)
summary(reg_g_2, vcov=sandwich)
reg_g_3 <- lm(traf ~ poly(lngdppc,3), data=tr)
summary(reg_g_3, vcov=sandwich)
AIC(reg_g, reg_g_2, reg_g_3)

# CONTINENT AND TRAFFIC MORTALITY
traf_af <- subset(tr, tr$continent == "Africa")
quantile(traf_af$tr, c(.25, .50,  .75, .95))
traf_eu <- subset(tr, tr$continent == "Europe")
quantile(traf_eu$tr, c(.25, .50,  .75, .95))
traf_na <- subset(tr, tr$continent == "North-America")
quantile(traf_na$tr, c(.25, .50,  .75, .95))
traf_sa <- subset(tr, tr$continent == "South-America")
quantile(traf_sa$tr, c(.25, .50,  .75, .95))
traf_oc <- subset(tr, tr$continent == "Oceania")
quantile(traf_oc$tr, c(.25, .50,  .75, .95))
traf_as <- subset(tr, tr$continent == "Asia")
quantile(traf_as$tr, c(.25, .50,  .75, .95))

reg_af <- lm( traf ~ africa, data=tr)
summary(reg_af, vcov=sandwich)
reg_eu <- lm(traf ~ europe, data=tr)
summary(reg_eu, vcov=sandwich)
reg_na <- lm(traf ~ northam, data=tr)
summary(reg_na, vcov=sandwich)
reg_sa <- lm(traf ~ southam, data=tr)
summary(reg_sa, vcov=sandwich)
reg_oc <- lm(traf ~ oceania, data=tr)
summary(reg_oc, vcov=sandwich)
reg_as <- lm(traf ~ asia, data=tr)
summary(reg_as, vcov=sandwich)

# LINEAR REG OF LNGDPPC ON URBAN GROWTH AND CONTINENTS
reg1_urb <- lm(lngdppc ~ urbgr, data=tr)
summary(reg1_urb, vcov=sandwich)
reg1_lif <- lm(lngdppc ~ lifeexp, data=tr)
summary(reg1_lif, vcov=sandwich)
reg1_af <- lm(lngdppc ~ africa, data=tr)
summary(reg1_af, vcov=sandwich)
reg1_eu <- lm(lngdppc ~ europe, data=tr)
summary(reg1_eu, vcov=sandwich)
reg1_na <- lm(lngdppc ~ northam, data=tr)
summary(reg1_na, vcov=sandwich)
reg1_sa <- lm(lngdppc ~ southam, data=tr)
summary(reg1_sa, vcov=sandwich)
reg1_oc <- lm(lngdppc ~ oceania, data=tr)
summary(reg1_oc, vcov=sandwich)
reg1_as <- lm(lngdppc ~ asia, data=tr)
summary(reg1_as, vcov=sandwich)

stargazer(list(reg1_urb, reg1_lif, reg1_af, reg1_eu, reg1_na, reg1_sa, reg1_oc, reg1_as), digits=2, out="traffic_mortality_1.html")

# RESIDUALS
reg_g_predict <- predict(reg_g)
tr$e_lngdppc <- resid(reg_g)
basicStats(tr[,13])
qplot(tr$e_lngdppc, geom="histogram", binwidth=3)
# countries with most negative residuals
e_neg_lngdppc <- subset(tr, e_lngdppc<(-8.0))
# countries with most positive residuals
e_pos_lngdppc <- subset(tr, e_lngdppc>(10.0))

# LINEAR REG OF OF TRAFFIC MORTALITY ON LNGDPPC
reg1 <- lm(traf ~ lngdppc, data=tr)
summary(reg1, vcov=sandwich)

reg2 <- lm(traf ~ lngdppc + urbgr, data=tr)
summary(reg2, vcov=sandwich)

reg3 <- lm(traf ~ lngdppc + urbgr + asia + europe + northam + oceania + southam, data=tr)
summary(reg3, vcov=sandwich)

reg4_1 <- lm(traf ~ lngdppc + urbgr + asia + europe + northam + oceania + southam + europe*urbgr, data=tr)
summary(reg4_1, vcov=sandwich)

reg4_2 <- lm(traf ~ lngdppc + urbgr + asia + europe + northam + oceania + southam + asia*urbgr, data=tr)
summary(reg4_2, vcov=sandwich)

reg5 <- lm(traf ~ lngdppc + urbgr + asia + europe + northam + oceania + southam + urbgr*lngdppc, data=tr)
summary(reg5, vcov=sandwich)

stargazer(list(reg1, reg2, reg3, reg4_1, reg4_2, reg5), digits=2, out="traffic_mortality_2.html")
