rm(list = ls())
library(WDI)
library(data.table)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(plm)
library(stargazer)
library(fBasics)
library(pander)
library(knitr)
library(dplyr)
library(sandwich) 
library(lmtest)

setwd("C:/Users/Keresztesi Luca/Box Sync/CEU 2nd trimester/Data Analysis 3/Term Project")

# DOWNLOADING DATA THROUGH World Bank API-------------------------------------------------------

# SEARCHING FOR DATA: Wage and salaried workers, female (% of female employed)
wag_inds <- WDIsearch('wage')
wagCode <- wag_inds[match
                    ("Wage and salaried workers, female (% of females employed)", 
                      wag_inds[,2],1)]
# DATA DOWNLOAD: Wage and salaried workers, female (% of females employed)
dat_wag = WDI(
  indicator = wagCode, 
  start = 1995, end = 2014)
# FILTERING OUT REGIONS
dt_wag <- data.table(dat_wag)
exclusionList <- dt_wag[,.(itemCnt = .N),by = .(code = dt_wag$iso2c)][1:47, 1]
wagData <- subset(dt_wag, !(dt_wag$iso2c %in%  exclusionList$code))

# SEARCHING FOR DATA: Maternal mortality ratio (per 100,000 live births)
mat_inds <- WDIsearch('maternal')
matCode <- mat_inds[match
                    ("Maternal mortality ratio (modeled estimate, per 100,000 live births)", 
                      mat_inds[,2],1)]
# DATA DOWNLOAD: Maternal mortality ratio (per 100,000 live births)
dat_mat = WDI(
  indicator = matCode, 
  start = 1995, end = 2014)
# FILTERING OUT REGIONS
dt_mat <- data.table(dat_mat)
exclusionList <- dt_mat[,.(itemCnt = .N),by = .(code = dt_mat$iso2c)][1:47, 1]
matData <- subset(dt_mat, !(dt_mat$iso2c %in%  exclusionList$code))

# SEARCHING FOR DATA: Health expenditure, total (% of GDP)
health_inds <- WDIsearch('health')
healthCode <- health_inds[match
                    ("Health expenditure, total (% of GDP)", 
                      health_inds[,2],1)]
# DATA DOWNLOAD: Health expenditure, total (% of GDP)
dat_health = WDI(
  indicator = healthCode, 
  start = 1995, end = 2014)
# FILTERING OUT REGIONS
dt_health <- data.table(dat_health)
exclusionList <- dt_health[,.(itemCnt = .N),by = .(code = dt_health$iso2c)][1:47, 1]
healthData <- subset(dt_health, !(dt_health$iso2c %in%  exclusionList$code))

# SEARCHING FOR DATA: GDP per capita 
gdp_inds <- WDIsearch('gdp')
grep("2005", gdp_inds, value = TRUE )
gdpppCode <- gdp_inds[match("GDP per capita, PPP (constant 2005 international $)",gdp_inds[,2]),1]
# DATA DOWNLOAD: GDP per capita
dat = WDI(
  indicator=gdpppCode, 
  start = 1990, end = 2014)
# FILTERING OUT REGIONS
dt <- data.table(dat)
exclusionList <-dt[,.(itemCnt=.N),by=.(code = dt$iso2c)][1:47,1]
gdpData <- subset(dt, !(dt$iso2c %in%  exclusionList$code))


# SEARCHING FOR DATA: Population, total
pop_inds <- WDIsearch('population')
popCode <- pop_inds[match
                    ("Population, total", 
                      pop_inds[,2],1)]
# DATA DOWNLOAD: Population
dat_pop = WDI(
  indicator = popCode, 
  start = 1990, end = 2014)
# FILTERING OUT REGIONS
dt_pop <- data.table(dat_pop)
exclusionList <- dt_pop[,.(itemCnt = .N),by = .(code = dt_pop$iso2c)][1:47, 1]
popData <- subset(dt_pop, !(dt_pop$iso2c %in%  exclusionList$code))

# MERGING DATA INTO PANEL
panelData_1 <- merge(wagData, gdpData,
                     by.x = c("iso2c", "year","country"), 
                     by.y = c("iso2c", "year", "country") ) 
panelData_2 <- merge(healthData, matData,
                     by.x = c("iso2c", "year","country"), 
                     by.y = c("iso2c", "year", "country") ) 
panelData_3 <- merge(popData, panelData_1,  
                     by.x = c("iso2c", "year","country"), 
                     by.y = c("iso2c", "year", "country") )
# panelData_4 <- merge(highData, mmortData,  
#                      by.x = c("iso2c", "year","country"), 
#                      by.y = c("iso2c", "year", "country") )
# panelData_5 <- merge(panelData_1, panelData_2,  
#                      by.x = c("iso2c", "year","country"), 
#                      by.y = c("iso2c", "year", "country") )
# panelData_6 <- merge(panelData_3, panelData_4,
#                      by.x = c("iso2c", "year","country"), 
#                      by.y = c("iso2c", "year", "country") ) 
panelData <- merge(panelData_2, panelData_3,
                     by.x = c("iso2c", "year","country"), 
                     by.y = c("iso2c", "year", "country") ) 

# CLEANING AND TRANSOFMRING THE DATA------------------------------------------------------------

dat <- NULL
dt_wag <- NULL
panelData$iso2c <- NULL

up <- data.table(panelData)
names(up) <- c('year', 'country', 'health', 'mmort', 'pop', 'salaried', 'gdppc')
up$pop <- up$pop/10^6 # Count the population in million
up <- subset(up, up$year < 2015 & up$year > 1994)


# DATA EXPLORATION------------------------------------------------------------------------------

# non-missing observations for each country
bp <- subset(up,
             !is.na(up$salaried) & 
               !is.na(up$mmort) & 
               !is.na(up$pop) & 
               !is.na(up$gdppc) & 
               !is.na(up$health)
             )

bp %>%
  count(country) %>%
  arrange(n) %>%
  print(n = 600)

# checking world trends
world_trend <- up %>%
  group_by(year) %>%
  summarise(
    salaried = weighted.mean(salaried, na.rm = TRUE),
    mmort = weighted.mean(mmort, w = pop, na.rm = TRUE),
    health = weighted.mean(health, w = pop, na.rm = TRUE),
    gdppc = weighted.mean(gdppc, w = pop, na.rm = TRUE))

world_trend$rellnsalaried = log(world_trend$salaried) - first(log(world_trend$salaried))
world_trend$rellnhealth = log(world_trend$health) - first(log(world_trend$health))
world_trend$rellngdp = log(world_trend$gdppc) - first(log(world_trend$gdppc))
world_trend$rellnmmort = log(world_trend$mmort) - first(log(world_trend$mmort)) 

# subplot (1)
p1 <- world_trend %>%
  ggplot(aes(year, mmort)) + 
  geom_line(size = 1, color = 'darkgreen') + 
  ylab('Maternal mortality ratio (per 100,000 live births)') # y-axis label

# subplot (2)
p2 <- world_trend %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = rellnmmort), size = 1, linetype = 'dotted', color = 'darkgreen') +
  geom_line(aes(y = rellnsalaried), size = 1, linetype = 'dashed', color = 'firebrick') +
  geom_text(x = 2004, y = -0.25, label = 'Maternal mortality ratio (per 100,000 live births)', color = 'darkgreen') +
  geom_text(x = 2009, y = -0.05, label = 'salaried female (% of total emolyed)', color = 'firebrick') +
  ylab('log change from 1995')

# arrange these two subplots
grid.arrange(p1, p2)

# subplot (3)
p3 <- world_trend %>%
  ggplot(aes(year, salaried)) + 
  geom_line(size = 1, color = 'firebrick') + 
  ylab('salaried female (% of total emolyed)') # y-axis label

# subplot (4)
p4 <- world_trend %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = rellnmmort), size = 1, linetype = 'dotted', color = 'darkgreen') +
  geom_line(aes(y = rellnhealth), size = 1, linetype = 'dashed', color = 'deepskyblue3') +
  geom_text(x = 2004, y = -0.25, label = 'Maternal mortality ratio (per 100,000 live births)', color = 'darkgreen') +
  geom_text(x = 2006, y = 0.01, label = 'Healthcare expenditure (% of GDP)', color = 'deepskyblue3') +
  ylab('log change from 1995')

# arrange these two subplots
grid.arrange(p3, p4)

# country-specific trends: selected countries, difference between 1995 and 2014
interesting_countries <- c(
  "United Kingdom", "Turkey", "Malaysia", "Hungary", 
  "Pakistan", "venezuela", "El Salvador", "Macedonia, FYR", "Mexico"
)

up_int <- subset(up, up$country %in% interesting_countries, na.rm = TRUE)
p5 <- up_int %>%
  group_by(country) %>%
  ggplot(aes(year, salaried)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Ratio of wage and salaried of female employment')

p6 <- up_int %>%
  group_by(country) %>%
  ggplot(aes(year, mmort)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Maternal mortality ratio (per 100,000 live births)')

p7 <- up_int %>%
  group_by(country) %>%
  ggplot(aes(year, health)) + 
  geom_line(aes(color = country, linetype = country), size = 1) +
  ylab('Health expenditure (% of GDP)')

# arrange these three subplots
grid.arrange(p5, p6, p7)


# examining the balanced panel with no missing values on key variables

ggplot(bp) + aes(x = mmort) +
  geom_histogram (binwidth = 20, 
                  fill ='darkgreen') +
  labs(
    x ="Maternal mortality ratio (per 100,000 live births)",
    title ="Maternal mortality ratio (per 100,000 live births) in years 1995-2014") +
  theme_bw()

ggplot(bp) + aes(x = salaried) +
  geom_histogram (binwidth = 1, 
                  fill ='firebrick') +
  labs(
    x ="Ratio of wage and salaried of female employment')",
    title ="Ratio of wage and salaried of female employment') in years 1995-2014") +
  theme_bw()


ggplot(bp) + aes(x = health) +
  geom_histogram (binwidth = 0.5, 
                  fill ='deepskyblue3') +
  labs(
    x ="Health expenditure (% of GDP)",
    title ="Health expenditure (% of GDP) in years 1995-2014") +
  theme_bw()

ggplot(bp) + aes(x = gdppc) +
  geom_histogram(binwidth = 1000, 
                 fill ='grey') +
  labs(
    x = "GDP per capita distribution across all years, constant 2005 prices",
    title = "Histogram of GDP per capita distribution in years 1995-2014") +
  theme_bw()

stargazer(
  bp,
  header = FALSE, 
  out = "summarystats.html",
  type = 'latex',
  omit = 'year',
  title = "Descriptive Statistics for the variables in the balanced panel")

up$lngdppc <- log(up$gdppc)
up$lnmmort <- log(up$mmort)
up$lnpop <- log(up$pop)
up$lnhealth <- log(up$health)


miss_lnmmort <- merge(up[, mean(mmort, na.rm = TRUE), by = country],
                  up[is.na(mmort), .N, by = country], by = "country", 
                  all = TRUE)

miss_salaried <- merge(up[, mean(salaried, na.rm = TRUE), by = country],
                    up[is.na(salaried), .N, by = country], by = "country", 
                    all = TRUE)

miss_lngdppc <- merge(up[, mean(lngdppc, na.rm = TRUE), by = country],
                      up[is.na(lngdppc), .N, by = country], by = "country", 
                      all = TRUE)

miss_health <- merge(up[, mean(health, na.rm = TRUE), by = country],
                     up[is.na(health), .N, by = country], by = "country", 
                     all = TRUE)

ggplot(miss_lnmmort) + aes(x = N) +
  geom_histogram (binwidth = 0.1, 
                  fill ='darkgreen') +
  labs(
    x ="Average number of missing observations",
    title ="Dist. of avg. missing obs. of Log Maternal mortality") +
  theme_bw()

ggplot(miss_salaried) + aes(x = N) +
  geom_histogram (binwidth = 1, 
                  fill ='firebrick') +
  labs(
    x ="Average number of missing observations",
    title ="Dist. of avg. missing obs. of Ratio of salaried female workers") +
  theme_bw()

ggplot(miss_lngdppc) + aes(x = N) +
  geom_histogram (binwidth = 1, 
                  fill ='grey') +
  labs(
    x ="Average number of missing observations",
    title ="Dist. of avg. missing obs. of Log GDP per capita") +
  theme_bw()

ggplot(miss_health) + aes(x = N) +
  geom_histogram (binwidth = 1, 
                  fill ='deepskyblue3') +
  labs(
    x ="Average number of missing observations",
    title ="Dist. of avg. missing obs. of Healthcare expenditure (% of GDP)") +
  theme_bw()

ggplot(miss_lnmmort, aes(y = V1, x = N)) + 
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x ="Average number of missing observations",
    y = "Mean Log Maternal Mortality",
    title ="Mean Log Maternal mortality in years 1995-2014") +
  theme_bw()

ggplot(miss_salaried, aes(y = V1, x = N)) + 
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x ="Average number of missing observations",
    y = "Mean Ratio of salaried female workers",
    title ="Mean Ratio of salaried female workers in years 1995-2014") +
  theme_bw()

ggplot(miss_lngdppc, aes(y = V1, x = N)) + 
  geom_point(color = "grey") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x ="Average number of missing observations",
    y = "Mean Log GDP per capita",
    title ="Mean Log GDP per capita in years 1995-2014") +
  theme_bw()

ggplot(miss_health, aes(y = V1, x = N)) + 
  geom_point(color = "deepskyblue3") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    x ="Average number of missing observations",
    y = "Mean Health expenditure (% of GDP)",
    title ="Mean Health expenditure (% of GDP) in years 1995-2014") +
  theme_bw()

# ANALYSIS--------------------------------------------------------------------------------------

# xcountry OLS: pooled OLS regardless country difference
dt <- up[, .(
  m_salaried = mean(salaried, na.rm = TRUE),
  m_lnmmort= mean(lnmmort, na.rm = TRUE),
  m_lngdppc = mean(lngdppc, na.rm = TRUE),
  m_lnpop = mean(lnpop, na.rm = TRUE),
  m_lnhealth = mean(lnhealth, na.rm = TRUE)),
  by = .(country)]

p8 <- ggplot(dt, aes(x = m_salaried, y = m_lnmmort)) +
  geom_point(size = 1.5, aes(col = (m_lnhealth))) +
  geom_smooth() +
  labs(
    x = "Average salaried female workers (% of total) per country",
    y = "Average Log Maternal Mortality per country",
    title ="Average Log Maternal Mortality on salaried female workers (% of total)") +
  scale_color_distiller("Log Health expenditure (% of GDP)", palette = "Spectral") +
  theme_bw()

p9 <- ggplot(dt, aes(x = m_salaried, y = m_lnmmort)) +
  geom_point(size = 1.5, aes(col = (m_lngdppc))) +
  geom_smooth() +
  labs(
    x = "Average salaried female workers (% of total) per country",
    y = "Average Log Maternal Mortality per country",
    title ="Average Log Maternal Mortality on salaried female workers (% of total)") +
  scale_color_distiller("Log GDP per capita (const. 2005 $)", palette = "Spectral") +
  theme_bw()

grid.arrange(p8, p9)


# OLS regressions
ols1995 <- lm(data = up[year == 1995], lnmmort ~ salaried)
ols2007 <- lm(data = up[year == 2007], lnmmort ~ salaried)
ols2014 <- lm(data = up[year == 2014], lnmmort ~ salaried)
ols2007_c <- lm(data = up[year == 2007], lnmmort ~ salaried + lnhealth + lngdppc)


ols_models <- list(ols1995, ols2007, ols2014, ols2007_c)

cov_ols_1         <- vcovHC(ols1995, type = "HC1")
rob_ols_1         <- sqrt(diag(cov_ols_1))
cov_ols_2         <- vcovHC(ols2007, type = "HC1")
rob_ols_2         <- sqrt(diag(cov_ols_2))
cov_ols_3         <- vcovHC(ols2014, type = "HC1")
rob_ols_3         <- sqrt(diag(cov_ols_3))
cov_ols_4         <- vcovHC(ols2007_c, type = "HC1")
rob_ols_4         <- sqrt(diag(cov_ols_4))

stargazer(
  title = "OLS models", 
  list(ols1995, ols2007, ols2014, ols2007_c), digits = 2, 
  column.labels = c('ols1995', 'ols2007', 'ols2014', 'ols2007c'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f"),
  dep.var.caption = 'Dependent variable: Maternal Mortality',
  out = "OLS.html",
  notes.align = "l",
  se = list(rob_ols_1, rob_ols_2, rob_ols_3, rob_ols_4),
  header = FALSE, 
  type ='latex'
)


# FE with explicit time dummies

fe1 <- plm( 
  lnmmort ~ salaried + year, data = up, 
  model = 'within')

fe2 <- plm( 
  lnmmort ~ salaried + lngdppc + year, data = up, 
  model = 'within')

fe3 <- plm( 
  lnmmort ~ salaried + lnhealth + year, data = up, 
  model = 'within')

fe4 <- plm( 
  lnmmort ~ salaried + lngdppc + lnhealth + lnpop + year, data = up, 
  model = 'within')

fe_models <- list(fe1, fe2, fe3, fe4)

cov_fe_1         <- vcovSCC(fe1, type = "HC1")
rob_fe_1         <- sqrt(diag(cov_fe_1))
cov_fe_2         <- vcovSCC(fe2, type = "HC1")
rob_fe_2         <- sqrt(diag(cov_fe_2))
cov_fe_3         <- vcovSCC(fe3, type = "HC1")
rob_fe_3         <- sqrt(diag(cov_fe_3))
cov_fe_4         <- vcovSCC(fe4, type = "HC1")
rob_fe_4         <- sqrt(diag(cov_fe_4))

stargazer(
  title = "Comparing FE models with expicit time dummies", 
  list(fe_models), digits = 2, 
  column.labels = c('FE1', 'FE2', 'FE3', 'FE4'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal Mortality',
  out = "FE.html",
  notes.align = "l",
  se = list(rob_fe_1, rob_fe_2, rob_fe_3, rob_fe_4),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex'
)


# FD with explicit time dummies and lags

diff1 <- plm(
  diff(lnmmort) ~ diff(salaried) + year, 
  data = up, model = 'pooling'
)

diff2 <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + year,
  data = up, model = 'pooling'
)

diff3 <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:4) + year, 
  data = up, model = 'pooling'
)

diff4 <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:6) + year,
  data = up, model = 'pooling'
)

cov_fd_1         <- vcovSCC(diff1, type = "HC1")
rob_fd_1    <- sqrt(diag(cov_fd_1))
cov_fd_2         <- vcovSCC(diff2, type = "HC1")
rob_fd_2    <- sqrt(diag(cov_fd_2))
cov_fd_3         <- vcovSCC(diff3, type = "HC1")
rob_fd_3    <- sqrt(diag(cov_fd_3))
cov_fd_4         <- vcovSCC(diff4, type = "HC1")
rob_fd_4    <- sqrt(diag(cov_fd_4))

fd_models <- list(diff1, diff2, diff3, diff4)

stargazer(
  title = "Comparing FD models with expicit time dummies", 
  list(fd_models), digits = 2, 
  column.labels = c('FD1', 'FD2', 'FD3', 'FD4'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal Mortality',
  out = "FD.html",
  notes.align = "l",
  se = list(rob_fd_1, rob_fd_2, rob_fd_3, rob_fd_4),
  add.lines = list(
    c("Cumulative Coeff", '-0.04', '-0.045', '-0.04', '1.26')),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex'
)

# FD with explicit time dummies, lags and controls

diff_c1 <- plm(
  diff(lnmmort) ~ diff(salaried) + year + diff(lnhealth) + diff(lngdppc) + diff(pop), 
  data = up, model = 'pooling'
)

diff_c2 <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + 
    stats::lag(diff(lnhealth), 1:2) + 
    stats::lag(diff(lngdppc), 1:2) + 
    stats::lag(diff(lnpop), 1:2) + 
    year,
  data = up, model = 'pooling'
)

diff_c3 <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:4) +
    stats::lag(diff(lnhealth), 1:4) + 
    stats::lag(diff(lngdppc), 1:4) + 
    stats::lag(diff(lnpop), 1:4) + 
    year, 
  data = up, model = 'pooling'
)


cov_fdc_1         <- vcovSCC(diff_c1, type = "HC1")
rob_fdc_1    <- sqrt(diag(cov_fdc_1))
cov_fdc_2         <- vcovSCC(diff_c2, type = "HC1")
rob_fdc_2    <- sqrt(diag(cov_fdc_2))
cov_fdc_3         <- vcovSCC(diff_c3, type = "HC1")
rob_fdc_3    <- sqrt(diag(cov_fdc_3))

fd_models_controls <- list(diff_c1, diff_c2, diff_c3)

stargazer(
  title = "Comparing FD models with expicit time dummies and controls", 
  list(fd_models_controls), digits = 2, 
  column.labels = c('FD5', 'FD6', 'FD7'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal Mortality',
  out = "FD_controls.html",
  notes.align = "l",
  se = list(rob_fdc_1, rob_fdc_2, rob_fdc_3),
  add.lines = list(
    c("Cumulative Coeff", '-0.01', '-0.104', '0.02')),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex'
)

# Multiple models

multiple_models = list(ols2007_c, fe4, diff2, diff_c2)

stargazer(
  title = "Comparing multiple models", 
  list(multiple_models), digits = 2, 
  column.labels = c('OLS2007c', 'FE4c', 'FD2', 'FD2c'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal mortality',
  out = "Multiple.html",
  notes.align = "l",
  se = list(rob_ols_4, rob_fe_4, rob_fd_2, rob_fdc_2),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex',
  omit = c('year', 'lngdppc', 'lnhealth', 'lnpop', 
           'stats::lag(diff(lnhealth), 1:2)', 
           'stats::lag(diff(lngdppc), 1:2)',
           'stats::lag(diff(lnpop), 1:2)')
)

# Grouping countries by income

# Group 1
up %>% 
  select(salaried, gdppc, pop) %>%
  as.data.frame() %>%
  stargazer(
    type = 'text', flip = TRUE, digits = 1,
    summary.stat = c('mean', 'min', 'median', 'p25', 'p75', 'max', 'n')
  )

up$group <- ifelse(up$gdppc <= 8500,1,2)
up_1 <- subset(up, up$group == 1)
up_2 <- subset(up, up$group == 2)

g_fd2c <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + 
    stats::lag(diff(lnhealth), 1:2) + 
    stats::lag(diff(lngdppc), 1:2) + 
    stats::lag(diff(lnpop), 1:2) + 
    year,
  data = up_1, model = 'pooling'
)

g_fe4c <- plm( 
  lnmmort ~ salaried + lngdppc + lnhealth + lnpop + year, 
  data = up_1, 
  model = 'within')

# Group 2

gg_fd2c <- plm(
  diff(lnmmort) ~ diff(salaried) + stats::lag(diff(salaried), 1:2) + 
    stats::lag(diff(lnhealth), 1:2) + 
    stats::lag(diff(lngdppc), 1:2) + 
    stats::lag(diff(lnpop), 1:2) + 
    year,
  data = up_2, model = 'pooling'
)

gg_fe4c <- plm( 
  lnmmort ~ salaried + lngdppc + lnhealth + lnpop + year, 
  data = up_2, 
  model = 'within')

# Standard errors were adjusted to make sure robust standard errors are used
g_cov_fd        <- vcovSCC(g_fd2c, type = "HC1")
g_rob_fd        <- sqrt(diag(g_cov_fd))
g_cov_fe        <- vcovSCC(g_fe4c, type = "HC1")
g_rob_fe        <- sqrt(diag(g_cov_fe))
gg_cov_fd        <- vcovSCC(gg_fd2c, type = "HC1")
gg_rob_fd        <- sqrt(diag(gg_cov_fd))
gg_cov_fe        <- vcovSCC(gg_fe4c, type = "HC1")
gg_rob_fe        <- sqrt(diag(gg_cov_fe))

stargazer(
  title = "Comparing multiple models for high- and low-income countries", 
  list(g_fd2c, g_fe4c, gg_fd2c, gg_fe4c), digits = 2, 
  column.labels = c('FD2c, G:1', 'FE4c, G:1', 'FD2c, G:2', 'FE4c, G:2'),
  model.names = FALSE,
  omit.stat = c("adj.rsq", "f", "ser"),
  dep.var.caption = 'Dependent variable: Log Maternal mortality',
  out = "Grouping.html",
  notes.align = "l",
  se = list(g_rob_fd, g_rob_fe, gg_rob_fd, gg_rob_fe),
  dep.var.labels.include = FALSE,
  header = FALSE, 
  type ='latex',
  omit = c('year', 'lngdppc', 'lnhealth', 'lnpop', 
           'stats::lag(diff(lnhealth), 1:2)', 
           'stats::lag(diff(lngdppc), 1:2)',
           'stats::lag(diff(lnpop), 1:2)')
)

