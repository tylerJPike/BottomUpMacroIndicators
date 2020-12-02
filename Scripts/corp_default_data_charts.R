# File: corp_default_data_charts.R
# Author: Tyler Pike
# Date: 10/9/2019
# Note(s): Create time series charts of bankruptcies and crsp/compustat universe.

# clear enviroment
rm(list=ls())

# libraries
library(policyPlot)
library(lubridate)
library(dplyr)
library(TTR)
library(moments)

# charting parameters
policyPlot::setcmpar()

#------------------------------------------------
# Load the indexes
#------------------------------------------------
data.fisd = readRDS("./Data/corp_default_data.RDS")
ts.fisd = data.fisd %>% mutate(default = lag(y1)) %>% 
  group_by(quarter) %>% 
  summarize(numerator = sum(default), 
            denominator = n()) %>%
  mutate(prob = if_else(is.na(denominator) | is.na(numerator), 0, 100*numerator/denominator)) %>%
  filter(quarter <= 2020.1)
ts.fisd$date = as.Date(as.yearqtr(as.character(ts.fisd$quarter), format = "%Y.%q"))

#------------------------------------------------
# Create Charts
#------------------------------------------------

# chart firm-level data over time
pdf('./Data/Figures/observed_data.pdf', height = 11.5, width = 8.5)
par(mfrow = c(3,1), mar = c(3,2,2,2))

  chart = 
    rplot.line(select(ts.fisd, date, numerator),
               Title = 'Observed defaults',
               Y2lab = 'Firms',
               Y1lab = 'Quarterly',
               Y2lim = c(-5,30),
               Y2int = 5,
               NBER = T,
               shading.border = F,
               Enddatelab = F,
               Zeroline = T)
  
  chart = 
    rplot.line(select(ts.fisd, date, denominator),
               Title = 'Observed firms',
               Y2lab = 'Firms',
               Y1lab = 'Quarterly',
               Y2lim = c(0,15000),
               Y2int = 3000,
               NBER = T,
               shading.border = F,
               Enddatelab = F)
  
  chart = 
    rplot.line(select(ts.fisd, date, prob),
               Title = 'Observed unconditional probability of default',
               Y2lab = 'Basis points',
               Y1lab = 'Quarterly',
               Y2lim = c(-.05,0.30),
               Y2int = 0.05,
               NBER = T,
               shading.border = F,
               Enddatelab = F,
               Zeroline = T)

dev.off()