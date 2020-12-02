# File: corp_default_index_charts.R
# Authors: Tyler Pike
# Date: 
# Note(s): Create charts of the NFCH and subindexes
#          Users will need access to the FRB plotting package, policyPlot

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
standardize = function(X){return((X-mean(X,na.rm = T))/sd(X,na.rm = T))}

Index = 
  read.csv('./Data/Ouput/indexes_aggregate.csv', stringsAsFactors = F) %>%
  mutate(date = as_date(date),
         NFCH_m = standardize(NFCH_m),
         NFCH_s = standardize(NFCH_s),
         NFCH = standardize(NFCH))

#------------------------------------------------
# Create index charts
#------------------------------------------------

# chart NFCH_m
pdf('./Evaluation/Index/NFCH_m.pdf', width = 11.5, height = 4.25 )
par(fig=c(0,1,0,1),mar=c(1.5,2,2.5,2))
  rplot.line(select(Index, date, NFCH_m),
             Y2lab = 'Standard Deviations',
             Y2lim = c(-4,4),
             Y2int = 1,
             NBER = T,
             shading.border = F,
             Enddatelab = F)
dev.off()

# chart NFCH_s
pdf('./Evaluation/Index/NFCH_s.pdf', width = 11.5, height = 4.25 )
par(fig=c(0,1,0,1),mar=c(1.5,2,2.5,2))
  rplot.line(select(Index, date, NFCH_s),
             Y2lab = 'Standard Deviations',
             Y2lim = c(-4,4),
             Y2int = 1,
             NBER = T,
             shading.border = F,
             Enddatelab = F)
dev.off()

# chart NFCH
pdf('./Evaluation/Index/NFCH.pdf', width = 11.5, height = 4.25 )
par(fig=c(0,1,0,1),mar=c(1.5,2,2.5,2))
  rplot.line(select(Index, date, NFCH),
             Y2lab = 'Standard Deviations',
             Y2lim = c(-4,4),
             Y2int = 1,
             NBER = T,
             shading.border = F,
             Enddatelab = F)
dev.off()

#------------------------------------------------
# Create sub-indexes charts
#------------------------------------------------
pdf('./Evaluation/Index/NFCH_s_components.pdf', width = 8, height = 6.25)
par(mar = c(2,2,2,2), mfrow = c(2,1))

  chart = 
    rplot.line(select(Index, date, sd_smooth),
               Title = 'Standard deviation',
               Y1lab = 'Quarterly',
               NBER = T,
               shading.border = F,
               Enddatelab = F)
  
  chart = 
    rplot.line(select(Index, date, skew_smooth),
               Title = 'Skew',
               Y1lab = 'Quarterly',
               NBER = T,
               shading.border = F,
               Enddatelab = F)

dev.off()

