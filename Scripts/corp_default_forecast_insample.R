# File: corp_default_forecast_insample.R
# Authors: Clay Wagar, Tyler Pike
# Date: 
# Note(s): Perform in-sample forecasting exercises 
#          Users will need access to FRB serves to access data, but may substitute public sources as desired

# clear enviroment
rm(list=ls())

# import libraries
library(lubridate)
library(dplyr)
library(tis)
library(stfm.helper, lib.loc="/stfm/shared1/R")
library(data.table)

#--------------------------------------------------------------------
# Pull other data for comparisons
#--------------------------------------------------------------------
# NFCH 
nfch = read.csv('./Data/Ouput/indexes_aggregate.csv',stringsAsFactors = F) %>%
  mutate(date = lubridate::ymd(date),
         date = ceiling_date(date, unit = 'months')) %>%
  select(date, meanDef = NFCH)
day(nfch$date) = day(nfch$date) - 1

# Chicago NFCI
quantmod::getSymbols('NFCI', src='FRED')
chicago  = data.table(date = index(NFCI), coredata(NFCI)) 
chi = chicago[,list(nfci = mean(NFCI)),by = list(quarter(date),year(date))]
chi$quarter = paste0(chi$year,".",chi$quarter)
chi$year <- NULL

# Treasury yields
treas3month = getfame_dt("sveny0025","yields")
treas3month$year = year(treas3month$date)
treas3month$quarter = quarter(treas3month$date)

treas3month = na.omit(treas3month)
t3 = treas3month[,.SD[which.max(date),],by=list(quarter,year)]
t3$date <- NULL #since it will be different than other data sets
t3$quarter = paste0(t3$year,".",t3$quarter)
t3$year <- NULL

treas10year = getfame_dt("sveny1000","yields")
treas10year$year = year(treas10year$date)
treas10year$quarter = quarter(treas10year$date)

treas10year = na.omit(treas10year)
t10 = treas10year[,.SD[which.max(date),],by=list(quarter,year)]
t10$date <- NULL
t10$quarter = paste0(t10$year,".",t10$quarter)
t10$year <- NULL

treasury = merge(t3,t10,by="quarter")

# Federal funds rate
ff = getfame_dt("rifspff_n.b","us") 
ff = ff[,list(ff = mean(rifspff_n.b,na.rm=TRUE)),by=list(quarter(date),year(date))]
ff$quarter = paste0(ff$year,".",ff$quarter)
ff$year <- NULL

# Core PCE inflation
pce = getfame_dt("ph.ce@maafe.q","us") 
names(pce)[2] = "pce"
pce$quarter = paste0(year(pce$date),".",quarter(pce$date))


# EBP and GZ spread
gz = read.csv("./Input/ebp_public.csv")
gz = data.table(gz)
gz$date = as.character(gz$date)
gz$date = paste0(gz$date,"01")
gz$date = as.Date(gz$date,"%b%Y%d")
gz$quarter = quarter(gz$date)
gz$year = year(gz$date)
GZ = gz[,list(ebp = mean(ebp)),by = list(quarter,year)]
GZ$quarter = paste0(GZ$year,".",GZ$quarter)
GZ$year <- NULL

# Nonfarm payroll employment
payroll = getfame_dt("ee.q","us")
names(payroll)[2] = "payroll"

# Industrial production
ip = getfame_dt("jqi_i12ymf.m","us")
ip$year = year(ip$date)
ip$quarter = quarter(ip$date)
IP = ip[,list(ip = mean(jqi_i12ymf.m)),by = list(quarter,year)]
IP = ip[,list(ip = mean(jqi_i12ymf.m)),by = list(quarter,year)]
IP$quarter = paste0(IP$year,".",IP$quarter)
IP$year = NULL

# Real GDP
gdp = getfame_dt("gdp_xcw_09.q","us")
names(gdp)[2] = "gdp"

# Unemployment
unemp = getfame_dt("ruc.q","us")
names(unemp)[2] = "unemp"

# merge data 
data = purrr::reduce(list(treasury,chi,ff,pce, GZ,IP), inner_join, by = 'quarter') 
data = purrr::reduce(list(data, gdp, unemp, payroll, nfch), inner_join, by = 'date')

# filter by date
data = filter(data, date < as.Date('2020-01-01'))

# cast data as data table
data = data.table(data)

#--------------------------------------------------------------------
# Transform data and create controls
#--------------------------------------------------------------------

# term spread, defined as the 10-year minus the 3-month Treasury yield
data[,TS := sveny1000 - sveny0025]

# real federal funds rate is defined as the average effective federal funds rate less realized inflation 
#  is given by the log difference between core PCE in period t-1 and its value lagged a year earlier
data[,pce.lag := shift(pce,1,type="lag")]
data[,pce.lag.lag4 := shift(pce.lag,4,type="lag")]
data[,realizedInflation := log(pce.lag / pce.lag.lag4)*100]
data[,RFF := ff - realizedInflation]

# divide by standard deviation so we may interpret unit increases as a standard deviation increase
data[, meanDef := meanDef/sd(meanDef, na.rm = T)]
data[, ebp := ebp/sd(ebp, na.rm = T)]
data[, nfci := nfci/sd(nfci, na.rm = T)]

# create leads and lags, using Egon and Simon's timing and annualization
#  NOTE: all dependent variables for regresion will be denoted gP  where g means gradient
# Payroll
data[,payroll.lag := shift(payroll,1,type="lag")]
data[,payroll.lag2 := shift(payroll,2,type="lag")]
data[,payroll.lag3 := shift(payroll,3,type="lag")]
data[,payroll.lag4 := shift(payroll,4,type="lag")]
data[,payroll.lag5 := shift(payroll,5,type="lag")]

data[,payroll.l1  := shift(payroll,1,type="lead")]
data[,payroll.l2  := shift(payroll,2,type="lead")]
data[,payroll.l3  := shift(payroll,3,type="lead")]
data[,payroll.l4  := shift(payroll,4,type="lead")]

data[,gPayroll := 400/(1+0) * log(payroll / payroll.lag)]  # h = 0
data[,gPayroll.l1 := 400/(1+1) * log(payroll.l1 / payroll.lag)]  # h = 1
data[,gPayroll.l2 := 400/(1+2) * log(payroll.l2 / payroll.lag)]  # h = 2
data[,gPayroll.l3 := 400/(1+3) * log(payroll.l3 / payroll.lag)]  # h = 3
data[,gPayroll.l4 := 400/(1+4) * log(payroll.l4 / payroll.lag)]  # h = 4

data[,gPayroll.lag1 := 400/(1+0) * log(payroll.lag / payroll.lag2)]
data[,gPayroll.lag2 := 400/(1+0) * log(payroll.lag2 / payroll.lag3)]
data[,gPayroll.lag3 := 400/(1+0) * log(payroll.lag3 / payroll.lag4)]
data[,gPayroll.lag4 := 400/(1+0) * log(payroll.lag4 / payroll.lag5)]


# Industrial production
data[,ip.lag := shift(ip,1,type="lag")]
data[,ip.lag2 := shift(ip,2,type="lag")]
data[,ip.lag3 := shift(ip,3,type="lag")]
data[,ip.lag4 := shift(ip,4,type="lag")]
data[,ip.lag5 := shift(ip,5,type="lag")]

data[,ip.l1  := shift(ip,1,type="lead")]
data[,ip.l2  := shift(ip,2,type="lead")]
data[,ip.l3  := shift(ip,3,type="lead")]
data[,ip.l4  := shift(ip,4,type="lead")]

data[,gIp := 400/(1+0) * log(ip / ip.lag)]  # h = 0
data[,gIp.l1 := 400/(1+1) * log(ip.l1 / ip.lag)]  # h = 1
data[,gIp.l2 := 400/(1+2) * log(ip.l2 / ip.lag)]  # h = 2
data[,gIp.l3 := 400/(1+3) * log(ip.l3 / ip.lag)]  # h = 3
data[,gIp.l4 := 400/(1+4) * log(ip.l4 / ip.lag)]  # h = 4

data[,gIp.lag1 := 400/(1+0) * log(ip.lag / ip.lag2)]
data[,gIp.lag2 := 400/(1+0) * log(ip.lag2 / ip.lag3)]
data[,gIp.lag3 := 400/(1+0) * log(ip.lag3 / ip.lag4)]
data[,gIp.lag4 := 400/(1+0) * log(ip.lag4 / ip.lag5)]


# Real gdp
data[,gdp.lag := shift(gdp,1,type="lag")]
data[,gdp.lag2 := shift(gdp,2,type="lag")]
data[,gdp.lag3 := shift(gdp,3,type="lag")]
data[,gdp.lag4 := shift(gdp,4,type="lag")]
data[,gdp.lag5 := shift(gdp,5,type="lag")]

data[,gdp.l1  := shift(gdp,1,type="lead")]
data[,gdp.l2  := shift(gdp,2,type="lead")]
data[,gdp.l3  := shift(gdp,3,type="lead")]
data[,gdp.l4  := shift(gdp,4,type="lead")]

data[,gGdp := 400/(1+0) * log(gdp / gdp.lag)]  # h = 0
data[,gGdp.l1 := 400/(1+1) * log(gdp.l1 / gdp.lag)]  # h = 1
data[,gGdp.l2 := 400/(1+2) * log(gdp.l2 / gdp.lag)]  # h = 2
data[,gGdp.l3 := 400/(1+3) * log(gdp.l3 / gdp.lag)]  # h = 3
data[,gGdp.l4 := 400/(1+4) * log(gdp.l4 / gdp.lag)]  # h = 4

data[,gGdp.lag1 := 400/(1+0) * log(gdp.lag / gdp.lag2)]
data[,gGdp.lag2 := 400/(1+0) * log(gdp.lag2 / gdp.lag3)]
data[,gGdp.lag3 := 400/(1+0) * log(gdp.lag3 / gdp.lag4)]
data[,gGdp.lag4 := 400/(1+0) * log(gdp.lag4 / gdp.lag5)]


# Unemployment
data[,unemp.lag := shift(unemp,1,type="lag")]
data[,unemp.lag2 := shift(unemp,2,type="lag")]
data[,unemp.lag3 := shift(unemp,3,type="lag")]
data[,unemp.lag4 := shift(unemp,4,type="lag")]
data[,unemp.lag5 := shift(unemp,5,type="lag")]

data[,unemp.l1  := shift(unemp,1,type="lead")]
data[,unemp.l2  := shift(unemp,2,type="lead")]
data[,unemp.l3  := shift(unemp,3,type="lead")]
data[,unemp.l4  := shift(unemp,4,type="lead")]

data[,gUnemp := 400/(1+0) * log(unemp / unemp.lag)]  # h = 0
data[,gUnemp.l1 := 400/(1+1) * log(unemp.l1 / unemp.lag)]  # h = 1
data[,gUnemp.l2 := 400/(1+2) * log(unemp.l2 / unemp.lag)]  # h = 2
data[,gUnemp.l3 := 400/(1+3) * log(unemp.l3 / unemp.lag)]  # h = 3
data[,gUnemp.l4 := 400/(1+4) * log(unemp.l4 / unemp.lag)]  # h = 4

data[,gUnemp.lag1 := 400/(1+0) * log(unemp.lag / unemp.lag2)]
data[,gUnemp.lag2 := 400/(1+0) * log(unemp.lag2 / unemp.lag3)]
data[,gUnemp.lag3 := 400/(1+0) * log(unemp.lag3 / unemp.lag4)]
data[,gUnemp.lag4 := 400/(1+0) * log(unemp.lag4 / unemp.lag5)]

# ensure the same sample across tests
data = na.omit(data)

#--------------------------------------------------------------------
# Estimate regressions (in-sample forecasts)
#--------------------------------------------------------------------

# Payroll
fPay.4 <- lm(gPayroll.l4 ~ TS + RFF + gPayroll.lag1 + gPayroll.lag2 + gPayroll.lag3 + gPayroll.lag4 + nfci + ebp + meanDef ,data = data)
fPay.4.nfci <- lm(gPayroll.l4 ~ TS + RFF + gPayroll.lag1 + gPayroll.lag2 + gPayroll.lag3 + gPayroll.lag4 + nfci ,data = data)
fPay.4.nfch <- lm(gPayroll.l4 ~  TS + RFF + gPayroll.lag1 + gPayroll.lag2 + gPayroll.lag3 + gPayroll.lag4 + meanDef ,data = data)
fPay.4.ebp <- lm(gPayroll.l4 ~ TS + RFF + gPayroll.lag1 + gPayroll.lag2 + gPayroll.lag3 + gPayroll.lag4 + ebp ,data = data)
fPay.4.ebp.nfch <- lm(gPayroll.l4 ~ TS + RFF + gPayroll.lag1 + gPayroll.lag2 + gPayroll.lag3 + gPayroll.lag4 + ebp + meanDef,data = data)
fPay.4.nfci.nfch <- lm(gPayroll.l4 ~ TS + RFF + gPayroll.lag1 + gPayroll.lag2 + gPayroll.lag3 + gPayroll.lag4 + nfci + meanDef,data = data)

fPay.4.vcov <- lmtest::coeftest(fPay.4,vcov=sandwich::NeweyWest(fPay.4,prewhite=FALSE))
fPay.4.nfci.vcov <- lmtest::coeftest(fPay.4.nfci,vcov = sandwich::NeweyWest(fPay.4.nfci,prewhite = FALSE))
fPay.4.nfch.vcov <- lmtest::coeftest(fPay.4.nfch,vcov = sandwich::NeweyWest(fPay.4.nfch,prewhite = FALSE))
fPay.4.ebp.vcov <- lmtest::coeftest(fPay.4.ebp,vcov = sandwich::NeweyWest(fPay.4.ebp,prewhite = FALSE))
fPay.4.ebp.nfch.vcov <- lmtest::coeftest(fPay.4.ebp.nfch,vcov = sandwich::NeweyWest(fPay.4.ebp.nfch,prewhite = FALSE))
fPay.4.nfci.nfch.vocv <- lmtest::coeftest(fPay.4.nfci.nfch,vcov = sandwich::NeweyWest(fPay.4.nfci.nfch,prewhite = FALSE))

# Industrial Production
fIp.4 <- lm(gIp.l4 ~ TS + RFF + gIp.lag1 + gIp.lag2 + gIp.lag3 + gIp.lag4 + nfci + ebp + meanDef ,data = data)
fIp.4.nfci <- lm(gIp.l4 ~ TS + RFF + gIp.lag1 + gIp.lag2 + gIp.lag3 + gIp.lag4 + nfci ,data = data)
fIp.4.nfch <- lm(gIp.l4 ~  TS + RFF + gIp.lag1 + gIp.lag2 + gIp.lag3 + gIp.lag4 + meanDef ,data = data)
fIp.4.ebp <- lm(gIp.l4 ~ TS + RFF + gIp.lag1 + gIp.lag2 + gIp.lag3 + gIp.lag4 + ebp ,data = data)
fIp.4.ebp.nfch <- lm(gIp.l4 ~ TS + RFF + gIp.lag1 + gIp.lag2 + gIp.lag3 + gIp.lag4 + ebp + meanDef,data = data)
fIp.4.nfci.nfch <- lm(gIp.l4 ~ TS + RFF + gIp.lag1 + gIp.lag2 + gIp.lag3 + gIp.lag4 + nfci + meanDef,data = data)

fIp.4.vcov <- lmtest::coeftest(fIp.4,vcov=sandwich::NeweyWest(fIp.4,prewhite=FALSE))
fIp.4.nfci.vcov <- lmtest::coeftest(fIp.4.nfci,vcov = sandwich::NeweyWest(fIp.4.nfci,prewhite = FALSE))
fIp.4.nfch.vcov <- lmtest::coeftest(fIp.4.nfch,vcov = sandwich::NeweyWest(fIp.4.nfch,prewhite = FALSE))
fIp.4.ebp.vcov <- lmtest::coeftest(fIp.4.ebp,vcov = sandwich::NeweyWest(fIp.4.ebp,prewhite = FALSE))
fIp.4.ebp.nfch.vcov <- lmtest::coeftest(fIp.4.ebp.nfch,vcov = sandwich::NeweyWest(fIp.4.ebp.nfch,prewhite = FALSE))
fIp.4.nfci.nfch.vocv <- lmtest::coeftest(fIp.4.nfci.nfch,vcov = sandwich::NeweyWest(fIp.4.nfci.nfch,prewhite = FALSE))

# Real GDP
fGdp.4 <- lm(gGdp.l4 ~ TS + RFF + gGdp.lag1 + gGdp.lag2 + gGdp.lag3 + gGdp.lag4 + nfci + ebp + meanDef ,data = data)
fGdp.4.nfci <- lm(gGdp.l4 ~ TS + RFF + gGdp.lag1 + gGdp.lag2 + gGdp.lag3 + gGdp.lag4 + nfci ,data = data)
fGdp.4.nfch <- lm(gGdp.l4 ~  TS + RFF + gGdp.lag1 + gGdp.lag2 + gGdp.lag3 + gGdp.lag4 + meanDef ,data = data)
fGdp.4.ebp <- lm(gGdp.l4 ~ TS + RFF + gGdp.lag1 + gGdp.lag2 + gGdp.lag3 + gGdp.lag4 + ebp ,data = data)
fGdp.4.ebp.nfch <- lm(gGdp.l4 ~ TS + RFF + gGdp.lag1 + gGdp.lag2 + gGdp.lag3 + gGdp.lag4 + ebp + meanDef,data = data)
fGdp.4.nfci.nfch <- lm(gGdp.l4 ~ TS + RFF + gGdp.lag1 + gGdp.lag2 + gGdp.lag3 + gGdp.lag4 + nfci + meanDef,data = data)

fGdp.4.vcov <- lmtest::coeftest(fGdp.4,vcov=sandwich::NeweyWest(fGdp.4,prewhite=FALSE))
fGdp.4.nfci.vcov <- lmtest::coeftest(fGdp.4.nfci,vcov = sandwich::NeweyWest(fGdp.4.nfci,prewhite = FALSE))
fGdp.4.nfch.vcov <- lmtest::coeftest(fGdp.4.nfch,vcov = sandwich::NeweyWest(fGdp.4.nfch,prewhite = FALSE))
fGdp.4.ebp.vcov <- lmtest::coeftest(fGdp.4.ebp,vcov = sandwich::NeweyWest(fGdp.4.ebp,prewhite = FALSE))
fGdp.4.ebp.nfch.vcov <- lmtest::coeftest(fGdp.4.ebp.nfch,vcov = sandwich::NeweyWest(fGdp.4.ebp.nfch,prewhite = FALSE))
fGdp.4.nfci.nfch.vocv <- lmtest::coeftest(fGdp.4.nfci.nfch,vcov = sandwich::NeweyWest(fGdp.4.nfci.nfch,prewhite = FALSE))

# Unemployment
fUnemp.4 <- lm(gUnemp.l4 ~ TS + RFF + gUnemp.lag1 + gUnemp.lag2 + gUnemp.lag3 + gUnemp.lag4 + nfci + ebp + meanDef ,data = data)
fUnemp.4.nfci <- lm(gUnemp.l4 ~ TS + RFF + gUnemp.lag1 + gUnemp.lag2 + gUnemp.lag3 + gUnemp.lag4 + nfci ,data = data)
fUnemp.4.nfch <- lm(gUnemp.l4 ~  TS + RFF + gUnemp.lag1 + gUnemp.lag2 + gUnemp.lag3 + gUnemp.lag4 + meanDef ,data = data)
fUnemp.4.ebp <- lm(gUnemp.l4 ~ TS + RFF + gUnemp.lag1 + gUnemp.lag2 + gUnemp.lag3 + gUnemp.lag4 + ebp ,data = data)
fUnemp.4.ebp.nfch <- lm(gUnemp.l4 ~ TS + RFF + gUnemp.lag1 + gUnemp.lag2 + gUnemp.lag3 + gUnemp.lag4 + ebp + meanDef,data = data)
fUnemp.4.nfci.nfch <- lm(gUnemp.l4 ~ TS + RFF + gUnemp.lag1 + gUnemp.lag2 + gUnemp.lag3 + gUnemp.lag4 + nfci + meanDef,data = data)

fUnemp.4.vcov <- lmtest::coeftest(fUnemp.4,vcov=sandwich::NeweyWest(fUnemp.4,prewhite=FALSE))
fUnemp.4.nfci.vcov <- lmtest::coeftest(fUnemp.4.nfci,vcov = sandwich::NeweyWest(fUnemp.4.nfci,prewhite = FALSE))
fUnemp.4.nfch.vcov <- lmtest::coeftest(fUnemp.4.nfch,vcov = sandwich::NeweyWest(fUnemp.4.nfch,prewhite = FALSE))
fUnemp.4.ebp.vcov <- lmtest::coeftest(fUnemp.4.ebp,vcov = sandwich::NeweyWest(fUnemp.4.ebp,prewhite = FALSE))
fUnemp.4.ebp.nfch.vcov <- lmtest::coeftest(fUnemp.4.ebp.nfch,vcov = sandwich::NeweyWest(fUnemp.4.ebp.nfch,prewhite = FALSE))
fUnemp.4.nfci.nfch.vocv <- lmtest::coeftest(fUnemp.4.nfci.nfch,vcov = sandwich::NeweyWest(fUnemp.4.nfci.nfch,prewhite = FALSE))

#--------------------------------------------------------------------
# Create latex output
#--------------------------------------------------------------------
# Payroll Employment 
stargazer::stargazer(
  fPay.4.ebp,
  fPay.4.nfci,
  fPay.4.nfch,
  fPay.4.ebp.nfch,
  fPay.4.nfci.nfch,
  fPay.4,
  title = "Forecast Results: Payroll Employment",
  align=TRUE,
  #add.lines = list(c("Fixed effects?", "No", "No")),
  se = list(fPay.4.ebp.vcov[,2],fPay.4.nfci.vcov[,2],fPay.4.nfch.vcov[,2],fPay.4.ebp.nfch.vcov[,2],fPay.4.nfci.nfch.vocv[,2],fPay.4.vcov[,2]),
  omit = c("lag","Constant","TS","RFF"),
  omit.stat = c("rsq","f","ser","n"),
  model.numbers = FALSE,
  dep.var.caption = "Payroll Employment",
  dep.var.labels = "",
  covariate.labels = c("EBP","NFCI",'NFCD'),
  notes.align = "l",
  omit.table.layout = "n",
  star.cutoffs = c(0.1,0.05,0.01),
  p = list(fPay.4.ebp.vcov[,4],fPay.4.nfci.vcov[,4],fPay.4.nfch.vcov[,4],fPay.4.ebp.nfch.vcov[,4],fPay.4.nfci.nfch.vocv[,4],fPay.4.vcov[,4]),
  style = "aer")

# Industrial Production 
stargazer::stargazer(
  fIp.4.ebp,
  fIp.4.nfci,
  fIp.4.nfch,
  fIp.4.ebp.nfch,
  fIp.4.nfci.nfch,
  fIp.4,
  title = "Forecast Results: Industrial Production",
  align=TRUE,
  #add.lines = list(c("Fixed effects?", "No", "No")),
  se = list(fIp.4.ebp.vcov[,2],fIp.4.nfci.vcov[,2],fIp.4.nfch.vcov[,2],fIp.4.ebp.nfch.vcov[,2],fIp.4.nfci.nfch.vocv[,2],fIp.4.vcov[,2]),
  omit = c("lag","Constant","TS","RFF"),
  omit.stat = c("rsq","f","ser","n"),
  model.numbers = FALSE,
  dep.var.caption = "Payroll Employment",
  dep.var.labels = "",
  covariate.labels = c("EBP","NFCI",'NFCD'),
  notes.align = "l",
  omit.table.layout = "n",
  star.cutoffs = c(0.1,0.05,0.01),
  p = list(fIp.4.ebp.vcov[,4],fIp.4.nfci.vcov[,4],fIp.4.nfch.vcov[,4],fIp.4.ebp.nfch.vcov[,4],fIp.4.nfci.nfch.vocv[,4],fIp.4.vcov[,4]),
  style = "aer")

# Real GDP
stargazer::stargazer(
  fGdp.4.ebp,
  fGdp.4.nfci,
  fGdp.4.nfch,
  fGdp.4.ebp.nfch,
  fGdp.4.nfci.nfch,
  fGdp.4,
  title = "Forecast Results: Industrial Production",
  align=TRUE,
  #add.lines = list(c("Fixed effects?", "No", "No")),
  se = list(fGdp.4.ebp.vcov[,2],fGdp.4.nfci.vcov[,2],fGdp.4.nfch.vcov[,2],fGdp.4.ebp.nfch.vcov[,2],fGdp.4.nfci.nfch.vocv[,2],fGdp.4.vcov[,2]),
  omit = c("lag","Constant","TS","RFF"),
  omit.stat = c("rsq","f","ser","n"),
  model.numbers = FALSE,
  dep.var.caption = "Payroll Employment",
  dep.var.labels = "",
  covariate.labels = c("EBP","NFCI",'NFCD'),
  notes.align = "l",
  omit.table.layout = "n",
  star.cutoffs = c(0.1,0.05,0.01),
  p = list(fGdp.4.ebp.vcov[,4],fGdp.4.nfci.vcov[,4],fGdp.4.nfch.vcov[,4],fGdp.4.ebp.nfch.vcov[,4],fGdp.4.nfci.nfch.vocv[,4],fGdp.4.vcov[,4]),
  style = "aer")

# Unemployment
stargazer::stargazer(
  fUnemp.4.ebp,
  fUnemp.4.nfci,
  fUnemp.4.nfch,
  fUnemp.4.ebp.nfch,
  fUnemp.4.nfci.nfch,
  fUnemp.4,
  title = "Forecast Results: Industrial Production",
  align=TRUE,
  #add.lines = list(c("Fixed effects?", "No", "No")),
  se = list(fUnemp.4.ebp.vcov[,2],fUnemp.4.nfci.vcov[,2],fUnemp.4.nfch.vcov[,2],fUnemp.4.ebp.nfch.vcov[,2],fUnemp.4.nfci.nfch.vocv[,2],fUnemp.4.vcov[,2]),
  omit = c("lag","Constant","TS","RFF"),
  omit.stat = c("rsq","f","ser","n"),
  model.numbers = FALSE,
  dep.var.caption = "Payroll Employment",
  dep.var.labels = "",
  covariate.labels = c("EBP","NFCI",'NFCD'),
  notes.align = "l",
  omit.table.layout = "n",
  star.cutoffs = c(0.1,0.05,0.01),
  p = list(fUnemp.4.ebp.vcov[,4],fUnemp.4.nfci.vcov[,4],fUnemp.4.nfch.vcov[,4],fUnemp.4.ebp.nfch.vcov[,4],fUnemp.4.nfci.nfch.vocv[,4],fUnemp.4.vcov[,4]),
  style = "aer")

