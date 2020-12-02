# File: corp_default_index_creation.R
# Authors: Tyler Pike
# Date: 
# Note(s): Create aggregate index of non-financial corporate health (NFCH)

# THE USER WILL NEED TO SUPPLY THIER OWN WRDS CREDENTIALS

# clear enviroment
rm(list=ls())


# libraries
library(policyPlot)
library(lubridate)
library(dplyr)
library(TTR)
library(moments)
library(RPostgres)

# establish connection to WRDS
wrds = dbConnect(Postgres(),
             #    host= user specified host,
             #    port= user specified port,
             #    user= username,
             #    password= password,
                 sslmode='require',
                 dbname='wrds')

#------------------------------------------------
# Load and fix the data
#------------------------------------------------
# load the firm-level predictions
load('./Estimation/Recursive1_predictions_y8.RData')

# append the new data
rf = pred_list[['RF']] %>% 
  mutate(Time = lubridate::ymd(paste0(substr(Time,1,4),'-',as.numeric(substr(Time,6,6))*3,'-1'))) %>%
  arrange(Time)  %>%
  group_by(q = quarter(Time), y = year(Time), GVKEY) %>%
  summarize(Time = ymd(paste0(max(y),'-',max(q)*3,'-01')),
            preds = mean(preds, na.rm = T)) %>%
  ungroup() %>%
  select(-q, -y) %>%
  na.omit()

#------------------------------------------------
# Load and merge corporate assets 
#------------------------------------------------
# set timing parameters
sdate = min(rf$Time)
edate = max(rf$Time)

# import compustat data 
cstat_request = paste("select gvkey, datadate, atq 
                  from comp.fundq
                  where indfmt='INDL'
                  and datafmt='STD'
                  and popsrc='D'
                  and consol='C'
                  and datadate between '", sdate, "' and '",edate, "'" ,sep="")
cstat_query = dbSendQuery(wrds,cstat_request)
comp = dbFetch(cstat_query) %>% rename(GVKEY = gvkey, Time = datadate)

day(comp$Time) = 1

# merge in assets
rf.weighted = rf %>%
  inner_join(comp, by = c('Time','GVKEY')) %>% 
  na.omit() %>%
  group_by(Time) %>% 
  mutate(preds = preds*(atq/sum(atq))) %>% 
  ungroup() %>% 
  arrange(GVKEY,Time) 

#------------------------------------------------
# Create the ML specific indexes
#------------------------------------------------
# function to create and prep data 
prepIndex <- function(pred_list){
  
  unweighted = na.omit(pred_list)  %>% 
    rename(date = Time) %>% 
    arrange(date)
  
  Index = unweighted %>% 
    group_by(date) %>% 
      summarize(mean = mean(preds, na.rm = T),
                sd = sd(preds, na.rm = T),
                skew = skewness(preds, na.rm = T),
                kurtosis = kurtosis(preds, na.rm = T)) %>% 
      arrange(date) %>% 
    ungroup() %>%
    mutate(mean_s = SMA(mean, n = 4),
           sd_s = SMA(sd, n = 4),
           skew_s = SMA(skew, n = 4),
           kurtosis_s = SMA(kurtosis, n = 4))
  
  return(Index)
}

# create the unweighted indexes 
Index.rf = prepIndex(rf) %>% filter(date >= as.Date('1989-01-01')) 

# create the weighted indexes 
Index.rf.weighted = prepIndex(rf.weighted) %>% filter(date >= as.Date('1989-01-01')) 


#------------------------------------------------
# Create the aggregate indexes
#------------------------------------------------
QoQ = function(X){return((X-lag(X,4))/lag(X))}
standardize = function(X){return((X-mean(X,na.rm = T))/sd(X,na.rm = T))}

# weighted mean index
Index.mean = select(Index.rf.weighted, date, NFCH_m = mean_s) %>%
  mutate(NFCH_m = standardize(NFCH_m))

# unweighted standard deviation and skew index
Index.tails =  Index.rf %>%
  mutate(sd = (QoQ(sd_s)),
         skew = (QoQ(skew_s))) %>%
  mutate(NFCH_s = 0.5*(sd + skew),
         NFCH_s = standardize(NFCH_s)) %>%
  select(date, NFCH_s, sd_smooth = sd_s, skew_smooth = skew_s)

# combine 
Index = full_join(Index.mean, Index.tails, by = 'date') %>%
  mutate(NFCH = 0.5*NFCH_m + 0.5*NFCH_s) %>%
  select(date, contains('NFCH'), contains('smooth'))

#------------------------------------------------
# Save the indexes
#------------------------------------------------
write.csv(Index, './Data/Ouput/indexes_aggregate.csv') 


