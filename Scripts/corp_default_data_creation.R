# File: corp_default_data_creation.R
# Author: Tyler Pike, Clay Wagar
# Date: 10/9/2019
# Note(s): Create series of bankruptcies and design matrix of explanetory variables. 
#          Bankruptcies may be sourced from NYU, UCLA, CM (FRB), Mergent (FRB), or FISD (FRB) databases.
#          Explanetory variables are created at the firm-level with CRSP and Compustat data.

# THIS SCRIPT WILL NOT WORK UNLESS THE USER HAS ACCESS TO THE FRB NETWORK
# THE USER WILL NEED TO SUPPLY THIER OWN WRDS CREDENTIALS

#housekeeping and other pre-trial matters 
rm(list = ls())

# libraries
library(dplyr)
library(lubridate)
library(stringr)
library(RPostgreSQL)                   # connect to sql
library(RPostgres)                     # connect to sql 
library(odbc)                          # connect to sql

# establish connection to WRDS
wrds = dbConnect(Postgres(),
             #    host= user specified host,
             #    port= user specified port,
             #    user= username,
             #    password= password,
                 sslmode='require',
                 dbname='wrds')

#---------------------------------------------------------------------------------------#
# Data cleaning functions 
#---------------------------------------------------------------------------------------#
# Winsorize/Trim
f.winsor = function(data, trim = FALSE, cut.low = .01, cut.high = .99){
  f.vectorwin<- function(x){
    x.low = quantile(x,na.rm=TRUE,probs=cut.low)
    x.high = quantile(x,na.rm=TRUE,probs=cut.high)
    if (trim){
      print("Trimming the data")
      x[x<x.low]<-NA
      x[x>x.high]<-NA}
    else if (!trim){
      print("Winsorizing the data")
      x[x<x.low]=x.low
      x[x>x.high]=x.high}
    return(x)
  }
  if(is.vector(data)){ data <- f.vectorwin(data)}
  if(!is.vector(data)){
    numvars <- names(data)[sapply(data, is.numeric)]
    data[,numvars] <- sapply(data[,numvars],f.vectorwin)
  }
  return(data)}

#---------------------------------------------------------------------------------------#
# Data creation function
#---------------------------------------------------------------------------------------#
# sourcing options are 
# 1. capital markets + NYU and UCLA data:  source = 'cm'
# 2. mergent defautls : source = 'mergent'
  
createData <- function(sdate, edate, source){

  #---------------------------------------------------------------------------------------#
  # Create explanetory variables 
  #---------------------------------------------------------------------------------------#
  # Load and filter data and clean variables
  sdate_e = sdate
  year(sdate_e) <- year(sdate_e) - 3
  #import compustat data 
  cstat_request = paste("select gvkey, datadate, fic, aqcy, capxy, dltisy, dvy, prstkcy, sstky,
                  dpq, dvpq, mibq, pstkq, req, txdbq, txditcq, txtq, xintq, ceqq, niq, 
                  cshoq, prccq, ibq, saleq, atq, dlcq, dlttq,  cheq, actq, lctq, ltq, 
                  fqtr,  seqq, cusip
                      from comp.fundq
                      where indfmt='INDL'
                      and datafmt='STD'
                      and popsrc='D'
                      and consol='C'
                      and datadate between '", sdate, "' and '",edate, "'" ,sep="")
  cstat_query = dbSendQuery(wrds,cstat_request)
  comp = dbFetch(cstat_query) 

  cstat_request = paste("select gvkey, sic, GSECTOR
                      from comp.company")
  cstat_query = dbSendQuery(wrds,cstat_request)
  comp_extra = dbFetch(cstat_query) 
  comp = comp %>% merge(comp_extra, all.x = T, by = c('gvkey')) %>% rename(GVKEY = gvkey) %>% 
    mutate(cusip = substr(cusip,1,6))



  comp_q <- comp %>%
    mutate(
      datadate = ymd(datadate),
      quarter = quarter(datadate, with_year = TRUE),
      sic = as.numeric(sic)) %>% 
    arrange(GVKEY, quarter, datadate) %>%
    filter(
      # Drop financials
      !(sic >= 6000 & sic < 7000),
      # Drop regulated utilities
      !(sic >= 4900 & sic < 4950),
      # Only US Companies
      fic == "USA") %>%
    # Format YTD variables as flows
    # - Check if fiscal quarter fqtr == 1
    # - If not, check if lag is available. If yes, then difference the data
    group_by(GVKEY) %>%
    mutate(
      fqtr = if_else(datadate == min(datadate), 1, 0),
      delta_days = difftime(datadate, lag(datadate), units = "days"),
      lag_available = as.numeric(round(delta_days/90)) == 1 & !is.na(delta_days)) %>%
    mutate_at(c('aqcy', 'capxy', 'dltisy', 'dvy', 'prstkcy', 'sstky'),
              funs(ifelse(fqtr == 1, ., ifelse(lag_available, . - lag(.), NA)))) %>%
    select(-delta_days, -lag_available) %>%
    rename(aqcq = aqcy, 
          capxq = capxy, 
          dltisq = dltisy, 
          dvq = dvy, 
          prstkcq = prstkcy, 
          sstkq = sstky)


  # Expand panel s.t. there are no missing quarters for each firm in between observations
  comp_q <- comp_q %>% 
    # Get data frame with gvkey, first, and last year
    select(GVKEY, quarter) %>% 
    group_by(GVKEY) %>%
    summarise(start_year = floor(min(quarter, na.rm = TRUE)), 
              end_year = floor(max(quarter, na.rm = TRUE))) %>%
    # Create a balanced sequence of years for each gvkey
    group_by(GVKEY) %>%
    do(data_frame("GVKEY" = .$GVKEY[1], "year" = seq(from = .$start_year, to = .$end_year))) %>%
    # Expand to quarters
    do(data_frame("GVKEY" = rep(.$GVKEY, each = 4), 
                  "quarter" = paste(rep(.$year, each = 4), c("1", "2", "3", "4"), sep = "."))) %>%
    mutate(quarter = as.numeric(quarter)) %>%
    # Populate firm_time panel with data from comp_q 
    left_join(comp_q)

  # Generate Composite variables
  comp_q <- comp_q %>%
    mutate_each(funs = funs(ifelse(is.na(.), 0, .)), dpq, dvpq, mibq, pstkq, 
                req, txdbq, txditcq, txtq, xintq, aqcq, capxq, dltisq, 
                dvq, prstkcq, sstkq) %>%
    mutate(seqq     = ifelse(is.na(seqq), ceqq, seqq)) %>%
    group_by(GVKEY) %>%
    mutate(
      EBIT = niq + xintq + txtq,
      DIVIDENDS = dvq + dvpq,
      MKTVALUE = cshoq*prccq,
      CASHFLOW = ibq + dpq,
      PAYOUT = (dvpq + dvq + prstkcq)/ibq,
      SALESGR = (saleq - lag(saleq))/lag(saleq),
      ASSETGR = (atq-lag(atq))/atq,
      TOTDEBT = dlcq+dlttq,
      SIZE = log(atq)) %>%
    mutate_at(c('EBIT', 'DIVIDENDS', 'MKTVALUE', 'CASHFLOW', 
                  'PAYOUT', 'SALESGR', 'ASSETGR', 'TOTDEBT', 'SIZE'), 
              funs(ifelse(is.finite(.), . , NA)))
  # Generate SIC3 and SIC4 codes
  comp_q <- comp_q %>%
    mutate(sic4 = as.character(sic),
          sic4 = ifelse(str_length(sic4) == 3, paste0("0", sic4), sic4),
          sic3 = str_sub(sic4, start = 1, end = 3))

  # Industry Sales Growth
  indsales <- comp_q %>%
    group_by(sic3, quarter) %>%
    summarise(INDSALES = sum(saleq, na.rm = TRUE),
              NO_FIRMS = sum(!is.na(saleq)),
              INDSALES = ifelse(NO_FIRMS == 0, NA, INDSALES)) %>%
    group_by(sic3) %>%
    mutate(INDSALESGR = (INDSALES-lag(INDSALES))/lag(INDSALES),
          INDSALESGR = ifelse(is.finite(INDSALESGR), INDSALESGR, NA))

  comp_q <- left_join(comp_q, indsales)
  rm(indsales)

  # Firm Age
  comp_q <- comp_q %>% 
    group_by(GVKEY) %>% 
    mutate(AGE = difftime(datadate, min(datadate, na.rm = T), units = "days"),
          AGE = round(as.numeric(AGE)/365))

  # Financial Constraint Indices from the Literature
  comp_q %<>%
    mutate(
      # Tobin's Q 
      # - with long-term liabilities and equity
      # - Try not to lose too many obs
      Q = (atq + cshoq*prccq - ceqq - txdbq)/atq,
      # Dividend Dummy
      DIVDUMMY = as.numeric(DIVIDENDS > 0),
      # KZ Index
      KZ = -1.002*CASHFLOW/atq + 0.283*Q+3.319*TOTDEBT/(TOTDEBT + seqq)-39.368*DIVIDENDS/atq-1.315*cheq/atq,
      # HP Index
      HP = 0.737*log(pmin(atq,4500)) + 0.043*log(pmin(atq,4500))^2-0.040*pmin(AGE,37),
      # Whited-Wu
      WW = -0.091*CASHFLOW/atq + 0.062*(DIVDUMMY) + 0.021*dlttq/atq,
      # Altman's Z-Score
      Z = 1.2*(actq - lctq)/atq + 1.4*req/atq + 3.3*(niq + xintq + txtq)/atq + 0.6*(cshoq*prccq)/ltq + 0.999*saleq/atq) %>%
    # Replace Non-Finite Values
    mutate_at(c('Q', 'DIVDUMMY', 'KZ', 'HP', 'WW', 'Z'), funs(ifelse(is.finite(.), . , NA)))

  # CHS variables
  phi <- 2^(-1/3)  # Parameter in CHS

  comp_q <- comp_q %>%
    mutate(
      # Market equity = price * shares outstanding
      me = abs(prccq)*cshoq,
      # Book Equity = stockholders' equity + deferred taxes and investment tax credit - Preferred Stock
      be = seqq + txditcq - pstkq,
      be = ifelse(be <=0, 1, be),
      be = be + .1*(me - be),
      # Adjusted total assets = Total assets + .1(me - be)
      TAadj = atq + .1*(me - be),
      # Total liabilities
      mibq = ifelse(is.na(mibq), 0, mibq),
      TotLiab = ltq + mibq,
      # Construct regressors
      NITA = niq/TAadj,
      TLTA = TotLiab/TAadj,
      NIMTA = niq/(me + TotLiab),
      TLMTA = TotLiab/(me+TotLiab),
      CASHMTA = cheq/(me + TotLiab),
      PRICE = log(pmin(abs(prccq), 15)),
      MB = me/be) %>%
    # Construct average NIMTA
    group_by(GVKEY) %>%
    arrange(GVKEY, datadate) %>%
    mutate(NIMTAAVG = ((1-phi^3)/(1-phi^12))*(lag(NIMTA,1) + phi^3*lag(NIMTA,2) + phi^6*lag(NIMTA, 3) + phi^9*lag(NIMTA, 4)))  # NOTE: This works because I balanced the panel above.

  # Add relative size
  temp <- comp_q %>%
    group_by(quarter) %>%
    filter(rank(me) <= 500) %>%
    summarise(TotCap = sum(me, na.rm = T))

  comp_q <- comp_q %>%
    left_join(temp) %>%
    mutate(
      RSIZE = log(me/TotCap)) %>%
    select(-TotCap)

  # Generate some more predictors based on JBF paper
  comp_q <- comp_q %>%
    mutate(WCTA =  (actq - lctq)/actq,
          CURRENTRATIO = actq/lctq,
          EBITMARGIN = EBIT/saleq,  # too many missings here, drop for now
          ASSETTURNOVER = saleq,
          ROE = niq/ceqq,
          EBITCOVER = ifelse(xintq == 0, EBIT/.01, EBIT/xintq),
          CAPEXTA = capxq/atq
          )

  # Select predictors 
  predictors <- comp_q %>%
    select(GVKEY, datadate, quarter, INDSALES, INDSALESGR, AGE, NITA, TLTA, NIMTA, 
          TLMTA, CASHMTA, PRICE, MB, NIMTAAVG,
          Q, DIVDUMMY, KZ, HP, WW, Z,
          WCTA, CURRENTRATIO, ASSETTURNOVER, ROE, 
          EBITCOVER, CAPEXTA, gsector, cusip) %>%
    ungroup() %>%
    group_by(quarter) %>%
    mutate_at(c('NITA', 'TLTA', 'NIMTA', 'INDSALES', 'INDSALESGR',
                'TLMTA', 'CASHMTA', 'PRICE', 'MB', 'NIMTAAVG',
                'Q', 'KZ', 'HP', 'WW', 'Z', 'WCTA', 'CURRENTRATIO', 'ASSETTURNOVER', 'ROE', 
                'EBITCOVER', 'CAPEXTA'),
              funs(f.winsor(., trim = T))) %>%
    ungroup() %>%
    # Drop duplicates 
    distinct(GVKEY, quarter, .keep_all = TRUE) %>%
    # Lag by one quarter to make sure data are available
    arrange(GVKEY, quarter) %>%
    group_by(GVKEY) %>%
    mutate_at(c('INDSALES', 'INDSALESGR', 'AGE', 'NITA', 'TLTA', 'NIMTA', 
                'TLMTA', 'CASHMTA', 'PRICE', 'MB', 'NIMTAAVG',
                'Q', 'DIVDUMMY', 'KZ', 'HP', 'WW', 'Z',
                'WCTA', 'CURRENTRATIO', 'ASSETTURNOVER', 'ROE', 
                'EBITCOVER', 'CAPEXTA', 'datadate', 'gsector'),
                funs(lag(.))) %>%
    ungroup()


  ### Prepare stock market variables ----
  #import stock market rerturn variable 
  crsp_request = paste("select sprtrn, caldt
                        from crspm.dsp500
                        where caldt between '", sdate,"' and '",edate,"'",sep="")

  crsp_query = dbSendQuery(wrds,crsp_request)
  crsp_m = dbFetch(crsp_query)
  sp500_q = crsp_m %>% 
    rename(date = caldt) %>%        
    group_by( date) %>%
    summarize(sprtrn = 1-(prod(1+sprtrn))) %>%
    mutate(quarter = quarter(date, with_year = TRUE))  

  #import company specific returns 
  crsp_request = paste("select permco, date, ret
                      from crspm.msf
                      where date between '", sdate,"' and '",edate,"'",sep="")
  crsp_query = dbSendQuery(wrds,crsp_request)
  crsp_m = dbFetch(crsp_query)

  crsp_m = crsp_m %>% group_by(date, permco) %>%
    summarize(trt1m = 1-(prod(1+ret))) %>%
    mutate(quarter = quarter(date, with_year = TRUE))

  crsp_m = inner_join(crsp_m, sp500_q)

  # Construct excess return variables
  crsp_m <- crsp_m %>%
    mutate(EXRET = log(1+trt1m/100) - log(1+sprtrn)) %>%
    # Construct average EXRET
    group_by(permco) %>%
    arrange(permco, date) %>%
    mutate(EXRETAVG = ((1-phi)/(1-phi^12))*(lag(EXRET,1) + phi^1*lag(EXRET,2) + phi^2*lag(EXRET, 3) + phi^4*lag(EXRET, 5)
                                            + phi^5*lag(EXRET, 6) + phi^6*lag(EXRET, 7) + phi^7*lag(EXRET, 8) + phi^8*lag(EXRET, 9)
                                            + phi^9*lag(EXRET, 10)+ phi^10*lag(EXRET, 11) + phi^11*lag(EXRET, 12) ))%>%  # NOTE: This does assume balanced data
    select(permco, quarter, date, EXRET, EXRETAVG) %>%
    ungroup() %>%
    group_by(quarter) %>%
    mutate_at(c('EXRET','EXRETAVG'), funs(f.winsor(., trim = T))) %>% rename(time_m = date) %>%
    ungroup()

  #import the crosswalk from permco to gvkey 
  ccm_query = dbSendQuery(wrds,"select gvkey, lpermco as permco 
                    from crsp.ccmxpf_linktable 
                    where substr(linktype,1,1)='L'
                    and (linkprim ='C' or linkprim='P')")
  ccm = dbFetch(ccm_query)

  #replace permco for gvkey 
  crsp_m = crsp_m %>% merge(ccm, by = 'permco') %>% select(-permco) %>% rename(GVKEY = gvkey)

  #create final list of predictors 
  predictors <- inner_join(crsp_m, predictors)

  #---------------------------------------------------------------------------------------#
  # Default Data 
  #---------------------------------------------------------------------------------------#

  # Capital Markets Data -- Default Data Post 1991----------------------------------------#

  if(source == 'cm'){
    # Create gvkey to company name cross walk
    cstat_request = paste0("select gvkey, conm 
                        from comp.fundq
                        where indfmt='INDL'
                        and datafmt='STD'
                        and popsrc='D'
                        and consol='C'
                        and datadate between '", sdate, "' and '",edate, "'")
    cstat_query = dbSendQuery(wrds,cstat_request)
    comp = dbFetch(cstat_query)
    comp = comp %>% rename(Name = conm)
    
    #import default data from R&S section CM 
    #(contact them for data if it needs to be updated)
    defaults = read.csv('cm_defaults.csv', stringsAsFactors = F)
    #fix up dates and other matters 
    defaults = defaults %>% mutate(Date = mdy(Date)) %>%
                select(-Amount, -SIC, -PID) %>%
                mutate(Name = toupper(Name),
                        Name = gsub("[[:punct:]]", "", Name)) 
    #merge gvkey into defaults 
    defaults = defaults %>% merge(comp, by = 'Name' ) %>% distinct() %>% arrange(Date) %>% 
                            rename(GVKEY = gvkey, date = Date) %>% mutate(defaultDate = date)

  } else if(source == 'mergent'){
    
  # Mergent Data -- Default Data Post 1984 ---------------------------------------------#
    
    bankruptcies = read.csv('./Data/Input/bankruptcy.csv', stringsAsFactors = F)  
    issuer_cusip = read.csv('./Data/Input/issuer_cusip.csv', stringsAsFactors = F)  
    defaults.a  = bankruptcies %>% left_join(issuer_cusip, by = 'Issuer_id') %>%
                                  mutate(defaultDate = mdy(Filing_date)) %>%
                                  select(cusip = Issuer_cusip, defaultDate)
    
  } else if(source == 'fisd'){

  # FISD Data -- Default Data Post 1984 ---------------------------------------------#

    #pull in permanent GVKEYs to match imperminant cusips 
    cstat_request = paste("select gvkey, cusip
                      from comp.fundq
                      where indfmt='INDL'
                      and datafmt='STD'
                      and popsrc='D'
                      and consol='C'
                      and datadate between '", sdate, "' and '",edate, "'" ,sep="")
    cstat_query = dbSendQuery(wrds,cstat_request)
    comp = dbFetch(cstat_query) %>% 
      distinct() %>% 
      mutate(cusip = substr(cusip,1,6))
    
    # merged issuer_cupsip.csv and fisd data 
    # yields 939 defaults since 1980
    rawData_fisd = 
      haven::read_sas('/cm/data2/fisd/data/bankruptcy.sas7bdat') %>% 
      mutate(Issuer_id = as.numeric(Issuer_id))

    rawData_link = 
      read.csv('./issuer_cusip.csv', stringsAsFactors = F)  %>% 
      mutate(Issuer_id = as.numeric(Issuer_id))
    
    defaults = rawData_fisd %>% 
      inner_join(rawData_link, by = 'Issuer_id') %>%
      select(Filing_date, Issuer_cusip) %>%
      mutate(month = quarter(Filing_date)*3, 
            year = year(Filing_date),
            defaultDate = ymd(paste0(year,'-',month,'-01'))) %>%
      filter(defaultDate >= as.Date(sdate)) %>%
      select(cusip = Issuer_cusip, defaultDate)
    
    defaults = inner_join(comp, defaults) %>% 
      rename(GVKEY = gvkey) %>% 
      select(-cusip) %>% 
      distinct()
    
  }

  #---------------------------------------------------------------------------------------#
  # Merge the predictor data with the default data 
  #---------------------------------------------------------------------------------------#

  # Merge defaults and predictors, store baseline dataset
  baseline <- left_join(predictors, defaults) %>%
    mutate(timeToDefault = round(quarter(defaultDate, with_year = T) - quarter, digits = 1)) %>%
    # Construct default variables
    mutate(y1 = timeToDefault == .1 | timeToDefault == .7,
          y2 = timeToDefault == .2 | timeToDefault == .8 | y1 == 1,
          y3 = timeToDefault == .3 | timeToDefault == .9 | y2 == 1,
          y4 = timeToDefault ==  1 | y3 == 1,
          y5 = timeToDefault == 1.1 | timeToDefault == 1.7 | y4 == 1,
          y6 = timeToDefault == 1.2 | timeToDefault == 1.8 | y5 == 1,
          y7 = timeToDefault == 1.3 | timeToDefault == 1.9 | y6 == 1,
          y8 = timeToDefault == 2 | y7 == 1) %>%
    mutate_each(funs = funs(ifelse(is.na(.),0,.)), y1, y2, y3, y4, y5, y6, y7, y8) %>%
    # Drop firms from database after default
    filter(timeToDefault >= 0 | is.na(timeToDefault)) %>%
    select(-timeToDefault, -defaultDate) %>% na.omit()

  # Scale and generate lags
  varsNum <- c("EXRET", "EXRETAVG", "INDSALES", "INDSALESGR", "NITA", "TLTA", "NIMTA", "TLMTA", "CASHMTA",
              "PRICE", "MB", "NIMTAAVG", "Q", "KZ", "HP", "WW", "Z", "WCTA", "CURRENTRATIO",
              "ASSETTURNOVER", "ROE", "EBITCOVER", "CAPEXTA")

  baselineScaled <- baseline %>%
    group_by(quarter) %>%
    mutate_at(varsNum, funs(as.numeric(scale(.)))) %>%
    ungroup()

  vars <- c("EXRET", "EXRETAVG", "INDSALES", "INDSALESGR", "AGE", "NITA", "TLTA", "NIMTA", "TLMTA", "CASHMTA",
            "PRICE", "MB", "NIMTAAVG", "Q", "DIVDUMMY", "KZ", "HP", "WW", "Z", "WCTA", "CURRENTRATIO",
            "ASSETTURNOVER", "ROE", "EBITCOVER", "CAPEXTA")
  vars1 <- setNames(vars, paste0("LAG1", vars))
  vars2 <- setNames(vars, paste0("LAG2", vars))

  baselineScaled <- baselineScaled %>%
    group_by(GVKEY) %>%
    arrange(GVKEY, quarter) %>%
    mutate_at(vars1, funs(lag(.))) %>%
    mutate_at(vars2, funs(lag(., n = 2))) %>%
    ungroup()

  ## Finishing touches
  baselineFinal <- baselineScaled %>%
    mutate(SEC10 = 1*(gsector == 10),
          SEC15 = 1*(gsector == 15),
          SEC20 = 1*(gsector == 20),
          SEC25 = 1*(gsector == 25),
          SEC30 = 1*(gsector == 30),
          SEC35 = 1*(gsector == 35),
          SEC40 = 1*(gsector == 40),
          SEC45 = 1*(gsector == 45),
          SEC50 = 1*(gsector == 50),
          SEC55 = 1*(gsector == 55),
          SEC60 = 1*(gsector == 60)) %>%
    select(y1, y2, y3, y4, y5, y6, y7, y8,
          starts_with("LAG"),
          starts_with("SEC"),
          one_of(vars),
          quarter, time_m, GVKEY)

  baselineFinaltest <- na.omit(baselineFinal)

  return(baselineFinal)
}

#---------------------------------------------------------------------------------------#
# Create data
#---------------------------------------------------------------------------------------#

# create data
Data = 
  createData(sdate = as.Date('1980-01-01'), 
                edate = Sys.Date(), 
                source = 'fisd') %>%
  filter(quarter >= 1984.1)

# save data
saveRDS(Data, file = './Data/Intermediate/corp_default_data.RDS')

