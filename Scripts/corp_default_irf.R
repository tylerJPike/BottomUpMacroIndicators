# File: corp_default_irf.R
# Author: Tyler Pike
# Date: 
# Note(s): Calculate the in- and out-of-sample AUC for each ML method's forecast

# clear enviroment
rm(list=ls())

# load libraries
library(dplyr)
library(stfm.helper, lib.loc="/stfm/shared1/R")
library(lubridate)

#-------------------------------------------------
#  Function to produce IRF
#-------------------------------------------------
jordaIRF <- function(data, shock, target, lags){
  
  # first create the proper table and variable names
  data = data %>% rename(target = target)
  data = as.data.frame(data)
  data = na.omit(data)
 
  ####################################################
  # generate the lags and leads
  ####################################################
  
  date = data$date
  data = data %>% select(-date)
  final = data 
  # generate lags
  for(i in 1:lags){
    temp = as.data.frame(lapply(data, MARGIN =  2, FUN = lag, n=i))
    colnames(temp) = paste0(colnames(temp),'.lag',i)
    final = cbind(temp,final)
  }
  # generate leads
  # not the most efficient but it works nonetheless 
  temp = matrix(ncol = lags, nrow = length(data$target))
  for(i in 1:lags){ 
    temp[,i] = lead(data$target, n = i)
  }
  colnames(temp) = paste0('target.l',c(1:lags))
  leads = colnames(temp) #used later
  final = cbind(temp,final)
  final$date = date
  data = final

  
  ####################################################
  # perform local impulse response regressions
  ####################################################
  
  # strip out the non-expanetory variables
  explanetory = data %>% select(-date, -leads)
  
  # strip out target variables 
  targets = data %>% select(leads)
  
  # generate the coef and sd for plotting
  # create storage matrix 
  irfData = matrix(ncol = 3, nrow = lags)
  colnames(irfData) = c('Horizon','Coef','Std.dev')
  irfData[,1] = c(1:lags) 
  # calculate regressions 
  for(i in 1:lags){
    # generate the projections 
    directProjection = lm(targets[,i] ~., data = explanetory)
    out = lmtest::coeftest(directProjection,vcov = sandwich::NeweyWest(directProjection,prewhite=FALSE))
    shockIndex = match(shock,rownames(out))
    # store the data
    irfData[i,2] <- out[shockIndex,1]
    irfData[i,3] <- out[shockIndex,2]
  }
  
  ####################################################
  # finalize data and return 
  ####################################################
  irfData = as.data.frame(irfData)
  # generate and store upper/lower bound 
  irfData$lowerBound <- irfData$Coef - 1.64*irfData$Std
  irfData$upperBound <- irfData$Coef + 1.64*irfData$Std
  return(irfData) 

}


#-------------------------------------------------
#  Function to plot IRF
#-------------------------------------------------
plotJordaIRF = function(plotData, ylim, ystep, title, ylab){
  # create the functions and set options for creating the plots
  opt = list(frame.lwd = 1.5,
             line.lwd  = c(1.7, 2),
             label.cex = .75,
             axis.cex  = .65,
             exhibit.title.cex = 1,
             chart.title.line = 0.8,
             chart.title.cex = 0.8,
             foot.cex  = .55,
             legend.cex = .7,
             line.types = rep(1,20),
             legend.inset = .03,
             yaxis.line = 0,
             tick.length = 0.025,
             yaxis.pos= .5,
             colors = c("black","firebrick","SteelBlue", "DarkOliveGreen3", "goldenrod1", "blueviolet","magenta"),
             key_adj_x=1,
             key.cex=0.75,
             paneltitleline=1.2,
             paneltitlecex=.85,
             tealbook.months = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."),
             tealbook.quarters = c("Q1","Q2","Q3","Q4"))



  forecasts <- function(dbpath=NULL,
                        keynames="",
                        chart_title="",
                        units="",
                        ymin,
                        ymax,
                        ystep,
                        xtickfreq,
                        frequency="",
                        horizshift=0,
                        vertshift=0,
                        note="",
                        footnote.placement=1,
                        key="topleft",
                        start.date=min(plot.data$date),
                        end.date=max(plot.data$date),
                        show.recent.months=FALSE,
                        n.months=0,
                        mult.conv.opt=1,
                        legcol=1,
                        lineatzero=FALSE,
                        dbtype="fame",
                        dataframe=NULL,
                        extra.xlim = 0,
                        colors=opt$colors,
                        interp=TRUE,
                        xmajortickfreq="year",
                        xminortickfreq="month",
                        xhighfreq="month",
                        xlowfreq="year",
                        hadj=opt$yaxis.pos,
                        lty1=opt$line.types[1],
                        lty2=opt$line.types[1],
                        two.printlastvalue=FALSE,
                        horizshift2=0,
                        vertshift2=0,
                        rmlastlabel=FALSE,
                        lowdateplacement=seq(place1, place2, by = xlowfreq),
                        xmin=ymin,
                        xmax=ymax,
                        xstep=ystep,
                        yaxis.shock.label="") {
    
    xmin = 0
    xmax = nrow(plotData) 
    xstep = 1
    

    plot(plotData$Horizon,  plotData$Coef,
         type = 'l',
         xlab = "",
         ylab = "",
         ylim =  ylim,#c(ymin,ymax),
         axes = FALSE,
         col  = "firebrick",
         #lty  = lty1,
         #lwd  = 1.7,
         main = "",
         bty  = 'u',
         yaxs = "i",
         xaxs = "i",
         xlim = c(.8,(nrow(plotData) + .2)),
         pch  = 16       #Consider 16, 19, 20
         #cex=1.3
    )


    polygon(c(plotData$Horizon,rev(plotData$Horizon)),c(plotData$upperBound,rev(plotData$lowerBound)),col="lightblue",border=NA)

    par(new=TRUE)
    plot(plotData$Horizon,  plotData$Coef,
         type = 'l',
         xlab = "",
         ylab = "",
         ylim =  ylim,#c(ymin,ymax),
         axes = FALSE,
         col  = "firebrick",
         #lty  = lty1,
         #lwd  = 1.7,
         main = "",
         bty  = 'u',
         yaxs = "i",
         xaxs = "i",
         xlim = c(.8,(nrow(plotData) + .2)),
         pch  = 16       #Consider 16, 19, 20
         #cex=1.3
    )
    

    set_chart_parameters()
    plotHookBox(lwd=opt$frame.lwd)

    #y axis

    axis(side = 4,
         at = seq(ymin,ymax, by = ystep),
         tck = opt$tick.length,
         cex.axis = opt$axis.cex,
         las = 2,
         hadj=hadj)

    axis(side = 2,
         at = seq(ymin,ymax, by = ystep),
         tck = opt$tick.length,
         cex.axis = opt$axis.cex,
         las = 2,
         labels=FALSE)


    axis(side = 1, at = seq(xmin,xmax,by = xstep),
         tck = opt$tick.length+.015,
         cex.axis = opt$axis.cex,
         las = 0,
         labels=FALSE,hadj=hadj)


    axis(side = 1, at = seq(xmin,xmax,by = xstep),
         tick=FALSE,
         cex.axis = opt$axis.cex,
         las = 0,
         labels=TRUE,
         hadj=hadj,
         line = -1)

    #labels
    mtext(units, side = 3, line =0 , adj = 1, outer = FALSE, cex = opt$legend.cex)
    mtext(chart_title, side = 3, line =0 , adj = 0, outer = FALSE, cex = paneltitle.cex)
    mtext(frequency, side = 3, line =-.5 , adj = .08, outer = FALSE, cex = opt$legend.cex)
    mtext(note, side = 1, line = footnote.placement, adj = 0, outer = FALSE, cex = opt$legend.cex)
    mtext(yaxis.shock.label, side = 2, line = .5, adj = .5,  outer = FALSE, cex = .7)

    abline(h=0,lwd = .5)

  }

  options(digits = 4)

  #print the plot
  forecasts(ymin= ylim[1],
            ymax= ylim[2],
            ystep= ystep,
            xmin=1,
            xmax=nrow(plotData),
            xstep=1,
            colors=c("firebrick","firebrick","firebrick"),
            legcol=1,
            keynames=c(""),
            hadj=.5,
            #note="Note: IRFs are calculated with zero short-run restrictions (cholesky) and represent the response to a one standard deviation \nshock to the Alternative FCI. The blue bands represent the 95 percent confidence interval around the point estimate in red.",
            footnote.placement=2.5,
            units = ylab,
            chart_title = title
            #yaxis.shock.label="Percentage Points"
  )

  mtext('Horizon (Quarters)',side = 1, line = .9,  adj=.5, cex = .75)
  
}

#------------------------------------------------------
# Data creation and preperation
#------------------------------------------------------
# the data menu collected will include: 
#    nfcd, 3-month treasury, 10-year treasury, Term spread, pce, 
#    realized inflation, IP, unemployment, real gdp, gz credit spread
            
#load in our index 
nfcd = read.csv('./Data/Ouput/indexes_aggregate.csv', stringsAsFactors = F) %>% 
  select(date, nfcd = NFCH) %>% 
  mutate(date = lubridate::ymd(date))


#load treasury data 
#handle the two series seperatly to get all quarters 
treasuryTickers = c('sveny0025', #3-month svensson treasury yield 
                    'sveny1000') #1- year svensson treasury yield 
treasuryData = getfame_dt(treasuryTickers,"yields")
treasuryData.3month = getfame_dt('sveny0025',"yields") %>% 
                                group_by(year = year(date), quarter = quarter(date)) %>% 
                                filter(date == max(date)) %>% 
                                ungroup() 

treasuryData.10year = getfame_dt('sveny1000',"yields") %>% 
                                group_by(year = year(date), quarter = quarter(date)) %>% 
                                filter(date == max(date)) %>% 
                                ungroup() 

day(treasuryData.3month$date) <- 1
day(treasuryData.10year$date) <- 1

treasuryData = merge(treasuryData.3month,treasuryData.10year, by = 'date')
treasurySpread = treasuryData %>% mutate(TS = sveny0025 - sveny1000) %>% select(TS,date)

#load in core pce for inflation
pce = as.data.frame(getfame_dt("ph.ce@maafe.q","us")) %>% rename(pce = 'ph.ce@maafe.q') %>% mutate(realizedInflation = log(lag(pce) /lag(pce,n=5))*100)
day(pce$date) <- 1

#load effective fed funds data
ff = as.data.frame(getfame_dt("rifspff_n.b","us")) %>% 
     group_by(year = year(date), quarter = quarter(date)) %>% 
     summarise(fedfunds = mean(rifspff_n.b, na.rm = T)) %>% 
     mutate(date = lubridate::ymd(paste0(year,'-',(quarter*3),'-01'))) %>% 
     ungroup() %>% select(date,fedfunds)

#create real effective federal funds rate 
#real federal funds rate is defined as the average effective federal funds rate less realized inflation is given by the log difference between core PCE in period t-1 and its value lagged a year earlier
rff = merge(pce,ff, by = 'date') %>% mutate(rff = fedfunds - realizedInflation) %>% select(date,rff)

#load gz spread data, transform dates and average over quarter
gz = read.csv("/mfa/prod1/ebp/ebp_public.csv", stringsAsFactors = T) %>% 
                                        mutate(date = dmy(paste0('01',date))) %>% 
                                        group_by(year = year(date), quarter = quarter(date)) %>% 
                                        summarise(gz = mean(gz_spread, na.rm = T)) %>% 
                                        mutate(date = lubridate::ymd(paste0(year,'-',(quarter*3),'-1'))) %>% 
                                        ungroup() %>% select(date,gz)

#load in Inudstrial Production 
IP = getfame_dt("JQI.M","us") %>% group_by(year = year(date), quarter = quarter(date)) %>% 
  summarise(IP = mean(JQI.M, na.rm = T)) %>% 
  mutate(date = lubridate::ymd(paste0(year,'-',(quarter*3),'-1'))) %>% 
  ungroup() %>% select(date,IP)

#load in unemployment and gdp 
usTickers = c('gdp_xcw_09.q','ruc.q')
econActivity = getfame_dt(usTickers,"us") %>% rename(rgdp = gdp_xcw_09.q, unemp = ruc.q)
day(econActivity$date) <- 1

#load nonfarm payroll employment (not used for now)
payroll = getfame_dt("ee.q","us")
names(payroll)[2] = "payroll"

#-----------------------------------------------------
# Determine chosen inputs 
#-----------------------------------------------------
# for now use, real federal funds, TS, IP. real gdp, and unemployment 
Data = inner_join(nfcd, rff, by = 'date') %>% 
        #inner_join(treasurySpread, by = 'date') %>% 
        inner_join(IP, by = 'date') %>% 
        inner_join(econActivity, by = 'date')

#standardize each variable so the interpretation is easy
standarize = function(X){return((X-mean(X,na.rm = T))/sd(X, na.rm = T))}
DLV = function(X){return(X-lag(X))}
Data = Data %>% mutate(IP = DLV(IP), rgdp = DLV(rgdp))
Data[,2:ncol(Data)] = apply(Data[,2:ncol(Data)] , MARGIN = 2, FUN = standarize) 

#-----------------------------------------------------
#Economic Activity repsonses to NFCD shock
#-----------------------------------------------------
#monetary policy response 
impulseResponse.a = jordaIRF(data = Data,
                           shock = 'nfcd',
                           target = 'rff',
                           lags = 8)


#GDP response 
impulseResponse.b = jordaIRF(data = Data,
                           shock = 'nfcd',
                           target = 'rgdp',
                           lags = 8)


#Unemployment response 
impulseResponse.c = jordaIRF(data = Data,
                           shock = 'nfcd',
                           target = 'unemp',
                           lags = 8)


#IP response 
impulseResponse.d = jordaIRF(data = Data,
                           shock = 'nfcd',
                           target = 'IP',
                           lags = 8)

#create pdf 
margin = c(2,2,2,2)
pdf('./Evaluation/JordaIRF/IRF_responses_to_nfcd_shocks.pdf')
par(fig=c(0,1,3/4,1),mar=margin)
plotJordaIRF(impulseResponse.a,
             ylim = c(-2,1),
             ystep = 1,
             title = 'Monetary Policy Response to NFCH Shock',
             ylab = 'Standard Deviations')

par(fig=c(0,1,2/4,3/4),mar=margin,new=T)
plotJordaIRF(impulseResponse.b,
             ylim = c(-1,1),
             ystep = .5,
             title = 'Real GDP Growth Response to NFCH Shock',
             ylab = 'Standard Deviations')

par(fig=c(0,1,1/4,2/4),mar=margin,new=T)
plotJordaIRF(impulseResponse.d,
             ylim = c(-2,1),
             ystep = 1,
             title = 'IP Response to NFCH Shock',
             ylab = 'Standard Deviations')
 
par(fig=c(0,1,0,1/4),mar=margin,new=T)
plotJordaIRF(impulseResponse.c,
             ylim = c(-.5,1.5),
             ystep = .5,
             title = 'Unemployment Response to NFCH Shock',
             ylab = 'Standard Deviations')
dev.off()


# save data 
export = 
  full_join(impulseResponse.a, impulseResponse.b, by = 'Horizon') %>%
  full_join(impulseResponse.c,  by = 'Horizon') %>%
  full_join(impulseResponse.d, by = 'Horizon')
write.csv(export, './Evaluation/JordaIRF./irf_nfchShocks_results.csv', row.names = F)



#-----------------------------------------------------
# NFCD epsonse to Economic Activity shock
#-----------------------------------------------------
#monetary policy response 
impulseResponse.a = jordaIRF(data = Data,
                           shock = 'rff',
                           target = 'nfcd',
                           lags = 8)

#GDP response 
impulseResponse.b = jordaIRF(data = Data,
                           shock = 'rgdp',
                           target = 'nfcd',
                           lags = 8)


#Unemployment response 
impulseResponse.c = jordaIRF(data = Data,
                           shock = 'unemp',
                           target = 'nfcd',
                           lags = 8)

#IP response 
impulseResponse.d = jordaIRF(data = Data,
                           shock = 'IP',
                           target = 'nfcd',
                           lags = 8)


#create pdf 
margin = c(2,2,2,2)
pdf('./Evaluation/JordaIRF/IRF_responses_to_macro_shocks.pdf')
par(fig=c(0,1,3/4,1),mar=margin)
plotJordaIRF(impulseResponse.a,
             ylim = c(-2,2),
             ystep = 1,
             title = 'NFCH Response to Monetary Policy Shock',
             ylab = 'Standard Deviations')

par(fig=c(0,1,2/4,3/4),mar=margin,new=T)
plotJordaIRF(impulseResponse.b,
             ylim = c(-1,1),
             ystep = .5,
             title = 'NFCH Response to Real GDP Growth Shock',
             ylab = 'Standard Deviations')

par(fig=c(0,1,1/4,2/4),mar=margin,new=T)
plotJordaIRF(impulseResponse.d,
             ylim = c(-1,1),
             ystep = .5,
             title = 'NFCH Response to IP Shock',
             ylab = 'Standard Deviations')

par(fig=c(0,1,0,1/4),mar=margin,new=T)
plotJordaIRF(impulseResponse.c,
             ylim = c(-6,2),
             ystep = 2,
             title = 'NFCH Response to Unemployment Shock',
             ylab = 'Standard Deviations')
dev.off()


export = 
  full_join(impulseResponse.a, impulseResponse.b, by = 'Horizon') %>%
  full_join(impulseResponse.c,  by = 'Horizon') %>%
  full_join(impulseResponse.d, by = 'Horizon')

write.csv(export, './Evaluation/irf_macroShocks_results.csv', row.names = F)


