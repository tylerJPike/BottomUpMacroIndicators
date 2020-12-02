# File: corp_default_auc.R
# Author: Clay Wagar, Tyler Pike
# Date: 
# Note(s): Calculate the in- and out-of-sample AUC for each ML method's forecast

# clear enviroment
rm(list=ls())

library(pROC)
library(dplyr)
library(stfm.helper, lib.loc="/stfm/shared1/R")

inSample = 1
outOfSample = 1

#----------------------------------------------------------
# Import firm-level predictions and calculate AUC
#----------------------------------------------------------

AUC_inSample = function(){

     GBM.AUC   = c()
     Logit.AUC = c()
     RF.AUC    = c()
     ANN.AUC   = c()
     SVM.AUC   = c()
     modelNames = c("fitLogit","fitGBM",'fitRF','fitANN', 'fitSVM')

     for(i in 1:8){

     # Load estimates 
     dataIn = paste0("./Estimation/entire1_Estimates_y",i,".RData")
               
     load(dataIn)
     pred_list = list(fitRF    = fitRF$pred, 
                    fitGBM   = fitGBM$pred, 
                    fitLogit = fitLogit$pred, 
                    fitANN   = fitANN$pred, 
                    fitSVM   = fitSVM$pred) 
     
     # Evaluate AUC
     roc_list <- list()
     for (jj in modelNames) {
     roc_list[[jj]] <- roc(ordered(pred_list[[jj]]$obs), pred_list[[jj]]$yes)
     print(jj)
     }


     print(i)

     Logit.AUC = c(Logit.AUC,roc_list$fitLogit$auc)
     ANN.AUC   = c(ANN.AUC,  roc_list$fitANN$auc)
     SVM.AUC   = c(SVM.AUC,  roc_list$fitSVM$auc)
     RF.AUC    = c(RF.AUC,   roc_list$fitRF$auc)
     GBM.AUC   = c(GBM.AUC,  roc_list$fitGBM$auc)

     rm(pred_list)

     }


     data.is = 
          data.frame(
               Horizon = c(1:8), 
               Logit = Logit.AUC, 
               ANN = ANN.AUC,
               SVM = SVM.AUC, 
               RF = RF.AUC, 
               GBM = GBM.AUC)

     return(data.is)

}

AUC_outOfSample = function(){
  
  GBM.AUC   = c()
  Logit.AUC = c()
  RF.AUC    = c()
  ANN.AUC   = c()
  SVM.AUC   = c()
  modelNames = c("fitLogit","fitGBM",'fitRF','fitANN', 'fitSVM')

  for(i in 1:8){

    # load logit estimates 
    load(paste0('../Estimation/Recursive1_predictions_y',i,'.RData'))
    fitLogit = pred_list 
    
    # load estimates
    load(paste0('./Estimation/Out-of-Sample/RF-GBM-SVM-ANN-Logit (fisd)/Recursive1_predictions_h',i,'.Rdata'))
    
    pred_list$Logit = fitLogit$Logit
    
    #Evaluate AUC
    roc_list <- list()
    for (jj in modelNames) {
      roc_list[[jj]] <- roc(ordered(pred_list[[jj]]$y), pred_list[[jj]]$preds)
      print(jj)
    }
    
    print(i)
    
    Logit.AUC = c(Logit.AUC,roc_list$Logit$auc)
    RF.AUC    = c(RF.AUC,   roc_list$RF$auc)
    GBM.AUC   = c(GBM.AUC,  roc_list$GBM$auc)
    ANN.AUC   = c(ANN.AUC,  roc_list$ANN$auc)
    SVM.AUC   = c(SVM.AUC,  roc_list$SVM$auc)
    
    rm(pred_list)
    
  }
  
  
  data.os = 
     data.frame(
          Horizon = c(1:8), 
          Logit = Logit.AUC, 
          ANN = ANN.AUC, 
          SVM = SVM.AUC, 
          GBM = GBM.AUC,
          RF = RF.AUC) 

  return(data.os)
          
}

#----------------------------------------------------------
# Function to plot AUC
#----------------------------------------------------------

plotAUC_is = function(data, yaxes){
  
  xlab = ''
  ylab = ''
  
  plot(data$Horizon, data$Logit, ylim = yaxes[1:2], col = 'black', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 1, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$SVM, ylim = yaxes[1:2], col = 'goldenrod', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 2, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$ANN, ylim = yaxes[1:2], col = 'steelblue', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 3, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$GBM, ylim = yaxes[1:2], col = 'forestgreen', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 4, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$RF, ylim = yaxes[1:2], col = 'firebrick', type = 'l',
       xlab = xlab, ylab = ylab, axes = F,lty = 5, lwd = 2)
  
  
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
  
  
  set_chart_parameters()
  plotHookBox(lwd=opt$frame.lwd)
  
  hadj = opt$yaxis.pos
 
  ymin  = yaxes[1]
  ymax  = yaxes[2]
  ystep = yaxes[3]
  xmin  = min(data$Horizon)
  xmax  = max(data$Horizon)
  xstep = 1

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
       labels=FALSE,
       hadj= opt$yaxis.pos)


  axis(side = 1, at = seq(xmin,xmax,by = xstep),
       tick=FALSE,
       cex.axis = opt$axis.cex,
       las = 0,
       labels=TRUE,
       hadj= opt$yaxis.pos,
       line = -1)

  legend(1,.925,
         legend = c('Logit','SVM','ANN','GBM','RF'),
         col = c('black','goldenrod','steelblue','forestgreen','firebrick'),
         lty = c(1:5),
         lwd = 2,
         cex = 1.75,
         bty = 'n')
  
   
  
  mtext('Area Under the Reciever Curve', side = 2, line =1 , adj = .5, outer = FALSE, cex = 1)
  mtext('Forecast Horizon (Quarters)', side = 1, line = 1 , adj = .5, outer = FALSE, cex = 1)
  mtext('In-Sample', side = 3, line =0 , adj = 0, outer = FALSE, cex = 1)
  
}

plotAUC_os = function(data, yaxes){
  
  xlab = ''
  ylab = ''
  
  plot(data$Horizon, data$Logit, ylim = yaxes[1:2], col = 'black', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 1, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$SVM, ylim = yaxes[1:2], col = 'goldenrod', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 2, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$ANN, ylim = yaxes[1:2], col = 'steelblue', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 3, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$GBM, ylim = yaxes[1:2], col = 'forestgreen', type = 'l',
       xlab = xlab, ylab = ylab, axes = F, lty = 4, lwd = 2)
  par(new = T)
  plot(data$Horizon, data$RF, ylim = yaxes[1:2], col = 'firebrick', type = 'l',
       xlab = xlab, ylab = ylab, axes = F,lty = 5, lwd = 2)
  
  
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
  
  
  set_chart_parameters()
  plotHookBox(lwd=opt$frame.lwd)
  
  hadj = .5
  
  ymin  = yaxes[1]
  ymax  = yaxes[2]
  ystep = yaxes[3]
  xmin  = min(data$Horizon)
  xmax  = max(data$Horizon)
  xstep = 1
  
  axis(side = 4,
       at = seq(ymin,ymax, by = ystep),
       tck = opt$tick.length,
       cex.axis = opt$axis.cex,
       las = 2,
       hadj=hadj)
  
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

  mtext('Forecast Horizon (Quarters)', side = 1, line = 1 , adj = .5, outer = FALSE, cex = 1)
  mtext('Out-of-Sample', side = 3, line =0 , adj = 0, outer = FALSE, cex = 1)
  
  
}

  
writePDFGraph = function(){
  pdf('./Evaluation/AUC/auc_plot.pdf', height = 8.5, width = 11.5)
  par(fig = c(0,.5,0,1), mar = c(3,2,1,.5))
  plotAUC_is(data.is, yaxes = c(.88,1,.02))
  par(fig = c(.5,1,0,1), new = T,mar = c(3,.5,1,2))
  plotAUC_os(data.os, yaxes = c(.88,1,.02))
  dev.off()
}

#----------------------------------------------------------
# Calculate and plot AUCs
#----------------------------------------------------------
# calcualte AUCs
data.is = AUC_inSample()
data.os = AUC_outOfSample()

# plot AUCs
plotAUC_is(data.is, yaxes = c(.875,1,.025))
plotAUC_os(data.os, yaxes = c(.875,1,.025))

# save AUC plots
writePDFGraph()

# save AUC data
write_csv(data.is, './Evaluation/AUC/auc_insample.csv')
write_csv(data.os, './Evaluation/AUC/auc_outsample.csv')