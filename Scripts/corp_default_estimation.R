# File: corp_default_estimation.R
# Authors: Tom Zimmermann, Clay Wagar, Tyler Pike
# Date: 
# Note(s): A boilerplate file for predicting binary outcomes in panel data (use logit, forests and nets)
#          Parallel back-end is set up to use the FRB enviroment, users may need to adjust the script accordingly

# clear enviroment
rm(list = ls())

# Load libraries
library(plyr)
library(dplyr)
library(caret)
library(glmnet)
library(stepPlr)
library(gbm)
library(pROC)
library(nnet)

# set random sed
set.seed(2149)

# Timing
scriptTime = proc.time()

# Experiment Settings 
# Type of estimation:
entirePeriod1 <- 1  # Standard cross-validation under iid assumption
entirePeriod2 <- 0  # Cross-validation that leaves groups intact
rolling1      <- 0  # Standard rolling estimation that cross-validates under iid assumption
rolling2      <- 0  # Rolling estimation with Cross-validation based on past WinCV periods
recursive1    <- 1  # Standard recursive estimation that cross-validates under iid assumption
recursive2    <- 0  # Recursive estimation with Cross-validation based on the past WinCV periods

# Type of model:
mLogit <- 1
mPLR   <- 0
mGBM   <- 1
mDNN   <- 0
mRF    <- 1
mANN   <- 1
mBGLM  <- 0
mNB    <- 0
mKNN   <- 0
mSVM   <- 1

# Other parameters
# For rolling and rescursive models:   How many periods back should data be used for training
WinLookback <- 20  
# For Rolling2 and Recursive2: Number of past periods for CV folds
WinCV <- 5  

#-----------------------------------------------------------------------
# Utility functions
#-----------------------------------------------------------------------
#function to adjust training data to 90-10 ratio

ninetyTen <- function(y,predictors){

     # as written this functions works only for two-class problems in which it
     # is known that one class represents greater than 90% of the observations

     #find minority class
     minClass = ifelse(
                length( which( y == unique(y)[1] )) / length(y) < .1,
                as.character(unique(y)[1]),
                as.character(unique(y)[2]))

     #keep all minority class values and randomly sample majority class to reach
     #90/10 split
     keep = c(which(y == minClass),
            sample(which(y != minClass), length(which(y == minClass))*9)
     )


     yNew = y[keep]
     predictorsNew = predictors[keep,]
     predictorsNew = as.data.frame(predictorsNew)

     output = list(predictorsNew,yNew)
     return(output)
      
}  

ninetyOne <- function(y,predictors){

     #as written this functions works only for two-class problems in which it
     #is known that one class represents greater than 90% of the observations

     #find minority class
     minClass = ifelse(
                length( which( y == unique(y)[1] )) / length(y) < .1,
                as.character(unique(y)[1]),
                as.character(unique(y)[2]))

     #keep all minority class values and randomly sample majority class to reach
     #90/10 split
     keep = c(which(y == minClass),
            sample(which(y != minClass), length(which(y == minClass))*99)
     )


     yNew = y[keep]
     predictorsNew = predictors[keep,]
     predictorsNew = as.data.frame(predictorsNew)

     output = list(predictorsNew,yNew)
     return(output)
      
}  

#-----------------------------------------------------------------------
# Define models' tuning grids
#-----------------------------------------------------------------------

if (mPLR == 1) {
  tuneGridPLR <- expand.grid(lambda = c(.001,.01,.1,.5,1), cp = "bic")
}

if (mGBM == 1) {
  tuneGridGBM <- expand.grid(interaction.depth = c(seq(3,9,by=2)),
                             #n.trees = c(200,400),
                             n.trees = c(400),
                             shrinkage = .1,
                             n.minobsinnode = 1)
}

if (mDNN == 1) {
  tuneGridDNN <- expand.grid(layer1 = 2:4, 
                             layer2 = 2:4,
                             layer3 = 2:3, 
                             hidden_dropout = 0, 
                             visible_dropout = 0)
}

if (mRF == 1){
     tuneGridRF <- expand.grid(mtry = c(1:8))
}
  
if (mANN == 1){
    tuneGridANN <- expand.grid(.size = seq(10,20,2),
                               .decay = c(.1,.001,.001))
}

if (mKNN == 1){
  tuneGridKNN <- expand.grid(k = seq(2,30,2))
}

if (mSVM == 1){
  tuneGridSVM <- expand.grid(sigma = c(.1),
                             C = c(1))
}

#-----------------------------------------------------------------------
# Prepare data
#-----------------------------------------------------------------------
# load data
df <- readRDS(file = "./Data/Intermedate/corp_default_data.RDS")   
df = na.omit(df)

# One or more outcomes (in columns)
outcomeAll <- df %>% select(y1, y2, y3, y4, y5, y6, y7, y8)

# Collect predictors             
predictorsAll <- select(df, -GVKEY, -quarter, - time_m, -starts_with("y"))

# Collect flags for sample restrictions 
# (NOTE: FLAG THAT CORRESPONDS TO AN OUTCOME VARIABLE HAS TO HAVE THE SAME NAME AS THAT OUTCOME VARIABLE)
flagsAll <- df %>%
  transmute(y1 = 1,
            y2 = 1,
            y3 = 1,
          	y4 = 1,
            y5 = 1,
           	y6 = 1)
        	  y7 = 1,
 			      y8 = 1)


# Tell R what ID and Time are in the dataset
IDsAll  <- df$GVKEY
TimeAll <- df$quarter  # NOTE: THIS NEEDS TO BE ADJUSTED FOR ROLLING AND RECURSIVE ESTIMATIONS !!

#-----------------------------------------------------------------------
# Estimation process
# Everything below here should be automatic (unless you add new models)
#-----------------------------------------------------------------------

# set models and model names
modelNames <- c("Logit", "PLR", "GBM", "DNN", "RF", 'ANN','BGLM','NB','KNN','SVM')
slct <- c(mLogit, mPLR, mGBM, mDNN, mRF, mANN, mBGLM, mNB, mKNN, mSVM)
modelNames <- modelNames[slct == 1]

modelNamesNoLogit <- c("PLR", "GBM", "DNN", "RF", "ANN",'NB','KNN','SVM')
slct <- c(mPLR, mGBM, mDNN, mRF, mANN, mNB, mKNN, mSVM)
modelNamesNoLogit <- modelNamesNoLogit[slct == 1]

### Entire period estimation with iid cross-validation ----

if (entirePeriod1 == 1) {

     library(doParallel)
     n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
     cl <-  makeCluster(n_cores)
     registerDoParallel(cl)


     
  # Define training setup
  trainCtrl <- trainControl(method = "cv", 
                            number = 5,
                            verboseIter = TRUE,
                            returnResamp = "all",
                            allowParallel = TRUE,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  for (ii in names(outcomeAll)) {

    start = proc.time()   
    # Select rows based on flag
    outcome <- outcomeAll[flagsAll[, ii] == 1, ]
    predictors <- predictorsAll[flagsAll[, ii] == 1, ]
    IDs <- IDsAll[flagsAll[, ii] == 1]
    
    y <- factor(outcome[[ii]], levels = c(0,1), labels = c("no", "yes"))

    ##90-10 downsampling
    downSample90 = ninetyTen(y,predictors)

    ##perform downsampling
     downSampleTrain = downSample(x = predictors,
                                  y = y,
                                  list=TRUE)
  
    y <- make.names(outcome[[ii]])   
    
    ## Estimations
    
    if (mLogit == 1) {
      fitLogit <- train(downSample90[[1]], downSample90[[2]], method="glm", family = "binomial",
                        trControl = trainCtrl)
    }
    
    if (mPLR == 1) {
      fitPLR <- train(downSample90[[1]], downSample90[[2]], method = "plr",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridPLR)
    }
    
    if (mGBM == 1) {
      
      fitGBM <- train(downSample90[[1]], downSample90[[2]], method = "gbm",
                      trControl = trainCtrl, tuneGrid = tuneGridGBM)
    }
    
    if (mDNN == 1) {
      fitDNN <- train(downSampleTrain[[1]], downSampleTrain[[2]],
                      method = "dnn",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridDNN)
    }
    
    if (mRF ==1){
         fitRF <- train(downSample90[[1]], downSample90[[2]],
                        method = "rf",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridRF
                        )
    }

    if (mANN ==1){
        #automatically bootstraps 25 times 
         fitANN <- train(downSample90[[1]], downSample90[[2]],   # I changed this for version 3, used to be downSampleTrain
                        method = "nnet",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridANN,
                        MaxNWts = 1000,
                        activation = 'relu',
                        maxit = 100,
                        trace = F
                      )
   }

   if (mBGLM ==1){
         fitBGLM <- train(downSample90[[1]], downSample90[[2]],
                        method = "bayesglm",
                        trControl = trainCtrl
                      )
   }

    if (mSVM ==1){
         fitSVM <- train(downSample90[[1]], downSample90[[2]],
                        method = "svmRadial",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridSVM
                      )
   }

    if (mKNN ==1){
      fitKNN <- train(downSample90[[1]], downSample90[[2]],
                     method = "knn",
                     trControl = trainCtrl,
                     tuneGrid = tuneGridKNN
      )
    }
    ## Evaluations
    
    # AUC as function of tuning parameters
    
    for (jj in modelNamesNoLogit) {
       
       pdf(file = paste0("./Estimation/entire1_TuningAUC_", ii, "_", jj, ".pdf"))
       temp     <- eval(as.name(paste0("fit", jj)))
       print(plot(temp))
       dev.off()
       
     }
    
    # ROC curves
    
    roc_list <- list()
    for (jj in modelNames) {
      
      temp     <- eval(as.name(paste0("fit", jj)))
      tempPred <- semi_join(temp$pred, temp$bestTune, by = names(temp$bestTune))  # This uses holdout-sample predictions for best tuning parameters only
      
      roc_list[[jj]] <- roc(tempPred$obs, tempPred$yes)
      
    }
    


    # Plot roc list
    data_graph <- lapply(names(roc_list), FUN = function(x){data_frame("model" = x, "sens" = roc_list[[x]]$sensitivities, "spec" = roc_list[[x]]$specificities)})
    data_graph <- bind_rows(data_graph)
    ggplot(data = data_graph, aes(x= 1-spec, y = sens, color = model)) + geom_line()
    ggsave(filename = paste0("./Estimation/entire1_rocCurves_", ii, ".pdf"), width = 6, height = 4)
    
    ######################
    ## Variable importance
    
    for (jj in modelNames) {
      temp     <- eval(as.name(paste0("fit", jj)))
      pdf(file = paste0("./Estimation/entire1_varImp_", ii, "_", jj, ".pdf"))
      print(plot(varImp(temp), top = 10))
      dev.off()
      
    }
    
    #######################
    ## Partial dependence [TODO]
    
    #######################
    ## Save model fits for later use
    # Note: THIS ONLY SAVES WHEN ALL 4 MODELS ARE RUN. GENERALIZE?
      save(fitLogit,  fitSVM, fitGBM, fitANN, fitRF, 
           file = paste0("./Estimation/entire1_Estimates_quarterl_y", ii, ".RData"))

      writeLines(paste0("\n\n\nLoop Time:\n\n-------",ii,"-----------\n\n\n"))
      print(proc.time()-start)
      writeLines("\n\n\n\n")
    
    
  }  # End loop over outcomes
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Entire period estimation with group-specific CV ----

if (entirePeriod2 == 1) {
  
  for (ii in names(outcomeAll)) {
    
    # Select rows based on flag
    outcome <- outcomeAll[flagsAll[, ii] == 1, ]
    predictors <- predictorsAll[flagsAll[, ii] == 1, ]
    IDs <- IDsAll[flagsAll[, ii] == 1]
    
    y <- factor(outcome[[ii]], levels = c(0,1), labels = c("no", "yes"))
    #y <- make.names(outcome[[ii]])   
    
    # Define training setup
    UIDs <- unique(IDs)
    
    # Draw folds of IDs
    IDFolds <- vector(mode = "list", length = 5)
    for(i in 1:5) IDFolds[[i]] <- sample(UIDs, floor(.8*length(UIDs)))
    
    # Create list of row indices corresponding to observations of IDs in IDFolds
    PanelFolds <- lapply(X = IDFolds, FUN = function(x){which(IDs %in% x)})
    
    trainCtrl <- trainControl(method = "cv",
                              index = PanelFolds,
                              returnResamp = "all",
                              verboseIter = T,
                              allowParallel = TRUE,
                              savePredictions = TRUE,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)
    
    
    ## Estimations
    
    if (mLogit == 1) {
      fitLogit <- train(predictors, y, method="glm", family = "binomial",
                        trControl = trainCtrl)
    }
    
    if (mPLR == 1) {
      fitPLR <- train(predictors, y, method = "plr",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridPLR)
    }
    
    if (mGBM == 1) {
      fitGBM <- train(predictors, y, method = "gbm",
                      trControl = trainCtrl, tuneGrid = tuneGridGBM)
    }
    
    if (mDNN == 1) {
      fitDNN <- train(predictors, y,
                      method = "dnn",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridDNN)
    }
    
    
    ## Evaluations
    
    ######################################
    # AUC as function of tuning parameters
    
    for (jj in modelNamesNoLogit) {
      
      pdf(file = paste0("./Estimation/entire2_TuningAUC_", ii, "_", jj, ".pdf"))
      temp     <- eval(as.name(paste0("fit", jj)))
      print(plot(temp))
      dev.off()
      
    }
    
    ###################################
    # ROC curves
    
    roc_list <- list()
    for (jj in modelNames) {
      
      temp     <- eval(as.name(paste0("fit", jj)))
      tempPred <- semi_join(temp$pred, temp$bestTune, by = names(temp$bestTune))  # This uses holdout-sample predictions for best tuning parameters only
      
      roc_list[[jj]] <- roc(tempPred$obs, tempPred$yes)
      
    }
    
    # Plot roc list
    data_graph <- lapply(names(roc_list), FUN = function(x){data_frame("model" = x, "sens" = roc_list[[x]]$sensitivities, "spec" = roc_list[[x]]$specificities)})
    data_graph <- bind_rows(data_graph)
    ggplot(data = data_graph, aes(x= 1-spec, y = sens, color = model)) + geom_line()
    ggsave(filename = paste0("./Estimation/entire2_rocCurves_", ii, ".pdf"), width = 6, height = 4)
    
    ######################
    ## Variable importance
    
    for (jj in modelNames) {
      temp     <- eval(as.name(paste0("fit", jj)))
      pdf(file = paste0("./Estimation/entire2_varImp_", ii, "_", jj, ".pdf"))
      print(plot(varImp(temp), top = 10))
      dev.off()
      
    }
    
    #######################
    ## Partial dependence [TODO]
    
    #######################
    ## Save model fits for later use
    if (length(modelNames) == 4) {  # Note: THIS ONLY SAVES WHEN ALL 4 MODELS ARE RUN. GENERALIZE?
      save(fitLogit, fitPLR, fitGBM, fitDNN, file = paste0("./Estimation/entire2_Estimates_", ii, ".RData"))
    }
    
  }  # End loop over outcomes
  
}

### Standard rolling estimation that cross-validates under iid assumption ----

if (rolling1 == 1) {
  
  # Define training setup
  trainCtrl <- trainControl(method = "cv", 
                            number = 5,
                            verboseIter = TRUE,
                            returnResamp = "all",
                            allowParallel = TRUE,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  for (ii in names(outcomeAll)) {  # Loop over outcomes
    
    # Select rows based on flag
    outcome <- outcomeAll[flagsAll[, ii] == 1, ]
    predictors <- predictorsAll[flagsAll[, ii] == 1, ]
    IDs <- IDsAll[flagsAll[, ii] == 1]
    Time <- TimeAll[flagsAll[, ii] == 1]
    
    # To make things easier, we'll add a pseudo time period ID which is just a running number
    TimeNew <- as.data.frame(Time)
    
    uniqueTime <- unique(TimeNew) %>%
      arrange() %>%
      mutate(samTime = row_number())
    
    TimeNew <- inner_join(TimeNew, uniqueTime, by = "Time")
    
    maxSamQ <- max(uniqueTime[,2])
    minSamQ <- min(uniqueTime[,2])
    
    y <- factor(outcome[[ii]], levels = c(0,1), labels = c("no", "yes"))
    #y <- make.names(outcome[[ii]])   
    
    # Initialize matrix to hold the predictions
    pred_list <- list()
    
    for (jj in modelNames) {
      
      pred_list[[jj]] <- as.data.frame(y) %>%
        bind_cols(as.data.frame(Time)) %>%
        mutate(preds = NA)
      
    }  # end loop over jj (models)
    
    for (tt in (minSamQ + WinLookback) : maxSamQ) {  # Loop over forecast periods
      
      # Prepare data
      ytt <- y[TimeNew[,2] < tt & TimeNew[,2] >= tt-WinLookback]
      predictorstt <- predictors[TimeNew[, 2] < tt & TimeNew[,2] >= tt-WinLookback, ]

      
      ## Estimations
      
      if (mLogit == 1) {
        fitLogit <- train(predictorstt, ytt, method="glm", family = "binomial",
                          trControl = trainCtrl)
      }
      
      if (mPLR == 1) {
        fitPLR <- train(predictorstt, ytt, method = "plr",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridPLR)
      }
      
      if (mGBM == 1) {
        fitGBM <- train(predictorstt, ytt, method = "gbm",
                        trControl = trainCtrl, tuneGrid = tuneGridGBM)
      }
      
      if (mDNN == 1) {
        fitDNN <- train(predictorstt, ytt,
                        method = "dnn",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridDNN)
      }
      
      
      ## Evaluations (for now let's just save predictions each round, easy to add e.g. variable importance, model fits...)
      
      # Use next period as testing data
      ytp <- y[TimeNew[, 2] == tt]
      predictorstp <- predictors[TimeNew[, 2] ==  tt, ]
      
      # Predict
      for (jj in modelNames) {
        
        temp     <- eval(as.name(paste0("fit", jj)))
        tempPred <- predict(object = temp, newdata = predictorstp, type = "prob")[["yes"]]
        pred_list[[jj]]$preds[TimeNew[,2]==tt] <- tempPred
        
      }
      
    }  # End loop over tt (periods)
    
    ###################################
    # Compute ROC curve
    roc_list <- list()
    for (jj in modelNames) {
      
      roc_list[[jj]] <- roc(pred_list[[jj]]$y, pred_list[[jj]]$preds)
      
    }
    
    # Plot roc curves
    data_graph <- lapply(names(roc_list), FUN = function(x){data_frame("model" = x, "sens" = roc_list[[x]]$sensitivities, "spec" = roc_list[[x]]$specificities)})
    data_graph <- bind_rows(data_graph)
    ggplot(data = data_graph, aes(x= 1-spec, y = sens, color = model)) + geom_line()
    ggsave(filename = paste0("./Estimation/rolling1_rocCurves_", ii, ".pdf"), width = 6, height = 4)
    
    # Save predictions
    save(pred_list, file = paste0("./Estimation/rolling1_predictions", ii, ".RData"))
  }  # End loop over outcomes
  
}

### Rolling estimation with CV based on past WinLookback periods ----

if (rolling2 == 1) {
  
  # Helpful function
  is.between <- function(x, a, b) {
    x >= a & x <= b
  }
  
  for (ii in names(outcomeAll)) {  # Loop over outcomes
    
    # Select rows based on flag
    outcome <- outcomeAll[flagsAll[, ii] == 1, ]
    predictors <- predictorsAll[flagsAll[, ii] == 1, ]
    IDs <- IDsAll[flagsAll[, ii] == 1]
    Time <- TimeAll[flagsAll[, ii] == 1]
    
    # To make things easier, we'll add a pseudo time period ID which is just a running number
    TimeNew <- as.data.frame(Time)
    
    uniqueTime <- unique(TimeNew) %>%
      arrange() %>%
      mutate(samTime = row_number())
    
    TimeNew <- inner_join(TimeNew, uniqueTime, by = "Time")
    
    maxSamQ <- max(uniqueTime[,2])
    minSamQ <- min(uniqueTime[,2])
    
    y <- factor(outcome[[ii]], levels = c(0,1), labels = c("no", "yes"))
    #y <- make.names(outcome[[ii]])   
    
    # Next, I define training and holdout periods
    panelSlices        <- list(list(),list())
    names(panelSlices) <- c("train","holdout")
    
    # Define folds
    for (tt in (minSamQ + WinLookback) : maxSamQ) {
      #    print(tt)
      panelSlices$train[[paste0("Training",tt)]] <-
        which(is.between(TimeNew[,2],
                         tt - WinLookback,
                         tt - 1))
      panelSlices$holdout[[paste0("Holdout",tt)]] <-
        which(TimeNew[,2] == tt)
    }
    
    # Define training setup
    trainCtrl <- trainControl(method = "cv",
                              index = panelSlices$train,
                              indexOut = panelSlices$holdout,
                              returnData = F,
                              returnResamp = "final",
                              verboseIter = T,
                              savePredictions = "all",
                              summaryFunction = twoClassSummary,
                              allowParallel = T,
                              classProbs = T
    )
    
    
    # Initialize matrix to hold the predictions
    pred_list <- list()
    
    for (jj in modelNames) {
      
      pred_list[[jj]] <- as.data.frame(y) %>%
        bind_cols(as.data.frame(Time)) %>%
        mutate(preds = NA)
      
    }  # end loop over jj (models)
    
    ## Estimations
    
    if (mLogit == 1) {
      fitLogit <- train(predictors, y, method="glm", family = "binomial",
                        trControl = trainCtrl)
    }
    
    if (mPLR == 1) {
      fitPLR <- train(predictors, y, method = "plr",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridPLR)
    }
    
    if (mGBM == 1) {
      fitGBM <- train(predictors, y, method = "gbm",
                      trControl = trainCtrl, tuneGrid = tuneGridGBM)
    }
    
    if (mDNN == 1) {
      fitDNN <- train(predictors, y,
                      method = "dnn",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridDNN)
    }
    
    
    ## Evaluations (Save predictions for best parameter fit) NOTE: WRITE MORE FLEXIBLY
    
    holdout_list = list()
    for (jj in modelNamesNoLogit) {
      
      temp     <- eval(as.name(paste0("fit", jj)))
      tempParam <- lapply(c("Resample", names(temp$bestTune)), as.symbol)  # This looks a bit weird but makes the code flexible.
      # It extracts the names of tuning parameters for any model and uses them for grouping below.
      holdout_list[[jj]] <- temp$pred %>%
        group_by_(.dots = tempParam) %>%
        mutate(aucs = auc(obs, yes))
      
      rm(temp, tempParam)
      
    }
    
    # Now find optimal tuning parameters based on WinCV past periods and assign predictions to pred_list
    for (jj in modelNamesNoLogit) {
      
      temp    <- holdout_list[[jj]]
      
      
      for (tt in (minSamQ + WinLookback + WinCV) : maxSamQ) {
        
        tempIdx <- which(is.between(TimeNew[,2],
                                    tt - WinCV,
                                    tt - 1))
        
        temp2 <- which(temp$rowIndex %in% tempIdx)
        tempT <- temp[temp2,]  # This looks terrible but I was not sure how to do it otherwise. The problem is that
        # it is difficult to subset based on Text, e.g. I could not figure out how to keep all
        # observations between Training4 and Training9
        
        if (jj == "PLR") {
          
          optTune <- tempT %>%
            group_by(lambda, cp) %>%
            summarise(aucs = mean(aucs)) %>%
            ungroup() %>%
            slice(which.min(aucs)) %>%
            head(1) %>%
            select(lambda, cp)
          
        }
        
        if (jj == "GBM") {
          optTune <- tempT %>%
            group_by(shrinkage, interaction.depth, n.trees, n.minobsinnode) %>%
            summarise(aucs = mean(aucs)) %>%
            ungroup() %>%
            slice(which.min(aucs)) %>%
            head(1) %>%
            select(shrinkage, interaction.depth, n.trees, n.minobsinnode)
        }
        
        if (jj == "DNN") {
          optTune <- tempT %>%
            group_by(layer1, layer2, layer3, hidden_dropout, visible_dropout) %>%
            summarise(aucs = mean(aucs)) %>%
            ungroup() %>%
            slice(which.min(aucs)) %>%
            head(1) %>%
            select(layer1, layer2, layer3, hidden_dropout, visible_dropout)
          
        }        
        
        testOutput <- temp %>%
          filter(Resample == paste0('Training', tt)) %>%
          semi_join(optTune)
        
        pred_list[[jj]]$preds[testOutput$rowIndex] <- testOutput$yes
        
        rm(optTune,testOutput, tempIdx, temp2, tempT)
        
        
      }  # End loop over time periods
      rm(temp)
    }  # End loop over models
    
    if (mLogit == 1) {  # For the logit, just add predictions, since there's nothing to tune [DROP FIRST WINBACK TIME PERIODS FOR COMPARISON?]
      pred_list[["Logit"]]$preds[fitLogit$pred$rowIndex] <- fitLogit$pred$yes
    }
    
    
    ###################################
    # Compute ROC curve
    roc_list <- list()
    for (jj in modelNames) {
      
      roc_list[[jj]] <- roc(pred_list[[jj]]$y, pred_list[[jj]]$preds)
      
    }
    
    # Plot roc curves
    data_graph <- lapply(names(roc_list), FUN = function(x){data_frame("model" = x, "sens" = roc_list[[x]]$sensitivities, "spec" = roc_list[[x]]$specificities)})
    data_graph <- bind_rows(data_graph)
    ggplot(data = data_graph, aes(x= 1-spec, y = sens, color = model)) + geom_line()
    ggsave(filename = paste0("./Estimation/rolling2_rocCurves_", ii, ".pdf"), width = 6, height = 4)
    
    # Save predictions
    save(pred_list, file = paste0("./Estimation/rolling2_predictions", ii, ".RData"))
    
  }  # End loop over outcomes
  
}

### Standard recursive estimation that cross-validates under iid assumption ----

if (recursive1 == 1) {


     library(doParallel)
     n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
     if(!is.na(n_cores)){
        cl <-  makeCluster(n_cores)
        registerDoParallel(cl)
     }

     
  # Define training setup
  trainCtrl <- trainControl(method = "cv", 
                            number = 5,
                            verboseIter = TRUE,
                            returnResamp = "all",
                            allowParallel = TRUE,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  for (ii in names(outcomeAll)) {  # Loop over outcomes
    
    # Select rows based on flag
    outcome <- outcomeAll[flagsAll[, ii] == 1, ]
    predictors <- predictorsAll[flagsAll[, ii] == 1, ]
    IDs <- IDsAll[flagsAll[, ii] == 1]
    Time <- TimeAll[flagsAll[, ii] == 1]
    
    # To make things easier, we'll add a pseudo time period ID which is just a running number
    TimeNew <- as.data.frame(Time)
    
    uniqueTime <- unique(TimeNew) %>%
      arrange() %>%
      mutate(samTime = row_number())
    
    TimeNew <- inner_join(TimeNew, uniqueTime, by = "Time")
    
    maxSamQ <- max(uniqueTime[,2])
    minSamQ <- min(uniqueTime[,2])
    
    y <- factor(outcome[[ii]], levels = c(0,1), labels = c("no", "yes"))
    #y <- make.names(outcome[[ii]])   
    
    
    # Initialize matrix to hold the predictions
    pred_list <- list()
    
    for (jj in modelNames) {
      
      pred_list[[jj]] <- as.data.frame(y) %>%
        bind_cols(as.data.frame(Time)) %>%
        mutate(preds = NA,
               GVKEY = IDs)  #ensures that we can track which predictions are for which bank
      
    }  # end loop over jj (models)
    
    for (tt in (minSamQ + WinLookback) : maxSamQ) {  # Loop over forecast periods
      start=proc.time()
      # Prepare data
      ytt <- y[TimeNew[,2] < tt]
      predictorstt <- predictors[TimeNew[, 2] < tt, ]

      ##Perform downsampling

      dS <- ninetyTen(ytt,predictorstt)

      #dS <- downSample(predictorstt,ytt,list=TRUE)  

      #dS <- ninetyOne(ytt,predictorstt)
      
      ## Estimations
      
      if (mLogit == 1) {
        fitLogit <- train(dS[[1]], dS[[2]], method="glm", family = "binomial",
                          trControl = trainCtrl)
      }
      
      if (mPLR == 1) {
        fitPLR <- train(predictorstt, ytt, method = "plr",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridPLR)
      }
      


      if (mGBM == 1) {
        fitGBM <- train(dS[[1]],dS[[2]], method = "gbm",
                        trControl = trainCtrl, tuneGrid = tuneGridGBM)
      }
      
      if (mDNN == 1) {
        fitDNN <- train(predictorstt, ytt,
                        method = "dnn",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridDNN)
      }
      
      if (mRF ==1){
         fitRF <- train(dS[[1]],dS[[2]],
                        method = "rf",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridRF
                        )
      }
      
      if (mANN ==1){
        #automatically bootstraps 25 times 
        fitANN <- train(dS[[1]],dS[[2]],
                        method = "nnet",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridANN,
                        MaxNWts = 10000,
                        activation = 'relu',
                        maxit = 100,
                        trace = F
        )
      }
      
      if (mSVM ==1){
        fitSVM <- train(dS[[1]],dS[[2]],
                        method = "svmRadial",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridSVM
        )
      }
      
      if (mKNN ==1){
        fitKNN <- train(dS[[1]],dS[[2]],
                        method = "knn",
                        trControl = trainCtrl,
                        tuneGrid = tuneGridKNN
        )
      }
      
      
      
      ## Evaluations (for now let's just save predictions each round, easy to add e.g. variable importance, model fits...)
      
      # Use next period as testing data
      ytp <- y[TimeNew[, 2] == tt]
      predictorstp <- predictors[TimeNew[, 2] ==  tt, ]
      
      # Predict
      for (jj in modelNames) {
        
        temp     <- eval(as.name(paste0("fit", jj)))
        tempPred <- predict(object = temp, newdata = predictorstp, type = "prob")[["yes"]]
        pred_list[[jj]]$preds[TimeNew[,2]==tt] <- tempPred

      }
        writeLines(paste0("\n\n\nLoop Time:\n\n-------",tt,"-----------\n\n\n---------",ii,"--------------\n\n\n"))
      print(proc.time()-start)
      writeLines("\n\n\n\n")
    }  # End loop over tt (periods)
    
    ###################################
    # Compute ROC curve
     roc_list <- list()
     for (jj in modelNames) {
       
       roc_list[[jj]] <- roc(pred_list[[jj]]$y, pred_list[[jj]]$preds)
       
     }
    
     #Plot roc curves
     data_graph <- lapply(names(roc_list), FUN = function(x){data_frame("model" = x, "sens" = roc_list[[x]]$sensitivities, "spec" = roc_list[[x]]$specificities)})
     data_graph <- bind_rows(data_graph)
     ggplot(data = data_graph, aes(x= 1-spec, y = sens, color = model)) + geom_line()
     ggsave(filename = paste0("./Estimation/Recursive1_rocCurves_", ii, ".pdf"), width = 6, height = 4)
    
    # Save predictions
    save(pred_list, file = paste0("./Estimation/Recursive1_predictions_", ii, ".RData"))
    
  }  # End loop over outcomes
  
}

### Recursive estimation with CV based on past WinLookback periods ----

if (recursive2 == 1) {
  
  # Helpful function
  is.between <- function(x, a, b) {
    x >= a & x <= b
  }
  
  for (ii in names(outcome)) {  # Loop over outcomes
    
    # Select rows based on flag
    outcome <- outcomeAll[flagsAll[, ii] == 1, ]
    predictors <- predictorsAll[flagsAll[, ii] == 1, ]
    IDs <- IDsAll[flagsAll[, ii] == 1]
    Time <- TimeAll[flagsAll[, ii] == 1]
    
    # To make things easier, we'll add a pseudo time period ID which is just a running number
    TimeNew <- as.data.frame(Time)
    
    uniqueTime <- unique(TimeNew) %>%
      arrange() %>%
      mutate(samTime = row_number())
    
    TimeNew <- inner_join(TimeNew, uniqueTime, by = "Time")
    
    maxSamQ <- max(uniqueTime[,2])
    minSamQ <- min(uniqueTime[,2])
    
    y <- factor(outcome[[ii]], levels = c(0,1), labels = c("no", "yes"))
    #y <- make.names(outcome[[ii]])   
    
    # Next, I define training and holdout periods
    panelSlices        <- list(list(),list())
    names(panelSlices) <- c("train","holdout")
    
    # Define folds
    for (tt in (minSamQ + WinLookback) : maxSamQ) {
      #    print(tt)
      panelSlices$train[[paste0("Training",tt)]] <-
        which(is.between(TimeNew[,2],
                         tt - WinLookback,
                         tt - 1))
      panelSlices$holdout[[paste0("Holdout",tt)]] <-
        which(TimeNew[,2] == tt)
    }
    
    # Define training setup
    trainCtrl <- trainControl(method = "cv",
                              index = panelSlices$train,
                              indexOut = panelSlices$holdout,
                              returnData = F,
                              returnResamp = "final",
                              verboseIter = T,
                              savePredictions = "all",
                              summaryFunction = twoClassSummary,
                              allowParallel = T,
                              classProbs = T
    )
    
    
    # Initialize matrix to hold the predictions
    pred_list <- list()
    
    for (jj in modelNames) {
      
      pred_list[[jj]] <- as.data.frame(y) %>%
        bind_cols(as.data.frame(Time)) %>%
        mutate(preds = NA)
      
    }  # end loop over jj (models)
    
    ## Estimations
    
    if (mLogit == 1) {
      fitLogit <- train(predictors, y, method="glm", family = "binomial",
                        trControl = trainCtrl)
    }
    
    if (mPLR == 1) {
      fitPLR <- train(predictors, y, method = "plr",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridPLR)
    }
    
    if (mGBM == 1) {
      fitGBM <- train(predictors, y, method = "gbm",
                      trControl = trainCtrl, tuneGrid = tuneGridGBM)
    }
    
    if (mDNN == 1) {
      fitDNN <- train(predictors, y,
                      method = "dnn",
                      trControl = trainCtrl,
                      tuneGrid = tuneGridDNN)
    }
    
    
    ## Evaluations (Save predictions for best parameter fit) NOTE: WRITE MORE FLEXIBLY
    
    holdout_list = list()
    for (jj in modelNamesNoLogit) {
      
      temp     <- eval(as.name(paste0("fit", jj)))
      tempParam <- lapply(c("Resample", names(temp$bestTune)), as.symbol)  # This looks a bit weird but makes the code flexible.
      # It extracts the names of tuning parameters for any model and uses them for grouping below.
      holdout_list[[jj]] <- temp$pred %>%
        group_by_(.dots = tempParam) %>%
        mutate(aucs = auc(obs, yes))
      
      rm(temp, tempParam)
      
    }
    
    # Now find optimal tuning parameters based on WinCV past periods and assign predictions to pred_list
    for (jj in modelNamesNoLogit) {
      
      temp    <- holdout_list[[jj]]
      
      
      for (tt in (minSamQ + WinLookback + WinCV) : maxSamQ) {
        
        tempIdx <- which(is.between(TimeNew[,2],
                                    tt - WinCV,
                                    tt - 1))
        
        temp2 <- which(temp$rowIndex %in% tempIdx)
        tempT <- temp[temp2,]  # This looks terrible but I was not sure how to do it otherwise. The problem is that
        # it is difficult to subset based on Text, e.g. I could not figure out how to keep all
        # observations between Training4 and Training9
        
        if (jj == "PLR") {
          
          optTune <- tempT %>%
            group_by(lambda, cp) %>%
            summarise(aucs = mean(aucs)) %>%
            ungroup() %>%
            slice(which.min(aucs)) %>%
            head(1) %>%
            select(lambda, cp)
          
        }
        
        if (jj == "GBM") {
          optTune <- tempT %>%
            group_by(shrinkage, interaction.depth, n.trees, n.minobsinnode) %>%
            summarise(aucs = mean(aucs)) %>%
            ungroup() %>%
            slice(which.min(aucs)) %>%
            head(1) %>%
            select(shrinkage, interaction.depth, n.trees, n.minobsinnode)
        }
        
        if (jj == "DNN") {
          optTune <- tempT %>%
            group_by(layer1, layer2, layer3, hidden_dropout, visible_dropout) %>%
            summarise(aucs = mean(aucs)) %>%
            ungroup() %>%
            slice(which.min(aucs)) %>%
            head(1) %>%
            select(layer1, layer2, layer3, hidden_dropout, visible_dropout)
          
        }        
        
        testOutput <- temp %>%
          filter(Resample == paste0('Training', tt)) %>%
          semi_join(optTune)
        
        pred_list[[jj]]$preds[testOutput$rowIndex] <- testOutput$yes
        
        rm(optTune,testOutput, tempIdx, temp2, tempT)
        
        
      }  # End loop over time periods
      rm(temp)
    }  # End loop over models
    
    if (mLogit == 1) {  # For the logit, just add predictions, since there's nothing to tune [DROP FIRST WINBACK TIME PERIODS FOR COMPARISON?]
      pred_list[["Logit"]]$preds[fitLogit$pred$rowIndex] <- fitLogit$pred$yes
    }
    
    
    ###################################
    # Compute ROC curve
    roc_list <- list()
    for (jj in modelNames) {
      
      roc_list[[jj]] <- roc(pred_list[[jj]]$y, pred_list[[jj]]$preds)
      
    }
    
    # Plot roc curves
    data_graph <- lapply(names(roc_list), FUN = function(x){data_frame("model" = x, "sens" = roc_list[[x]]$sensitivities, "spec" = roc_list[[x]]$specificities)})
    data_graph <- bind_rows(data_graph)
    ggplot(data = data_graph, aes(x= 1-spec, y = sens, color = model)) + geom_line()
    ggsave(filename = paste0("./Estimation/recursive2_rocCurves_", ii, ".pdf"), width = 6, height = 4)
    
    # Save predictions
    save(pred_list, file = paste0("./Estimation/recursive2_predictions", ii, ".RData"))
    
  }  # End loop over outcomes
  
}


print(proc.time()-scriptTime)

