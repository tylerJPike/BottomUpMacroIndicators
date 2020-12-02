#!/bin/bash

# This script acts a wrapper to run all scripts necessary to replication 
#   Bottom-up leading macroeconomic indicators: An application to non-financial corporate defaults using machine learning,
#   by Tyler Pike, Horacio Sapriza, and Tom Zimmermann

#-------------------------------------
# 1. Prepare data 
#-------------------------------------
# 1. create firm-level data  
Rscript corp_default_data_creation.R
# 2. calculate summary statistics and latex tables  
Rscript corp_default_data_tables.R
# 3. create summary charts of firm-level data  
Rscript corp_default_data_charts.R  

#-------------------------------------
# 2. Create NFCH 
#-------------------------------------
# 1. estimate firm-level defaults   
Rscript corp_default_estimation.R  
# 2. calculate cross-sectional moments and aggregate firm-level probabilities into macro index  
Rscript corp_default_index_creation.R    
# 3. chart aggregate index  
Rscript corp_default_index_charts.R  

#-------------------------------------    
# 3. Validate index    
#-------------------------------------
#1. compare in- and out-of-sample AUCs of machine learning methods  
Rscript: corp_default_auc.R  
# 2. estimate impulse response functions  
Rscript: corp_default_IRF.R  
# 3. perform forecasting exercises  
Rscript corp_default_forecast_insample.R
Rscript corp_default_forecast_outsample.R    
    