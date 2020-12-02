# Bottom-up leading macroeconomic indicators: An application to non-financial corporate defaults using machine learning

This repository holds the replication files for experiments from:

Tyler Pike, Horacio Sapriza, and Tom Zimmermann. **Bottom-up leading macroeconomic indicators: An application to non-financial corporate defaults using machine learning** September 2019. [Working Paper](https://www.federalreserve.gov/econres/feds/bottom-up-leading-macroeconomic-indicators-an-application-to-non-financial-corporate-defaults-using-machine-learning.htm)

Abstract: This paper constructs a leading macroeconomic indicator from microeconomic data using recent machine learning techniques. Using tree-based methods, we estimate probabilities of default for publicly traded non-financial firms in the United States. We then use the cross-section of out-of-sample predicted default probabilities to construct a leading indicator of non-financial corporate health. The index predicts real economic outcomes such as GDP growth and employment up to eight quarters ahead. Impulse responses validate the interpretation of the index as a measure of financial stress.

## Workflow
1. Prepare data   
    1. create firm-level data  
    script: corp_default_data_creation   
    output: corp_default_data_fisd.RDS  

    2. calculate summary statistics and latex tables  
    script: corp_default_data_tables  
    output: latex table in terminal  

    3. create summary charts of firm-level data  
    script: corp_default_data_charts  
    output: /Figures/observed_data.pdf  

2. Create NFCH 
    1. estimate firm-level defaults   
    script: corp_default_estimation  
    input:  corp_default_data_fisd.RDS  
    output: /Estimation/[predicted values], /Outputgraphs/[tuning AUC charts][ROC curves][variable importance]  

    2. calculate cross-sectional moments and aggregate firm-level probabilities into macro index  
    script: corp_default_index_creation    
    input: /Estimatin/[predicted values]  
    output: /Data/Output/indexes_aggregate.csv  
    
    3. chart aggregate index  
    script: corp_default_index_charts  
    input: /Data/Output/indexes_aggregate.csv  
    output: /Evaluation/Index/[charts]  

3. Validate index    
    1. compare in- and out-of-sample AUCs of machine learning methods  
    script: corp_default_auc  
    input: /Estimation/[predicted values]   
    output: /Evaluation/AUC/auc_plot.pdf  

    2. estimate impulse response functions  
    script: corp_default_IRF  
    input: /Data/Output/indexes_aggregate.csv   
    output: /Evaluation/JordaIRF/[charts]  

    3. perform forecasting exercises  
    scripts: corp_default_forecast_insample,  corp_default_forecast_outsample    
    input: /Data/Output/indexes_aggregate.csv   
    output: latex tables in terminal

## Notes
Set up
1. The required output files are included in this repo, however, if they need to be reinstantiated, then running the `initialize.sh` script will create the needed directories to run the replication.
2. One will need access to FRB data sources to fully run this replication, but publicly available data may be substituted in as a proxy in several instances.

Run-time
1. File paths assume a unix environment and that the working directory is the project's root.
2. The recursive index takes days to run, if constructed from scratch; parallel processing is necessary for reasonable execution times.
3. Users WRDS credentials will have to be supplied in `corp_default_data_creation.R` and `corp_default_index_creation.R`
4. Files may be run one-at-a-time independently or all together via `replicate.sh` 



