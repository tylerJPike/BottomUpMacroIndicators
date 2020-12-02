#!/bin/bash

# This script will create necessary file structure to run the code
#   associated with the NFCH construction and analysis project

# Data folders
mkdir ./Data
mkdir ./Data/Input
mkdir ./Data/Intermediate
mkdir ./Data/Output
mkdir ./Data/Analysis
mkdir ./Data/Figures

# Estimation folders for firm-level probabilities of default
#    probabilities are saved to the top-level and then it is suggested to reorganize accordingly
mkdir ./Estimation
mkdir ./Estimation/In-Sample
mkdir ./Estimation/Out-of-Sample

# Evaluation folders to hold anlaysis results
mkdir ./Evaluation
mkdir ./Evaluation/AUC
mkdir ./Evaluation/Forecast
mkdir ./Evaluation/JordaIRF
