# Elastic Net Model for Predicting Hippocampal Atrophy
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 20.Feb.2025

# Requirements:
# R version 4.3.1 or later
# install.packages("glmnet")
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("Metrics")

# Usage:
# Set working directory to the directory containing this script
# source('ENLR.R')

# Parameters:
# model_type: An integer (1, 2, 3, or 4) indicating the model configuration to use
# Y: response variable (hippocampal atrophy percentage)
# seed: seed number for reproducible results

# Returned values:
# A list of model performance metrics including:
# - Pearson_R: Pearson correlation coefficients
# - Spearman_R: Spearman correlation coefficients
# - allmae: Mean Absolute Error values
# - CI: Confidence Intervals for predictions
# - coef_all: Coefficients from the model

# Model name
# 1-BL_ MRI only  
# 2-BL_ MRI+Riskfactors  
# 3-Longitudinal_ MRI only 
# 4-Longitudinal_ MRI+Riskfactors  

# Load Required Libraries
library(Matrix)
library(ggplot2)
library(lattice)
library(Metrics)
library(caret)
library(randomForest)
library(dplyr)
library(glmnet)

# Load Required Function
source("/Users/maryamh/Desktop/Athrophy_Project/Paper/Functions/ENLR.R")

# Load Data
load("TotalData_bl_24_48.Rdata")

# Function to Calculate Annualized Percentage Change
Change <- function(MRI_24, MRI_48) {
  hippo_24 <- rowMeans(MRI_24[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  hippo_48 <- rowMeans(MRI_48[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  Annualized_percentage_change <- ((hippo_24 - hippo_48) / hippo_24) * 100 / 2
  return(Annualized_percentage_change)
}

# Calculate Hippocampal Atrophy for Training Data
Y <- Annualized_percentage_change

# Function to Prepare Data and Run Elastic Net Model
run_model <- function(model_type, Y, seed = 123) {
  set.seed(seed)
  
  if (model_type == 1) {
    Xdata <- cbind(MRI_24, Feild_strengt)
  } else if (model_type == 2) {
    Xdata <- cbind(MRI_24, Age, sex, DX_24, Feild_strengt)
  } else if (model_type == 3) {
    Xdata <- cbind(MRI_24, MRI_24bl, Feild_strengt)
  } else if (model_type == 4) {
    Xdata <- cbind(MRI_24, MRI_24bl, Age, sex, APOE4, DX_24, Feild_strengt)
  } else {
    stop("Invalid model type. Choose 1, 2, 3, or 4.")
  }
  
  # No exclusion of values based on Y
  RID = rownames(Xdata)
  
  # Normalization
  normParam <- preProcess(Xdata, method = c("center", "scale"))
  Xdata <- predict(normParam, Xdata)
  
  Pearson_R = vector()
  Spearman_R = vector()
  allmae = vector()
  yhat_pred = matrix(0, nrow = length(RID), ncol = 10)
  coef_all = list()
  
  for (ll in 1:10) {
    res = Regression_EL(Xdata, Y, 0.5, ll)
    coef_all[[ll]] = res$coefs
    yhat_pred[, ll] = res$yhat
    Pearson_R = c(Pearson_R, res$pearson_cor)
    Spearman_R = c(Spearman_R, res$spearman_cor)
    allmae = c(allmae, res$MAE)
  }
  
  # Confidence Interval calculation
  source("Functions/conf_int_2.R")
  CI = conf_int(yhat_pred, Y, nboot = 1000, alpha = 0.05)
  
  # Save results
  save(Pearson_R, Spearman_R, allmae, yhat_pred, Y, coef_all, CI, DX_bl, DX,
       file = paste0("Results/Model_", model_type, "_hippo_ENLR.Rdata"))
  
  list(Pearson_R = mean(Pearson_R), Spearman_R = mean(Spearman_R),
       allmae = mean(allmae), CI = CI, coef_all = coef_all)
}

