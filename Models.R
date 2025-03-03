# Elastic Net Model for Predicting Brain Atrophy
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

# Load Custom Functions
source("Functions/ENLR.R")
source("Functions/conf_int_2.R")

# Load Data
load("TotalData_bl_24_48.Rdata")

# Function to Calculate Annualized Percentage Change
Annualized_percentage_change <- function(MRI_24, MRI_48, region = "hippocampus") {
  if (region == "hippocampus") {
    region_24 <- rowMeans(MRI_24[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
    region_48 <- rowMeans(MRI_48[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  } else if (region == "ventricles") {
    region_24 <- rowMeans(roitot_24[, c("Right Inf Lat Vent", "Left Inf Lat Vent", 
                                        "Right Lateral Ventricle", "Left Lateral Ventricle")], na.rm = TRUE)
    region_48 <- rowMeans(roitot_48[, c("Right Inf Lat Vent", "Left Inf Lat Vent", 
                                        "Right Lateral Ventricle", "Left Lateral Ventricle")], na.rm = TRUE)
  } else if (region == "TGM") {
    region_24 <- totalgray_24
    region_48 <- totalgray_48
  } else {
    stop("Invalid region specified. Choose 'hippocampus', 'ventricles', or 'TGM'.")
  }
  
  return(((region_24 - region_48) / region_24) * 100 / 2)
}

# Function to Prepare Data and Run Elastic Net Model
run_model <- function(model_type, region = "hippocampus", seed = 123) {
  set.seed(seed)
  
  # Calculate Atrophy Rate for Selected Region
  Y <- Annualized_percentage_change(MRI_24, MRI_48, region)
  
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
  
  RID <- rownames(Xdata)
  
  # Normalization
  normParam <- preProcess(Xdata, method = c("center", "scale"))
  Xdata <- predict(normParam, Xdata)
  
  Pearson_R <- vector()
  Spearman_R <- vector()
  allmae <- vector()
  yhat_pred <- matrix(0, nrow = length(RID), ncol = 10)
  coef_all <- list()
  
  for (ll in 1:10) {
    res <- ENLR(Xdata, Y, 0.5, ll)
    coef_all[[ll]] <- res$coefs
    yhat_pred[, ll] <- res$yhat
    Pearson_R <- c(Pearson_R, res$pearson_cor)
    Spearman_R <- c(Spearman_R, res$spearman_cor)
    allmae <- c(allmae, res$MAE)
  }
  
  # Confidence Interval calculation
  CI <- conf_int(yhat_pred, Y, nboot = 1000, alpha = 0.05)
  
  save(Pearson_R, Spearman_R, allmae, yhat_pred, Y, coef_all, CI, DX_bl, DX,
       file = paste0("Results/Model_", model_type, "_", region, "_ENLR.Rdata"))
  
  return(list(Pearson_R = mean(Pearson_R), Spearman_R = mean(Spearman_R),
              allmae = mean(allmae), CI = CI, coef_all = coef_all))
}