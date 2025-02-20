# R script for comparing correlations of predicted and true atrophy rates using the cocor package
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 20-Feb-2025
#
# Description:
# This function loads two RData files, extracts the 5th highest Pearson correlation values,
# and compares the correlations of predicted and true atrophy rates using the cocor package.
# The function outputs the statistical comparison of the two correlations.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: cocor
#
# Usage:
# - Source this script in R or RStudio
# - Run using: compare_hippocampus_atrophy_correlations(path1, path2)
#
# Parameters:
# - path1: File path to the first RData file
# - path2: File path to the second RData file
#
# Returned values:
# - The output of the cocor function comparing the two correlation models

compare_hippocampus_atrophy_correlations <- function(path1, path2) {
  
  library(cocor)
  
  load(path1)
  ind1 <- sort(Pearson_R, index.return = TRUE)$ix[5]
  yhat1 <- yhat_pred[, ind1]
  Y1 <- Y
  
  load(path2)
  ind2 <- sort(Pearson_R, index.return = TRUE)$ix[5]
  yhat2 <- yhat_pred[, ind2]
  Y2 <- Y
  
  
  dat1 <- data.frame(label = Y, yhat_a = yhat2, yhat_b = yhat1)
  
  
  result <- cocor(~label + yhat_a | label + yhat_b, data = dat1)
  
  
  return(result)
}
