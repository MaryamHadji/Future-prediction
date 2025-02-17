# R script for calculating annualized percentage change in hippocampal volume based on longitudinal MRI data
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 17-Feb-2025
#
# Requirements:
# - R version 4.3.1 or later
#
# Usage:
# - Set working directory to the directory containing this R script
# - Run using: source('Annualized percentage change.R')
#
# Parameters:
# - MRI_24: Data frame containing MRI measurements at timepoint 24 months
# - MRI_48: Data frame containing MRI measurements at timepoint 48 months
#
# Returned values:
# - A numeric vector representing the annualized percentage change in hippocampal volume
#

Change <- function(MRI_24, MRI_48) {
  hippo_24 <- rowMeans(MRI_24[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  hippo_48 <- rowMeans(MRI_48[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  
  Annualized_percentage_change <- ((hippo_24 - hippo_48) / hippo_24) * 100 / 2
  
  return(Annualized_percentage_change)
}
