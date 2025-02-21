# R script for generating ROC curves and AUC values for CN and MCI groups based on hippocampal/Ventricle/TGM volume and other models
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 21-Feb-2025
#
# Description:
# This function loads ROC values for CN and MCI groups, calculates AUC and confidence intervals,
# and generates ROC plots with ggplot2. The function saves the plots to the specified output path.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: pROC, ggplot2
#
# Usage:
# - Source this script in R or RStudio
# - Run using: generate_roc_plots(group = 'CN' or 'MCI', roc_data_path, output_path)
#
# Parameters:
# - group: 'CN' or 'MCI' to specify the analysis group
# - roc_data_path: Path to the RData file containing ROC values
# - output_path: Directory path to save the generated ROC plot
#
# Returned values:
# - Saves a PNG file of the ROC plot to the specified directory

library(pROC)
library(ggplot2)

generate_roc_plots <- function(group = 'CN', roc_data_path = 'path/to/Roc_CN_Hippo.Rdata', output_path = 'path/to/output.png') {
  # Load ROC data
  load(roc_data_path)
  
  # Calculate AUC with confidence intervals
  auc5 = paste('Hippocampal volume,\nAUC = ', round(as.numeric(roc_5$auc), 2),
               '(', round(as.numeric(ci.auc(roc_5))[1], 2), ',',
               round(as.numeric(ci.auc(roc_5))[3], 2), ')\n', sep = '')
  auc1 = paste('Baseline,\nMRI Only,\nAUC = ', round(as.numeric(roc_1$auc), 2),
               '(', round(as.numeric(ci.auc(roc_1))[1], 2), ',',
               round(as.numeric(ci.auc(roc_1))[3], 2), ')\n', sep = '')
  auc2 = paste('Baseline,\nMRI+Risk Factors,\nAUC = ', round(as.numeric(roc_2$auc), 2),
               '(', round(as.numeric(ci.auc(roc_2))[1], 2), ',',
               round(as.numeric(ci.auc(roc_2))[3], 2), ')\n', sep = '')
  auc3 = paste('Longitudinal,\nMRI Only,\nAUC = ', round(as.numeric(roc_3$auc), 2),
               '(', round(as.numeric(ci.auc(roc_3))[1], 2), ',',
               round(as.numeric(ci.auc(roc_3))[3], 2), ')\n', sep = '')
  auc4 = paste('Longitudinal,\nMRI+Risk Factors,\nAUC = ', round(as.numeric(roc_4$auc), 2),
               '(', round(as.numeric(ci.auc(roc_4))[1], 2), ',',
               round(as.numeric(ci.auc(roc_4))[3], 2), ')\n', sep = '')
  
  # Prepare ROC list for plotting
  roc_list = list(
    'Hippocampal/Ventricle/TGM volume' = roc_5,
    'Baseline,\nMRI Only' = roc_1,
    'Baseline,\nMRI+Risk Factors' = roc_2,
    'Longitudinal,\nMRI Only' = roc_3,
    'Longitudinal,\nMRI+Risk Factors' = roc_4
  )
  
  # Generate ROC plot
  g = ggroc(roc_list, legacy.axes = TRUE)
  plot_title = paste('ROC Curve for Hippocampus/Ventricle/TGM,', group)
  
  plot = g +
    xlab('False Positive Rate') +
    ylab('True Positive Rate') +
    ggtitle(plot_title) +
    scale_colour_manual(values = c('black', '#009E73', '#0072B2', '#D55E00', '#CC79A7'),
                        labels = c(auc5, auc1, auc2, auc3, auc4)) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                 color = 'darkgrey', linetype = 'dashed') +
    geom_line(size = 1) +
    theme(
      text = element_text(size = 16, face = 'bold', family = 'Times New Roman'),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 18, face = 'bold', hjust = 0.5)
    )
  
  # Save the plot to the specified output path
  ggsave(filename = output_path, plot = plot, width = 8, height = 5, dpi = 300)
}
