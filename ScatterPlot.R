# R script for generating a scatter plot comparing predicted and true atrophy rates
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 20-Feb-2025
#
# Description:
# This script loads an RData file, extracts relevant data, and creates a scatter plot 
# comparing predicted and true atrophy rates for the hippocampus atrophy model.
# The plot is saved as a PNG file, with points colored by diagnosis labels ("CN", "MCI", "Dementia").
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: ggplot2, extrafont (optional for custom fonts)
#
# Usage:
# - Set the working directory to the directory containing this R script
# - Run using: source('Generate_Hippocampus_ScatterPlot.R')
#
# Parameters:
# - rdata_path: Path to the RData file containing Pearson correlation results
# - output_path: Path to save the generated scatter plot as a PNG file
# - yhat_pred: Matrix of predicted atrophy rates
# - Y: True atrophy rates
# - DX: Diagnosis labels ("CN", "MCI", "Dementia")
#
# Returned values:
# - A scatter plot comparing predicted and true atrophy rates, colored by diagnosis labels


Scatterplot <- function(rdata_path, output_path, yhat_pred, Y, DX) {
  
  load(rdata_path)
  
  
  i <- sort(Pearson_R, index.return = TRUE)
  i <- i$ix[6]
  
  
  yhat <- yhat_pred[, i]
  
  
  df <- data.frame(yhat, Y, Diagnosis = DX)
  df$Diagnosis <- factor(df$Diagnosis, levels = c("CN", "MCI", "Dementia"))
  
  
  Model_4 <- ggplot(df, aes(x = Y, y = yhat, color = Diagnosis)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "black") +
    scale_x_continuous(name = "True atrophy rate", breaks = c(-2, 0, 2, 4, 6, 8, 10), limits = c(-2, 10)) +
    scale_y_continuous(name = "Predicted atrophy rate", breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
    labs(x = "True atrophy rate", y = "Predicted atrophy rate") +
    ggtitle("Longitudinal MRI+\nRisk Factors Model") +
    theme(
      plot.title = element_text(family = "Times New Roman", size = 16, face = "bold", hjust = 0.5, vjust = 1),
      axis.title.x = element_text(family = "Times New Roman", size = 16, face = "bold"),
      axis.title.y = element_text(family = "Times New Roman", size = 16, face = "bold"),
      axis.text.x = element_text(family = "Times New Roman", size = 14, face = "bold"),
      axis.text.y = element_text(family = "Times New Roman", size = 14, face = "bold"),
      legend.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
      legend.text = element_text(family = "Times New Roman", size = 14, face = "bold")
    )
  
  # Save plot
  png(output_path, width = 6, height = 5, units = "in", res = 1200)
  print(Model_4_Hippocampus)
  dev.off()
}