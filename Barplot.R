# R script for generating a bar chart comparing correlation values for different models 
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 17-Feb-2025
#
# Description:
# This script generates a grouped bar chart with error bars to compare correlation values
# across different models using data from RData files. It calculates mean correlation values
# along with confidence intervals and visualizes them using ggplot2.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: ggplot2, extrafont
#
# Usage:
# - Set the working directory to the directory containing this R script
# - Run using: source('Generate_Hippocampus_Plot.R')
#
# Parameters:
# - file_paths: A list of file paths to RData files containing correlation results
# - output_path: The file path to save the resulting plot as a PNG
#
# Returned values:
# - A grouped bar chart with error bars comparing correlation values across models

library(ggplot2)
library(extrafont)

# Check if Times New Roman font is already available, otherwise import it once

if (!"Times New Roman" %in% fonts()) {
  font_import()
}

fonts()

# Validate file paths
validate_paths <- function(paths) {
  for (path in paths) {
    if (!file.exists(path)) {
      stop(paste("File does not exist:", path))
    }
  }
}

generate_hippocampus_plot <- function(file_paths, output_path) {
  ########################
  # Define color palettes
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbPalette_rev <- rev(cbPalette)
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cbbPalette_rev <- rev(cbbPalette)
  
  ##################################
  # Load correlation results
  Pearson_values <- c()
  CI_values <- list()
  
  # Validate paths before loading data
  validate_paths(file_paths)
  
  for (i in seq_along(file_paths)) {
    load(file_paths[[i]])  # Load each RData file
    Pearson_values <- c(Pearson_values, mean(Pearson_R))  # Assuming Pearson_R is defined in the RData
    CI_values[[i]] <- CI[,2]  # Assuming CI is a matrix or data frame with lower/upper CI in column 2
  }
  
  R <- Pearson_values
  
  Group <- c(rep("MRI Only", length(R)/2), rep("MRI + Risk Factors", length(R)/2))
  Model <- c("Baseline", "Longitudinal", "Baseline", "Longitudinal")
  CI <- do.call(rbind, CI_values)
  
  df <- data.frame(Group, Model, R, CI)
  df$Group <- factor(df$Group, levels = c("MRI Only", "MRI + Risk Factors"))
  df$Model <- factor(df$Model, levels = c("Baseline", "Longitudinal"))
  
  # Prepare p-values for the plot
  p_values_df <- data.frame(
    Group = c("MRI Only", "MRI + Risk Factors"),
    p_value = c(0.02, 0.03),
    y_position = c(0.65, 0.68)
  )
  
  # Create the bar plot
  Barplot <- ggplot(df, aes(fill = Model, y = R, x = Group)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.2, size = 0.5, position = position_dodge(0.5)) +
    scale_x_discrete(name = " ") +
    scale_y_continuous(name = "Correlation", breaks = c(0, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70), limits = c(0, 0.75)) +
    scale_fill_manual(values = cbPalette) +
    labs(title = "Hippocampus") +
    geom_text(data = p_values_df, aes(x = Group, y = y_position, label = paste0("P-value = ", p_value)),
              inherit.aes = FALSE, vjust = -0.5, size = 5, fontface = "bold", family = "Times New Roman") +
    theme(
      axis.line.x = element_line(size = 0.5, colour = "white"),
      axis.line.y = element_line(size = 0.5, colour = "white"),
      legend.title = element_text(size = 16, face = "bold", family = "Times New Roman"),
      legend.text = element_text(size = 16, face = "bold", family = "Times New Roman"),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      axis.text.x = element_text(colour = "black", size = 16, face = "bold", family = "Times New Roman"),
      axis.text.y = element_text(colour = "black", size = 16, face = "bold", family = "Times New Roman"),
      axis.title = element_text(size = 18, face = "bold", family = "Times New Roman"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Times New Roman")
    )
  
  # Save the plot
  png(output_path, width = 6, height = 5, units = "in", res = 1200)
  print(Barplot)
  dev.off()
}

