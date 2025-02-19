# This function generates a bar chart comparing correlation values for different models.
#
# It loads correlation results from multiple RData files, calculates means and confidence intervals,
# and visualizes them as a grouped bar chart with error bars.
#
# Libraries required:
# - ggplot2 (for visualization)
# - extrafont (for font handling)
#
# Example usage:
# generate_hippocampus_plot(
#   file_paths = list("/path/to/BL_MRI_hippo_ENLR.Rdata", "/path/to/bl&long_MRI_hippo_ENLR.Rdata", 
#                     "/path/to/BL_Comb_hippo_ENLR.Rdata", "/path/to/bl&long_Comb_hippo_ENLR.Rdata"),
#   output_path = "/path/to/save/Hippocampus.png"
# )

library(ggplot2)
library(extrafont)

font_import() 
fonts()       

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
  
  for (i in seq_along(file_paths)) {
    load(file_paths[[i]])
    Pearson_values <- c(Pearson_values, mean(Pearson_R))
    CI_values[[i]] <- CI[,2]
  }
  
  R <- Pearson_values
  
  Group <- c(rep("MRI Only", length(R)/2), rep("MRI + Risk Factors", length(R)/2))
  Model <- c("Baseline", "Longitudinal", "Baseline", "Longitudinal")
  CI <- do.call(rbind, CI_values)
  
  df <- data.frame(Group, Model, R, CI)
  df$Group <- factor(df$Group, levels = c("MRI Only", "MRI + Risk Factors"))
  df$Model <- factor(df$Model, levels = c("Baseline", "Longitudinal"))
  
  p_values_df <- data.frame(
    Group = c("MRI Only", "MRI + Risk Factors"),  
    p_value = c(0.02, 0.03),                      
    y_position = c(0.65, 0.68)                   
  )
  
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
