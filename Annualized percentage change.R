

Change <- function(MRI_24, MRI_48) {
  hippo_24 <- rowMeans(MRI_24[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  hippo_48 <- rowMeans(MRI_48[, c("Right Hippocampus", "Left Hippocampus")], na.rm = TRUE)
  
  Annualized_percentage_change <- ((hippo_24 - hippo_48) / hippo_24) * 100 / 2
  
  return(Annualized_percentage_change)
}
