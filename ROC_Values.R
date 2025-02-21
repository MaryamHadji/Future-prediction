# R script for identifying stable, converter, and unsure MCI and CN subjects based on longitudinal ADNI data
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 21-Feb-2025
#
# Description:
# This function loads baseline and longitudinal ADNI data, identifies stable subjects,
# converters to dementia, and those with uncertain conversion status for both MCI and CN groups.
# The results are saved as RData files for further analysis.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: readr, pROC
#
# Usage:
# - Source this script in R or RStudio
# - Run using: run_roc_analysis(group = 'CN' or 'MCI', output_path = "path/to/output.Rdata")
#
# Parameters:
# - group: 'CN' or 'MCI' to specify the analysis group
# - output_path: File path to save the resulting ROC values
#
# Returned values:
# - Saves ROC and CI values for multiple models in an RData file

library(readr)
library(pROC)

run_roc_analysis <- function(group = 'CN', output_path = "path/to/output.Rdata") {
  # Load appropriate label data based on the group
  if (group == 'CN') {
    load('path/to/RIDs_label_CN_converters_stables_60.Rdata')
  } else if (group == 'MCI') {
    load('path/to/RIDs_label_MCI_converters_stables_60.Rdata')
  } else {
    stop('Invalid group. Please choose either "CN" or "MCI".')
  }
  
  roc_list <- list()
  ci_list <- list()
  
 
  model_paths <- list(
    'path/to/BL_MRI_hippo/ventricle/TGM_ENLR.Rdata',
    'path/to/BL_Comb_hippo/ventricle/TGM_ENLR.Rdata',
    'path/to/bl&long_MRI_hippo/ventricle/TGM_ENLR.Rdata',
    'path/to/bl&long_Comb_hippo/ventricle/TGM_ENLR.Rdata'
  )
  
  # Loop through each model and calculate ROC and CI
  for (model_idx in 1:length(model_paths)) {
    load(model_paths[[model_idx]])
    i <- sort(Pearson_R, index.return = TRUE)$ix[6]
    yhat <- yhat_pred[, i]
    names(yhat) <- names(Y)
    ind_i <- match(rid_label$RID, names(yhat))
    yhat_i <- yhat[ind_i]
    
    if (!identical(as.numeric(names(yhat_i)), as.numeric(rid_label$RID))) {
      stop('Error: Names and RIDs do not match for model ', model_paths[[model_idx]])
    }
    
    roc_list[[model_idx]] <- roc(rid_label$Label, yhat_i)
    ci_list[[model_idx]] <- ci.auc(roc_list[[model_idx]])
    print(roc_list[[model_idx]])
    print(ci_list[[model_idx]])
  }
  
  # Additional model for brain regions
  load('path/to/TotalData_bl_24_48.Rdata')
  hippo_24 <- rowMeans(roitot_24[, c('Right Hippocampus', 'Left Hippocampus')])
  yhat <- hippo_24
  ind_i <- match(rid_label$RID, names(yhat))
  yhat_i <- yhat[ind_i]
  
  roc_list[[5]] <- roc(rid_label$Label, yhat_i)
  ci_list[[5]] <- ci.auc(roc_list[[5]])
  print(roc_list[[5]])
  print(ci_list[[5]])
  
  save(list = c(roc_list, ci_list), file = output_path)
}
