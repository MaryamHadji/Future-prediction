# R script for identifying stable, converter, and unsure MCI subjects based on longitudinal ADNI data
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 21-Feb-2025
#
# Description:
# This function loads baseline and longitudinal ADNI data, identifies stable MCI subjects,
# converters to dementia, and those with uncertain conversion status. The results are saved
# as an RData file for further analysis.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: readr
#
# Usage:
# - Source this script in R or RStudio
# - Run using: identify_mci_converters(bl_data_path, adnimerge_path, output_path)
#
# Parameters:
# - bl_data_path: Path to the baseline RData file (e.g., TotalData_bl_24_48.Rdata)
# - adnimerge_path: Path to the ADNI longitudinal data CSV file (e.g., ADNIMERGE_25Aug2023.csv)
# - output_path: File path to save the resulting RData file
#
# Returned values:
# - Saves 'stable', 'converter', 'converter_unsure', and 'rid_unstable' matrices in an RData file

identify_mci_converters <- function(bl_data_path, adnimerge_path, output_path) {
  library(readr)
  load(bl_data_path)
  RIDs <- data_bl_24_48$RID
  adnimerge <- read_csv(adnimerge_path)
  data_bl <- subset(adnimerge, RID %in% RIDs & VISCODE == 'm24' & DX == 'MCI')
  data_long <- subset(adnimerge, !VISCODE %in% c('bl', 'm06', 'm12', 'm18', 'm24') & !is.na(DX))
  rid_comm <- intersect(unique(data_long$RID), unique(data_bl$RID))
  data_long <- subset(data_long, RID %in% rid_comm & !is.na(DX))
  data_bl <- subset(data_bl, RID %in% rid_comm)
  data_long$VISCODE <- gsub('^bl$', 'm0', data_long$VISCODE)
  
  stable <- matrix(0, nrow = 0, ncol = 3)
  converter <- matrix(0, nrow = 0, ncol = 3)
  converter_unsure <- matrix(0, nrow = 0, ncol = 3)
  rid_unstable <- vector()
  
  for (i in seq_along(rid_comm)) {
    rid <- rid_comm[i]
    dd <- subset(data_long, RID == rid)
    time <- as.numeric(substring(dd$VISCODE, 2))
    i1 <- order(time)
    time <- time[i1]
    dd <- dd[i1,]
    DX <- dd$DX
    ind <- which(!DX %in% c('MCI'))
    
    if (length(ind) == 0) {
      stable <- rbind(stable, c(rid, max(time), 0))
    } else if (length(ind) == 1 & ind[1] == length(DX)) {
      converter_unsure <- rbind(converter_unsure, c(rid, max(time), time[ind[1]]))
    } else if (length(ind) > 1 & DX[(length(DX)-1)] == 'Dementia' & DX[length(DX)] == 'Dementia') {
      converter <- rbind(converter, c(rid, max(time), time[ind[1]]))
    } else {
      rid_unstable <- c(rid_unstable, rid)
    }
  }
  
  colnames(stable) <- colnames(converter) <- colnames(converter_unsure) <- c('RID', 'Max Follow Up', 'Conversion Time')
  
  save(stable, converter, rid_unstable, converter_unsure, file = output_path)
  
  load(output_path)
  ind <- which(stable[,'Max Follow Up'] >= 60)
  stable <- stable[ind,]
  data <- rbind(stable, converter, converter_unsure)
  RID <- data[,'RID']
  Label <- c(rep('MCI_stable', nrow(stable)), rep('MCI_converter', nrow(converter)), rep('MCI_converter', nrow(converter_unsure)))
  rid_label <- data.frame(RID, Label)
  save(rid_label, file = gsub('.Rdata$', '_RIDs_label_MCI_converters_stables_60.Rdata', output_path))
}