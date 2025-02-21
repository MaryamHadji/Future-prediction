# R script for identifying CN and MCI converters based on longitudinal data
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 21-Feb-2025
#
# Description:
# This function identifies stable and converting individuals in the CN and MCI groups
# based on their longitudinal diagnosis data. It categorizes subjects into stable, 
# converter, and unsure converter groups with at least 5 years of follow-up.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: readr
#
# Usage:
# - Source this script in R or RStudio
# - Run using: identify_converters(group = 'CN' or 'MCI', data_path, adnimerge_path, output_path)
#
# Parameters:
# - group: 'CN' or 'MCI' to specify the analysis group
# - data_path: Path to the longitudinal RData file
# - adnimerge_path: Path to the ADNIMERGE CSV file
# - output_path: Directory path to save the generated RData file
#
# Returned values:
# - Saves an RData file with the identified converters and stables to the specified directory

library(readr)

identify_converters <- function(group = 'CN', data_path = 'path/to/TotalData_bl_24_48.Rdata',
                                adnimerge_path = 'path/to/ADNIMERGE.csv',
                                output_path = 'path/to/RIDs_label_converters_stables.Rdata') {
  
  # Load longitudinal data
  load(data_path)
  maryam_RIDs <- data_bl_24_48$RID
  
  # Load ADNIMERGE data
  adnimerge <- read_csv(adnimerge_path)
  data_bl <- subset(adnimerge, VISCODE == 'bl')
  data_24 <- subset(adnimerge, VISCODE == 'm24')
  
  commrid <- intersect(data_bl$RID, data_24$RID)
  commrid2 <- setdiff(commrid, maryam_RIDs)
  
  # Filter based on group (CN or MCI)
  data_bl <- subset(data_24, RID %in% commrid2 & DX == group)
  data_long <- subset(adnimerge, RID %in% data_bl$RID & !VISCODE %in% c('bl','m06','m12','m18','m24'))
  data_long <- data_long[which(!is.na(data_long$DX)),]
  
  rid_bl <- unique(data_bl$RID)
  rid_long <- unique(data_long$RID)
  rid_bl_long <- intersect(rid_bl, rid_long)
  
  stable <- matrix(0, nrow = 0, ncol = 3)
  converter <- matrix(0, nrow = 0, ncol = 3)
  converter_unsure <- matrix(0, nrow = 0, ncol = 3)
  
  for (rid in rid_bl_long) {
    dd <- subset(data_long, RID == rid)
    time <- as.numeric(substring(dd$VISCODE, 2))
    i1 <- sort(time, index.return = TRUE)
    dd <- dd[i1$ix,]
    DX <- dd$DX
    ind <- which(!DX == 'CN')
    if (length(ind) == 0) {
      stable <- rbind(stable, c(rid, max(time), 0))
    } else if (length(ind) == 1 & ind[1] == length(DX)) {
      converter_unsure <- rbind(converter_unsure, c(rid, max(time), time[ind[1]]))
    } else if (length(ind) > 1 & DX[(length(DX) - 1)] %in% c('Dementia','MCI') &
               DX[length(DX)] %in% c('Dementia','MCI')) {
      converter <- rbind(converter, c(rid, max(time), time[ind[1]]))
    }
  }
  
  colnames(converter) <- colnames(converter_unsure) <- colnames(stable) <-
    c('RID', 'max follow up', 'Conversion time')
  
  # Filter stables with at least 60 months follow-up
  stable <- stable[as.numeric(stable[,'max follow up']) >= 60,]
  
  data <- rbind(stable, converter, converter_unsure)
  RID <- data[,'RID']
  Label <- c(rep(paste0(group, '_stable'), nrow(stable)),
             rep(paste0(group, '_converter'), nrow(converter)),
             rep(paste0(group, '_converter'), nrow(converter_unsure)))
  
  rid_label <- data.frame(RID, Label)
  
  # Save the output
  save(rid_label, file = output_path)
}
