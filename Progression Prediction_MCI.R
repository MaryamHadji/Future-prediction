# R script for identifying MCI converters based on longitudinal data
# Author: Maryam Hadji, University of Eastern Finland, Kuopio, Finland (maryamh@uef.fi)
# Last updated: 21-Feb-2025
#
# Description:
# This script identifies stable and converting individuals in the MCI group
# based on their longitudinal diagnosis data. It categorizes subjects into stable, 
# converter, and unsure converter groups with at least 5 years of follow-up.
#
# Requirements:
# - R version 4.3.1 or later
# - Required Libraries: readr

library(readr)

# Load longitudinal data
load('path/to/TotalData_bl_24_48.Rdata')
maryam_RIDs <- data_bl_24_48$RID

# Load ADNIMERGE data
adnimerge <- read_csv('path/to/ADNIMERGE_25Aug2023.csv')

data_bl <- subset(adnimerge, VISCODE == 'bl')
data_24 <- subset(adnimerge, VISCODE == 'm24')

commrid <- intersect(data_bl$RID, data_24$RID)
commrid2 <- setdiff(commrid, maryam_RIDs)

# Filter for MCI group
data_bl <- subset(data_24, RID %in% commrid2 & DX == 'MCI')
data_long <- subset(adnimerge, RID %in% data_bl$RID & !VISCODE %in% c('bl','m06','m12','m18','m24'))
data_long <- data_long[which(!is.na(data_long$DX)),]

rid_bl <- unique(data_bl$RID)
rid_long <- unique(data_long$RID)
rid_bl_long <- intersect(rid_bl, rid_long)
rid_bl_only <- setdiff(rid_bl, rid_bl_long)
rid_long_only <- setdiff(rid_long, rid_bl_long)

stable <- matrix(0, nrow = 0, ncol = 3)
converter <- matrix(0, nrow = 0, ncol = 3)
converter_unsure <- matrix(0, nrow = 0, ncol = 3)
rid_unstable <- vector()
rid_comm <- rid_bl_long

for (i in 1:length(rid_comm)) {
  print(i)
  rid <- rid_comm[i]
  dd <- subset(data_long, RID == rid)
  time <- as.numeric(substring(dd$VISCODE, 2))
  i1 <- sort(time, index.return = TRUE)
  time <- time[i1$ix]
  dd <- dd[i1$ix,]
  DX <- dd$DX
  ind <- which(!DX %in% c('CN','MCI'))
  
  if (length(ind) == 0) {
    stable <- rbind(stable, c(rid, max(time), 0))
  } else if (length(ind) == 1 & ind[1] == length(DX)) {
    converter_unsure <- rbind(converter_unsure, c(rid, max(time), time[ind[1]]))
  } else if (length(ind) > 1 & DX[(length(DX) - 1)] == 'Dementia' & DX[length(DX)] == 'Dementia') {
    converter <- rbind(converter, c(rid, max(time), time[ind[1]]))
  } else {
    rid_unstable <- c(rid_unstable, rid)
  }
}

colnames(converter) <- colnames(converter_unsure) <- colnames(stable) <-
  c('RID', 'max follow up', 'Conversion time')

# Filter stables with at least 60 months follow-up
stable <- stable[as.numeric(stable[,'max follow up']) >= 60,]

data <- rbind(stable, converter, converter_unsure)
RID <- data[,'RID']
Label <- c(rep('MCI_stable', nrow(stable)),
           rep('MCI_converter', nrow(converter)),
           rep('MCI_converter', nrow(converter_unsure)))

rid_label <- data.frame(RID, Label)

# Save the output
save(rid_label, file = 'path/to/RIDs_label_MCI_converters_stables_with5Y_followup.Rdata')