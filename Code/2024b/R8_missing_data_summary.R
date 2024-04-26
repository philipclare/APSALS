##############################################################################
##   
## APSALS Wave 11 Initiation Trajectories Paper
## Missing data plots
## Author: Philip J Clare
## Date: 19 June 2023
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## Analysis pre-registered at: DOI:10.17605/OSF.IO/BRDUV
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify paths to Katana/PC paths based on whether NCPUS is detected
workdir <- "D:/UNSW/APSALS - Documents/Papers/PIP46. Effect of age of initiation on trajectory of alcohol use and harm/"

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dplyr","naniar","VIM")
install <- !libs %in% installed.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load and strip data
#-----------------------------------------------------------------------------

dataread <- read_dta(file=paste0(workdir,"Data/initiation_analysis_data.dta"))

dataread <- zap_formats(dataread)
dataread <- zap_label(dataread)
dataread <- zap_labels(dataread)

dataread$zzC_ID <- factor(dataread$zzC_ID)

##############################################################################
# 3. Generate missing data plots
#-----------------------------------------------------------------------------

res<-summary(aggr(dataread))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataread <- dataread[,res$Variable]

aggr(dataread,
     cex.axis=0.7)
