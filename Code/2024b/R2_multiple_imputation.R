##############################################################################
##   
## APSALS Wave 11 Initiation Trajectories Paper
## Impute data
## Author: Philip J Clare
## Date: 19 June 2023
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## Analysis pre-registered at: DOI:10.17605/OSF.IO/BRDUV
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify paths to Katana/PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  workdir <- "/home/z3312911/APSALSW11/"
} else { # Manually defined for PC
  workdir <- "/Users/pjclare/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/APSALS - Documents/Papers/PIP46. Effect of age of initiation on trajectory of alcohol use and harm/"
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dplyr","haven","labelled","mice","miceadds","parallel","semTools","VIM")
install <- !libs %in% installed.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

# 1.3 Define arguments passed to R from Katana
iteration <- as.numeric(commandArgs(trailingOnly = TRUE))
# iteration <- 1 # For testing on local computer only

# 1.4 Set seed
set.seed(395702)
seeds <- sample.int(100000, 20)
set.seed(seeds[iteration])

##############################################################################
# 2. Load and merge data files
#-----------------------------------------------------------------------------

# 2.1 Load COVID data
dataread <- read_dta(file=paste0(workdir,"Data/initiation_analysis_data.dta"))
dataread <- zap_formats(dataread)
dataread <- zap_label(dataread)
dataread <- zap_labels(dataread)

dataread$zzC_ID <- factor(dataread$zzC_ID)

# 2.2. Get % of missing information
table(is.na(dataread))[2]/(table(is.na(dataread))[1]+table(is.na(dataread))[2])*100

# 2.3. Sort from most to least missing
res<-summary(aggr(dataread))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
dataread <- dataread[,res$Variable]

##############################################################################
# 3. Define Imputation Paramaters
#-----------------------------------------------------------------------------

m <- 1 # Number of imputations, set to one for Katana so each node runs a single M
maxit <- 50; # Number of mice iterations

##############################################################################
# 4. Imputation
#-----------------------------------------------------------------------------

# 4.1 MI using mice with random forests
start_time <- Sys.time()
imputation <- mice(data=dataread,
                 m=m,
                 maxit=maxit,
                 defaultMethod = c("rf","rf","rf","rf"))

end_time <- Sys.time()

# 4.2 Extract imputation as data frame and return to original variable order
imp_data <- mids2datlist(imputation)[[1]]
imp_data <- imp_data[,varorder]

imp_data$imp <- iteration[1]

# 4.3 Calculate and report time taken
time_taken <- end_time - start_time

cat('Imputation ', iteration[1], 'with ', maxit, 'iterations took:', time_taken, attr(time_taken,"units"), ".","\n")

##############################################################################
# 5. Save results
#-----------------------------------------------------------------------------

# 5.1 Save results as RData
write_dta(imp_data,path=paste0(workdir,"Output/Initiation imputed data ",iteration[1],".dta"))