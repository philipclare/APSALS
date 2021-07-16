##############################################################################
##   
## APSALS Covid-19 Survey
## Data cleaning
## Date: 15 June 2020
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify paths to Katana/PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  path <- "/home/z3312911/COVID/"
} else { # Manually defined for PC
  path <- "C:/Users/pjclare/Desktop/APSALS/COVID Survey/Data/"
  .libPaths("C:/Users/pjclare/Documents/My Dropbox/R Library") # R library path
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","mice","miceadds","parallel","dplyr","labelled","VIM","semTools")
install <- !libs %in% installed.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

# 1.3 Define arguments passed to R from Katana
iteration <- commandArgs(trailingOnly = TRUE)

# 1.4 Set seed
load(paste0(path,"seeds.RData"))
set.seed(eval(as.name("seeds"))[as.numeric(iteration[1])])

##############################################################################
# 2. Load and merge data files
#-----------------------------------------------------------------------------

# 2.1 Load COVID data
load(file=paste0(path,"Data for imputation.RData"))

# Sort from most to least missing
res<-summary(aggr(merged_data))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
merged_data <- merged_data[,res$Variable]

##############################################################################
# 3. Define Imputation Paramaters
#-----------------------------------------------------------------------------

m <- 1 # Number of imputations
maxit <- 10; # Number of mice iterations
if (Sys.getenv("NCPUS")!="") {
  numcores <- as.numeric(Sys.getenv("NCPUS")) # Number of cores to use
  cl.type = "FORK"
} else { # Manually defined for PC
  numcores <- 4 
  cl.type = "PSOCK"
}
n.imp.core = m/numcores # number of imputations per core

##############################################################################
# 4. Imputation
#-----------------------------------------------------------------------------

# 4.1 Parallel imputation of temp data using parLapply
start_time <- Sys.time()
imp_data <- mice(data=merged_data,
       m=n.imp.core,
       maxit=maxit,
       defaultMethod = c("cart","logreg","cart","cart"))

end_time <- Sys.time()

# 4.2 Calculate Time
time_taken <- end_time - start_time

cat('Iteration ', iteration[1], 'with ', maxit, 'iterations took:', time_taken, attr(time_taken,"units"), ".","\n")

##############################################################################
# 5. Save results
#-----------------------------------------------------------------------------

# 5.1 Save results as RData
save(imp_data,file=paste0(path,"Output/Imputed data ",iteration[1],".RData"))