##############################################################################
##   
## APSALS Covid-19 Survey
## Finalise data after imputation on Katana
## Date: 6 August 2020
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify paths to Katana/PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  path <- "/home/z3312911/COVID/"
} else { # Manually defined for PC
  path <- "C:/Users/z3312911/Cloudstor/APSALS/COVID Survey/Data/"
  .libPaths("C:/Users/z3312911/Dropbox/R Library") # R library path
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","mice","miceadds","parallel","dplyr","labelled","VIM","semTools","reshape2")
install <- !libs %in% installed.packages() #| libs %in% old.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load and merge data files
#-----------------------------------------------------------------------------

# 2.1 Load original data and get order
load(file=paste0(path,"Mental health data for imputation.RData"))

varorder<-summary(aggr(mh_data))$missings$Variable
varorder[41:42] <- c("ccalcqnt_11","ccalcqnt_12")

mh_data$ccalcqnt_11 <- case_when(mh_data$ccalcq_11 >= 13 ~ 13,
                                 mh_data$ccalcq_11 >= 11 ~ 11.5,
                                 mh_data$ccalcq_11 >= 7 ~ 8.5,
                                 mh_data$ccalcq_11 >= 5 ~ 5.5,
                                 mh_data$ccalcq_11 >= 3 ~ 3.5,
                                 mh_data$ccalcq_11 >= 1 ~ 1.5,
                                 mh_data$ccalcq_11 > 0 ~ 0.1,
                                 mh_data$ccalcq_11 == 0 ~ 0)
mh_data$ccalcqnt_12 <- case_when(mh_data$ccalcq_12 >= 13 ~ 13,
                                 mh_data$ccalcq_12 >= 11 ~ 11.5,
                                 mh_data$ccalcq_12 >= 7 ~ 8.5,
                                 mh_data$ccalcq_12 >= 5 ~ 5.5,
                                 mh_data$ccalcq_12 >= 3 ~ 3.5,
                                 mh_data$ccalcq_12 >= 1 ~ 1.5,
                                 mh_data$ccalcq_12 > 0 ~ 0.1,
                                 mh_data$ccalcq_12 == 0 ~ 0)
mh_data$datecom_10 <- as.Date(mh_data$datecom_10, origin = "1970-01-01")
mh_data$datecom_12 <- as.Date(mh_data$datecom_12, origin = "1970-01-01")
mh_data <- mh_data[,varorder]

# 2.2 Load and combine imputed data into single list
list <- seq(1,50)
imputed_mh <- lapply(list, function(x) {
  load(file=paste0(path,"Imputed split/Mental health/Imputed data mh ",x,".RData"))
  dat <- mids2datlist(imp_data)[[1]]
  dat$ccalcqnt_11 <- case_when(dat$ccalcq_11 >= 13 ~ 13,
                               dat$ccalcq_11 >= 11 ~ 11.5,
                               dat$ccalcq_11 >= 7 ~ 8.5,
                               dat$ccalcq_11 >= 5 ~ 5.5,
                               dat$ccalcq_11 >= 3 ~ 3.5,
                               dat$ccalcq_11 >= 1 ~ 1.5,
                               dat$ccalcq_11 > 0 ~ 0.1,
                               dat$ccalcq_11 == 0 ~ 0)
  dat$ccalcqnt_12 <- case_when(dat$ccalcq_12 >= 13 ~ 13,
                               dat$ccalcq_12 >= 11 ~ 11.5,
                               dat$ccalcq_12 >= 7 ~ 8.5,
                               dat$ccalcq_12 >= 5 ~ 5.5,
                               dat$ccalcq_12 >= 3 ~ 3.5,
                               dat$ccalcq_12 >= 1 ~ 1.5,
                               dat$ccalcq_12 > 0 ~ 0.1,
                               dat$ccalcq_12 == 0 ~ 0)
  dat$datecom_10 <- as.Date(dat$datecom_10, origin = "1970-01-01")
  dat$datecom_12 <- as.Date(dat$datecom_12, origin = "1970-01-01")
  dat <- dat[,varorder]
  dat
})

imputed_mh[[51]] <- mh_data
save(imputed_mh,file=paste0(path,"Mental health imputed data 20200922.RData"))
imputed_mh_df <- do.call("rbind",lapply(seq(1,51), function(x,imputed_mh) {
  y <- imputed_mh[[x]]
  y$imp <- x
  y
},imputed_mh=imputed_mh))
write_dta(imputed_mh_df,path=paste0(path,"Mental health imputed data 20200922.dta"))