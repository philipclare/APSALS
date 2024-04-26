##############################################################################
# PROGRAM: S2a-suicide-imputation-mice.R
# PURPOSE: Suicide Paper Imputation Using mice in parallel on Katana
# WRITTEN BY: Philip Clare & Phillip Hungerford
# DATE: 03/02/2021
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------
# 1.0 Time Imputation
start_time <- Sys.time()

# 1.1 Specify paths
# Windows
.libPaths("/Users/pjclare/Dropbox (Sydney Uni)/R Library Windows")
workdir <- "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP43. Role of peers on smoking and ecigarette use/"

# # Katana
# .libPaths("/home/z3312911/RPackages")
# workdir <- "/home/z3312911/hrs-imputation/"

# 1.2 Check install and load required libraries
libs <- c("randomForest","mice","miceadds","haven","rpart","VIM","semTools")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}

library(parallel)
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2.Load data
#-----------------------------------------------------------------------------
datalong <- read_dta(file=paste0(workdir,"Data/Vaping Analysis Data 20211021.dta"))
# save(datalong,file=paste0(workdir,"HRS Data Clean.RData"))
# load(file=paste0(workdir,"Data/seeds.RData"))
# set.seed(eval(as.name("seeds"))[as.numeric(args[1])])

datalong <- zap_formats(datalong)
datalong <- zap_labels(datalong)
datalong <- zap_label(datalong)


# datalong$zzC_ID <- factor(datalong$zzC_ID)
datalong$zzwave <- factor(datalong$zzwave)
datalong$b_parempl <- factor(datalong$b_parempl)
datalong$b_seifa <- factor(datalong$b_seifa)
datalong$b_hinc <- factor(datalong$b_hinc)

datalong$b_famposi <- ordered(datalong$b_famposi)
datalong$b_famconf <- ordered(datalong$b_famconf)

res<-summary(aggr(datalong))$missings
varorder <- res$Variable
res<-res[order(-res$Count),]
datalong <- datalong[,res$Variable]

##############################################################################
# 3. Check missingness
#-----------------------------------------------------------------------------

miss.case <- table(!complete.cases(datalong))
prop.table(miss.case)
miss.cell <- table(is.na(datalong))
prop.table(miss.cell)

##############################################################################
# 4. Define Imputation Parameters
#-----------------------------------------------------------------------------

m <- 30 # Number of imputations
maxit <- 50; # Number of mice iterations
set.seed(58646) # Not the seed used by parlmice, but needs to be set or parlmice fails
cluster.seed <- 32181 # Needs to be set within function for parallel computing
numcores <- 6 #as.numeric(Sys.getenv('NCPUS')) # Number of cores to use (default = ncore-1)
n.imp.core <- m/numcores # number of imputations per core (default = 2)
cl.type <- "PSOCK" # Can be PSOCK on Windows or FORK for *NIX based systems
method <- c("cart","rf","rf","rf")

##############################################################################
# 5. Imputation
#-----------------------------------------------------------------------------
start_time <- Sys.time()
# 5.1 Parallel imputation using parlmice
imp_mice <- parlmice(n.imp.core=n.imp.core,
                     data=datalong,
                     maxit=maxit,
                     n.core=numcores,
                     cl.type=cl.type,
                     defaultMethod=method)
# imp_mice <- mice(m=30,
#                  data=datalong,
#                  maxit=maxit,
#                  defaultMethod=method)

imp_mice <- mids2datlist(imp_mice)

datalong$imp <- 0
imp_stata <- rbind(do.call(rbind,lapply(seq(1,30), function (x) {
  imp_mice[[x]]$imp <- x
  imp_mice[[x]]
})),datalong)

##############################################################################
# 6. Save
# 6.1 Save imputation
save(imp_mice, file=paste0(workdir,"Data/imputed-mice.RData"))
write_dta(imp_stata, path=paste0(workdir,"Data/imputed-mice.dta"))

# 6.2 Calculate Time
end_time <- Sys.time()
time_taken_mice <- end_time - start_time

cat('Using mice, ', m, 'imputations with ', maxit, 'iterations took:', time_taken_mice, attr(time_taken_mice,"units"), ".","\n")

##############################################################################
################################### END ######################################
##############################################################################