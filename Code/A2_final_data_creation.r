## SYNTAX FILE 2 - FINAL DATA CREATION                       ##

cloudstor <- "C:/Users/z3312911/Cloudstor/" # change to master file path
.libPaths(paste0(cloudstor,"R Library"))

libs <- c("plyr","readr","parallel")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}

library ("plyr")
library ("readr")
library ("parallel")

set.seed(546432,kind="L'Ecuyer-CMRG")

load(file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/APSALS Imputed Data.RData"))

numcores <- future::availableCores()-1
cl <- makeCluster(numcores)
parallel::clusterEvalQ(cl, cloudstor <- "C:/Users/z3312911/Cloudstor/")
parallel::clusterEvalQ(cl, .libPaths(paste0(cloudstor,"R Library")))
parallel::clusterEvalQ(cl, library("plyr"))
parallel::clusterEvalQ(cl, library("readr"))
parallel::clusterEvalQ(cl, vnames <- c("c_age","homeacc","alcrules","alcmoney","singlep","smoking","cbclextn","cbcladn","cbclwdn","peeruse","peerdis","othsup","parsup"))

databinge6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$binge <- as.numeric(levels(x$binge))[x$binge]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"binge"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,99)]
})
save(databinge6,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/databinge6.RData"))
dataharms6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$anyharms <- as.numeric(levels(x$anyharms))[x$anyharms]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"anyharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,99)]
})
save(dataharms6,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/dataharms6.RData"))
dataabuse6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-30,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$abusediag <- as.numeric(levels(x$abusediag))[x$abusediag]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"abusediag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,99)]
})
save(dataabuse6,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/dataabuse6.RData"))
datadepend6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$dependdiag <- as.numeric(levels(x$dependdiag))[x$dependdiag]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"dependdiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,99)]
})
save(datadepend6,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/datadepend6.RData"))
dataaud6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-30)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$auddiag <- as.numeric(levels(x$auddiag))[x$auddiag]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"auddiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,99)]
})
save(dataaud6,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/dataaud6.RData"))

databinge7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$binge <- as.numeric(levels(x$binge))[x$binge]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"binge"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,113)]
})
save(databinge7,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/databinge7.RData"))
dataharms7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$anyharms <- as.numeric(levels(x$anyharms))[x$anyharms]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"anyharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,113)]
})
save(dataharms7,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/dataharms7.RData"))
dataabuse7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-30,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$abusediag <- as.numeric(levels(x$abusediag))[x$abusediag]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"abusediag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,113)]
})
save(dataabuse7,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/dataabuse7.RData"))
datadepend7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-31)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$dependdiag <- as.numeric(levels(x$dependdiag))[x$dependdiag]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"dependdiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,113)]
})
save(datadepend7,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/datadepend7.RData"))
dataaud7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-30)]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$auddiag <- as.numeric(levels(x$auddiag))[x$auddiag]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"auddiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:15,17:28,31:42,45:56,59:70,73:84,113)]
})
save(dataaud7,file=paste0(cloudstor,"PhD/Paper 7 - APSALs application/Analysis Data/dataaud7.RData"))