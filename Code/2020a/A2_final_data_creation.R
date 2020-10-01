
## SYNTAX FILE 2                                             ##
## CREATION OF IMPUTED DATA SUBSETS FOR EACH OUTCOME         ##

cloudstor <- "C:/Users/z3312911/Cloudstor/"
dropbox <- "C:/Users/z3312911/Dropbox/"
.libPaths(paste0(dropbox,"R Library"))

readdate <- "20181028"
writedate <- format(Sys.Date(), "%Y%m%d")

libs <- c("mitml","pan","haven","plyr","readr","ltmle","SuperLearner","Amelia")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
library ("mitml")
library ("pan")
library ("haven")
library ("plyr")
library ("readr")
library ("parallel")
library ("dplyr")

set.seed(546432,kind="L'Ecuyer-CMRG")

load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/APSALS Imputed Data 20191028.RData"))

numcores <- future::availableCores()-2
cl <- makeCluster(numcores)
parallel::clusterEvalQ(cl, cloudstor <- "C:/Users/z3312911/Cloudstor/")
parallel::clusterEvalQ(cl, dropbox <- "C:/Users/z3312911/Dropbox/")
parallel::clusterEvalQ(cl, .libPaths(paste0(dropbox,"R Library")))
parallel::clusterEvalQ(cl, library("plyr"))
parallel::clusterEvalQ(cl, library("dplyr"))
parallel::clusterEvalQ(cl, library("readr"))
parallel::clusterEvalQ(cl, library("parallel"))
parallel::clusterEvalQ(cl, vnames <- c("c_age","hhavguse","parmon","homeacc","alcrules","alcmoney","singlep","smoking","cbclextn","cbcladn","cbclwdn","peeruse","peerdis","othsup","parsup"))

imp <- parLapply(cl,imp, function (x) {
  x$bingef <- as.numeric(levels(x$bingef))[x$bingef]
  x$bingef <- ifelse(x$bingef>0,1,0)
  x$numharms <- as.numeric(levels(x$numharms))[x$numharms]
  x$numharms <- ifelse(x$numharms>0,1,0)

  x$abusediag <- as.numeric(levels(x$abusediag))[x$abusediag]
  x$dependdiag <- as.numeric(levels(x$dependdiag))[x$dependdiag]
  x$auddiag <- as.numeric(levels(x$auddiag))[x$auddiag]
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x %>% 
    rename(
      binge = bingef,
      anyharms = numharms
    )
})

databinge6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"binge"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})
save(databinge6,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/databinge6 20191028.RData"))
dataharms6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"anyharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})
save(dataharms6,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataharms6 20191028.RData"))
dataabuse6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"abusediag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})
save(dataabuse6,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataabuse6 20191028.RData"))
datadepend6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"dependdiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})
save(datadepend6,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/datadepend6 20191028.RData"))
dataaud6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-30)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"auddiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})
save(dataaud6,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataaud6 20191028.RData"))

databinge7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"binge"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})
save(databinge7,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/databinge7 20191028.RData"))
dataharms7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"anyharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})
save(dataharms7,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataharms7 20191028.RData"))
dataabuse7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"abusediag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})
save(dataabuse7,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataabuse7 20191028.RData"))
datadepend7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"dependdiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})
save(datadepend7,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/datadepend7 20191028.RData"))
dataaud7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-30)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"auddiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})
save(dataaud7,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataaud7 20191028.RData"))

