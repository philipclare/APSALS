
## SYNTAX FILE 10                                            ##
## SENSITIVITY ANALYSIS WITH CONTINUOUS OUTCOMES             ##

cloudstor <- "C:/Users/z3312911/Cloudstor/"
dropbox <- "C:/Users/z3312911/Dropbox/"
.libPaths(paste0(dropbox,"R Library"))

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
parallel::clusterEvalQ(cl, library("readr"))
parallel::clusterEvalQ(cl, library("ltmle"))
parallel::clusterEvalQ(cl, library("SuperLearner"))
parallel::clusterEvalQ(cl, library("gam"))
parallel::clusterEvalQ(cl, library("ranger"))
parallel::clusterEvalQ(cl, library("dplyr"))
parallel::clusterEvalQ(cl, vnames <- c("c_age","hhavguse","parmon","homeacc","alcrules","alcmoney","singlep","smoking","cbclextn","cbcladn","cbclwdn","peeruse","peerdis","othsup","parsup"))
parallel::clusterEvalQ(cl, Lvars <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2",
                                           "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3",
                                           "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4",
                                           "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5"))
parallel::clusterEvalQ(cl, create.Learner("SL.ranger", params = list(num.trees = 250)))
parallel::clusterEvalQ(cl, SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
                                         g=c("SL.mean","SL.glm","SL.gam","SL.ranger_1")))

imp <- parLapply(cl,imp, function (x) {
  x$alcmoney <- as.numeric(levels(x$alcmoney))[x$alcmoney]
  x$singlep <- as.numeric(levels(x$singlep))[x$singlep]
  x$smoking <- as.numeric(levels(x$smoking))[x$smoking]
  x$othsup <- as.numeric(levels(x$othsup))[x$othsup]
  x$parsup <- as.numeric(levels(x$parsup))[x$parsup]
  x$bingef <- as.numeric(levels(x$bingef))[x$bingef]
  x$numharms <- as.numeric(levels(x$numharms))[x$numharms]
  x
})

databinge6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"bingef"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})

jtbinge6 <- parLapply(cl=cl, databinge6, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="bingef6",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtbingeres6 <- parLapply(cl=cl, jtbinge6, function (x) {summary(x)})
jtbingeco6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {log(x$effect.measures$ATE$estimate)})),nrow=50,ncol=1)
jtbingese6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {x$effect.measures$ATE$std.dev})),nrow=50,ncol=1)
jtbingeres6 <- matrix(unlist(mi.meld(q=jtbingeco6, se=jtbingese6)),nrow=1,ncol=2)

rm(list=c("databinge6","jtbinge6","jtbingeco6","jtbingese6"))

databinge7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"bingef"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})

jtbinge7 <- parLapply(cl=cl, databinge7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="bingef7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtbingeres7 <- parLapply(cl=cl, jtbinge7, function (x) {summary(x)})
jtbingeco7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {log(x$effect.measures$ATE$estimate)})),nrow=50,ncol=1)
jtbingese7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {x$effect.measures$ATE$std.dev})),nrow=50,ncol=1)
jtbingeres7 <- matrix(unlist(mi.meld(q=jtbingeco7, se=jtbingese7)),nrow=1,ncol=2)

rm(list=c("databinge7","jtbinge7","jtbingeco7","jtbingese7"))

dataharms6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"numharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,108)]
})

jtharms6 <- parLapply(cl=cl, dataharms6, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="numharms6",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtharmsres6 <- parLapply(cl=cl, jtharms6, function (x) {summary(x)})
jtharmsco6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {log(x$effect.measures$ATE$estimate)})),nrow=50,ncol=1)
jtharmsse6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {x$effect.measures$ATE$std.dev})),nrow=50,ncol=1)
jtharmsres6 <- matrix(unlist(mi.meld(q=jtharmsco6, se=jtharmsse6)),nrow=1,ncol=2)

rm(list=c("dataharms6","jtharms6","jtharmsco6","jtharmsse6"))

dataharms7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"numharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:27,29:43,45:59,61:75,77:91,124)]
})

jtharms7 <- parLapply(cl=cl, dataharms7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="numharms7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtharmsres7 <- parLapply(cl=cl, jtharms7, function (x) {summary(x)})
jtharmsco7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {log(x$effect.measures$ATE$estimate)})),nrow=50,ncol=1)
jtharmsse7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {x$effect.measures$ATE$std.dev})),nrow=50,ncol=1)
jtharmsres7 <- matrix(unlist(mi.meld(q=jtharmsco7, se=jtharmsse7)),nrow=1,ncol=2)

rm(list=c("dataharms7","jtharms7","jtharmsco7","jtharmsse7"))

## COMBINE RESULTS INTO MATRIX FOR EXCEL ##
jtresultss4 <- matrix(c(jtbingeres6,jtbingeres7,jtharmsres6,jtharmsres7),byrow=TRUE,ncol=4,nrow=2)
rownames(jtresultss4) <- c("Number of binge drinking occasions","Number of harms")
colnames(jtresultss4) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")

save(jtresultss4,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Results/jtresults S4 20191028.RData"))