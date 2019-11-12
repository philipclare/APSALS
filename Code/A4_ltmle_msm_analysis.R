
## SYNTAX FILE 4                                             ##
## PRIMARY LTMLE MSM ANALYSIS                                ##

cloudstor <- "C:/Users/z3312911/Cloudstor/"
dropbox <- "C:/Users/z3312911/Dropbox/"
.libPaths(paste0(dropbox,"R Library"))

readdate <- "20181028"
writedate <- format(Sys.Date(), "%Y%m%d")

libs <- c("mitml","pan","plyr","readr","ltmle","SuperLearner","Amelia","gam","ranger")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
library ("plyr")
library ("readr")
library ("Amelia")
library ("parallel")
library ("ltmle")
library ("SuperLearner")
library ("gam")
library ("ranger")

set.seed(546432,kind="L'Ecuyer-CMRG")

numcores <- future::availableCores()-1
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
parallel::clusterEvalQ(cl, Lvars <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2",
                                      "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3",
                                      "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4",
                                      "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5"))
parallel::clusterEvalQ(cl, regimeList <- list(function(row) c(1,1,1,1,1),
                                              function(row) c(0,1,1,1,1),
                                              function(row) c(0,0,1,1,1),
                                              function(row) c(0,0,0,1,1),
                                              function(row) c(0,0,0,0,1),
                                              function(row) c(0,0,0,0,0)))
parallel::clusterEvalQ(cl, sum.measures <- as.array(matrix(c(5,4,3,2,1,0),nrow=6,ncol=1)))
parallel::clusterEvalQ(cl, colnames(sum.measures) <- "init")
parallel::clusterEvalQ(cl, create.Learner("SL.ranger", params = list(num.trees = 250)))
parallel::clusterEvalQ(cl, SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
                                         g=c("SL.mean","SL.glm","SL.gam","SL.ranger_1")))
## BINGE DRINKING ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/databinge6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/databinge7 20191028.RData"))

jtbingeMSM6 <- parLapply(cl=cl, databinge6, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="binge6",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtbingeMSMre6 <- parLapply(cl=cl, jtbingeMSM6, function (x) {summary(x)})
jtbingeMSMco6 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMhi6 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMlo6 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMres6 <- matrix(colMeans(cbind(jtbingeMSMco6,jtbingeMSMlo6,jtbingeMSMhi6)),nrow=1,ncol=3)

jtbingeMSM7 <- parLapply(cl=cl, databinge7, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="binge7",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtbingeMSMre7 <- parLapply(cl=cl, jtbingeMSM7, function (x) {summary(x)})
jtbingeMSMco7 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMhi7 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMlo7 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMres7 <- matrix(colMeans(cbind(jtbingeMSMco7,jtbingeMSMlo7,jtbingeMSMhi7)),nrow=1,ncol=3)

rm(list=c("databinge6","jtbingeMSM6","jtbingeMSMco6","jtbingeMSMlo6","jtbingeMSMhi6",
          "databinge7","jtbingeMSM7","jtbingeMSMco7","jtbingeMSMlo7","jtbingeMSMhi7"))

## HARMS ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataharms6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataharms7 20191028.RData"))

jtharmsMSM6 <- parLapply(cl=cl, dataharms6, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="anyharms6",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtharmsMSMre6 <- parLapply(cl=cl, jtharmsMSM6, function (x) {summary(x)})
jtharmsMSMco6 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMhi6 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMlo6 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMres6 <- matrix(colMeans(cbind(jtharmsMSMco6,jtharmsMSMlo6,jtharmsMSMhi6)),nrow=1,ncol=3)

jtharmsMSM7 <- parLapply(cl=cl, dataharms7, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="anyharms7",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtharmsMSMre7 <- parLapply(cl=cl, jtharmsMSM7, function (x) {summary(x)})
jtharmsMSMco7 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMhi7 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMlo7 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMres7 <- matrix(colMeans(cbind(jtharmsMSMco7,jtharmsMSMlo7,jtharmsMSMhi7)),nrow=1,ncol=3)

rm(list=c("dataharms6","jtharmsMSM6","jtharmsMSMco6","jtharmsMSMlo6","jtharmsMSMhi6",
          "dataharms7","jtharmsMSM7","jtharmsMSMco7","jtharmsMSMlo7","jtharmsMSMhi7"))

## DSM-IV ABUSE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataabuse6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataabuse7 20191028.RData"))

jtabuseMSM6 <- parLapply(cl=cl, dataabuse6, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="abusediag6",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtabuseMSMre6 <- parLapply(cl=cl, jtabuseMSM6, function (x) {summary(x)})
jtabuseMSMco6 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMhi6 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMlo6 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMres6 <- matrix(colMeans(cbind(jtabuseMSMco6,jtabuseMSMlo6,jtabuseMSMhi6)),nrow=1,ncol=3)

jtabuseMSM7 <- parLapply(cl=cl, dataabuse7, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="abusediag7",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtabuseMSMre7 <- parLapply(cl=cl, jtabuseMSM7, function (x) {summary(x)})
jtabuseMSMco7 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMhi7 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMlo7 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMres7 <- matrix(colMeans(cbind(jtabuseMSMco7,jtabuseMSMlo7,jtabuseMSMhi7)),nrow=1,ncol=3)

rm(list=c("dataabuse6","jtabuseMSM6","jtabuseMSMco6","jtabuseMSMlo6","jtabuseMSMhi6",
          "dataabuse7","jtabuseMSM7","jtabuseMSMco7","jtabuseMSMlo7","jtabuseMSMhi7"))

## DSM-IV DEPENDENCE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/datadepend6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/datadepend7 20191028.RData"))

jtdependMSM6 <- parLapply(cl=cl, datadepend6, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="dependdiag6",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtdependMSMre6 <- parLapply(cl=cl, jtdependMSM6, function (x) {summary(x)})
jtdependMSMco6 <- matrix(unlist(parLapply(cl=cl, jtdependMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMhi6 <- matrix(unlist(parLapply(cl=cl, jtdependMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMlo6 <- matrix(unlist(parLapply(cl=cl, jtdependMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMres6 <- matrix(colMeans(cbind(jtdependMSMco6,jtdependMSMlo6,jtdependMSMhi6)),nrow=1,ncol=3)

jtdependMSM7 <- parLapply(cl=cl, datadepend7, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="dependdiag7",
           SL.library=SLlib,
           survivalOutcome=FALSE,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtdependMSMre7 <- parLapply(cl=cl, jtdependMSM7, function (x) {summary(x)})
jtdependMSMco7 <- matrix(unlist(parLapply(cl=cl, jtdependMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMhi7 <- matrix(unlist(parLapply(cl=cl, jtdependMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMlo7 <- matrix(unlist(parLapply(cl=cl, jtdependMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMres7 <- matrix(colMeans(cbind(jtdependMSMco7,jtdependMSMlo7,jtdependMSMhi7)),nrow=1,ncol=3)

rm(list=c("datadepend6","jtdependMSM6","jtdependMSMco6","jtdependMSMlo6","jtdependMSMhi6",
          "datadepend7","jtdependMSM7","jtdependMSMco7","jtdependMSMlo7","jtdependMSMhi7"))

## DSM-5 AUD ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataaud6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataaud7 20191028.RData"))

jtaudMSM6 <- parLapply(cl=cl, dataaud6, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="auddiag6",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtaudMSMre6 <- parLapply(cl=cl, jtaudMSM6, function (x) {summary(x)})
jtaudMSMco6 <- matrix(unlist(parLapply(cl=cl, jtaudMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMhi6 <- matrix(unlist(parLapply(cl=cl, jtaudMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMlo6 <- matrix(unlist(parLapply(cl=cl, jtaudMSMre6, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMres6 <- matrix(colMeans(cbind(jtaudMSMco6,jtaudMSMlo6,jtaudMSMhi6)),nrow=1,ncol=3)

jtaudMSM7 <- parLapply(cl=cl, dataaud7, function (x) {
  ltmleMSM(x,
           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
           Lnodes=Lvars,
           Ynodes="auddiag7",
           survivalOutcome=FALSE,
           SL.library=SLlib,
           working.msm="Y ~ init",
           regimes=regimeList,
           summary.measures=sum.measures)})
jtaudMSMre7 <- parLapply(cl=cl, jtaudMSM7, function (x) {summary(x)})
jtaudMSMco7 <- matrix(unlist(parLapply(cl=cl, jtaudMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2])/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMhi7 <- matrix(unlist(parLapply(cl=cl, jtaudMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]+(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMlo7 <- matrix(unlist(parLapply(cl=cl, jtaudMSMre7, function (x) {plogis(x$cmat[1]+x$cmat[2]-(qnorm(0.975)*x$cmat[4]))/plogis(x$cmat[1])})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMres7 <- matrix(colMeans(cbind(jtaudMSMco7,jtaudMSMlo7,jtaudMSMhi7)),nrow=1,ncol=3)

rm(list=c("dataaud6","jtaudMSM6","jtaudMSMco6","jtaudMSMlo6","jtaudMSMhi6",
          "dataaud7","jtaudMSM7","jtaudMSMco7","jtaudMSMlo7","jtaudMSMhi7"))

## COMBINE RESULTS INTO MATRIX FOR EXCEL ##
MSMresults <- matrix(c(jtbingeMSMres6,jtbingeMSMres7,jtharmsMSMres6,jtharmsMSMres7,jtabuseMSMres6,jtabuseMSMres7,jtdependMSMres6,jtdependMSMres7,jtaudMSMres6,jtaudMSMres7),byrow=TRUE,ncol=6,nrow=5)
rownames(MSMresults) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(MSMresults) <- c("Wave 6 RR","Wave 6 Lower CI","Wave 6 Upper CI","Wave 7 RR","Wave 7 Lower CI","Wave 7 Upper CI")

save(MSMresults,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Results/MSMresults 20191028.RData"))
