## SYNTAX FILE 3 - LTMLE ANALYSIS                            ##

cloudstor <- "C:/Users/z3312911/Cloudstor/" # change to master file path
.libPaths(paste0(cloudstor,"R Library"))

libs <- c("parallel","SuperLearner","ltmle","Amelia","ranger","gam")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}

library ("parallel")
library ("Amelia")

set.seed(546432,kind="L'Ecuyer-CMRG")

numcores <- future::availableCores()
cl <- makeCluster(numcores)
parallel::clusterEvalQ(cl, cloudstor <- "C:/Users/z3312911/Cloudstor/")
parallel::clusterEvalQ(cl, .libPaths(paste0(cloudstor,"R Library")))
parallel::clusterEvalQ(cl, library("plyr"))
parallel::clusterEvalQ(cl, library("readr"))
parallel::clusterEvalQ(cl, library("ltmle"))
parallel::clusterEvalQ(cl, library("SuperLearner"))
parallel::clusterEvalQ(cl, library("gam"))
parallel::clusterEvalQ(cl, Lvars <- c("homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2",
                                      "homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3",
                                      "homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4",
                                      "homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5"))
parallel::clusterEvalQ(cl, regimeList <- list(function(row) c(1,1,1,1,1),
                                              function(row) c(0,1,1,1,1),
                                              function(row) c(0,0,1,1,1),
                                              function(row) c(0,0,0,1,1),
                                              function(row) c(0,0,0,0,1),
                                              function(row) c(0,0,0,0,0)))
parallel::clusterEvalQ(cl, sum.measures <- as.array(matrix(c(5,4,3,2,1,0),nrow=6,ncol=1)))
parallel::clusterEvalQ(cl, colnames(sum.measures) <- "init")
parallel::clusterEvalQ(cl, create.Learner("SL.ranger", params = list(num.trees = 250)))
parallel::clusterEvalQ(cl, create.Learner("SL.randomForest", params = list(num.trees = 250)))
parallel::clusterEvalQ(cl, SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
                                         g=c("SL.mean","SL.glm","SL.gam","SL.ranger_1")))

## BINGE DRINKING ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/databinge6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/databinge7.RData"))

jtbinge6 <- parLapply(cl=cl, databinge6, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="binge6",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtbingeres6 <- parLapply(cl=cl, jtbinge6, function (x) {summary(x)})
jtbingeco6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtbingese6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtbingeres6 <- matrix(unlist(mi.meld(q=jtbingeco6, se=jtbingese6)),nrow=1,ncol=2)

jtbinge7 <- parLapply(cl=cl, databinge7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="binge7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtbingeres7 <- parLapply(cl=cl, jtbinge7, function (x) {summary(x)})
jtbingeco7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtbingese7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtbingeres7 <- matrix(unlist(mi.meld(q=jtbingeco7, se=jtbingese7)),nrow=1,ncol=2)

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
jtbingeMSMres6 <- parLapply(cl=cl, jtbingeMSM6, function (x) {summary(x)})
jtbingeMSMco6 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMres6, function (x) {x$cmat[2]})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMse6 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMres6, function (x) {x$cmat[4]})),byrow=TRUE,nrow=50,ncol=1)
jtbingeMSMres6 <- matrix(unlist(mi.meld(q=jtbingeMSMco6, se=jtbingeMSMse6)),nrow=1,ncol=2)

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
jtbingeMSMres7 <- parLapply(cl=cl, jtbingeMSM7, function (x) {summary(x)})
jtbingeMSMco7 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMres7, function (x) {x$cmat[2]})),nrow=50,ncol=1)
jtbingeMSMse7 <- matrix(unlist(parLapply(cl=cl, jtbingeMSMres7, function (x) {x$cmat[4]})),nrow=50,ncol=1)
jtbingeMSMres7 <- matrix(unlist(mi.meld(q=jtbingeMSMco7, se=jtbingeMSMse7)),nrow=1,ncol=2)

rm(list=c("databinge6","jtbinge6","jtbingeMSM6","databinge7","jtbinge7","jtbingeMSM7"))

## HARMS ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataharms6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataharms7.RData"))

jtharms6 <- parLapply(cl=cl, dataharms6, function (x) {ltmle(x,
                                                           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                           Lnodes=Lvars,
                                                           Ynodes="anyharms6",
                                                           survivalOutcome=FALSE,
                                                           SL.library=SLlib,
                                                           abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtharmsres6 <- parLapply(cl=cl, jtharms6, function (x) {summary(x)})
jtharmsco6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtharmsse6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtharmsres6 <- matrix(unlist(mi.meld(q=jtharmsco6, se=jtharmsse6)),nrow=1,ncol=2)

jtharms7 <- parLapply(cl=cl, dataharms7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="anyharms7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtharmsres7 <- parLapply(cl=cl, jtharms7, function (x) {summary(x)})
jtharmsco7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtharmsse7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtharmsres7 <- matrix(unlist(mi.meld(q=jtharmsco7, se=jtharmsse7)),nrow=1,ncol=2)

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
jtharmsMSMres6 <- parLapply(cl=cl, jtharmsMSM6, function (x) {summary(x)})
jtharmsMSMco6 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMres6, function (x) {x$cmat[2]})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMse6 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMres6, function (x) {x$cmat[4]})),byrow=TRUE,nrow=50,ncol=1)
jtharmsMSMres6 <- matrix(unlist(mi.meld(q=jtharmsMSMco6, se=jtharmsMSMse6)),nrow=1,ncol=2)

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
jtharmsMSMres7 <- parLapply(cl=cl, jtharmsMSM7, function (x) {summary(x)})
jtharmsMSMco7 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMres7, function (x) {x$cmat[2]})),nrow=50,ncol=1)
jtharmsMSMse7 <- matrix(unlist(parLapply(cl=cl, jtharmsMSMres7, function (x) {x$cmat[4]})),nrow=50,ncol=1)
jtharmsMSMres7 <- matrix(unlist(mi.meld(q=jtharmsMSMco7, se=jtharmsMSMse7)),nrow=1,ncol=2)

rm(list=c("dataharms6","jtharms6","jtharmsMSM6","dataharms7","jtharms7","jtharmsMSM7"))

## DSM-IV ABUSE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataabuse6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataabuse7.RData"))

jtabuse6 <- parLapply(cl=cl, dataabuse6, function (x) {ltmle(x,
                                                           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                           Lnodes=Lvars,
                                                           Ynodes="abusediag6",
                                                           survivalOutcome=FALSE,
                                                           SL.library=SLlib,
                                                           abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtabuseres6 <- parLapply(cl=cl, jtabuse6, function (x) {summary(x)})
jtabuseco6 <- matrix(unlist(parLapply(cl=cl, jtabuseres6, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtabusese6 <- matrix(unlist(parLapply(cl=cl, jtabuseres6, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtabuseres6 <- matrix(unlist(mi.meld(q=jtabuseco6, se=jtabusese6)),nrow=1,ncol=2)

jtabuse7 <- parLapply(cl=cl, dataabuse7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvars,
                                                             Ynodes="abusediag7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtabuseres7 <- parLapply(cl=cl, jtabuse7, function (x) {summary(x)})
jtabuseco7 <- matrix(unlist(parLapply(cl=cl, jtabuseres7, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtabusese7 <- matrix(unlist(parLapply(cl=cl, jtabuseres7, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtabuseres7 <- matrix(unlist(mi.meld(q=jtabuseco7, se=jtabusese7)),nrow=1,ncol=2)

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
jtabuseMSMres6 <- parLapply(cl=cl, jtabuseMSM6, function (x) {summary(x)})
jtabuseMSMco6 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMres6, function (x) {x$cmat[2]})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMse6 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMres6, function (x) {x$cmat[4]})),byrow=TRUE,nrow=50,ncol=1)
jtabuseMSMres6 <- matrix(unlist(mi.meld(q=jtabuseMSMco6, se=jtabuseMSMse6)),nrow=1,ncol=2)

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
jtabuseMSMres7 <- parLapply(cl=cl, jtabuseMSM7, function (x) {summary(x)})
jtabuseMSMco7 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMres7, function (x) {x$cmat[2]})),nrow=50,ncol=1)
jtabuseMSMse7 <- matrix(unlist(parLapply(cl=cl, jtabuseMSMres7, function (x) {x$cmat[4]})),nrow=50,ncol=1)
jtabuseMSMres7 <- matrix(unlist(mi.meld(q=jtabuseMSMco7, se=jtabuseMSMse7)),nrow=1,ncol=2)

rm(list=c("dataabuse6","jtabuse6","jtabuseMSM6","dataabuse7","jtabuse7","jtabuseMSM7"))

## DSM-IV DEPENDENCE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/datadepend6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/datadepend7.RData"))

jtdepend6 <- parLapply(cl=cl, datadepend6, function (x) {ltmle(x,
                                                           Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                           Lnodes=Lvars,
                                                           Ynodes="dependdiag6",
                                                           survivalOutcome=FALSE,
                                                           SL.library=SLlib,
                                                           abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtdependres6 <- parLapply(cl=cl, jtdepend6, function (x) {summary(x)})
jtdependco6 <- matrix(unlist(parLapply(cl=cl, jtdependres6, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtdependse6 <- matrix(unlist(parLapply(cl=cl, jtdependres6, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtdependres6 <- matrix(unlist(mi.meld(q=jtdependco6, se=jtdependse6)),nrow=1,ncol=2)

jtdepend7 <- parLapply(cl=cl, datadepend7, function (x) {ltmle(x,
                                                               Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                               Lnodes=Lvars,
                                                               Ynodes="dependdiag7",
                                                               survivalOutcome=FALSE,
                                                               SL.library=SLlib,
                                                               abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtdependres7 <- parLapply(cl=cl, jtdepend7, function (x) {summary(x)})
jtdependco7 <- matrix(unlist(parLapply(cl=cl, jtdependres7, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtdependse7 <- matrix(unlist(parLapply(cl=cl, jtdependres7, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtdependres7 <- matrix(unlist(mi.meld(q=jtdependco7, se=jtdependse7)),nrow=1,ncol=2)

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
jtdependMSMres6 <- parLapply(cl=cl, jtdependMSM6, function (x) {summary(x)})
jtdependMSMco6 <- matrix(unlist(parLapply(cl=cl, jtdependMSMres6, function (x) {x$cmat[2]})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMse6 <- matrix(unlist(parLapply(cl=cl, jtdependMSMres6, function (x) {x$cmat[4]})),byrow=TRUE,nrow=50,ncol=1)
jtdependMSMres6 <- matrix(unlist(mi.meld(q=jtdependMSMco6, se=jtdependMSMse6)),nrow=1,ncol=2)

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
jtdependMSMres7 <- parLapply(cl=cl, jtdependMSM7, function (x) {summary(x)})
jtdependMSMco7 <- matrix(unlist(parLapply(cl=cl, jtdependMSMres7, function (x) {x$cmat[2]})),nrow=50,ncol=1)
jtdependMSMse7 <- matrix(unlist(parLapply(cl=cl, jtdependMSMres7, function (x) {x$cmat[4]})),nrow=50,ncol=1)
jtdependMSMres7 <- matrix(unlist(mi.meld(q=jtdependMSMco7, se=jtdependMSMse7)),nrow=1,ncol=2)

rm(list=c("datadepend6","jtdepend6","jtdependMSM6","datadepend7","jtdepend7","jtdependMSM7"))

## DSM-5 AUD ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataaud6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataaud7.RData"))

jtaud6 <- parLapply(cl=cl, dataaud6, function (x) {ltmle(x,
                                                         Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                         Lnodes=Lvars,
                                                         Ynodes="auddiag6",
                                                         SL.library=SLlib,
                                                         survivalOutcome=FALSE,
                                                         abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtaudres6 <- parLapply(cl=cl, jtaud6, function (x) {summary(x)})
jtaudco6 <- matrix(unlist(parLapply(cl=cl, jtaudres6, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtaudse6 <- matrix(unlist(parLapply(cl=cl, jtaudres6, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtaudres6 <- matrix(unlist(mi.meld(q=jtaudco6, se=jtaudse6)),nrow=1,ncol=2)

jtaud7 <- parLapply(cl=cl, dataaud7, function (x) {ltmle(x,
                                                         Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                         Lnodes=Lvars,
                                                         Ynodes="auddiag7",
                                                         survivalOutcome=FALSE,
                                                         SL.library=SLlib,
                                                         abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtaudres7 <- parLapply(cl=cl, jtaud7, function (x) {summary(x)})
jtaudco7 <- matrix(unlist(parLapply(cl=cl, jtaudres7, function (x) {log(x$effect.measures$OR$estimate)})),nrow=50,ncol=1)
jtaudse7 <- matrix(unlist(parLapply(cl=cl, jtaudres7, function (x) {x$effect.measures$OR$std.dev})),nrow=50,ncol=1)
jtaudres7 <- matrix(unlist(mi.meld(q=jtaudco7, se=jtaudse7)),nrow=1,ncol=2)

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
jtaudMSMres6 <- parLapply(cl=cl, jtaudMSM6, function (x) {summary(x)})
jtaudMSMco6 <- matrix(unlist(parLapply(cl=cl, jtaudMSMres6, function (x) {x$cmat[2]})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMse6 <- matrix(unlist(parLapply(cl=cl, jtaudMSMres6, function (x) {x$cmat[4]})),byrow=TRUE,nrow=50,ncol=1)
jtaudMSMres6 <- matrix(unlist(mi.meld(q=jtaudMSMco6, se=jtaudMSMse6)),nrow=1,ncol=2)

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
jtaudMSMres7 <- parLapply(cl=cl, jtaudMSM7, function (x) {summary(x)})
jtaudMSMco7 <- matrix(unlist(parLapply(cl=cl, jtaudMSMres7, function (x) {x$cmat[2]})),nrow=50,ncol=1)
jtaudMSMse7 <- matrix(unlist(parLapply(cl=cl, jtaudMSMres7, function (x) {x$cmat[4]})),nrow=50,ncol=1)
jtaudMSMres7 <- matrix(unlist(mi.meld(q=jtaudMSMco7, se=jtaudMSMse7)),nrow=1,ncol=2)

rm(list=c("dataaud6","jtaud6","jtaudMSM6","dataaud7","jtaud7","jtaudMSM7"))

## COMBINE RESULTS INTO MATRIX FOR EXCEL ##
jtresults <- matrix(c(jtbingeres6,jtbingeres7,jtharmsres6,jtharmsres7,jtabuseres6,jtabuseres7,jtdependres6,jtdependres7,jtaudres6,jtaudres7),byrow=TRUE,ncol=4,nrow=5)
rownames(jtresults) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(jtresults) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")

MSMresults <- matrix(c(jtbingeMSMres6,jtbingeMSMres7,jtharmsMSMres6,jtharmsMSMres7,jtabuseMSMres6,jtabuseMSMres7,jtdependMSMres6,jtdependMSMres7,jtaudMSMres6,jtaudMSMres7),byrow=TRUE,ncol=4,nrow=5)
rownames(MSMresults) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(MSMresults) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")

save(jtresults,file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Results/jtresults.RData"))
save(MSMresults,file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Results/MSMresults.RData"))