
## SYNTAX FILE 7                                             ##
## SENSITIVITY ANALYSIS OF SUPPLY AT AGE 15                  ##

cloudstor <- "C:/Users/z3312911/Cloudstor/"
dropbox <- "C:/Users/z3312911/Dropbox/"
.libPaths(paste0(dropbox,"R Library"))

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

set.seed(546432,kind="L'Ecuyer-CMRG")

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
parallel::clusterEvalQ(cl, Lvars <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2",
                                      "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3",
                                      "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4",
                                      "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5"))
parallel::clusterEvalQ(cl, create.Learner("SL.ranger", params = list(num.trees = 250)))
parallel::clusterEvalQ(cl, SLlib <- list(Q=c("SL.mean","SL.glm","SL.gam"),
                                         g=c("SL.mean","SL.glm","SL.gam","SL.ranger_1")))
## BINGE DRINKING ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/databinge6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/databinge7 20191028.RData"))

jtbinge6 <- parLapply(cl=cl, databinge6, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="binge6",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtbingeres6 <- parLapply(cl=cl, jtbinge6, function (x) {summary(x)})
jtbingeco6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtbingese6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtbingeres6 <- matrix(unlist(mi.meld(q=jtbingeco6, se=jtbingese6)),nrow=1,ncol=2)

jtbinge7 <- parLapply(cl=cl, databinge7, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="binge7",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtbingeres7 <- parLapply(cl=cl, jtbinge7, function (x) {summary(x)})
jtbingeco7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtbingese7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtbingeres7 <- matrix(unlist(mi.meld(q=jtbingeco7, se=jtbingese7)),nrow=1,ncol=2)

rm(list=c("databinge6","jtbinge6","jtbingeco6","jtbingese6",
          "databinge7","jtbinge7","jtbingeco7","jtbingese7"))

## HARMS ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataharms6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataharms7 20191028.RData"))

jtharms6 <- parLapply(cl=cl, dataharms6, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="anyharms6",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtharmsres6 <- parLapply(cl=cl, jtharms6, function (x) {summary(x)})
jtharmsco6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtharmsse6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtharmsres6 <- matrix(unlist(mi.meld(q=jtharmsco6, se=jtharmsse6)),nrow=1,ncol=2)

jtharms7 <- parLapply(cl=cl, dataharms7, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="anyharms7",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtharmsres7 <- parLapply(cl=cl, jtharms7, function (x) {summary(x)})
jtharmsco7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtharmsse7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtharmsres7 <- matrix(unlist(mi.meld(q=jtharmsco7, se=jtharmsse7)),nrow=1,ncol=2)

rm(list=c("dataharms6","jtharms6","jtharmsco6","jtharmsse6",
          "dataharms7","jtharms7","jtharmsco7","jtharmsse7"))

## DSM-IV ABUSE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataabuse6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataabuse7 20191028.RData"))

jtabuse6 <- parLapply(cl=cl, dataabuse6, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="abusediag6",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtabuseres6 <- parLapply(cl=cl, jtabuse6, function (x) {summary(x)})
jtabuseco6 <- matrix(unlist(parLapply(cl=cl, jtabuseres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtabusese6 <- matrix(unlist(parLapply(cl=cl, jtabuseres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtabuseres6 <- matrix(unlist(mi.meld(q=jtabuseco6, se=jtabusese6)),nrow=1,ncol=2)

jtabuse7 <- parLapply(cl=cl, dataabuse7, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="abusediag7",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtabuseres7 <- parLapply(cl=cl, jtabuse7, function (x) {summary(x)})
jtabuseco7 <- matrix(unlist(parLapply(cl=cl, jtabuseres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtabusese7 <- matrix(unlist(parLapply(cl=cl, jtabuseres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtabuseres7 <- matrix(unlist(mi.meld(q=jtabuseco7, se=jtabusese7)),nrow=1,ncol=2)

rm(list=c("dataabuse6","jtabuse6","jtabuseco6","jtabusese6",
          "dataabuse7","jtabuse7","jtabuseco7","jtabusese7"))


## DSM-IV DEPENDENCE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/datadepend6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/datadepend7 20191028.RData"))

jtdepend6 <- parLapply(cl=cl, datadepend6, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="dependdiag6",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtdependres6 <- parLapply(cl=cl, jtdepend6, function (x) {summary(x)})
jtdependco6 <- matrix(unlist(parLapply(cl=cl, jtdependres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtdependse6 <- matrix(unlist(parLapply(cl=cl, jtdependres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtdependres6 <- matrix(unlist(mi.meld(q=jtdependco6, se=jtdependse6)),nrow=1,ncol=2)

jtdepend7 <- parLapply(cl=cl, datadepend7, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="dependdiag7",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtdependres7 <- parLapply(cl=cl, jtdepend7, function (x) {summary(x)})
jtdependco7 <- matrix(unlist(parLapply(cl=cl, jtdependres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtdependse7 <- matrix(unlist(parLapply(cl=cl, jtdependres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtdependres7 <- matrix(unlist(mi.meld(q=jtdependco7, se=jtdependse7)),nrow=1,ncol=2)

rm(list=c("datadepend6","jtdepend6","jtdependco6","jtdependse6",
          "datadepend7","jtdepend7","jtdependco7","jtdependse7"))

## DSM-5 AUD ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataaud6 20191028.RData"))
load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/dataaud7 20191028.RData"))

jtaud6 <- parLapply(cl=cl, dataaud6, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="auddiag6",
        SL.library=SLlib,
        survivalOutcome=FALSE,
        abar=abar)})
jtaudres6 <- parLapply(cl=cl, jtaud6, function (x) {summary(x)})
jtaudco6 <- matrix(unlist(parLapply(cl=cl, jtaudres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtaudse6 <- matrix(unlist(parLapply(cl=cl, jtaudres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtaudres6 <- matrix(unlist(mi.meld(q=jtaudco6, se=jtaudse6)),nrow=1,ncol=2)

jtaud7 <- parLapply(cl=cl, dataaud7, function (x) {
  abar0 <- matrix(nrow=nrow(x), ncol=5)
  abar0[, 1] <- 0
  abar0[, 2] <- 0
  abar0[, 3] <- 0
  abar0[, 4] <- 0
  abar0[, 5] <- 0
  abar1 <- matrix(nrow=nrow(x), ncol=5)
  abar1[, 1] <- (x$c_age1+12)>=15
  abar1[, 2] <- (x$c_age2+12)>=15
  abar1[, 3] <- (x$c_age3+12)>=15
  abar1[, 4] <- (x$c_age4+12)>=15
  abar1[, 5] <- (x$c_age5+12)>=15
  abar <- list(abar0,abar1)
  ltmle(x,
        Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
        Lnodes=Lvars,
        Ynodes="auddiag7",
        survivalOutcome=FALSE,
        SL.library=SLlib,
        abar=abar)})
jtaudres7 <- parLapply(cl=cl, jtaud7, function (x) {summary(x)})
jtaudco7 <- matrix(unlist(parLapply(cl=cl, jtaudres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtaudse7 <- matrix(unlist(parLapply(cl=cl, jtaudres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtaudres7 <- matrix(unlist(mi.meld(q=jtaudco7, se=jtaudse7)),nrow=1,ncol=2)

rm(list=c("dataaud6","jtaud6","jtaudco6","jtaudse6",
          "dataaud7","jtaud7","jtaudco7","jtaudse7"))

## COMBINE RESULTS INTO MATRIX FOR EXCEL ##
jtresultss1 <- matrix(c(jtbingeres6,jtbingeres7,jtharmsres6,jtharmsres7,jtabuseres6,jtabuseres7,jtdependres6,jtdependres7,jtaudres6,jtaudres7),byrow=TRUE,ncol=4,nrow=5)
rownames(jtresultss1) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(jtresultss1) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")

save(jtresultss1,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Results/jtresults S1 20191028.RData"))

