
## SYNTAX FILE 9                                             ##
## SENSITIVITY ANALYSIS CONTROLLING FOR PAST OUTCOMES        ##

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
parallel::clusterEvalQ(cl, Lvarsbinge <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2","binge2",
                                           "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3","binge3",
                                           "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4","binge4",
                                           "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5","binge5"))
parallel::clusterEvalQ(cl, Lvarsharms <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2","anyharms2",
                                           "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3","anyharms3",
                                           "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4","anyharms4",
                                           "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5","anyharms5"))
parallel::clusterEvalQ(cl, Lvarsabuse <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2","abusediag2",
                                           "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3","abusediag3",
                                           "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4","abusediag4",
                                           "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5","abusediag5"))
parallel::clusterEvalQ(cl, Lvarsdepend <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2","dependdiag2",
                                            "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3","dependdiag3",
                                            "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4","dependdiag4",
                                            "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5","dependdiag5"))
parallel::clusterEvalQ(cl, Lvarsaud <- c("c_age2","hhavguse2","parmon2","homeacc2","alcrules2","alcmoney2","singlep2","smoking2","cbclextn2","cbcladn2","cbclwdn2","peeruse2","peerdis2","othsup2","auddiag2",
                                         "c_age3","hhavguse3","parmon3","homeacc3","alcrules3","alcmoney3","singlep3","smoking3","cbclextn3","cbcladn3","cbclwdn3","peeruse3","peerdis3","othsup3","auddiag3",
                                         "c_age4","hhavguse4","parmon4","homeacc4","alcrules4","alcmoney4","singlep4","smoking4","cbclextn4","cbcladn4","cbclwdn4","peeruse4","peerdis4","othsup4","auddiag4",
                                         "c_age5","hhavguse5","parmon5","homeacc5","alcrules5","alcmoney5","singlep5","smoking5","cbclextn5","cbcladn5","cbclwdn5","peeruse5","peerdis5","othsup5","auddiag5"))
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
  x$bingef <- ifelse(x$bingef>0,1,0)
  x$numharms <- as.numeric(levels(x$numharms))[x$numharms]
  x$numharms <- ifelse(x$numharms>0,1,0)
  x$abusediag <- as.numeric(levels(x$abusediag))[x$abusediag]
  x$dependdiag <- as.numeric(levels(x$dependdiag))[x$dependdiag]
  x$auddiag <- as.numeric(levels(x$auddiag))[x$auddiag]
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
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,108)]
})

jtbinge6 <- parLapply(cl=cl, databinge6, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvarsbinge,
                                                             Ynodes="binge6",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtbingeres6 <- parLapply(cl=cl, jtbinge6, function (x) {summary(x)})
jtbingeco6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtbingese6 <- matrix(unlist(parLapply(cl=cl, jtbingeres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtbingeres6 <- matrix(unlist(mi.meld(q=jtbingeco6, se=jtbingese6)),nrow=1,ncol=2)

rm(list=c("databinge6","jtbinge6","jtbingeco6","jtbingese6"))

databinge7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-28,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"binge"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,124)]
})

jtbinge7 <- parLapply(cl=cl, databinge7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvarsbinge,
                                                             Ynodes="binge7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtbingeres7 <- parLapply(cl=cl, jtbinge7, function (x) {summary(x)})
jtbingeco7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtbingese7 <- matrix(unlist(parLapply(cl=cl, jtbingeres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtbingeres7 <- matrix(unlist(mi.meld(q=jtbingeco7, se=jtbingese7)),nrow=1,ncol=2)

rm(list=c("databinge7","jtbinge7","jtbingeco7","jtbingese7"))

dataharms6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"anyharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,108)]
})

jtharms6 <- parLapply(cl=cl, dataharms6, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvarsharms,
                                                             Ynodes="anyharms6",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtharmsres6 <- parLapply(cl=cl, jtharms6, function (x) {summary(x)})
jtharmsco6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtharmsse6 <- matrix(unlist(parLapply(cl=cl, jtharmsres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtharmsres6 <- matrix(unlist(mi.meld(q=jtharmsco6, se=jtharmsse6)),nrow=1,ncol=2)

rm(list=c("dataharms6","jtharms6","jtharmsco6","jtharmsse6"))

dataharms7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-29,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"anyharms"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,124)]
})

jtharms7 <- parLapply(cl=cl, dataharms7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvarsharms,
                                                             Ynodes="anyharms7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtharmsres7 <- parLapply(cl=cl, jtharms7, function (x) {summary(x)})
jtharmsco7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtharmsse7 <- matrix(unlist(parLapply(cl=cl, jtharmsres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtharmsres7 <- matrix(unlist(mi.meld(q=jtharmsco7, se=jtharmsse7)),nrow=1,ncol=2)

rm(list=c("dataharms7","jtharms7","jtharmsco7","jtharmsse7"))

dataabuse6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"abusediag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,108)]
})

jtabuse6 <- parLapply(cl=cl, dataabuse6, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvarsabuse,
                                                             Ynodes="abusediag6",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtabuseres6 <- parLapply(cl=cl, jtabuse6, function (x) {summary(x)})
jtabuseco6 <- matrix(unlist(parLapply(cl=cl, jtabuseres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtabusese6 <- matrix(unlist(parLapply(cl=cl, jtabuseres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtabuseres6 <- matrix(unlist(mi.meld(q=jtabuseco6, se=jtabusese6)),nrow=1,ncol=2)

rm(list=c("dataabuse6","jtabuse6","jtabuseco6","jtabusese6"))

dataabuse7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-30,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"abusediag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,124)]
})

jtabuse7 <- parLapply(cl=cl, dataabuse7, function (x) {ltmle(x,
                                                             Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                             Lnodes=Lvarsabuse,
                                                             Ynodes="abusediag7",
                                                             survivalOutcome=FALSE,
                                                             SL.library=SLlib,
                                                             abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtabuseres7 <- parLapply(cl=cl, jtabuse7, function (x) {summary(x)})
jtabuseco7 <- matrix(unlist(parLapply(cl=cl, jtabuseres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtabusese7 <- matrix(unlist(parLapply(cl=cl, jtabuseres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtabuseres7 <- matrix(unlist(mi.meld(q=jtabuseco7, se=jtabusese7)),nrow=1,ncol=2)

rm(list=c("dataabuse7","jtabuse7","jtabuseco7","jtabusese7"))

datadepend6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"dependdiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,108)]
})

jtdepend6 <- parLapply(cl=cl, datadepend6, function (x) {ltmle(x,
                                                               Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                               Lnodes=Lvarsdepend,
                                                               Ynodes="dependdiag6",
                                                               survivalOutcome=FALSE,
                                                               SL.library=SLlib,
                                                               abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtdependres6 <- parLapply(cl=cl, jtdepend6, function (x) {summary(x)})
jtdependco6 <- matrix(unlist(parLapply(cl=cl, jtdependres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtdependse6 <- matrix(unlist(parLapply(cl=cl, jtdependres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtdependres6 <- matrix(unlist(mi.meld(q=jtdependco6, se=jtdependse6)),nrow=1,ncol=2)

rm(list=c("datadepend6","jtdepend6","jtdependco6","jtdependse6"))

datadepend7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-31)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"dependdiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,124)]
})

jtdepend7 <- parLapply(cl=cl, datadepend7, function (x) {ltmle(x,
                                                               Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                               Lnodes=Lvarsdepend,
                                                               Ynodes="dependdiag7",
                                                               survivalOutcome=FALSE,
                                                               SL.library=SLlib,
                                                               abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtdependres7 <- parLapply(cl=cl, jtdepend7, function (x) {summary(x)})
jtdependco7 <- matrix(unlist(parLapply(cl=cl, jtdependres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtdependse7 <- matrix(unlist(parLapply(cl=cl, jtdependres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtdependres7 <- matrix(unlist(mi.meld(q=jtdependco7, se=jtdependse7)),nrow=1,ncol=2)

rm(list=c("datadepend7","jtdepend7","jtdependco7","jtdependse7"))

dataaud6 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-30)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"auddiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,108)]
})

jtaud6 <- parLapply(cl=cl, dataaud6, function (x) {ltmle(x,
                                                         Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                         Lnodes=Lvarsaud,
                                                         Ynodes="auddiag6",
                                                         SL.library=SLlib,
                                                         survivalOutcome=FALSE,
                                                         abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtaudres6 <- parLapply(cl=cl, jtaud6, function (x) {summary(x)})
jtaudco6 <- matrix(unlist(parLapply(cl=cl, jtaudres6, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtaudse6 <- matrix(unlist(parLapply(cl=cl, jtaudres6, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtaudres6 <- matrix(unlist(mi.meld(q=jtaudco6, se=jtaudse6)),nrow=1,ncol=2)

rm(list=c("dataaud6","jtaud6","jtaudco6","jtaudse6"))

dataaud7 <- parLapply(cl=cl, imp, function (x) {
  x$cqcomp <- NULL
  x <- x[,c(-27,-28,-29,-30)]
  x <- reshape(x,idvar="zzC_ID",timevar="zzwave",v.names=c(vnames,"auddiag"),direction="wide",sep="")
  x <- subset(x, !(c_age1>=18 | c_age2>=18 | c_age3>=18 | c_age4>=18 | c_age5>=18))
  x <- x[,c(2:26,28,27,29:42,44,43,45:58,60,59,61:74,76,75,77:90,92,91,124)]
})

jtaud7 <- parLapply(cl=cl, dataaud7, function (x) {ltmle(x,
                                                         Anodes=c("parsup1","parsup2","parsup3","parsup4","parsup5"),
                                                         Lnodes=Lvarsaud,
                                                         Ynodes="auddiag7",
                                                         survivalOutcome=FALSE,
                                                         SL.library=SLlib,
                                                         abar=list(c(1,1,1,1,1),c(0,0,0,0,0)))})
jtaudres7 <- parLapply(cl=cl, jtaud7, function (x) {summary(x)})
jtaudco7 <- matrix(unlist(parLapply(cl=cl, jtaudres7, function (x) {log(x$effect.measures$RR$estimate)})),nrow=50,ncol=1)
jtaudse7 <- matrix(unlist(parLapply(cl=cl, jtaudres7, function (x) {x$effect.measures$RR$std.dev})),nrow=50,ncol=1)
jtaudres7 <- matrix(unlist(mi.meld(q=jtaudco7, se=jtaudse7)),nrow=1,ncol=2)

rm(list=c("dataaud7","jtaud7","jtaudco7","jtaudse7"))

## COMBINE RESULTS INTO MATRIX FOR EXCEL ##
jtresultss3 <- matrix(c(jtbingeres6,jtbingeres7,jtharmsres6,jtharmsres7,jtabuseres6,jtabuseres7,jtdependres6,jtdependres7,jtaudres6,jtaudres7),byrow=TRUE,ncol=4,nrow=5)
rownames(jtresultss3) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(jtresultss3) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")

save(jtresultss3,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Results/jtresults S3 20191028.RData"))