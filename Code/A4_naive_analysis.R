## SYNTAX FILE 4 - NAIVE ANALYSIS                            ##

cloudstor <- "C:/Users/z3312911/Cloudstor/" # change to master file path
.libPaths(paste0(cloudstor,"R Library"))

libs <- c("plyr","readr","Amelia","parallel")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
library ("plyr")
library ("readr")
library ("Amelia")
library ("parallel")

set.seed(546432,kind="L'Ecuyer-CMRG")

numcores <- future::availableCores()
cl <- makeCluster(numcores)
parallel::clusterEvalQ(cl, cloudstor <- "C:/Users/z3312911/Cloudstor/")
parallel::clusterEvalQ(cl, .libPaths(paste0(cloudstor,"R Library")))
parallel::clusterEvalQ(cl, library("plyr"))
parallel::clusterEvalQ(cl, library("readr"))
parallel::clusterEvalQ(cl, Lvars <- "b_hhavguse + b_parmon + b_pardem + b_parres + b_parcon + b_parrel + b_parborn + b_parempl + b_famposi + b_famconf + b_seifa + b_hinc + b_age + b_sex + homeacc1 + alcrules1 + alcmoney1 + singlep1 + smoking1 + cbclextn1 + cbcladn1 + cbclwdn1 + peeruse1 + peerdis1 + othsup1 + homeacc2 + alcrules2 + alcmoney2 + singlep2 + smoking2 + cbclextn2 + cbcladn2 + cbclwdn2 + peeruse2 + peerdis2 + othsup2 + homeacc3 + alcrules3 + alcmoney3 + singlep3 + smoking3 + cbclextn3 + cbcladn3 + cbclwdn3 + peeruse3 + peerdis3 + othsup3 + homeacc4 + alcrules4 + alcmoney4 + singlep4 + smoking4 + cbclextn4 + cbcladn4 + cbclwdn4 + peeruse4 + peerdis4 + othsup4 + homeacc5 + alcrules5 + alcmoney5 + singlep5 + smoking5 + cbclextn5 + cbcladn5 + cbclwdn5 + peeruse5 + peerdis5 + othsup5")
parallel::clusterEvalQ(cl, Bvars <- "b_hhavguse + b_parmon + b_pardem + b_parres + b_parcon + b_parrel + b_parborn + b_parempl + b_famposi + b_famconf + b_seifa + b_hinc + b_age + b_sex + homeacc1 + alcrules1 + alcmoney1 + singlep1 + smoking1 + cbclextn1 + cbcladn1 + cbclwdn1 + peeruse1 + peerdis1 + othsup1")

## BINGE DRINKING ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/databinge6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/databinge7.RData"))

glmbinge6 <- parLapply(cl=cl, databinge6, function (x) {glm(data=x,
                                                            formula=paste0("binge6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmbingeco6 <- matrix(unlist(parLapply(cl=cl, glmbinge6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmbingese6 <- matrix(unlist(parLapply(cl=cl, glmbinge6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmbingeres6a <- matrix(unlist(mi.meld(q=glmbingeco6, se=glmbingese6)),nrow=1,ncol=2)

glmbinge6 <- parLapply(cl=cl, databinge6, function (x) {glm(data=x,
                                                            formula=paste0("binge6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmbingeco6 <- matrix(unlist(parLapply(cl=cl, glmbinge6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmbingese6 <- matrix(unlist(parLapply(cl=cl, glmbinge6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmbingeres6b <- matrix(unlist(mi.meld(q=glmbingeco6, se=glmbingese6)),nrow=1,ncol=2)

glmbinge7 <- parLapply(cl=cl, databinge7, function (x) {glm(data=x,
                                                            formula=paste0("binge7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmbingeco7 <- matrix(unlist(parLapply(cl=cl, glmbinge7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmbingese7 <- matrix(unlist(parLapply(cl=cl, glmbinge7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmbingeres7a <- matrix(unlist(mi.meld(q=glmbingeco7, se=glmbingese7)),nrow=1,ncol=2)

glmbinge7 <- parLapply(cl=cl, databinge7, function (x) {glm(data=x,
                                                            formula=paste0("binge7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmbingeco7 <- matrix(unlist(parLapply(cl=cl, glmbinge7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmbingese7 <- matrix(unlist(parLapply(cl=cl, glmbinge7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmbingeres7b <- matrix(unlist(mi.meld(q=glmbingeco7, se=glmbingese7)),nrow=1,ncol=2)

rm(list=c("databinge6","databinge7"))

## HARMS ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataharms6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataharms7.RData"))

glmharms6 <- parLapply(cl=cl, dataharms6, function (x) {glm(data=x,
                                                            formula=paste0("anyharms6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmharmsco6 <- matrix(unlist(parLapply(cl=cl, glmharms6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmharmsse6 <- matrix(unlist(parLapply(cl=cl, glmharms6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmharmsres6a <- matrix(unlist(mi.meld(q=glmharmsco6, se=glmharmsse6)),nrow=1,ncol=2)

glmharms6 <- parLapply(cl=cl, dataharms6, function (x) {glm(data=x,
                                                            formula=paste0("anyharms6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmharmsco6 <- matrix(unlist(parLapply(cl=cl, glmharms6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmharmsse6 <- matrix(unlist(parLapply(cl=cl, glmharms6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmharmsres6b <- matrix(unlist(mi.meld(q=glmharmsco6, se=glmharmsse6)),nrow=1,ncol=2)

glmharms7 <- parLapply(cl=cl, dataharms7, function (x) {glm(data=x,
                                                            formula=paste0("anyharms7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmharmsco7 <- matrix(unlist(parLapply(cl=cl, glmharms7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmharmsse7 <- matrix(unlist(parLapply(cl=cl, glmharms7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmharmsres7a <- matrix(unlist(mi.meld(q=glmharmsco7, se=glmharmsse7)),nrow=1,ncol=2)

glmharms7 <- parLapply(cl=cl, dataharms7, function (x) {glm(data=x,
                                                            formula=paste0("anyharms7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmharmsco7 <- matrix(unlist(parLapply(cl=cl, glmharms7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmharmsse7 <- matrix(unlist(parLapply(cl=cl, glmharms7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmharmsres7b <- matrix(unlist(mi.meld(q=glmharmsco7, se=glmharmsse7)),nrow=1,ncol=2)

rm(list=c("dataharms6","dataharms7"))

## DSM-IV ABUSE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataabuse6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataabuse7.RData"))

glmabuse6 <- parLapply(cl=cl, dataabuse6, function (x) {glm(data=x,
                                                            formula=paste0("abusediag6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmabuseco6 <- matrix(unlist(parLapply(cl=cl, glmabuse6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmabusese6 <- matrix(unlist(parLapply(cl=cl, glmabuse6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmabuseres6a <- matrix(unlist(mi.meld(q=glmabuseco6, se=glmabusese6)),nrow=1,ncol=2)

glmabuse6 <- parLapply(cl=cl, dataabuse6, function (x) {glm(data=x,
                                                            formula=paste0("abusediag6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmabuseco6 <- matrix(unlist(parLapply(cl=cl, glmabuse6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmabusese6 <- matrix(unlist(parLapply(cl=cl, glmabuse6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmabuseres6b <- matrix(unlist(mi.meld(q=glmabuseco6, se=glmabusese6)),nrow=1,ncol=2)

glmabuse7 <- parLapply(cl=cl, dataabuse7, function (x) {glm(data=x,
                                                            formula=paste0("abusediag7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmabuseco7 <- matrix(unlist(parLapply(cl=cl, glmabuse7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmabusese7 <- matrix(unlist(parLapply(cl=cl, glmabuse7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmabuseres7a <- matrix(unlist(mi.meld(q=glmabuseco7, se=glmabusese7)),nrow=1,ncol=2)

glmabuse7 <- parLapply(cl=cl, dataabuse7, function (x) {glm(data=x,
                                                            formula=paste0("abusediag7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmabuseco7 <- matrix(unlist(parLapply(cl=cl, glmabuse7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmabusese7 <- matrix(unlist(parLapply(cl=cl, glmabuse7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmabuseres7b <- matrix(unlist(mi.meld(q=glmabuseco7, se=glmabusese7)),nrow=1,ncol=2)

rm(list=c("dataabuse6","dataabuse7"))

## DSM-IV DEPENDENCE ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/datadepend6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/datadepend7.RData"))

glmdepend6 <- parLapply(cl=cl, datadepend6, function (x) {glm(data=x,
                                                            formula=paste0("dependdiag6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmdependco6 <- matrix(unlist(parLapply(cl=cl, glmdepend6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmdependse6 <- matrix(unlist(parLapply(cl=cl, glmdepend6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmdependres6a <- matrix(unlist(mi.meld(q=glmdependco6, se=glmdependse6)),nrow=1,ncol=2)

glmdepend6 <- parLapply(cl=cl, datadepend6, function (x) {glm(data=x,
                                                              formula=paste0("dependdiag6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmdependco6 <- matrix(unlist(parLapply(cl=cl, glmdepend6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmdependse6 <- matrix(unlist(parLapply(cl=cl, glmdepend6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmdependres6b <- matrix(unlist(mi.meld(q=glmdependco6, se=glmdependse6)),nrow=1,ncol=2)

glmdepend7 <- parLapply(cl=cl, datadepend7, function (x) {glm(data=x,
                                                            formula=paste0("dependdiag7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmdependco7 <- matrix(unlist(parLapply(cl=cl, glmdepend7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmdependse7 <- matrix(unlist(parLapply(cl=cl, glmdepend7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmdependres7a <- matrix(unlist(mi.meld(q=glmdependco7, se=glmdependse7)),nrow=1,ncol=2)

glmdepend7 <- parLapply(cl=cl, datadepend7, function (x) {glm(data=x,
                                                              formula=paste0("dependdiag7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmdependco7 <- matrix(unlist(parLapply(cl=cl, glmdepend7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmdependse7 <- matrix(unlist(parLapply(cl=cl, glmdepend7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmdependres7b <- matrix(unlist(mi.meld(q=glmdependco7, se=glmdependse7)),nrow=1,ncol=2)

rm(list=c("datadepend6","datadepend7"))

## DSM-5 AUD ANALYSIS ##
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataaud6.RData"))
load(file=paste0(cloudstor,"PhD/Paper 6 - APSALs application/Analysis Data/dataaud7.RData"))

glmaud6 <- parLapply(cl=cl, dataaud6, function (x) {glm(data=x,
                                                            formula=paste0("auddiag6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmaudco6 <- matrix(unlist(parLapply(cl=cl, glmaud6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmaudse6 <- matrix(unlist(parLapply(cl=cl, glmaud6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmaudres6a <- matrix(unlist(mi.meld(q=glmaudco6, se=glmaudse6)),nrow=1,ncol=2)

glmaud6 <- parLapply(cl=cl, dataaud6, function (x) {glm(data=x,
                                                        formula=paste0("auddiag6 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmaudco6 <- matrix(unlist(parLapply(cl=cl, glmaud6, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmaudse6 <- matrix(unlist(parLapply(cl=cl, glmaud6, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmaudres6b <- matrix(unlist(mi.meld(q=glmaudco6, se=glmaudse6)),nrow=1,ncol=2)

glmaud7 <- parLapply(cl=cl, dataaud7, function (x) {glm(data=x,
                                                            formula=paste0("auddiag7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Lvars))})
glmaudco7 <- matrix(unlist(parLapply(cl=cl, glmaud7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmaudse7 <- matrix(unlist(parLapply(cl=cl, glmaud7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmaudres7a <- matrix(unlist(mi.meld(q=glmaudco7, se=glmaudse7)),nrow=1,ncol=2)

glmaud7 <- parLapply(cl=cl, dataaud7, function (x) {glm(data=x,
                                                        formula=paste0("auddiag7 ~ parsup1 + parsup2 + parsup3 + parsup4 + parsup5 + ",Bvars))})
glmaudco7 <- matrix(unlist(parLapply(cl=cl, glmaud7, function (x) {coef(x)[2]+coef(x)[3]+coef(x)[4]+coef(x)[5]+coef(x)[6]})),nrow=50,ncol=1)
glmaudse7 <- matrix(unlist(parLapply(cl=cl, glmaud7, function (x) {vcov(x)[2,2] + vcov(x)[3,3] + vcov(x)[4,4] + vcov(x)[5,5] + vcov(x)[6,6]
  + 2*vcov(x)[2,3] + 2*vcov(x)[2,4] + 2*vcov(x)[2,5] + 2*vcov(x)[2,6]
  + 2*vcov(x)[3,4] + 2*vcov(x)[3,5] + 2*vcov(x)[3,6]
  + 2*vcov(x)[4,5] + 2*vcov(x)[4,6]
  + 2*vcov(x)[5,6]})),nrow=50,ncol=1)
glmaudres7b <- matrix(unlist(mi.meld(q=glmaudco7, se=glmaudse7)),nrow=1,ncol=2)

rm(list=c("dataaud6","dataaud7"))

glmresultsa <- matrix(c(glmbingeres6a,glmbingeres7a,glmharmsres6a,glmharmsres7a,glmabuseres6a,glmabuseres7a,glmdependres6a,glmdependres7a,glmaudres6a,glmaudres7a),byrow=TRUE,ncol=4,nrow=5)
rownames(glmresultsa) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(glmresultsa) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")

glmresultsb <- matrix(c(glmbingeres6b,glmbingeres7b,glmharmsres6b,glmharmsres7b,glmabuseres6b,glmabuseres7b,glmdependres6b,glmdependres7b,glmaudres6b,glmaudres7b),byrow=TRUE,ncol=4,nrow=5)
rownames(glmresultsb) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(glmresultsb) <- c("Wave 6 log(OR)","Wave 6 SE","Wave 7 log(OR)","Wave 7 SE")