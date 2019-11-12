
## SYNTAX FILE 1                                             ##
## MULTIPLE IMPUTATION USING MIXED MODELS                    ##

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
library ("norm")

set.seed(76247)

fml <- c(-2,2,1,1,1,1,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

dataread <- as.data.frame(read_dta(paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/APSALS Analysis Data 20191028.dta")))
dataread$zzC_ID <- factor(dataread$zzC_ID)
dataread$zzwave <- factor(dataread$zzwave)
dataread$b_parrel <- factor(dataread$b_parrel)
dataread$b_parborn <- factor(dataread$b_parborn)
dataread$b_parempl <- factor(dataread$b_parempl)
dataread$b_seifa <- factor(dataread$b_seifa)
dataread$b_hinc <- factor(dataread$b_hinc)
dataread$b_sex <- factor(dataread$b_sex)
dataread$alcmoney <- factor(dataread$alcmoney)
dataread$singlep <- factor(dataread$singlep)
dataread$smoking <- factor(dataread$smoking)
dataread$othsup <- factor(dataread$othsup)
dataread$parsup <- factor(dataread$parsup)
dataread$bingef <- factor(dataread$bingef)
dataread$numharms <- factor(dataread$numharms)
dataread$abusediag <- factor(dataread$abusediag)
dataread$dependdiag <- factor(dataread$dependdiag)
dataread$auddiag <- factor(dataread$auddiag)

imp <- mitmlComplete(jomoImpute(data=dataread,type=fml,m=50,n.burn=100,n.iter=10),print="all")

save(imp,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Analysis Data/APSALS Imputed Data 20191028.RData"))


# # Number of imputations to use
# nimpute <- 50
# # List of time-varying variables
# timev <- c("cqcomp","homeacc","alcrules","alcmoney","singlep","smoking","cbclextn","cbcladn","cbclwdn","peeruse","peerdis","othsup","parsup","binge","anyharms","abusediag","dependdiag","auddiag")
# timec <- c("b_hhavguse","b_parmon","b_pardem","b_parres","b_parcon","b_parrel","b_parborn","b_parempl","b_famposi","b_famconf","b_seifa","b_hinc","b_age","b_sex")
# 
# # Reshape data into wide format and drop all lagged variables except those at baseline
# datawide <- dataread
# datawide <- reshape(datawide,
#                     v.names=timev,
#                     idvar="zzC_ID",
#                     timevar="zzwave",
#                     sep = "",
#                     direction="wide")
# datawide <- data.frame(datawide[,c(1:15,17:28,35:46,53:64,71:82,89:100,119:123,137:141)])
# # JM imputation via MVN
# # Run imputation and save as list of data frames
# datamatrix <- as.matrix(datawide[,-1])
# # datamatrix <- as.matrix(datawide[,5:36])
# s <- prelim.norm(datamatrix) #Drop id variable so it isn't used in imputation
# thetahat <- em.norm(s,maxits=10000)
# rngseed(269012)
# theta <- da.norm(s, thetahat, steps=100, showits=TRUE)
# ximp <- lapply(vector("list", length=nimpute),function (x) {data.frame(imp.norm(s,theta,datamatrix))})
# 
