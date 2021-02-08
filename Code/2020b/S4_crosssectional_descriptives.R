##############################################################################
##   
## APSALS Covid-19 Survey
## Alcohol analysis
## Date: 6 August 2020
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify paths to Katana/PC paths based on whether NCPUS is detected
if (Sys.getenv("NCPUS")!="") {
  .libPaths("/home/z3312911/RPackages")
  path <- "/home/z3312911/COVID/"
} else { # Manually defined for PC
  path <- "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/"
  .libPaths("/Users/pjclare/Dropbox (Sydney Uni)/R Library") # R library path
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","mice","miceadds","parallel","dplyr","sjlabelled","psych","Amelia")
install <- !libs %in% installed.packages() #| libs %in% old.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load and merge data files
#-----------------------------------------------------------------------------

# 2.1 Load original data and get order
load(file=paste0(path,"Full imputed data.RData"))
load(file=paste0(path,"Alcohol imputed data.RData"))
load(file="/Users/pjclare/Dropbox (Sydney Uni)/APSALS/COVID Survey/Data/Complete merged data.RData")
w9data <- read_dta("/Users/pjclare/Dropbox (Sydney Uni)/APSALS/COVID Survey/Data/Wave 9 Reduced.dta")

##############################################################################
# 3. Set-up Parallel Backend
#-----------------------------------------------------------------------------

# 3.1 Set up clusters
cl <- makeCluster(4)
clusterEvalQ(cl, path <- "C:/Users/pjclare/Desktop/APSALS/COVID Survey/Data/")
clusterEvalQ(cl, .libPaths("C:/Users/pjclare/Documents/My Dropbox/R Library")) # R library path)
clusterEvalQ(cl, libs <- c("haven","mice","miceadds","parallel","dplyr","sjlabelled","psych"))
clusterEvalQ(cl, lapply(libs, library, character.only = TRUE))
clusterSetRNGStream(cl, 58613)

##############################################################################
# 3. Table 1 - Subsample compared to greater APSALS cohort
#-----------------------------------------------------------------------------

w9data$ccalcfq <- w9data$ccalcfre_8 * w9data$ccalcqnt_8
merged_comp$ccalcfq <- merged_comp$ccalcfre_8 * merged_comp$ccalcqnt_8

demog <- rbind(matrix(c(mean(w9data$ccalcfq,na.rm=TRUE),sd(w9data$ccalcfq,na.rm=TRUE),mean(merged_comp$ccalcfq,na.rm=TRUE),sd(merged_comp$ccalcfq,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w9data$age_8,na.rm=TRUE),sd(w9data$age_8,na.rm=TRUE),mean(merged_comp$age_8,na.rm=TRUE),sd(merged_comp$age_8,na.rm=TRUE)),ncol=2),
               matrix(c(prop.table(table(w9data$sex_8)),prop.table(table(merged_comp$sex_8)[1:2])),ncol=2),
               matrix(c(prop.table(table(w9data$pinc_8)),prop.table(table(merged_comp$pinc_8))),ncol=2),
               matrix(c(prop.table(table(w9data$singlep_8)),prop.table(table(merged_comp$singlep_8))),ncol=2),
               matrix(c(prop.table(table(w9data$b_oldsib)),prop.table(table(merged_comp$b_oldsib))),ncol=2),
               matrix(c(prop.table(table(w9data$b_parborn)),prop.table(table(merged_comp$b_parborn))),ncol=2),
               matrix(c(prop.table(table(w9data$b_pareduc)),prop.table(table(merged_comp$b_pareduc))),ncol=2),
               matrix(c(prop.table(table(w9data$b_parrel)),prop.table(table(merged_comp$b_parrel))),ncol=2),
               matrix(c(prop.table(table(w9data$b_parempl)),prop.table(table(merged_comp$b_parempl))),ncol=2),
               matrix(c(mean(w9data$b_pardem,na.rm=TRUE),sd(w9data$b_pardem,na.rm=TRUE),mean(merged_comp$b_pardem,na.rm=TRUE),sd(merged_comp$b_pardem,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w9data$b_parres,na.rm=TRUE),sd(w9data$b_parres,na.rm=TRUE),mean(merged_comp$b_parres,na.rm=TRUE),sd(merged_comp$b_parres,na.rm=TRUE)),ncol=2),
               matrix(c(prop.table(table(w9data$b_seifa)),prop.table(table(merged_comp$b_seifa))),ncol=2),
               matrix(c(prop.table(table(w9data$b_famhist)),prop.table(table(merged_comp$b_famhist))),ncol=2),
               matrix(c(mean(w9data$peeruse_8,na.rm=TRUE),sd(w9data$peeruse_8,na.rm=TRUE),mean(merged_comp$peeruse_8,na.rm=TRUE),sd(merged_comp$peeruse_8,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w9data$peerdis_8,na.rm=TRUE),sd(w9data$peerdis_8,na.rm=TRUE),mean(merged_comp$peerdis_8,na.rm=TRUE),sd(merged_comp$peerdis_8,na.rm=TRUE)),ncol=2))

colnames(demog) <- c("Full sample","COVID Survey")
rownames(demog) <- c("Alcohol consumption","SD","Age Mean","SD","Male","Female","Income $1-$12,999","$13,000-$31,199","$31,200-$67,599","$67,600+",
                     "Not from single parent household","From single parent household","No older sibling(s)*","Have older sibling(s)*",
                     "Parent not born in Australia*","Parent born in Australia*",
                     "Parent education* - High school","Diploma/Trade","University",
                     "Parent religiousity* - Not/a little","Pretty/Very",
                     "Parent employment* - Employed","In workforce","Not in workforce",
                     "Parent demandingness* Mean","SD","Parent responsiveness* Mean","SD",
                     "SEIFA* - Bottom third","Middle","Top third",
                     "No Family history of alcohol problems*","No Family history of alcohol problems*",
                     "Peer substance use Mean","SD","Peer disapproval of substance use Mean","SD")

testdata <- w9data[,c("zzC_ID","ccalcfq","age_8","sex_8","pinc_8","singlep_8","b_oldsib","b_parborn","b_pareduc","b_parrel",
                      "b_parempl","b_pardem","b_parres","b_seifa","b_famhist","peeruse_8","peerdis_8")]
idlist <- as.numeric(levels(merged_comp$zzC_ID))[merged_comp$zzC_ID]
testdata$sample <- ifelse(is.element(testdata$zzC_ID,idlist),1,0)

tests <- rbind(c(t.test(ccalcfq ~ sample, data = testdata)$statistic,t.test(ccalcfq ~ sample, data = testdata)$parameter,t.test(ccalcfq ~ sample, data = testdata)$p.value),
               c(NA,NA,NA),
               c(t.test(age_8 ~ sample, data = testdata)$statistic,t.test(age_8 ~ sample, data = testdata)$parameter,t.test(age_8 ~ sample, data = testdata)$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$sex_8,testdata$sample))$statistic,chisq.test(table(testdata$sex_8,testdata$sample))$parameter,chisq.test(table(testdata$sex_8,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$pinc_8,testdata$sample))$statistic,chisq.test(table(testdata$pinc_8,testdata$sample))$parameter,chisq.test(table(testdata$pinc_8,testdata$sample))$p.value),
               c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA),
               c(chisq.test(table(testdata$singlep_8,testdata$sample))$statistic,chisq.test(table(testdata$singlep_8,testdata$sample))$parameter,chisq.test(table(testdata$singlep_8,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$b_oldsib,testdata$sample))$statistic,chisq.test(table(testdata$b_oldsib,testdata$sample))$parameter,chisq.test(table(testdata$b_oldsib,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$b_parborn,testdata$sample))$statistic,chisq.test(table(testdata$b_parborn,testdata$sample))$parameter,chisq.test(table(testdata$b_parborn,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$b_pareduc,testdata$sample))$statistic,chisq.test(table(testdata$b_pareduc,testdata$sample))$parameter,chisq.test(table(testdata$b_pareduc,testdata$sample))$p.value),
               c(NA,NA,NA),c(NA,NA,NA),
               c(chisq.test(table(testdata$b_parrel,testdata$sample))$statistic,chisq.test(table(testdata$b_parrel,testdata$sample))$parameter,chisq.test(table(testdata$b_parrel,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$b_parempl,testdata$sample))$statistic,chisq.test(table(testdata$b_parempl,testdata$sample))$parameter,chisq.test(table(testdata$b_parempl,testdata$sample))$p.value),
               c(NA,NA,NA),c(NA,NA,NA),
               c(t.test(b_pardem ~ sample, data = testdata)$statistic,t.test(b_pardem ~ sample, data = testdata)$parameter,t.test(b_pardem ~ sample, data = testdata)$p.value),
               c(NA,NA,NA),
               c(t.test(b_parres ~ sample, data = testdata)$statistic,t.test(b_parres ~ sample, data = testdata)$parameter,t.test(b_parres ~ sample, data = testdata)$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$b_seifa,testdata$sample))$statistic,chisq.test(table(testdata$b_seifa,testdata$sample))$parameter,chisq.test(table(testdata$b_seifa,testdata$sample))$p.value),
               c(NA,NA,NA),c(NA,NA,NA),
               c(chisq.test(table(testdata$b_famhist,testdata$sample))$statistic,chisq.test(table(testdata$b_famhist,testdata$sample))$parameter,chisq.test(table(testdata$b_famhist,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(t.test(peeruse_8 ~ sample, data = testdata)$statistic,t.test(peeruse_8 ~ sample, data = testdata)$parameter,t.test(peeruse_8 ~ sample, data = testdata)$p.value),
               c(NA,NA,NA),
               c(t.test(peerdis_8 ~ sample, data = testdata)$statistic,t.test(peerdis_8 ~ sample, data = testdata)$parameter,t.test(peerdis_8 ~ sample, data = testdata)$p.value),
               c(NA,NA,NA))

comb_table <- cbind(demog,tests)

View(comb_table)

##############################################################################
# 4. Table 4 - Alcohol-related harms
#-----------------------------------------------------------------------------

numhrmmn <- do.call("rbind",parLapply(cl,imputed_alc[1:50],function (x) {
  res <- c(describe(x$ccnumharm_11)$mean,describe(x$ccnumharm_11)$se,
           describe(x$ccnumharm_12)$mean,describe(x$ccnumharm_12)$se)
}))
numhrmres <- matrix(unlist(mi.meld(q=numhrmmn[,c(1,3)],se=numhrmmn[,c(2,4)]))[c(1,3,2,4)],ncol=2)

anyhrm <- colMeans(do.call("rbind",parLapply(cl,imputed_alc[1:50],function (x) {
  y <- x[,c("ccnumharm_11","ccnumharm_12")]
  anyhrm_11 <- ifelse(y$ccnumharm_11==0,0,1)
  anyhrm_12 <- ifelse(y$ccnumharm_12==0,0,1)
  prop <- colMeans(cbind(anyhrm_11,anyhrm_12), na.rm = TRUE)
})))


hrms11prop <- do.call("rbind",parLapply(cl,imputed_alc[1:50],function (x) {
  y <- x[,c("ccalchrma_11","ccalchrmb_11","ccalchrmc_11","ccalchrmd_11","ccalchrme_11","ccalchrmf_11","ccalchrmg_11","ccalcinjury_11")]
  prop <- colMeans(y, na.rm = TRUE)
}))

hrms12prop <- do.call("rbind",parLapply(cl,imputed_alc[1:50],function (x) {
  y <- x[,c("ccalchrma_12","ccalchrmb_12","ccalchrmc_12","ccalchrmd_12","ccalchrme_12","ccalchrmf_12","ccalchrmg_12","ccalcinjury_12")]
  prop <- colMeans(y, na.rm = TRUE)
}))

hrmssum <- rbind(numhrmres,
                 anyhrm,
                 matrix(cbind(colMeans(hrms11prop),colMeans(hrms12prop)),ncol=2))

colnames(hrmssum) <- c("February 2020","May-June 2020")
rownames(hrmssum) <- c("Total number of harms (mean)",
                       "Total number of harms (se)",
                       "Reported any harms",
                       "Were sick after drinking",
                       "Had a hangover",
                       "Were unable to remember what happened",
                       "Got in a physical fight",
                       "Damaged something",
                       "Got into trouble with friends",
                       "Had sex that they later regretted",
                       "Had an accident, injury or fall")
hrmssum

##############################################################################
# 5. Table 5 and EX - Change in consumption
#-----------------------------------------------------------------------------

chngprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,"ccalcchange"])-1
  tab <- table(y)
  prop <- c(tab[1]/443,tab[2]/443,tab[3]/443)
})
chngprop <- do.call("rbind",chngprop[1:50])
chngprop <- colMeans(chngprop)

decprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,c("ccalcchange","ccalcdeca","ccalcdecb","ccalcdecc","ccalcdecd","ccalcdece","ccalcdecf",
                       "ccalcdecg","ccalcdech","ccalcdeci","ccalcdecj","ccalcdeck","ccalcdecl","ccalcdecm")])-1
  y[2:14] <- lapply(y[2:14], function(x) replace(x, y$ccalcchange == 0 | y$ccalcchange == 2, NA))
  prop <- colMeans(y[2:14], na.rm = TRUE)
})
decprop <- do.call("rbind",decprop[1:50])
decprop <- colMeans(decprop)

incprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,c("ccalcchange","ccalcinca","ccalcincb","ccalcincd","ccalcince","ccalcincf",
                       "ccalcincg","ccalcinch","ccalcinci")])-1
  y[2:9] <- lapply(y[2:9], function(x) replace(x, y$ccalcchange == 1 | y$ccalcchange == 2, NA))
  prop <- colMeans(y[2:9], na.rm = TRUE)
})
incprop <- do.call("rbind",incprop[1:50])
incprop <- colMeans(incprop)

chgcomp <- parLapply(cl,imputed_alc,function (x) {
  y <- x[,c("zzC_ID","ccalcchange","ccalcfre_11","ccalcfre_12","ccalcqnt_11","ccalcqnt_12")]
  y$ccalcfq_11 <- y$ccalcfre_11 * y$ccalcqnt_11
  y$ccalcfq_12 <- y$ccalcfre_12 * y$ccalcqnt_12
  y$ccobchg <- ifelse(y$ccalcfq_11==y$ccalcfq_12,2,
                      ifelse(y$ccalcfq_11<y$ccalcfq_12,0,1))
  y$ccobchg <- factor(y$ccobchg,labels=c("Increase","Decrease","Stayed the same"))
  tab <- table(y$ccalcchang,y$ccobchg)
  prop <- tab/443
  prop <- matrix(prop,nrow=1)
  prop
})
chgcomp <- do.call("rbind",chgcomp[1:50])
chgcomp <- matrix(colMeans(chgcomp),nrow=3)
  
##############################################################################
# 6. COVID experiences
#-----------------------------------------------------------------------------


slfisoprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,"cccovidslfiso"])-1
  tab <- table(y)
  prop <- c(tab[1]/443,tab[2]/443)
})

qntprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,"cccovidqnt"])-1
  tab <- table(y)
  prop <- c(tab[1]/443,tab[2]/443)
})