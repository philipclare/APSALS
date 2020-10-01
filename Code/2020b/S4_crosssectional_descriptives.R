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
  path <- "C:/Users/pjclare/Desktop/APSALS/COVID Survey/Data/"
  .libPaths("C:/Users/pjclare/Documents/My Dropbox/R Library") # R library path
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
load(file=paste0(path,"Complete merged data.RData"))
w9data <- read_dta(file=paste0(path,"Wave 9 Reduced.dta"))

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
# 3. Subsample compared to greater APSALS cohort
#-----------------------------------------------------------------------------

demog <- rbind(matrix(c(mean(w9data$age_9,na.rm=TRUE),sd(w9data$age_9,na.rm=TRUE),mean(merged_comp$age_9,na.rm=TRUE),sd(merged_comp$age_9,na.rm=TRUE)),ncol=2),
               matrix(c(prop.table(table(w9data$sex_9)),prop.table(table(merged_comp$sex_9))),ncol=2),
               matrix(c(prop.table(table(w9data$pinc_9)),prop.table(table(merged_comp$pinc_9))),ncol=2),
               matrix(c(prop.table(table(w9data$singlep_9)),prop.table(table(merged_comp$singlep_9))),ncol=2),
               matrix(c(prop.table(table(w9data$b_oldsib)),prop.table(table(merged_comp$b_oldsib))),ncol=2),
               matrix(c(prop.table(table(w9data$b_parborn)),prop.table(table(merged_comp$b_parborn))),ncol=2),
               matrix(c(prop.table(table(w9data$b_pareduc)),prop.table(table(merged_comp$b_pareduc))),ncol=2),
               matrix(c(prop.table(table(w9data$b_parrel)),prop.table(table(merged_comp$b_parrel))),ncol=2),
               matrix(c(prop.table(table(w9data$b_parempl)),prop.table(table(merged_comp$b_parempl))),ncol=2),
               matrix(c(mean(w9data$b_pardem,na.rm=TRUE),sd(w9data$b_pardem,na.rm=TRUE),mean(merged_comp$b_pardem,na.rm=TRUE),sd(merged_comp$b_pardem,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w9data$b_parres,na.rm=TRUE),sd(w9data$b_parres,na.rm=TRUE),mean(merged_comp$b_parres,na.rm=TRUE),sd(merged_comp$b_parres,na.rm=TRUE)),ncol=2),
               matrix(c(prop.table(table(w9data$b_seifa)),prop.table(table(merged_comp$b_seifa))),ncol=2),
               matrix(c(prop.table(table(w9data$b_famhist)),prop.table(table(merged_comp$b_famhist))),ncol=2),
               matrix(c(mean(w9data$peeruse_9,na.rm=TRUE),sd(w9data$peeruse_9,na.rm=TRUE),mean(merged_comp$peeruse_9,na.rm=TRUE),sd(merged_comp$peeruse_9,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w9data$peerdis_9,na.rm=TRUE),sd(w9data$peerdis_9,na.rm=TRUE),mean(merged_comp$peerdis_9,na.rm=TRUE),sd(merged_comp$peerdis_9,na.rm=TRUE)),ncol=2))

colnames(demog) <- c("Full sample","COVID Survey")
rownames(demog) <- c("Age Mean","SD","Male","Female","Other","Income $1-$12,999","$13,000-$31,199","$31,200-$67,599","$67,600+",
                     "Not from single parent household","From single parent household","No older sibling(s)*","Have older sibling(s)*",
                     "Parent not born in Australia*","Parent born in Australia*",
                     "Parent education* - High school","Diploma/Trade","University",
                     "Parent religiousity* - Not/a little","Pretty/Very",
                     "Parent employment* - Employed","In workforce","Not in workforce",
                     "Parent demandingness* Mean","SD","Parent responsiveness* Mean","SD",
                     "SEIFA* - Bottom third","Middle","Top third",
                     "No Family history of alcohol problems*","No Family history of alcohol problems*",
                     "Peer substance use Mean","SD","Peer disapproval of substance use Mean","SD")

##############################################################################
# 4. Reasons for change in consumption
#-----------------------------------------------------------------------------

chngprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,"ccalcchange"])-1
  tab <- table(y)
  prop <- c(tab[1]/443,tab[2]/443,tab[3]/443)
})
chngprop <- do.call("rbind",chngprop)
chngprop <- colMeans(chngprop)

decprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,c("ccalcchange","ccalcdeca","ccalcdecb","ccalcdecc","ccalcdecd","ccalcdece","ccalcdecf",
                       "ccalcdecg","ccalcdech","ccalcdeci","ccalcdecj","ccalcdeck","ccalcdecl","ccalcdecm")])-1
  y[2:14] <- lapply(y[2:14], function(x) replace(x, y$ccalcchange == 0 | y$ccalcchange == 2, NA))
  prop <- colMeans(y[2:14], na.rm = TRUE)
})
decprop <- do.call("rbind",decprop)
decprop <- colMeans(decprop)

incprop <- parLapply(cl,imputed_alc,function (x) {
  y <- as_numeric(x[,c("ccalcchange","ccalcinca","ccalcincb","ccalcincd","ccalcince","ccalcincf",
                       "ccalcincg","ccalcinch","ccalcinci")])-1
  y[2:9] <- lapply(y[2:9], function(x) replace(x, y$ccalcchange == 1 | y$ccalcchange == 2, NA))
  prop <- colMeans(y[2:9], na.rm = TRUE)
})
incprop <- do.call("rbind",incprop)
incprop <- colMeans(incprop)

##############################################################################
# 5. Alcohol-related harms
#-----------------------------------------------------------------------------

numhrmmn <- do.call("rbind",parLapply(cl,imputed_alc,function (x) {
  res <- c(describe(x$ccnumharm_11)$mean,describe(x$ccnumharm_11)$se,
           describe(x$ccnumharm_12)$mean,describe(x$ccnumharm_12)$se)
}))
numhrmres <- matrix(unlist(mi.meld(q=numhrmmn[,c(1,3)],se=numhrmmn[,c(2,4)]))[c(1,3,2,4)],ncol=2)

anyhrm <- colMeans(do.call("rbind",parLapply(cl,imputed_alc,function (x) {
  y <- x[,c("ccnumharm_11","ccnumharm_12")]
  anyhrm_11 <- ifelse(y$ccnumharm_11==0,0,1)
  anyhrm_12 <- ifelse(y$ccnumharm_12==0,0,1)
  prop <- colMeans(cbind(anyhrm_11,anyhrm_12), na.rm = TRUE)
})))


hrms11prop <- do.call("rbind",parLapply(cl,imputed,function (x) {
  y <- x[,c("ccalchrma_11","ccalchrmb_11","ccalchrmc_11","ccalchrmd_11","ccalchrme_11","ccalchrmf_11","ccalchrmg_11","ccalcinjury_11")]
  prop <- colMeans(y, na.rm = TRUE)
}))

hrms12prop <- do.call("rbind",parLapply(cl,imputed,function (x) {
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