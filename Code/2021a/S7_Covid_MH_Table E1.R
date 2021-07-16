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
  path <- "C:/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP40. COVID-19 Health Paper/Data/"
  .libPaths("C:/Users/pjclare/Dropbox/R Library") # R library path
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

# 2.1 Load original data
w10data <- read_dta(file=paste0(path,"Wave 10 Reduced.dta"))

##############################################################################
# 3. Set-up Parallel Backend
#-----------------------------------------------------------------------------

# 3.1 Set up clusters
cl <- makeCluster(4)
clusterEvalQ(cl, path <- "C:/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP40. COVID-19 Health Paper/Data/")
clusterEvalQ(cl, .libPaths("C:/Users/pjclare/Dropbox/R Library")) # R library path)
clusterEvalQ(cl, libs <- c("haven","mice","miceadds","parallel","dplyr","sjlabelled","psych"))
clusterEvalQ(cl, lapply(libs, library, character.only = TRUE))
clusterSetRNGStream(cl, 58613)

##############################################################################
# 3. Subsample compared to greater APSALS cohort
#-----------------------------------------------------------------------------

demog <- rbind(matrix(c(mean(w10data$age_10,na.rm=TRUE),sd(w10data$age_10,na.rm=TRUE),mean(merged_comp$age_10,na.rm=TRUE),sd(merged_comp$age_10,na.rm=TRUE)),ncol=2),
               matrix(c(prop.table(table(w10data$sex_10)),prop.table(table(merged_comp$sex_10))),ncol=2),
               matrix(c(prop.table(table(w10data$pinc_10)),prop.table(table(merged_comp$pinc_10))),ncol=2),
               matrix(c(prop.table(table(w10data$singlep_10)),prop.table(table(merged_comp$singlep_10))),ncol=2),
               matrix(c(prop.table(table(w10data$b_oldsib)),prop.table(table(merged_comp$b_oldsib))),ncol=2),
               matrix(c(prop.table(table(w10data$b_parborn)),prop.table(table(merged_comp$b_parborn))),ncol=2),
               matrix(c(prop.table(table(w10data$b_pareduc)),prop.table(table(merged_comp$b_pareduc))),ncol=2),
               matrix(c(prop.table(table(w10data$b_parrel)),prop.table(table(merged_comp$b_parrel))),ncol=2),
               matrix(c(prop.table(table(w10data$b_parempl)),prop.table(table(merged_comp$b_parempl))),ncol=2),
               matrix(c(mean(w10data$b_pardem,na.rm=TRUE),sd(w10data$b_pardem,na.rm=TRUE),mean(merged_comp$b_pardem,na.rm=TRUE),sd(merged_comp$b_pardem,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w10data$b_parres,na.rm=TRUE),sd(w10data$b_parres,na.rm=TRUE),mean(merged_comp$b_parres,na.rm=TRUE),sd(merged_comp$b_parres,na.rm=TRUE)),ncol=2),
               matrix(c(prop.table(table(w10data$b_seifa)),prop.table(table(merged_comp$b_seifa))),ncol=2),
               matrix(c(mean(w10data$peeruse_10,na.rm=TRUE),sd(w10data$peeruse_10,na.rm=TRUE),mean(merged_comp$peeruse_10,na.rm=TRUE),sd(merged_comp$peeruse_10,na.rm=TRUE)),ncol=2),
               matrix(c(mean(w10data$peerdis_10,na.rm=TRUE),sd(w10data$peerdis_10,na.rm=TRUE),mean(merged_comp$peerdis_10,na.rm=TRUE),sd(merged_comp$peerdis_10,na.rm=TRUE)),ncol=2))

colnames(demog) <- c("Full sample","COVID Survey")
rownames(demog) <- c("Age Mean","SD","Male","Female","Income $1-$12,999","$13,000-$31,199","$31,200-$67,599","$67,600+",
                     "Not from single parent household","From single parent household","No older sibling(s)*","Have older sibling(s)*",
                     "Parent not born in Australia*","Parent born in Australia*",
                     "Parent education* - High school","Diploma/Trade","University",
                     "Parent religiousity* - Not/a little","Pretty/Very",
                     "Parent employment* - Employed","In workforce","Not in workforce",
                     "Parent demandingness* Mean","SD","Parent responsiveness* Mean","SD",
                     "SEIFA* - Bottom third","Middle","Top third",
                     "Peer substance use Mean","SD","Peer disapproval of substance use Mean","SD")

testdata <- w10data[,c("zzC_ID","age_10","sex_10","pinc_10","singlep_10","b_oldsib","b_parborn","b_pareduc","b_parrel",
                      "b_parempl","b_pardem","b_parres","b_seifa","b_famhist","peeruse_10","peerdis_10")]
idlist <- as.numeric(levels(merged_comp$zzC_ID))[merged_comp$zzC_ID]
testdata$sample <- ifelse(is.element(testdata$zzC_ID,idlist),1,0)

tests <- rbind(c(t.test(age_10 ~ sample, data = testdata, var.equal=TRUE)$statistic,t.test(age_10 ~ sample, data = testdata, var.equal=TRUE)$parameter,t.test(age_10 ~ sample, data = testdata, var.equal=TRUE)$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$sex_10,testdata$sample))$statistic,chisq.test(table(testdata$sex_10,testdata$sample))$parameter,chisq.test(table(testdata$sex_10,testdata$sample))$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$pinc_10,testdata$sample))$statistic,chisq.test(table(testdata$pinc_10,testdata$sample))$parameter,chisq.test(table(testdata$pinc_10,testdata$sample))$p.value),
               c(NA,NA,NA),c(NA,NA,NA),c(NA,NA,NA),
               c(chisq.test(table(testdata$singlep_10,testdata$sample))$statistic,chisq.test(table(testdata$singlep_10,testdata$sample))$parameter,chisq.test(table(testdata$singlep_10,testdata$sample))$p.value),
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
               c(t.test(b_pardem ~ sample, data = testdata, var.equal=TRUE)$statistic,t.test(b_pardem ~ sample, data = testdata, var.equal=TRUE)$parameter,t.test(b_pardem ~ sample, data = testdata, var.equal=TRUE)$p.value),
               c(NA,NA,NA),
               c(t.test(b_parres ~ sample, data = testdata, var.equal=TRUE)$statistic,t.test(b_parres ~ sample, data = testdata, var.equal=TRUE)$parameter,t.test(b_parres ~ sample, data = testdata, var.equal=TRUE)$p.value),
               c(NA,NA,NA),
               c(chisq.test(table(testdata$b_seifa,testdata$sample))$statistic,chisq.test(table(testdata$b_seifa,testdata$sample))$parameter,chisq.test(table(testdata$b_seifa,testdata$sample))$p.value),
               c(NA,NA,NA),c(NA,NA,NA),
               c(t.test(peeruse_10 ~ sample, data = testdata, var.equal=TRUE)$statistic,t.test(peeruse_10 ~ sample, data = testdata, var.equal=TRUE)$parameter,t.test(peeruse_10 ~ sample, data = testdata, var.equal=TRUE)$p.value),
               c(NA,NA,NA),
               c(t.test(peerdis_10 ~ sample, data = testdata, var.equal=TRUE)$statistic,t.test(peerdis_10 ~ sample, data = testdata, var.equal=TRUE)$parameter,t.test(peerdis_10 ~ sample, data = testdata, var.equal=TRUE)$p.value),
               c(NA,NA,NA))

comb_table <- cbind(demog,tests)
write.csv(comb_table,file=paste0(path,"Table E1.csv"))