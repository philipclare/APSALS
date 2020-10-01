##############################################################################
##   
## APSALS Covid-19 Survey
## Finalise data after imputation on Katana
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
  path <- "C:/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/"
  .libPaths("C:/Users/pjclare/Documents/My Dropbox/R Library") # R library path
}

# 1.2. Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("haven","mice","miceadds","parallel","dplyr","labelled","VIM","semTools","reshape2")
install <- !libs %in% installed.packages() #| libs %in% old.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

##############################################################################
# 2. Load and merge data files
#-----------------------------------------------------------------------------

# 2.1 Load original data and get order
load(file=paste0(path,"Data for imputation.RData"))
merged_data[74:89] <- lapply(merged_data[74:89],function(x) as.integer(x)-1)
merged_data$ccnumharm_11 <- merged_data$ccalchrma_11 + merged_data$ccalchrmb_11 + merged_data$ccalchrmc_11 + merged_data$ccalchrmd_11 + merged_data$ccalchrme_11 + merged_data$ccalchrmf_11 + merged_data$ccalchrmg_11
merged_data$ccnumharm_12 <- merged_data$ccalchrma_12 + merged_data$ccalchrmb_12 + merged_data$ccalchrmc_12 + merged_data$ccalchrmd_12 + merged_data$ccalchrme_12 + merged_data$ccalchrmf_12 + merged_data$ccalchrmg_12
merged_data <- merged_data[,c(1:73,131,74:81,132,82:130)]
varorder<-summary(aggr(merged_data))$missings$Variable
varorder[32:33] <- c("ccalcqnt_11","ccalcqnt_12")

# 2.2 Load and combine imputed data into single list
list <- seq(1,50)
imputed <- lapply(list, function(x) {
  load(file=paste0(path,"Imputed split/Imputed data ",1,".RData"))
  dat <- mids2datlist(imp_data)[[1]]
  table(dat$ccstndt_11)
  dat$ccalcqnt_11 <- case_when(dat$ccalcq_11 >= 13 ~ 13,
                               dat$ccalcq_11 >= 11 ~ 11.5,
                               dat$ccalcq_11 >= 7 ~ 8.5,
                               dat$ccalcq_11 >= 5 ~ 5.5,
                               dat$ccalcq_11 >= 3 ~ 3.5,
                               dat$ccalcq_11 >= 1 ~ 1.5,
                               dat$ccalcq_11 > 0 ~ 0.1,
                               dat$ccalcq_11 == 0 ~ 0)
  dat$ccalcqnt_12 <- case_when(dat$ccalcq_12 >= 13 ~ 13,
                               dat$ccalcq_12 >= 11 ~ 11.5,
                               dat$ccalcq_12 >= 7 ~ 8.5,
                               dat$ccalcq_12 >= 5 ~ 5.5,
                               dat$ccalcq_12 >= 3 ~ 3.5,
                               dat$ccalcq_12 >= 1 ~ 1.5,
                               dat$ccalcq_12 > 0 ~ 0.1,
                               dat$ccalcq_12 == 0 ~ 0)
  dat$datecom_9 <- as.Date(dat$datecom_9, origin = "1970-01-01")
  dat$datecom_10 <- as.Date(dat$datecom_10, origin = "1970-01-01")
  dat$datecom_11 <- as.Date(dat$datecom_11, origin = "1970-01-01")
  dat$datecom_12 <- as.Date(dat$datecom_12, origin = "1970-01-01")
  list <- c("ccalchrma_11","ccalchrmb_11","ccalchrmc_11","ccalchrmd_11","ccalchrme_11","ccalchrmf_11","ccalchrmg_11","ccalcinjury_11",
            "ccalchrma_12","ccalchrmb_12","ccalchrmc_12","ccalchrmd_12","ccalchrme_12","ccalchrmf_12","ccalchrmg_12","ccalcinjury_12")
  dat[list] <- lapply(dat[list],function(x) as.integer(x)-1)
  dat$ccnumharm_11 <- dat$ccalchrma_11 + dat$ccalchrmb_11 + dat$ccalchrmc_11 + dat$ccalchrmd_11 + dat$ccalchrme_11 + dat$ccalchrmf_11 + dat$ccalchrmg_11
  dat$ccnumharm_12 <- dat$ccalchrma_12 + dat$ccalchrmb_12 + dat$ccalchrmc_12 + dat$ccalchrmd_12 + dat$ccalchrme_12 + dat$ccalchrmf_12 + dat$ccalchrmg_12
  dat <- dat[,varorder]
  dat
})

save(imputed,file=paste0(path,"Full imputed data.RData"))
imputed_alc <- lapply(imputed, function(x) {x[,c(1:107,117:119,121:123)]})
save(imputed_alc,file=paste0(path,"Alcohol imputed data.RData"))

merged_data$ccalcqnt_11 <- case_when(merged_data$ccalcq_11 >= 13 ~ 13,
                                     merged_data$ccalcq_11 >= 11 ~ 11.5,
                                     merged_data$ccalcq_11 >= 7 ~ 8.5,
                                     merged_data$ccalcq_11 >= 5 ~ 5.5,
                                     merged_data$ccalcq_11 >= 3 ~ 3.5,
                                     merged_data$ccalcq_11 >= 1 ~ 1.5,
                                     merged_data$ccalcq_11 > 0 ~ 0.1,
                                     merged_data$ccalcq_11 == 0 ~ 0)
merged_data$ccalcqnt_12 <- case_when(merged_data$ccalcq_12 >= 13 ~ 13,
                                     merged_data$ccalcq_12 >= 11 ~ 11.5,
                                     merged_data$ccalcq_12 >= 7 ~ 8.5,
                                     merged_data$ccalcq_12 >= 5 ~ 5.5,
                                     merged_data$ccalcq_12 >= 3 ~ 3.5,
                                     merged_data$ccalcq_12 >= 1 ~ 1.5,
                                     merged_data$ccalcq_12 > 0 ~ 0.1,
                                     merged_data$ccalcq_12 == 0 ~ 0)
merged_data$datecom_9 <- as.Date(merged_data$datecom_9, origin = "1970-01-01")
merged_data$datecom_10 <- as.Date(merged_data$datecom_10, origin = "1970-01-01")
merged_data$datecom_11 <- as.Date(merged_data$datecom_11, origin = "1970-01-01")
merged_data$datecom_12 <- as.Date(merged_data$datecom_12, origin = "1970-01-01")
merged_data <- merged_data[,varorder]
merged_alc <- merged_data[,c(1:107,117:119,121:123)]
merged_mh <- merged_data[,c(1:25,108:116,120,124:132)]
imputed_alc[[51]] <- merged_alc

imputed_alc_long <- do.call("rbind",lapply(seq(1,51), function(x,imputed_alc) {
  y <- imputed_alc[[x]]
  y[,c("ccstndt_9","ccstndt_10")] <- NA
  y[,c("ccempl_9","ccempl_10")] <- NA
  y[,c("ccalcmax_9","ccalcmax_10")] <- NA
  y[,c("ccecstfrq_9","ccecstfrq_10")] <- NA
  y[,c("ccmethfrq_9","ccmethfrq_10")] <- NA
  y[,c("ccalcalone_9","ccalcalone_10")] <- NA
  y[,c("ccalcothers_9","ccalcothers_10")] <- NA
  y[,c("ccalcvirtual_9","ccalcvirtual_10")] <- NA
  y[,c("ccalcdlvr_9","ccalcdlvr_10")] <- NA
  y[,c("ccnumharm_9","ccnumharm_10")] <- NA
  y[,c("cchlthskb_9","cchlthskb_10")] <- NA
  y[,c("cchlthskc_9","cchlthskc_10")] <- NA
  y[,c("cchlthskd_9","cchlthskd_10")] <- NA
  
  y$ccstndt_9 <- factor(y$ccstndt_9,levels=c(0,1),labels=c("No","Yes"))
  y$ccstndt_10 <- factor(y$ccstndt_10,levels=c(0,1),labels=c("No","Yes"))
  y$ccempl_9 <- factor(y$ccempl_9,levels=c(0,1),labels=c("No","Yes"))
  y$ccempl_10 <- factor(y$ccempl_10,levels=c(0,1),labels=c("No","Yes"))
  y$ccecstfrq_9 <- ordered(y$ccecstfrq_9,levels=c(0,1,2),labels=c("None","Less than daily","Daily"))
  y$ccecstfrq_10 <- ordered(y$ccecstfrq_10,levels=c(0,1,2),labels=c("None","Less than daily","Daily"))
  y$ccmethfrq_9 <- ordered(y$ccmethfrq_9,levels=c(0,1,2),labels=c("None","Less than daily","Daily"))
  y$ccmethfrq_10 <- ordered(y$ccmethfrq_10,levels=c(0,1,2),labels=c("None","Less than daily","Daily"))
  y$ccalcdlvr_9 <- ordered(y$ccalcdlvr_9,levels=c(0,1,2),labels=c("Never","Once","More than once"))
  y$ccalcdlvr_10 <- ordered(y$ccalcdlvr_10,levels=c(0,1,2),labels=c("Never","Once","More than once"))
  y$cchlthskb_9 <- ordered(y$cchlthskb_9,levels=c(0,1),labels=c("No","Yes"))
  y$cchlthskb_10 <- ordered(y$cchlthskb_10,levels=c(0,1),labels=c("No","Yes"))
  y$cchlthskc_9 <- ordered(y$cchlthskc_9,levels=c(0,1),labels=c("No","Yes"))
  y$cchlthskc_10 <- ordered(y$cchlthskc_10,levels=c(0,1),labels=c("No","Yes"))
  y$cchlthskd_9 <- ordered(y$cchlthskd_9,levels=c(0,1),labels=c("No","Yes"))
  y$cchlthskd_10 <- ordered(y$cchlthskd_10,levels=c(0,1),labels=c("No","Yes"))
  
  y <- y[,c("zzC_ID","b_seifa","b_famhist","b_oldsib","sex_9","pinc_9","age_9","peeruse_9","peerdis_9","datecom_9","datecom_10","datecom_11","datecom_12",
            "ccstndt_9","ccstndt_10","ccstndt_11","ccstndt_12","ccempl_9","ccempl_10","ccempl_11","ccempl_12","ccalcfre_9","ccalcfre_10","ccalcfre_11","ccalcfre_12",
            "ccalcqnt_9","ccalcqnt_10","ccalcqnt_11","ccalcqnt_12","ccalcbngf_9","ccalcbngf_10","ccalcbngf_11","ccalcbngf_12","ccalcmax_9","ccalcmax_10","ccalcmax_11",
            "ccalcmax_12","ccalcalone_9","ccalcalone_10","ccalcalone_11","ccalcalone_12","ccalcothers_9","ccalcothers_10","ccalcothers_11","ccalcothers_12",
            "ccalcvirtual_9","ccalcvirtual_10","ccalcvirtual_11","ccalcvirtual_12","ccalcdlvr_9","ccalcdlvr_10","ccalcdlvr_11","ccalcdlvr_12","ccalcmon_9","ccalcmon_10",
            "ccalcmon_11","ccalcmon_12","ccnumharm_9","ccnumharm_10","ccnumharm_11","ccnumharm_12","ccsmkfrq_9","ccsmkfrq_10","ccsmkfrq_11","ccsmkfrq_12","ccecigfrq_9",
            "ccecigfrq_10","ccecigfrq_11","ccecigfrq_12","cccanfrq_9","cccanfrq_10","cccanfrq_11","cccanfrq_12","ccecstfrq_9","ccecstfrq_10","ccecstfrq_11","ccecstfrq_12",
            "ccmethfrq_9","ccmethfrq_10","ccmethfrq_11","ccmethfrq_12","cchlthskb_9","cchlthskb_10","cchlthskb_11","cchlthskb_12","cchlthskc_9","cchlthskc_10",
            "cchlthskc_11","cchlthskc_12","cchlthskd_9","cchlthskd_10","cchlthskd_11","cchlthskd_12")]
  
  y <- reshape(y,
               direction="long",
               idvar="zzC_ID",
               varying=list(c("datecom_9","datecom_10","datecom_11","datecom_12"),
                            c("ccstndt_9","ccstndt_10","ccstndt_11","ccstndt_12"),
                            c("ccempl_9","ccempl_10","ccempl_11","ccempl_12"),
                            c("ccalcfre_9","ccalcfre_10","ccalcfre_11","ccalcfre_12"),
                            c("ccalcqnt_9","ccalcqnt_10","ccalcqnt_11","ccalcqnt_12"),
                            c("ccalcbngf_9","ccalcbngf_10","ccalcbngf_11","ccalcbngf_12"),
                            c("ccalcmax_9","ccalcmax_10","ccalcmax_11","ccalcmax_12"),
                            c("ccalcalone_9","ccalcalone_10","ccalcalone_11","ccalcalone_12"),
                            c("ccalcothers_9","ccalcothers_10","ccalcothers_11","ccalcothers_12"),
                            c("ccalcvirtual_9","ccalcvirtual_10","ccalcvirtual_11","ccalcvirtual_12"),
                            c("ccalcdlvr_9","ccalcdlvr_10","ccalcdlvr_11","ccalcdlvr_12"),
                            c("ccalcmon_9","ccalcmon_10","ccalcmon_11","ccalcmon_12"),
                            c("ccnumharm_9","ccnumharm_10","ccnumharm_11","ccnumharm_12"),
                            c("ccsmkfrq_9","ccsmkfrq_10","ccsmkfrq_11","ccsmkfrq_12"),
                            c("ccecigfrq_9","ccecigfrq_10","ccecigfrq_11","ccecigfrq_12"),
                            c("cccanfrq_9","cccanfrq_10","cccanfrq_11","cccanfrq_12"),
                            c("ccecstfrq_9","ccecstfrq_10","ccecstfrq_11","ccecstfrq_12"),
                            c("ccmethfrq_9","ccmethfrq_10","ccmethfrq_11","ccmethfrq_12"),
                            c("cchlthskb_9","cchlthskb_10","cchlthskb_11","cchlthskb_12"),
                            c("cchlthskc_9","cchlthskc_10","cchlthskc_11","cchlthskc_12"),
                            c("cchlthskd_9","cchlthskd_10","cchlthskd_11","cchlthskd_12")),
               v.names=c("datecom","ccstndt","ccempl","ccalcfre","ccalcqnt","ccalcbngf","ccalcmax","ccalcalone","ccalcothers","ccalcvirtual",
                         "ccalcdlvr","ccalcmon","ccnumharm","ccsmkfrq","ccecigfrq","cccanfrq","ccecstfrq","ccmethfrq","cchlthskb","cchlthskc","cchlthskd"),
               sep="_",
               timevar="zzwave")
  y<-y[,c(1,10:11,2:9,12:31)]
  y$imp <- x
  y
},imputed_alc=imputed_alc))

write_dta(imputed_alc_long,path=paste0(path,"Alcohol imputed data - long.dta"))

imputed_mh <- lapply(imputed, function(x) {x[,c(1:25,108:116,120,124:132)]})
imputed_mh[[51]] <- merged_mh
save(imputed_mh,file=paste0(path,"Mental health imputed data.RData"))
imputed_mh_df <- do.call("rbind",lapply(seq(1,51), function(x,imputed_mh) {
  y <- imputed_mh[[x]]
  y$imp <- x
  y
},imputed_mh=imputed_mh))
write_dta(imputed_mh_df,path=paste0(path,"Mental health imputed data.dta"))