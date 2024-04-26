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
  path <- "/Users/pjclare/Dropbox (Sydney Uni)/APSALS/COVID Survey/Data/"
  savepath <- "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/"
  .libPaths("/Users/pjclare/Dropbox (Sydney Uni)/R Library") # R library path
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
hrmlist <- c("ccalchrma_11","ccalchrmb_11","ccalchrmc_11","ccalchrmd_11","ccalchrme_11","ccalchrmf_11","ccalchrmg_11","ccalcinjury_11",
          "ccalchrma_12","ccalchrmb_12","ccalchrmc_12","ccalchrmd_12","ccalchrme_12","ccalchrmf_12","ccalchrmg_12","ccalcinjury_12")
merged_data[hrmlist] <- lapply(merged_data[hrmlist],function(x) as.integer(x)-1)
merged_data$ccnumharm_11 <- merged_data$ccalchrma_11 + merged_data$ccalchrmb_11 + merged_data$ccalchrmc_11 + merged_data$ccalchrmd_11 + merged_data$ccalchrme_11 + merged_data$ccalchrmf_11 + merged_data$ccalchrmg_11
merged_data$ccnumharm_12 <- merged_data$ccalchrma_12 + merged_data$ccalchrmb_12 + merged_data$ccalchrmc_12 + merged_data$ccalchrmd_12 + merged_data$ccalchrme_12 + merged_data$ccalchrmf_12 + merged_data$ccalchrmg_12
merged_data <- merged_data[,c(1:109,162,110:117,163,118:161)]
varorder <- summary(aggr(merged_data))$missings$Variable
varorder[58:59] <- c("ccalcqnt_11","ccalcqnt_12")
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
merged_data$datecom_8 <- as.Date(merged_data$datecom_8, origin = "1970-01-01")
merged_data$datecom_9 <- as.Date(merged_data$datecom_9, origin = "1970-01-01")
merged_data$datecom_10 <- as.Date(merged_data$datecom_10, origin = "1970-01-01")
merged_data$datecom_11 <- as.Date(merged_data$datecom_11, origin = "1970-01-01")
merged_data$datecom_12 <- as.Date(merged_data$datecom_12, origin = "1970-01-01")
merged_data <- merged_data[,varorder]

# 2.2 Load and combine imputed data into single list
list <- seq(1,50)
imputed <- lapply(list, function(x) {
  load(file=paste0(path,"Imputed split/Imputed data ",x,".RData"))
  dat <- mids2datlist(imp_data)[[1]]
  table(dat$ccstndt_10)
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
  dat$datecom_8 <- as.Date(dat$datecom_8, origin = "1970-01-01")
  dat$datecom_9 <- as.Date(dat$datecom_9, origin = "1970-01-01")
  dat$datecom_10 <- as.Date(dat$datecom_10, origin = "1970-01-01")
  dat$datecom_11 <- as.Date(dat$datecom_11, origin = "1970-01-01")
  dat$datecom_12 <- as.Date(dat$datecom_12, origin = "1970-01-01")
  hrmlist <- c("ccalchrma_11","ccalchrmb_11","ccalchrmc_11","ccalchrmd_11","ccalchrme_11","ccalchrmf_11","ccalchrmg_11","ccalcinjury_11",
               "ccalchrma_12","ccalchrmb_12","ccalchrmc_12","ccalchrmd_12","ccalchrme_12","ccalchrmf_12","ccalchrmg_12","ccalcinjury_12")
  dat[hrmlist] <- lapply(dat[hrmlist],function(x) as.integer(x)-1)
  dat$ccnumharm_11 <- dat$ccalchrma_11 + dat$ccalchrmb_11 + dat$ccalchrmc_11 + dat$ccalchrmd_11 + dat$ccalchrme_11 + dat$ccalchrmf_11 + dat$ccalchrmg_11
  dat$ccnumharm_12 <- dat$ccalchrma_12 + dat$ccalchrmb_12 + dat$ccalchrmc_12 + dat$ccalchrmd_12 + dat$ccalchrme_12 + dat$ccalchrmf_12 + dat$ccalchrmg_12
  dat <- dat[,varorder]
  dat
  
})

imputed[[51]] <- merged_data

save(imputed,file=paste0(savepath,"Full imputed data.RData"))

imputed_alc <- lapply(imputed, function(x) {x[,c(1:119)]})
save(imputed_alc,file=paste0(savepath,"Alcohol imputed data.RData"))

imputed_alc_long <- do.call("rbind",lapply(seq(1,51), function(x,imputed) {
  y <- imputed[[x]]
  y[,c("ccstndt_8","ccstndt_9")] <- NA
  y[,c("ccempl_8","ccempl_9")] <- NA
  y[,c("ccalcmax_8","ccalcmax_9","ccalcmax_10")] <- NA
  y[,c("ccecstfrq_8","ccecstfrq_9","ccecstfrq_10")] <- NA
  y[,c("ccmethfrq_8","ccmethfrq_9","ccmethfrq_10")] <- NA
  y[,c("ccalcalone_8","ccalcalone_9","ccalcalone_10")] <- NA
  y[,c("ccalcothers_8","ccalcothers_9","ccalcothers_10")] <- NA
  y[,c("ccalcvirtual_8","ccalcvirtual_9","ccalcvirtual_10")] <- NA
  y[,c("ccalcdlvr_8","ccalcdlvr_9","ccalcdlvr_10")] <- NA
  y[,c("ccnumharm_8","ccnumharm_9","ccnumharm_10")] <- NA
  y[,c("cchlthskb_8","cchlthskb_9","cchlthskb_10")] <- NA
  y[,c("cchlthskc_8","cchlthskc_9","cchlthskc_10")] <- NA
  y[,c("cchlthskd_8","cchlthskd_9","cchlthskd_10")] <- NA
  
  y[c("ccstndt_8","ccstndt_9","ccstndt_10")] <- lapply(y[c("ccstndt_8","ccstndt_9","ccstndt_10")],function(x) {factor(x,levels=c(0,1),labels=c("No","Yes"))})
  y[c("ccempl_8","ccempl_9","ccempl_10")] <- lapply(y[c("ccempl_8","ccempl_9","ccempl_10")],function(x) {factor(x,levels=c(0,1),labels=c("No","Yes"))})
  y[c("ccecstfrq_8","ccecstfrq_9","ccecstfrq_10")] <- lapply(y[c("ccecstfrq_8","ccecstfrq_9","ccecstfrq_10")],function(x) {factor(x,levels=c(0,1,2),labels=c("None","Less than daily","Daily"))})
  y[c("ccmethfrq_8","ccmethfrq_9","ccmethfrq_10")] <- lapply(y[c("ccmethfrq_8","ccmethfrq_9","ccmethfrq_10")],function(x) {factor(x,levels=c(0,1,2),labels=c("None","Less than daily","Daily"))})
  y[c("ccalcdlvr_8","ccalcdlvr_9","ccalcdlvr_10")] <- lapply(y[c("ccalcdlvr_8","ccalcdlvr_9","ccalcdlvr_10")],function(x) {ordered(x,levels=c(0,1,2),labels=c("Never","Once","More than once"))})
  y[c("cchlthskb_8","cchlthskb_9","cchlthskb_10")] <- lapply(y[c("cchlthskb_8","cchlthskb_9","cchlthskb_10")],function(x) {factor(x,levels=c(0,1),labels=c("No","Yes"))})
  y[c("cchlthskc_8","cchlthskc_9","cchlthskc_10")] <- lapply(y[c("cchlthskc_8","cchlthskc_9","cchlthskc_10")],function(x) {factor(x,levels=c(0,1),labels=c("No","Yes"))})
  y[c("cchlthskd_8","cchlthskd_9","cchlthskd_10")] <- lapply(y[c("cchlthskd_8","cchlthskd_9","cchlthskd_10")],function(x) {factor(x,levels=c(0,1),labels=c("No","Yes"))})
  
  y <- y[,c("zzC_ID","b_seifa","b_famhist","b_oldsib","sex_8","sex_9","pinc_8","age_8","peeruse_8","peerdis_8",
            "datecom_8","datecom_9","datecom_10","datecom_11","datecom_12",
            "ccstndt_8","ccstndt_9","ccstndt_10","ccstndt_11","ccstndt_12",
            "ccempl_8","ccempl_9","ccempl_10","ccempl_11","ccempl_12",
            "ccalcfre_8","ccalcfre_9","ccalcfre_10","ccalcfre_11","ccalcfre_12",
            "ccalcqnt_8","ccalcqnt_9","ccalcqnt_10","ccalcqnt_11","ccalcqnt_12",
            "ccalcbngf_8","ccalcbngf_9","ccalcbngf_10","ccalcbngf_11","ccalcbngf_12",
            "ccalcmax_8","ccalcmax_9","ccalcmax_10","ccalcmax_11","ccalcmax_12",
            "ccalcalone_8","ccalcalone_9","ccalcalone_10","ccalcalone_11","ccalcalone_12",
            "ccalcothers_8","ccalcothers_9","ccalcothers_10","ccalcothers_11","ccalcothers_12",
            "ccalcvirtual_8","ccalcvirtual_9","ccalcvirtual_10","ccalcvirtual_11","ccalcvirtual_12",
            "ccalcdlvr_8","ccalcdlvr_9","ccalcdlvr_10","ccalcdlvr_11","ccalcdlvr_12",
            "ccalcmon_8","ccalcmon_9","ccalcmon_10","ccalcmon_11","ccalcmon_12",
            "ccnumharm_8","ccnumharm_9","ccnumharm_10","ccnumharm_11","ccnumharm_12",
            "ccsmkfrq_8","ccsmkfrq_9","ccsmkfrq_10","ccsmkfrq_11","ccsmkfrq_12",
            "ccecigfrq_8","ccecigfrq_9","ccecigfrq_10","ccecigfrq_11","ccecigfrq_12",
            "cccanfrq_8","cccanfrq_9","cccanfrq_10","cccanfrq_11","cccanfrq_12",
            "ccecstfrq_8","ccecstfrq_9","ccecstfrq_10","ccecstfrq_11","ccecstfrq_12",
            "ccmethfrq_8","ccmethfrq_9","ccmethfrq_10","ccmethfrq_11","ccmethfrq_12",
            "cchlthskb_8","cchlthskb_9","cchlthskb_10","cchlthskb_11","cchlthskb_12",
            "cchlthskc_8","cchlthskc_9","cchlthskc_10","cchlthskc_11","cchlthskc_12",
            "cchlthskd_8","cchlthskd_9","cchlthskd_10","cchlthskd_11","cchlthskd_12")]
  
  y <- reshape(y,
               direction="long",
               idvar="zzC_ID",
               varying=list(c("datecom_8","datecom_9","datecom_10","datecom_11","datecom_12"),
                            c("ccstndt_8","ccstndt_9","ccstndt_10","ccstndt_11","ccstndt_12"),
                            c("ccempl_8","ccempl_9","ccempl_10","ccempl_11","ccempl_12"),
                            c("ccalcfre_8","ccalcfre_9","ccalcfre_10","ccalcfre_11","ccalcfre_12"),
                            c("ccalcqnt_8","ccalcqnt_9","ccalcqnt_10","ccalcqnt_11","ccalcqnt_12"),
                            c("ccalcbngf_8","ccalcbngf_9","ccalcbngf_10","ccalcbngf_11","ccalcbngf_12"),
                            c("ccalcmax_8","ccalcmax_9","ccalcmax_10","ccalcmax_11","ccalcmax_12"),
                            c("ccalcalone_8","ccalcalone_9","ccalcalone_10","ccalcalone_11","ccalcalone_12"),
                            c("ccalcothers_8","ccalcothers_9","ccalcothers_10","ccalcothers_11","ccalcothers_12"),
                            c("ccalcvirtual_8","ccalcvirtual_9","ccalcvirtual_10","ccalcvirtual_11","ccalcvirtual_12"),
                            c("ccalcdlvr_8","ccalcdlvr_9","ccalcdlvr_10","ccalcdlvr_11","ccalcdlvr_12"),
                            c("ccalcmon_8","ccalcmon_9","ccalcmon_10","ccalcmon_11","ccalcmon_12"),
                            c("ccnumharm_8","ccnumharm_9","ccnumharm_10","ccnumharm_11","ccnumharm_12"),
                            c("ccsmkfrq_8","ccsmkfrq_9","ccsmkfrq_10","ccsmkfrq_11","ccsmkfrq_12"),
                            c("ccecigfrq_8","ccecigfrq_9","ccecigfrq_10","ccecigfrq_11","ccecigfrq_12"),
                            c("cccanfrq_8","cccanfrq_9","cccanfrq_10","cccanfrq_11","cccanfrq_12"),
                            c("ccecstfrq_8","ccecstfrq_9","ccecstfrq_10","ccecstfrq_11","ccecstfrq_12"),
                            c("ccmethfrq_8","ccmethfrq_9","ccmethfrq_10","ccmethfrq_11","ccmethfrq_12"),
                            c("cchlthskb_8","cchlthskb_9","cchlthskb_10","cchlthskb_11","cchlthskb_12"),
                            c("cchlthskc_8","cchlthskc_9","cchlthskc_10","cchlthskc_11","cchlthskc_12"),
                            c("cchlthskd_8","cchlthskd_9","cchlthskd_10","cchlthskd_11","cchlthskd_12")),
               v.names=c("datecom","ccstndt","ccempl","ccalcfre","ccalcqnt","ccalcbngf","ccalcmax","ccalcalone","ccalcothers","ccalcvirtual",
                         "ccalcdlvr","ccalcmon","ccnumharm","ccsmkfrq","ccecigfrq","cccanfrq","ccecstfrq","ccmethfrq","cchlthskb","cchlthskc","cchlthskd"),
               sep="_",
               timevar="zzwave")
  y<-y[,c(1,10:11,2:9,12:32)]
  y$imp <- x
  y
},imputed=imputed))

write_dta(imputed_alc_long,path=paste0(path,"Alcohol imputed data - long.dta"))
write_dta(imputed_alc_long,path="/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/Alcohol imputed data - long.dta")

