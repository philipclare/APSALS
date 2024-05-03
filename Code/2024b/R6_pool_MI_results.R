##############################################################################
##   
## APSALS Wave 11 Initiation Trajectories Paper
## Pool MI results using rubin's rules and put into long format for figures
## Author: Philip J Clare
## Date: 22 June 2023
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## Analysis pre-registered at DOI:10.17605/OSF.IO/BRDUV
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify working directory
workdir <- "D:/UNSW/APSALS - Documents/Papers/PIP46. Age of initiation and the trajectory of alcohol use and harm/"

# 1.2 Check and load packages
libs <- c("arm","Amelia","readxl","XLConnect")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load output, pool results, and format for figure creation
#-------------------------------------------------------------------------------------

# 2.1. Generic column names for each result
colnames <- as.vector(t(outer(seq(0,9), seq(0,10), paste, sep=".")))
colnames18 <- as.character(seq(11,18))
colnames20 <- as.character(seq(11,20))

# 2.2. Alcohol consumption results
p_alcfq_est <- read_excel(paste0(workdir,"Results/Raw/Primary alcfq_est 20240501.xlsx"),col_names = colnames)
p_alcfq_se <- read_excel(paste0(workdir,"Results/Raw/Primary alcfq_se 20240501.xlsx"),col_names = colnames)

p_alcfq <- as.data.frame(t(do.call(rbind,mi.meld(p_alcfq_est,p_alcfq_se))))
colnames(p_alcfq) <- c("estimate","se")
p_alcfq$lower <- p_alcfq$estimate - qnorm(0.975)*p_alcfq$se
p_alcfq$upper <- p_alcfq$estimate + qnorm(0.975)*p_alcfq$se
p_alcfq <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_alcfq[,c(1,3,4)])
colnames(p_alcfq) <- c("init","age","estimate","lower","upper")
p_alcfq$age <- p_alcfq$init+p_alcfq$age
p_alcfq <- p_alcfq[which(p_alcfq$age<=23),]

s_alcfq_est <- read_excel(paste0(workdir,"Results/Raw/Secondary alcfq_est 20240501.xlsx"),col_names = colnames)
s_alcfq_se <- read_excel(paste0(workdir,"Results/Raw/Secondary alcfq_se 20240501.xlsx"),col_names = colnames)

s_alcfq <- as.data.frame(t(do.call(rbind,mi.meld(s_alcfq_est,s_alcfq_se))))
colnames(s_alcfq) <- c("estimate","se")
s_alcfq$lower <- s_alcfq$estimate - qnorm(0.975)*s_alcfq$se
s_alcfq$upper <- s_alcfq$estimate + qnorm(0.975)*s_alcfq$se
s_alcfq <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_alcfq[,c(1,3,4)])
colnames(s_alcfq) <- c("init","age","estimate","lower","upper")
s_alcfq$age <- s_alcfq$init+s_alcfq$age
s_alcfq <- s_alcfq[which(s_alcfq$age<=23),]

rr18_p_alcfq_est <- read_excel(paste0(workdir,"Results/Raw/Primary alcfq_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_alcfq_se <- read_excel(paste0(workdir,"Results/Raw/Primary alcfq_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_alcfq <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_alcfq_est,rr18_p_alcfq_se))))
colnames(rr18_p_alcfq) <- c("estimate","se")
rr18_p_alcfq$lower <- rr18_p_alcfq$estimate - qnorm(0.975)*rr18_p_alcfq$se
rr18_p_alcfq$upper <- rr18_p_alcfq$estimate + qnorm(0.975)*rr18_p_alcfq$se
rr18_p_alcfq <- cbind(seq(11,17),exp(rr18_p_alcfq[,c(1,3,4)]))
colnames(rr18_p_alcfq) <- c("init","IRR","lower","upper")
rr18_p_alcfq <- rbind(rr18_p_alcfq,c(18,1,NA,NA))

rr20_p_alcfq_est <- read_excel(paste0(workdir,"Results/Raw/Primary alcfq_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_alcfq_se <- read_excel(paste0(workdir,"Results/Raw/Primary alcfq_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_alcfq <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_alcfq_est,rr20_p_alcfq_se))))
colnames(rr20_p_alcfq) <- c("estimate","se")
rr20_p_alcfq$lower <- rr20_p_alcfq$estimate - qnorm(0.975)*rr20_p_alcfq$se
rr20_p_alcfq$upper <- rr20_p_alcfq$estimate + qnorm(0.975)*rr20_p_alcfq$se
rr20_p_alcfq <- cbind(seq(11,20),exp(rr20_p_alcfq[,c(1,3,4)]))
colnames(rr20_p_alcfq) <- c("init","IRR","lower","upper")

rr18_s_alcfq_est <- read_excel(paste0(workdir,"Results/Raw/Secondary alcfq_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_alcfq_se <- read_excel(paste0(workdir,"Results/Raw/Secondary alcfq_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_alcfq <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_alcfq_est,rr18_s_alcfq_se))))
colnames(rr18_s_alcfq) <- c("estimate","se")
rr18_s_alcfq$lower <- rr18_s_alcfq$estimate - qnorm(0.975)*rr18_s_alcfq$se
rr18_s_alcfq$upper <- rr18_s_alcfq$estimate + qnorm(0.975)*rr18_s_alcfq$se
rr18_s_alcfq <- cbind(seq(11,17),exp(rr18_s_alcfq[,c(1,3,4)]))
colnames(rr18_s_alcfq) <- c("init","IRR","lower","upper")
rr18_s_alcfq <- rbind(rr18_s_alcfq,c(18,1,NA,NA))

rr20_s_alcfq_est <- read_excel(paste0(workdir,"Results/Raw/Secondary alcfq_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_alcfq_se <- read_excel(paste0(workdir,"Results/Raw/Secondary alcfq_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_alcfq <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_alcfq_est,rr20_s_alcfq_se))))
colnames(rr20_s_alcfq) <- c("estimate","se")
rr20_s_alcfq$lower <- rr20_s_alcfq$estimate - qnorm(0.975)*rr20_s_alcfq$se
rr20_s_alcfq$upper <- rr20_s_alcfq$estimate + qnorm(0.975)*rr20_s_alcfq$se
rr20_s_alcfq <- cbind(seq(11,20),exp(rr20_s_alcfq[,c(1,3,4)]))
colnames(rr20_s_alcfq) <- c("init","IRR","lower","upper")

# 2.3. Monthly HED results
p_hed_est <- read_excel(paste0(workdir,"Results/Raw/Primary monthlyhed_est 20240501.xlsx"),col_names = colnames)
p_hed_se <- read_excel(paste0(workdir,"Results/Raw/Primary monthlyhed_se 20240501.xlsx"),col_names = colnames)

p_hed <- as.data.frame(t(do.call(rbind,mi.meld(p_hed_est,p_hed_se))))
colnames(p_hed) <- c("estimate","se")
p_hed$lower <- p_hed$estimate - qnorm(0.975)*p_hed$se
p_hed$upper <- p_hed$estimate + qnorm(0.975)*p_hed$se
p_hed <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_hed[,c(1,3,4)])
colnames(p_hed) <- c("init","age","estimate","lower","upper")
p_hed$age <- p_hed$init+p_hed$age
p_hed <- p_hed[which(p_hed$age<=23),]

s_hed_est <- read_excel(paste0(workdir,"Results/Raw/Secondary monthlyhed_est 20240501.xlsx"),col_names = colnames)
s_hed_se <- read_excel(paste0(workdir,"Results/Raw/Secondary monthlyhed_se 20240501.xlsx"),col_names = colnames)

s_hed <- as.data.frame(t(do.call(rbind,mi.meld(s_hed_est,s_hed_se))))
colnames(s_hed) <- c("estimate","se")
s_hed$lower <- s_hed$estimate - qnorm(0.975)*s_hed$se
s_hed$upper <- s_hed$estimate + qnorm(0.975)*s_hed$se
s_hed <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_hed[,c(1,3,4)])
colnames(s_hed) <- c("init","age","estimate","lower","upper")
s_hed$age <- s_hed$init+s_hed$age
s_hed <- s_hed[which(s_hed$age<=23),]
s_hed$upper <- ifelse(s_hed$upper>1,1,s_hed$upper)

rr18_p_hed_est <- read_excel(paste0(workdir,"Results/Raw/Primary monthlyhed_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_hed_se <- read_excel(paste0(workdir,"Results/Raw/Primary monthlyhed_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_hed <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_hed_est,rr18_p_hed_se))))
colnames(rr18_p_hed) <- c("estimate","se")
rr18_p_hed$lower <- rr18_p_hed$estimate - qnorm(0.975)*rr18_p_hed$se
rr18_p_hed$upper <- rr18_p_hed$estimate + qnorm(0.975)*rr18_p_hed$se
rr18_p_hed <- cbind(seq(11,17),exp(rr18_p_hed[,c(1,3,4)]))
colnames(rr18_p_hed) <- c("init","RR","lower","upper")
rr18_p_hed <- rbind(rr18_p_hed,c(18,1,NA,NA))

rr20_p_hed_est <- read_excel(paste0(workdir,"Results/Raw/Primary monthlyhed_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_hed_se <- read_excel(paste0(workdir,"Results/Raw/Primary monthlyhed_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_hed <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_hed_est,rr20_p_hed_se))))
colnames(rr20_p_hed) <- c("estimate","se")
rr20_p_hed$lower <- rr20_p_hed$estimate - qnorm(0.975)*rr20_p_hed$se
rr20_p_hed$upper <- rr20_p_hed$estimate + qnorm(0.975)*rr20_p_hed$se
rr20_p_hed <- cbind(seq(11,20),exp(rr20_p_hed[,c(1,3,4)]))
colnames(rr20_p_hed) <- c("init","RR","lower","upper")

rr18_s_hed_est <- read_excel(paste0(workdir,"Results/Raw/Secondary monthlyhed_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_hed_se <- read_excel(paste0(workdir,"Results/Raw/Secondary monthlyhed_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_hed <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_hed_est,rr18_s_hed_se))))
colnames(rr18_s_hed) <- c("estimate","se")
rr18_s_hed$lower <- rr18_s_hed$estimate - qnorm(0.975)*rr18_s_hed$se
rr18_s_hed$upper <- rr18_s_hed$estimate + qnorm(0.975)*rr18_s_hed$se
rr18_s_hed <- cbind(seq(11,17),exp(rr18_s_hed[,c(1,3,4)]))
colnames(rr18_s_hed) <- c("init","RR","lower","upper")
rr18_s_hed <- rbind(rr18_s_hed,c(18,1,NA,NA))

rr20_s_hed_est <- read_excel(paste0(workdir,"Results/Raw/Secondary monthlyhed_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_hed_se <- read_excel(paste0(workdir,"Results/Raw/Secondary monthlyhed_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_hed <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_hed_est,rr20_s_hed_se))))
colnames(rr20_s_hed) <- c("estimate","se")
rr20_s_hed$lower <- rr20_s_hed$estimate - qnorm(0.975)*rr20_s_hed$se
rr20_s_hed$upper <- rr20_s_hed$estimate + qnorm(0.975)*rr20_s_hed$se
rr20_s_hed <- cbind(seq(11,20),exp(rr20_s_hed[,c(1,3,4)]))
colnames(rr20_s_hed) <- c("init","RR","lower","upper")

# 2.4. Any HED results - sensitivity results
p_anyhed_est <- read_excel(paste0(workdir,"Results/Raw/Primary anyhed_est 20240501.xlsx"),col_names = colnames)
p_anyhed_se <- read_excel(paste0(workdir,"Results/Raw/Primary anyhed_se 20240501.xlsx"),col_names = colnames)

p_anyhed <- as.data.frame(t(do.call(rbind,mi.meld(p_anyhed_est,p_anyhed_se))))
colnames(p_anyhed) <- c("estimate","se")
p_anyhed$lower <- p_anyhed$estimate - qnorm(0.975)*p_anyhed$se
p_anyhed$upper <- p_anyhed$estimate + qnorm(0.975)*p_anyhed$se
p_anyhed <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_anyhed[,c(1,3,4)])
colnames(p_anyhed) <- c("init","age","estimate","lower","upper")
p_anyhed$age <- p_anyhed$init+p_anyhed$age
p_anyhed <- p_anyhed[which(p_anyhed$age<=23),]

s_anyhed_est <- read_excel(paste0(workdir,"Results/Raw/Secondary anyhed_est 20240501.xlsx"),col_names = colnames)
s_anyhed_se <- read_excel(paste0(workdir,"Results/Raw/Secondary anyhed_se 20240501.xlsx"),col_names = colnames)

s_anyhed <- as.data.frame(t(do.call(rbind,mi.meld(s_anyhed_est,s_anyhed_se))))
colnames(s_anyhed) <- c("estimate","se")
s_anyhed$lower <- s_anyhed$estimate - qnorm(0.975)*s_anyhed$se
s_anyhed$upper <- s_anyhed$estimate + qnorm(0.975)*s_anyhed$se
s_anyhed <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_anyhed[,c(1,3,4)])
colnames(s_anyhed) <- c("init","age","estimate","lower","upper")
s_anyhed$age <- s_anyhed$init+s_anyhed$age
s_anyhed <- s_anyhed[which(s_anyhed$age<=23),]

rr18_p_anyhed_est <- read_excel(paste0(workdir,"Results/Raw/Primary anyhed_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_anyhed_se <- read_excel(paste0(workdir,"Results/Raw/Primary anyhed_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_anyhed <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_anyhed_est,rr18_p_anyhed_se))))
colnames(rr18_p_anyhed) <- c("estimate","se")
rr18_p_anyhed$lower <- rr18_p_anyhed$estimate - qnorm(0.975)*rr18_p_anyhed$se
rr18_p_anyhed$upper <- rr18_p_anyhed$estimate + qnorm(0.975)*rr18_p_anyhed$se
rr18_p_anyhed <- cbind(seq(11,17),exp(rr18_p_anyhed[,c(1,3,4)]))
colnames(rr18_p_anyhed) <- c("init","IRR","lower","upper")
rr18_p_anyhed <- rbind(rr18_p_anyhed,c(18,1,NA,NA))

rr20_p_anyhed_est <- read_excel(paste0(workdir,"Results/Raw/Primary anyhed_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_anyhed_se <- read_excel(paste0(workdir,"Results/Raw/Primary anyhed_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_anyhed <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_anyhed_est,rr20_p_anyhed_se))))
colnames(rr20_p_anyhed) <- c("estimate","se")
rr20_p_anyhed$lower <- rr20_p_anyhed$estimate - qnorm(0.975)*rr20_p_anyhed$se
rr20_p_anyhed$upper <- rr20_p_anyhed$estimate + qnorm(0.975)*rr20_p_anyhed$se
rr20_p_anyhed <- cbind(seq(11,20),exp(rr20_p_anyhed[,c(1,3,4)]))
colnames(rr20_p_anyhed) <- c("init","IRR","lower","upper")

rr18_s_anyhed_est <- read_excel(paste0(workdir,"Results/Raw/Secondary anyhed_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_anyhed_se <- read_excel(paste0(workdir,"Results/Raw/Secondary anyhed_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_anyhed <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_anyhed_est,rr18_s_anyhed_se))))
colnames(rr18_s_anyhed) <- c("estimate","se")
rr18_s_anyhed$lower <- rr18_s_anyhed$estimate - qnorm(0.975)*rr18_s_anyhed$se
rr18_s_anyhed$upper <- rr18_s_anyhed$estimate + qnorm(0.975)*rr18_s_anyhed$se
rr18_s_anyhed <- cbind(seq(11,17),exp(rr18_s_anyhed[,c(1,3,4)]))
colnames(rr18_s_anyhed) <- c("init","IRR","lower","upper")
rr18_s_anyhed <- rbind(rr18_s_anyhed,c(18,1,NA,NA))

rr20_s_anyhed_est <- read_excel(paste0(workdir,"Results/Raw/Secondary anyhed_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_anyhed_se <- read_excel(paste0(workdir,"Results/Raw/Secondary anyhed_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_anyhed <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_anyhed_est,rr20_s_anyhed_se))))
colnames(rr20_s_anyhed) <- c("estimate","se")
rr20_s_anyhed$lower <- rr20_s_anyhed$estimate - qnorm(0.975)*rr20_s_anyhed$se
rr20_s_anyhed$upper <- rr20_s_anyhed$estimate + qnorm(0.975)*rr20_s_anyhed$se
rr20_s_anyhed <- cbind(seq(11,20),exp(rr20_s_anyhed[,c(1,3,4)]))
colnames(rr20_s_anyhed) <- c("init","IRR","lower","upper")

# 2.5. Alcohol-related harms
p_harms_est <- read_excel(paste0(workdir,"Results/Raw/Primary numharms_est 20240501.xlsx"),col_names = colnames)
p_harms_se <- read_excel(paste0(workdir,"Results/Raw/Primary numharms_se 20240501.xlsx"),col_names = colnames)

p_harms <- as.data.frame(t(do.call(rbind,mi.meld(p_harms_est,p_harms_se))))
colnames(p_harms) <- c("estimate","se")
p_harms$lower <- p_harms$estimate - qnorm(0.975)*p_harms$se
p_harms$upper <- p_harms$estimate + qnorm(0.975)*p_harms$se
p_harms <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_harms[,c(1,3,4)])
colnames(p_harms) <- c("init","age","estimate","lower","upper")
p_harms$age <- p_harms$init+p_harms$age
p_harms <- p_harms[which(p_harms$age<=23),]

s_harms_est <- read_excel(paste0(workdir,"Results/Raw/Secondary numharms_est 20240501.xlsx"),col_names = colnames)
s_harms_se <- read_excel(paste0(workdir,"Results/Raw/Secondary numharms_se 20240501.xlsx"),col_names = colnames)

s_harms <- as.data.frame(t(do.call(rbind,mi.meld(s_harms_est,s_harms_se))))
colnames(s_harms) <- c("estimate","se")
s_harms$lower <- s_harms$estimate - qnorm(0.975)*s_harms$se
s_harms$upper <- s_harms$estimate + qnorm(0.975)*s_harms$se
s_harms <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_harms[,c(1,3,4)])
colnames(s_harms) <- c("init","age","estimate","lower","upper")
s_harms$age <- s_harms$init+s_harms$age
s_harms <- s_harms[which(s_harms$age<=23),]

rr18_p_harms_est <- read_excel(paste0(workdir,"Results/Raw/Primary numharms_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_harms_se <- read_excel(paste0(workdir,"Results/Raw/Primary numharms_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_harms <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_harms_est,rr18_p_harms_se))))
colnames(rr18_p_harms) <- c("estimate","se")
rr18_p_harms$lower <- rr18_p_harms$estimate - qnorm(0.975)*rr18_p_harms$se
rr18_p_harms$upper <- rr18_p_harms$estimate + qnorm(0.975)*rr18_p_harms$se
rr18_p_harms <- cbind(seq(11,17),exp(rr18_p_harms[,c(1,3,4)]))
colnames(rr18_p_harms) <- c("init","IRR","lower","upper")
rr18_p_harms <- rbind(rr18_p_harms,c(18,1,NA,NA))

rr20_p_harms_est <- read_excel(paste0(workdir,"Results/Raw/Primary numharms_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_harms_se <- read_excel(paste0(workdir,"Results/Raw/Primary numharms_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_harms <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_harms_est,rr20_p_harms_se))))
colnames(rr20_p_harms) <- c("estimate","se")
rr20_p_harms$lower <- rr20_p_harms$estimate - qnorm(0.975)*rr20_p_harms$se
rr20_p_harms$upper <- rr20_p_harms$estimate + qnorm(0.975)*rr20_p_harms$se
rr20_p_harms <- cbind(seq(11,20),exp(rr20_p_harms[,c(1,3,4)]))
colnames(rr20_p_harms) <- c("init","IRR","lower","upper")

rr18_s_harms_est <- read_excel(paste0(workdir,"Results/Raw/Secondary numharms_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_harms_se <- read_excel(paste0(workdir,"Results/Raw/Secondary numharms_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_harms <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_harms_est,rr18_s_harms_se))))
colnames(rr18_s_harms) <- c("estimate","se")
rr18_s_harms$lower <- rr18_s_harms$estimate - qnorm(0.975)*rr18_s_harms$se
rr18_s_harms$upper <- rr18_s_harms$estimate + qnorm(0.975)*rr18_s_harms$se
rr18_s_harms <- cbind(seq(11,17),exp(rr18_s_harms[,c(1,3,4)]))
colnames(rr18_s_harms) <- c("init","IRR","lower","upper")
rr18_s_harms <- rbind(rr18_s_harms,c(18,1,NA,NA))

rr20_s_harms_est <- read_excel(paste0(workdir,"Results/Raw/Secondary numharms_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_harms_se <- read_excel(paste0(workdir,"Results/Raw/Secondary numharms_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_harms <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_harms_est,rr20_s_harms_se))))
colnames(rr20_s_harms) <- c("estimate","se")
rr20_s_harms$lower <- rr20_s_harms$estimate - qnorm(0.975)*rr20_s_harms$se
rr20_s_harms$upper <- rr20_s_harms$estimate + qnorm(0.975)*rr20_s_harms$se
rr20_s_harms <- cbind(seq(11,20),exp(rr20_s_harms[,c(1,3,4)]))
colnames(rr20_s_harms) <- c("init","IRR","lower","upper")

# 2.6. Alcohol-related harms - binary sensitivity results
p_anyharms_est <- read_excel(paste0(workdir,"Results/Raw/Primary anyharms_est 20240501.xlsx"),col_names = colnames)
p_anyharms_se <- read_excel(paste0(workdir,"Results/Raw/Primary anyharms_se 20240501.xlsx"),col_names = colnames)

p_anyharms <- as.data.frame(t(do.call(rbind,mi.meld(p_anyharms_est,p_anyharms_se))))
colnames(p_anyharms) <- c("estimate","se")
p_anyharms$lower <- p_anyharms$estimate - qnorm(0.975)*p_anyharms$se
p_anyharms$upper <- p_anyharms$estimate + qnorm(0.975)*p_anyharms$se
p_anyharms <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_anyharms[,c(1,3,4)])
colnames(p_anyharms) <- c("init","age","estimate","lower","upper")
p_anyharms$age <- p_anyharms$init+p_anyharms$age
p_anyharms <- p_anyharms[which(p_anyharms$age<=23),]

s_anyharms_est <- read_excel(paste0(workdir,"Results/Raw/Secondary anyharms_est 20240501.xlsx"),col_names = colnames)
s_anyharms_se <- read_excel(paste0(workdir,"Results/Raw/Secondary anyharms_se 20240501.xlsx"),col_names = colnames)

s_anyharms <- as.data.frame(t(do.call(rbind,mi.meld(s_anyharms_est,s_anyharms_se))))
colnames(s_anyharms) <- c("estimate","se")
s_anyharms$lower <- s_anyharms$estimate - qnorm(0.975)*s_anyharms$se
s_anyharms$upper <- s_anyharms$estimate + qnorm(0.975)*s_anyharms$se
s_anyharms <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_anyharms[,c(1,3,4)])
colnames(s_anyharms) <- c("init","age","estimate","lower","upper")
s_anyharms$age <- s_anyharms$init+s_anyharms$age
s_anyharms <- s_anyharms[which(s_anyharms$age<=23),]

rr18_p_anyharms_est <- read_excel(paste0(workdir,"Results/Raw/Primary anyharms_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_anyharms_se <- read_excel(paste0(workdir,"Results/Raw/Primary anyharms_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_anyharms <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_anyharms_est,rr18_p_anyharms_se))))
colnames(rr18_p_anyharms) <- c("estimate","se")
rr18_p_anyharms$lower <- rr18_p_anyharms$estimate - qnorm(0.975)*rr18_p_anyharms$se
rr18_p_anyharms$upper <- rr18_p_anyharms$estimate + qnorm(0.975)*rr18_p_anyharms$se
rr18_p_anyharms <- cbind(seq(11,17),exp(rr18_p_anyharms[,c(1,3,4)]))
colnames(rr18_p_anyharms) <- c("init","RR","lower","upper")
rr18_p_anyharms <- rbind(rr18_p_anyharms,c(18,1,NA,NA))

rr20_p_anyharms_est <- read_excel(paste0(workdir,"Results/Raw/Primary anyharms_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_anyharms_se <- read_excel(paste0(workdir,"Results/Raw/Primary anyharms_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_anyharms <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_anyharms_est,rr20_p_anyharms_se))))
colnames(rr20_p_anyharms) <- c("estimate","se")
rr20_p_anyharms$lower <- rr20_p_anyharms$estimate - qnorm(0.975)*rr20_p_anyharms$se
rr20_p_anyharms$upper <- rr20_p_anyharms$estimate + qnorm(0.975)*rr20_p_anyharms$se
rr20_p_anyharms <- cbind(seq(11,20),exp(rr20_p_anyharms[,c(1,3,4)]))
colnames(rr20_p_anyharms) <- c("init","RR","lower","upper")

rr18_s_anyharms_est <- read_excel(paste0(workdir,"Results/Raw/Secondary anyharms_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_anyharms_se <- read_excel(paste0(workdir,"Results/Raw/Secondary anyharms_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_anyharms <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_anyharms_est,rr18_s_anyharms_se))))
colnames(rr18_s_anyharms) <- c("estimate","se")
rr18_s_anyharms$lower <- rr18_s_anyharms$estimate - qnorm(0.975)*rr18_s_anyharms$se
rr18_s_anyharms$upper <- rr18_s_anyharms$estimate + qnorm(0.975)*rr18_s_anyharms$se
rr18_s_anyharms <- cbind(seq(11,17),exp(rr18_s_anyharms[,c(1,3,4)]))
colnames(rr18_s_anyharms) <- c("init","RR","lower","upper")
rr18_s_anyharms <- rbind(rr18_s_anyharms,c(18,1,NA,NA))

rr20_s_anyharms_est <- read_excel(paste0(workdir,"Results/Raw/Secondary anyharms_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_anyharms_se <- read_excel(paste0(workdir,"Results/Raw/Secondary anyharms_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_anyharms <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_anyharms_est,rr20_s_anyharms_se))))
colnames(rr20_s_anyharms) <- c("estimate","se")
rr20_s_anyharms$lower <- rr20_s_anyharms$estimate - qnorm(0.975)*rr20_s_anyharms$se
rr20_s_anyharms$upper <- rr20_s_anyharms$estimate + qnorm(0.975)*rr20_s_anyharms$se
rr20_s_anyharms <- cbind(seq(11,20),exp(rr20_s_anyharms[,c(1,3,4)]))
colnames(rr20_s_anyharms) <- c("init","RR","lower","upper")

# 2.7. DSM-IV Dependence results
p_depend_est <- read_excel(paste0(workdir,"Results/Raw/Primary dependdiag_est 20240501.xlsx"),col_names = colnames)
p_depend_se <- read_excel(paste0(workdir,"Results/Raw/Primary dependdiag_se 20240501.xlsx"),col_names = colnames)

p_depend <- as.data.frame(t(do.call(rbind,mi.meld(p_depend_est,p_depend_se))))
colnames(p_depend) <- c("estimate","se")
p_depend$lower <- p_depend$estimate - qnorm(0.975)*p_depend$se
p_depend$upper <- p_depend$estimate + qnorm(0.975)*p_depend$se
p_depend <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_depend[,c(1,3,4)])
colnames(p_depend) <- c("init","age","estimate","lower","upper")
p_depend$age <- p_depend$init+p_depend$age
p_depend <- p_depend[which(p_depend$age>=14 & p_depend$age<=23),]

s_depend_est <- read_excel(paste0(workdir,"Results/Raw/Secondary dependdiag_est 20240501.xlsx"),col_names = colnames)
s_depend_se <- read_excel(paste0(workdir,"Results/Raw/Secondary dependdiag_se 20240501.xlsx"),col_names = colnames)

s_depend <- as.data.frame(t(do.call(rbind,mi.meld(s_depend_est,s_depend_se))))
colnames(s_depend) <- c("estimate","se")
s_depend$lower <- s_depend$estimate - qnorm(0.975)*s_depend$se
s_depend$upper <- s_depend$estimate + qnorm(0.975)*s_depend$se
s_depend <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_depend[,c(1,3,4)])
colnames(s_depend) <- c("init","age","estimate","lower","upper")
s_depend$age <- s_depend$init+s_depend$age
s_depend <- s_depend[which(s_depend$age>=14 & s_depend$age<=23),]

rr18_p_depend_est <- read_excel(paste0(workdir,"Results/Raw/Primary dependdiag_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_depend_se <- read_excel(paste0(workdir,"Results/Raw/Primary dependdiag_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_depend <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_depend_est,rr18_p_depend_se))))
colnames(rr18_p_depend) <- c("estimate","se")
rr18_p_depend$lower <- rr18_p_depend$estimate - qnorm(0.975)*rr18_p_depend$se
rr18_p_depend$upper <- rr18_p_depend$estimate + qnorm(0.975)*rr18_p_depend$se
rr18_p_depend <- cbind(seq(11,17),exp(rr18_p_depend[,c(1,3,4)]))
colnames(rr18_p_depend) <- c("init","RR","lower","upper")
rr18_p_depend <- rbind(rr18_p_depend,c(18,1,NA,NA))

rr20_p_depend_est <- read_excel(paste0(workdir,"Results/Raw/Primary dependdiag_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_depend_se <- read_excel(paste0(workdir,"Results/Raw/Primary dependdiag_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_depend <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_depend_est,rr20_p_depend_se))))
colnames(rr20_p_depend) <- c("estimate","se")
rr20_p_depend$lower <- rr20_p_depend$estimate - qnorm(0.975)*rr20_p_depend$se
rr20_p_depend$upper <- rr20_p_depend$estimate + qnorm(0.975)*rr20_p_depend$se
rr20_p_depend <- cbind(seq(11,20),exp(rr20_p_depend[,c(1,3,4)]))
colnames(rr20_p_depend) <- c("init","RR","lower","upper")

rr18_s_depend_est <- read_excel(paste0(workdir,"Results/Raw/Secondary dependdiag_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_depend_se <- read_excel(paste0(workdir,"Results/Raw/Secondary dependdiag_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_depend <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_depend_est,rr18_s_depend_se))))
colnames(rr18_s_depend) <- c("estimate","se")
rr18_s_depend$lower <- rr18_s_depend$estimate - qnorm(0.975)*rr18_s_depend$se
rr18_s_depend$upper <- rr18_s_depend$estimate + qnorm(0.975)*rr18_s_depend$se
rr18_s_depend <- cbind(seq(11,17),exp(rr18_s_depend[,c(1,3,4)]))
colnames(rr18_s_depend) <- c("init","RR","lower","upper")
rr18_s_depend <- rbind(rr18_s_depend,c(18,1,NA,NA))

rr20_s_depend_est <- read_excel(paste0(workdir,"Results/Raw/Secondary dependdiag_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_depend_se <- read_excel(paste0(workdir,"Results/Raw/Secondary dependdiag_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_depend <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_depend_est,rr20_s_depend_se))))
colnames(rr20_s_depend) <- c("estimate","se")
rr20_s_depend$lower <- rr20_s_depend$estimate - qnorm(0.975)*rr20_s_depend$se
rr20_s_depend$upper <- rr20_s_depend$estimate + qnorm(0.975)*rr20_s_depend$se
rr20_s_depend <- cbind(seq(11,20),exp(rr20_s_depend[,c(1,3,4)]))
colnames(rr20_s_depend) <- c("init","RR","lower","upper")

# 2.8. DSM-IV Abuse results
p_abuse_est <- read_excel(paste0(workdir,"Results/Raw/Primary abusediag_est 20240501.xlsx"),col_names = colnames)
p_abuse_se <- read_excel(paste0(workdir,"Results/Raw/Primary abusediag_se 20240501.xlsx"),col_names = colnames)

p_abuse <- as.data.frame(t(do.call(rbind,mi.meld(p_abuse_est,p_abuse_se))))
colnames(p_abuse) <- c("estimate","se")
p_abuse$lower <- p_abuse$estimate - qnorm(0.975)*p_abuse$se
p_abuse$upper <- p_abuse$estimate + qnorm(0.975)*p_abuse$se
p_abuse <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_abuse[,c(1,3,4)])
colnames(p_abuse) <- c("init","age","estimate","lower","upper")
p_abuse$age <- p_abuse$init+p_abuse$age
p_abuse <- p_abuse[which(p_abuse$age>=14 & p_abuse$age<=23),]

s_abuse_est <- read_excel(paste0(workdir,"Results/Raw/Secondary abusediag_est 20240501.xlsx"),col_names = colnames)
s_abuse_se <- read_excel(paste0(workdir,"Results/Raw/Secondary abusediag_se 20240501.xlsx"),col_names = colnames)

s_abuse <- as.data.frame(t(do.call(rbind,mi.meld(s_abuse_est,s_abuse_se))))
colnames(s_abuse) <- c("estimate","se")
s_abuse$lower <- s_abuse$estimate - qnorm(0.975)*s_abuse$se
s_abuse$upper <- s_abuse$estimate + qnorm(0.975)*s_abuse$se
s_abuse <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_abuse[,c(1,3,4)])
colnames(s_abuse) <- c("init","age","estimate","lower","upper")
s_abuse$age <- s_abuse$init+s_abuse$age
s_abuse <- s_abuse[which(s_abuse$age>=14 & s_abuse$age<=23),]

rr18_p_abuse_est <- read_excel(paste0(workdir,"Results/Raw/Primary abusediag_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_abuse_se <- read_excel(paste0(workdir,"Results/Raw/Primary abusediag_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_abuse <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_abuse_est,rr18_p_abuse_se))))
colnames(rr18_p_abuse) <- c("estimate","se")
rr18_p_abuse$lower <- rr18_p_abuse$estimate - qnorm(0.975)*rr18_p_abuse$se
rr18_p_abuse$upper <- rr18_p_abuse$estimate + qnorm(0.975)*rr18_p_abuse$se
rr18_p_abuse <- cbind(seq(11,17),exp(rr18_p_abuse[,c(1,3,4)]))
colnames(rr18_p_abuse) <- c("init","RR","lower","upper")
rr18_p_abuse <- rbind(rr18_p_abuse,c(18,1,NA,NA))

rr20_p_abuse_est <- read_excel(paste0(workdir,"Results/Raw/Primary abusediag_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_abuse_se <- read_excel(paste0(workdir,"Results/Raw/Primary abusediag_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_abuse <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_abuse_est,rr20_p_abuse_se))))
colnames(rr20_p_abuse) <- c("estimate","se")
rr20_p_abuse$lower <- rr20_p_abuse$estimate - qnorm(0.975)*rr20_p_abuse$se
rr20_p_abuse$upper <- rr20_p_abuse$estimate + qnorm(0.975)*rr20_p_abuse$se
rr20_p_abuse <- cbind(seq(11,20),exp(rr20_p_abuse[,c(1,3,4)]))
colnames(rr20_p_abuse) <- c("init","RR","lower","upper")

rr18_s_abuse_est <- read_excel(paste0(workdir,"Results/Raw/Secondary abusediag_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_abuse_se <- read_excel(paste0(workdir,"Results/Raw/Secondary abusediag_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_abuse <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_abuse_est,rr18_s_abuse_se))))
colnames(rr18_s_abuse) <- c("estimate","se")
rr18_s_abuse$lower <- rr18_s_abuse$estimate - qnorm(0.975)*rr18_s_abuse$se
rr18_s_abuse$upper <- rr18_s_abuse$estimate + qnorm(0.975)*rr18_s_abuse$se
rr18_s_abuse <- cbind(seq(11,17),exp(rr18_s_abuse[,c(1,3,4)]))
colnames(rr18_s_abuse) <- c("init","RR","lower","upper")
rr18_s_abuse <- rbind(rr18_s_abuse,c(18,1,NA,NA))

rr20_s_abuse_est <- read_excel(paste0(workdir,"Results/Raw/Secondary abusediag_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_abuse_se <- read_excel(paste0(workdir,"Results/Raw/Secondary abusediag_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_abuse <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_abuse_est,rr20_s_abuse_se))))
colnames(rr20_s_abuse) <- c("estimate","se")
rr20_s_abuse$lower <- rr20_s_abuse$estimate - qnorm(0.975)*rr20_s_abuse$se
rr20_s_abuse$upper <- rr20_s_abuse$estimate + qnorm(0.975)*rr20_s_abuse$se
rr20_s_abuse <- cbind(seq(11,20),exp(rr20_s_abuse[,c(1,3,4)]))
colnames(rr20_s_abuse) <- c("init","RR","lower","upper")

# 2.9. DSM-5 AUD results
p_aud_est <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_est 20240501.xlsx"),col_names = colnames)
p_aud_se <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_se 20240501.xlsx"),col_names = colnames)

p_aud <- as.data.frame(t(do.call(rbind,mi.meld(p_aud_est,p_aud_se))))
colnames(p_aud) <- c("estimate","se")
p_aud$lower <- p_aud$estimate - qnorm(0.975)*p_aud$se
p_aud$upper <- p_aud$estimate + qnorm(0.975)*p_aud$se
p_aud <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],p_aud[,c(1,3,4)])
colnames(p_aud) <- c("init","age","estimate","lower","upper")
p_aud$age <- p_aud$init+p_aud$age
p_aud <- p_aud[which(p_aud$age>=14 & p_aud$age<=23),]

s_aud_est <- read_excel(paste0(workdir,"Results/Raw/Secondary auddiag_est 20240501.xlsx"),col_names = colnames)
s_aud_se <- read_excel(paste0(workdir,"Results/Raw/Secondary auddiag_se 20240501.xlsx"),col_names = colnames)

s_aud <- as.data.frame(t(do.call(rbind,mi.meld(s_aud_est,s_aud_se))))
colnames(s_aud) <- c("estimate","se")
s_aud$lower <- s_aud$estimate - qnorm(0.975)*s_aud$se
s_aud$upper <- s_aud$estimate + qnorm(0.975)*s_aud$se
s_aud <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],s_aud[,c(1,3,4)])
colnames(s_aud) <- c("init","age","estimate","lower","upper")
s_aud$age <- s_aud$init+s_aud$age
s_aud <- s_aud[which(s_aud$age>=14 & s_aud$age<=23),]

rr18_p_aud_est <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_p_aud_se <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_p_aud <- as.data.frame(t(do.call(rbind,mi.meld(rr18_p_aud_est,rr18_p_aud_se))))
colnames(rr18_p_aud) <- c("estimate","se")
rr18_p_aud$lower <- rr18_p_aud$estimate - qnorm(0.975)*rr18_p_aud$se
rr18_p_aud$upper <- rr18_p_aud$estimate + qnorm(0.975)*rr18_p_aud$se
rr18_p_aud <- cbind(seq(11,17),exp(rr18_p_aud[,c(1,3,4)]))
colnames(rr18_p_aud) <- c("init","RR","lower","upper")
rr18_p_aud <- rbind(rr18_p_aud,c(18,1,NA,NA))

rr20_p_aud_est <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_aud_se <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_p_aud <- as.data.frame(t(do.call(rbind,mi.meld(rr20_p_aud_est,rr20_p_aud_se))))
colnames(rr20_p_aud) <- c("estimate","se")
rr20_p_aud$lower <- rr20_p_aud$estimate - qnorm(0.975)*rr20_p_aud$se
rr20_p_aud$upper <- rr20_p_aud$estimate + qnorm(0.975)*rr20_p_aud$se
rr20_p_aud <- cbind(seq(11,20),exp(rr20_p_aud[,c(1,3,4)]))
colnames(rr20_p_aud) <- c("init","RR","lower","upper")

rr18_s_aud_est <- read_excel(paste0(workdir,"Results/Raw/Secondary auddiag_est RR18 20240501.xlsx"),col_names = as.character(seq(11,18)))[,-8]
rr18_s_aud_se <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_se RR18 20240501.xlsx"),col_names = as.character(seq(11,17)))
rr18_s_aud <- as.data.frame(t(do.call(rbind,mi.meld(rr18_s_aud_est,rr18_s_aud_se))))
colnames(rr18_s_aud) <- c("estimate","se")
rr18_s_aud$lower <- rr18_s_aud$estimate - qnorm(0.975)*rr18_s_aud$se
rr18_s_aud$upper <- rr18_s_aud$estimate + qnorm(0.975)*rr18_s_aud$se
rr18_s_aud <- cbind(seq(11,17),exp(rr18_s_aud[,c(1,3,4)]))
colnames(rr18_s_aud) <- c("init","RR","lower","upper")
rr18_s_aud <- rbind(rr18_s_aud,c(18,1,NA,NA))

rr20_s_aud_est <- read_excel(paste0(workdir,"Results/Raw/Secondary auddiag_est RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_aud_se <- read_excel(paste0(workdir,"Results/Raw/Primary auddiag_se RR20 20240501.xlsx"),col_names = as.character(seq(11,20)))
rr20_s_aud <- as.data.frame(t(do.call(rbind,mi.meld(rr20_s_aud_est,rr20_s_aud_se))))
colnames(rr20_s_aud) <- c("estimate","se")
rr20_s_aud$lower <- rr20_s_aud$estimate - qnorm(0.975)*rr20_s_aud$se
rr20_s_aud$upper <- rr20_s_aud$estimate + qnorm(0.975)*rr20_s_aud$se
rr20_s_aud <- cbind(seq(11,20),exp(rr20_s_aud[,c(1,3,4)]))
colnames(rr20_s_aud) <- c("init","RR","lower","upper")

######################################################################################
# 3. Combine RR results into two summary tables
#-------------------------------------------------------------------------------------

p_rr18 <- cbind(rr18_p_alcfq,
              rr18_p_hed[,-1],
              rr18_p_harms[,-1],
              rr18_p_depend[,-1],rr18_p_abuse[,-1],rr18_p_aud[,-1])

p_rr20 <- cbind(rr20_p_alcfq,
              rr20_p_hed[,-1],
              rr20_p_harms[,-1],
              rr20_p_depend[,-1],rr20_p_abuse[,-1],rr20_p_aud[,-1])

s_rr18 <- cbind(rr18_s_alcfq,
                rr18_s_hed[,-1],
                rr18_s_harms[,-1],
                rr18_s_depend[,-1],rr18_s_abuse[,-1],rr18_s_aud[,-1])

s_rr20 <- cbind(rr20_s_alcfq,
                rr20_s_hed[,-1],
                rr20_s_harms[,-1],
                rr20_s_depend[,-1],rr20_p_abuse[,-1],rr20_s_aud[,-1])

######################################################################################
# 4. Save results ready for figure creation
#-------------------------------------------------------------------------------------

saveRDS(p_alcfq,paste0(workdir,"Results/Raw/Primary alcfq_res 20240501.rds"))
saveRDS(p_hed,paste0(workdir,"Results/Raw/Primary hed_res 20240501.rds"))
saveRDS(p_anyhed,paste0(workdir,"Results/Raw/Primary anyhed_res 20240501.rds"))
saveRDS(p_harms,paste0(workdir,"Results/Raw/Primary harms_res 20240501.rds"))
saveRDS(p_anyharms,paste0(workdir,"Results/Raw/Primary anyharms_res 20240501.rds"))
saveRDS(p_depend,paste0(workdir,"Results/Raw/Primary depend_res 20240501.rds"))
saveRDS(p_abuse,paste0(workdir,"Results/Raw/Primary abuse_res 20240501.rds"))
saveRDS(p_aud,paste0(workdir,"Results/Raw/Primary aud_res 20240501.rds"))

saveRDS(s_alcfq,paste0(workdir,"Results/Raw/Secondary alcfq_res 20240501.rds"))
saveRDS(s_hed,paste0(workdir,"Results/Raw/Secondary hed_res 20240501.rds"))
saveRDS(s_anyhed,paste0(workdir,"Results/Raw/Secondary anyhed_res 20240501.rds"))
saveRDS(s_harms,paste0(workdir,"Results/Raw/Secondary harms_res 20240501.rds"))
saveRDS(s_anyharms,paste0(workdir,"Results/Raw/Secondary anyharms_res 20240501.rds"))
saveRDS(s_depend,paste0(workdir,"Results/Raw/Secondary depend_res 20240501.rds"))
saveRDS(s_abuse,paste0(workdir,"Results/Raw/Secondary abuse_res 20240501.rds"))
saveRDS(s_aud,paste0(workdir,"Results/Raw/Secondary aud_res 20240501.rds"))

######################################################################################
# 5. Save summary output to excel for tables
#-------------------------------------------------------------------------------------

wb1 <- loadWorkbook(paste0(workdir,"Results/Raw/primary-model-results-20240501.xlsx"), create = TRUE)
createSheet(wb1, name = "alcfq")
createSheet(wb1, name = "hed")
createSheet(wb1, name = "anyhed")
createSheet(wb1, name = "harms")
createSheet(wb1, name = "anyharms")
createSheet(wb1, name = "dependence")
createSheet(wb1, name = "abuse")
createSheet(wb1, name = "aud")

writeWorksheet(wb1,p_alcfq,"alcfq",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_hed,"hed",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_anyhed,"anyhed",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_harms,"harms",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_anyharms,"anyharms",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_depend,"dependence",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_abuse,"abuse",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb1,p_aud,"aud",startRow = 1, startCol = 1, header = FALSE)

saveWorkbook(wb1)

wb2 <- loadWorkbook(paste0(workdir,"Results/Raw/secondary-model-results-20240501.xlsx"), create = TRUE)
createSheet(wb2, name = "alcfq")
createSheet(wb2, name = "hed")
createSheet(wb2, name = "anyhed")
createSheet(wb2, name = "harms")
createSheet(wb2, name = "anyharms")
createSheet(wb2, name = "dependence")
createSheet(wb2, name = "abuse")
createSheet(wb2, name = "aud")

writeWorksheet(wb2,s_alcfq,"alcfq",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_hed,"hed",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_anyhed,"anyhed",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_harms,"harms",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_anyharms,"anyharms",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_depend,"dependence",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_abuse,"abuse",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb2,s_aud,"aud",startRow = 1, startCol = 1, header = FALSE)

saveWorkbook(wb2)

wb3 <- loadWorkbook(paste0(workdir,"Results/Raw/relative-results-20240501.xlsx"), create = TRUE)
createSheet(wb3, name = "age18")
createSheet(wb3, name = "age20")
createSheet(wb3, name = "age18_s")
createSheet(wb3, name = "age20_s")

writeWorksheet(wb3,p_rr18,"age18",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb3,p_rr20,"age20",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb3,s_rr18,"age18_s",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb3,s_rr20,"age20_s",startRow = 1, startCol = 1, header = FALSE)

saveWorkbook(wb3)