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
.libPaths("/Users/pjclare/Dropbox (Sydney Uni)/R Library Mac")
workdir <- "/Users/pjclare/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/APSALS - Documents/Papers/PIP46. Effect of age of initiation on trajectory of alcohol use and harm/"

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

# 2.2. Alcohol consumption results
alcfq_est <- read_excel(paste0(workdir,"Results/Raw/alcfq_est 20231116.xlsx"),col_names = colnames)
alcfq_se <- read_excel(paste0(workdir,"Results/Raw/alcfq_se 20231116.xlsx"),col_names = colnames)

alcfq <- as.data.frame(t(do.call(rbind,mi.meld(alcfq_est,alcfq_se))))
colnames(alcfq) <- c("estimate","se")
alcfq$lower <- alcfq$estimate - qnorm(0.975)*alcfq$se
alcfq$upper <- alcfq$estimate + qnorm(0.975)*alcfq$se
alcfq <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],exp(alcfq[,c(1,3,4)]))
colnames(alcfq) <- c("init","age","estimate","lower","upper")
alcfq$age <- alcfq$init+alcfq$age
alcfq <- alcfq[which(alcfq$age<=23),]

# 2.3. Monthly HED results
hed_est <- read_excel(paste0(workdir,"Results/Raw/monthlyhed_est 20231116.xlsx"),col_names = colnames)
hed_se <- read_excel(paste0(workdir,"Results/Raw/monthlyhed_se 20231116.xlsx"),col_names = colnames)

hed <- as.data.frame(t(do.call(rbind,mi.meld(hed_est,hed_se))))
colnames(hed) <- c("estimate","se")
hed$lower <- hed$estimate - qnorm(0.975)*hed$se
hed$upper <- hed$estimate + qnorm(0.975)*hed$se
hed <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],invlogit(hed[,c(1,3,4)]))
colnames(hed) <- c("init","age","estimate","lower","upper")
hed$age <- hed$init+hed$age
hed <- hed[which(hed$age<=23),]

# 2.4. Any HED results - sensitivity results
anyhed_est <- read_excel(paste0(workdir,"Results/Raw/anyhed_est 20231116.xlsx"),col_names = colnames)
anyhed_se <- read_excel(paste0(workdir,"Results/Raw/anyhed_se 20231116.xlsx"),col_names = colnames)

anyhed <- as.data.frame(t(do.call(rbind,mi.meld(anyhed_est,anyhed_se))))
colnames(anyhed) <- c("estimate","se")
anyhed$lower <- anyhed$estimate - qnorm(0.975)*anyhed$se
anyhed$upper <- anyhed$estimate + qnorm(0.975)*anyhed$se
anyhed <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],invlogit(anyhed[,c(1,3,4)]))
colnames(anyhed) <- c("init","age","estimate","lower","upper")
anyhed$age <- anyhed$init+anyhed$age
anyhed <- anyhed[which(anyhed$age<=23),]

# 2.5. Alcohol-related harms
harms_est <- read_excel(paste0(workdir,"Results/Raw/numharms_est 20231116.xlsx"),col_names = colnames)
harms_se <- read_excel(paste0(workdir,"Results/Raw/numharms_se 20231116.xlsx"),col_names = colnames)

harms <- as.data.frame(t(do.call(rbind,mi.meld(harms_est,harms_se))))
colnames(harms) <- c("estimate","se")
harms$lower <- harms$estimate - qnorm(0.975)*harms$se
harms$upper <- harms$estimate + qnorm(0.975)*harms$se
harms <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],exp(harms[,c(1,3,4)]))
colnames(harms) <- c("init","age","estimate","lower","upper")
harms$age <- harms$init+harms$age
harms <- harms[which(harms$age<=23),]

# 2.6. Alcohol-related harms - binary sensitivity results
anyharms_est <- read_excel(paste0(workdir,"Results/Raw/anyharms_est 20231116.xlsx"),col_names = colnames)
anyharms_se <- read_excel(paste0(workdir,"Results/Raw/anyharms_se 20231116.xlsx"),col_names = colnames)

anyharms <- as.data.frame(t(do.call(rbind,mi.meld(anyharms_est,anyharms_se))))
colnames(anyharms) <- c("estimate","se")
anyharms$lower <- anyharms$estimate - qnorm(0.975)*anyharms$se
anyharms$upper <- anyharms$estimate + qnorm(0.975)*anyharms$se
anyharms <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],invlogit(anyharms[,c(1,3,4)]))
colnames(anyharms) <- c("init","age","estimate","lower","upper")
anyharms$age <- anyharms$init+anyharms$age
anyharms <- anyharms[which(anyharms$age<=23),]

# 2.7. DSM-IV Dependence results
depend_est <- read_excel(paste0(workdir,"Results/Raw/dependdiag_est 20231116.xlsx"),col_names = colnames)
depend_se <- read_excel(paste0(workdir,"Results/Raw/dependdiag_se 20231116.xlsx"),col_names = colnames)

depend <- as.data.frame(t(do.call(rbind,mi.meld(depend_est,depend_se))))
colnames(depend) <- c("estimate","se")
depend$lower <- depend$estimate - qnorm(0.975)*depend$se
depend$upper <- depend$estimate + qnorm(0.975)*depend$se
depend <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],invlogit(depend[,c(1,3,4)]))
colnames(depend) <- c("init","age","estimate","lower","upper")
depend$age <- depend$init+depend$age
depend <- depend[which(depend$age>=14 & depend$age<=23),]

# 2.8. DSM-IV Abuse results
abuse_est <- read_excel(paste0(workdir,"Results/Raw/abusediag_est 20231116.xlsx"),col_names = colnames)
abuse_se <- read_excel(paste0(workdir,"Results/Raw/abusediag_se 20231116.xlsx"),col_names = colnames)

abuse <- as.data.frame(t(do.call(rbind,mi.meld(abuse_est,abuse_se))))
colnames(abuse) <- c("estimate","se")
abuse$lower <- abuse$estimate - qnorm(0.975)*abuse$se
abuse$upper <- abuse$estimate + qnorm(0.975)*abuse$se
abuse <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],invlogit(abuse[,c(1,3,4)]))
colnames(abuse) <- c("init","age","estimate","lower","upper")
abuse$age <- abuse$init+abuse$age
abuse <- abuse[which(abuse$age>=14 & abuse$age<=23),]

# 2.9. DSM-5 AUD results
aud_est <- read_excel(paste0(workdir,"Results/Raw/auddiag_est 20231116.xlsx"),col_names = colnames)
aud_se <- read_excel(paste0(workdir,"Results/Raw/auddiag_se 20231116.xlsx"),col_names = colnames)

aud <- as.data.frame(t(do.call(rbind,mi.meld(aud_est,aud_se))))
colnames(aud) <- c("estimate","se")
aud$lower <- aud$estimate - qnorm(0.975)*aud$se
aud$upper <- aud$estimate + qnorm(0.975)*aud$se
aud <- cbind(expand.grid(seq(0,10), seq(11,20))[,c(2,1)],invlogit(aud[,c(1,3,4)]))
colnames(aud) <- c("init","age","estimate","lower","upper")
aud$age <- aud$init+aud$age
aud <- aud[which(aud$age>=14 & aud$age<=23),]

######################################################################################
# 3. Save results ready for figure creation
#-------------------------------------------------------------------------------------

saveRDS(alcfq,paste0(workdir,"Results/Raw/alcfq_res 20231116.rds"))
saveRDS(hed,paste0(workdir,"Results/Raw/hed_res 20231116.rds"))
saveRDS(anyhed,paste0(workdir,"Results/Raw/anyhed_res 20231116.rds"))
saveRDS(harms,paste0(workdir,"Results/Raw/harms_res 20231116.rds"))
saveRDS(anyharms,paste0(workdir,"Results/Raw/anyharms_res 20231116.rds"))
saveRDS(depend,paste0(workdir,"Results/Raw/depend_res 20231116.rds"))
saveRDS(abuse,paste0(workdir,"Results/Raw/abuse_res 20231116.rds"))
saveRDS(aud,paste0(workdir,"Results/Raw/aud_res 20231116.rds"))

######################################################################################
# 4. Save summary output to excel for tables
#-------------------------------------------------------------------------------------

wb <- loadWorkbook(paste0(workdir,"Results/Raw/model-results-20231116.xlsx"), create = TRUE)
createSheet(wb, name = "alcfq")
createSheet(wb, name = "hed")
createSheet(wb, name = "anyhed")
createSheet(wb, name = "harms")
createSheet(wb, name = "anyharms")
createSheet(wb, name = "dependence")
createSheet(wb, name = "abuse")
createSheet(wb, name = "aud")

writeWorksheet(wb,alcfq,"alcfq",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,hed,"hed",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,anyhed,"anyhed",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,harms,"harms",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,anyharms,"anyharms",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,depend,"dependence",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,abuse,"abuse",startRow = 1, startCol = 1, header = FALSE)
writeWorksheet(wb,aud,"aud",startRow = 1, startCol = 1, header = FALSE)

saveWorkbook(wb)
