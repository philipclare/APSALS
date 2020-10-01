
## SYNTAX FILE 6                                             ##
## E-VALUE ANALYSIS                                          ##

cloudstor <- "C:/Users/z3312911/Cloudstor/"
dropbox <- "C:/Users/z3312911/Dropbox/" # change to master file path
.libPaths(paste0(dropbox,"R Library"))

libs <- c("EValue","ggplot2")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
library ("EValue")

load(file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Results/jtresults 20191028.RData"))

e1 <- evalues.RR(est=exp(jtresults[1,1]), lo=exp(jtresults[1,1]-qnorm(0.975)*jtresults[1,2]), hi=exp(jtresults[1,1]+qnorm(0.975)*jtresults[1,2]))
e2 <- evalues.RR(est=exp(jtresults[2,1]), lo=exp(jtresults[2,1]-qnorm(0.975)*jtresults[2,2]), hi=exp(jtresults[2,1]+qnorm(0.975)*jtresults[2,2]))
e3 <- evalues.RR(est=exp(jtresults[3,1]), lo=exp(jtresults[3,1]-qnorm(0.975)*jtresults[3,2]), hi=exp(jtresults[3,1]+qnorm(0.975)*jtresults[3,2]))
e4 <- evalues.RR(est=exp(jtresults[4,1]), lo=exp(jtresults[4,1]-qnorm(0.975)*jtresults[4,2]), hi=exp(jtresults[4,1]+qnorm(0.975)*jtresults[4,2]))
e5 <- evalues.RR(est=exp(jtresults[5,1]), lo=exp(jtresults[5,1]-qnorm(0.975)*jtresults[5,2]), hi=exp(jtresults[5,1]+qnorm(0.975)*jtresults[5,2]))

e6 <- evalues.RR(est=exp(jtresults[1,5]), lo=exp(jtresults[1,5]-qnorm(0.975)*jtresults[1,6]), hi=exp(jtresults[1,5]+qnorm(0.975)*jtresults[1,6]))
e7 <- evalues.RR(est=exp(jtresults[2,5]), lo=exp(jtresults[2,5]-qnorm(0.975)*jtresults[2,6]), hi=exp(jtresults[2,5]+qnorm(0.975)*jtresults[2,6]))
e8 <- evalues.RR(est=exp(jtresults[3,5]), lo=exp(jtresults[3,5]-qnorm(0.975)*jtresults[3,6]), hi=exp(jtresults[3,5]+qnorm(0.975)*jtresults[3,6]))
e9 <- evalues.RR(est=exp(jtresults[4,5]), lo=exp(jtresults[4,5]-qnorm(0.975)*jtresults[4,6]), hi=exp(jtresults[4,5]+qnorm(0.975)*jtresults[4,6]))
e10 <- evalues.RR(est=exp(jtresults[5,5]), lo=exp(jtresults[5,5]-qnorm(0.975)*jtresults[5,6]), hi=exp(jtresults[5,5]+qnorm(0.975)*jtresults[5,6]))

evalue <- matrix(c(e1[2,1],e2[2,1],e3[2,1],e4[2,1],e5[2,1],e6[2,1],e7[2,1],e8[2,1],e9[2,1],e10[2,1]),ncol=2,nrow=5)
rownames(evalue) <- c("Binge drinking","Any harms","DSM-IV Abuse","DSM-IV Dependence","DSM-5 AUD")
colnames(evalue) <- c("Wave 6","Wave 7")

save(evalue,file=paste0(cloudstor,"PhD/Paper 5 - APSALs application/Results/evalue 20191028.RData"))