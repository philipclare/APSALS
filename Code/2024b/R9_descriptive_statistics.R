##############################################################################
##   
## APSALS Wave 11 Initiation Trajectories Paper
## Create figures from Stata analysis output
## Author: Philip J Clare
## Date: 22 June 2023
## Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
## Analysis pre-registered at DOI:10.17605/OSF.IO/BRDUV
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1. Specify working directory
workdir <- "/Users/pjclare/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/APSALS - Documents/Papers/PIP46. Effect of age of initiation on trajectory of alcohol use and harm/"
.libPaths("/Users/pjclare/Dropbox (Sydney Uni)/R Library Mac")

# 1.2 Check and load packages
libs <- c("dplyr","fastDummies","flextable","haven","table1","tidyr")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
lapply(libs, library, character.only = TRUE)

######################################################################################
# 2. Load data 
#-------------------------------------------------------------------------------------

# 2.1 Load data
raw_data <- read_dta(file=paste0(workdir,"Data/initiation_analysis_data.dta"))
raw_data <- zap_formats(raw_data)
raw_data <- zap_label(raw_data)
raw_data <- zap_labels(raw_data)
raw_data <- raw_data[which(raw_data$wave==0),]

######################################################################################
# 3. Load and process data 
#-------------------------------------------------------------------------------------

raw_data$b_sex <- factor(raw_data$b_sex,levels=c("0","1"),labels=c("Male","Female"))
raw_data$b_hinc <- factor(raw_data$b_hinc,levels=c("0","1","2","3"),labels=c("Up to $34,000","$35-80,000","$81-180,000",">$180,000"))
raw_data$b_singlep <- factor(raw_data$b_singlep,levels=c("0","1"),labels=c("No","Yes"))
raw_data$b_alcmoney <- factor(raw_data$b_alcmoney,levels=c("0","1"),labels=c("No","Yes"))
raw_data$b_parborn <- factor(raw_data$b_parborn,levels=c("0","1"),labels=c("No","Yes"))
raw_data$b_pareduc <- factor(raw_data$b_pareduc,levels=c("0","1","2"),labels=c("High school or less","Diploma/Trade/Non-trade","University"))
raw_data$b_parempl <- factor(raw_data$b_parempl,levels=c("0","1","2"),labels=c("Employed","Unemployed - in workforce","Unemployed - not in workforce"))
raw_data$b_seifa <- factor(raw_data$b_seifa,levels=c("0","1","2"),labels=c("Lowest tertile","Middle tertile","Highest tertile"))
raw_data$b_parrel <- factor(raw_data$b_parrel,levels=c("0","1"),labels=c("Not/a little","Pretty/very"))
raw_data$b_singlep <- factor(raw_data$b_singlep,levels=c("0","1"),labels=c("No","Yes"))

strata <- c(list("Mean (SD) / n (%)"=raw_data))

labels <- list(
  variables=list(c_age = "Age of initiation",
                 b_sex = "Sex",
                 b_alcmoney = "Has money to buy alcohol",
                 b_cbclextn = "CBCL Externalising",
                 b_cbcladn = "CBCL Anxious/depressed",
                 b_cbclwdn = "CBCL Withdrawn/depressed",
                 b_homeacc = "Home access to alcohol",
                 b_famconf = "Family conflict",
                 b_famposi = "Family positive relations",
                 b_hinc = "Household income",
                 b_seifa = "Area-level socioeconomic disadvantage (SEIFA)",
                 b_hhavguse = "Parent alcohol use",
                 b_parmon = "Parental monitoring",
                 b_pardem = "Parental demandingess",
                 b_parres = "Parental responsiveness",
                 b_parborn = "Parent born in Australia",
                 b_pareduc = "Parent education",
                 b_parempl = "Parent employment",
                 b_parrel = "Parent religiousness",
                 b_peeruse = "Peer substance use",
                 b_peerdis = "Peer disapproval of substance use"
  ))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=4), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}
my.render.mdn <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=4), c("",
                                                           "Median (IQR)"=sprintf("%s [%s, %s]", median(x), 
                                                                                  quantile(x, 0.25), quantile(x, 0.75))))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%2.1f%%)", FREQ, PCT))))
}

rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(my.render.cat(x))
  what <- switch(name,
                 c_age = "Median (q25, q75)",
                 b_cbclextn  = "Mean (SD)",
                 b_cbcladn  = "Mean (SD)",
                 b_cbclwdn  = "Mean (SD)",
                 b_homeacc  = "Mean (SD)",
                 b_famconf  = "Mean (SD)",
                 b_famposi  = "Mean (SD)",
                 b_hhavguse  = "Median (q25, q75)",
                 b_parmon  = "Mean (SD)",
                 b_pardem  = "Mean (SD)",
                 b_parres  = "Mean (SD)",
                 b_peeruse  = "Mean (SD)",
                 b_peerdis  = "Mean (SD)")
  parse.abbrev.render.code(c("", what))(x)
}

tbl1 <- table1(strata,
               labels=labels,
               render=rndr)
               # render.continuous=my.render.cont,
               # render.categorical=my.render.cat,
               # render.missing=NULL)

tbl1
t1flex(tbl1) %>% 
  save_as_docx(path=paste0(workdir,"Results/Table 1.docx"))

