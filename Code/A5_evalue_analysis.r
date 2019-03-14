## SYNTAX FILE 5 - E-VALUE ANALYSIS                          ##

cloudstor <- "C:/Users/z3312911/Cloudstor/" # change to master file path
.libPaths(paste0(cloudstor,"R Library"))

libs <- c("EValue")
missing <- !libs %in% installed.packages()
if (any(missing)) {
  install.packages(libs[missing])
}
library ("EValue")

evalues.OR(est=3.71, lo=1.30, hi=10.61, rare = 0)
evalues.OR(est=3.41, lo=1.15, hi=10.08, rare = 0)
evalues.OR(est=1.59, lo=0.51, hi=4.97, rare = 0)
evalues.OR(est=2.07, lo=0.70, hi=6.18, rare = 0)
evalues.OR(est=1.89, lo=0.70, hi=5.06, rare = 0)

evalues.OR(est=6.27, lo=0.62, hi=63.48, rare = 0)
evalues.OR(est=4.23, lo=0.76, hi=23.53, rare = 0)
evalues.OR(est=1.92, lo=0.71, hi=5.19, rare = 0)
evalues.OR(est=1.76, lo=0.64, hi=4.87, rare = 0)
evalues.OR(est=2.37, lo=0.86, hi=6.53, rare = 0)


evalues.OR(est=1.22, lo=1.09, hi=1.36, rare = 0)
evalues.OR(est=1.24, lo=1.10, hi=1.39, rare = 0)
evalues.OR(est=1.11, lo=0.94, hi=1.30, rare = 0)
evalues.OR(est=1.15, lo=0.99, hi=1.33, rare = 0)
evalues.OR(est=1.16, lo=1.02, hi=1.33, rare = 0)

evalues.OR(est=1.32, lo=1.12, hi=1.56, rare = 0)
evalues.OR(est=1.31, lo=1.11, hi=1.54, rare = 0)
evalues.OR(est=1.07, lo=0.93, hi=1.22, rare = 0)
evalues.OR(est=1.09, lo=0.95, hi=1.24, rare = 0)
evalues.OR(est=1.11, lo=0.98, hi=1.25, rare = 0)