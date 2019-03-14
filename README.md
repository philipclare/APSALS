# Parental Supply of Alcohol LTMLE Application Code

This repository contains R code used in the TMLE analysis of APSALS by Clare et al.

| Description | R-code |
| --- | --- |
| A1 - Multiple imputation | [Multiple imputation code](Code/A1_multiple_imputation.R) |
| A2 - Final data creation | [Final data creation code](Code/A2_final_data_creation.R) |
| Longitudinal TMLE with a single outcome measurement, both manually and using the package 'ltmle' (3). | [Single Outcome Longitudinal Code](Code/long-single-y.R) |
| Longitudinal TMLE with a repeated outcome measurement, both manually and using the package 'ltmle' (3). | [Repeated Outcome Longitudinal Code](Code/long-repeated-y.R) |

The longitudinal dataset, ldata.RData, is also included in the repository.

1. Sofrygin O, van der Laan Mark J, Neugebauer R. simcausal R Package: Conducting Transparent and Reproducible Simulation Studies of Causal Effect Estimation with Complex Longitudinal Data. Journal of Statistical Software. 2017;81(2):1-47.
2. Gruber S, van der Laan MJ. tmle: An R Package for Targeted Maximum Likelihood Estimation. Journal of Statistical Software. 2012;51(13):1-35.
3. Lendle SD, Schwab J, Petersen ML, van der Laan MJ. ltmle: An R Package Implementing Targeted Minimum Loss-Based Estimation for Longitudinal Data. Journal of Statistical Software. 2017;81(1):1-21.
