# The Australian Parental Supply of Alcohol Longitudinal Study (APSALS) Code

This repository contains R code used in a number of articles using the Australian Parental Supply of Alcohol Longitudinal Study (APSALS).


## The overall effect of parental supply of alcohol across adolescence on alcohol-related harms in early adulthood—a prospective cohort study
Code for all analysis in the article by Clare et al published in Addiction, 2020: https://doi.org/10.1111/add.15005

| Description | R-code |
| --- | --- |
| A1 - Multiple imputation | [Multiple imputation](Code/2020a/A1_multiple_imputation.R) |
| A2 - Final data creation | [Final data creation](Code/2020a/A2_final_data_creation.R) |
| A3 - LTMLE analysis of parental supply of alcohol on harms using the package 'ltmle' (1). | [LTMLE analysis](Code/2020a/A3_ltmle_analysis.R) |
| A4 - LTMLE marginal structural model analysis of earlier initiation of supply. | [LTMLE MSM analysis](Code/2020a/A4_ltmle_msm_analysis.R) |
| A5 - Sensitivity analysis using naive analysis (GLMs) | [Naive analysis](Code/2020a/A5_naive_analysis.R) |
| A6 - E-Value sensitivity analysis | [E-value analysis](Code/2020a/A6_evalue_analysis.R) |
| A7 - Secondary analysis of exposure (parental supply) beginning at age 15. | [LTMLE - supply from age 15](Code/2020a/A7_secondary_supply_at_age_15.R) |
| A8 - Sensitivity analysis with lagged predictors. | [LTMLE - lagged predictors](Code/2020a/A8_sensitivity_lagged_predictors.R) |
| A9 - Sensitivity analysis controlling for past obervations of outcome | [LTMLE - control for past outcomes](Code/2020a/A9_sensitivity_control_for_past_outcomes.R) |
| A10 - Sensitivity analysis with continuous outcomes. | [LTMLE - continuous outcomes](Code/2020a/A10_sensitivity_continuous_outcomes.R) |


## Alcohol use among young Australian adults during the COVID-19 pandemic: a prospective cohort study 
R and Stata code for all analysis of APSALS COVID-19 alcohol data (in progress).

| Description | R-code |
| --- | --- |
| S1 - Multiple imputation using UNSW HPC 'Katana' | [Multiple imputation](Code/2020b/S1_multiple_imputation.R) |
| S2 - Final data creation | [Final data creation](Code/2020b/S2_data_finalise_after_imputation.R) |
| S3 - Import MI data into Stata for analysis | [Stata import](Code/2020b/S3_import_data_into_stata.do) |
| S4 - Cross-sectional descriptives in R | [Cross-sectional descriptives](Code/2020b/S3_import_data_into_stata.do) |
| S5 - Longitudinal descriptives in Stata | [Longitudinal descriptives](Code/2020b/S5_longitudinal_descriptives.do) |

| S6 - Primary analyses using mixed effects models with discrete time | [Primary analyis](Code/2020b/S6_primary_analysis) |
| S7 - Sensitivity analysis using continuous time and 'high risk' consumption variable | [Sensitivity analysis](Code/2020b/S7_sensitivity_analysis) |


1. Lendle SD, Schwab J, Petersen ML, van der Laan MJ. ltmle: An R Package Implementing Targeted Minimum Loss-Based Estimation for Longitudinal Data. Journal of Statistical Software. 2017;81(1):1-21.
