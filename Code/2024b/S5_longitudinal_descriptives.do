******************************************************************************
* PROGRAM: S6 Covid Analysis - Primary analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

use "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/Alcohol imputed data - for analysis.dta", clear

******************************************************************************
* 1. Trend descriptives
*-----------------------------------------------------------------------------

matrix table1=J(10,12,.)

tab qtryr if sex_8!=3 & _mi_m==1, matcell(x)
	matrix table1[1,1]=x'
mi estimate: prop ccalcfcat, over(qtryr)
	matrix res=e(b_mi)
	matrix table1[2,1]=res[1,1..12] \ res[1,13..24] \ res[1,25..36]
mi estimate: prop ccalcqcat, over(qtryr)
	matrix res=e(b_mi)
	matrix table1[5,1]=res[1,1..12] \ res[1,13..24] \ res[1,25..36]
mi estimate: prop ccbngfcat, over(qtryr)
	matrix res=e(b_mi)
	matrix table1[8,1]=res[1,1..12] \ res[1,13..24] \ res[1,25..36]

matrix list table1

matrix table1v2=J(10,5,.)

tab zzwave if sample==1 & _mi_m==1, matcell(x)
	matrix table1v2[1,1]=x'
mi estimate, esampvaryok: prop ccalcfcat if sample==1, over(zzwave)
	matrix res=e(b_mi)
	matrix table1v2[2,1]=res[1,1..5] \ res[1,6..10] \ res[1,11..15]
mi estimate, esampvaryok: prop ccalcqcat if sample==1, over(zzwave)
	matrix res=e(b_mi)
	matrix table1v2[5,1]=res[1,1..5] \ res[1,6..10] \ res[1,11..15]
mi estimate, esampvaryok: prop ccbngfcat if sample==1, over(zzwave)
	matrix res=e(b_mi)
	matrix table1v2[8,1]=res[1,1..5] \ res[1,6..10] \ res[1,11..15]

matrix list table1v2
