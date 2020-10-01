******************************************************************************
* PROGRAM: S6 Covid Analysis - Primary analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

use "C:\Users\pjclare\Desktop\APSALS\COVID Survey\Data\Alcohol imputed data - for analysis.dta", clear

******************************************************************************
* 1. Trend descriptives
*-----------------------------------------------------------------------------

matrix table1=J(10,8,.)

tab qtryr if sex_9!=3 & _mi_m==1, matcell(x)
	matrix table1[1,1]=x'
mi estimate: prop ccalcfcat, over(qtryr)
	matrix res=e(b_mi)
	matrix table1[2,1]=res[1,1..8] \ res[1,9..16] \ res[1,17..24]
mi estimate: prop ccalcqcat, over(qtryr)
	matrix res=e(b_mi)
	matrix table1[5,1]=res[1,1..8] \ res[1,9..16] \ res[1,17..24]
mi estimate: prop ccbngfcat, over(qtryr)
	matrix res=e(b_mi)
	matrix table1[8,1]=res[1,1..8] \ res[1,9..16] \ res[1,17..24]

matrix list table1

matrix table1v2=J(10,4,.)

tab zzwave if sample==1 & _mi_m==1, matcell(x)
	matrix table1v2[1,1]=x'
mi estimate: prop ccalcfcat if sample==1, over(zzwave)
	matrix res=e(b_mi)
	matrix table1v2[2,1]=res[1,1..4] \ res[1,5..8] \ res[1,9..12]
mi estimate: prop ccalcqcat if sample==1, over(zzwave)
	matrix res=e(b_mi)
	matrix table1v2[5,1]=res[1,1..4] \ res[1,5..8] \ res[1,9..12]
mi estimate: prop ccbngfcat if sample==1, over(zzwave)
	matrix res=e(b_mi)
	matrix table1v2[8,1]=res[1,1..4] \ res[1,5..8] \ res[1,9..12]

matrix list table1v2
