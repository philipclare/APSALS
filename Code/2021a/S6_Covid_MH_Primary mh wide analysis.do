******************************************************************************
* PROGRAM: S6 Covid Analysis - Primary analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

global filepath="C:\Users\z3312911\UNSW\APSALS - Documents\Papers\PIP40. COVID-19 Health Paper\Data"
global verdate="20200922"

cd "$filepath"
use "$filepath\Mental health imputed data wide - for analysis $verdate.dta", clear

******************************************************************************
* 1. Mental health
*-----------------------------------------------------------------------------

* 1.1 Table 1 - Descriptives
matrix table1=J(13,4,.)
mi estimate, esampvaryok: mean ccphqscore_10
	matrix mn=r(table)
	mata st_matrix("samp",sqrt(st_matrix("e(_N)")))
	matrix table1[1,1]=mn[1,1]
	matrix table1[1,2]=mn[2,1]'*samp
mi estimate, esampvaryok: prop phqdisorder_10
	matrix mn=r(table)
	matrix table1[2,1]=mn[1,2]
mi estimate, esampvaryok: prop phqseverity_10
	matrix mn=r(table)
	matrix table1[3,1]=mn[1,1..5]'
mi estimate, esampvaryok: mean ccgadscore_10
	matrix mn=r(table)
	matrix table1[8,1]=mn[1,1]
	matrix table1[8,2]=mn[2,1]'*samp
mi estimate, esampvaryok: prop gaddisorder_10
	matrix mn=r(table)
	matrix table1[9,1]=mn[1,2]
mi estimate, esampvaryok: prop gadseverity_10
	matrix mn=r(table)
	matrix table1[10,1]=mn[1,1..4]'
mi estimate, esampvaryok: mean ccphqscore_12
	matrix mn=r(table)
	mata st_matrix("samp",sqrt(st_matrix("e(_N)")))
	matrix table1[1,3]=mn[1,1]
	matrix table1[1,4]=mn[2,1]'*samp
mi estimate, esampvaryok: prop phqdisorder_12
	matrix mn=r(table)
	matrix table1[2,3]=mn[1,2]
mi estimate, esampvaryok: prop phqseverity_12
	matrix mn=r(table)
	matrix table1[3,3]=mn[1,1..5]'
mi estimate, esampvaryok: mean ccgadscore_12
	matrix mn=r(table)
	matrix table1[8,3]=mn[1,1]
	matrix table1[8,4]=mn[2,1]'*samp
mi estimate, esampvaryok: prop gaddisorder_12
	matrix mn=r(table)
	matrix table1[9,3]=mn[1,2]
mi estimate, esampvaryok: prop gadseverity_12
	matrix mn=r(table)
	matrix table1[10,3]=mn[1,1..4]'
	
* 1.2 Table 2 - Descriptives
matrix table2=J(23,2,.)	
mi estimate, esampvaryok: mean ccfinstress
	matrix mn=r(table)
	matrix table2[1,1]=mn[1,1]
	matrix table2[1,2]=mn[2,1]'*samp
mi estimate, esampvaryok: prop anyfinstress
	matrix mn=r(table)
	matrix table2[2,1]=mn[1,2]
mi estimate, esampvaryok: prop ccmenthlth_11
	matrix mn=r(table)
	matrix table2[3,1]=mn[1,1..5]'
mi estimate, esampvaryok: prop mhchg3
	matrix mn=r(table)
	matrix table2[8,1]=mn[1,1..3]'
mi estimate, esampvaryok: prop cccovidwor
	matrix mn=r(table)
	matrix table2[11,1]=mn[1,1..5]'
mi estimate, esampvaryok: prop ccphyshlth_11
	matrix mn=r(table)
	matrix table2[16,1]=mn[1,1..5]'
mi estimate, esampvaryok: prop phchg3
	matrix mn=r(table)
	matrix table2[21,1]=mn[1,1..3]'

matrix list table1
matrix list table2

* 1.3 Table E2 - Descriptives
matrix tableE2=J(6,2,.)
mi estimate, esampvaryok: prop ccstndt_10
	matrix mn=r(table)
	matrix tableE2[1,1]=mn[1,2]
mi estimate, esampvaryok: prop ccempl_10
	matrix mn=r(table)
	matrix tableE2[2,1]=mn[1,2]
mi estimate, esampvaryok: prop chlivealn_10
	matrix mn=r(table)
	matrix tableE2[3,1]=mn[1,2]
mi estimate, esampvaryok: prop chlivefam_10
	matrix mn=r(table)
	matrix tableE2[4,1]=mn[1,2]
mi estimate, esampvaryok: prop chliveptn_10
	matrix mn=r(table)
	matrix tableE2[5,1]=mn[1,2]
mi estimate, esampvaryok: prop chlivehmt_10
	matrix mn=r(table)
	matrix tableE2[6,1]=mn[1,2]
mi estimate, esampvaryok: prop ccstndt_12
	matrix mn=r(table)
	matrix tableE2[1,2]=mn[1,2]
mi estimate, esampvaryok: prop ccempl_12
	matrix mn=r(table)
	matrix tableE2[2,2]=mn[1,2]
mi estimate, esampvaryok: prop chlivealn_12
	matrix mn=r(table)
	matrix tableE2[3,2]=mn[1,2]
mi estimate, esampvaryok: prop chlivefam_12
	matrix mn=r(table)
	matrix tableE2[4,2]=mn[1,2]
mi estimate, esampvaryok: prop chliveptn_12
	matrix mn=r(table)
	matrix tableE2[5,2]=mn[1,2]
mi estimate, esampvaryok: prop chlivehmt_12
	matrix mn=r(table)
	matrix tableE2[6,2]=mn[1,2]
	
matrix list tableE2
	
* 1.4 Table E3 - Correlations between mental health variables 
matrix tableE3=J(4,4,.)
local vars "cccovidwor ccphqscore_12 ccgadscore_12 ccfinstress"
matrix colnames tableE3 = `vars'
matrix rownames tableE3 = `vars'
forval i=1/4 {
	forval j=1/4 {
		local xvar `: word `i' of `vars''
		local yvar `: word `j' of `vars''
		scalar corr=0
		mi xeq 1/50: correlate `xvar' `yvar' ; scalar corr = corr + atanh(r(rho))
		scalar corr = tanh(corr/50)
		matrix tableE3[`i',`j']=corr
	}
}
matrix list tableE3

******************************************************************************
* 2. Perceived change in mental health
*-----------------------------------------------------------------------------

* 2.1 Table E7 - Perceived change in mental health
mi estimate, esampvaryok dots: ///
mlogit mhchg3 i.srmh i.srph i.sex_10 c.age_10 ccstndt_10 ccempl_10 chlivealn_10 i.b_seifa b_oldsib peeruse_10 peerdis_10 if sex_10!=3, b(1)	
	
* 2.2 Figure 2 and Tables E11 - Perceived change in mental health - time by gender interaction
mi estimate (c1: _b[Worse:1.srmh]) (c2: _b[Worse:2.srmh]) (c3: _b[Worse:2.sex_10]) ///
(c4: _b[Worse:1.srmh] + _b[Worse:2.sex_10] + _b[Worse:1.srmh#2.sex_10]) ///
(c5: _b[Worse:2.srmh] + _b[Worse:2.sex_10] + _b[Worse:2.srmh#2.sex_10]) ///
(c6: _b[Better:1.srmh]) (c7: _b[Better:2.srmh]) (c8: _b[Better:2.sex_10]) ///
(c9: _b[Better:1.srmh] + _b[Better:2.sex_10] + _b[Better:1.srmh#2.sex_10]) ///
(c10: _b[Better:2.srmh] + _b[Better:2.sex_10] + _b[Better:2.srmh#2.sex_10]), esampvaryok dots: ///
mlogit mhchg3 i.srmh##i.sex_10 i.srph c.age_10 ccstndt_10 ccempl_10 chlivealn_10 i.b_seifa b_oldsib peeruse_10 peerdis_10 if sex_10!=3, b(1)

******************************************************************************
* 3. Positive actions around COVID
*-----------------------------------------------------------------------------

* 3.1 Table E14 - Positive actions around COVID table
matrix tableE14=J(8,1,.)
mi estimate, esampvaryok: prop cccovidposia
	matrix temp=e(b_mi)
	matrix tableE14[1,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposib
	matrix temp=e(b_mi)
	matrix tableE14[2,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposic
	matrix temp=e(b_mi)
	matrix tableE14[3,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposid
	matrix temp=e(b_mi)
	matrix tableE14[4,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposie
	matrix temp=e(b_mi)
	matrix tableE14[5,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposif
	matrix temp=e(b_mi)
	matrix tableE14[6,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposig
	matrix temp=e(b_mi)
	matrix tableE14[7,1]=temp[1,2]
mi estimate, esampvaryok: prop cccovidposih
	matrix temp=e(b_mi)
	matrix tableE14[8,1]=temp[1,2]
	
matrix list tableE14

* 3.2 % reporting any positive behaviour and mean/sd of number of behaviours, for text
foreach i in cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih {
	recode `i' 1=0 2=1
}
label define noyes 0 "No" 1 "Yes"
label values cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih noyes

egen posi=rowtotal(cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih), missing
mi estimate: prop posi
mi estimate: mean posi
	matrix mn=r(table)
	mata st_matrix("samp",sqrt(st_matrix("e(_N)")))
	matrix temp=mn[2,1]'*samp
	matrix list temp

* 3.3 Table E15 - Associations between positive actions and mental health/financial stress
matrix tableE15a=J(8,12,.)
matrix tableE15b=J(8,12,.)
local posi="cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih"

forvalues i=1/8 {
	local j `: word `i' of `posi''
	mi estimate: regress ccphqscore_12 `j' if sex_10!=3
		matrix temp=r(table)'
		matrix tableE15a[`i',1]=temp[1,1..6]
}
mi estimate: regress ccphqscore_12 cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih ///
i.sex_10 c.age_10 ccstndt_10 ccempl_10 chlivealn_10 i.b_seifa b_oldsib diagmh peeruse_10 peerdis_10 if sex_10!=3
	matrix temp=r(table)'
	matrix tableE15a[1,7]=temp[1..8,1..6]

forvalues i=1/8 {
	local j `: word `i' of `posi''
	mi estimate: regress ccgadscore_12 `j' if sex_10!=3
		matrix temp=r(table)'
		matrix tableE15b[`i',1]=temp[1,1..6]
}
mi estimate: regress ccgadscore_12 cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih ///
i.sex_10 c.age_10 ccstndt_10 ccempl_10 chlivealn_10 i.b_seifa b_oldsib peeruse_10 peerdis_10 if sex_10!=3
	matrix temp=r(table)'
	matrix tableE15b[1,7]=temp[1..8,1..6]
	
matrix list tableE15a
matrix list tableE15b