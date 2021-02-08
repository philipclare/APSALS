******************************************************************************
* PROGRAM: S5 Covid Analysis - Import alcohol data into Stata.do
* PURPOSE: Import imputed data into Stata for analysis
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. MICE
*-----------------------------------------------------------------------------
* 1.0 Load MICE imputed data exported from R

cd "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data"
use "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/Alcohol imputed data - long.dta", clear

decode zzC_ID, gen(cid)
destring cid, replace
drop zzC_ID
rename cid zzC_ID

replace imp=0 if imp==51

merge m:1 zzC_ID using "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/COVID Survey States.dta", keepusing(state) keep(match master)

replace ccalcfre=0 if ccalcqnt==0 & ccalcfre!=0
replace ccalcqnt=0 if ccalcfre==0 & ccalcqnt!=0
replace ccalcqnt=.1 if ccalcqnt>0 & ccalcqnt<1

gen alcfqqnt=ccalcfre*ccalcqnt
gen alcgrp=.
forvalues i=0/50 {
	xtile temp=alcfqqnt if zzwave==1 & imp==`i', nq(2)
	replace alcgrp=temp if zzwave==1 & imp==`i'
	drop temp
}

gen cchvyalc=0 if alcfqqnt<40 & ccalcfre<5
replace cchvyalc=1 if (alcfqqnt>=40 & alcfqqnt!=.) | (ccalcfre>=5 & ccalcfre!=.)

gen highrisk=0 if cchvyalc==0 & ccalcbngf<2.5 & zzwave==1
replace highrisk=1 if cchvyalc==1 | ccalcbng>=2.5 & zzwave==1

egen cid = group(imp zzC_ID)
xtset cid zzwave
forvalues i=2/5 {
	replace alcgrp = l1.alcgrp if zzwave==`i'
	replace highrisk = l1.highrisk if zzwave==`i'
}

gen sample=1 if sex_8!=3. & sex_9!=3
replace sample=0 if datecom>td(1feb2020) & zzwave==3

replace ccalcmon=ccalcmon-1
label define ccalcmon 0 "No" 1 "Yes", replace

recode ccsmkfrq 1=0 2=1 3=1
label define ccsmkfrq 0 "No" 1 "Yes", replace
recode ccecigfrq 1=0 2=1 3=1
label define ccecigfrq 0 "No" 1 "Yes", replace
recode cccanfrq 1=0 2=1 3=1
label define cccanfrq 0 "No" 1 "Yes", replace
recode ccecstfrq 1=0 2=1
label define cccanfrq 0 "No" 1 "Yes", replace
recode ccmethfrq 1=0 2=1
label define cccanfrq 0 "No" 1 "Yes", replace

gen lccalcmax=l1.ccalcmax
gen lccalcdlvr=l1.ccalcdlvr
gen lccalcalone=l1.ccalcalone
gen lccalcothers=l1.ccalcothers
gen lccalcvirtual=l1.ccalcvirtual
gen lccecstfrq=l1.ccecstfrq
gen lccmethfrq=l1.ccmethfrq

drop cid

* 1.1 Set data as MI data using mi import
	mi import flong, m(imp) id(zzC_ID zzwave) imp(b_seifa b_famhist b_oldsib sex_8 pinc_8 ///
	age_8 peeruse_8 peerdis_8 datecom ccstndt ccempl ccalcfre ccalcqnt ccalcbngf ccalcmax ///
	ccalcalone ccalcothers ccalcvirtual ccalcdlvr ccalcmon ccsmkfrq ccecigfrq cccanfrq ///
	ccecstfrq ccmethfrq highrisk) passive(alcfqqnt sample lccalcmax lccalcdlvr lccalcalone lccalcothers ///
	lccalcvirtual lccecstfrq lccmethfrq cchvyalc) clear

mi xtset zzC_ID zzwave
mi passive: gen ccalcweekly=ccalcfre>=5 & ccalcfre!=.

mi passive: gen ccalcfcat=0 if ccalcfre==0
mi passive: replace ccalcfcat=1 if ccalcfre>0 & ccalcfre<5
mi passive: replace ccalcfcat=2 if ccalcfre>=5 & ccalcfre!=.

mi passive: gen ccalcqcat=0 if ccalcqnt==0
mi passive: replace ccalcqcat=1 if ccalcqnt>0 & ccalcqnt<5
mi passive: replace ccalcqcat=2 if ccalcqnt>=5 & ccalcqnt!=.

mi passive: gen ccbngfcat=0 if ccalcbngf==0
mi passive: replace ccbngfcat=1 if ccalcbngf>0 & ccalcbngf<5
mi passive: replace ccbngfcat=2 if ccalcbngf>=5 & ccalcbngf!=.

mi passive: gen qtryr=qofd(datecom)
format qtryr %tq

* 1.2 Save newly created data for analysis
save "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/Alcohol imputed data - for analysis.dta", replace
