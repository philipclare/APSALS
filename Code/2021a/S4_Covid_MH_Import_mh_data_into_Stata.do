******************************************************************************
* PROGRAM: S5 Covid Analysis - Import mh data into Stata.do
* PURPOSE: Import imputed data into Stata for analysis
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. MICE
*-----------------------------------------------------------------------------
* 1.0 Load MICE imputed data exported from R

global filepath="C:\Users\z3312911\UNSW\APSALS - Documents\Papers\PIP40. COVID-19 Health Paper\Data"
global verdate="20200922"

cd "$filepath"
use "$filepath\Mental health imputed data $verdate.dta", clear

replace imp=0 if imp==51

replace ccphqscore_12=ccphqscore_12-9
replace ccgadscore_12=ccgadscore_12-7

gen srmh=5-ccmenthlth_11
recode srmh 1=0 2=1 3=2 4=2
gen srph=5-ccphyshlth_11
recode srph 1=0 2=1 3=2 4=2
label define pge 0 "Poor-fair" 1 "Good" 2 "Very good-Excellent"
label values srmh srph pge

gen mhchg3=ccmenthlthchng
recode mhchg3 5=0 4=0 3=1 2=2 1=2
gen phchg3=ccphyshlthchng
recode phchg3 5=0 4=0 3=1 2=2 1=2
label define chg 0 "Worse" 1 "Same" 2 "Better"
label values mhchg3 phchg3 chg

gen anyfinstress=ccfinstress
replace anyfinstress=1 if anyfinstress>0 & anyfinstress!=.
label define bfstr 0 "No" 1 "Yes"
label values anyfinstress bfstr


******************************************************************************
* 2. Wide version of imputed data
*-----------------------------------------------------------------------------

preserve

egen phqseverity_10=cut(ccphqscore_10), at(0,5,10,15,20,30) icodes
egen phqseverity_12=cut(ccphqscore_12), at(0,5,10,15,20,30) icodes
label define phqsev 0 "None" 1 "Minimal" 2 "Mild" 3 "Moderately severe" 4 "Severe"
label values phqseverity_10 phqseverity_12 phqsev
egen gadseverity_10=cut(ccgadscore_10), at(0,5,10,15,30) icodes
egen gadseverity_12=cut(ccgadscore_12), at(0,5,10,15,30) icodes
label define gadsev 0 "None" 1 "Mild" 2 "Moderate" 3 "Severe"
label values gadseverity_10 gadseverity_12 gadsev

gen phqdisorder_10=0 if ccphqscore_10<10
replace phqdisorder_10=1 if ccphqscore_10>=10 & ccphqscore_10!=.
gen phqdisorder_12=0 if ccphqscore_12<10
replace phqdisorder_12=1 if ccphqscore_12>=10 & ccphqscore_12!=.
gen gaddisorder_10=0 if ccgadscore_10<10
replace gaddisorder_10=1 if ccgadscore_10>=10 & ccgadscore_10!=.
gen gaddisorder_12=0 if ccgadscore_12<10
replace gaddisorder_12=1 if ccgadscore_12>=10 & ccgadscore_12!=.
label define sev 0 "No" 1 "Yes"
label values phqdisorder_10 phqdisorder_12 gaddisorder_10 gaddisorder_12 sev

gen emplchg=0 if ccempl_12==2
replace emplchg=1 if ccempl_11==2 & ccempl_12==1
replace emplchg=2 if ccempl_11==1 & ccempl_12==1

* 2.1 Set data as MI data using mi import
	mi import flong, m(imp) id(zzC_ID) imp(datecom_10 datecom_12 ///
	b_seifa b_famhist b_oldsib sex_10 pinc_10 age_10 peeruse_10 peerdis_10 diagmh_10 ccmhtrta_10 ccmhtrtb_10 ccmhtrtc_10 ///
	chlivefam_10 chlivefam_12 chliveptn_10 chliveptn_12 chlivehmt_10 chlivehmt_12 chlivesm ccstndt_10 ccstndt_11 ccstndt_12 ///
	ccempl_10 ccempl_11 ccempl_12 cccovidtd cccovidwor 	cccovidslfiso cccovidqnt ///
	ccphqscore_10 ccgadscore_10 ccphqscore_12 phqdisorder_10 phqdisorder_12 ccgadscore_10 ccgadscore_12 gaddisorder_12 ///
	ccphyshlth_11 ccphyshlthchng ccmenthlth_11 ccmenthlthchng cchlthska_11 cchlthska_12 ///
	ccfinstress cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie ///
	cccovidposif cccovidposig cccovidposih) passive(srmh srph mhchg3 phchg3 emplchg) clear

* 2.2 Save newly created data for analysis
save "$filepath\Mental health imputed data wide - for analysis $verdate.dta", replace
restore

******************************************************************************
* 3. Long version of imputed data
*-----------------------------------------------------------------------------

rename *_10 *10
rename *_11 *11
rename *_12 *12

reshape long datecom ccstndt ccempl ccphqscore ccgadscore cchlthska chlivealn chlivefam chliveptn chlivehmt, i(zzC_ID imp) j(zzwave)

egen newid = group(zzC_ID imp)
xtset newid zzwave
gen ccstndt10=ccstndt if zzwave==10
replace ccstndt10=l1.ccstndt10 if zzwave==11
replace ccstndt10=l2.ccstndt10 if zzwave==12

gen ccempl10=ccempl if zzwave==10
replace ccempl10=l1.ccempl10 if zzwave==11
replace ccempl10=l2.ccempl10 if zzwave==12

gen chlivealn10=chlivealn if zzwave==10
replace chlivealn10=l1.chlivealn10 if zzwave==11
replace chlivealn10=l2.chlivealn10 if zzwave==12

xtset, clear
drop newid

recode cchlthska 1=0 2=1
label define cchlthska 0 "No" 1 "Yes", replace

egen phqseverity=cut(ccphqscore), at(0,5,10,15,20,30) icodes
label define phqsev 0 "None" 1 "Minimal" 2 "Mild" 3 "Moderately severe" 4 "Severe"
label values phqseverity phqsev
egen gadseverity=cut(ccgadscore), at(0,5,10,15,30) icodes
label define gadsev 0 "None" 1 "Mild" 2 "Moderate" 3 "Severe"
label values gadseverity gadsev

gen phqmodsev=0 if ccphqscore<15
replace phqmodsev=1 if ccphqscore>=15 & ccphqscore!=.
gen gadmodsev=0 if ccgadscore<10
replace gadmodsev=1 if ccgadscore>=10 & ccgadscore!=.
label define modsev 0 "No" 1 "Yes"
label values phqmodsev gadmodsev modsev

gen phqdisorder=0 if ccphqscore<10
replace phqdisorder=1 if ccphqscore>=10 & ccphqscore!=.
gen gaddisorder=0 if ccgadscore<10
replace gaddisorder=1 if ccgadscore>=10 & ccgadscore!=.
label define sev 0 "No" 1 "Yes"
label values phqdisorder gaddisorder sev

gen sample=1 if sex10!=3 & zzwave!=11
replace sample=0 if datecom>=td(14mar2020) & zzwave==10

* 3.1 Set data as MI data using mi import
	mi import flong, m(imp) id(zzC_ID zzwave) imp(datecom b_seifa b_famhist b_oldsib ///
	sex10 pinc10 age10 peeruse10 peerdis10 diagmh10 ccmhtrta10 ccmhtrtb10 ccmhtrtc10 chlivealn10 chlivealn chlivefam chliveptn chlivehmt chlivesm ///
	ccstndt10 ccstndt ccempl10 ccempl cccovidtd cccovidwor cccovidslfiso cccovidqnt ///
	ccgadscore ccphqscore ccphyshlth11 ccphyshlthchng ///
	ccmenthlth11 ccmenthlthchng cchlthska ccfinstress ///
	cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig ///
	cccovidposih) passive(srmh srph phqseverity gadseverity phqmodsev gadmodsev phqsev gadsev) clear

* 3.2 Save newly created data for analysis
save "$filepath\Mental health imputed data long - for analysis $verdate.dta", replace
