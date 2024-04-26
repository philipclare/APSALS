//**************************************************************************//
//   
// APSALS Wave 11 Initiation Trajectories Paper
// Create figures from Stata analysis output
// Author: Philip J Clare
// Date: 19 June 2023
// Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
// Analysis pre-registered at DOI:10.17605/OSF.IO/BRDUV
//
//**************************************************************************//
// 1. Setup Environment
//--------------------------------------------------------------------------//

// 1.1. Specify working directories

global datadir "D:/UNSW/APSALS - Documents/Data/Master datasets/Wave 11"
global workdir "D:/UNSW/APSALS - Documents/Papers/PIP46. Effect of age of initiation on trajectory of alcohol use and harm"

//**************************************************************************//
// 2. Load data
//--------------------------------------------------------------------------//

use "$datadir/Combined Data Wave 11 26May2023.dta" , clear

//**************************************************************************//
// 3. Remove empty cases and run internal consistency checks
//--------------------------------------------------------------------------//

// Drop if child did not complete survey
drop if cqcomp==0

recode ccsipage (8=9) (9=8)
label define Agetrylbl 8 "Over 18 years" 9 "Never had any alcohol", modify

// Freq and quant to 0 if never had alcohol, not tried alcohol in past year, or inconsistent
replace ccalcf = 0 if ccalctryyr == 0 | ccsipage == 9 | ccalcq == 0 | ccalcqnt == 0 | ccalc == 0
replace ccalcf_w11 = 0 if ccalcf == 0
replace ccalcfre = 0 if ccalcf == 0
replace ccalcfre_w11 = 0 if ccalcf == 0
replace ccalcfmonth = 0 if ccalcf == 0
replace ccalcq = 0 if ccalcf == 0 | ccalcfre == 0 
replace ccalcq_w11 = 0 if ccalcq == 0 | ccalcf_w11 == 0
replace ccalcqnt = 0 if ccalcq == 0
replace ccalcqnt_w11 = 0 if ccalcq == 0 | ccalcfre_w11 == 0 
replace ccalcqntB = 0 if ccalcq == 0
replace ccalcqntB_w11 = 0 if ccalcq == 0 | ccalcfre_w11 == 0 
replace ccalcqmonth = 0 if ccalcf == 0
replace ccalcFQ = 0 if ccalcq == 0 | ccalcf == 0
replace ccalcFQ_w11 = 0 if ccalcq == 0 | ccalcf == 0 | ccalcf_w11 == 0 | ccalcq_w11 == 0 
replace ccalcNSD = 0 if ccalcq == 0 | ccalcf == 0
replace ccalcmthly = 0 if ccalcq == 0 | ccalcf == 0
replace ccalc = 0 if ccalcq == 0 | ccalcf == 0
replace ccalcmax = 0 if ccalcq == 0 | ccalcf == 0

// No supply if had not consumed alcohol at all
foreach i in a b c d e e1 e2 e3 f g h {
	replace ccalcsrc`i'f=0 if ccalcf==0 | ccalcsrc`i'q==0 | ccsipage == 9
	replace ccalcsrc`i'_fre=0 if ccalcf==0 | ccalcsrc`i'q==0 | ccsipage == 9
	replace ccalcsrc`i'_FQ=0 if ccalcf==0 | ccalcsrc`i'q==0 | ccsipage == 9
	replace ccalcsrc`i'_qnt=0 if ccalcf==0 | ccalcsrc`i'q==0 | ccsipage == 9
	replace ccalcsrc`i'q=0 if ccalcf==0 | ccalcsrc`i'f==0 | ccsipage == 9
	replace ccalcsrc`i'_nsd=0 if ccalcf==0 | ccalcsrc`i'f==0 | ccsipage == 9
}

// No harms if had not consumed alcohol at all
foreach i in a b c d e f g h i j k l m n o p q {
	replace cchrm`i' = 0 if ccalcf == 0 | ccalcq == 0
}

// Correct inconsistent data for other alcohol-related variables 
replace ccparsupFQ = 0 if ccalcq == 0 | ccalcf == 0
replace ccparsupYN = 0 if ccalcq == 0 | ccalcf == 0
replace ccparsupNSD = 0 if ccalcq == 0 | ccalcf == 0
replace ccalcbngf = 0 if ccalcq == 0 | ccalcf == 0
replace ccalcbngq = 0 if ccalcq == 0 | ccalcf == 0
replace ccalcbngf_w11 = 0 if ccalcq == 0 | ccalcf == 0 | ccalcf_w11 == 0 | ccalcq_w11 == 0 
replace ccalcbngq_w11 = 0 if ccalcq == 0 | ccalcf == 0 | ccalcf_w11 == 0 | ccalcq_w11 == 0 
replace ccbsup = 0 if ccalcq == 0 | ccalcf == 0

//**************************************************************************//
// 4. Generate outcome variables
//--------------------------------------------------------------------------//

// 4.1. Overall alcohol consumption
gen freq=ccalcf 
gen quant=ccalcq 

// Recode ordinal consumption variables to reflect their response category
recode freq 0=0 1=5.5 2=12 3=30 4=78 5=182 6=286 7=365
recode quant 0=0 1=0.1 2=1.5 3=3.5 4=5.5 5=8.5 6=11.5 7=13

gen alcfq=freq*quant
replace alcfq=0 if ccsipage==8

// 4.2. Heavy episodic drinking
rename ccalcbngf bingef
replace ccalcbngf=0 if ccsipage==8

gen hed_cat=0 if bingef==0
replace hed_cat=1 if bingef==1
replace hed_cat=2 if bingef>1 & bingef!=.

gen anyhed=0 if hed_cat==0
replace anyhed=1 if hed_cat>0 & hed_cat!=.

gen monthlyhed=0 if hed_cat<=1
replace monthlyhed=1 if hed_cat>1 & hed_cat!=.

// 4.3. Alcohol-related harms
rename cchrm_score totharms
replace totharms=0 if ccsipage==8
foreach i in a b c d e f g h i j k l m n o p q {
	gen hrm`i'=cchrm`i'
	replace hrm`i'=1 if hrm`i'>1 & hrm`i'!=.
	gen f1hrm`i'=f1.hrm`i'
}
egen numharms=rowtotal(hrmb hrmc hrmd hrme hrmg hrmh hrmi hrmj hrmk hrml hrmm hrmn hrmo hrmp), missing
replace numharms=0 if ccsipage==8

gen harmsnum14 = 0
gen harmsnomiss14 = 0
foreach i in b c d e g h i j k l m n o p  {
	replace harmsnum14 = harmsnum14 + 1 if cchrm`i' > 0 & !missing(cchrm`i') & ccsipage != 9 & ccalcf != 0
	replace harmsnomiss14 = harmsnomiss14 + 1 if !missing(cchrm`i')
}
replace harmsnum14 = (harmsnum14/harmsnomiss14)*14
label var harmsnum14 "PRO RATA Num different alcohol-related harms experienced (0-14)"
replace harmsnum14=0 if ccsipage==8

// 4.4. DSM Outcomes
// For the 9 abuse items, recode initial "more than once" follow-up as no if response to initial question was negative or if didn't drink enough
foreach i in ccdiscabA ccdiscabB ccdiscabC ccdiscabD ccdiscabE ccdiscabF ccdiscabG ccdiscabG ccdiscabH ccdiscabI {
	replace `i'=0 if (ccalcf==0 | ccalcf==1 | ccsipage==8) & zzwave>1
	replace `i'b=0 if `i'==0
}
replace ccdiscabGc=0 if ccdiscabG==0
// For the dependence items, recode if didn't drink enough
foreach i in ccdiscdepa ccdiscdepb ccdiscdepc ccdiscdepd ccdiscdepe ccdiscdepf ccdiscdepg ccdiscdeph ccdiscdepi ccdiscdepj ccdiscdepm ccdiscdepn ccdiscdepo ccdiscdepp ccdiscdepq ccdiscdepr ccdiscdeps_a ccdiscdeps_b ccdiscdeps_c ccdiscdeps_d ccdiscdeps_e ccdiscdeps_f ccdiscdeps_g ccdiscdeps_h ccdiscdeps_i ccdiscdeps_j ccdiscdeps_k ccdiscdeps_l ccdiscdept {
	replace `i'=0 if (ccalcf==0 | ccalcf==1 | ccsipage==8) & zzwave>4
}

// For each of the 7 symptoms of dependence, create an indicator of whether that symptom was experienced
gen deplarger=.
replace deplarger=0 if ccdiscdepa==0 & ccdiscdepb==0
replace deplarger=1 if ccdiscdepa==1 | ccdiscdepb==1
gen depquit=.
replace depquit=0 if (ccdiscdepc==0 | ccdiscdepd==1) & ccdiscdepe==0
replace depquit=1 if (ccdiscdepc==1 & ccdiscdepd==0) | ccdiscdepe==1
gen deptolerance=.
replace deptolerance=0 if ccdiscdepf==0 & ccdiscdepg==0
replace deptolerance=1 if ccdiscdepf==1 | ccdiscdepg==1
gen deptimesp=.
replace deptimesp=0 if ccdiscdeph==0 & ccdiscdepi==0 & ccdiscdepj==0
replace deptimesp=1 if ccdiscdeph==1 | ccdiscdepi==1 | ccdiscdepj==1
gen depgiveup=.
replace depgiveup=0 if ccdiscdepm==0 
replace depgiveup=1 if ccdiscdepm==1
gen depphysical=.
replace depphysical=0 if (ccdiscdepn==0 | ccdiscdepo==0) & (ccdiscdepp==0 | ccdiscdepq==0)
replace depphysical=1 if (ccdiscdepn==1 & ccdiscdepo==1) | (ccdiscdepp==1 & ccdiscdepq==1)
gen depwithdr=.
replace depwithdr=0 if ccdiscdepr==0 & ccdiscdept==0
replace depwithdr=1 if (ccdiscdepr==1 & (ccdiscdeps_a==1 | ccdiscdeps_b==1 | ccdiscdeps_c==1 | ccdiscdeps_d==1 | ccdiscdeps_e==1 | ccdiscdeps_f==1 | ccdiscdeps_g==1 | ccdiscdeps_h==1 | ccdiscdeps_i==1 | ccdiscdeps_j==1 | ccdiscdeps_k==1 | ccdiscdeps_l==1)) | ccdiscdept==1

// For each of the 4 symptoms of abuse, create an indicator of whether that symptom was experienced
gen abmajrole=.
replace abmajrole=0 if (ccdiscabA==0 & ccdiscabAb==0) & (ccdiscabB==0 & ccdiscabBb==0) & (ccdiscabC==0 & ccdiscabCb==0) & (ccdiscabD==0 & ccdiscabDb==0)
replace abmajrole=1 if (ccdiscabA==1 & ccdiscabAb==1) | (ccdiscabB==1 & ccdiscabBb==1) | (ccdiscabC==1 & ccdiscabCb==1) | (ccdiscabD==1 & ccdiscabDb==1)
gen absocial=.
replace absocial=0 if (ccdiscabE==0 & ccdiscabEb==0) & (ccdiscabG==0 & ccdiscabGb==0)
replace absocial=1 if (ccdiscabE==1 & ccdiscabEb==1) | (ccdiscabG==1 & ccdiscabGb==1 & ccdiscabGc==1)
gen abhazard=.
replace abhazard=0 if (ccdiscabH==0 & ccdiscabHb==0)
replace abhazard=1 if (ccdiscabH==1 & ccdiscabHb==1)
gen ablegal=.
replace ablegal=0 if (ccdiscabI==0 & ccdiscabIb==0)
replace ablegal=1 if (ccdiscabI==1 & ccdiscabIb==1)

// Create an indicator for the additional symptom added in the DSM-V
gen smpcrave=.
replace smpcrave=0 if ccdiscdepl==0 
replace smpcrave=1 if ccdiscdepl==1

// Count the number of Abuse (DSM-IV), Dependence (DSM-IV) and AUD (DSM-V) symptoms
egen depend=rowtotal(deplarger depquit deptolerance deptimesp depgiveup depphysical depwithdr) if zzwave>=5, missing
egen abuse=rowtotal(abmajrole absocial abhazard ablegal) if zzwave>=2, missing
egen aud=rowtotal(abmajrole absocial abhazard deplarger depquit deptolerance deptimesp depgiveup depphysical depwithdr smpcrave) if zzwave>=5, missing
// Recode each as 0 if respondent did not report consuming alcohol
replace depend=0 if (ccalcf==0 | ccalcf==1) & zzwave>4
replace abuse=0 if (ccalcf==0 | ccalcf==1) & zzwave>1
replace aud=0 if (ccalcf==0 | ccalcf==1) & zzwave>4

// Generate 'diagnosis' variables
// For dependence (DSM-IV), criteria is at least three of the seven symptoms
gen dependdiag=depend
recode dependdiag (1=0) (2=0) (3=1) (4=1) (5=1) (6=1) (7=1)
// For abuse (DSM-IV), criteria is at least one of the four symptoms
gen abusediag=abuse
recode abusediag (2=1) (3=1) (4=1)
// Create alternate version of 'abuse' in waves 5/6, taking into account additional criteria of no dependence
gen abusediagb=abusediag if dependdiag!=.
replace abusediagb=0 if abusediagb==1 & dependdiag==1
// For AUD (DSM-V), criteria is at least two of the eleven symptoms
gen auddiag=aud
recode auddiag (1=0) (2=1) (3=1) (4=1) (5=1) (6=1) (7=1) (8=1) (9=1) (10=1) (11=1)

//**************************************************************************//
// 5. Generate covariates
//--------------------------------------------------------------------------//

// Rename/label time-varying covariates for analysis
gen sex=c_sex
gen hinc=pfinc
gen singlep=1-cfbthpar
gen age=c_age
gen paruse=ppalcFQ
gen alcrules=ccrules_score
gen parmon=cpmon_score
gen peeruse=ccpruse_score
gen peerdis=ccprdis_score
gen homeacc=pchom_score
gen famconf=pfrel_cf
gen famposi=pfrel_pr
gen alcmoney=ccalcmon
recode smoking (2=1)
label define smk 0 "Not true" 1 "Somewhat, sometimes, very or often true"
label values smoking smk
gen hhavguse=pfalcFQ_avg
gen parcon=ppstrict_pcon
gen school=ccscl_r 
gen schooltyp=ccscltyp 
gen alcnorms=ccnorms_score
 
// Create baseline variables of time-varying (to be carried forward)
gen b_sex=c_sex if zzwave==1
gen b_hinc=pfinc if zzwave==1
gen b_singlep=singlep if zzwave==1
gen b_age=c_age if zzwave==1
gen b_paruse=paruse if zzwave==1
gen b_alcrules=alcrules if zzwave==1
gen b_parmon=cpmon_score if zzwave==1
gen b_peeruse=peeruse if zzwave==1
gen b_peerdis=peerdis if zzwave==1
gen b_homeacc=homeacc if zzwave==1
gen b_famconf=famconf if zzwave==1
gen b_famposi=famposi if zzwave==1
gen b_alcmoney=alcmoney if zzwave==1
gen b_smoking=smoking if zzwave==1
gen b_hhavguse=hhavguse if zzwave==1
gen b_parcon=ppstrict_pcon if zzwave==1
gen b_cbclextn=cbclextn if zzwave==1
gen b_cbcladn=cbcladn if zzwave==1
gen b_cbclspn=cbclspn if zzwave==1
gen b_cbclwdn=cbclwdn if zzwave==1
gen b_alcnorms=alcnorms if zzwave==1

// Create and label time-constant variables
gen b_parborn=ppbrn
gen b_pareduc=0 if ppedu_baseline==0 | ppedu_baseline==1
replace b_pareduc=1 if ppedu_baseline==2
replace b_pareduc=2 if ppedu_baseline==3 | ppedu_baseline==4
label define edu 0 "High school or less" 1 "Diploma, Trade, non-trade" 2 "University degree"
label values b_pareduc edu
label var b_pareduc "Parent education"
gen b_parempl=0 if ppemp==1
replace b_parempl=1 if ppemp==2
replace b_parempl=2 if (ppemp==0 | ppemp>2) & ppemp!=.
label define emp 0 "Employed (full-time/part-time)" 1 "Unemployed - in workforce" 2 "Unemployed - not in workforce"
label values b_parempl emp
label var b_parempl "Parent employment status"
gen b_pardem=cpdmd_score
gen b_parres=cprsp_score
gen b_seifa=seifa_lowhigh
gen b_famhist=pffamhis
recode b_famhist (0=.) (2=0) (3=.)
label values b_famhist ny
gen b_oldsib=ppoldsibsYN
gen b_parrel=pprelimp
recode b_parrel (1=0) (2=0) (3=1) (4=1)
label define rel 0 "Not/a little" 1 "Pretty/very"
label values b_parrel rel
label var b_parrel "Importance of religion"

egen w=max(zzwave)
local maxw=w
drop w
replace c_age=(cqdatcom-ccdob)/365.25 if c_age==. & cqdatcom!=. & ccdob!=.

// Loop over waves after baseline to carry forward baseline/time-invariant covariates
foreach k in b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn b_alcnorms {
	forvalues i=2/`maxw' {
		local j=`i'-1
		replace `k'=l`j'.`k' if zzwave==`i'
	}
}

//**************************************************************************//
// 5. Generate initiation variables
//--------------------------------------------------------------------------//

keep zzC_ID zzwave c_age ccalcNSD ///
alcfq hed_cat anyhed monthlyhed totharms harmsnum14 abusediag dependdiag auddiag ///
hinc singlep hhavguse alcrules parmon parcon peeruse peerdis homeacc famconf famposi alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn b_alcnorms ///
ccsipage_A ccfulage_A

rename harmsnum14 numharms

replace ccfulage_A=. if ccfulage_A==1
 
reshape wide c_age ccalcNSD ///
alcfq hed_cat anyhed monthlyhed totharms numharms abusediag dependdiag auddiag ///
hinc singlep hhavguse alcrules parmon parcon peeruse peerdis homeacc famconf famposi alcmoney ccsipage_A ccfulage_A, i(zzC_ID) j(zzwave)
reshape long

foreach i in hinc singlep hhavguse alcrules parmon parcon peeruse peerdis homeacc famconf famposi alcmoney {
	rename `i' b_`i'
	forvalues j=1/3 {
		replace b_`i'=l1.b_`i' if l1.b_`i'!=.
	}
}

xtset zzC_ID zzwave

gen wave_18=.
gen wave_init_a=.
replace wave_init_a=1 if ccsipage_A!=. & zzwave==1 & int(c_age-1)<=(ccsipage_A+4)
replace wave_init_a=-1 if wave_init_a==. & zzwave==1 & int(c_age-1)>(ccsipage_A+4)
replace wave_init_a=-1 if ccsipage_A==. & ccalcNSD>0 & zzwave==1

gen wave_init_b=.
replace wave_init_b=1 if ccfulage_A!=. & zzwave==1 & int(c_age-1)<=(ccfulage_A+3)
replace wave_init_b=-1 if wave_init_b==. & zzwave==1 & int(c_age-1)>(ccfulage_A+3)
replace wave_init_b=-1 if ccfulage_A==. & ccalcNSD>0 & zzwave==1

gen age_init_a=.
replace age_init_a=ccsipage_A+4 if ccsipage_A!=. & zzwave==1 & int(c_age-1)>=(ccsipage_A+4)
replace age_init_a=-1 if age_init_a==. & zzwave==1 & int(c_age-1)>(ccsipage_A+4)
replace age_init_a=-1 if ccsipage_A==. & ccalcNSD>0 & zzwave==1

gen age_init_b=.
replace age_init_b=ccfulage_A+3 if ccfulage_A!=. & zzwave==1 & int(c_age-1)<=(ccfulage_A+3)
replace age_init_b=-1 if age_init_b==. & zzwave==1 & int(c_age-1)>(ccfulage_A+3)
replace age_init_b=-1 if ccfulage_A==. & ccalcNSD>0 & zzwave==1

forvalues i=2/`maxw' {
	replace wave_18=l1.wave_18 if wave_18==. & l1.wave_18!=.
	replace age_init_a=l1.age_init_a if age_init_a==. & l1.age_init_a!=.
	replace age_init_b=l1.age_init_b if age_init_b==. & l1.age_init_b!=.
	replace wave_init_a=l1.wave_init_a if wave_init_a==. & l1.wave_init_a!=.
	replace wave_init_b=l1.wave_init_b if wave_init_b==. & l1.wave_init_b!=.
	replace wave_18=`i' if c_age>=18 & zzwave==`i' & wave_18==. & l1.wave_18==.
	replace wave_init_a=`i' if ccalcNSD>0 & ccalcNSD!=. & zzwave==`i' & wave_init_a==. & l1.wave_init_a==.
	replace wave_init_b=`i' if ccalcNSD>1 & ccalcNSD!=. & zzwave==`i' & wave_init_b==. & l1.wave_init_b==.
	replace age_init_a=int(c_age-0.5) if ccalcNSD>0 & ccalcNSD!=. & zzwave==`i' & age_init_a==. & l1.age_init_a==.
	replace age_init_b=int(c_age-0.5) if ccalcNSD>1 & ccalcNSD!=. & zzwave==`i' & age_init_b==. & l1.age_init_b==.
}

bysort zzC_ID: egen maxw=max(zzwave)
drop if age_init_a==. & maxw>8
replace age_init_b=99 if age_init_b==. & maxw>8

keep if zzwave>=wave_init_a

bysort zzC_ID: gen wave=_n-1
replace age_init_a=age_init_a-11
replace age_init_b=age_init_b-11
drop if age_init_a<0 | age_init_b<0 | (age_init_a>9 & age_init_a!=.)

drop ccsipage_A ccfulage_A b_alcnorms wave_18 wave_init_a wave_init_b

//**************************************************************************//
// 6. Save cleaned data ready for imputation
//--------------------------------------------------------------------------//

save "$workdir\Data\initiation_analysis_data.dta", replace

drop if age_init_a<0 | age_init_b<0
replace age_init_a=age_init_a+11
replace age_init_b=age_init_b+11
