
use "C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP43. Role of peers on smoking and ecigarette use\Data\imputed-mice.dta", clear

egen newid=group(imp zzC_ID)

xtset newid zzwave

gen yr=0 if ccsmkyr==0 & ccecigyr==0
replace yr=1 if ccsmkyr==1 & ccecigyr==0
replace yr=2 if ccsmkyr==0 & ccecigyr==1
replace yr=3 if ccsmkyr==1 & ccecigyr==1

gen week=0 if ccsmkweekly==0 & ccecigweekly==0
replace week=1 if ccsmkweekly==1 & ccecigweekly==0
replace week=2 if ccsmkweekly==0 & ccecigweekly==1
replace week=3 if ccsmkweekly==1 & ccecigweekly==1

gen daily=0 if ccsmkdaily==0 & ccecigdaily==0
replace daily=1 if ccsmkdaily==1 & ccecigdaily==0
replace daily=2 if ccsmkdaily==0 & ccecigdaily==1
replace daily=3 if ccsmkdaily==1 & ccecigdaily==1

label define smkecig 0 "Neither" 1 "Smoke only" 2 "Vape only" 3 "Smoke and vape"
label values yr week daily smkecig

gen f1ccsmkyr=f1.ccsmkyr
gen f1ccsmkweekly=f1.ccsmkweekly
gen f1ccsmkdaily=f1.ccsmkdaily
gen f1ccecigyr=f1.ccecigyr
gen f1ccecigweekly=f1.ccecigweekly
gen f1ccecigdaily=f1.ccecigdaily

gen f1yr=f1.yr
gen f1week=f1.week
gen f1daily=f1.daily

corr f1ccsmkyr f1ccsmkweekly f1ccecigyr f1ccecigweekly

drop newid

mi import flong, m(imp) id(zzC_ID zzwave) imp(ccalcyr peerdisecig ccsmkweekly peerecig ccecigyr ccecigweekly ccsmkyr peerdissmk peersmk c_age singlep b_hinc b_parcon b_famposi b_famconf b_parrel b_parborn b_parempl b_seifa b_pardem b_parres b_parmon b_cbclextn b_cbcladn b_cbclspn b_cbclwdn b_sex f1ccsmkyr f1ccsmkweekly f1ccecigyr f1ccecigweekly) clear

mi xtset zzC_ID zzwave

save "C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP43. Role of peers on smoking and ecigarette use\Data\final analysis data.dta", replace
