//**************************************************************************//
//   
// APSALS Wave 11 Initiation Trajectories Paper
// Run secondary analyses and save output
// Author: Philip J Clare
// Date: 19 June 2023
// Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
// Analysis pre-registered at DOI:10.17605/OSF.IO/BRDUV
//
//**************************************************************************//
// 1. Setup Environment
//--------------------------------------------------------------------------//

// 1.1. Specify working directory
global workdir "C:/Users/pcla5984/UNSW/APSALS - Documents/Papers/PIP46. Age of initiation and the trajectory of alcohol use and harm"

// 1.2. Start log
log using "$workdir/Logs/Secondary analysis 20231116.smcl", replace

// 1.3. Define program to estimate margins
capture program drop figureres
program figureres, rclass
version 17

local end_mata end

qui su imp
local nimp=r(max)

matrix pr=J(`nimp',110,.)
matrix se=J(`nimp',110,.)

matrix rr18=J(`nimp',8,.)
matrix rr18_se=J(`nimp',8,.)

matrix rr20=J(`nimp',10,.)
matrix rr20_se=J(`nimp',10,.)

forvalues progi=1/`nimp' {
	disp "Imputation `progi'"
	
	preserve
	qui keep if imp==`progi'
	
	if "`1'"=="alcfq" | "`1'"=="numharms" {
		qui menbreg `1' ///
		c.age_init_b##c.age_init_b##c.age_init_b##c.wave_b##c.wave_b##c.wave_b ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
	
		qui margins, at(age_init=(0 / 9) wave_b=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom _b[`progj'._at]
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
		
		forvalues progj=8(10)78 {
			local j=(`progj'+2)/10
			qui nlcom ln(_b[`progj'._at]/_b[78._at])
			matrix temp=r(table)
			matrix rr18[`progi',`j']=temp[1,1]
			matrix rr18_se[`progi',`j']=temp[2,1]
		}

		forvalues progj=10(10)100 {
			local j=(`progj')/10
			qui nlcom ln(_b[`progj'._at]/_b[80._at])
			matrix temp=r(table)
			matrix rr20[`progi',`j']=temp[1,1]
			matrix rr20_se[`progi',`j']=temp[2,1]
		}
	
	}
	else if "`1'"=="monthlyhed" | "`1'"=="dependdiag" {
		qui melogit `1' ///
		c.age_init_b ///
		c.wave_b ///
		c.wave_b#c.wave_b#c.wave_b ///
		c.age_init_b#c.wave_b /// 
		c.age_init_b#c.wave_b#c.wave_b#c.wave_b ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
	
		qui margins, at(age_init=(0 / 9) wave_b=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom _b[`progj'._at]
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
		
		forvalues progj=8(10)78 {
			local j=(`progj'+2)/10
			qui nlcom ln(_b[`progj'._at]/_b[78._at])
			matrix temp=r(table)
			matrix rr18[`progi',`j']=temp[1,1]
			matrix rr18_se[`progi',`j']=temp[2,1]
		}

		forvalues progj=10(10)100 {
			local j=(`progj')/10
			qui nlcom ln(_b[`progj'._at]/_b[80._at])
			matrix temp=r(table)
			matrix rr20[`progi',`j']=temp[1,1]
			matrix rr20_se[`progi',`j']=temp[2,1]
		}
		
	}
	else if "`1'"=="anyhed" | "`1'"=="anyharms" {
		qui melogit `1' ///
		c.age_init_b##c.age_init_b##c.age_init_b##c.wave_b##c.wave_b##c.wave_b ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
	
		qui margins, at(age_init=(0 / 9) wave_b=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom _b[`progj'._at]
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
		
		forvalues progj=8(10)78 {
			local j=(`progj'+2)/10
			qui nlcom ln(_b[`progj'._at]/_b[78._at])
			matrix temp=r(table)
			matrix rr18[`progi',`j']=temp[1,1]
			matrix rr18_se[`progi',`j']=temp[2,1]
		}

		forvalues progj=10(10)100 {
			local j=(`progj')/10
			qui nlcom ln(_b[`progj'._at]/_b[80._at])
			matrix temp=r(table)
			matrix rr20[`progi',`j']=temp[1,1]
			matrix rr20_se[`progi',`j']=temp[2,1]
		}
		
	}
	else if "`1'"=="abusediag" {
		qui melogit `1' ///
		c.age_init_b##c.wave_b##c.wave_b##c.wave_b ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
			
		qui margins, at(age_init=(0 / 9) wave_b=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom _b[`progj'._at]
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
		
		forvalues progj=8(10)78 {
			local j=(`progj'+2)/10
			qui nlcom ln(_b[`progj'._at]/_b[78._at])
			matrix temp=r(table)
			matrix rr18[`progi',`j']=temp[1,1]
			matrix rr18_se[`progi',`j']=temp[2,1]
		}

		forvalues progj=10(10)100 {
			local j=(`progj')/10
			qui nlcom ln(_b[`progj'._at]/_b[80._at])
			matrix temp=r(table)
			matrix rr20[`progi',`j']=temp[1,1]
			matrix rr20_se[`progi',`j']=temp[2,1]
		}
		
	}
	else if "`1'"=="auddiag" {
		qui melogit `1' ///
		c.age_init_b ///
		c.age_init_b#c.age_init_b#c.age_init_b ///
		c.wave_b##c.wave_b##c.wave_b ///
		c.age_init_b#c.wave_b##c.wave_b##c.wave_b ///
		c.age_init_b#c.age_init_b#c.age_init_b#c.wave_b##c.wave_b##c.wave_b ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:	
			
		qui margins, at(age_init=(0 / 9) wave_b=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom _b[`progj'._at]
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
		
		forvalues progj=8(10)78 {
			local j=(`progj'+2)/10
			qui nlcom ln(_b[`progj'._at]/_b[78._at])
			matrix temp=r(table)
			matrix rr18[`progi',`j']=temp[1,1]
			matrix rr18_se[`progi',`j']=temp[2,1]
		}

		forvalues progj=10(10)100 {
			local j=(`progj')/10
			qui nlcom ln(_b[`progj'._at]/_b[80._at])
			matrix temp=r(table)
			matrix rr20[`progi',`j']=temp[1,1]
			matrix rr20_se[`progi',`j']=temp[2,1]
		}
		
	}
			
	restore
		
}

return matrix est = pr
return matrix se = se
return matrix est_rr18 = rr18
return matrix se_rr18 = rr18_se
return matrix est_rr20 = rr20
return matrix se_rr20 = rr20_se

end

//**************************************************************************//
// 2. Load data
//--------------------------------------------------------------------------//

use "$workdir/Data/Katana Imputations/Initiation imputed data 1.dta", clear
forvalues i=2/20 {
	append using "$workdir/Data/Katana Imputations/Initiation imputed data `i'.dta"
}

gen anyharms=0 if numharms==0
replace anyharms=1 if numharms>0 & numharms!=.

drop if age_init_b>9
sort imp zzC_ID zzwave
bysort imp zzC_ID: gen wave_b=_n-1

//**************************************************************************//
// 3. Run models and extract marginal probabilities/means
//--------------------------------------------------------------------------//

foreach i in alcfq monthlyhed numharms dependdiag abusediag auddiag anyhed anyharms {
	figureres `i'
	matrix fin_est=r(est)
	matrix fin_se=r(se)
	matrix fin_est_rr18=r(est_rr18)
	matrix fin_se_rr18=r(se_rr18)
	matrix fin_est_rr20=r(est_rr20)
	matrix fin_se_rr20=r(se_rr20)
	putexcel set "$workdir/Results/Raw/Secondary `i'_est 20240501.xlsx", replace
	putexcel A1=matrix(fin_est)
	putexcel set "$workdir/Results/Raw/Secondary `i'_se 20240501.xlsx", replace
	putexcel A1=matrix(fin_se)
	putexcel set "$workdir/Results/Raw/Secondary `i'_est RR18 20240501.xlsx", replace
	putexcel A1=matrix(fin_est_rr18)
	putexcel set "$workdir/Results/Raw/Secondary `i'_se RR18 20240501.xlsx", replace
	putexcel A1=matrix(fin_se_rr18)
	putexcel set "$workdir/Results/Raw/Secondary `i'_est RR20 20240501.xlsx", replace
	putexcel A1=matrix(fin_est_rr20)
	putexcel set "$workdir/Results/Raw/Secondary `i'_se RR20 20240501.xlsx", replace
	putexcel A1=matrix(fin_se_rr20)
}

log close
