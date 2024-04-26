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

// 1.1. Specify working directory
global workdir "/Users/pjclare/Library/CloudStorage/OneDrive-SharedLibraries-UNSW/APSALS - Documents/Papers/PIP46. Effect of age of initiation on trajectory of alcohol use and harm"

// 1.2. Start log
log using "$workdir/Logs/Primary analysis 20231116.smcl", replace

// 1.3. Define program to estimate margins
capture program drop figureres
program figureres, rclass
version 17

local end_mata end

qui su imp
local nimp=r(max)

matrix pr=J(`nimp',110,.)
matrix se=J(`nimp',110,.)

forvalues progi=1/`nimp' {
	disp "Imputation `progi'"
	
	preserve
	qui keep if imp==`progi'
	
	if "`1'"=="alcfq" {
		qui menbreg `1' ///
		c.age_init_a ///
		c.age_init_a#c.age_init_a#c.age_init_a ///
		c.wave##c.wave##c.wave ///
		c.age_init_a#c.wave##c.wave##c.wave ///
		c.age_init_a#c.age_init_a#c.age_init_a#c.wave##c.wave##c.wave ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
	
		qui margins, at(age_init=(0 / 9) wave=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom ln(_b[`progj'._at])
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
	
	}
	else if "`1'"=="monthlyhed" | "`1'"=="anyhed" | "`1'"=="anyharms" {
		qui melogit `1' ///
		c.age_init_a ///
		c.age_init_a#c.age_init_a#c.age_init_a ///
		c.wave##c.wave##c.wave ///
		c.age_init_a#c.wave##c.wave##c.wave ///
		c.age_init_a#c.age_init_a#c.age_init_a#c.wave##c.wave##c.wave ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
	
		qui margins, at(age_init=(0 / 9) wave=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom ln(_b[`progj'._at])
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
	
	}
	else if "`1'"=="numharms" {
		qui menbreg `1' ///
		c.age_init_a##c.age_init_a##c.age_init_a##c.wave##c.wave ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
	
		qui margins, at(age_init=(0 / 9) wave=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom ln(_b[`progj'._at])
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
	}
	else if "`1'"=="dependdiag" | "`1'"=="auddiag" {
		qui melogit `1' ///
		c.age_init_a##c.age_init_a##c.wave##c.wave ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:
			
		qui margins, at(age_init=(0 / 9) wave=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom logit(_b[`progj'._at])
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
	}
	else if "`1'"=="abusediag" {
		qui melogit `1' ///
		c.age_init_a ///
		c.age_init_a#c.age_init_a#c.age_init_a ///
		c.wave ///
		c.wave#c.wave ///
		c.age_init_a#c.wave /// 
		c.age_init_a#c.age_init_a#c.age_init_a#c.wave ///
		c.age_init_a#c.wave#c.wave ///
		c.age_init_a#c.age_init_a#c.age_init_a#c.wave#c.wave ///
		b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
		b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn if imp==`progi' || zzC_ID:	
			
		qui margins, at(age_init=(0 / 9) wave=(0 / 10)) post

		forvalues progj=1/110 {
			qui nlcom logit(_b[`progj'._at])
			matrix temp=r(table)
			matrix pr[`progi',`progj']=temp[1,1]
			matrix se[`progi',`progj']=temp[2,1]
		}
	}
			
	restore
		
}

return matrix est = pr
return matrix se = se

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

//**************************************************************************//
// 3. Run models and extract marginal probabilities/means
//--------------------------------------------------------------------------//

foreach i in alcfq monthlyhed numharms dependdiag abusediag auddiag anyhed anyharms {
	figureres `i'
	matrix fin_est=r(est)
	matrix fin_se=r(se)
	putexcel set "$workdir/Results/Raw/`i'_est 20231116.xlsx", replace
	putexcel A1=matrix(fin_est)
	putexcel set "$workdir/Results/Raw/`i'_se 20231116.xlsx", replace
	putexcel A1=matrix(fin_se)
}

log close
