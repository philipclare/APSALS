//**************************************************************************//
//   
// APSALS Wave 11 Initiation Trajectories Paper
// Assess model fit for non-linear terms
// Author: Philip J Clare
// Date: 19 June 2023
// Licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
// Analysis pre-registered at DOI:10.17605/OSF.IO/BRDUV
//
//**************************************************************************//
// 1. Setup Environment
//--------------------------------------------------------------------------//

// 1.1. Specify working directory

global workdir "D:/UNSW/APSALS - Documents/Papers/PIP46. Age of initiation and the trajectory of alcohol use and harm"

//**************************************************************************//
// 2. Load data
//--------------------------------------------------------------------------//

//use "$workdir/Data/initiation_analysis_data.dta", clear
use "$workdir/Data/Katana Imputations/Initiation imputed data 1.dta", clear

gen anyharms=0 if numharms==0
replace anyharms=1 if numharms>0 & numharms!=.

//**************************************************************************//
// 3. Fit models with various polynomials and ouput fit statistics
//--------------------------------------------------------------------------//

capture program drop fit

program fit

if "`1'"=="alcfq" | "`1'"=="numharms" {
	local 3="menbreg"
}
else if "`1'"=="monthlyhed" | "`1'"=="anyhed"  | "`1'"=="dependdiag" | "`1'"=="abusediag" | "`1'"=="auddiag" | "`1'"=="anyharms" {
	local 3="melogit"
}
if "`1'"=="dependdiag" | "`1'"=="abusediag" | "`1'"=="auddiag" {
	local 4="if zzwave>=5"
}

matrix `1'fit=J(16,2,.)

// 1. Age linear x time linear
qui `3' `1' ///
c.`2'##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[1,1]=temp[1,5..6]

// 2. Age squared x time linear
qui `3' `1' ///
c.`2'##c.`2'##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[2,1]=temp[1,5..6]

// 3. Age cubed x time linear
qui `3' `1' ///
c.`2' c.`2'#c.`2'#c.`2' c.wave c.`2'#c.wave c.`2'#c.`2'#c.`2'#c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[3,1]=temp[1,5..6]

// 4. Age squared/cubed x time linear
qui `3' `1' ///
c.`2'##c.`2'##c.`2'##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[4,1]=temp[1,5..6]

// 5. Age linear x time squared
qui `3' `1' ///
c.`2'##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[5,1]=temp[1,5..6]

// 6. Age squared x time squared
qui `3' `1' ///
c.`2'##c.`2'##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[6,1]=temp[1,5..6]

// 7. Age cubed x time squared
qui `3' `1' /// 
c.`2' /// 
c.`2'#c.`2'#c.`2' /// 
c.wave##c.wave ///
c.`2'#c.wave##c.wave ///
c.`2'#c.`2'#c.`2'#c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[7,1]=temp[1,5..6]

// 8. Age squared/cubed x time squared
qui `3' `1' ///
c.`2'##c.`2'##c.`2'##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[8,1]=temp[1,5..6]

// 9. Age linear x time cubed
qui `3' `1' /// 
c.`2' ///
c.wave ///
c.wave#c.wave#c.wave ///
c.`2'#c.wave /// 
c.`2'#c.wave#c.wave#c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[9,1]=temp[1,5..6]

// 10. Age squared x time cubed
qui `3' `1' /// 
c.`2'##c.`2' ///
c.wave ///
c.wave#c.wave#c.wave ///
c.`2'##c.`2'#c.wave /// 
c.`2'##c.`2'#c.wave#c.wave#c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[10,1]=temp[1,5..6]

// 11. Age cubed x time cubed
qui `3' `1' /// 
c.`2' ///
c.`2'#c.`2'#c.`2' ///
c.wave ///
c.wave#c.wave#c.wave /// 
c.`2'#c.wave ///
c.`2'#c.`2'#c.`2'#c.wave /// 
c.`2'#c.wave#c.wave#c.wave /// 
c.`2'#c.`2'#c.`2'#c.wave#c.wave#c.wave  ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[11,1]=temp[1,5..6]

// 12. Age squared/cubed x time cubed
qui `3' `1' ///
c.`2'##c.`2'##c.`2' ///
c.wave ///
c.wave#c.wave#c.wave  ///
c.`2'##c.`2'##c.`2'#c.wave ///
c.`2'##c.`2'##c.`2'#c.wave#c.wave#c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[12,1]=temp[1,5..6]

// 13. Age linear x time squared/cubed
qui `3' `1' /// 
c.`2'##c.wave##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[13,1]=temp[1,5..6]

// 14. Age squared x time squared/cubed
qui `3' `1' /// 
c.`2'##c.`2'##c.wave##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[14,1]=temp[1,5..6]

// 15. Age cubed x time squared/cubed
qui `3' `1' /// 
c.`2' ///
c.`2'#c.`2'#c.`2' ///
c.wave##c.wave##c.wave ///
c.`2'#c.wave##c.wave##c.wave ///
c.`2'#c.`2'#c.`2'#c.wave##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[15,1]=temp[1,5..6]

// 16. Age squared/cubed x time squared/cubed
qui `3' `1' ///
c.`2'##c.`2'##c.`2'##c.wave##c.wave##c.wave ///
b_hinc b_singlep b_hhavguse b_alcrules b_parmon b_parcon b_peeruse b_peerdis b_homeacc b_famconf b_famposi b_alcmoney ///
b_sex b_parborn b_pareduc b_parempl b_pardem b_parres b_seifa b_parrel b_cbclextn b_cbcladn b_cbclwdn `4' || zzC_ID:
qui estat ic
matrix temp=r(S)
matrix `1'fit[16,1]=temp[1,5..6]

end

fit alcfq age_init_a
fit monthlyhed age_init_a
fit anyhed age_init_a
fit numharms age_init_a
fit anyharms age_init_a
fit dependdiag age_init_a
fit abusediag age_init_a
fit auddiag age_init_a

matrix fit_primary=alcfqfit,monthlyhedfit,anyhedfit,numharmsfit,anyharmsfit,dependdiagfit,abusediagfit,auddiagfit

fit alcfq age_init_b
fit monthlyhed age_init_b
fit anyhed age_init_b
fit numharms age_init_b
fit anyharms age_init_b
fit dependdiag age_init_b
fit abusediag age_init_b
fit auddiag age_init_b

matrix fit_secondary=alcfqfit,monthlyhedfit,anyhedfit,numharmsfit,anyharmsfit,dependdiagfit,abusediagfit,auddiagfit

matrix list fit_primary
matrix list fit_secondary
