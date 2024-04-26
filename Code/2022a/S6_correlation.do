
use "C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP43. Role of peers on smoking and ecigarette use\Data\final analysis data.dta", clear

// Correlation between adolescent and peer variables

matrix res=J(30,36,.)

forvalues i=1/30 {
	disp "`i'"
	qui polychoric ccsmkyr ccecigyr peersmk peerdissmk peerecig peerdisecig if zzwave>=3 & _mi_m==`i'
	matrix temp=r(R)
	matrix res[`i',1]=temp[1,1]
	matrix res[`i',7]=temp[2,1..2]
	matrix res[`i',13]=temp[3,1..3]
	matrix res[`i',19]=temp[4,1..4]
	matrix res[`i',25]=temp[5,1..5]
	matrix res[`i',31]=temp[6,1..6]
}
matrix results = J(rowsof(res),1,1)'*res/rowsof(res)
mata: st_matrix("res_matrix", rowshape( st_matrix("results")', 6) )