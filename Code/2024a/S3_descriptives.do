
use "D:\UNSW\APSALS - Documents\Papers\PIP43. Role of peers on smoking and ecigarette use\Data\final analysis data.dta", clear

matrix demog=J(8,2,.)

gen age=c_age+11
su age if _mi_m==0 & zzwave==1
	matrix demog[1,1]=r(mean)
	matrix demog[1,2]=r(sd)
	
tab b_sex if _mi_m==0 & zzwave==1, matcell(temp)
	matrix demog[2,1]=temp[2,1]
	matrix demog[2,2]=temp[2,1]/(temp[1,1]+temp[2,1])*100
	
tab ccalcyr if _mi_m==0 & zzwave==1, matcell(temp)
	matrix demog[3,1]=temp[2,1]
	matrix demog[3,2]=temp[2,1]/(temp[1,1]+temp[2,1])*100
		
su b_cbclextn if _mi_m==0 & zzwave==1
	matrix demog[4,1]=r(mean)
	matrix demog[4,2]=r(sd)
su b_cbcladn if _mi_m==0 & zzwave==1
	matrix demog[5,1]=r(mean)
	matrix demog[5,2]=r(sd)
su b_cbclspn if _mi_m==0 & zzwave==1
	matrix demog[6,1]=r(mean)
	matrix demog[6,2]=r(sd)
su b_cbclwdn if _mi_m==0 & zzwave==1
	matrix demog[7,1]=r(mean)
	matrix demog[7,2]=r(sd)
	
tab singlep if _mi_m==0 & zzwave==1, matcell(temp)
	matrix demog[8,1]=temp[2,1]
	matrix demog[8,2]=temp[2,1]/(temp[1,1]+temp[2,1])*100
			
matrix list demog