
use "C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP43. Role of peers on smoking and ecigarette use\Data\final analysis data.dta", clear

// Association between peer variables and subsequent outcome variables
mi estimate, cmdok or level(99.5) dots: melogit f1ccsmkyr peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age ///
b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc if zzwave>=3 || zzC_ID:

mi estimate, cmdok or level(99.5) dots: melogit f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age ///
b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc if zzwave>=3 || zzC_ID:

mi estimate, cmdok or level(99.5) dots: melogit ccsmkdaily peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age ///
b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc if zzwave>=3 || zzC_ID:

mi estimate, cmdok or level(99.5) dots: melogit f1ccecigyr peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age ///
b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc if zzwave>=3 || zzC_ID:

mi estimate, cmdok or level(99.5) dots: melogit f1ccecigweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age ///
b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc if zzwave>=3 || zzC_ID:

mi estimate, cmdok or level(99.5) dots: melogit ccecigdaily peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age ///
b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc if zzwave>=3 || zzC_ID:

// Association between peer variables and subsequent outcome variables
mi estimate, cmdok eform level(99.5) dots: ///
gsem (1.f1yr <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M1[zzC_ID]) ///
(2.f1yr <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M2[zzC_ID]) ///
(3.f1yr <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M3[zzC_ID]) if zzwave>=3, mlogit

mi estimate, cmdok eform level(99.5) dots: ///
gsem (1.f1week <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M1[zzC_ID]) ///
(2.f1week <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M2[zzC_ID]) ///
(3.f1week <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M3[zzC_ID]) if zzwave>=3, mlogit

mi estimate, cmdok eform level(99.5) i(1/14 16/19 21/30) dots: ///
gsem (1.f1daily <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M1[zzC_ID]) ///
(2.f1daily <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M2[zzC_ID]) ///
(3.f1daily <- f1ccsmkweekly peersmk peerdissmk peerecig peerdisecig i.zzwave ccalcyr c_age b_sex b_cbclextn b_cbcladn b_cbclspn b_cbclwdn ///
singlep b_pardem b_parres b_parcon b_parrel b_parmon b_parborn b_famposi b_famconf i.b_parempl i.b_seifa i.b_hinc M3[zzC_ID]) if zzwave>=3, mlogit
