
use "C:\Users\pcla5984\UNSW\APSALS - Documents\Papers\PIP43. Role of peers on smoking and ecigarette use\Data\final analysis data.dta", clear

// Raw trends in outcome variables
mi estimate,  level(99.5): prop ccsmkyr, over(zzwave)
mi estimate,  level(99.5): prop ccsmkweekly, over(zzwave)
mi estimate,  level(99.5): prop ccsmkdaily, over(zzwave)
mi estimate,  level(99.5): prop ccecigyr, over(zzwave)
mi estimate,  level(99.5): prop ccecigweekly, over(zzwave)
mi estimate,  level(99.5): prop ccecigdaily, over(zzwave)

// Raw trends in combined variables
mi estimate,  level(99.5): prop yr, over(zzwave)
mi estimate,  level(99.5): prop week, over(zzwave)
mi estimate,  level(99.5)  i(1/14 16/19 21/28): prop daily, over(zzwave)

// Raw trends in predictor variables
mi estimate,  level(99.5): prop peersmk if zzwave>=3, over(zzwave)
mi estimate,  level(99.5): prop peerdissmk if zzwave>=3, over(zzwave)
mi estimate,  level(99.5): prop peerecig if zzwave>=3, over(zzwave)
mi estimate,  level(99.5): prop peerdisecig if zzwave>=3, over(zzwave)

// Raw trends in predictor variables
mi estimate,  level(99.5) i(1/4 6/30): prop peersmk if zzwave>=3, over(ccsmkweekly zzwave)
mi estimate,  level(99.5): prop peerdissmk if zzwave>=3, over(ccsmkweekly zzwave)
mi estimate,  level(99.5): prop peerecig if zzwave>=3, over(ccsmkweekly zzwave)
mi estimate,  level(99.5): prop peerdisecig if zzwave>=3, over(ccsmkweekly zzwave)
