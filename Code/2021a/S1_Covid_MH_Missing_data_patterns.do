******************************************************************************
* PROGRAM: S5 Covid Analysis - Import mh data into Stata.do
* PURPOSE: Import imputed data into Stata for analysis
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. MICE
*-----------------------------------------------------------------------------
* 1.0 Load MICE imputed data exported from R

cd "C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP40. COVID-19 Health Paper\Data\"
use "C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP40. COVID-19 Health Paper\Data\Mental health imputed data 20200922.dta", clear

misstable summ ccphqscore_12 ccgadscore_12 ccfinstress ccmenthlth_11 ccmenthlthchng ccphyshlth_11 ccphyshlthchng cchlthska_12 ///
cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih ///
chlivealn_12 chlivefam_12 chliveptn_12 chlivehmt_12 chlivesm ccstndt_12 ccempl_12 cccovidtd cccovidwor cccovidslfiso cccovidqnt ///
cchlthska_11 ccstndt_11 ccempl_11 ///
ccphqscore_10 ccgadscore_10 ///
chlivealn_10 chlivefam_10 chliveptn_10 chlivehmt_10 ccstndt_10 ccempl_10 ///
sex_10 pinc_10 age_10 peeruse_10 peerdis_10 b_seifa b_famhist b_oldsib diagmh_10 ccmhtrta_10 ccmhtrtb_10 ccmhtrtc_10 if imp==51, all

misstable patt ccphqscore_12 ccgadscore_12 ccfinstress ccmenthlth_11 ccmenthlthchng ccphyshlth_11 ccphyshlthchng cchlthska_12 ///
cccovidposia cccovidposib cccovidposic cccovidposid cccovidposie cccovidposif cccovidposig cccovidposih ///
chlivealn_12 chlivefam_12 chliveptn_12 chlivehmt_12 chlivesm ccstndt_12 ccempl_12 cccovidtd cccovidwor cccovidslfiso cccovidqnt ///
cchlthska_11 ccstndt_11 ccempl_11 ///
ccphqscore_10 ccgadscore_10 ///
chlivealn_10 chlivefam_10 chliveptn_10 chlivehmt_10 ccstndt_10 ccempl_10 ///
sex_10 pinc_10 age_10 peeruse_10 peerdis_10 b_seifa b_famhist b_oldsib diagmh_10 ccmhtrta_10 ccmhtrtb_10 ccmhtrtc_10 if imp==51, freq asis
