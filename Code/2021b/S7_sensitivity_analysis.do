******************************************************************************
* PROGRAM: S7 Covid Analysis - Sensitivity analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

use "C:\Users\pjclare\Desktop\APSALS\COVID Survey\Data\Alcohol imputed data - for analysis.dta", clear

******************************************************************************
* 1. Sensitivity analysis of continuous time trends
*-----------------------------------------------------------------------------

* 1.1 Figure E6
mi estimate, cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)

* 1.2 Figure E7 - Time by gender interaction
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)

* 1.3 Figure E8 - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:, intp(30)

******************************************************************************
* 2. Sensitivity analysis of continuous time trends excluding retrospective data
*-----------------------------------------------------------------------------

* 1.1 Figure E9
mi estimate, cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)

* 2.2 Figure E10 - Time by gender interaction
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate (male0: _b[230.qtryr]) (male1: _b[231.qtryr]) (male2: _b[232.qtryr]) (male3: _b[233.qtryr]) ///
(male4: _b[234.qtryr]) (male5: _b[235.qtryr]) (male6: _b[236.qtryr]) (male7: _b[237.qtryr]) ///
(male8: _b[238.qtryr]) (male9: _b[239.qtryr]) (male11: _b[241.qtryr]) ///
(female0: _b[2.sex_8] + _b[230.qtryr] + _b[230.qtryr#2.sex_8]) (female1: _b[2.sex_8] + _b[231.qtryr] + _b[231.qtryr#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[232.qtryr] + _b[232.qtryr#2.sex_8]) (female3: _b[2.sex_8] + _b[233.qtryr] + _b[233.qtryr#2.sex_8]) ///
(female4: _b[2.sex_8] + _b[234.qtryr] + _b[234.qtryr#2.sex_8]) (female5: _b[2.sex_8] + _b[235.qtryr] + _b[235.qtryr#2.sex_8]) ///
(female6: _b[2.sex_8] + _b[236.qtryr] + _b[236.qtryr#2.sex_8]) (female7: _b[2.sex_8] + _b[237.qtryr] + _b[237.qtryr#2.sex_8]) ///
(female8: _b[2.sex_8] + _b[238.qtryr] + _b[238.qtryr#2.sex_8]) (female9: _b[2.sex_8] + _b[239.qtryr] + _b[239.qtryr#2.sex_8]) ///
(female10: _b[2.sex_8]) (female11: _b[2.sex_8] + _b[241.qtryr] + _b[241.qtryr#2.sex_8]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)

* 2.3 Figure E11 - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)
mi estimate (low0: _b[230.qtryr]) (low1: _b[231.qtryr]) (low2: _b[232.qtryr]) (low3: _b[233.qtryr]) ///
(low4: _b[234.qtryr]) (low5: _b[235.qtryr]) (low6: _b[236.qtryr]) (low7: _b[237.qtryr]) ///
(low8: _b[238.qtryr]) (low9: _b[239.qtryr]) (low11: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[230.qtryr] + _b[230.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[231.qtryr] + _b[231.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[232.qtryr] + _b[232.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[233.qtryr] + _b[233.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high7: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high8: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high9: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high10: _b[2.alcgrp]) (high11: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave!=4 || zzC_ID:, intp(30)


******************************************************************************
* 3. Secondary alcohol outcomes - Sensitivity with high risk split
*-----------------------------------------------------------------------------

* 3.1 Figure E5 and Table E11 - Trends of frequency, quantity and binge drinking - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk] + _b[3.zzwave] + _b[3.zzwave#1.highrisk]) (high3: _b[1.highrisk]) ///
(high4: _b[1.highrisk] + _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg ccalcfre ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk] + _b[3.zzwave] + _b[3.zzwave#1.highrisk]) (high3: _b[1.highrisk]) ///
(high4: _b[1.highrisk] + _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg ccalcqnt ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk] + _b[3.zzwave] + _b[3.zzwave#1.highrisk]) (high3: _b[1.highrisk]) ///
(high4: _b[1.highrisk] + _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg ccalcbngf ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk] + _b[3.zzwave] + _b[3.zzwave#1.highrisk]) (high3: _b[1.highrisk]) ///
(high4: _b[1.highrisk] + _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg alcfqqnt ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)

* 3.2 Figure E6 and Tables E12 & E13 - Maximum consumption, harms, delivery, and drinking context - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (c1: _b[5.zzwave]) (c2: _b[1.highrisk]) (c3: _b[5.zzwave] + _b[1.highrisk] + _b[5.zzwave#1.highrisk]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok esampvaryok dots: ///
mixed ccalcmax ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[1.highrisk]) (c3: _b[5.zzwave] + _b[1.highrisk] + _b[5.zzwave#1.highrisk]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok esampvaryok dots: ///
menbreg ccnumharm ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate (c1: _b[5.zzwave]) (c2: _b[1.highrisk]) (c3: _b[5.zzwave] + _b[1.highrisk] + _b[5.zzwave#1.highrisk]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok esampvaryok dots: ///
mixed ccalcalone ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[1.highrisk]) (c3: _b[5.zzwave] + _b[1.highrisk] + _b[5.zzwave#1.highrisk]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok esampvaryok dots: ///
mixed ccalcothers ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[1.highrisk]) (c3: _b[5.zzwave] + _b[1.highrisk] + _b[5.zzwave#1.highrisk]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok esampvaryok dots: ///
mixed ccalcvirtual ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[1.highrisk]) (c3: _b[5.zzwave] + _b[1.highrisk] + _b[5.zzwave#1.highrisk]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#1.highrisk]), cmdok esampvaryok dots: ///
meologit ccalcdlvr ib4.zzwave##i.highrisk i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)
