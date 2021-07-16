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
mi estimate, cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)

* 1.2 Figure E7 - Time by gender interaction
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)

* 1.3 Figure E8 - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:, intp(30)

******************************************************************************
* 2. Sensitivity analysis of continuous time trends excluding retrospective data
*-----------------------------------------------------------------------------

* 1.1 Figure E9
mi estimate, cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)

* 2.2 Figure E10 - Time by gender interaction
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate (male0: _b[234.qtryr]) (male1: _b[235.qtryr]) (male2: _b[236.qtryr]) (male3: _b[237.qtryr]) ///
(male4: _b[238.qtryr]) (male5: _b[239.qtryr]) (male7: _b[241.qtryr]) ///
(female0: _b[2.sex_9] + _b[234.qtryr] + _b[234.qtryr#2.sex_9]) (female1: _b[2.sex_9] + _b[235.qtryr] + _b[235.qtryr#2.sex_9]) ///
(female2: _b[2.sex_9] + _b[236.qtryr] + _b[236.qtryr#2.sex_9]) (female3: _b[2.sex_9] + _b[237.qtryr] + _b[237.qtryr#2.sex_9]) ///
(female4: _b[2.sex_9] + _b[238.qtryr] + _b[238.qtryr#2.sex_9]) (female5: _b[2.sex_9] + _b[239.qtryr] + _b[239.qtryr#2.sex_9]) ///
(female6: _b[2.sex_9]) (female7: _b[2.sex_9] + _b[241.qtryr] + _b[241.qtryr#2.sex_9]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)

* 2.3 Figure E11 - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcfre ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcqnt ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg ccalcbngf ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)
mi estimate (low0: _b[234.qtryr]) (low1: _b[235.qtryr]) (low2: _b[236.qtryr]) (low3: _b[237.qtryr]) ///
(low4: _b[238.qtryr]) (low5: _b[239.qtryr]) (low7: _b[241.qtryr]) ///
(high0: _b[2.alcgrp] + _b[234.qtryr] + _b[234.qtryr#2.alcgrp]) (high1: _b[2.alcgrp] + _b[235.qtryr] + _b[235.qtryr#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[236.qtryr] + _b[236.qtryr#2.alcgrp]) (high3: _b[2.alcgrp] + _b[237.qtryr] + _b[237.qtryr#2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[238.qtryr] + _b[238.qtryr#2.alcgrp]) (high5: _b[2.alcgrp] + _b[239.qtryr] + _b[239.qtryr#2.alcgrp]) ///
(high6: _b[2.alcgrp]) (high7: _b[2.alcgrp] + _b[241.qtryr] + _b[241.qtryr#2.alcgrp]), nocoef cmdok dots esampvaryok: menbreg alcfqqnt ib240.qtryr##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave!=3 || zzC_ID:, intp(30)


******************************************************************************
* 3. Secondary alcohol outcomes - Sensitivity with high risk split
*-----------------------------------------------------------------------------

* 3.1 Figure E3 and Table E8 - Trends of frequency, quantity and binge drinking - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk]) (high3: _b[1.highrisk] + _b[4.zzwave] + _b[4.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg ccalcfre ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk]) (high3: _b[1.highrisk] + _b[4.zzwave] + _b[4.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg ccalcqnt ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk]) (high3: _b[1.highrisk] + _b[4.zzwave] + _b[4.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg ccalcbngf ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[1.highrisk] + _b[1.zzwave] + _b[1.zzwave#1.highrisk]) (high1: _b[1.highrisk] + _b[2.zzwave] + _b[2.zzwave#1.highrisk]) ///
(high2: _b[1.highrisk]) (high3: _b[1.highrisk] + _b[4.zzwave] + _b[4.zzwave#1.highrisk]), cmdok dots esampvaryok: menbreg alcfqqnt ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)

* 3.2 Figure E4 and Tables E9 & E10 - Maximum consumption, harms, delivery, and drinking context - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (c1: _b[4.zzwave]) (c2: _b[1.highrisk]) (c3: _b[4.zzwave] + _b[1.highrisk] + _b[4.zzwave#1.highrisk]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#1.highrisk]), esampvaryok dots: ///
mixed ccalcmax ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[1.highrisk]) (c3: _b[4.zzwave] + _b[1.highrisk] + _b[4.zzwave#1.highrisk]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#1.highrisk]), esampvaryok cmdok dots: ///
menbreg ccnumharm ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate (c1: _b[4.zzwave]) (c2: _b[1.highrisk]) (c3: _b[4.zzwave] + _b[1.highrisk] + _b[4.zzwave#1.highrisk]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#1.highrisk]), esampvaryok dots: ///
mixed ccalcalone ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[1.highrisk]) (c3: _b[4.zzwave] + _b[1.highrisk] + _b[4.zzwave#1.highrisk]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#1.highrisk]), esampvaryok dots: ///
mixed ccalcothers ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[1.highrisk]) (c3: _b[4.zzwave] + _b[1.highrisk] + _b[4.zzwave#1.highrisk]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#1.highrisk]), esampvaryok dots: ///
mixed ccalcvirtual ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[1.highrisk]) (c3: _b[4.zzwave] + _b[1.highrisk] + _b[4.zzwave#1.highrisk]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#1.highrisk]), esampvaryok cmdok dots: ///
meologit ccalcdlvr ib3.zzwave##i.highrisk i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)
