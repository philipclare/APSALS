******************************************************************************
* PROGRAM: S6 Covid Analysis - Primary analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

use "/Users/pjclare/UNSW/APSALS - Documents/Papers/PIP39. COVID-19 Alcohol Paper/Data/Alcohol imputed data - for analysis.dta", clear

******************************************************************************
* 1. Primary analysis of alcohol trends
*-----------------------------------------------------------------------------

* 1.1 Figure 1 and Table E2 - Trends of frequency, quantity and binge drinking
mi estimate, cmdok dots esampvaryok: menbreg ccalcfre ib4.zzwave i.sex_8 i.highrisk i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcqnt ib4.zzwave i.sex_8 i.highrisk i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcbngf ib4.zzwave i.sex_8 i.highrisk i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg alcfqqnt ib4.zzwave i.sex_8 i.highrisk i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)

* 1.2 Table 2 and Table E3 - Maximum consumption, harms, delivery, and drinking context
mi estimate, esampvaryok dots: mixed ccalcmax ib4.zzwave i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate, esampvaryok cmdok dots: menbreg ccnumharm ib4.zzwave i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate, esampvaryok dots: mixed ccalcalone ib4.zzwave i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate, esampvaryok dots: mixed ccalcothers ib4.zzwave i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate, esampvaryok dots: mixed ccalcvirtual ib4.zzwave i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate, esampvaryok cmdok dots: meologit ccalcdlvr ib4.zzwave i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)

******************************************************************************
* 2. Analysis of gender differences
*-----------------------------------------------------------------------------

* 2.1 Figure E1 and Table E4 - Trends of frequency, quantity and binge drinking - Time by gender interaction
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[3.zzwave]) (male4: _b[5.zzwave]) ///
(female0: _b[2.sex_8] + _b[1.zzwave] + _b[1.zzwave#2.sex_8]) (female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) (female3: _b[2.sex_8]) ///
(female4: _b[2.sex_8] + _b[5.zzwave] + _b[5.zzwave#2.sex_8]), cmdok dots esampvaryok: menbreg ccalcfre ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[3.zzwave]) (male4: _b[5.zzwave]) ///
(female0: _b[2.sex_8] + _b[1.zzwave] + _b[1.zzwave#2.sex_8]) (female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) (female3: _b[2.sex_8]) ///
(female4: _b[2.sex_8] + _b[5.zzwave] + _b[5.zzwave#2.sex_8]), cmdok dots esampvaryok: menbreg ccalcqnt ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[3.zzwave]) (male4: _b[5.zzwave]) ///
(female0: _b[2.sex_8] + _b[1.zzwave] + _b[1.zzwave#2.sex_8]) (female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) (female3: _b[2.sex_8]) ///
(female4: _b[2.sex_8] + _b[5.zzwave] + _b[5.zzwave#2.sex_8]), cmdok dots esampvaryok: menbreg ccalcbngf ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[3.zzwave]) (male4: _b[5.zzwave]) ///
(female0: _b[2.sex_8] + _b[1.zzwave] + _b[1.zzwave#2.sex_8]) (female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) (female3: _b[2.sex_8]) ///
(female4: _b[2.sex_8] + _b[5.zzwave] + _b[5.zzwave#2.sex_8]), cmdok dots esampvaryok: menbreg alcfqqnt ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)

* 2.2 Figure E2 and Tables E5 & E6 - Maximum consumption, harms, delivery, and drinking context - Time by gender interaction
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.sex_8]) (c3: _b[5.zzwave] + _b[2.sex_8] + _b[5.zzwave#2.sex_8]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.sex_8]), esampvaryok dots: ///
mixed ccalcmax ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.sex_8]) (c3: _b[5.zzwave] + _b[2.sex_8] + _b[5.zzwave#2.sex_8]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.sex_8]), esampvaryok cmdok dots: ///
menbreg ccnumharm ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.sex_8]) (c3: _b[5.zzwave] + _b[2.sex_8] + _b[5.zzwave#2.sex_8]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.sex_8]), esampvaryok dots: ///
mixed ccalcalone ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.sex_8]) (c3: _b[5.zzwave] + _b[2.sex_8] + _b[5.zzwave#2.sex_8]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.sex_8]), esampvaryok dots: ///
mixed ccalcothers ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.sex_8]) (c3: _b[5.zzwave] + _b[2.sex_8] + _b[5.zzwave#2.sex_8]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.sex_8]), esampvaryok dots: ///
mixed ccalcvirtual ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.sex_8]) (c3: _b[5.zzwave] + _b[2.sex_8] + _b[5.zzwave#2.sex_8]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.sex_8]), esampvaryok cmdok dots: ///
meologit ccalcdlvr ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)

******************************************************************************
* 3. Analysis of difference by W8 drinking
*-----------------------------------------------------------------------------

* 3.1 Figure E3 and Table E7 - Trends of frequency, quantity and binge drinking - Time interaction split by top/bottom half of alcohol freq*quant at wave 8
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[3.zzwave] + _b[3.zzwave#2.alcgrp]) (high3: _b[2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[5.zzwave] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg ccalcfre ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[3.zzwave] + _b[3.zzwave#2.alcgrp]) (high3: _b[2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[5.zzwave] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg ccalcqnt ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[3.zzwave] + _b[3.zzwave#2.alcgrp]) (high3: _b[2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[5.zzwave] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg ccalcbngf ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[3.zzwave] + _b[3.zzwave#2.alcgrp]) (high3: _b[2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[5.zzwave] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg alcfqqnt ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)

* 3.2 Figure E4 and Tables E8 & E9 - Maximum consumption, harms, delivery, and drinking context - Time interaction split by top/bottom half of alcohol freq*quant at wave 8
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[5.zzwave] + _b[2.alcgrp] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcmax ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[5.zzwave] + _b[2.alcgrp] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), esampvaryok cmdok dots: ///
menbreg ccnumharm ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[5.zzwave] + _b[2.alcgrp] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcalone ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[5.zzwave] + _b[2.alcgrp] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcothers ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[5.zzwave] + _b[2.alcgrp] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcvirtual ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 || zzC_ID:
mi estimate (c1: _b[5.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[5.zzwave] + _b[2.alcgrp] + _b[5.zzwave#2.alcgrp]) ///
(diff: _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), esampvaryok cmdok dots: ///
meologit ccalcdlvr ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sex_8!=3 & zzwave>2 || zzC_ID:, intp(30)

******************************************************************************
* 4. Drivers of change analysis
*-----------------------------------------------------------------------------

* 4.1 Figure E7 and Tables E14 & E15 - Money for alcohol analysis
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[3.zzwave]) (male4: _b[5.zzwave]) ///
(female0: _b[2.sex_8] + _b[1.zzwave] + _b[1.zzwave#2.sex_8]) (female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) ///
(female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) (female3: _b[2.sex_8]) ///
(female4: _b[2.sex_8] + _b[5.zzwave] + _b[5.zzwave#2.sex_8]), cmdok  esampvaryok noisily: melogit ccalcmon ib4.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[3.zzwave]) (low4: _b[5.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp] + _b[3.zzwave] + _b[3.zzwave#2.alcgrp]) (high3: _b[2.alcgrp]) ///
(high4: _b[2.alcgrp] + _b[5.zzwave] + _b[5.zzwave#2.alcgrp]), cmdok dots esampvaryok: melogit ccalcmon ib4.zzwave##i.alcgrp i.sex_8 i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)


/*

* 3.1 Figure S5 and Table S9 - Smoking and cannabis
mi estimate (male1: _b[2.zzwave]) (male2: _b[3.zzwave]) (male3: _b[4.zzwave]) (female0: _b[2.sex_8]) ///
(female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) (female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) ///
(female3: _b[2.sex_8] + _b[4.zzwave] + _b[4.zzwave#2.sex_8]), cmdok dots esampvaryok: melogit ccsmkfrq ib3.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[2.zzwave]) (male2: _b[3.zzwave]) (male3: _b[4.zzwave]) (female0: _b[2.sex_8]) ///
(female1: _b[2.sex_8] + _b[2.zzwave] + _b[2.zzwave#2.sex_8]) (female2: _b[2.sex_8] + _b[3.zzwave] + _b[3.zzwave#2.sex_8]) ///
(female3: _b[2.sex_8] + _b[4.zzwave] + _b[4.zzwave#2.sex_8]), cmdok dots esampvaryok: melogit cccanfrq ib3.zzwave##i.sex_8 i.alcgrp i.b_seifa b_famhist b_oldsib age_8 peeruse_8 peerdis_8 if sample==1 || zzC_ID:, intp(30)

