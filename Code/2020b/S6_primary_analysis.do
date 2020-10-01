******************************************************************************
* PROGRAM: S6 Covid Analysis - Primary analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

use "C:\Users\pjclare\Desktop\APSALS\COVID Survey\Data\Alcohol imputed data - for analysis.dta", clear

******************************************************************************
* 1. Primary analysis of alcohol trends
*-----------------------------------------------------------------------------

* 1.1 Figure 1 and Table E2 - Trends of frequency, quantity and binge drinking
mi estimate, cmdok dots esampvaryok: menbreg ccalcfre ib3.zzwave i.sex_9 i.highrisk i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg ccalcqnt ib3.zzwave i.sex_9 i.highrisk i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: meologit ccalcbngf ib3.zzwave i.sex_9 i.highrisk i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate, cmdok dots esampvaryok: menbreg alcfqqnt ib3.zzwave i.sex_9 i.highrisk i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)

* 1.2 Table 2 and Table E4 - Maximum consumption, harms, delivery, and drinking context
mi estimate, esampvaryok dots: mixed ccalcmax ib3.zzwave i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate, esampvaryok cmdok dots: menbreg ccnumharm ib3.zzwave i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate, esampvaryok dots: mixed ccalcalone ib3.zzwave i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate, esampvaryok dots: mixed ccalcothers ib3.zzwave i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate, esampvaryok dots: mixed ccalcvirtual ib3.zzwave i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate, esampvaryok cmdok dots: meologit ccalcdlvr ib3.zzwave i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)

******************************************************************************
* 2. Analysis of gender differences
*-----------------------------------------------------------------------------

* 2.1 Figure E1 and Table E5 - Trends of frequency, quantity and binge drinking - Time by gender interaction
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[4.zzwave]) ///
(female0: _b[2.sex_9] + _b[1.zzwave] + _b[1.zzwave#2.sex_9]) (female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) ///
(female2: _b[2.sex_9]) (female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: menbreg ccalcfre ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[4.zzwave]) ///
(female0: _b[2.sex_9] + _b[1.zzwave] + _b[1.zzwave#2.sex_9]) (female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) ///
(female2: _b[2.sex_9]) (female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: menbreg ccalcqnt ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[4.zzwave]) ///
(female0: _b[2.sex_9] + _b[1.zzwave] + _b[1.zzwave#2.sex_9]) (female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) ///
(female2: _b[2.sex_9]) (female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: menbreg ccalcbngf ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[4.zzwave]) ///
(female0: _b[2.sex_9] + _b[1.zzwave] + _b[1.zzwave#2.sex_9]) (female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) ///
(female2: _b[2.sex_9]) (female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: menbreg alcfqqnt ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)

* 2.2 Figure E2 and Tables E6 & E7 - Maximum consumption, harms, delivery, and drinking context - Time by gender interaction
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.sex_9]) (c3: _b[4.zzwave] + _b[2.sex_9] + _b[4.zzwave#2.sex_9]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.sex_9]), esampvaryok dots: ///
mixed ccalcmax ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.sex_9]) (c3: _b[4.zzwave] + _b[2.sex_9] + _b[4.zzwave#2.sex_9]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.sex_9]), esampvaryok cmdok dots: ///
menbreg ccnumharm ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.sex_9]) (c3: _b[4.zzwave] + _b[2.sex_9] + _b[4.zzwave#2.sex_9]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.sex_9]), esampvaryok dots: ///
mixed ccalcalone ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.sex_9]) (c3: _b[4.zzwave] + _b[2.sex_9] + _b[4.zzwave#2.sex_9]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.sex_9]), esampvaryok dots: ///
mixed ccalcothers ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.sex_9]) (c3: _b[4.zzwave] + _b[2.sex_9] + _b[4.zzwave#2.sex_9]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.sex_9]), esampvaryok dots: ///
mixed ccalcvirtual ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.sex_9]) (c3: _b[4.zzwave] + _b[2.sex_9] + _b[4.zzwave#2.sex_9]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.sex_9]), esampvaryok cmdok dots: ///
meologit ccalcdlvr ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)

******************************************************************************
* 3. Secondary alcohol outcomes
*-----------------------------------------------------------------------------

* 3.1 Figure E3 and Table E8 - Trends of frequency, quantity and binge drinking - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp]) (high3: _b[2.alcgrp] + _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg ccalcfre ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp]) (high3: _b[2.alcgrp] + _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg ccalcqnt ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp]) (high3: _b[2.alcgrp] + _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg ccalcbngf ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp]) (high3: _b[2.alcgrp] + _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), cmdok dots esampvaryok: menbreg alcfqqnt ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)

* 3.2 Figure E4 and Tables E9 & E10 - Maximum consumption, harms, delivery, and drinking context - Time interaction split by top/bottom half of alcohol freq*quant at wave 9
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[4.zzwave] + _b[2.alcgrp] + _b[4.zzwave#2.alcgrp]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcmax ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[4.zzwave] + _b[2.alcgrp] + _b[4.zzwave#2.alcgrp]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), esampvaryok cmdok dots: ///
menbreg ccnumharm ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[4.zzwave] + _b[2.alcgrp] + _b[4.zzwave#2.alcgrp]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcalone ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[4.zzwave] + _b[2.alcgrp] + _b[4.zzwave#2.alcgrp]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcothers ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[4.zzwave] + _b[2.alcgrp] + _b[4.zzwave#2.alcgrp]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), esampvaryok dots: ///
mixed ccalcvirtual ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 || zzC_ID:
mi estimate (c1: _b[4.zzwave]) (c2: _b[2.alcgrp]) (c3: _b[4.zzwave] + _b[2.alcgrp] + _b[4.zzwave#2.alcgrp]) ///
(diff: _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), esampvaryok cmdok dots: ///
meologit ccalcdlvr ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sex_9!=3 & zzwave>2 || zzC_ID:, intp(30)



******************************************************************************
* 4. Drivers of change analysis
*-----------------------------------------------------------------------------

* 4.1 Figure E5 and Tables E11 & E12 - Money for alcohol analysis
mi estimate (male1: _b[1.zzwave]) (male2: _b[2.zzwave]) (male3: _b[4.zzwave]) ///
(female0: _b[2.sex_9] + _b[1.zzwave] + _b[1.zzwave#2.sex_9]) (female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) ///
(female2: _b[2.sex_9]) (female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: melogit ccalcmon ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (low1: _b[1.zzwave]) (low2: _b[2.zzwave]) (low3: _b[4.zzwave]) ///
(high0: _b[2.alcgrp] + _b[1.zzwave] + _b[1.zzwave#2.alcgrp]) (high1: _b[2.alcgrp] + _b[2.zzwave] + _b[2.zzwave#2.alcgrp]) ///
(high2: _b[2.alcgrp]) (high3: _b[2.alcgrp] + _b[4.zzwave] + _b[4.zzwave#2.alcgrp]), cmdok dots esampvaryok: melogit ccalcmon ib3.zzwave##i.alcgrp i.sex_9 i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)


/*

* 3.1 Figure S5 and Table S9 - Smoking and cannabis
mi estimate (male1: _b[2.zzwave]) (male2: _b[3.zzwave]) (male3: _b[4.zzwave]) (female0: _b[2.sex_9]) ///
(female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) (female2: _b[2.sex_9] + _b[3.zzwave] + _b[3.zzwave#2.sex_9]) ///
(female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: melogit ccsmkfrq ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)
mi estimate (male1: _b[2.zzwave]) (male2: _b[3.zzwave]) (male3: _b[4.zzwave]) (female0: _b[2.sex_9]) ///
(female1: _b[2.sex_9] + _b[2.zzwave] + _b[2.zzwave#2.sex_9]) (female2: _b[2.sex_9] + _b[3.zzwave] + _b[3.zzwave#2.sex_9]) ///
(female3: _b[2.sex_9] + _b[4.zzwave] + _b[4.zzwave#2.sex_9]), cmdok dots esampvaryok: melogit cccanfrq ib3.zzwave##i.sex_9 i.alcgrp i.b_seifa b_famhist b_oldsib age_9 peeruse_9 peerdis_9 if sample==1 || zzC_ID:, intp(30)

