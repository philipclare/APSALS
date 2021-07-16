******************************************************************************
* PROGRAM: S6 Covid Analysis - Primary analysis.do
* PURPOSE: Primary and secondary analyses
* WRITTEN BY: Philip Clare
* DATE: 06/08/2020
******************************************************************************
* 1. Load data for analysis
*-----------------------------------------------------------------------------
* 1.0 Load analysis data

global filepath="C:\Users\pjclare\UNSW\APSALS - Documents\Papers\PIP40. COVID-19 Health Paper\Data"
global verdate="20200922"

cd "$filepath"
use "$filepath\Mental health imputed data long - for analysis $verdate.dta", clear

******************************************************************************
* 1. Mental health
*-----------------------------------------------------------------------------

* 1.2 Table E4 - PHQ and GAD continuous analysis
mi estimate, esampvaryok dots: ///
mixed ccphqscore i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate, esampvaryok dots: ///
mixed ccgadscore i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

* 1.3 Table E5 - PHQ and GAD categories analysis
mi estimate, esampvaryok cmdok dots: ///
meologit phqseverity i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate, esampvaryok cmdok dots: ///
meologit gadseverity i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

* 1.4 Table E6 - PHQ and GAD categories analysis
mi estimate, esampvaryok cmdok dots: ///
melogit phqdisorder i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate, esampvaryok cmdok dots: ///
melogit gaddisorder i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

******************************************************************************
* 2. Gender split
*-----------------------------------------------------------------------------

* 2.1 Figure 1 and Table E8 - PHQ and GAD continuous analysis
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]), esampvaryok dots: ///
mixed ccphqscore i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]) , esampvaryok dots: ///
mixed ccgadscore i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

* 2.2 Figure E1 and Table E9 - PHQ and GAD categories analysis
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]), esampvaryok cmdok dots: ///
meologit phqseverity i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]) , esampvaryok cmdok dots: ///
meologit gadseverity i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

* 2.3 Figure E2 and Table E10 - PHQ and GAD categories analysis
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]), esampvaryok cmdok dots: ///
melogit phqdisorder i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]) , esampvaryok cmdok dots: ///
melogit gaddisorder i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

******************************************************************************
* 3. Help seeking analysis
*-----------------------------------------------------------------------------

* 3.1 Table E12 - Help seeking
mi estimate, cmdok dots esampvaryok: ///
melogit cchlthska i.zzwave i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sex10!=3 & zzwave>=11 || zzC_ID:, intp(30)

* 3.3 Figure E3 and Table E13 - Help seeking with gender interaction
mi estimate (c1: _b[12.zzwave]) (c2: _b[2.sex10]) ///
(c3: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]), cmdok dots esampvaryok: ///
melogit cchlthska i.zzwave##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sex10!=3 & zzwave>=11 || zzC_ID:, intp(30)


******************************************************************************
* 4. Split by previous diagnosis
*-----------------------------------------------------------------------------

* 4.1 Interaction with time
mi estimate (c1: _b[12.zzwave]) (c2: _b[1.diagmh10]) ///
(c3: _b[12.zzwave] + _b[1.diagmh10] + _b[12.zzwave#1.diagmh10]), esampvaryok dots: ///
mixed ccphqscore i.zzwave##i.diagmh10 i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate (c1: _b[12.zzwave]) (c2: _b[1.diagmh10]) ///
(c3: _b[12.zzwave] + _b[1.diagmh10] + _b[12.zzwave#1.diagmh10]) , esampvaryok dots: ///
mixed ccgadscore i.zzwave##i.diagmh10 i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:

* 4.1 Interaction with gender and time
mi estimate (c1: _b[12.zzwave]) (c2: _b[1.diagmh10]) (c3: _b[12.zzwave] + _b[1.diagmh10] + _b[12.zzwave#1.diagmh10]) ///
(c4: _b[2.sex10]) (c5: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]) (c6: _b[1.diagmh10] + _b[2.sex10] + _b[1.diagmh10#2.sex10]) ///
(c7: _b[12.zzwave] + _b[1.diagmh10] + _b[2.sex10] + _b[12.zzwave#1.diagmh10] + _b[12.zzwave#2.sex10] + _b[1.diagmh10#2.sex10]+ _b[12.zzwave#1.diagmh10#2.sex10]), ///
esampvaryok dots: mixed ccphqscore i.zzwave##i.diagmh10##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
mi estimate (c1: _b[12.zzwave]) (c2: _b[1.diagmh10]) (c3: _b[12.zzwave] + _b[1.diagmh10] + _b[12.zzwave#1.diagmh10]) ///
(c4: _b[2.sex10]) (c5: _b[12.zzwave] + _b[2.sex10] + _b[12.zzwave#2.sex10]) (c6: _b[1.diagmh10] + _b[2.sex10] + _b[1.diagmh10#2.sex10]) ///
(c7: _b[12.zzwave] + _b[1.diagmh10] + _b[2.sex10] + _b[12.zzwave#1.diagmh10] + _b[12.zzwave#2.sex10] + _b[1.diagmh10#2.sex10]+ _b[12.zzwave#1.diagmh10#2.sex10]), ///
esampvaryok dots: mixed ccgadscore i.zzwave##i.diagmh10##i.sex10 c.age10 ccstndt10 ccempl10 chlivealn10 i.b_seifa b_oldsib peeruse10 peerdis10 if sample==1 || zzC_ID:
