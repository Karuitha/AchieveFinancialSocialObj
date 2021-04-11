cd "C:\Users\John Karuitha\OneDrive - University of Witwatersrand\Documents\My Thesis\Karuitha and Ojah Data\THESIS\Dissertation\Objective 1 Binary\Chapters in Progress\AchieveFinancialSocialObj"
use "data.dta" 

set more off, perm
xtset mfiid year
set matsize 11000
**by mfiid, sort: egen rounds = count( year )
//NB: Average is 7, while median is 5 and mode is 2
//Regressions with cooperatives

**encode sustainability ****************
label define sustainability 0 "OSS" 1 "Non-OSS"
encode sustainability, generate(sustainability1) label(sustainability)
drop sustainability
rename sustainability1 sustainability 
replace sustainability = . if sustainability == 2
****************************************

gen finsoc=.
tostring finsoc, replace
replace finsoc="ss" if sustainability==0 & percent_of_female_borrowers>=0.5
replace finsoc="sf" if sustainability==0 & percent_of_female_borrowers<0.5
replace finsoc="fs" if sustainability==1 & percent_of_female_borrowers>=0.5
replace finsoc="ff" if sustainability==1 & percent_of_female_borrowers<0.5

replace finsoc=. if sustainability==0 & percent_of_female_borrowers ==.
replace finsoc=. if sustainability==0 & percent_of_female_borrowers==.
replace finsoc=. if sustainability==1 & percent_of_female_borrowers==.
replace finsoc=. if sustainability==1 & percent_of_female_borrowers==.

*****************************************
*encode finsoc 
label define finsoc 0 "ff" 1 "fs" 2 "sf" 3 "ss"
encode finsoc, generate(finsoc1) label(finsoc)
drop finsoc
rename finsoc1 finsoc

******************************************
//nonoss=1
//oss=2
//women_borrowers>=0.5, 
//women_borrowers<0.5

//CODE FOR FINAL ANALYSIS
mlogit finsoc ib2.age ib2.legal_tradition asset_structure fdev kkm education i.currentlegalstatus i.year, baseoutcome(0) nocons
vif, uncentered
mlogit finsoc ib2.age ib2.legal_tradition asset_structure_w fdev_w kkm_w education_w i.currentlegalstatus i.year, baseoutcome(0) nocons

mprobit finsoc ib2.age ib2.legal_tradition asset_structure fdev kkm education i.currentlegalstatus i.year, baseoutcome(0) nocons

local indepvar    i.currentlegalstatus i.year
local depvar finsoc

//mlogit for each of ss,sf, fs,ff good
mlogit `depvar' `indepvar' 
est store a12
mlogit `depvar' `indepvar' if rounds>=5
est store a13
mlogit `depvar' `indepvar' if rounds>=7
est store a15
mlogit `depvar' `indepvar' if rounds<=7
est store a16
esttab a12 a13 a15 a16 using multinomial_by_success1.rtf, se replace

//mprobit for each of ss,sf, fs,ff good
local indepvar ib2.age ib2.region lassets gdpgrowthannual dkkm deduc fd_no_mfi ib2.legaltradition i.currentlegalstatus i.year
local depvar finsoc1
mprobit `depvar' `indepvar' 
est store a17
mprobit `depvar' `indepvar' if rounds>=5
est store a18
mprobit `depvar' `indepvar' if rounds>=7
est store a20
mprobit `depvar' `indepvar' if rounds<=7
est store a21

esttab a17 a18 a20 a21 using multinomial_probit_by_success1.rtf, se replace

//END OF CODE
//Estimate MER and store the graphs
margins, dydx(region lassets dkkm fd_no_mfi) at(legaltradition=( 1 2 3)) post
est sto b11
marginsplot, noci 
graph save margins_legal, replace
margins, dydx(region lassets dkkm fd_no_mfi legaltradition) at(region=( 1 2 3 4 5)) post
est sto b12
marginsplot, noci
graph save margins_region, replace
margins, dydx(region lassets dkkm fd_no_mfi legaltradition) at(age=(1 3 2)) post
est sto b13
marginspot, noci
graph save  margins_age, replace
margins, dydx(region lassets dkkm fd_no_mfi legaltradition) at(lassets=(1 (5) 25)) post
est sto b14
marginsplot, xlabel(0(5)25) noci
graph save margins_size, replace
margins, dydx(region lassets dkkm legaltradition) at(fd_no_mfi=(-2 (2) 8))
marginsplot, xlabel(-2(2)8) noci
graph save margins_fd, replace
margins, dydx(region lassets fd_no_mfi legaltradition) at(dkkm=( -1 (0.2) 1.2))
marginsplot, xlabel(-1(0.2)1.2) noci
graph save margins_kkm, replace
esttab b11 b12 b13 b14 using multinomial_margins.rtf, se replace

//Summary statistics
asdoc tab region finsoc1, row replace
est sto region
esttab region using region_summary.rtf, replace
asdoc tab currentlegalstatus finsoc1, row 
est sto legalstatus
esttab legalstatus using legalstatus_summary.rtf, replace
asdoc tab legaltradition finsoc1, row
est sto legaltrad
esttab legaltrad using legaltrad_summary.rtf, replace
asdoc xtsum finsoc1 age region lassets gdpgrowthannual dkkm deduc fd_no_mfi legaltradition currentlegalstatus, replace

//Margins by UCLA
margins age, atmeans predict (outcome(2))
marginsplot, name(age)
margins region, atmeans predict (outcome(2))
marginsplot, name(region)
margins legaltradition, atmeans predict (outcome(2))
marginsplot, name(legal_Tradition)
margins currentlegalstatus, atmeans predict (outcome(2)) 
marginsplot, name(Current_Legal_Status)
margins, at(lassets=(1 (5) 25)) predict (outcome(2))
marginsplot, name(Size)
margins, at(gdpgrowthannual=(-55 (10) 35)) predict (outcome(2))
marginsplot, name(GDPgrowth)
graph combine age region legal_Tradition Current_Legal_Status Size GDPgrowth, ycommon

margins age, atmeans predict (outcome(3))
marginsplot, name(age)
margins region, atmeans predict (outcome(3))
marginsplot, name(region)
margins legaltradition, atmeans predict (outcome(3))
marginsplot, name(legal_Tradition)
margins currentlegalstatus, atmeans predict (outcome(3)) 
marginsplot, name(Current_Legal_Status)
margins, at(lassets=(1 (5) 25)) predict (outcome(3))
marginsplot, name(Size)
margins, at(gdpgrowthannual=(-55 (10) 35)) predict (outcome(3))
marginsplot, name(GDPgrowth)
graph combine age region legal_Tradition Current_Legal_Status Size GDPgrowth, ycommon

margins age, atmeans predict (outcome(4))
marginsplot, name(age)
margins region, atmeans predict (outcome(4))
marginsplot, name(region)
margins legaltradition, atmeans predict (outcome(4))
marginsplot, name(legal_Tradition)
margins currentlegalstatus, atmeans predict (outcome(4)) 
marginsplot, name(Current_Legal_Status)
margins, at(lassets=(1 (5) 25)) predict (outcome(4))
marginsplot, name(Size)
margins, at(gdpgrowthannual=(-55 (10) 35)) predict (outcome(4))
marginsplot, name(GDPgrowth)
graph combine age region legal_Tradition Current_Legal_Status Size GDPgrowth, ycommon

margins age, atmeans predict (outcome(5))
marginsplot, name(age)
margins region, atmeans predict (outcome(5))
marginsplot, name(region)
margins legaltradition, atmeans predict (outcome(5))
marginsplot, name(legal_Tradition)
margins currentlegalstatus, atmeans predict (outcome(5)) 
marginsplot, name(Current_Legal_Status)
margins, at(lassets=(1 (5) 25)) predict (outcome(5))
marginsplot, name(Size)
margins, at(gdpgrowthannual=(-55 (10) 35)) predict (outcome(5))
marginsplot, name(GDPgrowth)
graph combine age region legal_Tradition Current_Legal_Status Size GDPgrowth, ycommon
