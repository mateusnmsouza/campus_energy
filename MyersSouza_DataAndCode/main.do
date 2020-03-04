********************************************************************************
********************************************************************************
/*
Social Comparison Nudges Without Monetary Incentives: 
       Evidence from Home Energy Reports
	   
Journal of Environmental Economics and Management, January 2020

Erica Myers and Mateus Souza

University of Illinois at Urbana-Champaign
Department of Agricultural and Consumer Economics

CODE FOR MAIN ANALYSES IN THE MANUSCRIPT

*/


****************************** PREAMBLE ****************************************

*** install packages
foreach x in winsor2 reghdfe ftools byhist {
ssc install `x', replace
}
reghdfe, compile

clear all
log close _all
capture _all
scalar drop _all
set more off, perm

*** Directory with all the data and code
cd "P:\iSEE\MyersSouza_DataAndCode"

********************************************************************************


*** read anonymized data used in analyses for the paper
use "data_anonym.dta"

*** Balance tables for main (Fall) intervention
sort roomCluster RoundDateTime
by roomCluster : egen meanSTPT_pre = mean(CTLSTPT) if email_intervent==0
by roomCluster : egen STPT_pre = min(meanSTPT_pre)
by roomCluster : egen meanSTPT_post = mean(CTLSTPT) if email_intervent==1
by roomCluster : egen STPT_post = min(meanSTPT_post)

keep roomCluster clusttype floor wing location tab_college_desc* treat treatA treatB STPT_pre STPT_post
duplicates drop
drop if roomCluster==.

tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)

matrix input rendemog = (.,.,.,.,.)
local vars STPT_pre STPT_post tab_clusttype1 tab_clusttype2 tab_clusttype3 tab_clusttype4 ///
			tab_floor1 tab_floor2 tab_floor3 tab_floor4 tab_floor5 tab_floor6 tab_wing1 tab_wing2 ///
			tab_location1 tab_location2 tab_location3 tab_location4 tab_location5 tab_location6 ///
			tab_college_desc1 tab_college_desc2 tab_college_desc3 tab_college_desc4 tab_college_desc5 tab_college_desc6 tab_college_desc7 tab_college_desc8 tab_college_desc9 tab_college_desc10
foreach x of local vars {
ttest `x', by(treatA)
sca rend1`x' = `r(mu_1)'
sca rend2`x' = `r(mu_2)'
sca renp1`x' = `r(p)'
ttest `x', by(treatB)
sca rend3`x' = `r(mu_2)'
sca renp2`x' = `r(p)'
matrix ren`x' = (rend1`x' , rend2`x', renp1`x', rend3`x', renp2`x')
matrix rendemog = (rendemog \ ren`x')
}
putexcel set "./Results/balancetables3.xlsx", modify sheet("fall")
putexcel C3 = matrix(rendemog)


*************
*** Balance tables for Winter Break intervention
clear all
use "data_anonym.dta"

gen outdiff_abs = abs(outdiff)

gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

sort room RoundDateTime
by room : egen meanSTPT_pre = mean(CTLSTPT) if post_winteremail==0
by room : egen meanSTPT_post = mean(CTLSTPT) if post_winteremail==1
by room : egen STPT_pre = min(meanSTPT_pre)
by room : egen STPT_post = min(meanSTPT_post)

keep room roomtype floor wing location tab_college_desc* winterbreak_treat STPT_pre STPT_post
duplicates drop
drop if roomtype==.

tabulate roomtype, gen(tab_roomtype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)

matrix input rendemog = (.,.,.)
local vars STPT_pre STPT_post tab_roomtype1 tab_roomtype2 tab_floor1 tab_floor2 tab_floor3 tab_floor4 tab_floor5 tab_floor6 ///
			tab_wing1 tab_wing2 tab_location1 tab_location2 tab_location3 tab_location4 tab_location5 tab_location6 ///
			tab_college_desc1 tab_college_desc2 tab_college_desc3 tab_college_desc4 tab_college_desc5 tab_college_desc6 tab_college_desc7 tab_college_desc8 tab_college_desc9 tab_college_desc10

foreach x of local vars {
ttest `x', by(winterbreak_treat)
sca rend1`x' = `r(mu_1)'
sca rend2`x' = `r(mu_2)'
sca renp1`x' = `r(p)'
matrix ren`x' = (rend1`x' , rend2`x', renp1`x')
matrix rendemog = (rendemog \ ren`x')
}
putexcel set "./Results/balancetables3.xlsx", modify sheet("winter")
putexcel C3 = matrix(rendemog)


**********
*** Balance tables for Spring intervention
clear all
use "data_anonym.dta"

sort room RoundDateTime
by room : egen meanSTPT_pre = mean(CTLSTPT) if email_intervent_spring==0
by room : egen meanSTPT_post = mean(CTLSTPT) if email_intervent_spring==1
by room : egen STPT_pre = min(meanSTPT_pre)
by room : egen STPT_post = min(meanSTPT_post)

gen outdiff_abs = abs(outdiff)

keep roomCluster clusttype floor wing location tab_college_desc* treat_spring individtreat_spring simpletreat_spring STPT_pre STPT_post
duplicates drop
drop if roomCluster==.


tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)

matrix input rendemog = (.,.,.,.,.)
local vars STPT_pre STPT_post tab_clusttype1 tab_clusttype2 tab_clusttype3 tab_clusttype4 tab_floor1 tab_floor2 tab_floor3 tab_floor4 tab_floor5 tab_floor6 ///
		tab_wing1 tab_wing2 tab_location1 tab_location2 tab_location3 tab_location4 tab_location5 tab_location6 ///
		tab_college_desc1 tab_college_desc2 tab_college_desc3 tab_college_desc4 tab_college_desc5 tab_college_desc6 tab_college_desc7 tab_college_desc8 tab_college_desc9 tab_college_desc10

foreach x of local vars {
ttest `x', by(individtreat_spring)
sca rend1`x' = `r(mu_1)'
sca rend2`x' = `r(mu_2)'
sca renp1`x' = `r(p)'
ttest `x', by(simpletreat_spring)
sca rend3`x' = `r(mu_2)'
sca renp2`x' = `r(p)'
matrix ren`x' = (rend1`x' , rend2`x', renp1`x', rend3`x', renp2`x')
matrix rendemog = (rendemog \ ren`x')
}
putexcel set "./Results/balancetables3.xlsx", modify sheet("spring")
putexcel C3 = matrix(rendemog)


********************************************************************************
********************************************************************************
************************** DESCRIPTIVE STATISTICS ******************************

*** Number of observations by date
clear all
use "data_anonym.dta"

keep date obsbydate
duplicates drop
gen monthly = date
format monthly %tm

twoway (line obsbydate date), xtitle("Date") ytitle("Number of Observations") ///
	tlabel(01aug2016(30)22apr2018, labsize(vsmall) angle(45)) graphregion(color(white)) bgcolor(white)
graph export "./Results/obs_by_date.png", replace

gen double statdate = date

twoway (line obsbydate date) if statdate>=21059 & statdate<=21167, xtitle("Date") ytitle("Number of Observations") ///
	tlabel(28aug2017(31)14dec2017, labsize(vsmall) angle(45)) graphregion(color(white)) bgcolor(white)
graph export "./Results/obsbydate_fall.png", replace


*** number of observations by room by date
clear all
use "data_anonym.dta"

keep if obsroomdate>=20 & obsroomdate!=.
keep room date
duplicates drop

sort date
by date : gen numrooms=_n
by date : egen totrooms = max(numrooms)

keep date totrooms
duplicates drop

twoway (line totrooms date), xtitle("Date") ytitle("Number of Rooms") ///
	tlabel(01aug2016(30)22apr2018, labsize(vsmall) angle(45)) graphregion(color(white)) bgcolor(white)
graph export "./Results/rooms_by_date.png", replace

gen double statdate = date

twoway (line totrooms date) if statdate>=21059 & statdate<=21167, xtitle("Date") ytitle("Number of Observations") ///
	tlabel(28aug2017(31)14dec2017, labsize(vsmall) angle(45)) graphregion(color(white)) bgcolor(white)
graph export "./Results/roomsbydate_fall.png", replace



clear all
use "data_anonym.dta"

* fix hour of day variable so it can be interacted with treatment (cannot have a 0 value)
replace hourofday = hourofday + 1
label define hofd 0 "NoTreat" 1 "0" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6" 8 "7" 9 "8" 10 "9" 11 "10" 12 "11" ///
				13 "12" 14 "13" 15 "14" 16 "15" 17 "16" 18 "17" 19 "18" 20 "19" 21 "20" 22 "21" 23 "22" 24 "23"
label values hourofday hofd

* interact hourofday with treatment
gen treatXhour = treat*hourofday
label values treatXhour hofd

* number of observations by therm set up
gen roundCTLSTPT = round(CTLSTPT)
gen aux = 1

* pre-intervention
bysort roundCTLSTPT : egen ton = total(aux) if treat==0 & roomtype!=. & email_intervent==0 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_notreat = max(ton)

bysort roundCTLSTPT : egen tot = total(aux) if treat==1 & roomtype!=. & email_intervent==0 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_treat = max(tot)
drop ton tot

count if treat==0 & roomtype!=. & email_intervent==0 & roundCTLSTPT!=.
sca samp_notreat = `r(N)'
count if treat==1 & roomtype!=. & email_intervent==0  & roundCTLSTPT!=.
sca samp_treat = `r(N)'


* during intervention
bysort roundCTLSTPT : egen ton = total(aux) if treat==0 & roomtype!=. & email_intervent==1 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_notreat_post = max(ton)

bysort roundCTLSTPT : egen tot = total(aux) if treat==1 & roomtype!=. & email_intervent==1 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_treat_post = max(tot)

count if treat==0 & roomtype!=. & email_intervent==1 & roundCTLSTPT!=.
sca samp_notreat_post = `r(N)'
count if treat==1 & roomtype!=. & email_intervent==1  & roundCTLSTPT!=.
sca samp_treat_post = `r(N)'

** frequencies
gen therm_freq_notreat = 100*therm_obs_notreat/samp_notreat
gen therm_freq_treat = 100*therm_obs_treat/samp_treat

gen therm_freq_notreat_post = 100*therm_obs_notreat_post/samp_notreat_post
gen therm_freq_treat_post = 100*therm_obs_treat_post/samp_treat_post

** Histograms/density of thermostat setpoints
graph bar therm_freq_notreat therm_freq_treat, over(roundCTLSTPT) ///
		legend(order(1 "Control" 2 "Treated" )) graphregion(color(white)) bgcolor(white) ///
		b1title("Baseline Thermostat Setpoints ({superscript:o}F)") ytitle("Percent of Observations")
graph export "./Results/histogram_setpoints2.png", replace

graph bar therm_freq_notreat_post therm_freq_treat_post, over(roundCTLSTPT) ///
		legend(order(1 "Control" 2 "Treated" )) graphregion(color(white)) bgcolor(white) ///
		b1title("Thermostat Setpoints During Treatment Period ({superscript:o}F)") ytitle("Percent of Observations")
graph export "./Results/histogram_setpoints_post.png", replace


** Setpoints by hour of day - NO CONTROLS during-treatment
reg CTLSTPT ibn.hourofday if ///
			 treat==1 & roomtype!=. & email_intervent==1, vce(cluster room) nocons
est sto mean_setpoints_treat
	
reg CTLSTPT ibn.hourofday if ///
			 treat==0 & roomtype!=. & email_intervent==1, vce(cluster room) nocons
est sto mean_setpoints_control

* difference between treat and control
reghdfe CTLSTPT ib0.treatXhour if roomtype!=. & email_intervent==1, nocons vce(cluster room) absorb(ibn.hourofday)
est sto mean_setpoints_diff

coefplot mean_setpoints_control (mean_setpoints_treat, msymbol(Th)) ///
			(mean_setpoints_diff,  color(black) msymbol(none) axis(2) recast(connected) lpattern(dash) ciopts(lpattern(blank))), vertical ///
			xlabel(1 "0" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6" 8 "7" 9 "8" 10 "9" 11 "10" 12 "11" 13 "12" ///
			14 "13" 15 "14" 16 "15" 17 "16" 18 "17" 19 "18" 20 "19" 21 "20" 22 "21" 23 "22" 24 "23") ///
			rename(*.treatXhour = .hourofday) ///
			xtitle("Hour of Day") ytitle("Temperature ({superscript:o}F)") ytitle("Temperature Difference ({superscript:o}F)", axis(2)) ///
			legend(order(2 "Control" 4 "Treated" 6 "Difference")) title("Average Setpoints by Hour of Day" "(During Trial Period)") ///
			keep(*hour*) graphregion(color(white)) bgcolor(white) ///
			note("Note: 95% confidence intervals based on room-level clustered standard errors")
graph export "./Results/setpoints_byhour_intervent.png", replace

** Setpoints by hour of day - NO CONTROLS pre-treatment
reg CTLSTPT ibn.hourofday if ///
			 treat==1 & roomtype!=. & email_intervent==0, vce(cluster room) nocons
est sto mean_setpoints_treat
	
reg CTLSTPT ibn.hourofday if ///
			 treat==0 & roomtype!=. & email_intervent==0, vce(cluster room) nocons
est sto mean_setpoints_control

* difference between treat and control
reghdfe CTLSTPT ib0.treatXhour if roomtype!=. & email_intervent==0, nocons vce(cluster room) absorb(ibn.hourofday)
est sto mean_setpoints_diff

coefplot mean_setpoints_control (mean_setpoints_treat, msymbol(Th)) ///
			(mean_setpoints_diff,  color(black) msymbol(none) axis(2) recast(connected) lpattern(dash) ciopts(lpattern(blank))), vertical ///
			xlabel(1 "0" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6" 8 "7" 9 "8" 10 "9" 11 "10" 12 "11" 13 "12" ///
			14 "13" 15 "14" 16 "15" 17 "16" 18 "17" 19 "18" 20 "19" 21 "20" 22 "21" 23 "22" 24 "23") ///
			rename(*.treatXhour = .hourofday) ///
			xtitle("Hour of Day") ytitle("Temperature ({superscript:o}F)") ytitle("Temperature Difference ({superscript:o}F)", axis(2)) ///
			legend(order(2 "Control" 4 "Treated" 6 "Difference")) title("Average Setpoints by Hour of Day" "(Baseline)") ///
			keep(*hour*) graphregion(color(white)) bgcolor(white) ///
			note("Note: 95% confidence intervals based on room-level clustered standard errors")
graph export "./Results/setpoints_byhour_preintervent.png", replace


** Setpoints by date   - NO CONTROLS
gen stable_date = 0
replace stable_date = 1 if CTLSTPT!=. & email_intervent!=. & roomtype!=.
sort date
by date : egen stabledate = total(stable_date) 
drop stable_date
gen statdate = date
order statdate, after(date)

*tab statdate if stabledate<=20000 & CTLSTPT!=. & email_intervent!=. & roomtype!=.
*tab date if stabledate<=20000 & CTLSTPT!=. & email_intervent!=. & roomtype!=.
/* dates with less than 20,000 available CTLSTPT observations:
       date |      Freq.     Percent        Cum.
------------+-----------------------------------
  13oct2017 |     30,720       16.67       16.67
  27oct2017 |     30,720       16.67       33.34
  06nov2017 |     30,720       16.67       50.02
  07nov2017 |     30,663       16.64       66.66
  08nov2017 |     30,720       16.67       83.33
  24nov2017 |     30,720       16.67      100.00
------------+-----------------------------------
      Total |    184,263      100.00
*/

* interact date variable with treatment
gen treatXdate = treat*date

** Setpoints by date
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 ///
			& treat==1 & email_intervent!=. & roomtype!=. , nocons vce(cluster room)		
est sto stptbydate_treat
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 ///
			& treat==0 & email_intervent!=. & roomtype!=. , nocons vce(cluster room)		
est sto stptbydate_control

* difference between treat and control
reghdfe CTLSTPT ib0.treatXdate if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 ///
			& email_intervent!=. & roomtype!=. , nocons vce(cluster room) absorb(ibn.date)	
est sto stptbydate_diff


coefplot stptbydate_control (stptbydate_treat, msymbol(Th)) ///
					(stptbydate_diff,  color(black) msymbol(none) axis(2) recast(connected) lpattern(dash) ciopts(lpattern(blank))), vertical  ///
					xlabel(1 "08/28" 15 "09/11" 30 "09/26" 45 "10/11" 60 "10/28" ///
					75 "11/15" 90 "12/01" 103 "12/14", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature ({superscript:o}F)") ytitle("Temperature Difference ({superscript:o}F)", axis(2)) ///
					legend(order(2 "Control" 4 "Treated" 6 "Difference")) title("Average Setpoints by Date") ///
					rename(*.treatXdate = .date) keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(16.5) text(72.5 16.6 "Treatment Starts", placement(e) color(red) size(small) orient(vertical) just(left)) ///
					xline(77.5) text(72.85 78 "Break", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
					xline(85.5) ///
					note("Note: 95% confidence intervals based on room-level clustered standard errors")
graph export "./Results/setpoints_bydate.png", replace


** Number of observations by treatment group
quietly : tab room if individual_treatment ==1 & roomtype!=. & CTLSTPT!=. & email_intervent==1
dis r(r)
quietly : tab room if cluster_treatment ==1 & roomtype!=. & CTLSTPT!=. & email_intervent==1
dis r(r)
quietly : tab room if pure_control ==1 & roomtype!=. & CTLSTPT!=. & email_intervent==1
dis r(r)

quietly : tab room if winterbreak_treat==1 & roomtype!=. & CTLSTPT!=. & statdatetime>=1827705600000 & statdatetime<=1830383100000
dis r(r)
quietly : tab room if winterbreak_treat==0 & roomtype!=. & CTLSTPT!=. & statdatetime>=1827705600000 & statdatetime<=1830383100000
dis r(r)

********************************************************************************
** Temperature by date
tempfile clean_campuseneregy
save `clean_campuseneregy'

keep if statdatetime>=1820880000000 & statdatetime<=1839973500000
keep hourlydrybulbtempf date hourofday hourofsample
duplicates drop

reg hourlydrybulbtempf ibn.date, nocons vce(cluster hourofsample)
est sto temp_bydate
coefplot temp_bydate, vertical ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(off) title("Average Outdoor Temperature by Date") ///
					graphregion(color(white)) bgcolor(white) xlabel(1 "09/13/17" 31 "10/13/17" 62 "11/13/17" 92 "12/13/17" 123 "01/13/18" ///
					154 "02/13/18" 182 "03/13/18" 213 "04/13/18", angle(45) labsize(small))
graph export "P:\iSEE\CampusEnergy2018\Results\outdoortemp_bydate.png", replace



** Temperature by time of day
reg hourlydrybulbtempf ibn.hourofday, nocons vce(cluster hourofsample)
est sto temp_byhour
coefplot temp_byhour, vertical xlabel(1 "0" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6" 8 "7" 9 "8" 10 "9" 11 "10" 12 "11" 13 "12" 14 "13" 15 "14" 16 "15" 17 "16" 18 "17" 19 "18" 20 "19" 21 "20" 22 "21" 23 "22" 24 "23") ///
			xtitle("Hour of Day") ytitle("Temperature (Degrees Fahrenheit)") legend(off) title("Average Outdoor Temperature by Hour of Day") ///
			graphregion(color(white)) bgcolor(white)
graph export "P:\iSEE\CampusEnergy2018\Results\outdoortemp_byhour.png", replace

clear
use `clean_campuseneregy'


********************************************************************************
** Graphs from Power Calculations
tempfile clean_campuseneregy
save `clean_campuseneregy'

use "./power_graphs.dta"

forval i = 50(5)85 {
gen lower`i' = Coeff`i' - (1.96*Std`i')
gen upper`i' = Coeff`i' + (1.96*Std`i')
}

gen tempsort = .

forval i = 50(5)85 {
sca true_eff = `i'/100
local true = true_eff
sort Coeff`i'
replace tempsort = _n
twoway rcap upper`i' lower`i' tempsort, lstyle(ci) ||   ///
       scatter Coeff`i' tempsort, mstyle(p1)          ///
       legend(order(2 "Coefficient on treatment" "(True = 0.`i' F)"))                         ///
       note("with 95% confidence interval; clustering by suite")         ///
	   xtitle("Iteration") ///
	   ytitle("Recovered effect size") ///
	   graphregion(color(white)) bgcolor(white) yline(`true') yline(0, lcolor(black))
graph export "./Results/power`i'.png", replace
}

clear
use `clean_campuseneregy'




********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
***************************** REGRESSIONS **************************************
********************************************************************************
clear all
use "data_anonym.dta"

label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Sep. 13"
label values email_intervent posttreatlabel


********************************************************************************
*************** Model Using Absolute Difference as Outcome *********************
gen outdiff_abs = abs(outdiff)

* some descriptives on the outdiff variable, to be added manually to result tables
*sum outdiff if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000 & roomtype!=. & treat!=.
*dis 25*24*46.15*0.454
*sum outdiff if email_intervent!=. & roomtype!=. & treat!=.
sum outdiff_abs if email_intervent!=. & roomtype!=. & treat!=.
sum outdiff_abs if email_intervent==1 & roomtype!=. & treat!=.


** Simple regs on treatment
reg outdiff_abs i.treat if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg outdiff_abs i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe outdiff_abs i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe outdiff_abs i.treat if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen auxvar = mean(CTLSTPT) if email_intervent==0
by room : egen avg_stpt_pre = min(auxvar)
drop auxvar

reghdfe outdiff_abs i.treat avg_stpt_pre if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe outdiff_abs 1.treat##1.email_intervent if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect_outdiff.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat 1.treat#1.email_intervent)



** Regs separating the 2 treatment arms
label define indtreatlabel 1 "Room-level Treatment"
label values individual_treatment indtreatlabel
label define clusttreatlabel 1 "Suite-level Treatment"
label values cluster_treatment clusttreatlabel

reg outdiff_abs i.individual_treatment i.cluster_treatment if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg outdiff_abs i.individual_treatment i.cluster_treatment i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe outdiff_abs i.individual_treatment i.cluster_treatment hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe outdiff_abs i.individual_treatment i.cluster_treatment if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe outdiff_abs i.individual_treatment i.cluster_treatment avg_stpt_pre if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe outdiff_abs 1.individual_treatment##1.email_intervent 1.cluster_treatment##1.email_intervent if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/septreat_effect_outdiff.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.individual_treatment 1.cluster_treatment 1.individual_treatment#1.email_intervent 1.cluster_treatment#1.email_intervent)
			
			
			
********************************************************************************
******************** Model Using Thermostats as Outcome ************************

** Simple regs on treatment
reg CTLSTPT i.treat if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.treat if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.treat avg_stpt_pre if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.treat#i.email_intervent if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat 1.treat#1.email_intervent)
			
*esttab teffect teffect_control teffect_morecont using "./Results/treat_effect.tex", replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) keep(1.treat)

** Regs separating the 2 treatment arms
label define indtreatlabel 1 "Room-level Treatment"
label values individual_treatment indtreatlabel
label define clusttreatlabel 1 "Suite-level Treatment"
label values cluster_treatment clusttreatlabel

reg CTLSTPT i.individual_treatment i.cluster_treatment if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.individual_treatment i.cluster_treatment i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.individual_treatment i.cluster_treatment hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.individual_treatment i.cluster_treatment if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.individual_treatment i.cluster_treatment avg_stpt_pre if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.individual_treatment#i.email_intervent i.cluster_treatment#i.email_intervent if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/septreat_effect.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.individual_treatment 1.cluster_treatment 1.individual_treatment#1.email_intervent 1.cluster_treatment#1.email_intervent)



** Simple regs on treatment - only for cold days
sort date
by date : egen maxtemp = max(hourlydrybulbtempf)

gen coldday = 1 if maxtemp<65

reg CTLSTPT i.treat if email_intervent==1 & roomtype!=. & coldday==1, vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. & coldday==1, vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. & coldday==1, ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.treat if email_intervent==1 & roomtype!=. & coldday==1, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.treat avg_stpt_pre if email_intervent==1 & roomtype!=. & coldday==1, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates not possible with cold days

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre using "./Results/treat_effect_coldday.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat)


*************************
** Effects by hour of day - Appendix
gen graphhourofday = hourofday + 1
label values graphhourofday hourdaylab

forval x = 1(1)24 {
gen hour`x' = 0
replace hour`x' = 1 if graphhourofday==`x'
gen treatXhour`x' = treat*hour`x'
label define hourlab`x' 1 "`x'"
label values treatXhour`x' hourlab`x'
}

reghdfe CTLSTPT i.treatXhour* if email_intervent==1 & roomtype!=., absorb(i.floor i.wing i.location i.window i.date) vce(cluster roomCluster)
est sto treathour_datecont

coefplot treathour_datecont, vertical xlabel(1 "1am" 2 "2am" 3 "3am" 4 "4am" 5 "5am" 6 "6am" 7 "7am" 8 "8am" 9 "9am" 10 "10am" 11 "11am" 12 "noon" 13 "1pm" 14 "2pm" 15 "3pm" 16 "4pm" 17 "5pm" 18 "6pm" 19 "7pm" 20 "8pm" 21 "9pm" 22 "10pm" 23 "11pm" 24 "midnight", labsize(small) angle(45)) ///
			xtitle("Hour of Day") ytitle("Treatment Effect") legend(off) title("Treatment Effects by Hour of Day") ///
			graphregion(color(white)) bgcolor(white)
graph export "./Results/treatbyhour.png", replace

** Effect by weekday
forval i = 1(1)7 {
gen weekday`i' = 0
replace weekday`i' = 1 if weekday==`i'
gen treatXweekday`i' = treat*weekday`i'
}
label define weekdaylab1 1 "Monday" 
label define weekdaylab2 1 "Tuesday" 
label define weekdaylab3 1 "Wednesday" 
label define weekdaylab4 1 "Thursday" 
label define weekdaylab5 1 "Friday" 
label define weekdaylab6 1 "Saturday" 
label define weekdaylab7 1 "Sunday"
label values treatXweekday1 weekdaylab1
label values treatXweekday2 weekdaylab2
label values treatXweekday3 weekdaylab3
label values treatXweekday4 weekdaylab4
label values treatXweekday5 weekdaylab5
label values treatXweekday6 weekdaylab6
label values treatXweekday7 weekdaylab7

reghdfe CTLSTPT i.treatXweekday* if email_intervent==1 & roomtype!=., absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto treatweekday_moreconts

esttab treatweekday_moreconts using "./Results/effectbyweekday.tex", replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label


** Effect by bins of outdoor temperature
gen tempbins2 = ""
forval i = -20(10)90 {
replace tempbins2 = "`i'" if hourlydrybulbtempf>`i' & hourlydrybulbtempf<=`i'+10
}
encode tempbins2, gen(tempbins)
gen treatXtempbins = treat*tempbins

reg CTLSTPT i.tempbins i.treatXtempbins if email_intervent==1, vce(cluster roomCluster)
est sto treattemp

reghdfe CTLSTPT i.tempbins i.treatXtempbins if email_intervent==1, absorb(date) vce(cluster roomCluster)
est sto treattemp_conts

esttab treattemp treattemp_conts using "./Results/effectbytemp.csv", replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)

** Effect based on rating received in previous week
gen treatXlaggood = treat*lag_good
gen treatXlagbad = treat*lag_bad
gen treatXlaggreat = treat*lag_great

reg CTLSTPT i.treat i.treatXlaggood i.treatXlagbad i.lag_good i.lag_bad if email_intervent==1, vce(cluster roomCluster)
est sto treatlagrate

reghdfe CTLSTPT i.treat i.treatXlaggood i.treatXlagbad i.lag_good i.lag_bad if email_intervent==1, absorb(date) vce(cluster roomCluster)
est sto treatlagrate_conts

reghdfe CTLSTPT i.treat i.treatXlaggood i.treatXlagbad i.lag_good i.lag_bad if email_intervent==1, absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto treatlagrate_moreconts

esttab treatlagrate treatlagrate_conts treatlagrate_moreconts using "./Results/effectbylagrate.tex", replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)



** Effect by bedroom type
label define singbed 1 "Single-Bedroom"
label values single_bedroom singbed

reg outdiff_abs i.treat##i.single_bedroom if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg outdiff_abs i.treat##i.single_bedroom i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe outdiff_abs i.treat##i.single_bedroom hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe outdiff_abs i.treat##i.single_bedroom if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe outdiff_abs i.treat##i.single_bedroom avg_stpt_pre if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe outdiff_abs 1.treat##1.email_intervent##1.single_bedroom if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/effectbybedtype.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label ///
						keep(1.treat 1.single_bedroom 1.treat#1.single_bedroom 1.treat#1.email_intervent 1.email_intervent#1.single_bedroom 1.treat#1.email_intervent#1.single_bedroom)

						
************************** EVENT STUDY REGRESSIONS
* Assess effect around time of of treatment
egen hourfromtreat = group(timefromtreat_lab_hour)

forval i = 34(1)48 {
gen hourtreat`i' = 0 if hourfromtreat!=.
replace hourtreat`i' = 1 if hourfromtreat==`i'
gen hourXtreat`i' = hourtreat`i'*treat
gen hourXintervent`i' = hourtreat`i'*email_intervent
gen treatXhourfrom`i' = treat*hourtreat`i'
gen interventXtreatXhourfrom`i' = treatXhourfrom`i'*email_intervent
}

forval i = 50(1)64 {
gen hourtreat`i' = 0 if hourfromtreat!=.
replace hourtreat`i' = 1 if hourfromtreat==`i'
gen hourXtreat`i' = hourtreat`i'*treat
gen hourXintervent`i' = hourtreat`i'*email_intervent
gen treatXhourfrom`i' = treat*hourtreat`i'
gen interventXtreatXhourfrom`i' = treatXhourfrom`i'*email_intervent
}

gen interventXtreat = email_intervent*treat

reg CTLSTPT i.interventXtreatXhourfrom* i.hourXintervent* i.treatXhourfrom* i.interventXtreat i.hourtreat* i.email_intervent i.treat, vce(cluster roomCluster)
est sto hourfomrtreat

coefplot hourfomrtreat , vertical keep(1.interventXtreatXhourfrom*) ///
	xlab(1 "-15" 3 "-13" 5 "-11" 7 "-9" 9 "-7" 11 "-5" 13 "-3" 15 "-1" ///
	16 "1" 18 "3" 20 "5" 22 "7" 24 "9" 26 "11" 28 "13" 30 "15", labsize(small)) ///
	xtitle("Hours Since Wednesday 5pm") ytitle("Treatment Effects") xline(15.5) graphregion(color(white)) bgcolor(white)
graph export "./Results/effects_aroundtreat.png", replace

			
********************************************************************************
********************************************************************************
********************************************************************************
************************* WINTER BREAK TREATMENT EFFECTS ***********************
* dec 1st: 1827705600000
* dec 5th: 1828051200000
* dec 15th: 1828915200000
* dec 25th: 1829864700000
* dec 31st: 1830383100000

** Setpoints by date
reg CTLSTPT ibn.date if winterbreak_treat==1 & roomtype!=. & statdatetime>=1827705600000 & statdatetime<=1833061500000, nocons vce(cluster room)		
est sto wbrkstptsbydate_treat
reg CTLSTPT ibn.date if winterbreak_treat==0 & roomtype!=. & statdatetime>=1827705600000 & statdatetime<=1833061500000, nocons vce(cluster room)		
est sto wbrkstptsbydate_control

* difference between treat and control
gen winttreatXdate = winterbreak_treat*date

reghdfe CTLSTPT ib0.winttreatXdate if statdatetime>=1827705600000 & statdatetime<=1833061500000 ///
			& roomtype!=. , nocons vce(cluster room) absorb(ibn.date)	
est sto wbrkstptsbydate_diff

coefplot wbrkstptsbydate_control (wbrkstptsbydate_treat , msymbol(Th)) ///
					(wbrkstptsbydate_diff,  color(black) msymbol(none) axis(2) lpattern(dash) recast(connected) ciopts(lpattern(blank))) ///
					, vertical  xlabel(1 "12/01" 8 "12/08" 15 "12/15" 23 "12/22" 30 "12/30" ///
					38 "01/07" 46 "01/15" 53 "01/22" 61 "01/30", labsize(small)) ///
					xtitle("Date") ytitle("Temperature ({superscript:o}F)") ytitle("Temperature Difference ({superscript:o}F)", axis(2)) ///
					legend(order(2 "Control" 4 "Treated" 6 "Difference")) title("Average Setpoints by Date") ///
					rename(*.winttreatXdate = .date) keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(15) text(69.3 14.9 "1st Email", placement(w) color(red) size(small) orient(vertical) just(left)) ///
					xline(18) text(69.3 17.9 "2nd Email", placement(w) color(red) size(small) orient(vertical) just(left)) ///
					xline(20) text(69.3 19.9 "Last Email", placement(w) color(red) size(small) orient(vertical) just(left)) ///
					xline(22) text(69.3 21.9 "Exams End", placement(W) color(red) size(small) orient(vertical) just(left)) ///
					xline(47) text(70 47.1 "Instruction Resumes", placement(E) color(red) size(small) orient(vertical) just(left)) ///
					note("Note: 95% confidence intervals based on room-level clustered standard errors.")
graph export "./Results/wintbreak_persistence.png", replace

sum hourlydrybulbtempf if statdatetime>=1827705600000 & statdatetime<=1833061500000, detail

* Treatment effect of the winter break intervention
label var post_winteremail "Post Dec. 15"
label var winterbreak_treat "Treated"

reg CTLSTPT i.winterbreak_treat if post_winteremail==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.winterbreak_treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if post_winteremail==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.winterbreak_treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if post_winteremail==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.winterbreak_treat if post_winteremail==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if post_winteremail==0
by room : egen avg_stpt_prewinter = min(aux)
drop aux

reghdfe CTLSTPT i.winterbreak_treat avg_stpt_prewinter if post_winteremail==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.winterbreak_treat#i.post_winteremail if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using  "./Results/winterbreak_diffdiff.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.winterbreak_treat 1.winterbreak_treat#1.post_winteremail)




********************************************************************************
************ number of thermostat changes by room by week - appendix
clear all
use "data_anonym.dta"

tsset room RoundDateTime, clocktime delta(15 minutes)
gen therm_change = 1 if CTLSTPT!=L.CTLSTPT & CTLSTPT!=. & L.CTLSTPT!=. & roomtype!=.

** variable to help identify rooms with complete week of observations
sort room Week
gen obs_roomweek = 0
by room Week : replace obs_roomweek = 1 if CTLSTPT!=. & roomtype!=.
by room Week : egen obsroomweek = total(obs_roomweek) 
drop obs_roomweek

** number of times thermostat changes by room by date
sort room date
by room date : egen thermchange = total(therm_change)

** number of times thermostat changes by room by week of sample
sort room Week
by room Week : egen thermchange_week = total(therm_change)

keep Week room thermchange_week treat obsroomweek
duplicates drop

encode(Week), gen(week)

keep if obsroomweek>=665 & obsroomweek<=672
keep if treat!=.


** regressions for average thermostat changes per week
* fall 2017
reg thermchange_week ib69.week if week>=57 & week<=72 & treat==0, vce(cluster room) nocons
parmest, saving("./Results/thermchange_week_notreat_fall2017.dta", replace)
reg thermchange_week ib69.week if week>=57 & week<=72 & treat==1, vce(cluster room) nocons
parmest, saving("./Results/thermchange_week_treat_fall2017.dta", replace)

* fall 2018
reg thermchange_week ib17.week if week>=5 & week<=20  & treat==0, vce(cluster room) nocons
parmest, saving("./Results/thermchange_week_notreat_fall2016.dta", replace)
reg thermchange_week ib17.week if week>=5 & week<=20  & treat==1, vce(cluster room) nocons
parmest, saving("./Results/thermchange_week_treat_fall2016.dta", replace)


***** plotting results - fall 2017
clear all
use "./Results/thermchange_week_notreat_fall2017.dta"

ge week = _n

rename estimate thermchange_week_mean_notreat
rename max95 thermchange_week_upper_notreat
rename min95 thermchange_week_lower_notreat

merge 1:1 parm using "./Results/thermchange_week_treat_fall2017.dta"

rename estimate thermchange_week_mean_treat
rename max95 thermchange_week_upper_treat
rename min95 thermchange_week_lower_treat


twoway (connected thermchange_week_mean_treat week, sort color(red) msymbol(Th)) ///
	(line thermchange_week_upper_treat week, sort color(red) lpattern(dash)) ///
	(line thermchange_week_lower_treat week, sort color(red) lpattern(dash)) ///
	(connected thermchange_week_mean_notreat week, sort color(blue) msymbol(o)) ///
	(line thermchange_week_upper_notreat week, sort color(blue) lpattern(dash)) ///
	(line thermchange_week_lower_notreat week, sort color(blue) lpattern(dash)) ///
	, xtitle("Week of Sample (Fall 2017)") graphregion(color(white)) bgcolor(white) ///
	xlabel(1(1)16) ytitle("Avg. Number of" "Thermostat Changes per Room") ///
	legend(order(1 "Treated" 4 "Control" )) ///
	ylabel(0(2)24) xline(2.5) xline(13) ///
	text(18 2.45 "Treatment Starts", placement(w) color(red) size(small) orient(vertical) just(left)) ///
	text(18 12.95 "Thanksgiving Break", placement(w) color(red) size(small) orient(vertical) just(left)) ///
	note("Note: 95% confidence intervals based on room-level clustered standard errors.")
graph export "./Results/avg_themrchange_weekly2.png", replace


***** plotting results - fall 2016
clear all
use "./Results/thermchange_week_notreat_fall2016.dta"

ge week = _n

rename estimate thermchange_week_mean_notreat
rename max95 thermchange_week_upper_notreat
rename min95 thermchange_week_lower_notreat

merge 1:1 parm using "./Results/thermchange_week_treat_fall2016.dta"

rename estimate thermchange_week_mean_treat
rename max95 thermchange_week_upper_treat
rename min95 thermchange_week_lower_treat


twoway (connected thermchange_week_mean_treat week, sort color(red) msymbol(Th)) ///
	(line thermchange_week_upper_treat week, sort color(red) lpattern(dash)) ///
	(line thermchange_week_lower_treat week, sort color(red) lpattern(dash)) ///
	(connected thermchange_week_mean_notreat week, sort color(blue) msymbol(o)) ///
	(line thermchange_week_upper_notreat week, sort color(blue) lpattern(dash)) ///
	(line thermchange_week_lower_notreat week, sort color(blue) lpattern(dash)) ///
	, xtitle("Week of Prior Year Sample (Fall 2016)") graphregion(color(white)) bgcolor(white) ///
	xlabel(1(1)16) ytitle("Avg. Number of" "Thermostat Changes per Room") ///
	legend(order(1 "Treated" 4 "Control" )) ///
	ylabel(0(2)14) xline(2.5) xline(13) ///
	text(2.5 2.45 "(Proxy) Treatment", placement(w) color(red) size(small) orient(vertical) just(left)) ///
	text(11 12.95 "Thanksgiving Break", placement(w) color(red) size(small) orient(vertical) just(left)) ///
	note("Note: 95% confidence intervals based on room-level clustered standard errors.")
graph export "./Results/avg_themrchange_weekly2016.png", replace


********************************************************************************
********************************************************************************
********************************************************************************
************ Testing for spillover effects *************
clear all
use "data_anonym.dta"


** compare average setpoint in fall 2016 versus fall 2017
gen fall2016_restricted = 1 if RoundDateTime>=1787961600000 & RoundDateTime<=1796773500000
gen fall2017_restricted = 1 if RoundDateTime>=1819497600000 & RoundDateTime<=1828309500000

sort date
by date : gen fall2016date = 1 if fall2016_restricted==1 & _n==1
replace fall2016date = sum(fall2016date)
replace fall2016date = . if fall2016_restricted==.
by date : gen fall2017date = 1 if fall2017_restricted==1 & _n==1
replace fall2017date = sum(fall2017date)
replace fall2017date = . if fall2017_restricted==.

gen double statdate = date
sort room date
by room date : gen start_room = 1 if statdate==20689 & obsroomdate>90 & _n==1
by room : egen starting_room = max(start_room)


set matsize 10000
** Setpoints by date - control
* significant data losses in dates: 47, 48, 49, 52, 61, 62, 63, 72, 73, 74, 91
reg CTLSTPT ibn.fall2016date if fall2016_restricted==1 & starting_room==1 ///
			& treat==0 & roomtype!=. ///
			& fall2016date!=47 & fall2016date!=48 & fall2016date!=49 ///
			& fall2016date!=52 & fall2016date!=61 & fall2016date!=62 & fall2016date!=63 ///
			& fall2016date!=72 & fall2016date!=73 & fall2016date!=74 & fall2016date!=91 ///
			, nocons vce(cluster room)		
est sto stptbydate_control_2016

reg CTLSTPT ibn.fall2017date if fall2017_restricted==1 & starting_room==1 ///
			& treat==0 & roomtype!=. ///
			& fall2017date!=47 & fall2017date!=48 & fall2017date!=49 ///
			& fall2017date!=52 & fall2017date!=61 & fall2017date!=62 & fall2017date!=63 ///
			& fall2017date!=72 & fall2017date!=73 & fall2017date!=74 & fall2017date!=91 ///
			, nocons vce(cluster room)		
est sto stptbydate_control_2017

coefplot (stptbydate_control_2016) (stptbydate_control_2017, msymbol(Th)), vertical  ///
					xlabel(1 "08/28" 32 "09/27" 57 "10/26" 82 "11/28", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") ///
					legend(order(2 "Fall 2016" 4 "Fall 2017")) title("Average Setpoints by Date - Control Rooms") ///
					rename(*.fall2016date = .fall2017date) keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(18) text(73.5 18 "Treatment", placement(w) color(red) size(small) orient(vertical) just(left)) ///
					xline(72.5) text(73.5 73 "Break", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
					xline(80.5)
graph export "./Results/setpbydate_fall1617_control.png", replace


set matsize 10000
** Setpoints by date - treated
* significant data losses in dates: 47, 48, 49, 52, 61, 62, 63, 72, 73, 74, 91
reg CTLSTPT ibn.fall2016date if fall2016_restricted==1 & starting_room==1 ///
			& treat==1 & roomtype!=. ///
			& fall2016date!=47 & fall2016date!=48 & fall2016date!=49 ///
			& fall2016date!=52 & fall2016date!=61 & fall2016date!=62 & fall2016date!=63 ///
			& fall2016date!=72 & fall2016date!=73 & fall2016date!=74 & fall2016date!=91 ///
			, nocons vce(cluster room)		
est sto stptbydate_treated_2016

reg CTLSTPT ibn.fall2017date if fall2017_restricted==1 & starting_room==1 ///
			& treat==1 & roomtype!=. ///
			& fall2017date!=47 & fall2017date!=48 & fall2017date!=49 ///
			& fall2017date!=52 & fall2017date!=61 & fall2017date!=62 & fall2017date!=63 ///
			& fall2017date!=72 & fall2017date!=73 & fall2017date!=74 & fall2017date!=91 ///
			, nocons vce(cluster room)		
est sto stptbydate_treated_2017

coefplot stptbydate_treated_2016 (stptbydate_treated_2017, msymbol(Th)), vertical  ///
					xlabel(1 "08/28" 32 "09/27" 57 "10/26" 82 "11/28", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") ///
					legend(order(2 "Fall 2016" 4 "Fall 2017")) title("Average Setpoints by Date - Treated Rooms") ///
					rename(*.fall2016date = .fall2017date) keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(18) text(73.5 18 "Treatment", placement(w) color(red) size(small) orient(vertical) just(left)) ///
					xline(73) text(70.5 73 "Thanksgiving", placement(E) color(red) size(small) orient(vertical) just(left))
graph export "./Results/setpbydate_fall1617_treated.png", replace


** Spilovers: DID using proxy treatment date for Fall 2016
gen proxy_treat = 0 if fall2017date<=16 | fall2016date<=16
replace proxy_treat = 1 if (fall2017date>16 & fall2017date!=.) | (fall2016date>16 & fall2016date!=.)

* regression to check if control rooms are behave differently in fall 2016 versu 2017, pre and post-treatment
reghdfe CTLSTPT fall1617##proxy_treat if roomtype!=. & treat==0, absorb(room) vce(cluster roomCluster)
est sto sillover_reg
sum CTLSTPT if e(sample)

label define treatdate 1 "Post Sep. 13"
label define fall17 1 "Fall 2017"
label values proxy_treat treatdate
label values fall1617 fall17

esttab sillover_reg using "./Results/spilloverreg.tex", ///
				replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
				label



********************************************************************************
*** heterogeneity using quintiles of pre-treat setting
clear all
use "data_anonym.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0

* mark first pre-treatment observation of each room
gen aux = 1
by room : gen roomobs = sum(aux) if email_intervent==0 & CTLSTPT!=. & roomtype!=.
replace roomobs = . if roomobs!=1

* quintiles of room average usage pre-treatment setpoint
xtile stpt_quintile_pretreat = avg_stpt_pretreat if roomobs==1, nq(5)
by room : egen stpt_quintile = max(stpt_quintile_pretreat)
drop stpt_quintile_pretreat
rename stpt_quintile stpt_quintile_pretreat

* Generate interactions of treated X quintile X post-treat
tabulate stpt_quintile_pretreat, gen(stpt_quintile)

forval i = 1/5 {
gen stpt_quintile_treat`i' = treat*email_intervent*stpt_quintile`i'
}

forval i = 1/5 {
gen stpt_quintile_post`i' = email_intervent*stpt_quintile`i'
}

gen treatXpost = treat*email_intervent

***** absolute difference in temperature as outcome
gen outdiff_abs = abs(outdiff)

reghdfe outdiff_abs i.stpt_quintile_treat2 i.stpt_quintile_treat3 i.stpt_quintile_treat4 i.stpt_quintile_treat5 ///
		i.stpt_quintile_post* ///
		i.treatXpost ///
		if roomtype!=. & email_intervent!=., ///
		absorb(i.room i.RoundDateTime) ///
		vce(cluster roomCluster)
est sto stpt_quintile_het


lincomest 1.treatXpost
est sto _1
* Coefficients for each quintile
foreach i in 2 3 4 5 {
est restore stpt_quintile_het
lincomest 1.stpt_quintile_treat`i' + 1.treatXpost
est sto _`i'
}

coefplot _1 || _2 || _3 || _4 || _5 , vertical bycoefs ///
		ytitle("Treatment Effect (in degrees F)") xtitle("Quintile of Baseline Thermostat Setpoint")	///
		xlabel(1(1)5) ylabel(-1.2(0.4)1.2, labsize(vsmall)) ///
		graphregion(color(white)) bgcolor(white) yline(0)
graph export "./Results/treateffect_quintile_outdiff.png", replace



*************** deciles instad of quintiles
* deciles of room average usage pre-treatment setpoint
xtile stpt_decile_pretreat = avg_stpt_pretreat if roomobs==1, nq(10)
by room : egen stpt_decile = max(stpt_decile_pretreat)
drop stpt_decile_pretreat
rename stpt_decile stpt_decile_pretreat

* Generate interactions of treated X decile X post-treat
tabulate stpt_decile_pretreat, gen(stpt_decile)

forval i = 1/10 {
gen stpt_decile_treat`i' = treat*email_intervent*stpt_decile`i'
}

forval i = 1/10 {
gen stpt_decile_post`i' = email_intervent*stpt_decile`i'
}

** with absolute difference as outcome
reghdfe outdiff_abs i.stpt_decile_treat2 i.stpt_decile_treat3 i.stpt_decile_treat4 i.stpt_decile_treat5 i.stpt_decile_treat6 ///
		i.stpt_decile_treat7 i.stpt_decile_treat8 i.stpt_decile_treat9 i.stpt_decile_treat10 ///
		i.treatXpost i.stpt_decile_post* ///
		if roomtype!=. & email_intervent!=., ///
		absorb(i.room i.RoundDateTime) ///
		vce(cluster roomCluster)
est sto stpt_decile_het


lincomest 1.treatXpost
est sto _1
* Coefficients for each decile
foreach i in 2 3 4 5 6 7 8 9 10 {
est restore stpt_decile_het
lincomest 1.stpt_decile_treat`i' + 1.treatXpost
est sto _`i'
}

coefplot _1 || _2 || _3 || _4 || _5 || _6 || _7 || _8 || _9 || _10 , vertical bycoefs ///
		ytitle("Treatment Effect (in degrees F)") xtitle("Decile of Baseline Thermostat Setpoint")	///
		xlabel(1(1)10) ylabel(-2(0.4)2, labsize(vsmall)) ///
		graphregion(color(white)) bgcolor(white) yline(0)
graph export "./Results/treateffect_decile_outdiff.png", replace




********************************************************************************
******* Treatment Effects for Spring - simple emails
clear all
use "data_anonym.dta"

label define treatlab2 1 "Spring Treatment"
label values simpletreat_spring treatlab2
label define posttreatlabel2 1 "Post Jan. 31"
label values email_intervent_spring posttreatlabel2

reg CTLSTPT i.simpletreat_spring if email_intervent_spring==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.simpletreat_spring i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_spring==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.simpletreat_spring hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent_spring==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.simpletreat_spring if email_intervent_spring==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if email_intervent_spring==0
by room : egen avg_stpt_prespring = min(aux)
drop aux

reghdfe CTLSTPT i.simpletreat_spring avg_stpt_prespring if email_intervent_spring==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.simpletreat_spring#i.email_intervent_spring if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect_simplespring.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label ///
			keep(1.simpletreat_spring 1.simpletreat_spring#1.email_intervent_spring)

			
********************************************************************************
******* Treatment Effects for Spring
label values individtreat_spring treatlab

** Simple regs on treatment
reg CTLSTPT i.individtreat_spring if email_intervent_spring!=. & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.individtreat_spring i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_spring!=. & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.individtreat_spring if email_intervent_spring!=. & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.date) vce(cluster roomCluster)
est sto teffect_date

reghdfe CTLSTPT i.individtreat_spring if email_intervent_spring!=. & roomtype!=. , absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Diff-in diff-estimates
reghdfe CTLSTPT i.individtreat_spring#i.email_intervent_spring if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff


esttab teffect teffect_phys teffect_date teffect_full teffect_diffdiff using "./Results/treat_effect_individspring.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label ///
			keep(1.individtreat_spring 1.individtreat_spring#1.email_intervent_spring)
			
			
			
			
********************************************************************************
********************************************************************************
*************************** SURVEYS

*** pre-treament survey
clear all
use "./pre_treat_survey.dta"

/* Total NEP score: ranges from 15 to 75
Values above 45 indicate high environmental concern. */
foreach x in 2 4 6 8 10 12 14 {
gen reverse_`x' = 1 if Q`x'==5
replace reverse_`x' = 2 if Q`x'==4
replace reverse_`x' = 3 if Q`x'==3
replace reverse_`x' = 4 if Q`x'==2
replace reverse_`x' = 5 if Q`x'==1
}

egen NEP_score = rowtotal(Q1 Q3 Q5 Q7 Q9 Q11 Q13 Q15 reverse*)


* histograms of responses
foreach x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 {
sum Q`x'
sca mean = r(mean)
histogram Q`x', discrete percent xline(`=round(scalar(mean), 0.01)') ///
		xlabel(1 "Strongly Disagree" 2 "Mildly Disagree" 3 "Unsure" 4 "Mildly Agree" 5 "Strongly Agree", labcolor(black) angle(45) labsize(small)) ///
		xlabel(`=round(scalar(mean), 0.01)', add custom labcolor(red) labsize(vsmall)) ///
		xscale(range(1(1)5)) start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''", size(small)) xtitle("")
graph export "./Results/NEP_hist_Q`x'.png", replace
}

* average and histogram of NEP score
sum NEP_score
sca mean = r(mean)
histogram NEP_score, start(35) discrete percent xline(`=round(scalar(mean), 0.1)') ///
		xlabel(35(2)69, add labsize(small) labcolor(black)) ///
		text(7 55 "Avg. = `=round(scalar(mean), 0.1)'", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		xtitle("NEP Score")
graph export "./Results/NEPscore_hist.png", replace


** Merge with treatment/randomization assignment for fall
merge m:1 room using "room_randomization.dta"
keep if _merge==3
drop _merge

gen treat = 1 if pure_control==0
replace treat = 0 if pure_control==1
			
* Histogram separating treatment and control	
sum NEP_score if treat==0
sca mean_notreat = r(mean)
sum NEP_score if treat==1
sca mean_treat = r(mean)

label define treatment 0 "Control" 1 "Treated"
label values treat treatment

local mean_notreat : di %2.1f scalar(mean_notreat)
local mean_treat : di %2.1f scalar(mean_treat)
byhist NEP_score, by(treat) start(35) percent discrete tw1(xline(`=round(scalar(mean_notreat), 0.1)', lcolor(blue)) ///
		xlabel(35(2)69, add labsize(small) labcolor(black)) bcolor(blue%30) ///
		text(13 57.2 "Avg. = `mean_notreat'", placement(e) color(blue) size(small) orient(vertical) just(left))) ///
		tw2(text(13 53.5 "Avg. = `mean_treat'", placement(e) color(red) size(small) orient(vertical) just(left)) ///
		bcolor(red%30) ///
		xline(`=round(scalar(mean_treat), 0.1)', lcolor(red)) ///
		graphregion(color(white)) bgcolor(white))
graph export "./Results/NEPscore_hist_bytreat.png", replace


*** Heterogeneity of treatment effects by NEP score
* will aggreagate information by room
drop treat NEP_score

sort room
forval i = 1(2)15 {
by room : egen mean_Q`i' = mean(Q`i')
}
forval i = 2(2)14 {
by room : egen mean_Q`i' = mean(reverse_`i')
}
drop Q1-reverse_14
duplicates drop

egen NEP_score = rowtotal(mean*)

**** variables indicating level of environmental concern
* will get the top 
sum NEP_score, detail
sca NEP_75 = r(p75)
gen NEP_high = NEP_score>NEP_75

* merge with the thermosta data
merge 1:m room using "data_anonym.dta", nogen keep(3)

* regressions for NEP score in levels
reg CTLSTPT 1.treat##c.NEP_score if email_intervent==1, vce(cluster roomCluster)
est sto simple_lev

reghdfe CTLSTPT 1.treat##c.NEP_score if email_intervent==1, absorb(RoundDateTime) vce(cluster roomCluster)
est sto moreconts_lev

reghdfe CTLSTPT 1.treat##1.email_intervent##c.NEP_score, absorb(room RoundDateTime) vce(cluster roomCluster)
est sto full_lev

esttab simple_lev moreconts_lev full_lev using "./Results/treateffect_bynep.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)

			
* regressions with indicator for high NEP
reg CTLSTPT i.treat##i.NEP_high if email_intervent==1, vce(cluster roomCluster)
est sto simple_ind

reghdfe CTLSTPT i.treat##i.NEP_high if email_intervent==1, absorb(RoundDateTime) vce(cluster roomCluster)
est sto moreconts_ind

reghdfe CTLSTPT i.treat##i.NEP_high##email_intervent, absorb(room RoundDateTime) vce(cluster roomCluster)
est sto full_ind

esttab simple_ind moreconts_ind full_ind using "./Results/treateffect_bynep_ind.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)


*********************************
****** Post-treatment survey
clear all
use "./post_treat_survey.dta"

***** Histograms
forval x = 2/17 {
sum Q`x'
sca mean = r(mean)
histogram Q`x', discrete percent xline(`=round(scalar(mean), 0.01)') ///
		xlabel(1 "Never" 2 "Rarely" 3 "Sometimes" 4 "Often" 5 "All The Time", labcolor(black) angle(45)) ///
		text(25 `=round(scalar(mean), 0.01)+0.05' "Avg. = `=round(scalar(mean), 0.01)'", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
		xscale(range(1(1)5)) start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''") xtitle("")
graph export "./Results/PostSurvey_hist_Q`x'.png", replace width(2500)
}

forval x = 18/24 {
sum Q`x'
sca mean = r(mean)
histogram Q`x', discrete percent xline(`=round(scalar(mean), 0.01)') ///
		xlabel(1 "Not at All Informative" 2 "Slightly Informative" 3 "Moderately Informative" 4 "Very Informative" 5 "Extremely Informative", labcolor(black) angle(45)) ///
		text(25 `=round(scalar(mean), 0.01)+0.05' "Avg. = `=round(scalar(mean), 0.01)'", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
		xscale(range(1(1)5))start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''") xtitle("")
graph export "./Results/PostSurvey_hist_Q`x'.png", replace width(2500)
}

forval x = 25/31 {
sum Q`x'
sca mean = r(mean)
local means : di %6.2f scalar(mean)
histogram Q`x', discrete percent xline(`means') ///
		xlabel(1 "Very Disappointed" 2 "Moderately Disappointed" 3 "Slightly Disappointed" 4 "Neither Disappointed nor Pleased" 5 "Slightly Pleased" 6 "Moderately Pleased" 7 "Very Pleased", labcolor(black) angle(45) labsize(vsmall)) ///
		text(25 `=round(scalar(mean), 0.01)+0.05' "Avg. = `=round(scalar(mean), 0.01)'", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
		xscale(range(1(1)5)) start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''") xtitle("")
graph export "./Results/PostSurvey_hist_Q`x'.png", replace width(2500)
}


********************************************************************************
****** Simulating randomizations to check balance
clear all
use "data_anonym.dta"

tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)

set seed 12345
sort roomCluster
by roomCluster : gen suite_obs = 1 if _n==1

** For Fall
matrix drop _all
matrix input balancemat = (.,.,.,.,.,.,.,.,.)
forval i = 1/100 {
	by roomCluster : gen rand`i' = runiform() if _n==1
	by roomCluster : egen random`i' = max(rand`i')
	gen treat_sim`i'_A = 0 if random`i'<=0.3333333
	gen treat_sim`i'_B = 0 if random`i'<=0.3333333
	replace treat_sim`i'_A = 1 if random`i'>0.3333333 & random`i'<=0.6666666
	replace treat_sim`i'_B = 1 if random`i'>0.6666666
	
	count if treat_sim`i'_A==1 & suite_obs==1
	sca obsA = r(N)
	count if treat_sim`i'_B==1 & suite_obs==1
	sca obsB = r(N)
	count if treat_sim`i'_B==0 & suite_obs==1
	sca obscont = r(N)
	sca iter =`i'
	
	reg CTLSTPT i.treat_sim`i'_A if email_intervent==0 , vce(cluster roomCluster)
	matrix coefs = r(table)
	sca rend1 = coefs[1,3]
	sca rend2 = coefs[1,2] + coefs[1,3]
	sca renp1 = coefs[4,2]
	reg CTLSTPT i.treat_sim`i'_B if email_intervent==0 , vce(cluster roomCluster)
	matrix coefs = r(table)
	sca rend3 = coefs[1,2] + coefs[1,3]
	sca renp2 = coefs[4,2]
	matrix ren`i' = (iter, rend1, rend2, renp1, rend3, renp2, obscont, obsA, obsB)
	matrix balancemat = (balancemat \ ren`i')
	drop rand`i' random`i' treat_sim`i'_A treat_sim`i'_B
}
putexcel set "./Results/balance_simulations.xlsx", modify sheet("fall")
putexcel C3 = matrix(balancemat)

** for winter break
set seed 12345
sort room
by room : gen room_obs = 1 if _n==1
gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

matrix drop _all
matrix input balancemat = (.,.,.,.,.,.)
forval i = 1/100 {
	by room : gen rand`i' = runiform() if _n==1 & winterbreak_treat!=.
	by room : egen random`i' = max(rand`i')
	gen treat_sim`i'_A = 0 if random`i'<=0.5 & winterbreak_treat!=.
	replace treat_sim`i'_A = 1 if random`i'>0.5 & winterbreak_treat!=.
	
	count if treat_sim`i'_A==1 & room_obs==1
	sca obsA = r(N)
	count if treat_sim`i'_A==0 & room_obs==1
	sca obscont = r(N)
	sca iter =`i'

	reg CTLSTPT i.treat_sim`i'_A if post_winteremail==0 , vce(cluster room)
	matrix coefs = r(table)
	sca rend1 = coefs[1,3]
	sca rend2 = coefs[1,2] + coefs[1,3]
	sca renp1 = coefs[4,2]
	matrix ren`i' = (iter, rend1, rend2, renp1, obscont, obsA)
	matrix balancemat = (balancemat \ ren`i')
	drop rand`i' random`i' treat_sim`i'_A
}
putexcel set "./Results/balance_simulations.xlsx", modify sheet("winter")
putexcel C3 = matrix(balancemat)

** For Spring
matrix drop _all
matrix input balancemat = (.,.,.,.,.,.,.,.,.)
forval i = 1/100 {
	by room : gen rand`i' = runiform() if _n==1 & treat_spring!=.
	by room : egen random`i' = max(rand`i')
	gen treat_sim`i'_A = 0 if random`i'<=0.3333333 & treat_spring!=.
	gen treat_sim`i'_B = 0 if random`i'<=0.3333333 & treat_spring!=.
	replace treat_sim`i'_A = 1 if random`i'>0.3333333 & random`i'<=0.6666666 & treat_spring!=.
	replace treat_sim`i'_B = 1 if random`i'>0.6666666 & treat_spring!=.
	
	count if treat_sim`i'_A==1 & room_obs==1
	sca obsA = r(N)
	count if treat_sim`i'_B==1 & room_obs==1
	sca obsB = r(N)
	count if treat_sim`i'_B==0 & room_obs==1
	sca obscont = r(N)
	sca iter =`i'
	
	reg CTLSTPT i.treat_sim`i'_A if email_intervent_spring==0 , vce(cluster room)
	matrix coefs = r(table)
	sca rend1 = coefs[1,3]
	sca rend2 = coefs[1,2] + coefs[1,3]
	sca renp1 = coefs[4,2]
	reg CTLSTPT i.treat_sim`i'_B if email_intervent_spring==0 , vce(cluster room)
	matrix coefs = r(table)
	sca rend3 = coefs[1,2] + coefs[1,3]
	sca renp2 = coefs[4,2]
	matrix ren`i' = (iter, rend1, rend2, renp1, rend3, renp2, obscont, obsA, obsB)
	matrix balancemat = (balancemat \ ren`i')
	drop rand`i' random`i' treat_sim`i'_A treat_sim`i'_B
}
putexcel set "./Results/balance_simulations.xlsx", modify sheet("spring")
putexcel C3 = matrix(balancemat)


********************************************************************************
********************************************************************************
*** Regression discontinuity, using distance from usage cutoff
**** Distance from cutoff, above cutoff and interactions
tsset room RoundDateTime, clocktime delta(15 minutes)
gen dist_avg = L672.Usage - L672.AllNeighbors
*sum dist_avg, detail
gen distXbad = dist_avg*L672.bad

gen dist_eff = L672.Usage - L672.EfficientNeighbors
*sum dist_eff, detail
gen distXgood = dist_eff*L672.good

gen dist_avgXtreat = dist_avg*treat
gen distXbadXtreat = distXbad*treat
gen dist_effXtreat = dist_eff*treat
gen distXgoodXtreat = distXgood*treat
**********************************************************
**** generate weights for triangular kernel
**** (manually enter different bandwidths when estimating 
**** the bandwidth robustness tests)
**** bw_l = bandwidth below threshold
**** bw_u = bandwidth above threshold
**********************************************************
gen u=.
gen v=.
g bw_l = 0.2
g bw_u = 0.2

replace u=dist_avg/bw_l if dist_avg<0
replace u=dist_avg/bw_u if dist_avg>0
gen weight_avg=1-abs(u)
replace weight_avg=. if dist_avg<-bw_l
replace weight_avg=. if dist_avg>bw_u

replace v=dist_eff/bw_l if dist_eff<0
replace v=dist_eff/bw_u if dist_eff>0
gen weight_eff=1-abs(v)
replace weight_eff=. if dist_eff<-bw_l
replace weight_eff=. if dist_eff>bw_u

gen treatXbad = treat*lag_bad
gen treatXgood = treat*lag_good

*** regressions using the RD weights
reg CTLSTPT i.treat dist_avg i.lag_bad i.treatXbad distXbad dist_avgXtreat distXbadXtreat [aweight = weight_avg] if email_intervent!=. & roomtype!=., vce(cluster roomCluster)
est sto teffect_RD_avg
reg CTLSTPT i.treat dist_eff i.lag_good i.treatXgood distXgood dist_effXtreat distXgoodXtreat [aweight = weight_eff] if email_intervent!=. & roomtype!=., vce(cluster roomCluster)
est sto teffect_RD_eff

esttab teffect_RD_avg using "./Results/treat_RD_avg.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label
			
esttab teffect_RD_eff using "./Results/treat_RD_eff.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label
			
** Graphical analysis
gen dist_avg_bins = .
forval i = -0.5(0.025)-0.025 {
replace dist_avg_bins = `i' if dist_avg >`i'-0.025 & dist_avg<=`i'
}
forval i = 0.025(0.025)0.6 {
replace dist_avg_bins = `i' if dist_avg >=`i' & dist_avg<`i'+0.025
}
replace dist_avg_bins = -0.525 if dist_avg<-0.525
replace dist_avg_bins = 0.625 if dist_avg>=0.625 & dist_avg!=.

gen dist_eff_bins = .
forval i = -0.5(0.025)-0.025 {
replace dist_eff_bins = `i' if dist_eff >`i'-0.025 & dist_eff<=`i'
}
forval i = 0.025(0.025)0.6 {
replace dist_eff_bins = `i' if dist_eff >=`i' & dist_eff<`i'+0.025
}
replace dist_eff_bins = -0.525 if dist_eff<-0.525
replace dist_eff_bins = 0.625 if dist_eff>=0.625 & dist_eff!=.

sort dist_avg_bins
by dist_avg_bins : egen ctlstpt_avg = mean(CTLSTPT) if (email_intervent==1 & treat==1 & roomtype!=.) & (lag_good==1 | lag_bad==1)

sort dist_eff_bins
by dist_eff_bins : egen ctlstpt_eff = mean(CTLSTPT) if (email_intervent==1 & treat==1 & roomtype!=.) & (lag_good==1 | lag_great==1)

twoway 	(scatter ctlstpt_avg dist_avg_bins if dist_avg<=0.55 & dist_avg>=-0.55, sort mcolor(gs8)) ///
		(lfit CTLSTPT dist_avg if lag_good==1 & dist_avg>=-0.55, fcolor(none) lcolor(red)) ///
		(lfit CTLSTPT dist_avg if lag_bad==1 & dist_avg<=0.55, fcolor(none) lcolor(red)) ///
		if (email_intervent==1 & treat==1 & roomtype!=.) ///
		, xtitle("Distance From Previous Week's Average Usage Cutoff" "(kBTU, binned)") ///
		ytitle("Thermostat Setpoint") ///
		xline(0, lcolor(black)) ///
		xlabel(-0.5(0.1)0.5) ///
		legend(order(1 2) lab(1 "Average Setpoint Within Bin") lab(2 "Linear Fit")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Results/RD_average.png", replace

			
twoway 	(scatter ctlstpt_eff dist_eff_bins if dist_eff<=0.55 & dist_eff>=-0.55, sort mcolor(gs8)) ///
		(lfit CTLSTPT dist_eff if lag_great==1 & dist_eff>=-0.55, fcolor(none) lcolor(red)) ///
		(lfit CTLSTPT dist_eff if lag_good==1 & dist_eff<=0.55, fcolor(none) lcolor(red)) ///
		if (email_intervent==1 & treat==1 & roomtype!=.) ///
		, xtitle("Distance From Previous Week's Efficient Usage Cutoff" "(kBTU, binned)") ///
		ytitle("Thermostat Setpoint") ///
		xline(0, lcolor(black)) ///
		xlabel(-0.5(0.1)0.5) ///
		legend(order(1 2) lab(1 "Average Setpoint Within Bin") lab(2 "Linear Fit")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Results/RD_efficient.png", replace			


********************************************************************************			
*** What thermostat reductions represent in energy space
/* parameters based on regressions from thermostat_energy.do, which
establishes a relationship between thermostat changes and energy usage,
based on historical data */

clear all
use "data_anonym.dta"

** simulate a 0.24 reduction in thermostats for control rooms
sort room hourofsample
by room hourofsample : egen hourly_stpt_sim = mean(CTLSTPT-0.24)

* difference between stpt and outdoor temperature
gen outdiff_sim = hourly_stpt_sim - hourlydrybulbtempf
replace outdiff_sim = ceil(outdiff_sim - 0.5)

* usage calculation for control rooms
gen steam_usage = 9.6*0.0472917*outdiff_sim if roomtype==2
replace steam_usage = 9.6*0.0378844*outdiff_sim if roomtype==1
replace steam_usage = 0 if outdiff_sim<=0

gen chw_usage = (-0.7942)*outdiff_sim if roomtype==2
replace chw_usage = (-0.9678)*outdiff_sim if roomtype==1
replace chw_usage = 0 if outdiff_sim>=0 & outdiff_sim!=.

gen total_usage = steam_usage + chw_usage

* now transform hourly usage into weekly
by room hourofsample : gen unique_hour = 1 if _n==1

sort room Week
by room Week : egen totuseweek = mean(total_usage) if unique_hour==1
by room Week : egen total_usage_weekly_sim = max(totuseweek)
replace total_usage_weekly_sim = total_usage_weekly_sim*168
replace total_usage_weekly_sim = total_usage_weekly_sim/1000

drop steam_usage chw_usage total_usage unique_hour totuseweek
gen Usage_sim = total_usage_weekly_sim if treat==0

sum Usage if email_intervent==1 & treat==0 & roomtype!=.
sca avg_usage = r(mean)
sum Usage_sim if email_intervent==1 & treat==0 & roomtype!=.
sca avg_usage_sim = r(mean)

dis (avg_usage_sim-avg_usage)/avg_usage


********************************************************************************
********************************************************************************
*********** Longer-term treatment effects
clear all
use "data_anonym.dta"

** define always treatment versus always control
gen always_treat = .
replace always_treat = 1 if treat==1 & individtreat_spring==1
replace always_treat = 0 if treat==0 & individtreat_spring==0

** dates which should be used for full year treatment
gen email_intervent_full = .
replace email_intervent_full = 0 if RoundDateTime>=1819497600000 & RoundDateTime<1820941200000
replace email_intervent_full = 1 if RoundDateTime>=1820941200000 & RoundDateTime<1839974400000 /* starting when first email was sent in fall */
replace email_intervent_full = . if RoundDateTime<1819497600000  /* first day of fall semester */
replace email_intervent_full = . if RoundDateTime>=1839974400000 /* last available date of spring semester */
replace email_intervent_full = . if RoundDateTime>=1828915200000 & RoundDateTime<1831680000000 /* dropping winter break */

** regressions comparing always treatment versus always control
label define treatlab 1 "Always Treated"
label values always_treat treatlab
label define posttreatlabel 1 "Post Sep. 13"
label values email_intervent_full posttreatlabel

** Simple regs on treatment
reg CTLSTPT i.always_treat if email_intervent_full==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.always_treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_full==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.always_treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent_full==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.always_treat if email_intervent_full==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if email_intervent_full==0
by room : egen avg_stpt_pre = min(aux)
drop aux

reghdfe CTLSTPT i.always_treat avg_stpt_pre if email_intervent_full==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.always_treat##i.email_intervent_full if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/effect_alwaystreat.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.always_treat 1.always_treat#1.email_intervent_full)
			
******* graphically
gen stable_date = 0
replace stable_date = 1 if CTLSTPT!=. & email_intervent_full!=. & roomtype!=.
sort date
by date : egen stabledate = total(stable_date) 
drop stable_date
gen statdate = date
order statdate, after(date)

/* dates with less than 20,000 available CTLSTPT observations:
       date |      Freq.     Percent        Cum.
------------+-----------------------------------
  13oct2017 |     17,805       10.35       10.35
  27oct2017 |     11,830        6.88       17.22
  06nov2017 |      8,968        5.21       22.44
  07nov2017 |      2,671        1.55       23.99
  08nov2017 |      9,741        5.66       29.65
  24nov2017 |     16,335        9.49       39.14
  16mar2018 |     15,398        8.95       48.09
  17mar2018 |     12,765        7.42       55.51
  18mar2018 |     12,768        7.42       62.93
  19mar2018 |     12,767        7.42       70.35
  20mar2018 |     13,082        7.60       77.96
  22mar2018 |     17,906       10.41       88.36
  02apr2018 |     19,826       11.52       99.89
  14apr2018 |        197        0.11      100.00
------------+-----------------------------------
      Total |    172,059      100.00
*/


** Setpoints by date - always control versus always treated
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 & statdate!=21259 & statdate!=21260 & statdate!=21261 ///
			& statdate!=21262 & statdate!=21263 & statdate!=21265 & statdate!=21276 & statdate!=21288 ///
			& always_treat==1 & email_intervent_full!=. & roomtype!=. , nocons vce(cluster room)		
est sto stptbydate_treat
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 & statdate!=21259 & statdate!=21260 & statdate!=21261 ///
			& statdate!=21262 & statdate!=21263 & statdate!=21265 & statdate!=21276 & statdate!=21288 ///
			& always_treat==0 & email_intervent_full!=. & roomtype!=. , nocons vce(cluster room)		
est sto stptbydate_control

coefplot stptbydate_control stptbydate_treat, vertical  ///
					xlabel(1 "08/28" 15 "09/11" 30 "09/26" 45 "10/11" 60 "10/28" ///
					75 "11/15" 90 "12/01" 103 "12/14" 119 "01/31" ///
					134 "02/15" 147 "02/28" 162 "03/15" 172 "03/31" 185 "04/15", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Always Control" 4 "Always Treated")) title("Average Setpoints by Date") ///
					keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(16.5) text(73.2 16.6 "Treatment Starts", placement(e) color(red) size(small) orient(vertical) just(left))
graph export "./Results/setpoints_bydate_alwaystreat.png", replace


********************************************************************************
************** Within room, across-time variance
clear all
use "data_anonym.dta"

gen outdiff_abs = abs(outdiff)

*** for Fall
sort room RoundDateTime
by room : egen auxvar = sd(CTLSTPT) if email_intervent!=. & roomtype!=.
by room : egen sd_stpt = min(auxvar)
drop auxvar
gen var_stpt = sd_stpt^2

sort room RoundDateTime
by room : egen auxvar = sd(outdiff_abs) if email_intervent!=. & roomtype!=.
by room : egen sd_outdiff = min(auxvar)
drop auxvar
gen var_outdiff = sd_outdiff^2

sort room RoundDateTime
by room : egen auxvar = sd(CTLSTPT) if email_intervent==1 & roomtype!=.
by room : egen sd_stpt2 = min(auxvar)
drop auxvar
gen var_stpt2 = sd_stpt2^2

sort room RoundDateTime
by room : egen auxvar = sd(outdiff_abs) if email_intervent==1 & roomtype!=.
by room : egen sd_outdiff2 = min(auxvar)
drop auxvar
gen var_outdiff2 = sd_outdiff2^2


*** for Winter Break
gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

by room : egen auxvar = sd(CTLSTPT) if post_winteremail!=. & roomtype!=.
by room : egen sd_stpt_winter = min(auxvar)
drop auxvar
gen var_stpt_winter = sd_stpt_winter^2

by room : egen auxvar = sd(CTLSTPT) if post_winteremail==1 & roomtype!=.
by room : egen sd_stpt_winter2 = min(auxvar)
drop auxvar
gen var_stpt_winter2 = sd_stpt_winter2^2


keep if roomtype!=. & treat!=.
keep room sd_stpt var_stpt sd_stpt_winter var_stpt_winter var_outdiff sd_outdiff sd_stpt2 var_stpt2 sd_stpt_winter2 var_stpt_winter2 var_outdiff2 sd_outdiff2
duplicates drop

sum sd_stpt sd_stpt2 sd_outdiff sd_outdiff2 sd_stpt_winter sd_stpt_winter2, detail



********************************************************************************			
*** What thermostat reductions represent in energy space - winter break
clear all
use "data_anonym.dta"

sort room hourofsample
by room hourofsample : egen hourly_stpt_sim = mean(CTLSTPT+1.1)

* difference between stpt and outdoor temperature
gen outdiff_sim = hourly_stpt_sim - hourlydrybulbtempf
replace outdiff_sim = ceil(outdiff_sim - 0.5)

* usage calculation for control rooms
gen steam_usage = 9.6*0.0472917*outdiff_sim if roomtype==2
replace steam_usage = 9.6*0.0378844*outdiff_sim if roomtype==1
replace steam_usage = 0 if outdiff_sim<=0

gen chw_usage = (-0.7942)*outdiff_sim if roomtype==2
replace chw_usage = (-0.9678)*outdiff_sim if roomtype==1
replace chw_usage = 0 if outdiff_sim>=0 & outdiff_sim!=.

gen total_usage = steam_usage + chw_usage

* now transform hourly usage into weekly
by room hourofsample : gen unique_hour = 1 if _n==1

sort room Week
by room Week : egen totuseweek = mean(total_usage) if unique_hour==1
by room Week : egen total_usage_weekly_sim = max(totuseweek)
replace total_usage_weekly_sim = total_usage_weekly_sim*168
replace total_usage_weekly_sim = total_usage_weekly_sim/1000

drop steam_usage chw_usage total_usage unique_hour totuseweek
gen Usage_sim = total_usage_weekly_sim

gen post_winteremail = 1 if RoundDateTime>=1829520000000 & RoundDateTime<=1834271100000 

sum Usage if post_winteremail==1 & winterbreak_treat==1 & roomtype!=.
sca avg_usage_treat = r(mean)
sum Usage_sim if post_winteremail==1 & winterbreak_treat==1 & roomtype!=. & Usage!=.
sca avg_usage_notreat = r(mean)

dis (avg_usage_treat-avg_usage_notreat)/avg_usage_notreat



**************************** checking if there is treat effect for rooms that completed survey
tempfile final_file
save `final_file'

clear
use "pre_treat_survey.dta"
keep room
duplicates drop

merge 1:m room using `final_file'

drop if _merge==2
rename _merge pre_survey_rooms

label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Sep. 13"
label values email_intervent posttreatlabel


** Simple regs on treatment
reg CTLSTPT i.treat if email_intervent==1 & roomtype!=. & pre_survey_rooms==3, vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. & pre_survey_rooms==3, vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. & pre_survey_rooms==3, ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.treat if email_intervent==1 & roomtype!=. & pre_survey_rooms==3, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full


*** full disaggregation of treat
reg CTLSTPT i.treat#winterbreak_treat if statdatetime>=1827705600000 & statdatetime<=1833061500000 & roomtype!=., vce(cluster roomCluster)


***************************** regressions with alternative outcome
*** outcome is set at 68
gen roundCTLSTPT = round(CTLSTPT)

gen set68 = 0 if roundCTLSTPT!=.
replace set68 = 1 if roundCTLSTPT==68

gen pre_postOct15 = 0 if email_intervent==0
replace pre_postOct15 = 1 if statdatetime>=1823644800000 & email_intervent==1

reg set68 i.treat if email_intervent==1 & roomtype!=. & statdatetime>=1823644800000, vce(cluster roomCluster)
est sto teffect

reg set68 i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & roomtype!=. & statdatetime>=1823644800000, vce(cluster roomCluster)
est sto teffect_phys

reghdfe set68 i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & roomtype!=. ///
			& statdatetime>=1823644800000, absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe set68 i.treat if email_intervent==1 & roomtype!=. & statdatetime>=1823644800000 , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if email_intervent==0
by room : egen avg_stpt_pre = min(aux)
drop aux

reghdfe set68 i.treat avg_stpt_pre if email_intervent==1 & roomtype!=.  & statdatetime>=1823644800000, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe set68 i.treat##i.pre_postOct15 if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Oct. 15"
label values pre_postOct15 posttreatlabel

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect_set68.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat 1.treat#1.pre_postOct15)



			
********************************************************************************
********************************************************************************
*********************** RANDOMIZATION INFERENCE ********************************
clear all
use "data_anonym.dta"

keep if email_intervent!=. & roomtype!=.

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0

* mark first pre-treatment observation of each room
gen aux = 1
by room : gen roomobs = sum(aux) if email_intervent==0 & CTLSTPT!=. & roomtype!=.
replace roomobs = . if roomobs!=1

* quintiles of room average usage pre-treatment setpoint
xtile stpt_quintile_pretreat = avg_stpt_pretreat if roomobs==1, nq(5)
by room : egen stpt_quintile = max(stpt_quintile_pretreat)
drop stpt_quintile_pretreat
rename stpt_quintile stpt_quintile_pretreat

* deciles of room average usage pre-treatment setpoint
xtile stpt_decile_pretreat = avg_stpt_pretreat if roomobs==1, nq(10)
by room : egen stpt_decile = max(stpt_decile_pretreat)
drop stpt_decile_pretreat
rename stpt_decile stpt_decile_pretreat


*** re-randomizing treatment by suite
sort roomCluster RoundDateTime
by roomCluster : gen suiteobs = sum(aux) if email_intervent==1 & roomtype!=.
replace suiteobs = . if suiteobs!=1

tempfile final_file
save `final_file'

keep if suiteobs==1
keep roomCluster

forval i = 1/200 {
gen rand`i' = runiform()
}

forval i = 1/200 {
gen treated_boots`i' = 0 if rand`i'<=0.33
replace treated_boots`i' = 1 if rand`i'>0.33 & rand`i'!=.
}
keep roomCluster treated_boots*

merge 1:m roomCluster using `final_file', nogen keep(3)

** testing null of no heterogeneity with observed data
gen outdiff_abs = abs(outdiff)

reghdfe outdiff_abs i.treat i.stpt_quintile_pretreat if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster) ///
			residuals(resid_full)
est sto teffect_avgpre
matrix results = r(table)
scalar ll = results[5,2]
scalar ul = results[6,2]
scalar meaneffect = results[1,2]

forval i = `=scalar(ll)'(0.05)`=scalar(ul)' {
di `i'
}

/* this method takes too long to run - infeasible given our sample size
*ksmirnov outdiff_boots1_nui1, by(treated_boots1)
*/

*** will use variance comparison method
sdtest resid_full if email_intervent==1 & roomtype!=., by(treat)
sca fstat_full = r(F)
di fstat_full
/*
fstat with true data:
1.0251118
*/

/**** now replicating over iterations and nuisance parameters
first generating the "science tables" */

forval i = 1/200 {
	local counter = 1
	forval j = `=scalar(ll)'(0.05)`=scalar(ul)' {
	di "Iteration `i' with nuisance parameter `counter' = `j'"
	
	gen outdiff_boots`i'_nui`counter' = outdiff_abs if treated_boots`i'==0 & treat==0
	replace outdiff_boots`i'_nui`counter' = outdiff_abs + `j' if treated_boots`i'==1 & treat==0
	replace outdiff_boots`i'_nui`counter' = outdiff_abs - `j' if treated_boots`i'==0 & treat==1
	replace outdiff_boots`i'_nui`counter' = outdiff_abs if treated_boots`i'==1 & treat==1
		
	local counter = `counter' + 1
	}
}

* will compare variances of distributions of simulated treated versus control groups
local counter = 1
forval j = `=scalar(ll)'(0.05)`=scalar(ul)' {
	matrix input fstat_nui`counter' = (., .)
		forval i = 1/200 {
		di "Iteration `i' with nuisance parameter `counter' = `j'"
		
		reghdfe outdiff_boots`i'_nui`counter' i.treated_boots`i' i.stpt_quintile_pretreat if email_intervent==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster) ///
			residuals(resid_boots`i')
		
		sdtest resid_boots`i' if email_intervent==1 & roomtype!=. , by(treated_boots`i')
		sca fstat = r(F)
		matrix iter = (`i', fstat)
		matrix fstat_nui`counter' = (fstat_nui`counter' \ iter)
		drop resid_boots`i'		
	}
	esttab matrix(fstat_nui`counter') using "./Results/ri_fstat_newnui`counter'.csv", replace
	local counter = `counter' + 1
}


*************** Analyze results from randomization inference
/* nuisance parameters:
-.22302033
-.17302033
-.12302033
-.07302033
-.02302033
.02697967
.07697967
.12697967
.17697967
.22697967
.27697967
*/

matrix pvals = (.)
local counter = 1

foreach x in -0.22302033 -0.17302033 -0.12302033 -0.07302033 -0.02302033 ///
	0.02697967 0.07697967 0.12697967 0.17697967 0.22697967 0.27697967 {
clear
import delimited "./Results/ri_fstat_nui`counter'.csv", delimiter(comma) rowrange(4)

drop v1
rename v2 iteration
rename v3 fstat

replace iteration = subinstr(iteration, `"""', "", .)
replace iteration = subinstr(iteration, "=", "", .)
replace fstat = subinstr(fstat, `"""', "", .)
replace fstat = subinstr(fstat, "=", "", .)

destring fstat, replace

/* probability that f-stat from real data is larger than simulated f-stats
this will be the p-value of the sharp-null hypothesis */
count
sca totsamp = `r(N)'
count if fstat>=1.0251118 & fstat!=.
sca sigsamp = `r(N)'

sca pval = sigsamp/totsamp
di "P-val for nuisance parameter `counter'=" pval 
matrix pvals = (pvals / pval)


kdensity fstat, xline(1.0251118) graphregion(color(white)) bgcolor(white) ///
	xtitle("F-stat distribution with nuisance parameter = `=round(`x', 0.01)'" "P-val = 0`=scalar(pval)'") ///
	title("") text(3 1.03 "F-stat with real data = 1.025", placement(e) color(red) size(small) orient(horizontal) just(left)) ///
	note("")
	
graph export "./Results/fstat_dist_nui`counter'.png", replace

local counter = `counter' +1
}




********************************************************************************
********************************************************************************
************************ TREATMENT INTERACTIONS ********************************

clear all
use "data_anonym.dta"

/* here we test if there are any interaction effects between Fall and Winter Break treatments */

* auxiliary variables
label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Sep. 13"
label values email_intervent posttreatlabel

gen outdiff_abs = abs(outdiff)

* some descriptives on the outdiff variable, to be added manually to result tables
*sum outdiff if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000 & roomtype!=. & treat!=.
*dis 25*24*46.15*0.454
*sum outdiff if email_intervent!=. & roomtype!=. & treat!=.
sum outdiff_abs if email_intervent!=. & roomtype!=. & treat!=.
sum outdiff_abs if email_intervent==1 & roomtype!=. & treat!=.

gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

* Treatment effect of the winter break intervention
label var post_winteremail "Post Dec. 15"
label var winterbreak_treat "Treated"


******* regressions

reg CTLSTPT i.winterbreak_treat##i.treat if post_winteremail==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.winterbreak_treat##i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if post_winteremail==1 & roomtype!=. , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.winterbreak_treat##i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if post_winteremail==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.winterbreak_treat##i.treat if post_winteremail==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if post_winteremail==0
by room : egen avg_stpt_prewinter = min(aux)
drop aux

reghdfe CTLSTPT i.winterbreak_treat##i.treat avg_stpt_prewinter if post_winteremail==1 & roomtype!=. , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.winterbreak_treat##i.post_winteremail##i.treat if roomtype!=. , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using  "./Results/treat_interactions.tex", ///
				replace nonotes b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.winterbreak_treat 1.treat 1.winterbreak_treat#1.treat ///
				1.winterbreak_treat#1.post_winteremail 1.post_winteremail#1.treat 1.winterbreak_treat#1.post_winteremail#1.treat)












***** Original Stata random number generator state used for simulations:

/*
XAAd3f1eac41d105c9d1005543932d8db3906f97a8bf546c7f111d0a1b95e22de0ef3c10925ca4b0d0059edc1abc5164d07e73
> 405500bf0cd4351ca2417ca810ff394737681dec9f98b5bbe1d2c996120616d87a7d0501705c968f3ddf6581899d83217792
> bce1d9c9f83750fcc554c57de86d0202abf57ab7abbd7fa543ce55dc7efa7238350b95a88a8c3271c4c1f7d202540e00ea84
> 4051ebb256b2df4c682cd4d25478fd240ba4d186e1dea6ad54e00b0866db23c2eec20aaed4c62f43bf8b466ef019be7c59f3
> 81c9c4988c06d867b99940a68be6c2f6e2e8e3c7a2f37980c86c0c93900a07970926eb4795f07a017af7f87f26c970589d83
> 5e46e89b0b44970c86784ffff88e101ab3879279f63233baf3d0205f38a1b430cafde80015d3a9a3f239264e5c661c1ec681
> cf4bb32ecad8d95c96c4156dc24ab7176d4360b7cff3e43e5869670381cf63795d1b22ea9058a1037ff6306e7f1d28c2c14e
> 3e1778f013a6351c56ba402acc29cc194d87e0620979af4989fa8898202db03de9114dc9b272d3ada6156e30268167d425e9
> 466252797cee8c98b4120d70d81d12bf9a9b4d3af01d8e1125c1106554505c653c7439b249ee64d07f739719a4d2118c43e1
> 0263e33c3985a4835201fc99973ca6dd487994add2297f7df997aec3b4020fe6d24bbb9e21d155ed30be77c8390c24655d38
> d90a6e15027054ff4709ab9467d18d53d60208cf7fa4c4bd04650ccc7d0dcec7c1b28d4fa69de911dde6a48147542785521f
> 75eae1f937d50509b81a4ab3c0d61e99c6e33fdeedb0b3e4d631bdc51659802500e87cf2947f1e1170b6829d482d345f37ed
> cb1f7d662820408a81bf60a47759c63f73c16004b86bbaf2806e6b7668e7fd7aaff537bb0fbea879f169a318330a83129cd9
> f9ffedb9b535bfd0873ebc15c557995cf0275449996de7bc7c2b4356b7322f0a73d3c3c210e68f3a9e92b3b1b5009ba2289e
> a02ccc8863569fe040752991768e9884e237609c8f760ff926a80993979b9f9cc112e664621209f6e57236001ce1483faf6b
> f5ee1f2174bc3f2c488e6230b2a5d080c8679a2e40a0a3c96ee5a284a22559654ae81d68c5160cb78391b2c7a2e7b0ee7d70
> 8521e3f72e17996d2ce1142dc5723a24a8ec1d6e9f0e8c7f79a23bedaacd689bbe4676d5fd53af7a7b607a3daa6f9568c262
> a4696afb2b6be0dfdd5e329bc8cfd3963639fbe52fb15caf48a317e60ce78ea4b51a95409c4411aada28101d485092ccd0f7
> f9c7f515497266b9b12543a9fe24db2f150da75d058a75beac2ea02ad16cef22d092f2e7bc1e9414ae5a9525db5599263220
> 6eb65c97793ee8d97a793c4b9c1c41e65cbaea6388d069d3a79d99ccdb4fe8e63d9f8ba585a9065247bd1fdec0bbe48821df
> 6d64a78f36deb07f8d0b773ea561cba7342cd87f6aac68ba2e4cd3348ad120946ee47922fb41d98e6ccfd838cb58831fbcb7
> 026753ba410960a29d9eb164e9b0445b0b4d79c9ae546a3ad4fa4ed4f57eb999d08f553208e83108760805be7491f5f3f384
> 824a7f6de1f67848b7d4ac074ef9317c05dfc4d3776d0da573098457f3dd7a5af9e402a6c046ed686f25ede7149c2fe9c5f8
> 3ee02c4c3317f4000792b2435fecca67c0d76087159ddd93acdd918f5f6a3b502969e05ca89cffc1d748de1caeb063658ecc
> 2381e184ca5c0adc261eb918a4b728cd19344e059abe6687253787faef7a7a8008a04d6c0bb2acffb9f66a20d0d657b4fd5f
> b8e5c2ded3b9f26f2a2cd95e860a04bfaa7bdbda57c15bec34c68eb0653f9413da289f8df061feb0ec574965f94fae7619be
> a1482e0b0488417331268dae42ed4c40947189633096162f2f12f509d7b380cc1541c713ab11cc7df7b9b9a58cd122c52d89
> c14ba22b722771838a8f21a502af2a634f89479d7c4450ebb4cfe56950c80244dd4a1d65b0564398e1abd7efc2f325ca4d87
> a2b291a42cf2fc37af872d052e0500cdb6a20eeca7b88ec68842b65bf745cb548e9b501a3684e7f05b5837701648041ea2e3
> 4f1bd5c08e3b489b0aa176319a555eaec5eb4e43f0824f026117fac248f3b5f4796b8d0b0b15d4be5858fdb4a906acb1420e
> e98956b6f05d5180ef8acd346224c8054ab81719f501f8a3629d687418aeff4d81ccdab3a002d9bad8fcca6beca701e6798e
> ea7083ade0852b1c2537ba7100715d4defe5231fdbbbe84b404cdb604e89d1a77d86c4b222c599ba878fb1233b1b0ad9fef6
> d7f95e7d2620aea90f01972f2c2e83dc1f2eae10d5c84e07d3f626b7561b89cbce11fbe767e33892f5cd4de16c4ccf2d4f41
> e9eda76af406ebfa2f25d5eb7c67a257bd42d4ab57a5b07a430f460e0be3011c794e9a39e1a7b2d636fb28d4249b4ea98072
> bca215c7678aaaef9abecd1ddb0fbeb66c93316b2e09d2b2fcfa0845badc9b1e28f187a74924ad74f664201f96be5ddb94c4
> 3689c9bc42b267890702a71a955b7af4bf66dcadd9276330f9eb7f3e696b85024b2563ec4973748cb2cb4f9b3e853cb55800
> c5a6c3348e8a0579aef99c0cc767101c6f791fc8864ae194610bd967f1a6f126533bd40836d8a18bdea76d354f3ddba415f8
> 040a42cb2869b7cb617f8ebdc638dfa189e273bc4ee9541a20d469c1462c17ad4867867c92678a282ea57cfe31a019f6dfd5
> 94345de99cd7a5725769453e3cb0f6e3b99c6bf3d56b85aafebaaf2d051f4018aabe4751eaaeabbc437c3e28b6e1776adc01
> e29c5e8086177295a6d10ee354bc1c0c1bcdcdca6d04694505d5150e938bc5c5bc577b8c0cec7a356edecd532c7080bb1de6
> 6e95acc8f0c7567e7053e6f00bc706fe8dc666cbf3f478a123c6a99f66658c61f117b6f04a942f3a4d0c02ddf9f4f061c144
> 54d757d7c6a49cfb715874d24c8597f1fab8b93a0848f31a8316ad71181b3fbee6aa7cd4423ec353faf8eb989e106b3f16a1
> f7751b0dd7816226444d5b22491151cdf8110c45a6b08363dbb3bc6afc6ef28a58116a3b73d814fa1a2c7bfa01f069d6bf2a
> b4a850645e8346ee1d21d444faad44e8db38e34f17a63a2d6a45804ccdfd3cc26c4955c760135d42594e56ffdf1446a0ff21
> 1111323c5da41bb89e394e8df8cde55e9b1e8cfc46fd50c644797178ad834910060c764982d9da610bf17619438ceb833959
> 606088c9c966ec9daf9d0bf3e96b6496e02f5eded780f9cf343cf4e1cff94fda2c9790a532f0dd76cebfcf157287eed6122f
> 646da85c3e4c0f9442455e09dbe5a1d4db4f1495cf1919eeeac10b43205ad7a69a079cfe9edbbafc9dda192eb4656efb2270
> df90a716fc6bddd1d59a6ed2bd695005dd057f57807da127572b1643f210c6c61c27b3498650b76870e21f9a00bbbddcb801
> f51cc835e8440f33add1e9501a061e2311423f7b594e54917640a0de20c1b6fc077b6792eaed49315c6f04fe3fd2510271bb
> ee4ef00a0afbbcc6313135bef29082b2a34ceccdaee55733148bbcffdcc1d475024908aac7a817021ee07ea8b4ce80001000
> 000983941
*/














