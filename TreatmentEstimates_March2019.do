********************************************************************************
********************************************************************************
******* BEHAVIORAL INTERVENTIONS TO REDUCE ENERGY CONSUMPTION ON CAMPUS ********
****************************** ANALYSES ****************************************
* Erica Myers, Mateus Souza - April 2018
* Revised March 2018

clear
log close _all
capture _all
scalar drop _all
*reghdfe, cache(clear)
set more off, perm

*** Directory with all the data
cd "P:\iSEE\CampusEnergy2018"

gen RoundDateTime = 0
format RoundDateTime %tc
gen Room = "appen"
format Room %5s
gen double CTLSTPT = 99

** Opening room data files, cleaning and reshaping
local files : dir "./Data/Room/AllSetPoints" files "*.csv"

foreach file in `files' {
	tempfile final_file
	save `final_file'
	
	clear
	import delimited using "./Data/Room/AllSetPoints/`file'", varnames(3) case(preserve)

	rename (Date Time) (date time)
	rename *, upper
	drop if DATE==""
	drop if TIME==""

	** Convert setpoints to numeric
	foreach var of varlist *CTLSTPT {
		capture confirm string var `var'
		if _rc==0 {
		replace `var' = subinstr(`var', "{", "", .)
		destring `var', replace
		}
	}

	**** cleaning date/time variables, rounding to the closest 15 minute interval
	gen DateTime = DATE + " " + TIME
	gen double datetime = clock(DateTime, "MDYhms")
	format datetime %tc
	gen minutes = substr(TIME, -8, 2)
	destring minutes, replace
	gen double RoundDateTime = 15 * 60000 * floor(datetime / (15 * 60 * 1000)) if minutes<=7 | ///
							(minutes>=15 & minutes<=22) | (minutes>=30 & minutes<=37) | ///
							(minutes>=45 & minutes<=52)
	replace RoundDateTime = 15 * 60000 * ceil(datetime / (15 * 60 * 1000)) if (minutes>7 & minutes<15) | ///
							(minutes>22 & minutes<30) | (minutes>37 & minutes<45) | minutes>52						
	format RoundDateTime %tc
	drop minutes DateTime datetime DATE TIME
	order RoundDateTime, first

	/***** Now collapse observations to 15 minute intervals. Means will be taken 
	in case a room has multiple observation within a 15 minute interval ******/
	collapse (mean) *CTLSTPT, by(RoundDateTime)

	** Now we can reshape the data to long, rather than wide
	reshape long B1252FCU@CTLSTPT, i(RoundDateTime) j(Room) string
	rename B1252FCUCTLSTPT CTLSTPT
	
	append using `final_file'
}


** Dropping duplicates in terms of room and date/time
duplicates tag Room RoundDateTime, gen(tagdups)
sort Room RoundDateTime CTLSTPT
by Room RoundDateTime : replace CTLSTPT=CTLSTPT[1] if tagdups>0 & tagdups!=.
drop tagdups
duplicates drop

save "final_clean_rooms.dta", replace


**************************************************
*** merging with weather information
clear
import delimited ".\Data\weather_complete.csv"

gen double datetime = clock(date, "YMD hm")
order datetime, after(date)
format datetime %tc

gen minutes = substr(date, -2, 2)
destring minutes, replace

gen double RoundDateTime = 15 * 60000 * floor(datetime / (15 * 60 * 1000)) if minutes<=7 | ///
						(minutes>=15 & minutes<=22) | (minutes>=30 & minutes<=37) | ///
						(minutes>=45 & minutes<=52)
replace RoundDateTime = 15 * 60000 * ceil(datetime / (15 * 60 * 1000)) if (minutes>7 & minutes<15) | ///
						(minutes>22 & minutes<30) | (minutes>37 & minutes<45) | minutes>52						
format RoundDateTime %tc
order RoundDateTime, after(date)

gen roundminutes = mm(RoundDateTime)

drop if reporttpye=="SOD"
drop if roundminutes!=0

foreach var in hourlyvisibility hourlydrybulbtempf hourlydrybulbtempc hourlywetbulbtempf hourlywetbulbtempc ///
			hourlydewpointtempf hourlydewpointtempc hourlyrelativehumidity hourlywindspeed ///
			hourlystationpressure hourlysealevelpressure hourlyprecip hourlyaltimetersetting {
			
replace `var' = "" if substr(`var', -1,1)=="s"
replace `var' = "" if substr(`var', -1,1)=="V"
replace `var' = "" if `var'=="*"
replace `var' = "0.005" if `var'=="T"
destring `var', replace
}

encode hourlywinddirection, gen(winddirection)
drop hourlywinddirection
rename winddirection hourlywinddirection

drop station station_name reporttpye elevation latitude longitude date datetime minutes roundminutes
drop dailymaximumdrybulbtemp-dailycoolingdegreedays
drop dailyweather-monthlytotalseasontodatecoolingd

duplicates tag RoundDateTime, gen(tagdups)
foreach var of varlist hourlyskyconditions-dailysunset {
sort RoundDateTime `var'
by RoundDateTime : replace `var' = `var'[1] if tagdups>0 & tagdups!=.
}

drop tagdups
duplicates drop

expand 4
sort RoundDateTime
by RoundDateTime : gen obs = _n
replace RoundDateTime = RoundDateTime+(900000*(obs-1))
drop obs

******************************************************
** merge weather with room data
merge m:m RoundDateTime using "final_clean_rooms.dta"
drop if _merge==1
drop _merge

** Merge with treatment/randomization assignment for fall
merge m:1 Room using "room_randomization_clean.dta"
drop if _merge==2
drop _merge

** Merge with treatment/randomization assignment for spring
merge m:1 Room using "room_randomization_spring.dta"
drop if _merge==2
drop _merge

replace simpletreat_spring = . if simpletreat_spring==0 & control_spring==0
replace individtreat_spring = . if individtreat_spring==0 & control_spring==0
gen treat_spring = 0 if control_spring==1
replace treat_spring = 1 if control_spring==0

** Merge with winter break treatment assignment
merge m:1 Room using "winterbreak_treat.dta", keep(1 3) nogen

** Fixing some of the variable formatting
foreach x in cluster_type room_type simple_room_type {
	replace `x' = "" if `x'=="NA"
}


*** merge with some info about physical structure of the room (obtained from floor plans)
merge m:1 Room using "./Data/room_physical.dta", update
drop if _merge==2
drop _merge

encode wing, gen(wing2)
drop wing
rename wing2 wing

encode location, gen(location2)
drop location
rename location2 location

encode window, gen(window2)
drop window
rename window2 window

** Creating some factor variables (also anonymizes room)
egen room = group(Room)
egen roomCluster = group(room_cluster)

** Adjustments to the treatment variables (naming according to Pre-Analysis Plan)
gen treat = 1 if pure_control==0
replace treat = 0 if pure_control==1

gen treatA = 1 if individual_treatment==1
replace treatA = 0 if pure_control==1

gen treatB = 1 if cluster_treatment==1
replace treatB = 0 if pure_control==1

replace treatA=1 if room_cluster==3023 | room_cluster==3027 | room_cluster==3032 | room_cluster==6023
replace treat=1 if room_cluster==3023 | room_cluster==3027 | room_cluster==3032 | room_cluster==6023
replace treatB=. if room_cluster==3023 | room_cluster==3027 | room_cluster==3032 | room_cluster==6023


** Adjust the date/time variables
gen date = dofc(RoundDateTime)
format date %td

gen year = year(date)
gen monthofyear = month(date)
gen monthofsample = mofd(date)
format monthofsample %tm
gen dayofmonth = day(date)
gen hourofday = hh(RoundDateTime)
gen minutes = mm(RoundDateTime)
gen double timeofday = mdyhms(1,1,1960, hourofday, minutes, 0)
format timeofday %tcHH:MM
gen double statdatetime = mdyhms(monthofyear, dayofmonth, year, hourofday, minutes, 0) /* numeric, to impose restrictions on regressions */
order statdatetime, after(RoundDateTime)

gen double hourofsample = mdyhms(monthofyear, dayofmonth, year, hourofday, 0, 0)
format hourofsample %tc /* human readable */

gen weekday = dow(date)
replace weekday = 7 if weekday==0

/** week of year are being misallocated in 2018 (sat-sun, rather than sun-sat)
Will correct by adding one day to 2018 dates 
2016 has a similar issue (Fri-Thurs). Will subtract 2 days from 2016 dates */
gen weekofyear = week(date)
replace weekofyear = week(date+1) if year==2018
replace weekofyear = week(date-2) if year==2016

* Week variable to allow merge with the final usage file from AWS
gen Week = string(year) + "-" + string(weekofyear)
replace Week = string(year) + "-0" + string(weekofyear) if strlen(string(weekofyear))==1

* merging with usage file from AWS (Fall)
merge m:1 Room Week using "usage_result.dta"
drop if _merge==2
drop _merge

* merging with usage file from AWS (Spring)
merge m:1 Room Week using "usage_result_spring.dta", update
drop if _merge==2
drop _merge


** Variables that identify if room received great, good, or bad efficiency ratings
/* note that control rooms did not receive any rating, but will also be categorized
according to the same usage projections as the treated rooms */
sort room hourofsample
by room hourofsample : egen hourly_stpt = mean(CTLSTPT)

* difference between stpt and outdoor temperature
gen outdiff = hourly_stpt - hourlydrybulbtempf
replace outdiff = ceil(outdiff - 0.5)

* usage calculation for control rooms
gen steam_usage = 9.6*0.0472917*outdiff if simple_room_type=="Single Bedroom"
replace steam_usage = 9.6*0.0378844*outdiff if simple_room_type=="Double Bedroom"
replace steam_usage = 0 if outdiff<=0

gen chw_usage = (-0.7942)*outdiff if simple_room_type=="Single Bedroom"
replace chw_usage = (-0.9678)*outdiff if simple_room_type=="Double Bedroom"
replace chw_usage = 0 if outdiff>=0 & outdiff!=.

gen total_usage = steam_usage + chw_usage

* now transform hourly usage into weekly
by room hourofsample : gen unique_hour = 1 if _n==1

sort room Week
by room Week : egen totuseweek = mean(total_usage) if unique_hour==1
by room Week : egen total_usage_weekly = max(totuseweek)
replace total_usage_weekly = total_usage_weekly*168
replace total_usage_weekly = total_usage_weekly/1000

drop steam_usage chw_usage total_usage unique_hour totuseweek
replace Usage = total_usage_weekly if treat==0

* now who would be efficient/average neighbors for control rooms
levelsof Week, local(weeks)
foreach x of local weeks{
_pctile total_usage_weekly if Week=="`x'" & treat!=. & simple_room_type=="Single Bedroom", percentiles(15 50)
replace EfficientNeighbors = r(r1) if Week=="`x'" & treat==0 & simple_room_type=="Single Bedroom"
replace AllNeighbors = r(r2) if Week=="`x'" & treat==0 & simple_room_type=="Single Bedroom"

_pctile total_usage_weekly if Week=="`x'" & treat!=. & simple_room_type=="Double Bedroom", percentiles(15 50)
replace EfficientNeighbors = r(r1) if Week=="`x'" & treat==0 & simple_room_type=="Double Bedroom"
replace AllNeighbors = r(r2) if Week=="`x'" & treat==0 & simple_room_type=="Double Bedroom"
}

* finally identify rating received by the room
gen great = 0 if Usage!=.
replace great = 1 if Usage<EfficientNeighbors & Usage!=. & AllNeighbors!=. & EfficientNeighbors!=.

gen good = 0 if Usage!=.
replace good = 1 if Usage>=EfficientNeighbors & Usage<AllNeighbors & Usage!=. & AllNeighbors!=. & EfficientNeighbors!=.

gen bad = 0 if Usage!=.
replace bad = 1 if Usage>=AllNeighbors & Usage!=. & AllNeighbors!=. & EfficientNeighbors!=.


** Date and time Fall intervention started and ended (09/13 5pm - 12/14 11:45pm)
gen email_intervent = 0
replace email_intervent = 1 if RoundDateTime>=1820941200000 & RoundDateTime<1828915200000 /* starting when first email was sent in fall */
replace email_intervent = . if RoundDateTime>=1828915200000 /* when wint break intervention starts */
replace email_intervent = . if RoundDateTime<1819497600000  /* first day of fall semester */

** Date and time Spring intervention started and ended (01/16 5pm - end of april)
gen email_intervent_spring = 0
replace email_intervent_spring = 1 if RoundDateTime>=1833037200000 & RoundDateTime<1839974400000 /* starting when first email was sent in spring */
replace email_intervent_spring = . if RoundDateTime>=1839974400000 /* last available date of spring semester */
replace email_intervent_spring = . if RoundDateTime<1831680000000  /* first day of Spring semester */
order email_intervent_spring, after(email_intervent)

** auxiliary date indicators
gen fall2017 = 1 if RoundDateTime>=1819497600000 & RoundDateTime<1828915200000 /* first day of fall 2017 semester up to when wint break intervention starts */
gen spring2018 = 1 if RoundDateTime>=1831680000000 & RoundDateTime<1839974400000 /* first day of spring 2018 semester up to last available date */
gen fall2016 = 1 if RoundDateTime>=1787443200000 & RoundDateTime<1796774400000 /* academic dates for fall 2016 */
gen spring2017 = 1 if RoundDateTime>=1800230400000 & RoundDateTime<1809475200000 /* academic dates for spring 2017 */

gen fall1617 = 0 if fall2016==1
replace fall1617 = 1 if fall2017==1
gen spring1718 = 0 if spring2017==1
replace spring1718 = 1 if spring2018==1


** Lagged treatment assignment variables
tsset room RoundDateTime, clocktime delta(15 minutes)
gen lag_great = L672.great
gen lag_good = L672.good
gen lag_bad = L672.bad

** Variables to analyze if there is effect around the time email was sent
* event-study type variable to indicate distance from treatment time
gen timediff = ((RoundDateTime - L.RoundDateTime)/900000)*15
sort room RoundDateTime
by room RoundDateTime : replace timediff = 15 if timediff[1]==.
sort room Week RoundDateTime
by room Week : gen weekday_by_time = sum(timediff)

gen treatTime = .
replace treatTime = weekday_by_time if weekday==3 & hourofday==17 & minutes==0
sort room Week treatTime
by room Week : replace treatTime = treatTime[1]
gen timefromtreat_lab = weekday_by_time - treatTime

gen time_from_treat = 1 if timefromtreat_lab >=-2880 & timefromtreat_lab<=2880
sort room Week RoundDateTime
by room Week : replace time_from_treat = sum(time_from_treat)
sort room RoundDateTime
replace time_from_treat = . if timefromtreat_lab <-2880 | timefromtreat_lab>2880
replace timefromtreat_lab = . if timefromtreat_lab <-2880 | timefromtreat_lab>2880
drop timediff

* hourly variable, rather than 15-min
gen timefromtreat_lab_hour = timefromtreat_lab/60
replace timefromtreat_lab_hour = . if timefromtreat_lab_hour!=int(timefromtreat_lab_hour)

*labmask time_from_treat, values(timefromtreat_lab)

*** First difference of CTLSTPT
gen CTLSTPT_fd = CTLSTPT - L.CTLSTPT


** Variable to identify single-bedroom versus double bedroom
gen single_bedroom = 0 if simple_room_type=="Double Bedroom"
replace single_bedroom = 1 if simple_room_type=="Single Bedroom"

*** Winsorize outcome variables
*tempdist
foreach x in CTLSTPT CTLSTPT_fd {
winsor2 `x', cuts(0.5 99.5) trim replace
}

/*** merging with data from rooms that did not have changes in occupancy
dropping people who moved late into the building, or who changed rooms during the semester */
merge m:1 Room using "./Data/stable_rooms.dta", nogen keep(1 3)

*** number of observations by date
gen obs_date = 0
replace obs_date = 1 if CTLSTPT!=. & simple_room_type!=""
sort date
by date : egen obsbydate = total(obs_date) 
drop obs_date

*** observations by room by date
sort Room date
gen obs_roomdate = 0
by Room date : replace obs_roomdate = 1 if CTLSTPT!=. & simple_room_type!=""
by Room date : egen obsroomdate = total(obs_roomdate) 
drop obs_roomdate

*** students' demographics, majors/field of sutdy
tempfile clean_campuseneregy
save `clean_campuseneregy'

clear
import delimited using "./student_details.csv", varnames(1)
merge 1:1 netid using "netid_list.dta"
keep if _merge==3
drop _merge
drop netid

gen female = 1 if sex=="F"
replace female = 0 if sex=="M"

sort Room
foreach x in college_code college_desc dept_code dept_desc major_code major_desc residency sex female {
by Room : gen `x'1 = `x'[1]
by Room : gen `x'2 = `x'[2]
by Room : gen `x'3 = `x'[3]
by Room : gen `x'4 = `x'[4]
}
drop college_code-sex
drop female

replace college_desc1="College of Business" if college_code1=="KM"
replace college_desc2="College of Business" if college_code2=="KM"
replace college_desc3="College of Business" if college_code3=="KM"
replace college_desc4="College of Business" if college_code4=="KM"
duplicates drop

*** percent of roommates that are female
egen roommates_n = rownonmiss(female1 female2 female3 female4)
egen female_n = rowtotal(female1 female2 female3 female4)
gen female_pct = female_n/roommates_n

*** college affiliation percentages
forval i = 1/4 {
gen aces_college`i' = college_desc`i'=="Agr, Consumer, & Env Sciences"
gen ahs_college`i' = college_desc`i'=="Applied Health Sciences"
gen business_college`i' = college_desc`i'=="College of Business"
gen media_college`i' = college_desc`i'=="College of Media"
gen dgs_college`i' = college_desc`i'=="Division of General Studies"
gen education_college`i' = college_desc`i'=="Education"
gen engineering_college`i' = college_desc`i'=="Engineering"
gen finearts_college`i' = college_desc`i'=="Fine & Applied Arts"
gen las_college`i' = college_desc`i'=="Liberal Arts & Sciences"
gen socialwork_college`i' = college_desc`i'=="School of Social Work"
}
foreach x in aces ahs business media dgs education engineering finearts las socialwork {
egen `x'_college_n = rowtotal(`x'_college*)
gen `x'_college_pct = `x'_college_n/roommates_n
}
drop *_college1 *_college2 *_college3 *_college4

*** residency status percentages
forval i =1/4 {
gen instate`i' = residency`i'=="Resident, In State Tuition"
gen outstate`i' = residency`i'=="Non-Resident, Out ST Tuition"
gen international`i' = residency`i'=="International, Out ST Tuition"
}
egen instate_n =rowtotal(instate*)
egen outstate_n =rowtotal(outstate*)
egen international_n =rowtotal(international*)
gen instate_pct = instate_n/roommates_n
gen outstate_pct = outstate_n/roommates_n
gen international_pct = international_n/roommates_n
drop instate1 instate2 instate3 instate4 outstate1 outstate2 outstate3 outstate4 international1 international2 international3 international4


merge 1:m Room using `clean_campuseneregy'
drop if _merge==1
drop _merge

save "final_analysis.dta", replace



*** Balance tables for main (Fall) intervention
clear all
use "final_analysis.dta"

sort room_cluster RoundDateTime
by room_cluster : egen meanSTPT_pre = mean(CTLSTPT) if email_intervent==0
by room_cluster : egen STPT_pre = min(meanSTPT_pre)
by room_cluster : egen meanSTPT_post = mean(CTLSTPT) if email_intervent==1
by room_cluster : egen STPT_post = min(meanSTPT_post)

***** Balance Tables
keep room_cluster cluster_type floor wing location college_desc1 treat treatA treatB STPT_pre STPT_post
duplicates drop
drop if room_cluster==.

encode cluster_type, gen(clusttype)

tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)
tabulate college_desc1, gen(tab_college_desc)

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
use "final_analysis.dta"

gen outdiff_abs = abs(outdiff)

gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

sort Room RoundDateTime
by Room : egen meanSTPT_pre = mean(CTLSTPT) if post_winteremail==0
by Room : egen meanSTPT_post = mean(CTLSTPT) if post_winteremail==1
by Room : egen STPT_pre = min(meanSTPT_pre)
by Room : egen STPT_post = min(meanSTPT_post)

***** Balance Tables
keep Room simple_room_type floor wing location college_desc1 winterbreak_treat STPT_pre STPT_post
duplicates drop
drop if simple_room_type==""

encode simple_room_type, gen(roomtype)

tabulate roomtype, gen(tab_roomtype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)
tabulate college_desc1, gen(tab_college_desc)

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
use "final_analysis.dta"

sort Room RoundDateTime
by Room : egen meanSTPT_pre = mean(CTLSTPT) if email_intervent_spring==0
by Room : egen meanSTPT_post = mean(CTLSTPT) if email_intervent_spring==1
by Room : egen STPT_pre = min(meanSTPT_pre)
by Room : egen STPT_post = min(meanSTPT_post)

gen outdiff_abs = abs(outdiff)

***** Balance Tables
keep room_cluster cluster_type floor wing location college_desc1 treat_spring individtreat_spring simpletreat_spring STPT_pre STPT_post
duplicates drop
drop if room_cluster==.

encode cluster_type, gen(clusttype)

tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)
tabulate college_desc1, gen(tab_college_desc)

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



/*
*****************************************************************
*** Balance tables with clustered standard errors
clear all
use "final_analysis.dta"

encode cluster_type, gen(clusttype)

tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)

gen outdiff_abs = abs(outdiff)

**** For fall
matrix input rendemog = (.,.,.,.,.)
local vars CTLSTPT outdiff_abs tab_clusttype1 tab_clusttype2 tab_clusttype3 tab_clusttype4 ///
			tab_floor1 tab_floor2 tab_floor3 tab_floor4 tab_floor5 tab_floor6 tab_wing1 tab_wing2 ///
			tab_location1 tab_location2 tab_location3 tab_location4 tab_location5 tab_location6 ///
			aces_college_pct ahs_college_pct business_college_pct media_college_pct dgs_college_pct ///
			education_college_pct engineering_college_pct finearts_college_pct las_college_pct socialwork_college_pct ///
			female_pct instate_pct outstate_pct international_pct
foreach x of local vars {
reg `x' i.treatA if email_intervent==0 , vce(cluster roomCluster)
matrix coefs = r(table)
sca rend1`x' = coefs[1,3]
sca rend2`x' = coefs[1,2] + coefs[1,3]
sca renp1`x' = coefs[4,2]
reg `x' i.treatB if email_intervent==0 , vce(cluster roomCluster)
matrix coefs = r(table)
sca rend3`x' = coefs[1,2] + coefs[1,3]
sca renp2`x' = coefs[4,2]
matrix ren`x' = (rend1`x' , rend2`x', renp1`x', rend3`x', renp2`x')
matrix rendemog = (rendemog \ ren`x')
}
putexcel set "./Results/newbalancetables_clusteredSE.xlsx", modify sheet("fall")
putexcel C3 = matrix(rendemog)

* standard deviations of CTLSTPT to be added manually to the balance tables:
sum CTLSTPT if email_intervent==0 & treatA==0, detail
sum CTLSTPT if email_intervent==0 & treatA==1, detail
sum CTLSTPT if email_intervent==0 & treatB==1, detail
sum outdiff_abs if email_intervent==0 & treatA==0, detail
sum outdiff_abs if email_intervent==0 & treatA==1, detail
sum outdiff_abs if email_intervent==0 & treatB==1, detail


**** For winter break
gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

matrix input rendemog = (.,.,.)
local vars CTLSTPT outdiff_abs tab_clusttype1 tab_clusttype2 tab_clusttype3 tab_clusttype4 ///
			tab_floor1 tab_floor2 tab_floor3 tab_floor4 tab_floor5 tab_floor6 tab_wing1 tab_wing2 ///
			tab_location1 tab_location2 tab_location3 tab_location4 tab_location5 tab_location6 ///
			aces_college_pct ahs_college_pct business_college_pct media_college_pct dgs_college_pct ///
			education_college_pct engineering_college_pct finearts_college_pct las_college_pct socialwork_college_pct ///
			female_pct instate_pct outstate_pct international_pct
foreach x of local vars {
reg `x' i.winterbreak_treat if post_winteremail==0 , vce(cluster room)
matrix coefs = r(table)
sca rend1`x' = coefs[1,3]
sca rend2`x' = coefs[1,2] + coefs[1,3]
sca renp1`x' = coefs[4,2]
matrix ren`x' = (rend1`x' , rend2`x', renp1`x')
matrix rendemog = (rendemog \ ren`x')
}
putexcel set "./Results/newbalancetables_clusteredSE.xlsx", modify sheet("winter")
putexcel C3 = matrix(rendemog)

* standard deviations of CTLSTPT to be added manually to the balance tables:
sum CTLSTPT if post_winteremail==0 & winterbreak_treat==0, detail
sum CTLSTPT if post_winteremail==0 & winterbreak_treat==1, detail
sum outdiff_abs if post_winteremail==0 & winterbreak_treat==0, detail
sum outdiff_abs if post_winteremail==0 & winterbreak_treat==1, detail


**** For spring
matrix input rendemog = (.,.,.,.,.)
local vars CTLSTPT outdiff_abs tab_clusttype1 tab_clusttype2 tab_clusttype3 tab_clusttype4 ///
			tab_floor1 tab_floor2 tab_floor3 tab_floor4 tab_floor5 tab_floor6 tab_wing1 tab_wing2 ///
			tab_location1 tab_location2 tab_location3 tab_location4 tab_location5 tab_location6 ///
			aces_college_pct ahs_college_pct business_college_pct media_college_pct dgs_college_pct ///
			education_college_pct engineering_college_pct finearts_college_pct las_college_pct socialwork_college_pct ///
			female_pct instate_pct outstate_pct international_pct
foreach x of local vars {
reg `x' i.individtreat_spring if email_intervent_spring==0 , vce(cluster room)
matrix coefs = r(table)
sca rend1`x' = coefs[1,3]
sca rend2`x' = coefs[1,2] + coefs[1,3]
sca renp1`x' = coefs[4,2]
reg `x' i.simpletreat_spring if email_intervent_spring==0 , vce(cluster room)
matrix coefs = r(table)
sca rend3`x' = coefs[1,2] + coefs[1,3]
sca renp2`x' = coefs[4,2]
matrix ren`x' = (rend1`x' , rend2`x', renp1`x', rend3`x', renp2`x')
matrix rendemog = (rendemog \ ren`x')
}
putexcel set "./Results/newbalancetables_clusteredSE.xlsx", modify sheet("spring")
putexcel C3 = matrix(rendemog)

* standard deviations of CTLSTPT to be added manually to the balance tables:
sum CTLSTPT if email_intervent_spring==0 & individtreat_spring==0, detail
sum CTLSTPT if email_intervent_spring==0 & individtreat_spring==1, detail
sum CTLSTPT if email_intervent_spring==0 & simpletreat_spring==1, detail
sum outdiff_abs if email_intervent_spring==0 & individtreat_spring==0, detail
sum outdiff_abs if email_intervent_spring==0 & individtreat_spring==1, detail
sum outdiff_abs if email_intervent_spring==0 & simpletreat_spring==1, detail

*/

********************************************************************************
clear all
use "final_analysis.dta"

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
bysort roundCTLSTPT : egen ton = total(aux) if treat==0 & simple_room_type!="" & email_intervent==0 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_notreat = max(ton)

bysort roundCTLSTPT : egen tot = total(aux) if treat==1 & simple_room_type!="" & email_intervent==0 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_treat = max(tot)
drop ton tot

count if treat==0 & simple_room_type!="" & email_intervent==0 & roundCTLSTPT!=.
sca samp_notreat = `r(N)'
count if treat==1 & simple_room_type!="" & email_intervent==0  & roundCTLSTPT!=.
sca samp_treat = `r(N)'


* during intervention
bysort roundCTLSTPT : egen ton = total(aux) if treat==0 & simple_room_type!="" & email_intervent==1 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_notreat_post = max(ton)

bysort roundCTLSTPT : egen tot = total(aux) if treat==1 & simple_room_type!="" & email_intervent==1 & roundCTLSTPT!=.
bysort roundCTLSTPT : egen therm_obs_treat_post = max(tot)

count if treat==0 & simple_room_type!="" & email_intervent==1 & roundCTLSTPT!=.
sca samp_notreat_post = `r(N)'
count if treat==1 & simple_room_type!="" & email_intervent==1  & roundCTLSTPT!=.
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

/** Histograms/density of thermostat setpoints
twoway (histogram CTLSTPT if treat==0 & simple_room_type!="" & email_intervent==0, percent start(66.5) width(1) color(blue%30)) || ///
       (histogram CTLSTPT if treat==1 & simple_room_type!="" & email_intervent==0, percent start(66.5) width(1) color(red%30)) ///
	   , legend(order(1 "Control" 2 "Treated" )) graphregion(color(white)) bgcolor(white) ///
	   xtitle(Baseline Thermostat Setpoint) xlabel(67(1)78)
graph export "./Results/histogram_setpoints.png", replace
*/

** Setpoints by hour of day - NO CONTROLS during-treatment
reg CTLSTPT ibn.hourofday if ///
			 treat==1 & simple_room_type!="" & email_intervent==1, vce(cluster room) nocons
est sto mean_setpoints_treat
	
reg CTLSTPT ibn.hourofday if ///
			 treat==0 & simple_room_type!="" & email_intervent==1, vce(cluster room) nocons
est sto mean_setpoints_control

* difference between treat and control
reghdfe CTLSTPT ib0.treatXhour if simple_room_type!="" & email_intervent==1, nocons vce(cluster room) absorb(ibn.hourofday)
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
			 treat==1 & simple_room_type!="" & email_intervent==0, vce(cluster room) nocons
est sto mean_setpoints_treat
	
reg CTLSTPT ibn.hourofday if ///
			 treat==0 & simple_room_type!="" & email_intervent==0, vce(cluster room) nocons
est sto mean_setpoints_control

* difference between treat and control
reghdfe CTLSTPT ib0.treatXhour if simple_room_type!="" & email_intervent==0, nocons vce(cluster room) absorb(ibn.hourofday)
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
replace stable_date = 1 if CTLSTPT!=. & email_intervent!=. & simple_room_type!=""
sort date
by date : egen stabledate = total(stable_date) 
drop stable_date
gen statdate = date
order statdate, after(date)

*tab statdate if stabledate<=20000 & CTLSTPT!=. & email_intervent!=. & simple_room_type!=""
*tab date if stabledate<=20000 & CTLSTPT!=. & email_intervent!=. & simple_room_type!=""
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
			& treat==1 & email_intervent!=. & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_treat
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 ///
			& treat==0 & email_intervent!=. & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_control

* difference between treat and control
reghdfe CTLSTPT ib0.treatXdate if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 ///
			& email_intervent!=. & simple_room_type!="" , nocons vce(cluster room) absorb(ibn.date)	
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


/*
reg CTLSTPT ibn.date ///
			i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if treat==1 & email_intervent==1 & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_treat
reg CTLSTPT ibn.date ///
			i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if treat==0 & email_intervent==1 & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_control

coefplot stptbydate_control stptbydate_treat, vertical xlabel(1 "09/13" 8 "09/20" 15 "09/27" 22 "10/04" 29 "10/11" 36 "10/18" 43 "10/25" 50 "11/01" ///
					57 "11/08" 64 "11/15" 71 "11/22" 78 "11/29" 85 "12/06" 92 "12/13", labsize(small)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Control" 4 "Treated")) title("Average Setpoints by Date") ///
					keep(*date) graphregion(color(white)) bgcolor(white)
graph export "P:\iSEE\CampusEnergy2018\Results\setpoints_bydate_conts.png", replace
*/
/*			
***** Separating treatment
reg CTLSTPT ibn.date if date!=21129 & date!=21130 ///
			& individual_treatment==1 & email_intervent==1 & simple_room_type!="", nocons vce(cluster room)		
est sto stptbydate_individtreat
reg CTLSTPT ibn.date if date!=21129 & date!=21130 ///
			& cluster_treatment==1 & email_intervent==1 & simple_room_type!="", nocons vce(cluster room)		
est sto stptbydate_clustertreat

coefplot stptbydate_control stptbydate_individtreat stptbydate_clustertreat, vertical xlabel(1 "09/13" 8 "09/20" 15 "09/27" 22 "10/04" 29 "10/11" 36 "10/18" 43 "10/25" 50 "11/01" ///
					55 "11/08" 62 "11/15" 69 "11/22" 76 "11/29" 83 "12/06" 90 "12/13", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Control" 4 "Room-level Treatment" 6 "Suite-level Treatment")) title("Average Setpoints by Date") ///
					keep(*date) graphregion(color(white)) bgcolor(white)
graph export "P:\iSEE\CampusEnergy2018\Results\setpoints_bydate_subdivide.png", replace
*/
/*
** Setpoints by week
reg CTLSTPT ibn.weekofsample if treat==1 & email_intervent!=1 & simple_room_type!="" & freq>7000, nocons vce(cluster room)		
est sto stptbyweek_treat
reg CTLSTPT ibn.weekofsample if treat==0 & email_intervent!=1 & simple_room_type!="" & freq>7000, nocons vce(cluster room)		
est sto stptbyweek_control

coefplot stptbyweek_treat stptbyweek_control, vertical ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Control" 4 "Treated")) title("Average Setpoints by Week")
graph export "P:\iSEE\CampusEnergy2018\Results\setpoints_byweek.png", replace

xlabel(1 "09/13" 8 "09/20" 15 "09/27" 22 "10/04" 29 "10/11" 36 "10/18" 43 "10/25" 50 "11/01" ///
					57 "11/08" 64 "11/15" 71 "11/22" 78 "11/29" 85 "12/06" 92 "12/13", labsize(small)) ///
			
reg CTLSTPT ibn.date if individual_treatment==1 & email_intervent!=1 & simple_room_type!="", nocons vce(cluster room)		
est sto stptbydate_individtreat
reg CTLSTPT ibn.date if cluster_treatment==1 & email_intervent!=1 & simple_room_type!="", nocons vce(cluster room)		
est sto stptbydate_clustertreat

coefplot stptbydate_control stptbydate_individtreat stptbydate_clustertreat, vertical xlabel(1 "09/13" 8 "09/20" 15 "09/27" 22 "10/04" 29 "10/11" 36 "10/18" 43 "10/25" 50 "11/01" ///
					57 "11/08" 64 "11/15" 71 "11/22" 78 "11/29" 85 "12/06" 92 "12/13", labsize(small)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Control" 4 "Individual Treatment" 6 "Cluster Treatment")) title("Average Setpoints by Date")
graph export "P:\iSEE\CampusEnergy2018\Results\setpoints_bydate_subdivide.png", replace
*/


** Number of observations by treatment group
quietly : tab Room if individual_treatment ==1 & simple_room_type!="" & CTLSTPT!=. & email_intervent==1
dis r(r)
quietly : tab Room if cluster_treatment ==1 & simple_room_type!="" & CTLSTPT!=. & email_intervent==1
dis r(r)
quietly : tab Room if pure_control ==1 & simple_room_type!="" & CTLSTPT!=. & email_intervent==1
dis r(r)

quietly : tab Room if winterbreak_treat==1 & simple_room_type!="" & CTLSTPT!=. & statdatetime>=1827705600000 & statdatetime<=1830383100000
dis r(r)
quietly : tab Room if winterbreak_treat==0 & simple_room_type!="" & CTLSTPT!=. & statdatetime>=1827705600000 & statdatetime<=1830383100000
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
use "final_analysis.dta"

label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Sep. 13"
label values email_intervent posttreatlabel


********************************************************************************
*************** Model Using Absolute Difference as Outcome *********************
gen outdiff_abs = abs(outdiff)

* some descriptives on the outdiff variable, to be added manually to result tables
*sum outdiff if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000 & simple_room_type!="" & treat!=.
*dis 25*24*46.15*0.454
*sum outdiff if email_intervent!=. & simple_room_type!="" & treat!=.
sum outdiff_abs if email_intervent!=. & simple_room_type!="" & treat!=.
sum outdiff_abs if email_intervent==1 & simple_room_type!="" & treat!=.


** Simple regs on treatment
reg outdiff_abs i.treat if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg outdiff_abs i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe outdiff_abs i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe outdiff_abs i.treat if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen auxvar = mean(CTLSTPT) if email_intervent==0
by room : egen avg_stpt_pre = min(auxvar)
drop auxvar

reghdfe outdiff_abs i.treat avg_stpt_pre if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe outdiff_abs 1.treat##1.email_intervent if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect_outdiff.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat 1.treat#1.email_intervent)



** Regs separating the 2 treatment arms
label define indtreatlabel 1 "Room-level Treatment"
label values individual_treatment indtreatlabel
label define clusttreatlabel 1 "Suite-level Treatment"
label values cluster_treatment clusttreatlabel

reg outdiff_abs i.individual_treatment i.cluster_treatment if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg outdiff_abs i.individual_treatment i.cluster_treatment i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe outdiff_abs i.individual_treatment i.cluster_treatment hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe outdiff_abs i.individual_treatment i.cluster_treatment if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe outdiff_abs i.individual_treatment i.cluster_treatment avg_stpt_pre if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe outdiff_abs 1.individual_treatment##1.email_intervent 1.cluster_treatment##1.email_intervent if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/septreat_effect_outdiff.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.individual_treatment 1.cluster_treatment 1.individual_treatment#1.email_intervent 1.cluster_treatment#1.email_intervent)
			
			
			
********************************************************************************
******************** Model Using Thermostats as Outcome ************************

** Simple regs on treatment
reg CTLSTPT i.treat if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.treat if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.treat avg_stpt_pre if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.treat#i.email_intervent if simple_room_type!="" , ///
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

reg CTLSTPT i.individual_treatment i.cluster_treatment if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.individual_treatment i.cluster_treatment i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.individual_treatment i.cluster_treatment hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.individual_treatment i.cluster_treatment if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.individual_treatment i.cluster_treatment avg_stpt_pre if email_intervent==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.individual_treatment#i.email_intervent i.cluster_treatment#i.email_intervent if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/septreat_effect.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.individual_treatment 1.cluster_treatment 1.individual_treatment#1.email_intervent 1.cluster_treatment#1.email_intervent)



** Simple regs on treatment - only for cold days
sort date
by date : egen maxtemp = max(hourlydrybulbtempf)

gen coldday = 1 if maxtemp<65

reg CTLSTPT i.treat if email_intervent==1 & simple_room_type!="" & coldday==1, vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" & coldday==1, vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" & coldday==1, ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.treat if email_intervent==1 & simple_room_type!="" & coldday==1, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.treat avg_stpt_pre if email_intervent==1 & simple_room_type!="" & coldday==1, ///
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

reghdfe CTLSTPT i.treatXhour* if email_intervent==1 & simple_room_type!="", absorb(i.floor i.wing i.location i.window i.date) vce(cluster roomCluster)
est sto treathour_datecont

coefplot treathour_datecont, vertical xlabel(1 "1am" 2 "2am" 3 "3am" 4 "4am" 5 "5am" 6 "6am" 7 "7am" 8 "8am" 9 "9am" 10 "10am" 11 "11am" 12 "noon" 13 "1pm" 14 "2pm" 15 "3pm" 16 "4pm" 17 "5pm" 18 "6pm" 19 "7pm" 20 "8pm" 21 "9pm" 22 "10pm" 23 "11pm" 24 "midnight", labsize(small) angle(45)) ///
			xtitle("Hour of Day") ytitle("Treatment Effect") legend(off) title("Treatment Effects by Hour of Day") ///
			graphregion(color(white)) bgcolor(white)
graph export "./Results/treatbyhour.png", replace


/*
label define hourdaylab 1 "1am" 2 "2am" 3 "3am" 4 "4am" 5 "5am" 6 "6am" 7 "7am" 8 "8am" 9 "9am" 10 "10am" ///
		11 "11am" 12 "noon" 13 "1pm" 14 "2pm" 15 "3pm" 16 "4pm" 17 "5pm" 18 "6pm" 19 "7pm" 20 "8pm" 21 "9pm" 22 "10pm" 23 "11pm" 24 "midnight"
gen graphhourofday = hourofday + 1
label values graphhourofday hourdaylab

reg CTLSTPT i.treat#i.hourofday if email_intervent!=. & simple_room_type!="", vce(cluster roomCluster)
est sto treathour

reghdfe CTLSTPT i.treat#ibn.graphhourofday if email_intervent!=. & simple_room_type!="", absorb(i.floor i.wing i.location i.window i.date) vce(cluster roomCluster)
est sto treathour_datecont

esttab treathour_datecont using "./Results/effectbyhour.tex", ///
		replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat*)



reghdfe CTLSTPT i.treat##ib17.hourofday if simple_room_type!="" & email_intervent==1 ///
			& timefromtreat_lab_hour!=., absorb(datetime) vce(cluster roomCluster)
est sto treat_hour_reg

coefplot treat_hour_reg, vertical xlabel(1 "0" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6" 8 "7" 9 "8" 10 "9" ///
			11 "10" 12 "11" 13 "12" 14 "13" 15 "14" 16 "15" 17 "16" 18 "17" 19 "18" 20 "19" 21 "20" 22 "21" 23 "22" 24 "23") ///
			drop(1.treat) xtitle("Hour of Day") ytitle("Temperature (Degrees Fahrenheit)") ///
			legend(order(2 "Control" 4 "Treated")) title("Average Setpoints by Hour of Day") ///
			graphregion(color(white)) bgcolor(white)

			
coefplot treat_hour_reg, vertical xlabel(1 "0" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6" 8 "7" 9 "8" 10 "9" ///
			11 "10" 12 "11" 13 "12" 14 "13" 15 "14" 16 "15" 17 "16" 18 "18" 19 "19" 20 "20" 21 "21" 22 "22" 23 "23") ///
			drop(1.treat) xtitle("Hour of Day") ytitle("Effect on Setpoint" "(Omitted 5pm)") ///
			legend(order(2 "Treated")) ///
			graphregion(color(white)) bgcolor(white) xline(17.5)
graph export "P:\iSEE\CampusEnergy2018\Results\hourofday_regs.png", replace
*/

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

reghdfe CTLSTPT i.treatXweekday* if email_intervent==1 & simple_room_type!="", absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
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
reg CTLSTPT i.treat##i.single_bedroom if email_intervent==1, vce(cluster roomCluster)
est sto treatbedroom

reghdfe CTLSTPT i.treat##i.single_bedroom if email_intervent==1, absorb(date) vce(cluster roomCluster)
est sto treatbedroom_conts

reghdfe CTLSTPT i.treat##i.single_bedroom if email_intervent==1, absorb(i.floor i.wing i.location i.window RoundDateTime) vce(cluster roomCluster)
est sto treatbedroom_moreconts

reghdfe CTLSTPT i.treat##i.single_bedroom##i.email_intervent, absorb(i.room i.floor i.wing i.location i.window RoundDateTime) vce(cluster roomCluster)
est sto treatbedroom_full

esttab treatbedroom treatbedroom_conts treatbedroom_moreconts treatbedroom_full using "./Results/effectbybedtype.csv", replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)


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

/*
forval i = 1(1)97 {
replace treatXhourfrom`i' = treatXhourfrom`i'*email_intervent
}

reghdfe CTLSTPT i.treatXhourfrom34 i.treatXhourfrom35 i.treatXhourfrom36 i.treatXhourfrom37 i.treatXhourfrom38 ///
	i.treatXhourfrom39 i.treatXhourfrom40 i.treatXhourfrom41 i.treatXhourfrom42 i.treatXhourfrom43 ///
	i.treatXhourfrom44 i.treatXhourfrom45 i.treatXhourfrom46 i.treatXhourfrom47 i.treatXhourfrom48 ///
	i.treatXhourfrom49 i.treatXhourfrom50 i.treatXhourfrom51 i.treatXhourfrom52 i.treatXhourfrom53 i.treatXhourfrom54  ///
	i.treatXhourfrom55 i.treatXhourfrom56 i.treatXhourfrom57 i.treatXhourfrom58 i.treatXhourfrom59  ///
	i.treatXhourfrom60 i.treatXhourfrom61 i.treatXhourfrom62 i.treatXhourfrom63 i.treatXhourfrom64  ///
	if email_intervent!=0 & simple_room_type!="" & hourfromtreat>=34 & hourfromtreat<=64, ///
	absorb(i.treat i.hourofday) vce(cluster roomCluster)

reghdfe CTLSTPT i.treatXhourfrom34 i.treatXhourfrom35 i.treatXhourfrom36 i.treatXhourfrom37 i.treatXhourfrom38 ///
	i.treatXhourfrom39 i.treatXhourfrom40 i.treatXhourfrom41 i.treatXhourfrom42 i.treatXhourfrom43 ///
	i.treatXhourfrom44 i.treatXhourfrom45 i.treatXhourfrom46 i.treatXhourfrom47 i.treatXhourfrom48 ///
	i.treatXhourfrom49 i.treatXhourfrom50 i.treatXhourfrom51 i.treatXhourfrom52 i.treatXhourfrom53 i.treatXhourfrom54  ///
	i.treatXhourfrom55 i.treatXhourfrom56 i.treatXhourfrom57 i.treatXhourfrom58 i.treatXhourfrom59  ///
	i.treatXhourfrom60 i.treatXhourfrom61 i.treatXhourfrom62 i.treatXhourfrom63 i.treatXhourfrom64  ///
	if email_intervent!=. & simple_room_type!="" & hourfromtreat>=34 & hourfromtreat<=64, ///
	absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto hourfomrtreat

coefplot hourfomrtreat , vertical keep(1.treatXhourfrom*) ///
	xlab(1 "-15" 2 "-14" 3 "-13" 4 "-12" 5 "-11" 6 "-10" 7 "-9" 8 "-8" 9 "-7" 10 "-6" 11 "-5" 12 "-4" 13 "-3"  14 "-2" 15 "-1" ///
	16 "0" 17 "1" 18 "2" 19 "3" 20 "4" 21 "5" 22 "6" 23 "7" 24 "8" 25 "9" 26 "10" 27 "11" 28 "12" 29 "13" 30 "14" 31 "15", labsize(vsmall)) ///
	xtitle("Hours Since Wednesday 5pm") ytitle("Treatment Effects")
graph export "./Results/effects_aroundtreat.png", replace


reghdfe CTLSTPT ib0.treat##ib193.time_from_treat if time_from_treat>163 & time_from_treat<223 ///
			& email_intervent!=. & simple_room_type!="" , ///
			absorb(room##hourofday) vce(cluster roomCluster)
est sto treat_time

coefplot treat_time , vertical baselevels drop(1.treat) xline(49.5) ///
			xlabel(1 "-720" 9 "-600" 17 "-480" 25 "-360" 33 "-240" 41 "-120" 49.5 "0" 57 "120" 65 "240" 73 "360" 81 "480" 90 "600" 98 "720") ///
			xtitle("Minutes from Treatment") ytitle("Treatment Effect" "(Compared to Treated at Wed. 5pm)") ///
			graphregion(color(white)) bgcolor(white)
graph export "P:\iSEE\CampusEnergy2018\Results\treat_by_time.png", replace

* hourly
reghdfe CTLSTPT i.treat#i.time_from_treat ///
			if timefromtreat_lab_hour !=. & email_intervent!=. & simple_room_type!="" ///
			& timefromtreat_lab_hour>=-24 & timefromtreat_lab_hour<=24 , ///
			absorb(hourofday#room) vce(cluster roomCluster)
est sto treat_time_hour
coefplot treat_time_hour, vertical baselevels drop xline(49.5) ///
			xlabel(none) ///
			graphregion(color(white)) bgcolor(white)
graph export "P:\iSEE\CampusEnergy2018\Results\treat_by_hour.png", replace

encode timefromtreat_lab_hour
			
reghdfe tempdist ib0.treat##ib193.time_from_treat ///
			if timefromtreat_lab_hour !=. & email_intervent!=. & simple_room_type!="" , ///
			absorb(datetime timeofday room) vce(cluster roomCluster)
est sto treatdist_time_hour
coefplot treatdist_time_hour, vertical baselevels drop(1.treat) xline(49.5) ///
			graphregion(color(white)) bgcolor(white) ///
			xlabel(none)
graph export "P:\iSEE\CampusEnergy2018\Results\treatdist_by_hour.png", replace

reghdfe CTLSTPT ib0.treat#ib193.time_from_treat ib193.time_from_treat ///
			if timefromtreat_lab_hour !=. & email_intervent!=. & simple_room_type!="" , ///
			absorb(datetime timeofday room) vce(cluster roomCluster)

*/
/*
reghdfe CTLSTPT i.treat#i.email_intervent#1.single_bedroom if simple_room_type!="" , ///
			absorb(room datetime) vce(cluster roomCluster)			
*/
			
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
reg CTLSTPT ibn.date if winterbreak_treat==1 & simple_room_type!="" & statdatetime>=1827705600000 & statdatetime<=1833061500000, nocons vce(cluster room)		
est sto wbrkstptsbydate_treat
reg CTLSTPT ibn.date if winterbreak_treat==0 & simple_room_type!="" & statdatetime>=1827705600000 & statdatetime<=1833061500000, nocons vce(cluster room)		
est sto wbrkstptsbydate_control

* difference between treat and control
gen winttreatXdate = winterbreak_treat*date

reghdfe CTLSTPT ib0.winttreatXdate if statdatetime>=1827705600000 & statdatetime<=1833061500000 ///
			& simple_room_type!="" , nocons vce(cluster room) absorb(ibn.date)	
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

reg CTLSTPT i.winterbreak_treat if post_winteremail==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.winterbreak_treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if post_winteremail==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.winterbreak_treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if post_winteremail==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.winterbreak_treat if post_winteremail==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if post_winteremail==0
by room : egen avg_stpt_prewinter = min(aux)
drop aux

reghdfe CTLSTPT i.winterbreak_treat avg_stpt_prewinter if post_winteremail==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.winterbreak_treat#i.post_winteremail if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using  "./Results/winterbreak_diffdiff.tex", ///
						replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.winterbreak_treat 1.winterbreak_treat#1.post_winteremail)




********************************************************************************
********************************************************************************
************************** DESCRIPTIVE STATISTICS ******************************

*** Number of observations by date
clear
log close _all
capture _all
scalar drop _all
reghdfe, cache(clear)
set more off

cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

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
use "final_analysis.dta"

keep if obsroomdate>=20 & obsroomdate!=.
keep Room date
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


********************************************************************************
************ number of thermostat changes by room by week - appendix
clear all
use "final_analysis.dta"

tsset room RoundDateTime, clocktime delta(15 minutes)
gen therm_change = 1 if CTLSTPT!=L.CTLSTPT & CTLSTPT!=. & L.CTLSTPT!=. & simple_room_type!=""

** variable to help identify rooms with complete week of observations
sort Room Week
gen obs_roomweek = 0
by Room Week : replace obs_roomweek = 1 if CTLSTPT!=. & simple_room_type!=""
by Room Week : egen obsroomweek = total(obs_roomweek) 
drop obs_roomweek

** number of times thermostat changes by room by date
sort room date
by room date : egen thermchange = total(therm_change)

** number of times thermostat changes by room by week of sample
sort room Week
by room Week : egen thermchange_week = total(therm_change)

keep Week Room thermchange_week treat obsroomweek
duplicates drop

encode(Week), gen(week)
encode(Room), gen(room)

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
use "final_analysis.dta"


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


/*
sort room date
by room date : gen daysin = 1 if obsroomdate>90 & obsroomdate!=. & _n==1
by room : egen days_in_sample = total(daysin)

gen double statdate = date

sort room date
by room date : gen full_room = 1 if statdate>=20689 & statdate<=21295 & obsroomdate>90 & _n==1
by room : egen days_in_sample = total(full_room)

20689

1787529600000
*/

gen double statdate = date
sort room date
by room date : gen start_room = 1 if statdate==20689 & obsroomdate>90 & _n==1
by room : egen starting_room = max(start_room)


set matsize 10000
** Setpoints by date - control
* significant data losses in dates: 47, 48, 49, 52, 61, 62, 63, 72, 73, 74, 91
reg CTLSTPT ibn.fall2016date if fall2016_restricted==1 & starting_room==1 ///
			& treat==0 & simple_room_type!="" ///
			& fall2016date!=47 & fall2016date!=48 & fall2016date!=49 ///
			& fall2016date!=52 & fall2016date!=61 & fall2016date!=62 & fall2016date!=63 ///
			& fall2016date!=72 & fall2016date!=73 & fall2016date!=74 & fall2016date!=91 ///
			, nocons vce(cluster room)		
est sto stptbydate_control_2016

reg CTLSTPT ibn.fall2017date if fall2017_restricted==1 & starting_room==1 ///
			& treat==0 & simple_room_type!="" ///
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
			& treat==1 & simple_room_type!="" ///
			& fall2016date!=47 & fall2016date!=48 & fall2016date!=49 ///
			& fall2016date!=52 & fall2016date!=61 & fall2016date!=62 & fall2016date!=63 ///
			& fall2016date!=72 & fall2016date!=73 & fall2016date!=74 & fall2016date!=91 ///
			, nocons vce(cluster room)		
est sto stptbydate_treated_2016

reg CTLSTPT ibn.fall2017date if fall2017_restricted==1 & starting_room==1 ///
			& treat==1 & simple_room_type!="" ///
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


/*
set matsize 10000
** Setpoints by date - FULL SAMPLE
reg CTLSTPT ibn.date if complete_room==1 & statdatetime>=1787529600000 & statdatetime<1839973500000 ///
			& treat==1 & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_treat_full

reg CTLSTPT ibn.date if complete_room==1 & statdatetime>=1787529600000 & statdatetime<1839973500000 ///
			& treat==0 & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_control_full

coefplot stptbydate_control_full stptbydate_treat_full, vertical  ///
					xlabel(1 "08/23/2016" 32 "09/23/2016" 62 "10/23/2016" 93 "11/23/2016" ///
					123 "12/23/2016" 154 "01/23/2017" 185 "02/23/2017" 213 "03/23/2017" ///
					244 "04/23/2017" 274 "05/23/2017" 305 "06/23/2017" 335 "07/23/2017" ///
					359 "08/23/2017" 390 "09/23/2017" 420 "10/23/2017" 450 "11/23/2017" ///
					, labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Control" 4 "Treated")) title("Average Setpoints by Date") ///
					keep(*date) graphregion(color(white)) bgcolor(white)
graph export "./Results/setpoints_bydate_full.png", replace
*/

/*
******************* Regressions to assess spillovers
* diff-in-diff comparing fall 2016 w/ 2017, treat vs control
reg CTLSTPT 1.fall1617##0.treat if simple_room_type!="" & starting_room==1, ///
		vce(cluster roomCluster)
est sto spill_fall_nocont

reghdfe CTLSTPT 1.fall1617##0.treat if simple_room_type!="" & starting_room==1, ///
		absorb(dayofmonth#monthofyear) vce(cluster roomCluster)
est sto spill_fall_date

reghdfe CTLSTPT 1.fall1617##0.treat if simple_room_type!="" & starting_room==1, ///
		absorb(i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear) ///
		vce(cluster roomCluster)
est sto spill_fall_room

reghdfe CTLSTPT 1.fall1617##0.treat  ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset if simple_room_type!="" & starting_room==1, ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear) ///
		vce(cluster roomCluster)
est sto spill_fall_weather

reghdfe CTLSTPT 1.fall1617##0.treat  ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset if simple_room_type!="" & starting_room==1, ///
		absorb(i.hourlywinddirection i.room dayofmonth#monthofyear) ///
		vce(cluster roomCluster)
est sto spill_fall_full

label define treated 0 "Control"
label define fall17 1 "Fall 2017"
label values treat treated
label values fall1617 fall17

esttab spill_fall_nocont spill_fall_date spill_fall_room spill_fall_weather using "./Results/spillover_fall.tex", ///
				replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
				keep(1.treat 1.fall1617 1.fall1617#1.treat) label


* diff-in-diff comparing spring 2017 w/ 2018, treat vs control
reg CTLSTPT i.spring1718##simpletreat_spring if simple_room_type!="" , ///
		vce(cluster roomCluster)
est sto spill_spring_nocont

reghdfe CTLSTPT i.spring1718##simpletreat_spring if simple_room_type!="" , ///
		absorb(dayofmonth#monthofyear) vce(cluster roomCluster)
est sto spill_spring_date
*/

** Spilovers: DID using proxy treatment date for Fall 2016
gen proxy_treat = 0 if fall2017date<=16 | fall2016date<=16
replace proxy_treat = 1 if (fall2017date>16 & fall2017date!=.) | (fall2016date>16 & fall2016date!=.)

* regression to check if control rooms are behave differently in fall 2016 versu 2017, pre and post-treatment
reghdfe CTLSTPT fall1617##proxy_treat if simple_room_type!="" & treat==0, absorb(room) vce(cluster roomCluster)
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
******* Heterogeneity by pre-treat setting
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

sum avg_stpt_pretreat, detail
sort avg_stpt_pretreat
gen stpt_level_pretreat = 2 if avg_stpt_pretreat<=r(p25)
replace stpt_level_pretreat = 1 if avg_stpt_pretreat>r(p25) & avg_stpt_pretreat<=r(p75)
replace stpt_level_pretreat = 3 if avg_stpt_pretreat>r(p75)
label define stps 2 "low" 1 "medium" 3 "high"
label values stpt_level_pretreat stps

merge 1:m room using "final_analysis.dta", nogen keep(3)

* Regressions including indicator for pre-treat setting
label define treated 1 "Treated"
label values treat treated
label define post 1 "PostSep14"
label values email_intervent post
label define stps 2 "low set" 1 "medium set" 3 "high set"
label values stpt_level_pretreat stps

reg CTLSTPT i.stpt_level_pretreat##treat if simple_room_type!="" & email_intervent!=., ///
		vce(cluster roomCluster)
est sto stpthet_nocont

reghdfe CTLSTPT i.stpt_level_pretreat##treat if simple_room_type!="" & email_intervent!=., ///
		absorb(dayofmonth#monthofyear) vce(cluster roomCluster)
est sto stpthet_date

reghdfe CTLSTPT i.stpt_level_pretreat##treat if simple_room_type!="" & email_intervent!=., ///
		absorb(i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear) ///
		vce(cluster roomCluster)
est sto stpthet_room

reghdfe CTLSTPT i.stpt_level_pretreat##treat  ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset if simple_room_type!="" & email_intervent!=., ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear) ///
		vce(cluster roomCluster)
est sto stpthet_did

reghdfe CTLSTPT i.stpt_level_pretreat##i.treat##i.email_intervent  ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset if simple_room_type!="" & email_intervent!=., ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear) ///
		vce(cluster roomCluster)
est sto stpthet_tripdiff

reghdfe CTLSTPT i.stpt_level_pretreat##i.treat##i.email_intervent  ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset if simple_room_type!="" & email_intervent!=., ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
		vce(cluster roomCluster)
est sto stpthet_roomfe

esttab stpthet_nocont stpthet_date stpthet_room stpthet_did  stpthet_tripdiff stpthet_roomfe /// 
				using "./Results/stpthet_fall.tex", ///
				replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
				keep(1.treat 2.stpt_level_pretreat 3.stpt_level_pretreat ///
				2.stpt_level_pretreat#1.treat 3.stpt_level_pretreat#1.treat ///
				1.email_intervent 2.stpt_level_pretreat#1.email_intervent ///
				3.stpt_level_pretreat#1.email_intervent 1.treat#1.email_intervent ///
				2.stpt_level_pretreat#1.treat#1.email_intervent ///
				3.stpt_level_pretreat#1.treat#1.email_intervent) label


*** heterogeneity using deciles of pre-treat setting
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

xtile stpt_decile_pretreat = avg_stpt_pretreat, nq(10)

merge 1:m room using "final_analysis.dta", nogen keep(3)

* Generate interactions of treated X decile X post-treat
tabulate stpt_decile_pretreat, gen(stpt_decile)

forval i = 1/10 {
gen stpt_decile_treat`i' = treat*email_intervent*stpt_decile`i'
}

forval i = 1/10 {
gen stpt_decile_post`i' = email_intervent*stpt_decile`i'
}

gen treatXpost = treat*email_intervent

reghdfe CTLSTPT i.stpt_decile_treat2 i.stpt_decile_treat3 i.stpt_decile_treat4 i.stpt_decile_treat5 i.stpt_decile_treat6 ///
		i.stpt_decile_treat7 i.stpt_decile_treat8 i.stpt_decile_treat9 i.stpt_decile_treat10 ///
		i.treatXpost i.stpt_decile_post* ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset ///
		if simple_room_type!="" & email_intervent!=., ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
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
graph export "./Results/treateffect_decile.png", replace


*** heterogeneity using quintiles of pre-treat setting
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

xtile stpt_quintile_pretreat = avg_stpt_pretreat, nq(5)

merge 1:m room using "final_analysis.dta", nogen keep(3)

* Generate interactions of treated X decile X post-treat
tabulate stpt_quintile_pretreat, gen(stpt_quintile)

forval i = 1/5 {
gen stpt_quintile_treat`i' = treat*email_intervent*stpt_quintile`i'
}

forval i = 1/5 {
gen stpt_quintile_post`i' = email_intervent*stpt_quintile`i'
}

gen treatXpost = treat*email_intervent

reghdfe CTLSTPT i.stpt_quintile_treat2 i.stpt_quintile_treat3 i.stpt_quintile_treat4 i.stpt_quintile_treat5 ///
		i.stpt_quintile_post* ///
		i.treatXpost ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset ///
		if simple_room_type!="" & email_intervent!=., ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
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
graph export "./Results/treateffect_quintile.png", replace



*** heterogeneity using quartiles of pre-treat setting
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

xtile stpt_quartile_pretreat = avg_stpt_pretreat, nq(4)

merge 1:m room using "final_analysis.dta", nogen keep(3)

* Generate interactions of treated X decile X post-treat
tabulate stpt_quartile_pretreat, gen(stpt_quartile)

forval i = 1/4 {
gen stpt_quartile_treat`i' = treat*email_intervent*stpt_quartile`i'
}

forval i = 1/4 {
gen stpt_quartile_post`i' = email_intervent*stpt_quartile`i'
}

gen treatXpost = treat*email_intervent

reghdfe CTLSTPT i.stpt_quartile_treat2 i.stpt_quartile_treat3 i.stpt_quartile_treat4 ///
		i.stpt_quartile_post* ///
		i.treatXpost ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset ///
		if simple_room_type!="" & email_intervent!=., ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
		vce(cluster roomCluster)
est sto stpt_quartile_het

lincomest 1.treatXpost
est sto _1
* Coefficients for each quartile
foreach i in 2 3 4 {
est restore stpt_quartile_het
lincomest 1.stpt_quartile_treat`i' + 1.treatXpost
est sto _`i'
}

coefplot _1 || _2 || _3 || _4 , vertical bycoefs ///
		ytitle("Treatment Effect (in degrees F)") xtitle("Quartile of Baseline Thermostat Setpoint")	///
		xlabel(1(1)4) ylabel(-1.2(0.4)1.2, labsize(vsmall)) ///
		graphregion(color(white)) bgcolor(white) yline(0)
graph export "./Results/treateffect_quartile.png", replace


*** heterogeneity using deciles of pre-treat setting - pooling Fall and Spring
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0 | email_intervent_spring==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

xtile stpt_decile_pretreat = avg_stpt_pretreat, nq(10)

merge 1:m room using "final_analysis.dta", nogen keep(3)

* Generate interactions of treated X decile X post-treat
tabulate stpt_decile_pretreat, gen(stpt_decile)

** pooling the 'post' variable for Fall and Spring
gen posttreat = 0 if email_intervent==0 | email_intervent_spring==0
replace posttreat = 1 if email_intervent==1 | email_intervent_spring==1

** Pooling treatment variable for Fall and Spring
gen treat_fa = treat*email_intervent
gen treat_sp = individtreat_spring*email_intervent_spring
gen treat_fasp = treat_fa
replace treat_fasp = treat_sp if treat_fasp==.

forval i = 1/10 {
gen stpt_decile_treat`i' = treat_fasp*stpt_decile`i'
}

forval i = 1/10 {
gen stpt_decile_post`i' = posttreat*stpt_decile`i'
}

reghdfe CTLSTPT i.stpt_decile_treat* ///
		i.stpt_decile_post* ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset ///
		if simple_room_type!="" , ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
		vce(cluster roomCluster)
est sto stpt_decile_het

coefplot stpt_decile_het , keep(1.stpt_decile_treat*) vertical ///
		ytitle("Treatment Effect (in degrees F)") xtitle("Decile of Baseline Thermostat Setpoint")	///
		xlabel(1(1)10) ylabel(-1.2(0.2)1.2, labsize(vsmall)) ///
		graphregion(color(white)) bgcolor(white) yline(0)
graph export "./Results/treateffect_decile_pooled.png", replace


*** heterogeneity using quintiles of pre-treat setting - pooling Fall and Spring
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0 | email_intervent_spring==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

xtile stpt_quintile_pretreat = avg_stpt_pretreat, nq(5)

merge 1:m room using "final_analysis.dta", nogen keep(3)

** pooling the 'post' variable for Fall and Spring
gen posttreat = 0 if email_intervent==0 | email_intervent_spring==0
replace posttreat = 1 if email_intervent==1 | email_intervent_spring==1

** Pooling treatment variable for Fall and Spring
gen treat_fa = treat*email_intervent
gen treat_sp = individtreat_spring*email_intervent_spring
gen treat_fasp = treat_fa
replace treat_fasp = treat_sp if treat_fasp==.

* Generate interactions of treated X decile X post-treat
tabulate stpt_quintile_pretreat, gen(stpt_quintile)

forval i = 1/5 {
gen stpt_quintile_treat`i' = treat_fasp*stpt_quintile`i'
}

forval i = 1/5 {
gen stpt_quintile_post`i' = posttreat*stpt_quintile`i'
}

reghdfe CTLSTPT i.stpt_quintile_treat* ///
		i.stpt_quintile_post* ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset ///
		if simple_room_type!="" , ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
		vce(cluster roomCluster)
est sto stpt_quintile_het

coefplot stpt_quintile_het , vertical keep(1.stpt_quintile_treat*) ///
		ytitle("Treatment Effect (in degrees F)") xtitle("Quintile of Baseline Thermostat Setpoint")	///
		xlabel(1(1)5) ylabel(-1.2(0.4)1.2, labsize(vsmall)) ///
		graphregion(color(white)) bgcolor(white) yline(0)
graph export "./Results/treateffect_quintile_pooled.png", replace


*** heterogeneity using quartiles of pre-treat setting - pooling Fall and Spring
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

* avg setting before treatment, by room
sort room RoundDateTime
by room : egen avg_stpt_pretreat = mean(CTLSTPT) if email_intervent==0 | email_intervent_spring==0

keep if simple_room_type!=""
keep room avg_stpt_pretreat
duplicates drop
drop if avg_stpt_pretreat==.

xtile stpt_quartile_pretreat = avg_stpt_pretreat, nq(4)

merge 1:m room using "final_analysis.dta", nogen keep(3)

** pooling the 'post' variable for Fall and Spring
gen posttreat = 0 if email_intervent==0 | email_intervent_spring==0
replace posttreat = 1 if email_intervent==1 | email_intervent_spring==1

** Pooling treatment variable for Fall and Spring
gen treat_fa = treat*email_intervent
gen treat_sp = individtreat_spring*email_intervent_spring
gen treat_fasp = treat_fa
replace treat_fasp = treat_sp if treat_fasp==.

* Generate interactions of treated X decile X post-treat
tabulate stpt_quartile_pretreat, gen(stpt_quartile)

forval i = 1/4 {
gen stpt_quartile_treat`i' = treat_fasp*stpt_quartile`i'
}

forval i = 1/4 {
gen stpt_quartile_post`i' = posttreat*stpt_quartile`i'
}

reghdfe CTLSTPT i.stpt_quartile_treat* ///
		i.stpt_quartile_post* ///
		hourlyvisibility hourlydrybulbtempf hourlywetbulbtempf hourlydewpointtempf ///
		hourlyrelativehumidity hourlywindspeed hourlyprecip dailysunrise dailysunset ///
		if simple_room_type!="" , ///
		absorb(i.hourlywinddirection i.single_bedroom i.floor i.wing i.location i.window dayofmonth#monthofyear i.room) ///
		vce(cluster roomCluster)
est sto stpt_quartile_het

coefplot stpt_quartile_het , vertical keep(1.stpt_quartile_treat*) ///
		ytitle("Treatment Effect (in degrees F)") xtitle("Quartile of Baseline Thermostat Setpoint")	///
		xlabel(1(1)4) ylabel(-1.2(0.4)1.2, labsize(vsmall)) ///
		graphregion(color(white)) bgcolor(white) yline(0)
graph export "./Results/treateffect_quartile_pooled.png", replace


********************************************************************************
******* Treatment Effects for Spring - simple emails
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"
use "final_analysis.dta"

label define treatlab2 1 "Spring Treatment"
label values simpletreat_spring treatlab2
label define posttreatlabel2 1 "Post Jan. 31"
label values email_intervent_spring posttreatlabel2

reg CTLSTPT i.simpletreat_spring if email_intervent_spring==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.simpletreat_spring i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_spring==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.simpletreat_spring hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent_spring==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.simpletreat_spring if email_intervent_spring==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if email_intervent_spring==0
by room : egen avg_stpt_prespring = min(aux)
drop aux

reghdfe CTLSTPT i.simpletreat_spring avg_stpt_prespring if email_intervent_spring==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.simpletreat_spring#i.email_intervent_spring if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect_simplespring.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label ///
			keep(1.simpletreat_spring 1.simpletreat_spring#1.email_intervent_spring)

			
********************************************************************************
******* Treatment Effects for Spring
label values individtreat_spring treatlab

** Simple regs on treatment
reg CTLSTPT i.individtreat_spring if email_intervent_spring!=. & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.individtreat_spring i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_spring!=. & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.individtreat_spring if email_intervent_spring!=. & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.date) vce(cluster roomCluster)
est sto teffect_date

reghdfe CTLSTPT i.individtreat_spring if email_intervent_spring!=. & simple_room_type!="" , absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Diff-in diff-estimates
reghdfe CTLSTPT i.individtreat_spring#i.email_intervent_spring if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff


esttab teffect teffect_phys teffect_date teffect_full teffect_diffdiff using "./Results/treat_effect_individspring.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label ///
			keep(1.individtreat_spring 1.individtreat_spring#1.email_intervent_spring)
			
			
			
			
********************************************************************************
********************************************************************************
*************************** SURVEYS
clear all
cd "P:\iSEE\CampusEnergy2018"

* pre-intervention survey
import delimited using "./pre_treat_survey.csv", varnames(1) case(preserve)

** dropping responses with negative "attention check"
drop if Q162!=1

* compiling the responses, cleaning variables
rename *RoomNumber RoomNumber
rename Q2Name Name

sort RoomNumber
replace RoomNumber=substr(RoomNumber, 1, 5) if strlen(RoomNumber)==6
replace RoomNumber="1011D" if _n==1
replace RoomNumber="1023A" if _n==4
replace RoomNumber="2008A" if _n==7
replace RoomNumber="2014A" if _n==11
replace RoomNumber="2030B" if _n==15
replace RoomNumber="2031B" if _n==17
replace RoomNumber="2039B" if _n==18
replace RoomNumber="2039B" if _n==19
replace RoomNumber="2042B" if _n==21
replace RoomNumber="2042A" if _n==22
replace RoomNumber="3010C" if _n==25
replace RoomNumber="3011A" if _n==26
replace RoomNumber="3018B" if _n==28
replace RoomNumber="3022A" if _n==30
replace RoomNumber="3022B" if _n==31
replace RoomNumber="3029B" if _n==33
replace RoomNumber="3029A" if _n==34
replace RoomNumber="3029B" if _n==35
replace RoomNumber="3036A" if _n==37
replace RoomNumber="3037A" if _n==38
replace RoomNumber="3037B" if _n==39
replace RoomNumber="3037A" if _n==40
replace RoomNumber="3039B" if _n==42
replace RoomNumber="3042A" if _n==44
replace RoomNumber="4022C" if _n==49
replace RoomNumber="4023A" if _n==51
replace RoomNumber="4032B" if _n==55
replace RoomNumber="4037B" if _n==58
replace RoomNumber="4037B" if _n==59
replace RoomNumber="4037A" if _n==60
replace RoomNumber="4040B" if _n==63
replace RoomNumber="5011A" if _n==67
replace RoomNumber="5021C" if _n==70
replace RoomNumber="5029B" if _n==72
replace RoomNumber="5032B" if _n==73
replace RoomNumber="5032A" if _n==74
drop if _n==75 | _n==76
replace RoomNumber="6010C" if _n==75
replace RoomNumber="6018A" if _n==78
replace RoomNumber="6027B" if _n==81
replace RoomNumber="6036A" if _n==84
replace RoomNumber="6037A" if _n==85
replace RoomNumber="4021A" if _n==86
replace RoomNumber="6039A" if _n==87
replace RoomNumber="6040A" if _n==88
replace RoomNumber="6042A" if _n==89
replace RoomNumber="4029A" if _n==90
replace RoomNumber="1011A" if _n==91
drop Name
sort RoomNumber
rename RoomNumber Room

forval i = 4/19 {
gen Q`i' =.
replace Q`i' = 1 if Q`i'1==1
replace Q`i' = 2 if Q`i'2==1
replace Q`i' = 3 if Q`i'3==1
replace Q`i' = 4 if Q`i'4==1
replace Q`i' = 5 if Q`i'5==1
}
	
rename Q4 Q1
rename Q5 Q2
rename Q6 Q3
rename Q7 Q4
rename Q8 Q5
rename Q9 Q6
rename Q10 Q7
rename Q11 Q8
rename Q12 Q9
rename Q13 Q10
rename Q14 Q11
rename Q15 Q12
rename Q17 Q13
rename Q18 Q14
rename Q19 Q15
drop Q16 /* that is the attention check */

label var Q1 "We are approaching the limit of the number of people the earth can support."	
label var Q2 "Humans have the right to modify the natural environment to suit their needs."
label var Q3 "When humans interfere with nature it often produces disastrous consequences."
label var Q4 "Human ingenuity will ensure that we do NOT make the earth unlivable."
label var Q5 "Humans are severely abusing the environment."
label var Q6 "The earth has plenty of natural resources if we just learn how to develop them."
label var Q7 "Plants and animals have as much right as humans to exist."
label var Q8 "The balance of nature is strong enough to cope with the impacts of modern industrial nations."
label var Q9 "Despite our special abilities humans are still subject to the laws of nature."
label var Q10 `"The so­called ecological crisis facing humankind has been greatly exaggerated."'
label var Q11 "The earth is like a spaceship with very limited room and resources."
label var Q12 "Humans were meant to rule over the rest of nature."
label var Q13 "The balance of nature is very delicate and easily upset."
label var Q14 "Humans will eventually learn enough about how nature works to be able to control it."
label var Q15 "If things continue on their present course, we will soon experience a major ecological catastrophe."

drop Q41StronglyDisagree-Q195StronglyAgree
drop Q1IConsent

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
merge m:1 Room using "room_randomization_clean.dta"
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

sort Room
forval i = 1(2)15 {
by Room : egen mean_Q`i' = mean(Q`i')
}
forval i = 2(2)14 {
by Room : egen mean_Q`i' = mean(reverse_`i')
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
merge 1:m Room using "final_analysis.dta", nogen keep(3)

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
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"

* post-intervention survey
import delimited using "./post_treat_survey.csv", varnames(1) case(preserve) bindquotes(strict)
drop Q1I*

* merge with room numbers
rename UserIdofSubmittie email
merge 1:1 email using "email_list.dta", nogen keep(1 3)

order Room, first
sort Room
replace Room="4037B" if _n==1
replace Room="2039B" if _n==2
replace Room="3037A" if _n==3
replace Room="6014A" if _n==4
replace Room="2022C" if _n==5
replace Room="6029A" if _n==6
sort Room
drop email

* cleaning, adding labels and redifining variables
label define level 1 "Never" 2 "Rarely" 3 "Sometimes" 4 "Often" 5 "All The Time"
forval i = 2/17 {
gen Q`i' =.
replace Q`i' = 1 if Q`i'Never==1
replace Q`i' = 2 if Q`i'Rarely==1
replace Q`i' = 3 if Q`i'Sometimes==1
replace Q`i' = 4 if Q`i'Often==1
replace Q`i' = 5 if Q`i'Allthetime==1
label values Q`i' level
}

label define level2 1 "Not at All" 2 "Slightly" 3 "Moderately" 4 "Very" 5 "Extremely"
forval i = 18/24 {
gen Q`i' =.
replace Q`i' = 1 if Q`i'Notatallinformative==1
replace Q`i' = 2 if Q`i'Slightlyinformative==1
replace Q`i' = 3 if Q`i'Moderatelyinformative==1
replace Q`i' = 4 if Q`i'Veryinformative==1
replace Q`i' = 5 if Q`i'Extremelyinformative==1
label values Q`i' level2
}

label define level3 1 "Very Disappointed" 2 "Moderately Disappointed" 3 "Slightly Disappointed" 4 "Neither Disappointed nor Pleased" 5 "Slightly Pleased" 6 "Moderately Pleased" 7 "Very Pleased"
forval i = 25/31 {
gen Q`i' =.
replace Q`i' = 1 if Q`i'Verydisappointed==1
replace Q`i' = 2 if Q`i'Moderatelydisappointed==1
replace Q`i' = 3 if Q`i'Slightlydisappointed==1
replace Q`i' = 4 if Q`i'Neitherdisappointednorpleased==1
replace Q`i' = 5 if Q`i'Slightlypleased==1
replace Q`i' = 6 if Q`i'Moderatelypleased==1
replace Q`i' = 7 if Q`i'Verypleased==1
label values Q`i' level3
}

drop SubmittedOn-Q32Pleasesharewithusyourthoughts

label var Q2 `"For cold days, I set the thermostat" "as low as comfortably possible."'
label var Q3 `"I compared my energy reports with those" "of roommates, or other Hall residents."'
label var Q4 `"I lowered the thermostat before stepping out of my dorm."'
label var Q5 `"I opened my energy report email for the week," "to check my energy usage."'
label var Q6 `"I lowered the thermostat in my dorm before going to sleep."'
label var Q7 `"I felt that the thermostat did not work properly," "and did not remain at the temperature that I chose."'
label var Q8 `"I deleted the energy report email," "without reading its contents."'
label var Q9 `"I turned off lights and unplugged appliances, to save electricity."'
label var Q10 `"I felt that my efforts to conserve energy" "did not lead to reports with positive feedback."'
label var Q11 `"I talked about the energy reports with people" "who do not currently live in the Hall."'
label var Q12 `"I closed the draperies and shades on south-facing windows" "during cold nights to reduce the chill I felt from cold windows."'
label var Q13 `"I read the subject line of my energy report email."'
label var Q14 `"I opened the draperies and shades on south­facing windows" "during cold days, to allow the sunlight in."'
label var Q15 `"I blocked or marked the energy report emails as spam."'
label var Q16 `"I talked to my roommates or other Hall" "residents about my energy reports."'
label var Q17 `"I was feeling cold," "so I did not want to lower the thermostat in my dorm."'
label var Q18 `"The email subject line, which presented percent comparisons."'
label var Q19 `"The Last Week Neighbor Comparison graph."'
label var Q20 `"The Past Weeks Neighbor Comparison graph."'
label var Q21 `"The Efficiency Standing box."'
label var Q22 `"The Energy Saving Tips at the bottom."'
label var Q23 `"The bars/lines that showed my OWN energy usage."'
label var Q24 `"The bars/lines that showed the average NEIGHBORS energy usage."'
label var Q25 `"You received a Below Average efficiency rating for the week?"'
label var Q26 `"You received a Good efficiency rating for the week?"'
label var Q27 `"You received a Great efficiency rating for the week?"'
label var Q28 `"The historical graphs revealed that you" "typically use MORE energy than average?"'
label var Q29 `"The historical graphs revealed that you typically use LESS energy than average?"'
label var Q30 `"The historical graphs revealed that you are typically close to an average level of energy usage?"'
label var Q31 `"You tried to lower your energy usage, but still received a poor energy rating for the following week?"'


***** Histograms
forval x = 2/17 {
sum Q`x'
sca mean = r(mean)
histogram Q`x', discrete percent xline(`=round(scalar(mean), 0.01)') ///
		xlabel(1 "Never" 2 "Rarely" 3 "Sometimes" 4 "Often" 5 "All The Time", labcolor(black) angle(45)) ///
		xlabel(`=round(scalar(mean), 0.01)', add custom labcolor(red) labsize(vsmall)) ///
		xscale(range(1(1)5)) start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''") xtitle("")
graph export "./Results/PostSurvey_hist_Q`x'.png", replace
}

forval x = 18/24 {
sum Q`x'
sca mean = r(mean)
histogram Q`x', discrete percent xline(`=round(scalar(mean), 0.01)') ///
		xlabel(1 "Not at All Informative" 2 "Slightly Informative" 3 "Moderately Informative" 4 "Very Informative" 5 "Extremely Informative", labcolor(black) angle(45)) ///
		xlabel(`=round(scalar(mean), 0.01)', add custom labcolor(red) labsize(vsmall)) ///
		xscale(range(1(1)5))start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''") xtitle("")
graph export "./Results/PostSurvey_hist_Q`x'.png", replace
}

forval x = 25/31 {
sum Q`x'
sca mean = r(mean)
local means : di %6.2f scalar(mean)
histogram Q`x', discrete percent xline(`means') ///
		xlabel(1 "Very Disappointed" 2 "Moderately Disappointed" 3 "Slightly Disappointed" 4 "Neither Disappointed nor Pleased" 5 "Slightly Pleased" 6 "Moderately Pleased" 7 "Very Pleased", labcolor(black) angle(45) labsize(vsmall)) ///
		xlabel(`means', add custom labcolor(red) labsize(vsmall) format(%6.2f)) ///
		xscale(range(1(1)5)) start(1) ///
		graphregion(color(white)) bgcolor(white) bcolor(blue%30) barw(0.5) ///
		title("`: variable label Q`x''") xtitle("")
graph export "./Results/PostSurvey_hist_Q`x'.png", replace
}


********************************************************************************
****** Simulating randomizations to check balance
clear all
cd "\\ad.uillinois.edu\aces\ACE\personal\nogueir2\iSEE\CampusEnergy2018"

use "final_analysis.dta"

*levelsof room if treat!=.

encode cluster_type, gen(clusttype)

tabulate clusttype, gen(tab_clusttype)
tabulate floor, gen(tab_floor)
tabulate wing, gen(tab_wing)
tabulate location, gen(tab_location)
tabulate college_desc1, gen(tab_college_desc)

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
******* Energy savings from winter break treatment *****************************
/*
* event study based on start of semester
reghdfe CTLSTPT i.fall1617 if treat==0 & simple_room_type!="" , ///
		absorb(i.room) vce(cluster roomCluster)

reghdfe CTLSTPT i.fall2017 if treat==1 & simple_room_type!="" , ///
		absorb(i.room) vce(cluster roomCluster)


reghdfe CTLSTPT i.fall1617##treat if simple_room_type!="" , ///
		absorb(dayofmonth#monthofyear) vce(cluster roomCluster)

est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent!=. & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat if email_intervent!=. & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.date) vce(cluster roomCluster)
est sto teffect_date

reghdfe CTLSTPT i.treat if email_intervent!=. & simple_room_type!="" , absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Diff-in diff-estimates
reghdfe CTLSTPT i.treat#i.fall_treat i.fall2017 if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff
*/

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
reg CTLSTPT i.treat dist_avg i.lag_bad i.treatXbad distXbad dist_avgXtreat distXbadXtreat [aweight = weight_avg] if email_intervent!=. & simple_room_type!="", vce(cluster roomCluster)
est sto teffect_RD_avg
reg CTLSTPT i.treat dist_eff i.lag_good i.treatXgood distXgood dist_effXtreat distXgoodXtreat [aweight = weight_eff] if email_intervent!=. & simple_room_type!="", vce(cluster roomCluster)
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
by dist_avg_bins : egen ctlstpt_avg = mean(CTLSTPT) if (email_intervent==1 & treat==1 & simple_room_type!="") & (lag_good==1 | lag_bad==1)

sort dist_eff_bins
by dist_eff_bins : egen ctlstpt_eff = mean(CTLSTPT) if (email_intervent==1 & treat==1 & simple_room_type!="") & (lag_good==1 | lag_great==1)

twoway 	(scatter ctlstpt_avg dist_avg_bins if dist_avg<=0.55 & dist_avg>=-0.55, sort mcolor(gs8)) ///
		(lfit CTLSTPT dist_avg if lag_good==1 & dist_avg>=-0.55, fcolor(none) lcolor(red)) ///
		(lfit CTLSTPT dist_avg if lag_bad==1 & dist_avg<=0.55, fcolor(none) lcolor(red)) ///
		if (email_intervent==1 & treat==1 & simple_room_type!="") ///
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
		if (email_intervent==1 & treat==1 & simple_room_type!="") ///
		, xtitle("Distance From Previous Week's Efficient Usage Cutoff" "(kBTU, binned)") ///
		ytitle("Thermostat Setpoint") ///
		xline(0, lcolor(black)) ///
		xlabel(-0.5(0.1)0.5) ///
		legend(order(1 2) lab(1 "Average Setpoint Within Bin") lab(2 "Linear Fit")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Results/RD_efficient.png", replace			


********************************************************************************			
*** What thermostat reductions represent in energy space
clear all
cd "P:\iSEE\CampusEnergy2018"

use "final_analysis.dta"

** simulate a 0.24 reduction in thermostats for control rooms
sort room hourofsample
by room hourofsample : egen hourly_stpt_sim = mean(CTLSTPT-0.24)

* difference between stpt and outdoor temperature
gen outdiff_sim = hourly_stpt_sim - hourlydrybulbtempf
replace outdiff_sim = ceil(outdiff_sim - 0.5)

* usage calculation for control rooms
gen steam_usage = 9.6*0.0472917*outdiff_sim if simple_room_type=="Single Bedroom"
replace steam_usage = 9.6*0.0378844*outdiff_sim if simple_room_type=="Double Bedroom"
replace steam_usage = 0 if outdiff_sim<=0

gen chw_usage = (-0.7942)*outdiff_sim if simple_room_type=="Single Bedroom"
replace chw_usage = (-0.9678)*outdiff_sim if simple_room_type=="Double Bedroom"
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

sum Usage if email_intervent==1 & treat==0 & simple_room_type!=""
sca avg_usage = r(mean)
sum Usage_sim if email_intervent==1 & treat==0 & simple_room_type!=""
sca avg_usage_sim = r(mean)

dis (avg_usage_sim-avg_usage)/avg_usage


********************************************************************************
********************************************************************************
*********** Longer-term treatment effects
clear all
cd "P:\iSEE\CampusEnergy2018"

use "final_analysis.dta"

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
reg CTLSTPT i.always_treat if email_intervent_full==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.always_treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_full==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.always_treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent_full==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.always_treat if email_intervent_full==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if email_intervent_full==0
by room : egen avg_stpt_pre = min(aux)
drop aux

reghdfe CTLSTPT i.always_treat avg_stpt_pre if email_intervent_full==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.always_treat##i.email_intervent_full if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/effect_alwaystreat.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.always_treat 1.always_treat#1.email_intervent_full)
			
/*
*********************************
** define always treatment versus control in specific semester
gen always_treat2 = .
replace always_treat2 = 1 if treat==1 & individtreat_spring==1
replace always_treat2 = 0 if treat==0 & RoundDateTime<1831680000000
replace always_treat2 = 0 if individtreat_spring==0 & RoundDateTime>=1831680000000 /* different control rooms for spring */

** dates which should be used for full year treatment
gen email_intervent_full2 = .
replace email_intervent_full2 = 0 if RoundDateTime>=1819497600000 & RoundDateTime<1820941200000
replace email_intervent_full2 = 1 if RoundDateTime>=1820941200000 & RoundDateTime<1839974400000 /* starting when first email was sent in fall */
replace email_intervent_full2 = . if RoundDateTime<1819497600000  /* first day of fall semester */
replace email_intervent_full2 = . if RoundDateTime>=1839974400000 /* last available date of spring semester */
replace email_intervent_full2 = . if RoundDateTime>=1828915200000 & RoundDateTime<1831680000000 /* dropping winter break */

** regressions comparing always treatment versus always control
label values always_treat2 treatlab
label values email_intervent_full2 posttreatlabel

** Simple regs on treatment
reg CTLSTPT i.always_treat2 if email_intervent_full2==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.always_treat2 i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent_full==1 & simple_room_type!="" , vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.always_treat2 hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent_full2==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.always_treat2 if email_intervent_full2==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
reghdfe CTLSTPT i.always_treat2 avg_stpt_pre if email_intervent_full2==1 & simple_room_type!="" , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe CTLSTPT i.always_treat2##i.email_intervent_full2 if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/effect_alwaystreat2.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.always_treat2 1.always_treat2#1.email_intervent_full2)
*/			

******* graphically
gen stable_date = 0
replace stable_date = 1 if CTLSTPT!=. & email_intervent_full!=. & simple_room_type!=""
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
			& always_treat==1 & email_intervent_full!=. & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_treat
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 & statdate!=21259 & statdate!=21260 & statdate!=21261 ///
			& statdate!=21262 & statdate!=21263 & statdate!=21265 & statdate!=21276 & statdate!=21288 ///
			& always_treat==0 & email_intervent_full!=. & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_control

coefplot stptbydate_control stptbydate_treat, vertical  ///
					xlabel(1 "08/28" 15 "09/11" 30 "09/26" 45 "10/11" 60 "10/28" ///
					75 "11/15" 90 "12/01" 103 "12/14" 119 "01/31" ///
					134 "02/15" 147 "02/28" 162 "03/15" 172 "03/31" 185 "04/15", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Always Control" 4 "Always Treated")) title("Average Setpoints by Date") ///
					keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(16.5) text(73.2 16.6 "Treatment Starts", placement(e) color(red) size(small) orient(vertical) just(left))
graph export "./Results/setpoints_bydate_alwaystreat.png", replace


/*
** Setpoints by date - always treated versus sometimes control
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 & statdate!=21259 & statdate!=21260 & statdate!=21261 ///
			& statdate!=21262 & statdate!=21263 & statdate!=21265 & statdate!=21276 & statdate!=21288 ///
			& always_treat2==1 & email_intervent_full2!=. & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_treat
reg CTLSTPT ibn.date if statdate!=21105 & statdate!=21119 & statdate!=21129 & statdate!=21130 ///
			& statdate!=21131 & statdate!=21147 & statdate!=21259 & statdate!=21260 & statdate!=21261 ///
			& statdate!=21262 & statdate!=21263 & statdate!=21265 & statdate!=21276 & statdate!=21288 ///
			& always_treat2==0 & email_intervent_full2!=. & simple_room_type!="" , nocons vce(cluster room)		
est sto stptbydate_control

coefplot stptbydate_control stptbydate_treat, vertical  ///
					xlabel(1 "08/28" 15 "09/11" 30 "09/26" 45 "10/11" 60 "10/28" ///
					75 "11/15" 90 "12/01" 103 "12/14" 119 "01/31" ///
					134 "02/15" 147 "02/28" 162 "03/15" 172 "03/31" 185 "04/15", labsize(small) angle(45)) ///
					xtitle("Date") ytitle("Temperature (Degrees Fahrenheit)") legend(order(2 "Control" 4 "Always Treated")) title("Average Setpoints by Date") ///
					keep(*date) graphregion(color(white)) bgcolor(white) ///
					xline(16.5) text(72.5 16.6 "Treatment Starts", placement(e) color(red) size(small) orient(vertical) just(left))
graph export "./Results/setpoints_bydate_alwaystreat2.png", replace
*/


********************************************************************************
************** Within room, across-time variance
clear all
cd "P:\iSEE\CampusEnergy2018"

use "final_analysis.dta"

gen outdiff_abs = abs(outdiff)

*** for Fall
sort room RoundDateTime
by room : egen auxvar = sd(CTLSTPT) if email_intervent!=. & simple_room_type!=""
by room : egen sd_stpt = min(auxvar)
drop auxvar
gen var_stpt = sd_stpt^2

sort room RoundDateTime
by room : egen auxvar = sd(outdiff_abs) if email_intervent!=. & simple_room_type!=""
by room : egen sd_outdiff = min(auxvar)
drop auxvar
gen var_outdiff = sd_outdiff^2

sort room RoundDateTime
by room : egen auxvar = sd(CTLSTPT) if email_intervent==1 & simple_room_type!=""
by room : egen sd_stpt2 = min(auxvar)
drop auxvar
gen var_stpt2 = sd_stpt2^2

sort room RoundDateTime
by room : egen auxvar = sd(outdiff_abs) if email_intervent==1 & simple_room_type!=""
by room : egen sd_outdiff2 = min(auxvar)
drop auxvar
gen var_outdiff2 = sd_outdiff2^2


*** for Winter Break
gen post_winteremail = 0 if RoundDateTime>=1827705600000 & RoundDateTime<1828915200000
replace post_winteremail = 1 if RoundDateTime>=1828915200000 & RoundDateTime<=1830383100000

by room : egen auxvar = sd(CTLSTPT) if post_winteremail!=. & simple_room_type!=""
by room : egen sd_stpt_winter = min(auxvar)
drop auxvar
gen var_stpt_winter = sd_stpt_winter^2

by room : egen auxvar = sd(CTLSTPT) if post_winteremail==1 & simple_room_type!=""
by room : egen sd_stpt_winter2 = min(auxvar)
drop auxvar
gen var_stpt_winter2 = sd_stpt_winter2^2


keep if simple_room_type!="" & treat!=.
keep room sd_stpt var_stpt sd_stpt_winter var_stpt_winter var_outdiff sd_outdiff sd_stpt2 var_stpt2 sd_stpt_winter2 var_stpt_winter2 var_outdiff2 sd_outdiff2
duplicates drop

sum sd_stpt sd_stpt2 sd_outdiff sd_outdiff2 sd_stpt_winter sd_stpt_winter2, detail



********************************************************************************			
*** What thermostat reductions represent in energy space - winter break
clear all
cd "P:\iSEE\CampusEnergy2018"

use "final_analysis.dta"

sort room hourofsample
by room hourofsample : egen hourly_stpt_sim = mean(CTLSTPT+1.1)

* difference between stpt and outdoor temperature
gen outdiff_sim = hourly_stpt_sim - hourlydrybulbtempf
replace outdiff_sim = ceil(outdiff_sim - 0.5)

* usage calculation for control rooms
gen steam_usage = 9.6*0.0472917*outdiff_sim if simple_room_type=="Single Bedroom"
replace steam_usage = 9.6*0.0378844*outdiff_sim if simple_room_type=="Double Bedroom"
replace steam_usage = 0 if outdiff_sim<=0

gen chw_usage = (-0.7942)*outdiff_sim if simple_room_type=="Single Bedroom"
replace chw_usage = (-0.9678)*outdiff_sim if simple_room_type=="Double Bedroom"
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

sum Usage if post_winteremail==1 & winterbreak_treat==1 & simple_room_type!=""
sca avg_usage_treat = r(mean)
sum Usage_sim if post_winteremail==1 & winterbreak_treat==1 & simple_room_type!="" & Usage!=.
sca avg_usage_notreat = r(mean)

dis (avg_usage_treat-avg_usage_notreat)/avg_usage_notreat



**************************** checking if there is treat effect for rooms that completed survey
merge m:1 Room using "pre_survey_rooms.dta"
drop if _merge==2
rename _merge pre_survey_rooms

label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Sep. 13"
label values email_intervent posttreatlabel


** Simple regs on treatment
reg CTLSTPT i.treat if email_intervent==1 & simple_room_type!="" & pre_survey_rooms==3, vce(cluster roomCluster)
est sto teffect

reg CTLSTPT i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" & pre_survey_rooms==3, vce(cluster roomCluster)
est sto teffect_phys

reghdfe CTLSTPT i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" & pre_survey_rooms==3, ///
			absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe CTLSTPT i.treat if email_intervent==1 & simple_room_type!="" & pre_survey_rooms==3, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full


*** full disaggregation of treat
reg CTLSTPT i.treat#winterbreak_treat if statdatetime>=1827705600000 & statdatetime<=1833061500000 & simple_room_type!="", vce(cluster roomCluster)


***************************** regressions with alternative outcome
*** outcome is set at 68
gen roundCTLSTPT = round(CTLSTPT)

gen set68 = 0 if roundCTLSTPT!=.
replace set68 = 1 if roundCTLSTPT==68

gen pre_postOct15 = 0 if email_intervent==0
replace pre_postOct15 = 1 if statdatetime>=1823644800000 & email_intervent==1

reg set68 i.treat if email_intervent==1 & simple_room_type!="" & statdatetime>=1823644800000, vce(cluster roomCluster)
est sto teffect

reg set68 i.treat i.floor 2.wing 2.location 3.location 4.location 5.location 6.location ///
			2.window 3.window 4.window ///
			if email_intervent==1 & simple_room_type!="" & statdatetime>=1823644800000, vce(cluster roomCluster)
est sto teffect_phys

reghdfe set68 i.treat hourlydrybulbtempf hourlywindspeed hourlyprecip hourlyrelativehumidity if email_intervent==1 & simple_room_type!="" ///
			& statdatetime>=1823644800000, absorb(i.floor i.wing i.location i.window) vce(cluster roomCluster)
est sto teffect_weather

reghdfe set68 i.treat if email_intervent==1 & simple_room_type!="" & statdatetime>=1823644800000 , ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_full

* Model with average pre-treatment setpoint
sort room RoundDateTime
by room : egen aux = mean(CTLSTPT) if email_intervent==0
by room : egen avg_stpt_pre = min(aux)
drop aux

reghdfe set68 i.treat avg_stpt_pre if email_intervent==1 & simple_room_type!=""  & statdatetime>=1823644800000, ///
			absorb(i.floor i.wing i.location i.window i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_avgpre

* Diff-in diff-estimates
reghdfe set68 i.treat##i.pre_postOct15 if simple_room_type!="" , ///
			absorb(i.room i.RoundDateTime) vce(cluster roomCluster)
est sto teffect_diffdiff

label define treatlab 1 "Treated"
label values treat treatlab
label define posttreatlabel 1 "Post Oct. 15"
label values pre_postOct15 posttreatlabel

esttab teffect teffect_phys teffect_weather teffect_full teffect_avgpre teffect_diffdiff using "./Results/treat_effect_set68.tex", ///
			replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) label keep(1.treat 1.treat#1.pre_postOct15)
