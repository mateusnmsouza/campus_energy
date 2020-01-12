********************************************************************************
********************************************************************************
/*
Social Comparison Nudges Without Monetary Incentives: 
       Evidence from Home Energy Reports
	   
Journal of Environmental Economics and Management, January 2020

Erica Myers and Mateus Souza

University of Illinois at Urbana-Champaign
Department of Agricultural and Consumer Economics

CODE FOR RELATIONSHIP BETWEEN THERMOSTAT SETPOINTS AND ENERGY USAGE

*/

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

cd "P:\iSEE\MyersSouza_DataAndCode"

use "powercalc_data.dta"

drop factordate factorroom samplehour factorhour
******** Cleaning some of the variables and creating fixed effects
gen samplehour = substr(rounddatetime, 1, 13)
replace samplehour = subinstr(samplehour, "T", " ", .)
gen hour = clock(samplehour, "YMD h")
gen hourofday = substr(samplehour, -2, 2)
destring hourofday, replace

ds, has(type string) 
local stringvars = r(varlist)

foreach x of varlist `stringvars'  {
replace `x' = "" if `x'=="NaN"
replace `x' = "" if `x'=="NA"
}

destring aov1 auxtemp ctlstpt ctltemp roomtemp mtr1comd mtr2comd mtr3comd chwbtudemand chwbtudemandconsumption chwbtutot chwbtutotconsumption chwbtutptime chwchwrtemp chwchwstemp chwdeltat chwdemand chwdemandconsumption chwdp chwdp2 chwfcv chwflow chwflowconsumption chwload chwloadconsumption chwrp condmtrfwd condmtrfwdconsumption condmtrrev condmtrrevconsumption condmtrsum condmtrsumconsumption hourlydewpointtempf , replace

sort room
by room: gen roomID = 1 if _n==1
replace roomID = sum(roomID)
replace roomID = . if missing(room)

sort sampleday
by sampleday: gen dateID = 1 if _n==1
replace dateID = sum(dateID)
replace dateID = . if missing(sampleday)

gen dayofsample = date(sampleday, "YMD")
gen monthofyear = month(dayofsample)

sort samplehour
by samplehour: gen hourID = 1 if _n==1
replace hourID = sum(hourID)
replace hourID = . if missing(samplehour)

sort rounddatetime
by rounddatetime: gen timeID = 1 if _n==1
replace timeID = sum(timeID)
replace timeID = . if missing(rounddatetime)

gen tagfirst = .
sort room rounddatetime
by room : replace tagfirst = 1 if _n==1

gen tagfirstday = .
sort dayofsample
by dayofsample : replace tagfirstday = 1 if _n==1

set more off
*** Number of rooms in sample
distinct room
sca distinctrooms = r(ndistinct)

** Number of room clusters
gen roomdigits = substr(room, 3, 2)

gen room_cluster = substr(room, 1, 4)
replace room_cluster = "" if room_type==""
destring room_cluster, replace

sort room_cluster
by room_cluster: gen room_clusterID = 1 if _n==1
replace room_clusterID = sum(room_clusterID)
replace room_clusterID = . if missing(room_cluster)

*** Variable to identify types of clusters
gen cluster_type = "Special Units" if roomdigits=="07"
replace cluster_type = "Single Bedrooms" if room_type=="Single Bedroom"
replace cluster_type = "Single Bedrooms" if room_type=="Accessible Single Room"
replace cluster_type = "Double Bedrooms" if room_type=="Double Bedroom"
replace cluster_type = "Double Bedrooms" if room_type=="Accessible Double Bedroom"
replace cluster_type = "RA Bedrooms" if room_type=="Accessible RA Bedroom"
replace cluster_type = "RA Bedrooms" if room_type=="RA Bedroom"
replace cluster_type = "RA Bedrooms" if roomdigits=="40"

** Number of clusters
quietly : tab room if cluster_type=="Single Bedrooms"
sca n_singleclusters = round(r(r)/4)

quietly : tab room if cluster_type=="Double Bedrooms"
sca n_doubleclusters = round(r(r)/2)

sort room_cluster
by room_cluster : gen obsbycluster = _n
gen tagfirstsingle = obsbycluster if obsbycluster==1 & cluster_type=="Single Bedrooms"
gen tagfirstdouble = obsbycluster if obsbycluster==1 & cluster_type=="Double Bedrooms"

** Adjusting some vars
gen fanon = 0 if do6=="OFF"
replace fanon = 1 if do6=="ON"

sort room
by room: gen NEWroomID = 1 if _n==1 & cluster_type!=""
replace NEWroomID = sum(NEWroomID)
replace NEWroomID = . if missing(room)

*** Creating some room by hour averages
sort room samplehour
by room samplehour : egen hourlystpt = mean(ctlstpt)
by room samplehour : egen hourlymtr1comd = mean(mtr1comd)
by room samplehour : egen hourlymtr2comd = mean(mtr2comd)
by room samplehour : egen hourlyCHWconsumption = total(chwbtutotconsumption), missing

*** Creating interactions with rooms
** Dummies for each room
tab roomID, gen(tab_roomID)

set more off
forval i = 1/377 {
gen mtr1comd_byroom`i' = mtr1comd*tab_roomID`i'
gen mtr2comd_byroom`i' = mtr2comd*tab_roomID`i'
}

set more off
forval i = 1/377 {
gen stpt_byroom`i' = ctlstpt*tab_roomID`i'
}

**** Create and adjust energy usage per square feet:
gen CHW_persqft = chwbtutotconsumption/182600
replace CHW_persqft = . if CHW_persqft<0

gen STEAM_persqft = condmtrfwdconsumption/182600
replace STEAM_persqft = . if STEAM_persqft<0

gen ELEC_persqft = electric_consumption/182600
replace ELEC_persqft = . if ELEC_persqft<0

**** Room square footage:
** Single bedroom = 96
** Double bedroom = 180

foreach x in mtr1comd mtr2comd aov1 hourlydrybulbtempf hourlydewpointtempf hourlyrelativehumidity hourlyprecip {
 destring `x', replace
}


*** Lagged consumption
sort room rounddatetime
by room : gen lag_CHW = chwbtutot[_n-1]
by room : gen lag_STEAM = condmtrfwd[_n-1]
by room : gen lag_ELEC = electric_meter[_n-1]
by room : gen lag_CHW_persqft = CHW_persqft[_n-1]
by room : gen lag_STEAM_persqft = STEAM_persqft[_n-1]
by room : gen lag_ELEC_persqft = ELEC_persqft[_n-1]

**** Demean the set-point variable
gen demean_stpt = ctlstpt - 71.62515

**** Diff the main independent variables
sort room rounddatetime
foreach x in ctlstpt mtr1comd mtr2comd {
by room : gen diff`x' = `x'[_n-1] - `x'
}

**** Lag main independent variables
sort room rounddatetime
foreach x in ctlstpt mtr1comd mtr2comd {
by room : gen lag`x' = `x'[_n-1]
}

gen outdiff = ctlstpt - hourlydrybulbtempf

gen dum_roomtype = 0 if room_type=="Single Bedroom"
replace  dum_roomtype = 1 if room_type=="Double Bedroom"

gen doubleoutdiff = dum_roomtype*outdiff

********************************************************************************
********************************************************************************
******** REGRESSIONS TO ASSESS EFFECT OF THERMOSTAT ON ENERGY USAGE ************
reg chwbtutotconsumption outdiff doubleoutdiff mtr1comd mtr2comd lagmtr1comd lagmtr2comd aov1 hourlydewpointtempf hourlyrelativehumidity hourlyprecip if dayofsample<20820 | dayofsample>20838
est sto CHW_reg
matrix CHW_reg = r(table)

reg condmtrfwdconsumption outdiff doubleoutdiff mtr1comd mtr2comd lagmtr1comd lagmtr2comd aov1 hourlydewpointtempf hourlyrelativehumidity hourlyprecip if dayofsample<20820 | dayofsample>20838
est sto STEAM_reg
matrix STEAM_reg = r(table)

**** Results table, converted to kBTU
sca b1_chw = CHW_reg[1,1]*1000
sca b2_chw = CHW_reg[1,2]*1000
sca se1_chw = CHW_reg[2,1]*1000
sca se2_chw = CHW_reg[2,2]*1000
sca b1_steam = STEAM_reg[1,1]*9.6
sca b2_steam = STEAM_reg[1,2]*9.6
sca se1_steam = STEAM_reg[2,1]*9.6
sca se2_steam = STEAM_reg[2,2]*9.6
matrix resultmat = ((b1_chw, b1_steam) \ (se1_chw, se1_steam) \ (b2_chw, b2_steam) \ (se2_chw, se2_steam))

putexcel set ".\Results\energy_bystpt.xlsx", modify
putexcel C3 = matrix(resultmat)

