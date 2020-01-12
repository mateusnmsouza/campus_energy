********************************************************************************
********************************************************************************
/*
Social Comparison Nudges Without Monetary Incentives: 
       Evidence from Home Energy Reports
	   
Journal of Environmental Economics and Management, January 2020

Erica Myers and Mateus Souza

University of Illinois at Urbana-Champaign
Department of Agricultural and Consumer Economics

CODE FOR POWER CALCULATIONS PERFORMED PRIOR TO THE RANDOMIZED CONTROLLED TRIAL

*/


*** install packages
foreach x in winsor2 reghdfe ftools {
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

use "powercalc_data.dta"

******** Cleaning some of the variables and creating fixed effects
drop factordate factorroom factorhour samplehour

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

destring ctltemp hourlyrelativehumidity, replace

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


*** Interact weather variables with room structure
set more off
foreach x in hourlydrybulbtempf hourlyrelativehumidity hourlyprecip {
	destring `x', replace
}

foreach x in room_type floor wing wing_detail window_position {
	tabulate `x', gen(tab_`x')
}

forval i = 1/6 {
	gen tempXfloor`i' = hourlydrybulbtempf*tab_floor`i'
	gen humidXfloor`i' = hourlyrelativehumidity*tab_floor`i'
	gen precipXfloor`i' = hourlyprecip*tab_floor`i'
	
	gen tempXwingdetail`i' = hourlydrybulbtempf*tab_wing_detail`i'
	gen humidXwingdetail`i' = hourlyrelativehumidity*tab_wing_detail`i'
	gen precipXwingdetail`i' = hourlyprecip*tab_wing_detail`i'
}

forval i = 1/7 {
	gen tempXroomtype`i' = hourlydrybulbtempf*tab_room_type`i'
	gen humidXroomtype`i' = hourlyrelativehumidity*tab_room_type`i'
	gen precipXroomtype`i' = hourlyprecip*tab_room_type`i'
}

forval i = 1/4 {
	gen tempXwindowposition`i' = hourlydrybulbtempf*tab_window_position`i'
	gen humidXwindowposition`i' = hourlyrelativehumidity*tab_window_position`i'
	gen precipXwindowposition`i' = hourlyprecip*tab_window_position`i'
}



set more off
*** Number of rooms in sample
distinct room
sca distinctrooms = r(ndistinct)

** Number of room clusters
gen roomdigits = substr(room, 3, 2)

gen room_cluster = substr(room, 1, 4)
replace room_cluster = "" if room_type==""
replace room_cluster = "" if roomdigits=="40"

sort room_cluster
by room_cluster: gen room_clusterID = 1 if _n==1
replace room_clusterID = sum(room_clusterID)
replace room_clusterID = . if missing(room_cluster)

gen tagfirstcluster = .
sort room_cluster room rounddatetime
by room_cluster : replace tagfirstcluster = 1 if _n==1

quietly : tab tagfirstcluster if room_type=="Single Bedroom"
sca n_singleclusters = r(N)

quietly : tab tagfirstcluster if room_type=="Double Bedroom"
sca n_doubleclusters = r(N)

**************************** POWER CALCS ***************************************
set more off

foreach effect in .5 .6 .7 .8 {   // loop over effect sizes (% of treated rooms that change thrmostat setting)
	matrix alliters = ( . , . , . , . , ., . )
		forval i=1/100 {   // number of iterations
		* Randomizing rooms to be treated
		set more off
		gen rand_single = runiform() if tagfirstcluster==1 & room_type=="Single Bedroom"
		gen rand_double = runiform() if tagfirstcluster==1 & room_type=="Double Bedroom"		
		sort rand_single
		gen treat_single = 0 if rand_single!=. | rand_double!=.
		replace treat_single = 1 if _n<=(1/3)*n_singleclusters		// assign 1/3 of single-bedroom clusters to treatment 1
		gen treat_single2 = 0 if treat_single==0
		replace treat_single2 = 1 if _n>(2/3)*n_singleclusters	& rand_single!=. // assign 1/3 of single-bedroom clusters to treatment 2

		sort rand_double
		gen treat_double = 0 if rand_single!=. | rand_double!=.
		replace treat_double = 1 if _n<=(1/3)*n_doubleclusters		// assign 1/3 of double-bedroom clusters to treatment 1
		gen treat_double2 = 0 if treat_double==0
		replace treat_double2 = 1 if _n>(2/3)*n_doubleclusters	& rand_double!=. // assign 1/3 of double-bedroom clusters to treatment 2

		egen treat1 = rowtotal(treat_single treat_double), missing
		egen treat2 = rowtotal(treat_single2 treat_double2), missing
		sort room_cluster room rounddatetime
		by room_cluster : replace treat1 = treat1[1]
		by room_cluster : replace treat2 = treat2[1]
		replace treat1 = . if room_type!="Single Bedroom" & room_type!="Double Bedroom"
		replace treat2 = . if room_type!="Single Bedroom" & room_type!="Double Bedroom"
		
		* Randomizing days and rooms of sample which will have an effect (also determies the intensity of effect)
		gen post = 0 if dayofsample<20851
		replace post = 1 if dayofsample>=20851  // assuming treatment starts March 15
		gen postxtreat1 = 0 if treat1!=.
		replace postxtreat1 = 1 if treat1==1 & post==1
		gen postxtreat2 = 0 if treat2!=.
		replace postxtreat2 = 1 if treat2==1 & post==1
		quietly : sum postxtreat1 if postxtreat1==1
		sca n_postxtreatobs1 = r(N)
		quietly : sum postxtreat2 if postxtreat2==1
		sca n_postxtreatobs2 = r(N)
		
		gen rand_effectday1 = runiform() if postxtreat1==1
		sort rand_effectday1
		gen treat_effect1 = 1 if _n<=`effect'*n_postxtreatobs1
		*gen treat_effect1 = 1 if _n<=0.6*n_postxtreatobs1
		
		gen rand_effectday2 = runiform() if postxtreat2==1
		sort rand_effectday2
		gen treat_effect2 = 1 if _n<=(`effect'+0.05)*n_postxtreatobs2
		*gen treat_effect2 = 1 if _n<=0.8*n_postxtreatobs2
		
		* Create mock treatment effect
		gen mock_stpt = ctlstpt
		replace mock_stpt = ctlstpt + 1 if postxtreat1==1 & treat_effect1==1 // create "mock treatment effect" for treatment 1
		replace mock_stpt = ctlstpt + 1 if postxtreat2==1 & treat_effect2==1 // create "mock treatment effect" for treatment 2	
		
		reghdfe mock_stpt postxtreat1 postxtreat2 , absorb(i.roomID i.timeID) vce(cluster room_clusterID)
		matrix coefs = e(b)
		sca beta1_iter`i' = coefs[1,1]
		sca beta2_iter`i' = coefs[1,2]
		matrix vars = e(V)
		sca sd1_iter`i' = vars[1,1]^(1/2)
		sca sd2_iter`i' = vars[2,2]^(1/2)
		sca t1_iter`i' = abs(beta1_iter`i'/(sd1_iter`i'))
		sca p1_iter`i' = 2*ttail(e(df_r), t1_iter`i')
		dis p1_iter`i'
		sca t2_iter`i' = abs(beta2_iter`i'/(sd2_iter`i'))
		sca p2_iter`i' = 2*ttail(e(df_r), t2_iter`i')
		dis p2_iter`i'
		matrix est_iter`i' = (beta1_iter`i', sd1_iter`i', p1_iter`i', beta2_iter`i', sd2_iter`i', p2_iter`i')
		matrix alliters = (alliters \ est_iter`i')
		
		drop rand_single rand_double treat_single treat_double treat_single2 treat_double2 treat1 treat2 post postxtreat1 postxtreat2 rand_effectday1 rand_effectday2 treat_effect1 treat_effect2 mock_stpt 
		}
	putexcel set ".\Results\ClusterPowerCalcs.xlsx", modify sheet("`effect'")
	putexcel C3 = matrix(alliters)
}	

