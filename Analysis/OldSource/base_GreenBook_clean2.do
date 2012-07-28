**************************
* Federal Reserve Green Book Estimates Main Data Set creation file
* Updated 28 July 2012
* Used Stata 12
**************************

//// Note: GNP used to 1991 (inclusive) and GDP from 1992
//// Note: Implicit deflator used before 1996:Q2 and Chain-weighted PI used from 1996:Q2 onwards 


//////// CPI Data ////////////


import excel "~/Dropbox/GreenBook/Base_Data/GBweb.xls", sheet("Deflator or CW Price Index") firstrow allstring clear

	drop QTR6 QTR7 QTR8 QTR9 GREENBOOK
	destring QTR0 QTR1 QTR2 QTR3 QTR4 QTR5, replace

	replace QTR1 = . if QTR1 == 0
	replace QTR2 = . if QTR2 == 0
	replace QTR1 = . if QTR1 == 0
	replace QTR2 = . if QTR2 == 0
	replace QTR3 = . if QTR3 == 0
	replace QTR4 = . if QTR4 == 0
	replace QTR5 = . if QTR5 == 0
	
	rename QTR0 GB_CPI_QTR0
	rename QTR1 GB_CPI_QTR1
	rename QTR2 GB_CPI_QTR2
	rename QTR3 GB_CPI_QTR3
	rename QTR4 GB_CPI_QTR4
	rename QTR5 GB_CPI_QTR5

	
	label variable GB_CPI_QTR0 "Green Book CPI % change estimate, current quarter"
	label variable GB_CPI_QTR1 "Green Book CPI % change estimate, current quarter+1"
	label variable GB_CPI_QTR2 "Green Book CPI % change estimate, current quarter+2"
	label variable GB_CPI_QTR3 "Green Book CPI % change estimate, current quarter+3"
	label variable GB_CPI_QTR4 "Green Book CPI % change estimate, current quarter+4"
	label variable GB_CPI_QTR4 "Green Book CPI % change estimate, current quarter+5"
	label variable GB_CPI_QTR5 "Green Book CPI % change estimate, current quarter+6"

	rename DATE Quarter
	destring Quarter, replace
	sort Quarter 
	order Quarter

save "~/Dropbox/GreenBook/Base_Data/cpi_estimates_and_real.dta", replace

*********
** GNP Deflator (beginning to 1991) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ 

import excel "~/Dropbox/GreenBook/Base_data/FRED_GNP_implicit_deflator.xls", sheet("FRED Graph") firstrow allstring clear
		
		gen quarter = substr(observation_date, 1, 5)
		gen Year = substr(observation_date, 6, 4)
		drop observation_date
		
			
		replace quarter = ".4" if quarter == "01oct"
		replace quarter = ".3" if quarter == "01jul"
		replace quarter = ".2" if quarter == "01apr"
		replace quarter = ".1" if quarter == "01jan"
		
		gen Quarter = Year + quarter
		
		drop quarter Year
		order Quarter

	rename GNPDEF_PC1 deflator
		destring Quarter deflator, replace

		drop if Quarter < 1965.4
		drop if Quarter >= 1992.1

		sort Quarter
	
		label variable deflator "GNP, GDP, GDP chain-type deflator (see Green Book), quarterly % change (FRED 2011)"
	
save "~/Dropbox/GreenBook/Base_Data/clean_FRED_gnp_deflator.dta", replace

///// GDP Deflator (1992 to 1996:Q1) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ /////

import excel "~/Dropbox/GreenBook/Base_data/FRED_GDP_implicit_deflator.xls", sheet("FRED Graph") firstrow allstring  clear

		gen quarter = substr(observation_date, 1, 5)
		gen Year = substr(observation_date, 6, 4)
		drop observation_date
		
			
		replace quarter = ".4" if quarter == "01oct"
		replace quarter = ".3" if quarter == "01jul"
		replace quarter = ".2" if quarter == "01apr"
		replace quarter = ".1" if quarter == "01jan"
		
		gen Quarter = Year + quarter
		
		drop quarter Year
		order Quarter

	rename GDPDEF_PC1 deflator
		destring Quarter deflator, replace

		drop if Quarter < 1992.1
		drop if Quarter > 1996.1


		sort Quarter
	
		label variable deflator "GNP, GDP, GDP chain-type deflator (see Green Book), quarterly % change (FRED 2011)"
	
save "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp_deflator.dta", replace

*********
** GDP chain-type PI (1996:Q2 to present) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ 

import excel "~/Dropbox/GreenBook/Base_data/FRED_GDP_chain_PI.xls", sheet("FRED Graph") firstrow allstring clear

		gen quarter = substr(observation_date, 1, 5)
		gen Year = substr(observation_date, 6, 4)
		drop observation_date
		
			
		replace quarter = ".4" if quarter == "01oct"
		replace quarter = ".3" if quarter == "01jul"
		replace quarter = ".2" if quarter == "01apr"
		replace quarter = ".1" if quarter == "01jan"
		
		gen Quarter = Year + quarter
		
		drop quarter Year
		order Quarter

	rename GDPCTPI_PC1 deflator
		destring Quarter deflator, replace

		sort Quarter
	
		label variable deflator "GNP, GDP, GDP chain-type deflator (see Green Book), quarterly % change (FRED 2011)"
	
save "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp_chain.dta", replace


*********
** CPI (all quarters) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ 

import excel "~/Dropbox/GreenBook/Base_data/FRED_cpi.xls", sheet("FRED Graph") firstrow allstring clear

		gen quarter = substr(observation_date, 1, 5)
		gen Year = substr(observation_date, 6, 4)
		drop observation_date
		
			
		replace quarter = ".4" if quarter == "01oct"
		replace quarter = ".3" if quarter == "01jul"
		replace quarter = ".2" if quarter == "01apr"
		replace quarter = ".1" if quarter == "01jan"
		
		gen Quarter = Year + quarter
		
		drop quarter Year
		order Quarter

	rename CPIAUCSL_PC1 cpi_change
		destring Quarter cpi_change, replace

		sort Quarter
	
		label variable cpi_change "CPI Change, quarterly % change (FRED 2011)"

save "~/Dropbox/GreenBook/Base_Data/clean_FRED_cpi.dta", replace

***********************************************
**** Append deflator files

use "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp_chain.dta", clear

append using "~/Dropbox/GreenBook/Base_Data/clean_FRED_gnp_deflator.dta" "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp_deflator.dta"
	
	sort Quarter
	
merge Quarter using "~/Dropbox/GreenBook/Base_Data/clean_FRED_cpi.dta"
	drop if _merge != 3
	drop _merge
	sort Quarter
	
save "~/Dropbox/GreenBook/Base_Data/deflator_combined.dta", replace

***********************************************
**** Partisan composition of Congress. Base data from http://www.infoplease.com/ipa/A0774721.html
*** Transform to be in terms of Democrats/Republicans



**** Merge Files


use "~/Dropbox/GreenBook/Base_Data/cpi_estimates_and_real.dta", clear
	merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/deflator_combined.dta"
	drop if _merge != 3
	drop _merge
	duplicates drop Quarter, force
	sort Quarter
	
	
	merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/pres_elections_quarter.dta"
	drop if _merge != 3
	drop _merge
	duplicates drop Quarter, force
	sort Quarter
	
	label variable pres_election "Quarter with US presidential election"
	label variable time_to_election "Quarters until the next presidential election quarter"
	label variable pres_part "President's party, 0 = Dem., 1 = Rep."
	
**** Partisan composition of Congress. Base data from http://www.infoplease.com/ipa/A0774721.html
*** Transformed to be in terms of Democrats/Republicans
	use "~/Dropbox/GreenBook/GB_FRED_cpi.dta", clear
		egen id = group(Quarter)
		sort id
	merge id using "/General_Data/us_congress_1965_2013.dta"
		drop if _merge == 2
		drop _merge
		drop id


saveold "~/Dropbox/GreenBook/GB_FRED_cpi.dta", replace

***********************************************************
***********************************************************
*************** GNP/GDP % Change **************************

******************* GNP up to and including 1991, GDP from 1992

import excel "~/Dropbox/GreenBook/Base_Data/GBweb.xls", sheet("Real GNP-GDP") firstrow allstring clear

	drop QTR6 QTR7 QTR8 QTR9 GREENBOOK
	destring QTR0 QTR1 QTR2 QTR3 QTR4 QTR5, replace

	replace QTR1 = . if QTR1 == 0
	replace QTR2 = . if QTR2 == 0
	replace QTR3 = . if QTR3 == 0
	replace QTR4 = . if QTR4 == 0
	replace QTR5 = . if QTR5 == 0

	
	rename QTR0 GB_growth_QTR0
	rename QTR1 GB_growth_QTR1
	rename QTR2 GB_growth_QTR2
	rename QTR3 GB_growth_QTR3
	rename QTR4 GB_growth_QTR4
	rename QTR5 GB_growth_QTR5

	
	label variable GB_growth_QTR0 "Green Book GNP/GDP % change estimate, current quarter"
	label variable GB_growth_QTR1 "Green Book GNP/GDP % change estimate, current quarter+1"
	label variable GB_growth_QTR2 "Green Book GNP/GDP % change estimate, current quarter+2"
	label variable GB_growth_QTR3 "Green Book GNP/GDP % change estimate, current quarter+3"
	label variable GB_growth_QTR4 "Green Book GNP/GDP % change estimate, current quarter+4"
	label variable GB_growth_QTR4 "Green Book GNP/GDP % change estimate, current quarter+5"
	label variable GB_growth_QTR5 "Green Book GNP/GDP % change estimate, current quarter+6"

	rename DATE Quarter
	destring Quarter, replace
	sort Quarter 
	order Quarter

save "~/Dropbox/GreenBook/Base_Data/growth_estimates_and_real.dta", replace


****** GNP Change Actual up to 1992:Q1,  Quarterly % Change, Data from FRED www.http://research.stlouisfed.org/ 

import excel "~/Dropbox/GreenBook/Base_data/FRED_gnp.xls", sheet("FRED Graph") firstrow allstring clear
		
		gen quarter = substr(observation_date, 1, 5)
		gen Year = substr(observation_date, 6, 4)
		drop observation_date
		
			
		replace quarter = ".4" if quarter == "01oct"
		replace quarter = ".3" if quarter == "01jul"
		replace quarter = ".2" if quarter == "01apr"
		replace quarter = ".1" if quarter == "01jan"
		
		gen Quarter = Year + quarter
		
		drop quarter Year
		order Quarter
	
	rename GNPC96_PC1 actual_growth
		destring Quarter actual_growth, replace
		
		drop if Quarter > 1992.1
		
		sort Quarter
		
		label variable actual_growth "GNP, GDP real growth (see Green Book), quarterly % change (FRED 2011)"
		
save "~/Dropbox/GreenBook/Base_Data/clean_FRED_gnp.dta", replace

****** GDP Change Actual from 1992:Q1,  Quarterly % Change, Data from FRED www.http://research.stlouisfed.org/ 

import excel "~/Dropbox/GreenBook/Base_data/FRED_gdp.xls", sheet("FRED Graph") firstrow allstring clear
		
		gen quarter = substr(observation_date, 1, 5)
		gen Year = substr(observation_date, 6, 4)
		drop observation_date
		
			
		replace quarter = ".4" if quarter == "01oct"
		replace quarter = ".3" if quarter == "01jul"
		replace quarter = ".2" if quarter == "01apr"
		replace quarter = ".1" if quarter == "01jan"
		
		gen Quarter = Year + quarter
		
		drop quarter Year
		order Quarter
	
	rename GDPC1_PC1 actual_growth
		destring Quarter actual_growth, replace
		
		drop if Quarter < 1992.1
		
		sort Quarter
		
		label variable actual_growth "GNP, GDP real growth (see Green Book), quarterly % change (FRED 2011)"
		
save "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp.dta", replace

***********************************************
**** Append growth files

use "~/Dropbox/GreenBook/Base_Data/clean_FRED_gnp.dta", clear

append using "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp.dta"

	sort Quarter
	
	save "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp_gnp.dta", replace
	
********************** Other Data **********

** Merge with main "growth" file

use "~/Dropbox/GreenBook/Base_Data/growth_estimates_and_real.dta", clear
	merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/clean_FRED_gdp_gnp.dta"
	drop if _merge != 3
	duplicates drop Quarter, force
	drop _merge
	sort Quarter
	
	merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/pres_elections_quarter.dta"
	drop if _merge != 3
	drop _merge
	duplicates drop Quarter, force
	sort Quarter
	
	label variable pres_election "Quarter with US presidential election"
	label variable time_to_election "Quarters until the next presidential election quarter"
	label variable pres_part "President's party, 0 = Dem., 1 = Rep."
	
**** Partisan composition of Congress. Base data from http://www.infoplease.com/ipa/A0774721.html
*** Transformed to be in terms of Democrats/Republicans
	use "~/Dropbox/GreenBook/GB_FRED_growth.dta", clear
		egen id = group(Quarter)
		sort id
	merge id using "/General_Data/us_congress_1965_2013.dta"
		drop if _merge == 2
		drop _merge
		drop id
	
saveold "~/Dropbox/GreenBook/GB_FRED_growth.dta", replace
*/

** Manufacturing Output (% change from same quarter in previous year) from FRED

insheet using "~/Dropbox/GreenBook/Base_Data/manufOutput.csv", clear

		gen decQuarter = substr(quarter, 5, 6)
		gen Year = substr(quarter, 1, 4)
		drop quarter
		
			
		replace decQuarter = ".4" if decQuarter == "-10-01"
		replace decQuarter = ".3" if decQuarter == "-07-01"
		replace decQuarter = ".2" if decQuarter == "-04-01"
		replace decQuarter = ".1" if decQuarter == "-01-01"
		
		gen Quarter = Year + decQuarter

		drop Year decQuarter
		
		order Quarter	
		destring Quarter, replace
		
		sort Quarter	
		
		label variable manufoutput "US manufacturing output % change from same quarter last year (FRED)"
		
	saveold "~/Dropbox/GreenBook/Base_Data/FRED_manufOutputClean.dta", replace
	
	** Merge with Growth Green CPI
	use "~/Dropbox/GreenBook/GB_FRED_cpi.dta", clear
		merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/FRED_manufOutputClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "~/Dropbox/GreenBook/GB_FRED_cpi.dta", replace

	** Merge with Growth Green Book
	use "~/Dropbox/GreenBook/GB_FRED_growth.dta", clear
		merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/FRED_manufOutputClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "~/Dropbox/GreenBook/GB_FRED_growth.dta", replace


** Recession Dummy from FRED

insheet using "~/Dropbox/GreenBook/Base_Data/recession.csv", clear

		gen decQuarter = substr(quarter, 1, 6)
		gen Year = substr(quarter, 7, 4)
		drop quarter
		
			
		replace decQuarter = ".4" if decQuarter == "01/10/"
		replace decQuarter = ".3" if decQuarter == "01/07/"
		replace decQuarter = ".2" if decQuarter == "01/04/"
		replace decQuarter = ".1" if decQuarter == "01/01/"
		
		gen Quarter = Year + decQuarter

		drop Year decQuarter
		
		order Quarter	
		destring Quarter, replace
		
		sort Quarter	
		
		label variable recession "US recession dummy (FRED)"
		
	saveold "~/Dropbox/GreenBook/Base_Data/FRED_recessionClean.dta", replace
	
	** Merge with Growth Green CPI
	use "~/Dropbox/GreenBook/GB_FRED_cpi.dta", clear
		merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/FRED_recessionClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "~/Dropbox/GreenBook/GB_FRED_cpi.dta", replace

	** Merge with Growth Green Book
	use "~/Dropbox/GreenBook/GB_FRED_growth.dta", clear
		merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/FRED_recessionClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "~/Dropbox/GreenBook/GB_FRED_growth.dta", replace
	
** Real retail and food sales from FRED

insheet using "~/Dropbox/GreenBook/Base_Data/retailFoodSales.csv", clear

		gen decQuarter = substr(quarter, 1, 6)
		gen Year = substr(quarter, 7, 4)
		drop quarter
		
			
		replace decQuarter = ".4" if decQuarter == "01/10/"
		replace decQuarter = ".3" if decQuarter == "01/07/"
		replace decQuarter = ".2" if decQuarter == "01/04/"
		replace decQuarter = ".1" if decQuarter == "01/01/"
		
		gen Quarter = Year + decQuarter

		drop Year decQuarter
		
		order Quarter	
		destring Quarter, replace
		
		sort Quarter	
		
		label variable sales "US real retail & food sales % change from quarter in previous year (FRED)"
		
	saveold "~/Dropbox/GreenBook/Base_Data/FRED_salesClean.dta", replace
	
	** Merge with Growth Green CPI
	use "~/Dropbox/GreenBook/GB_FRED_cpi.dta", clear
		merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/FRED_salesClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "~/Dropbox/GreenBook/GB_FRED_cpi.dta", replace

	** Merge with Growth Green Book
	use "~/Dropbox/GreenBook/GB_FRED_growth.dta", clear
		merge m:m Quarter using "~/Dropbox/GreenBook/Base_Data/FRED_salesClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "~/Dropbox/GreenBook/GB_FRED_growth.dta", replace
	
	
	**** Other variables were added later on. Documentation of their addition is available in other source files and the paper's markup files.


