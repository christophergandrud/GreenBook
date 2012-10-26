**************************
* Federal Reserve Green Book Estimates Compared to Actual Updated Fall 2012
* Christopher Gandrud
* Updated 26 October 2012
* Using Stata 12.1
**************************



//// Note: GNP used to 1991 (inclusive) and GDP from 1992
//// Note: Implicit deflator used before 1996:Q2 and Chain-weighted PI used from 1996:Q2 onwards 


//////// CPI Data ////////////

** Data from http://www.phil.frb.org/research-and-data/real-time-center/greenbook-data/philadelphia-data-set.cfm
** Accessed 26 October 2012

import excel "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/GBweb.xls", sheet("PGDPdot") firstrow clear

	drop GB_PGDPdot8 GB_PGDPdot9 GB_PGDPdot10 GB_PGDPdot11 GBdate
		
	rename GB_PGDPdot2 GB_CPI_QTR0
	rename GB_PGDPdot3 GB_CPI_QTR1
	rename GB_PGDPdot4 GB_CPI_QTR2
	rename GB_PGDPdot5 GB_CPI_QTR3
	rename GB_PGDPdot6 GB_CPI_QTR4
	rename GB_PGDPdot7 GB_CPI_QTR5
	
	replace GB_CPI_QTR0 = . if GB_CPI_QTR0 == 0
	replace GB_CPI_QTR1 = . if GB_CPI_QTR1 == 0
	replace GB_CPI_QTR2 = . if GB_CPI_QTR2 == 0
	replace GB_CPI_QTR3 = . if GB_CPI_QTR3 == 0
	replace GB_CPI_QTR4 = . if GB_CPI_QTR4 == 0
	replace GB_CPI_QTR5 = . if GB_CPI_QTR5 == 0
	
	rename DATE Quarter
	destring Quarter, replace
	sort Quarter 
	order Quarter
	
save "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/cpi_estimates_and_real.dta", replace

*********
** GNP Deflator (beginning to 1991) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ 

import excel "/Users/christophergandrud/Dropbox/GreenBook/Base_data/FRED_GNP_implicit_deflator.xls", sheet("FRED Graph") firstrow allstring clear
		
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
	
save "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_gnp_deflator.dta", replace


///// GDP Deflator (1992 to 1996:Q1) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ /////

import excel "/Users/christophergandrud/Dropbox/GreenBook/Base_data/FRED_GDP_implicit_deflator.xls", sheet("FRED Graph") firstrow allstring  clear

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
	
save "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_gdp_deflator.dta", replace

*********
** GDP chain-type PI (1996:Q2 to present) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ 

import excel "/Users/christophergandrud/Dropbox/GreenBook/Base_data/FRED_GDP_chain_PI.xls", sheet("FRED Graph") firstrow allstring clear

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
	
save "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_gdp_chain.dta", replace

*********
** CPI (all quarters) Quarterly % Change Data from FRED www.http://research.stlouisfed.org/ 

import excel "/Users/christophergandrud/Dropbox/GreenBook/Base_data/FRED_cpi.xls", sheet("FRED Graph") firstrow allstring clear

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

save "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_cpi.dta", replace

***********************************************
**** Append deflator files

use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_gdp_chain.dta", clear

append using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_gnp_deflator.dta" "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_gdp_deflator.dta"
	
	sort Quarter
	
merge Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/clean_FRED_cpi.dta"
	drop if _merge != 3
	drop _merge
	sort Quarter
	
save "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/deflator_combined.dta", replace

**** Merge Files


use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/cpi_estimates_and_real.dta", clear
	merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/deflator_combined.dta"
	drop if _merge != 3
	drop _merge
	duplicates drop Quarter, force
	sort Quarter
	
	
***********************************************
**** Presidential Party & Election Variables

* President
	merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/PresidentBase.dta"
	drop if _merge != 3
	drop _merge
	duplicates drop Quarter, force
	sort Quarter
	
	* Reverse Rep and Dem
	gen PresParty = 1 if pres_party == 0
	replace PresParty = 0 if pres_party == 1
	drop pres_party
	rename PresParty pres_party

* Elections
	merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/pres_elections_quarter.dta"
	drop if _merge != 3
	drop _merge
	duplicates drop Quarter, force
	sort Quarter
	
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", replace
	
**** Partisan composition of Congress. Base data from http://www.infoplease.com/ipa/A0774721.html
*** Transformed to be in terms of Democrats/Republicans
	use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", clear
		egen id = group(Quarter)
		sort id
	merge id using "/General_Data/Partisan_Congress/us_congress_1965_2013.dta"
		drop if _merge == 2
		drop _merge
		drop id


saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", replace

** Manufacturing Output (% change from same quarter in previous year) from FRED

insheet using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/manufOutput.csv", clear

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
		
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FRED_manufOutputClean.dta", replace
	
	** Merge with Growth Green CPI
	use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", clear
		merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FRED_manufOutputClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", replace
	
** Recession Dummy from FRED

insheet using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/recession.csv", clear

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
		
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FRED_recessionClean.dta", replace

	** Merge with Green CPI
	use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", clear
		merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FRED_recessionClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", replace
	
** Real retail and food sales from FRED

insheet using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/retailFoodSales.csv", clear

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
		
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FRED_salesClean.dta", replace
	
	** Merge with Growth Green CPI
	use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", clear
		merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FRED_salesClean.dta"
		
		drop if _merge == 2
		drop _merge
		
	saveold "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", replace



