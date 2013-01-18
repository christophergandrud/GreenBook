############
# Greenbook Data Merge Mid-June & Updated for the 2006 Data in October 2012 (Part2)
# Christopher Gandrud
# 17 January 2013
############


##### Stata Code ####

use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012/JuneOct2012Data.dta", clear
destring Quarter, replace
sort Quarter

label variable DebtGDP "Nominal federal gov. debt to nominal GDP, (FRED)"
label variable ExpenditureGDP "Nominal federal gov. expenditure to nominal GDP, (FRED)"
label variable PotentialGDP "Nominal potential GDP to nominal GDP, (FRED)"

save, replace

use "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/Update2006/GB_FRED_cpi.dta", clear
sort Quarter

outsheet using "/git_repositories/GreenBook/Data/GB_FRED_cpi_2006.csv", comma replace

merge m:m Quarter using "/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012/JuneOct2012Data.dta"
drop if _merge == 2
drop _merge
save, replace

outsheet using "/git_repositories/GreenBook/Data/GB_FRED_cpi_2006.csv", comma replace