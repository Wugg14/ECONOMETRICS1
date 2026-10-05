*** econ5813pa2.do ***
*** ------------------------------- ***
*** Created by Mark Steininger ***
***************************************
********************************************************
*** This program creates variables and estimates ***
*** regressions for ECON 5813 Project Assignment #2 ***
********************************************************
**************************
*** Begin the log file ***
**************************
capture log close
log using econ5813pa2_IPUMS_EXTRACT.log, replace
set more off
set linesize 255
set varabbrev off

*************************************************
*** Load the ACS data (2010 - 2024) ***
*** ----------------------------------------- ***
*** Downloaded from https://usa.ipums.org/usa ***
*************************************************
clear all
use ./econ5813_acs_d1.dta
describe, short

****************************************************
*** Check that the data is correct ***
*** -------------------------------------------- ***
*** Modify this to be consistent with your data. ***
*** Add additional data integrity checks as you ***
*** think of them. ***
****************************************************
* Data includes years 2010 through 2024
tab year, missing
assert inrange(year,2010,2024)==1 & r(r)==15

* Data includes individuals between the ages of 25 and 59
tab age, missing
assert inrange(age,25,59)==1
* Data includes individuals who are employed
tab empstat, missing
assert empstat==1

********************************************************************************
*** Limit sample to specific occupations                                    ***
*** Occupations: Logisticians, and Shipping, Receiving, and Traffic Clerks   ***
********************************************************************************
keep if occ==0700 | occ==5610
describe, short


*****************
*** Qestion 1 ***
*****************

*************
*** Q1 A. ***
*************
*** Recreate Weeks Worked for this Calculation***
generate weeksworked = .
replace weeksworked = 7 if wkswork2==1
replace weeksworked = 20 if wkswork2==2
replace weeksworked = 33 if wkswork2==3
replace weeksworked = 43.5 if wkswork2==4
replace weeksworked = 48.5 if wkswork2==5
replace weeksworked = 51 if wkswork2==6
label variable weeksworked "Weeks worked"
tab weeksworked, missing
assert inlist(weeksworked,7,20,33,43.5,48.5,51)==1 & r(r)==6

generate wage = 0
replace wage = incwage / (uhrswork * weeksworked)
label variable wage "Wage Calculation"

*************
*** Q1 B. ***
*************
generate female = 0
replace female = 1 if sex == 2
label variable wage "Female Dummy Variable"

*************
*** Q1 C. ***
*************
gen float schoolyr = 0
replace schoolyr = 2.5 if educd<=17
replace schoolyr = 5.5 if educd>=20 & educd<=23
replace schoolyr = 7.5 if educd>=24 & educd<=26
replace schoolyr = 9 if educd==30
replace schoolyr = 10 if educd==40
replace schoolyr = 11 if educd==50
replace schoolyr = 12 if educd>=60 & educd<=64
replace schoolyr = 13 if educd>=65 & educd<=71
replace schoolyr = 14 if educd>=80 & educd<=90
replace schoolyr = 16 if educd>=100 & educd<=101
replace schoolyr = 18 if educd>=110 & educd<=115
replace schoolyr = 20 if educd==116

label variable schoolyr "School Year"
tab schoolyr
assert inrange(schoolyr,2.5,20)==1 & r(r)==12

*************
*** Q1 D. ***
*************
gen float exp = 0
label variable exp "Portential Labor Market Expierence"
replace exp = age - schoolyr - 6
assert exp < age if !missing(exp)

*************
*** Q1 E. ***
*************
gen float private = 0
label variable private "Employed By Private Firm"
**Private if classified for private** 
replace private = 1 if (classwkrd == 22)

gen float govt = 0
label variable govt "Employed By Government"
**Private if classified for government codes, inlist returns 1 or 0** 
replace govt = inlist(classwkrd, 25, 27, 28)

gen float othemp = 0
label variable othemp "Employed By Other Employer"
**otheremp if classified for any code not previously used** 
replace othemp = !inlist(classwkrd, 22, 25, 27, 28)

***check no-one got double marked**
assert private+govt+othemp==1

*************
*** Q1 F. ***
*************
