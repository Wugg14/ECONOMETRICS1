***************************************
*** econ5813pa1.do ***
*** ------------------------------- ***
*** Created by Mark Steininger ***
***************************************
********************************************************
*** This program creates variables and estimates ***
*** regressions for ECON 5813 Project Assignment #1 ***
********************************************************
**************************
*** Begin the log file ***
**************************
capture log close
log using econ5813_IPUMS_EXTRACT.log, replace
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
*** Qestion 3 ***
*****************

*************
*** Q3 A. ***
*************
describe
compress
describe

**************
*** Q3 B. ***
**************
list age sex statefip in 1/20

*************
*** Q3 C. ***
*************
sort statefip age

*************
*** Q3 D. ***
*************
list age sex statefip in 1/20

*************
*** Q3 E. ***
*************
preserve
keep if sex == 2
list age sex statefip incwage in 1/10
restore

preserve
keep if sex == 1
list age sex statefip incwage in 1/10
restore


*****************
*** Qestion 4 ***
*****************

*************
*** Q4 A. ***
*************
clear all
use ./econ5813_acs_d1.dta

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

********************
*** Q4 A.        ***
*** Drop empstat ***
********************
drop empstat

**********************************
*** Q4 B.                       ***
*** Create WeeksWorked Variable ***
**********************************
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


*************
*** Q4 C. ***
*************
drop if weeksworked <= 26

*************
*** Q4 D. ***
*************
drop if uhrswork < 20
drop if uhrswork > 60

*************
*** Q4 E. ***
*************
describe

*************
*** Q4 F. ***
*************
generate female = 0
replace female = 1 if sex == 2
generate male = 0
replace male = 1 if sex == 1

*************
*** Q4 H. ***
*************
tab female male, missing

*****************
*** Qestion 5 ***
*****************

*************
*** Q5 A. ***
*************
tab occ

**************************************************
*** Q5 B. Create wage variable                 ***
*** Formula: income / (hours * weeks)          ***
**************************************************
generate wage = 0
replace wage = incwage / (uhrswork * weeksworked)

*************
*** Q5 C. ***
*************
mean wage

*************
*** Q5 D. ***
*************
mean wage if inrange(age, 20, 30)

*****************
*** Qestion 6 ***
*****************

*************
*** Q6 A. ***
*************
mean wage if female == 1
mean wage if male == 1

*********************************
*** Q6 B.                     ***
*** Perform two sample t-test ***
*********************************
ttest wage, by(sex) level(90)

*************
*** Q6 C. ***
*************
ttest wage, by(sex) level(90)

*****************
*** Qestion 7 ***
*****************

*************
*** Q7 A. ***
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
*** Q7 B. ***
*************
regress wage schoolyr

*************
*** Q7 C. ***
*************
regress wage schoolyr age

*************
*** Q7 D. ***
*************
gen schoolyr2 = schoolyr^2
label variable schoolyr "School Year Squared"
gen age2 = age^2
label variable schoolyr "Age Squared"
regress wage schoolyr schoolyr2 age age2

*************
*** Q7 E. ***
*************
regress wage schoolyr schoolyr2 age age2 if sex == 1
regress wage schoolyr schoolyr2 age age2 if sex == 2

*************
*** Q7 F. ***
*************
gen ed_base = schoolyr - 12
label variable ed_base "School Baseline"
gen ln_ageminus20 = ln(age - 20) if age > 20
label variable ln_ageminus20 "Natural Log of Age Minus 20"
gen ed_age_interact = ed_base * ln_ageminus20
label variable ed_age_interact "Education and Log Age Interaction Term"
regress wage ed_base ln_ageminus20 ed_age_interact

mean schoolyr
mean age

*****************
*** Qestion 8 ***
*****************

regress wage schoolyr age, robust
generate PersonID = _n
expand 2
**run it again**
regress wage schoolyr age, robust
**new argument**
regress wage schoolyr age, vce(cluster PersonID)

*****************
*** Qestion 9 ***
*****************

**(1)**
regress schoolyr age female

**(2)**
regress wage age female

**school year residual first**
regress schoolyr age female
predict schoolyr_resid, residual
label variable schoolyr_resid "School Year Residual"
** wage residual next**
regress wage age female
predict wage_resid, residual
label variable wage_resid "Wage Residual"
**verify, these should be like crazy small numbers**
sum schoolyr_resid wage_resid

**(3)**
regress wage_resid schoolyr_resid

**(4)**
regress wage schoolyr age female