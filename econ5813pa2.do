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
gen byte private = 0
label variable private "Employed By Private Firm"
**Private if classified for private** 
replace private = 1 if (classwkrd == 22)

gen byte govt = 0
label variable govt "Employed By Government"
**Private if classified for government codes, inlist returns 1 or 0** 
replace govt = inlist(classwkrd, 25, 27, 28)

gen byte othemp = 0
label variable othemp "Employed By Other Employer"
**otheremp if classified for any code not previously used** 
replace othemp = !inlist(classwkrd, 22, 25, 27, 28)

***check no-one got double marked**
assert private+govt+othemp==1

*************
*** Q1 F. ***
*************
gen byte married = 0
label variable married "Is This Individual Married"
replace married = inlist(marst, 1, 2)

*************
*** Q1 G. ***
*************
gen byte foreign = 0
label variable foreign "Foreign Born"
** Include All Codes for States and Territories **
replace foreign = 1 if bpl > 115

*************
*** Q1 G. ***
*************
gen byte hisp = (hispand>=100 & hispand<=499)
label variable hisp "Hispanic"
**missing codes in the white range I've corrected here**
gen byte white = inrange(raced, 100, 199) & hisp==0
**missing codes in the black range for educd I've corrected**
gen byte black = inrange(raced, 200, 299) & hisp==0
gen byte indian = inrange(raced,300,399)==1 & hisp==0
gen byte asian = inrange(raced,400,699)==1 & hisp==0
gen byte other_race = inrange(raced,700,996) & hisp==0
label variable white "White"
label variable black "Black"
label variable indian "Indian"
label variable asian "Asian"
label variable other_race "Other Race"
**this assertion kept failing, I've corrected the ranges from the provided code**
assert white+hisp+black+asian+indian+other_race==1
gen byte race_cat6 = 0
replace race_cat6 = 1 if white==1
replace race_cat6 = 2 if hisp==1
replace race_cat6 = 3 if black==1
replace race_cat6 = 4 if asian==1
replace race_cat6 = 5 if indian==1
replace race_cat6 = 6 if other_race==1
label variable race_cat6 "Race Category (race is non-Hispanic)"
#delimit ;
label define race_cat6_lbl
0 "ERROR!"
1 "White"
2 "Hispanic"
3 "Black"
4 "Asian"
5 "Indian"
6 "Other race";
#delimit cr
label values race_cat6 race_cat6_lbl
tab race_cat6, missing
assert inlist(race_cat6,1,2,3,4,5,6)==1 & r(r)==6

*************
*** Q1 J. ***
*************
**all vals below HS diplomas**
gen byte elem = ((educd <= 61) | missing(educd))
** all vals with dimplomas or ged and some college**
gen byte hs = inrange(educd,63,71 )
** bachelors and associates**
gen byte college = (educd == 81 | educd == 101)
** masters and include other professional degrees**
gen byte ma = (educd == 114 | educd == 115)
gen byte phd = (educd == 116)

**assert check dummy vars**
assert elem+hs+college+ma+phd==1
gen byte educ_cat5 = 0
replace educ_cat5 = 1 if elem==1
replace educ_cat5 = 2 if hs==1
replace educ_cat5 = 3 if college==1
replace educ_cat5 = 4 if ma==1
replace educ_cat5 = 5 if phd==1
label variable educ_cat5 "Education Category"
#delimit ;
label define educ_cat5_lbl
0 "ERROR!"
1 "Elemtary School"
2 "High School"
3 "College"
4 "Masters Degree/Professional Degree"
5 "PhD";
#delimit cr
label values educ_cat5 educ_cat5_lbl
tab educ_cat5, missing
assert inlist(educ_cat5,1,2,3,4,5)==1 & r(r)==5

**************************
*** Q3 Means and SEs *****
*** for Vars by Gender ***
**************************
*** weeksworked ***
mean weeksworked if female == 0
mean weeksworked if female == 1
mean weeksworked

*** wage ***
mean wage if female == 0
mean wage if female == 1
mean wage

*** female ***
mean female if female == 0
mean female if female == 1
mean female

*** schoolyr ***
mean schoolyr if female == 0
mean schoolyr if female == 1
mean schoolyr


*** exp ***
mean exp if female == 0
mean exp if female == 1
mean exp

*** private ***
mean private if female == 0
mean private if female == 1
mean private

*** govt ***
mean govt if female == 0
mean govt if female == 1
mean govt

*** othemp ***
mean othemp if female == 0
mean othemp if female == 1
mean othemp

*** married ***
mean married if female == 0
mean married if female == 1
mean married

*** foreign ***
mean foreign if female == 0
mean foreign if female == 1
mean foreign

*** hisp ***
mean hisp if female == 0
mean hisp if female == 1
mean hisp

*** white ***
mean white if female == 0
mean white if female == 1
mean white


*** black ***
mean black if female == 0
mean black if female == 1
mean black

*** indian ***
mean indian if female == 0
mean indian if female == 1
mean indian

*** asian ***
mean asian if female == 0
mean asian if female == 1
mean asian

*** other_race ***
mean other_race if female == 0
mean other_race if female == 1
mean other_race

*** race_cat6 ***
mean race_cat6 if female == 0
mean race_cat6 if female == 1
mean race_cat6

*** elem ***
mean elem if female == 0
mean elem if female == 1
mean elem

*** hs ***
mean hs if female == 0
mean hs if female == 1
mean hs

*** college ***
mean college if female == 0
mean college if female == 1
mean college

*** ma ***
mean ma if female == 0
mean ma if female == 1
mean ma

*** phd ***
mean phd if female == 0
mean phd if female == 1
mean phd

*** educ_cat5 ***
mean educ_cat5 if female == 0
mean educ_cat5 if female == 1
mean educ_cat5

*****************
*** Qestion 4***
*****************
regress wage exp