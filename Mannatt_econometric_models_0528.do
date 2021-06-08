
****************************************************************************************************************************************************************
/*Manna Toth
Thesis
Central European University
2021 May */

*The analysis was conducted on STATA 14.2 
****************************************************************************************************************************************************************
*2021/May/26 (After the consultation with Cristina)

import delimited "/Users/manna/Desktop/Thesis_202105/DATA/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
duplicates report sc_code year
set more off
xtset sc_code year
*net describe collin, from(https://stats.idre.ucla.edu/stat/stata/ado/analysis)

gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)


asdoc sum
*net describe collin, from(https://stats.idre.ucla.edu/stat/stata/ado/analysis)
*net install collin

******** OLS with year and sub-county fixed effects

* Mortalities - merged table
set more off
xtreg ln_funds_decision ln_revenues i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) replace tex text ctitle("1")
collin ln_funds_decision ln_revenues

xtreg ln_funds_decision ln_revenues population_total i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio   i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatien

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party  i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality

///////



* Treatable mortality
set more off
xtreg ln_funds_decision ln_treatable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) replace tex text ctitle("1")

xtreg ln_funds_decision ln_treatable ln_revenues i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("2")

xtreg ln_funds_decision ln_treatable ln_revenues population_total i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("3")

xtreg ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("4")

xtreg ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("5")

xtreg ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("6")

xtreg ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("7")

xtreg ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("8")

xtreg ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
collin ln_funds_decision ln_treatable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio close_run_governing_party
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("9")


*preventable mortality
set more off
xtreg ln_funds_decision ln_preventable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_funds_decision ln_preventable  ln_revenues i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_funds_decision ln_preventable  ln_revenues population_total i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("3")

xtreg ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("4")

xtreg ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("5")

xtreg ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("6")


xtreg ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("7")

xtreg ln_funds_decision ln_preventable ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("8")

xtreg ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
collin ln_funds_decision ln_preventable  ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio close_run_governing_party
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("8")



*mortality ratio
set more off
xtreg ln_funds_decision mortality_ratio i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) replace tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues population_total i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues population_total 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  close_run_governing_party i.year, fe cluster(sc_code)
collin ln_funds_decision mortality_ratio ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  close_run_governing_party
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")


* log mortality ratio
set more off
xtreg ln_funds_decision ln_mortality i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
collin ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department  one_day_ratio close_run_governing_party
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("1")


**unitroot test
xtunitroot llc ln_mortality
xtunitroot llc mortality_ratio
xtunitroot llc preventable_total
xtunitroot llc treatable_total
xtunitroot llc ln_revenues
xtunitroot llc population_total
xtunitroot llc ln_budget_outpatient
xtunitroot llc gp_empty_population_ratio
xtunitroot llc gymnasium_ratio
xtunitroot llc one_day_ratio


**** y eu funds with lags and first difference where needed (one_day_ratio, gp_empty_percapita)
set more off
xtreg ln_funds_decision l(0/2).treatable_total ln_revenues population_total ln_budget_outpatient d.gp_empty_population_ratio gymnasium_ratio icu_department d.one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) replace tex text ctitle("1")

xtreg ln_funds_decision l(0/2).preventable_total ln_revenues population_total ln_budget_outpatient d.gp_empty_population_ratio gymnasium_ratio icu_department d.one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_funds_decision l(0/2).ln_mortality ln_revenues population_total ln_budget_outpatient d.gp_empty_population_ratio gymnasium_ratio icu_department d.one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("3")

**different election variables
xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio close_run_governing_party i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio governing_party i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio overwhelming_victory_governing_p i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("3")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio close_run_opposition i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("4")

xtreg ln_funds_decision ln_mortality ln_revenues population_total ln_budget_outpatient gp_empty_population_ratio gymnasium_ratio icu_department one_day_ratio overwhelming_victory_non_governi i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc", se bdec(2) append tex text ctitle("5")



* Heterogeinity test

import delimited "/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
set more off
xtset sc_code year
gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)

keep if region_code ==60

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) replace tex text ctitle("60")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("60")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("60")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality


import delimited "/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
set more off
xtset sc_code year

gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)

keep if region_code ==30

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("30")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable


xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("30")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("30")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality


import delimited "/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
set more off
xtset sc_code year
gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)

keep if region_code ==12 | region_code == 11

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("11")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_treatable


xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("11")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_preventable

xtreg ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality i.year, fe cluster(sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Factors/factors_OLS666.doc",  se bdec(2) append tex text ctitle("11")
collin ln_funds_decision ln_revenues population_total gymnasium_ratio gp_empty_population_ratio one_day_ratio icu_department ln_budget_outpatient close_run_governing_party ln_mortality

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

*II. EFFECT OF FUNDS


*ssc install overid
import delimited "/Users/manna/Desktop/Thesis_202105/DATA/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
duplicates report sc_code year
set more off
xtset sc_code year
*net describe collin, from(https://stats.idre.ucla.edu/stat/stata/ado/analysis)

gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)
gen ln_infant_mortality = log(infant_mortality)
gen ln_distance = log(distance_county_capitals)
gen mean_incidences = (cardio_ratio + cancer_ratio + respiratory_ratio)/3

********************
* 1. Simple OLS
*Preventable mortality
set more off

xtreg ln_preventable ln_funds_effective ///
i.year ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_preventable ln_funds_effective ln_revenues  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_preventable ln_funds_effective ln_revenues population_total  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtreg ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")


xtreg ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio icu_department  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtreg ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  hours_outpatient_percapita  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtreg ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  ln_budget_total ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")



set more off
collin ln_preventable ln_funds_effective
collin ln_preventable ln_funds_effective ln_revenues
collin ln_preventable ln_funds_effective ln_revenues population_total
collin ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio
collin ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  icu_department 
collin ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  hours_outpatient_percapita 
collin ln_preventable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  ln_budget_total


*Treatable mortality
set more off

xtreg ln_treatable ln_funds_effective i.year ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_treatable ln_revenues ///
ln_funds_effective i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtreg  ln_treatable ln_revenues gp_empty_population_ratio  ///
ln_funds_effective i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtreg ln_treatable ln_revenues gp_empty_population_ratio population_total  ///
ln_funds_effective i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")

xtreg ln_treatable ln_revenues gp_empty_population_ratio population_total  icu_department ///
ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtreg ln_treatable ln_revenues gp_empty_population_ratio population_total  hours_outpatient_percapita ///
ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtreg ln_treatable ln_revenues gp_empty_population_ratio population_total  ln_budget_total ///
ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")


set more off
collin ln_treatable ln_funds_effective
collin ln_treatable ln_funds_effective ln_revenues
collin ln_treatable ln_funds_effective ln_revenues population_total
collin ln_treatable ln_funds_effective ln_revenues population_total gp_empty_population_ratio
collin ln_treatable ln_funds_effective ln_revenues population_total gp_empty_population_ratio icu_department 
collin ln_treatable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  hours_outpatient_percapita 
collin ln_treatable ln_funds_effective ln_revenues population_total gp_empty_population_ratio  ln_budget_total

xtunitroot llc ln_treatable
xtunitroot llc ln_preventable
xtunitroot llc ln_funds_effective
xtunitroot llc ln_revenues
xtunitroot llc population_total
xtunitroot llc ln_revenues
xtunitroot llc gp_empty_population_ratio
xtunitroot llc ln_budget_total
xtunitroot llc icu_department




*************************************************************************************************************************************************************************
*First difference of gp_empty_ratio and lags
*Preventable
set more off

xtreg ln_preventable l(0/3).ln_funds_effective ///
i.year ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_preventable l(0/3).ln_funds_effective ln_revenues ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_preventable l(0/3).ln_funds_effective ln_revenues population_total  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtreg ln_preventable l(0/3).ln_funds_effective ln_revenues population_total d.gp_empty_population_ratio  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")


xtreg ln_preventable l(0/3).ln_funds_effective ln_revenues population_total d.gp_empty_population_ratio icu_department  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtreg ln_preventable l(0/3).ln_funds_effective ln_revenues population_total d.gp_empty_population_ratio  hours_outpatient_percapita  ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtreg ln_preventable l(0/3).ln_funds_effective ln_revenues population_total d.gp_empty_population_ratio  ln_budget_total ///
i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")





*Treatable mortality
set more off

xtreg ln_treatable l(0/3).ln_funds_effective i.year ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtreg ln_treatable ln_revenues ///
l(0/3).ln_funds_effective i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtreg  ln_treatable ln_revenues d.gp_empty_population_ratio  ///
l(0/3).ln_funds_effective i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  ///
l(0/3).ln_funds_effective i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")

xtreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  icu_department ///
l(0/3).ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
l(0/3).ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  ln_budget_total ///
l(0/3).ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")



*****************************************************************************************************************************************************************
*** 2. 2SLS IV instruments: income (revenues and personal income percapita) and governing indicators (governing party close run, and governing_party)

*Choosing the best instrument
*the best instruments based on F statistics_ ln_revenues and governing_party

keep if year <2020


xtreg ln_funds_effective governing_party, fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) replace tex text ctitle("2") 

xtreg ln_funds_effective close_run_governing_party, fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) append tex text ctitle("2") 




*******************************
* 2. 2SLS  (ln_funds_effective =  governing_party)

*Preventable mortality
set more off

xtivreg ln_preventable (ln_funds_effective = governing_party) ,fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtivreg ln_preventable ln_revenues  ///
(ln_funds_effective =  governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtivreg ln_preventable ln_revenues population_total  ///
(ln_funds_effective =  governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtivreg ln_preventable ln_revenues gp_empty_population_ratio population_total  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")

xtivreg ln_preventable ln_revenues gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtivreg ln_preventable ln_revenues gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtivreg ln_preventable ln_revenues gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")








*Treatable mortality
set more off
xtivreg ln_treatable (ln_funds_effective = governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

set more off
xtivreg ln_treatable ln_revenues  ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

set more off
xtivreg ln_treatable ln_revenues population_total  ///
(ln_funds_effective = governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

set more off
xtivreg ln_treatable ln_revenues gp_empty_population_ratio population_total  ///
(ln_funds_effective = governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")

set more off
xtivreg ln_treatable ln_revenues gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

set more off
xtivreg ln_treatable ln_revenues gp_empty_population_ratio population_total hours_outpatient_percapita ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

set more off
xtivreg ln_treatable ln_revenues gp_empty_population_ratio population_total  ln_budget_total ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")


*** ICU coefficient test

set more off
xtreg ln_treatable  icu_department ///
 i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/ICU.doc", se bdec(2) replace tex text ctitle("2")

xtreg ln_preventable icu_department ///
 i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/ICU.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_treatable  icu_department ///
ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/ICU.doc", se bdec(2) append tex text ctitle("2")

xtreg ln_preventable icu_department ///
ln_funds_effective  i.year,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/ICU.doc", se bdec(2) append tex text ctitle("2")

xtivreg ln_preventable icu_department ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/ICU.doc", se bdec(2) append tex text ctitle("1")

xtivreg ln_treatable  icu_department ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/ICU.doc", se bdec(2) append tex text ctitle("2")





*****************************************************************************************************


*First differences
* 2SLS (ln_funds_effective = governing_party)

* Preventable
set more off
xtivreg ln_preventable (ln_funds_effective = governing_party) ,fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtivreg ln_preventable ln_revenues  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtivreg ln_preventable ln_revenues population_total  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")



*treatable mortalities
set more off
xtivreg ln_treatable (ln_funds_effective = governing_party) ,fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")

xtivreg ln_treatable ln_revenues  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")

xtivreg ln_treatable ln_revenues population_total  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  ///
(ln_funds_effective = governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")


******************************

import delimited "/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
set more off
xtset sc_code year
gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)

keep if region_code ==60

*First differences
* 2SLS (ln_funds_effective = governing_party)

* Preventable
set more off
xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) replace tex text ctitle("60")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("60")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("60")



*treatable mortalities
set more off

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) replace tex text ctitle("60")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("60")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("60")


import delimited "/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
set more off
xtset sc_code year
gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)

keep if region_code ==30




*First differences
* 2SLS (ln_funds_effective = governing_party)

* Preventable
set more off
xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("30")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("30")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("30")



*treatable mortalities
set more off
set more off

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("30")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("30")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("30")


import delimited "/Users/manna/Desktop/Thesis_202105/DATA/ready_to_use/data_for_analysis2.csv", clear

sort sc_code year 
replace distance_county_capitals = distance_county_capitals[_n+1] if missing(distance_county_capitals)
set more off
xtset sc_code year
gen ln_mortality = log(mortality_ratio)
gen ln_treatable = log(treatable_total)
gen ln_preventable = log(preventable_total)

keep if region_code ==12 | region_code == 11


*First differences
* 2SLS (ln_funds_effective = governing_party)

* Preventable
set more off
xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("11")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("11")

xtivreg ln_preventable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("11")




*treatable mortalities
set more off

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  icu_department ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("11")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective =  governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("11")

xtivreg ln_treatable ln_revenues d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/treatable_IV1_FE_h.doc", se bdec(2) append tex text ctitle("11")
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






































































********************************
*2SLS using first differenece of gp_empty
*Preventable mortality
set more off

xtivreg ln_preventable (ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")
xtoverid

xtivreg ln_preventable gymnasium_ratio  ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")
xtoverid

xtivreg ln_preventable gymnasium_ratio population_total  ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total  ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total  icu_department ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences icu_department ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences hours_outpatient_percapita ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ln_budget_total ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid



*Treatable mortality
set more off

xtivreg ln_treatable (ln_funds_effective =  governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")
xtoverid

xtivreg ln_treatable gymnasium_ratio  ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")
xtoverid

xtivreg ln_treatable gymnasium_ratio population_total  ///
(ln_funds_effective =  governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total  ///
(ln_funds_effective = governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ///
(ln_funds_effective = governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective =  governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total hours_outpatient_percapita ///
(ln_funds_effective = governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total  ln_budget_total ///
(ln_funds_effective = governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences icu_department ///
(ln_funds_effective = governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences hours_outpatient_percapita ///
(ln_funds_effective = governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ln_budget_total ///
(ln_funds_effective = governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid




////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

***IV insturments: incidence ratios by revenue and mean incidences

*calculating mean of incidence ratios (cancer, cardio and respiratory ratios)

*Choosing the best instrument
*the best instruments based on F statistics_ ln_revenues and governing_party


xtreg mean_incidences ln_revenues, fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) replace tex text ctitle("1") 

xtreg mean_incidences ln_budget_total, fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) append tex text ctitle("1") 

xtreg mean_incidences ln_personal_income,  fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) append tex text ctitle("1") 

xtreg mean_incidences ln_personal_income  ln_budget_total, fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) append tex text ctitle("1") 

xtreg mean_incidences  ln_revenues ln_budget_total, fe vce(cluste sc_code) 
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", addstat("F test model", e(F)) se bdec(2) append tex text ctitle("1") 



* 2. 2SLS  (ln_funds_effective = ln_revenues governing_party)

reg3 (ln_preventable ln_funds_effective gymnasium_ratio gp_empty_population_ratio population_total mean_incidences i.year i.sc_code) ///
 (ln_funds_effective = ln_revenues governing_party) (mean_incidences  = ln_personal_income) , 2sls
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")
overid






*******************************************
*2SLS using first differenece of gp_empty
*Preventable mortality
set more off

xtivreg ln_preventable (ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")
xtoverid

xtivreg ln_preventable gymnasium_ratio  ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")
xtoverid

xtivreg ln_preventable gymnasium_ratio population_total  ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total  ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ///
(ln_funds_effective = ln_revenues governing_party),fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total  icu_department ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total  hours_outpatient_percapita ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total ln_budget_total ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences icu_department ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences hours_outpatient_percapita ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_preventable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ln_budget_total ///
(ln_funds_effective = ln_revenues governing_party), fe  vce(cluster sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid



*Treatable mortality
set more off

xtivreg ln_treatable (ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) replace tex text ctitle("1")
xtoverid

xtivreg ln_treatable gymnasium_ratio  ///
(ln_funds_effective = ln_revenues governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("2")
xtoverid

xtivreg ln_treatable gymnasium_ratio population_total  ///
(ln_funds_effective = ln_revenues governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("3")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total  ///
(ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ///
(ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("4")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total icu_department ///
(ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total hours_outpatient_percapita ///
(ln_funds_effective = ln_revenues governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total  ln_budget_total ///
(ln_funds_effective = ln_revenues governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences icu_department ///
(ln_funds_effective = ln_revenues governing_party) ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("5")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences hours_outpatient_percapita ///
(ln_funds_effective = ln_revenues governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("6")
xtoverid

xtivreg ln_treatable gymnasium_ratio d.gp_empty_population_ratio population_total mean_incidences ln_budget_total ///
(ln_funds_effective = ln_revenues governing_party)  ,fe  vce(cluste sc_code)
outreg2 using "/Users/manna/Desktop/Thesis_202105/RESULTS/Effect/preventable_IV1_FE.doc", se bdec(2) append tex text ctitle("7")
xtoverid














