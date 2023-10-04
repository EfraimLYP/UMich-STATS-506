use "/Users/yipengliu/Desktop/Graduate Courses/STATS 506/Homework/Problem Set 3/VIX_D.dta", clear
merge 1:1 SEQN using "/Users/yipengliu/Desktop/Graduate Courses/STATS 506/Homework/Problem Set 3/DEMO_D.dta"
keep if _merge == 3
count

gen age_bracket = int(RIDAGEYR/10)
gen age_bracket_str = ""
replace age_bracket_str = "10-19" if age_bracket == 1
replace age_bracket_str = "20-29" if age_bracket == 2
replace age_bracket_str = "30-39" if age_bracket == 3
replace age_bracket_str = "40-49" if age_bracket == 4
replace age_bracket_str = "50-59" if age_bracket == 5
replace age_bracket_str = "60-69" if age_bracket == 6
replace age_bracket_str = "70-79" if age_bracket == 7
replace age_bracket_str = "80-89" if age_bracket == 8
tabulate age_bracket_str VIQ220

keep if VIQ220 == 1 | VIQ220 == 2
gen logicalVIQ220 = (VIQ220 == 1)

logit logicalVIQ220 RIDAGEYR
estat ic

gen RIDRETH1_1 = (RIDRETH1 == 1)
gen RIDRETH1_2 = (RIDRETH1 == 2)
gen RIDRETH1_3 = (RIDRETH1 == 3)
gen RIDRETH1_4 = (RIDRETH1 == 4)
gen logicalRIAGENDR = (RIAGENDR == 1)
logit logicalVIQ220 RIDAGEYR RIDRETH1_1 RIDRETH1_2 RIDRETH1_3 RIDRETH1_4 logicalRIAGENDR
estat ic

drop if missing(INDFMPIR)
logit logicalVIQ220 RIDAGEYR RIDRETH1_1 RIDRETH1_2 RIDRETH1_3 RIDRETH1_4 logicalRIAGENDR INDFMPIR
estat ic

matrix outputTable = J(3, 10, .)
matrix rownames outputTable = "model1" "model2" "model3"
matrix colnames outputTable = "RIDAGEYR" "RIDRETH1_1" "RIDRETH1_2" "RIDRETH1_3" "RIDRETH1_4" "logicalRIAGENDR" "INDFMPIR" "sample size" "pseudo-R square" "AIC value"
matrix outputTable[1, 1] = exp(0.0246729)
matrix outputTable[1, 8] = 6545
matrix outputTable[1, 9] = 0.0497
matrix outputTable[1, 10] = 8475.887
matrix outputTable[2, 1] = exp(0.0225742)
matrix outputTable[2, 2] = exp(-0.6509919)
matrix outputTable[2, 3] = exp(-0.4946699)
matrix outputTable[2, 4] = exp(0.0179388)
matrix outputTable[2, 5] = exp(-0.38912)
matrix outputTable[2, 6] = exp(-0.5020895)
matrix outputTable[2, 8] = 6545
matrix outputTable[2, 9] = 0.0720
matrix outputTable[2, 10] =  8287.761
matrix outputTable[3, 1] = exp(0.0221883)
matrix outputTable[3, 2] = exp(-.5327271)
matrix outputTable[3, 3] = exp(-.4167045)
matrix outputTable[3, 4] = exp(-.0311981)
matrix outputTable[3, 5] = exp(-.3253425)
matrix outputTable[3, 6] = exp(-.5162712)
matrix outputTable[3, 7] = exp(.1135978)
matrix outputTable[3, 8] = 6247
matrix outputTable[3, 9] = 0.0734
matrix outputTable[3, 10] =  87909.808
matrix list outputTable

tabulate logicalRIAGENDR logicalVIQ220, chi2
