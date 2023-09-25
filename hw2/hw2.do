import delimited using "/Users/yipengliu/Desktop/Graduate Courses/STATS 506/Homework/Problem Set 2/cars.csv", clear

rename dimensionsheight dim_h
rename dimensionslength dim_l
rename dimensionswidth dim_w
rename engineinformationdriveline eng_info_dr
rename engineinformationenginetype eng_info_type
rename engineinformationhybrid eng_info_hbd
rename engineinformationnumberofforward eng_info_numgears
rename engineinformationtransmission eng_info_trans
rename fuelinformationcitympg fuel_info_city
rename fuelinformationfueltype fuel_info_type
rename fuelinformationhighwaympg fuel_info_hwy_mpg
rename identificationclassification iden_clf
rename identificationid iden_id
rename identificationmake iden_make
rename identificationmodelyear iden_mdyear
rename identificationyear iden_year
rename engineinformationenginestatistic eng_info_stats_hp
rename v18 eng_info_stats_tq

keep if fuel_info_type == "Gasoline"

tabulate iden_year, generate(dummy)

regress fuel_info_hwy_mpg eng_info_stats_hp eng_info_stats_tq dim_l dim_h dim_w dummy1 dummy2 dummy3

regress fuel_info_hwy_mpg c.eng_info_stats_hp##c.eng_info_stats_tq dim_l dim_h dim_w dummy1 dummy2 dummy3

sum eng_info_stats_tq
margins, at(eng_info_stats_tq = (98(10)774) eng_info_stats_hp = (263, 267.5, 317) dummy1 = 1)
marginsplot
