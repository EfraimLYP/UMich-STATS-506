import delimited "/Users/yipengliu/Desktop/Graduate Courses/STATS 506/Homework/Problem Set 4/public2022_useful.csv", clear
codebook

recode b3 (1=0) (2=0) (3=1) (4=1) (5=1), generate(b3_binary)

svyset caseid [pw = weight_pop]

svy: logit b3_binary i.nd2 i.b7_b i.gh1 i.ppeducat i.race_5cat

cd "/Users/yipengliu/Desktop/Graduate Courses/STATS 506/Homework/Problem Set 4"
export delimited using "public2022_useful_edited.csv"
