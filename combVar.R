#Michelle Stuhlmacher
#2023/07/07

#GOAL: Combine all variables into single dataframe

#STEPS:
#1. Import data and libraries
#2. Rename and format DFs so they can be combined
#3. Combine DFs and remove extra columns
#4. Calcuate greening delta variables
#5. Add redlining and park variables
#6. Export

# STEP 1 -----------------------------------------------
# Import data and libraries

#Libraries
library(sf)
library(dplyr)

#Set working directory
#setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop
setwd("C:/Users/mfstu/OneDrive - DePaul University/Research/Redlining") #big girl build

#Census data
chiC = st_read('./Data/Census/Cleaned/megatractCHI_20241217.shp')

#Redlining proportions
chiR = read.csv('./Data/Redlining/PropByTract/megatractRedPropCHI_20241217.csv')

#NDVI
chiN2010 = read.csv('./Data/EOSummaryTables/NDVI/ChicagoNDVI2010_20241217.csv')
chiN2015 = read.csv('./Data/EOSummaryTables/NDVI/ChicagoNDVI2015_20241217.csv')
chiN2020 = read.csv('./Data/EOSummaryTables/NDVI/ChicagoNDVI2020_20241217.csv')

#LST
chiL2010 = read.csv('./Data/EOSummaryTables/LST/ChicagoLST2010_20241217.csv')
chiL2015 = read.csv('./Data/EOSummaryTables/LST/ChicagoLST2015_20241217.csv')
chiL2020 = read.csv('./Data/EOSummaryTables/LST/ChicagoLST2020_20241217.csv')

#Tree Canopy Coverage
chiT = st_read('./Data/EOSummaryTables/TCC/ChicagoTCCallYears_20250326.shp')

#Park
chiP = read.csv("./Data/CPD_Boundaries/ParkBuffAreaByTract_20250114.csv")

# STEP 2 -----------------------------------------------
# Rename and format DFs so they can be combined

#Calculate proportion variables for census data
chiC$pctW = chiC$raceW/chiC$totPop
chiC$pctB = chiC$raceB/chiC$totPop
chiC$pctN = chiC$raceN/chiC$totPop
chiC$pctA = chiC$raceA/chiC$totPop
chiC$pctP = chiC$raceP/chiC$totPop
chiC$pctO = chiC$raceO/chiC$totPop
chiC$pct2 = chiC$race2/chiC$totPop
chiC$pctNW = chiC$raceNW/chiC$totPop
chiC$pctFBW = chiC$raceFBW/chiC$totPop
chiC$pctH = chiC$orgnHisp/chiC$totPop
chiC$pctBach = chiC$gteBach/chiC$eduSum
chiC$pctHS = chiC$eduHS/chiC$eduSum
chiC$pctVacHou = chiC$vacHou/chiC$hoUnitT #percent vacant housing
chiC$pctOccHou = chiC$occHou/chiC$hoUnitT #percent occupied housing (need to do this for 1940)
chiC$pctOwnHou = chiC$ownHou/chiC$occHou #percent owner occupied housing
chiC$pctGte30Hou = chiC$houGte30/chiC$hoUnitT #percent housing greater (or equal to) 30 years old. Not 30 for 2015

#Separate out the megatracts by year
chiC1940 = chiC[chiC$YEAR == '1940', ]
chiC2010 = chiC[chiC$YEAR == '2006-2010', ]
chiC2015 = chiC[chiC$YEAR == '2011-2015', ]
chiC2020 = chiC[chiC$YEAR == '2016-2020', ]

#Rename megatracts to include years for variables that will be kept
names(chiC1940) = c("totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2","raceNW","raceFBW","orgnHisp","eduSum","gteBach",
                    "eduHS","empM","empF","medInc","medRent","homeVal1940","hoUnitT","occHou","vacHou","ownHou","rentHou","houGte30","homeWRadio",
                    "homeRadioT","homeNRep","homeRepT","pplHoUnit1940","medYrHou1940","cluster_id","YEAR","geometry","pctW1940","pctB1940","pctN1940","pctA1940",
                    "pctP1940","pctO1940","pct21940","pctNW1940","pctFBW1940","pctH","pctBach","pctHS1940","pctVacHou1940","pctOccHou1940","pctOwnHou1940","pctGte30Hou1940")
names(chiC2010) = c("totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2","raceNW","raceFBW","orgnHisp","eduSum","gteBach",
                    "eduHS","empM","empF","medInc2010","medRent2010","homeVal2010","hoUnitT","occHou","vacHou","ownHou","rentHou","houGte30","homeWRadio",
                    "homeRadioT","homeNRep","homeRepT","pplHoUnit2010","medYrHou2010","cluster_id","YEAR","geometry","pctW2010","pctB2010","pctN2010","pctA2010",
                    "pctP2010","pctO2010","pct22010","pctNW2010","pctFBW2010","pctH2010","pctBach2010","pctHS2010","pctVacHou2010","pctOccHou2010","pctOwnHou2010","pctGte30Hou2010")
names(chiC2015) = c("totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2","raceNW","raceFBW","orgnHisp","eduSum","gteBach",
                    "eduHS","empM","empF","medInc2015","medRent2015","homeVal2015","hoUnitT","occHou","vacHou","ownHou","rentHou","houGte30","homeWRadio",
                    "homeRadioT","homeNRep","homeRepT","pplHoUnit2015","medYrHou2015","cluster_id","YEAR","geometry","pctW2015","pctB2015","pctN2015","pctA2015",
                    "pctP2015","pctO2015","pct22015","pctNW2015","pctFBW2015","pctH2015","pctBach2015","pctHS2015","pctVacHou2015","pctOccHou2015","pctOwnHou2015","pctGte30Hou2015")
names(chiC2020) = c("totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2","raceNW","raceFBW","orgnHisp","eduSum","gteBach",
                    "eduHS","empM","empF","medInc2020","medRent2020","homeVal2020","hoUnitT","occHou","vacHou","ownHou","rentHou","houGte30","homeWRadio",
                    "homeRadioT","homeNRep","homeRepT","pplHoUnit2020","medYrHou2020","cluster_id","YEAR","geometry","pctW2020","pctB2020","pctN2020","pctA2020",
                    "pctP2020","pctO2020","pct22020","pctNW2020","pctFBW2020","pctH2020","pctBach2020","pctHS2020","pctVacHou2020","pctOccHou2020","pctOwnHou2020","pctGte30Hou2020")

#Rename NDVI and LST
names(chiN2010) = c("cluster_id","meanNDVI2010","medNDVI2010")
names(chiN2015) = c("cluster_id","meanNDVI2015","medNDVI2015")
names(chiN2020) = c("cluster_id","meanNDVI2020","medNDVI2020")

names(chiL2010) = c("cluster_id","meanLST2010K","medLST2010K")
names(chiL2015) = c("cluster_id","meanLST2015K","medLST2015K")
names(chiL2020) = c("cluster_id","meanLST2020K","medLST2020K")

#Convert LST from Kelvins to Celsius (0K − 273.15 = -273.1C) 
chiL2010$meanLST2010 = chiL2010$meanLST2010K - 273.15
chiL2015$meanLST2015 = chiL2015$meanLST2015K - 273.15
chiL2020$meanLST2020 = chiL2020$meanLST2020K - 273.15

chiL2010$medLST2010 = chiL2010$medLST2010K - 273.15
chiL2015$medLST2015 = chiL2015$medLST2015K - 273.15
chiL2020$medLST2020 = chiL2020$medLST2020K - 273.15

#Rename tree
names(chiT) = c("cluster_id","meanTCC2011","medTCC2011","meanTCC2015","medTCC2015","meanTCC2020","medTCC2020","geometry")

# STEP 3 -----------------------------------------------
# Combine together and remove extra columns

#Combine
chi2010_1 = merge(chiC2010,chiN2010,by = "cluster_id")
chi2010_2 = merge(chi2010_1,chiL2010, by = "cluster_id")
chi2010 = merge(chi2010_2,st_drop_geometry(chiT), by = "cluster_id")

chi2015_1 = merge(chiC2015,chiN2015,by = "cluster_id")
chi2015_2 = merge(chi2015_1,chiL2015, by = "cluster_id")
chi2015 = merge(chi2015_2,st_drop_geometry(chiT), by = "cluster_id")

chi2020_1 = merge(chiC2020,chiN2020,by = "cluster_id")
chi2020_2 = merge(chi2020_1,chiL2020, by = "cluster_id")
chi2020 = merge(chi2020_2,st_drop_geometry(chiT), by = "cluster_id")

#Remove extra columns
keep40 = c("cluster_id","homeVal1940","pplHoUnit1940","pctW1940","pctB1940","pctNW1940","pctFBW1940","pctHS1940","pctOwnHou1940",
           "pctOccHou1940","geometry")
chi1940 = chiC1940[keep40]
  
keep10 = c("cluster_id","medInc2010","medRent2010","homeVal2010","medYrHou2010","pctW2010","pctB2010","pctN2010","pctA2010",
           "pctP2010","pctO2010","pct22010","pctH2010","pctBach2010","pctVacHou2010","pctOwnHou2010","pctGte30Hou2010","meanNDVI2010",
           "medNDVI2010","meanLST2010","medLST2010","meanTCC2011","medTCC2011","geometry")
chi2010 = chi2010[keep10]

keep15 = c("cluster_id","medInc2015","medRent2015","homeVal2015","medYrHou2015","pctW2015","pctB2015","pctN2015","pctA2015",
           "pctP2015","pctO2015","pct22015","pctH2015","pctBach2015","pctVacHou2015","pctOwnHou2015","pctGte30Hou2015","meanNDVI2015",
           "medNDVI2015","meanLST2015","medLST2015","meanTCC2015","medTCC2015","geometry")
chi2015 = chi2015[keep15]

keep20 = c("cluster_id","medInc2020","medRent2020","homeVal2020","medYrHou2020","pctW2020","pctB2020","pctN2020","pctA2020",
           "pctP2020","pctO2020","pct22020","pctH2020","pctBach2020","pctVacHou2020","pctOwnHou2020","pctGte30Hou2020","meanNDVI2020",
           "medNDVI2020","meanLST2020","medLST2020","meanTCC2020","medTCC2020","geometry")
chi2020 = chi2020[keep20]

# STEP 4 -----------------------------------------------
# Calculate greening delta variables

#Add the yearly dataframes together (one per city)
chiDF1 = merge(chi1940,st_drop_geometry(chi2010),by = "cluster_id")
chiDF2 = merge(chiDF1,st_drop_geometry(chi2015),by = "cluster_id")
chiDF = merge(chiDF2,st_drop_geometry(chi2020),by = "cluster_id")

#DeltaNDVI2010_2015
chiDF$deltaMeanNDVI2010_2015 = chiDF$meanNDVI2015 - chiDF$meanNDVI2010
chiDF$deltaMedNDVI2010_2015 = chiDF$medNDVI2015 - chiDF$medNDVI2010

#DeltaNDVI2015_2020
chiDF$deltaMeanNDVI2015_2020 = chiDF$meanNDVI2020 - chiDF$meanNDVI2015
chiDF$deltaMedNDVI2015_2020 = chiDF$medNDVI2020 - chiDF$medNDVI2015

#DeltaNDVI2010_2020
chiDF$deltaMeanNDVI2010_2020 = chiDF$meanNDVI2020 - chiDF$meanNDVI2010
chiDF$deltaMedNDVI2010_2020 = chiDF$medNDVI2020 - chiDF$medNDVI2010

#DeltaTree2010-2015
chiDF$deltaMeanTCC2010_2015 = chiDF$meanTCC2015 - chiDF$meanTCC2011
chiDF$deltaMedTCC2010_2015 = chiDF$medTCC2015 - chiDF$medTCC2011

#DeltaTree2015-2020
chiDF$deltaMeanTCC2015_2020 = chiDF$meanTCC2020 - chiDF$meanTCC2015
chiDF$deltaMedTCC2015_2020 = chiDF$medTCC2020 - chiDF$medTCC2015

#DeltaTree2010-2020
chiDF$deltaMeanTCC2010_2020 = chiDF$meanTCC2020 - chiDF$meanTCC2011
chiDF$deltaMedTCC2010_2020 = chiDF$medTCC2020 - chiDF$medTCC2011

#DeltaLST2010-2015
chiDF$deltaMeanLST2010_2015 = chiDF$meanLST2015 - chiDF$meanLST2010
chiDF$deltaMedLST2010_2015 = chiDF$medLST2015 - chiDF$medLST2010

#DeltaLST2015-2020
chiDF$deltaMeanLST2015_2020 = chiDF$meanLST2020 - chiDF$meanLST2015
chiDF$deltaMedLST2015_2020 = chiDF$medLST2020 - chiDF$medLST2015

#DeltaLST2010-2020
chiDF$deltaMeanLST2010_2020 = chiDF$meanLST2020 - chiDF$meanLST2010
chiDF$deltaMedLST2010_2020 = chiDF$medLST2020 - chiDF$medLST2010

#Adjust the monetary variables (income, rent, housing value) for inflation
#2006-2010 = 2010 inflation adjusted dollars
#2011-2015 = 2015 inflation adjusted dollars
#2016-2020 = 2020 inflation adjusted dollars

#U.S. Bureau of Labor Statistics Inflation Calculator:
#https://www.bls.gov/data/inflation_calculator.htm

#$1 in Dec. 1940 has the same buying power as $18.47 in Dec. 2020
#$1 in Dec. 2000 has the same buying power as $1.50 in Dec. 2020
#$1 in Dec. 2010 has the same buying power as $1.19 in Dec. 2020
#$1 in Dec. 2015 has the same buying power as $1.10 in Dec. 2020

chiDF$incomeInf2010 = chiDF$medInc2010 * 1.19
chiDF$incomeInf2015 = chiDF$medInc2015 * 1.10

chiDF$rentInf2010 = chiDF$medRent2010 * 1.19
chiDF$rentInf2015 = chiDF$medRent2015 * 1.10

chiDF$housValInf1940 = chiDF$homeVal1940 * 18.47
chiDF$housValInf2010 = chiDF$homeVal2010 * 1.19
chiDF$housValInf2015 = chiDF$homeVal2015 * 1.10

# STEP 5  -----------------------------------------------
# Add redlining and park variables

#ADD REDLINING VARIABLES
#remove all but the relevant redlining variables
keepR = c("cluster_id","proportionA","proportionB","proportionC","proportionD","proportionCorD","primary_grade_4levels","primary_grade_3levels",
          "prmr4_50","prmr3_50","prmr4_25","prmr3_25","mixGrade","finalGrade","finalGrade2")
chiR = chiR[keepR]

#Add redlining variables to final df
chiDFR = merge(chiDF,chiR,by = "cluster_id")

#ADD PARK VARIABLES
chiP = chiP[, !names(chiP) %in% "X"] #remove X column

chiDFfinal = merge(chiDFR,chiP,by = "cluster_id")

# STEP 6 -----------------------------------------------
# Export

#Remove geometry and export csv
chiDF_csv = st_drop_geometry(chiDFfinal) #remove geometry
write.csv(chiDF_csv, "./Data/EOSummaryTables/Combined/chiDF_20250326.csv")