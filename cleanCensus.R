#Michelle Stuhlmacher
#2022.06.23

#Clean Census Data (Chicago)

#Variables (1940) =
#Total Population
#Median home value
#% Non-White, Black, Foreign-born White
#% Employed
#% with high school diploma
#Number of housing units
#% of homes needing major repairs
#% of homes with radios
#Number of people per housing unit

#Variables (2000,2010,2015,2020) = 
#Total Population
#% White, Black, Native American, Asian, Native Hawaiian and Other Pacific Islander
#% Hispanic or Latino Origin
#Median household income
#Median gross rent
#Median home value
#Educational attainment (percent of population with a bachelor's degree or higher)
#Average age of housing stock
#Housing stock over 30 years old
#Housing vacancy
#Number of Renters

#STEPS:
#1. Read in data and import libraries
#2. Subset to counties of interest
#3. Subset variables of interest (by year)
#4. Merge with SHP and export

# STEP 1 -----------------------------------------------
#Import data and libraries

#Libraries
library(dplyr)
library(sf)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop

#Import files
csv1940a = read.csv('./Data/Census/nhgis0010_csv_us_tract_1940/nhgis0010_csv/nhgis0010_ds76_1940_tract.csv')
csv1940b = read.csv('./Data/Census/nhgis0010_csv_us_tract_1940/nhgis0010_csv/nhgis0016_ds76_1940_tract.csv')
csv1940c = read.csv('./Data/Census/nhgis0010_csv_us_tract_1940/nhgis0010_csv/nhgis0023_ds76_1940_tract.csv')
shp1940 = read_sf('./Data/Census/nhgis0010_shape_us_tract_1940/nhgis0010_shape/nhgis0010_shapefile_tl2008_us_tract_1940/US_tract_1940_conflated.shp') #2008 TIGER lines

csv2000a = read.csv('./Data/Census/nhgis0011_csv_2000/nhgis0011_csv/nhgis0011_ds146_2000_tract.csv')
csv2000b = read.csv('./Data/Census/nhgis0011_csv_2000/nhgis0011_csv/nhgis0011_ds151_2000_tract.csv')
csv2000c = read.csv('./Data/Census/nhgis0011_csv_2000/nhgis0011_csv/nhgis0017_ds146_2000_tract.csv') #total population
shp2000 = read_sf('./Data/Census/nhgis0015_shape_2000/nhgis0015_shapefile_tl2008_us_tract_2000/US_tract_2000_conflated.shp') #2008 TIGER lines

csv2010a = read.csv('./Data/Census/nhgis0001_csv_2010_2015_2020/nhgis0001_ds176_20105_tract.csv')
csv2010b = read.csv('./Data/Census/nhgis0001_csv_2010_2015_2020/nhgis0012_ds176_20105_tract.csv')
shp2010 = read_sf('./Data/Census/nhgis0001_shapefile_tl2010_us_tract_2010/US_tract_2010.shp')

csv2015a = read.csv('./Data/Census/nhgis0001_csv_2010_2015_2020/nhgis0001_ds215_20155_tract.csv')
csv2015b = read.csv('./Data/Census/nhgis0001_csv_2010_2015_2020/nhgis0013_ds215_20155_tract.csv')
shp2015 = read_sf('./Data/Census/nhgis0001_shapefile_tl2015_us_tract_2015/US_tract_2015.shp')

csv2020a = read.csv('./Data/Census/nhgis0001_csv_2010_2015_2020/nhgis0001_ds249_20205_tract.csv')
csv2020b = read.csv('./Data/Census/nhgis0001_csv_2010_2015_2020/nhgis0014_ds249_20205_tract.csv')
shp2020 = read_sf('./Data/Census/nhgis0001_shapefile_tl2020_us_tract_2020/US_tract_2020.shp')

# STEP 2 -----------------------------------------------
#Join yearly csv files
#1940
csv1940d = merge(csv1940a, csv1940b, by = "GISJOIN")
csv1940 = merge(csv1940d, csv1940c, by = "GISJOIN")

#2000
csv2000d = merge(csv2000a, csv2000b, by = "GISJOIN")
csv2000 = merge(csv2000d, csv2000c, by = "GISJOIN")

#2010
csv2010 = merge(csv2010a, csv2010b, by = "GISJOIN")

#2015
csv2015 = merge(csv2015a, csv2015b, by = "GISJOIN")

#2020
csv2020 = merge(csv2020a, csv2020b, by = "GISJOIN")

#Subset to the counties of interest
csv1940_s = subset(csv1940, STATE.x == "Illinois" & COUNTY.x == "Cook")         

csv2000_s = subset(csv2000, STATE.x == "Illinois" & COUNTY.x == "Cook")         

csv2010_s = subset(csv2010, STATE.x == "Illinois" & COUNTY.x == "Cook County")  

csv2015_s = subset(csv2015, STATE.x == "Illinois" & COUNTY.x == "Cook County")      

csv2020_s = subset(csv2020, STATE.x == "Illinois" & COUNTY.x == "Cook County") 

# STEP 3A -----------------------------------------------
#Subset to variables of interest 1940

##ALL VARIABLES##
#BUB001:      Total Population
#BUQ001:      White Population
#BUQ002:      Non-white Population
#BVG001:      Black Population
#BVQ001:      Population per occupied dwelling unit
#BVT001:      Native, Male, White Population
#BVT002:      Native, Female, White Population
#BVT003:      Foreign, Male, White Population
#BVT004:      Foreign, Female, White Population
#BUD001:     Number of Male Employed Persons
#BUD002:     Number of Female Employed Persons
#BUH001:      Male Persons 25 Years and Over, No school completed
#BUH002:      Male Persons 25 Years and Over, Elementary 1 - 4
#BUH003:      Male Persons 25 Years and Over, Elementary 5 - 6
#BUH004:      Male Persons 25 Years and Over, Elementary 7 - 8
#BUH005:      Male Persons 25 Years and Over, High school 1 - 3
#BUH006:      Male Persons 25 Years and Over, High school 4
#BUH007:      Male Persons 25 Years and Over, College 1 - 3
#BUH008:      Male Persons 25 Years and Over, College 4+
#BUH009:      Male Persons 25 Years and Over, Education Not reported
#BUH010:      Female Persons 25 Years and Over, No school completed
#BUH011:      Female Persons 25 Years and Over, Elementary 1 - 4
#BUH012:      Female Persons 25 Years and Over, Elementary 5 - 6
#BUH013:      Female Persons 25 Years and Over, Elementary 7 - 8
#BUH014:      Female Persons 25 Years and Over, High school 1 - 3
#BUH015:      Female Persons 25 Years and Over, High school 4
#BUH016:      Female Persons 25 Years and Over, College 1 - 3
#BUH017:      Female Persons 25 Years and Over, College 4+
#BUH018:      Female Persons 25 Years and Over, Not reported
#BU1001:      Total number of Dwelling Units
#BVC001:      Median value of homes
#BVK001:      Dwelling Units Not needing major repairs
#BVK002:      Dwelling Units Needing major repairs
#BVK003:      Dwelling Units Not reporting
#BVM001:      Occupied Dwelling Units with Radio
#BVM002:      Occupied Dwelling Units with No radio
#BVM003:      Occupied Dwelling Units, Not reporting
#BU2001:      Owner occupied
#BU2002:      Tenant occupied

#DEFINING VARIABLES
#Total Population = BUB001
#Median home value = BVC001
#White population = BUQ001
#Non-White population = BUQ002
#Black population = BVG001
#Foreign-born White
csv1940_s$raceFBW = csv1940_s$BVT003 + csv1940_s$BVT004
#Number of Male Employed Persons = BUD001
#Number of Female Employed Persons = BUD002    
#total number of ppl for education variable
csv1940_s$eduSum = csv1940_s$BUH001 + csv1940_s$BUH002 + csv1940_s$BUH003 + csv1940_s$BUH004 +
  csv1940_s$BUH005 + csv1940_s$BUH006 + csv1940_s$BUH007 + csv1940_s$BUH008 + csv1940_s$BUH009 +
  csv1940_s$BUH010 + csv1940_s$BUH011 + csv1940_s$BUH012 + csv1940_s$BUH013 + csv1940_s$BUH014 +
  csv1940_s$BUH015 + csv1940_s$BUH016 + csv1940_s$BUH017 + csv1940_s$BUH018
#Number with four years of high school
csv1940_s$eduHS = csv1940_s$BUH006 + csv1940_s$BUH007 + csv1940_s$BUH008 + csv1940_s$BUH015 + 
                      csv1940_s$BUH016 + csv1940_s$BUH017
#Number of housing units = BU1001
#Number of homes needing major repairs = BVK002
csv1940_s$homeRepT = csv1940_s$BVK001 + csv1940_s$BVK002 + csv1940_s$BVK003
#Number of homes with radios = BVM001
csv1940_s$homeRadioT = csv1940_s$BVM001 + csv1940_s$BVM002 + csv1940_s$BVM003
#Number of people per housing unit = BVQ001
#Number of occupied housing units
csv1940_s$occHou = csv1940_s$BU2001 + csv1940_s$BU2002
#Number of owner occupied housing units = BU2001
#Number of tenant occupied housing units = BU2002

#Subset to columns of interest
csv1940_sn = csv1940_s[, c("GISJOIN","STATE.x","COUNTY.x","YEAR.x","BUB001","BVC001","BUQ001","BUQ002","BVG001","raceFBW",
                           "BUD001","BUD002","eduSum","eduHS","BU1001","BVK002","homeRepT","BVM001","homeRadioT","BVQ001",
                           "occHou","BU2001","BU2002")]

#Rename
colnames(csv1940_sn) =  c("GISJOIN","STATE","COUNTY","YEAR","totPop","homeVal","raceW","raceNW","raceB","raceFBW",
                          "empM","empF","eduSum","eduHS","hoUnitT","homeNRep","homeRepT","homeWRadio","homeRadioT","pplHoUnit",
                          "occHou","ownHou","rentHou")
  
# STEP 3B -----------------------------------------------
#Subset to variables of interest 2000

#FL5001:      Total Population
#FMR001:      White alone
#FMR002:      Black or African American alone
#FMR003:      American Indian and Alaska Native alone
#FMR004:      Asian alone
#FMR005:      Native Hawaiian and Other Pacific Islander alone
#FMR006:      Some other race alone
#FMR007:      Two or more races
#FMS001:      Not Hispanic or Latino >> White alone
#FMS002:      Not Hispanic or Latino >> Black or African American alone
#FMS003:      Not Hispanic or Latino >> American Indian and Alaska Native alone
#FMS004:      Not Hispanic or Latino >> Asian alone
#FMS005:      Not Hispanic or Latino >> Native Hawaiian and Other Pacific Islander alone
#FMS006:      Not Hispanic or Latino >> Some other race alone
#FMS007:      Not Hispanic or Latino >> Two or more races
#FMS008:      Hispanic or Latino >> White alone
#FMS009:      Hispanic or Latino >> Black or African American alone
#FMS010:      Hispanic or Latino >> American Indian and Alaska Native alone
#FMS011:      Hispanic or Latino >> Asian alone
#FMS012:      Hispanic or Latino >> Native Hawaiian and Other Pacific Islander alone
#FMS013:      Hispanic or Latino >> Some other race alone
#FMS014:      Hispanic or Latino >> Two or more races
csv2000_s$orgnHisp = csv2000_s$FMS008 + csv2000_s$FMS009 + csv2000_s$FMS010 + csv2000_s$FMS011 + csv2000_s$FMS012 + csv2000_s$FMS013 + csv2000_s$FMS014
#GMY001:      Median income in 1999
#GBO001:      Median gross rent
#GB7001:      Median home value (owner occupied units)
#GKT001:      Male >> No schooling completed
#GKT002:      Male >> Nursery to 4th grade
#GKT003:      Male >> 5th and 6th grade
#GKT004:      Male >> 7th and 8th grade
#GKT005:      Male >> 9th grade
#GKT006:      Male >> 10th grade
#GKT007:      Male >> 11th grade
#GKT008:      Male >> 12th grade, no diploma
#GKT009:      Male >> High school graduate (includes equivalency)
#GKT010:      Male >> Some college, less than 1 year
#GKT011:      Male >> Some college, 1 or more years, no degree
#GKT012:      Male >> Associate degree
#GKT013:      Male >> Bachelor's degree
#GKT014:      Male >> Master's degree
#GKT015:      Male >> Professional school degree
#GKT016:      Male >> Doctorate degree
#GKT017:      Female >> No schooling completed
#GKT018:      Female >> Nursery to 4th grade
#GKT019:      Female >> 5th and 6th grade
#GKT020:      Female >> 7th and 8th grade
#GKT021:      Female >> 9th grade
#GKT022:      Female >> 10th grade
#GKT023:      Female >> 11th grade
#GKT024:      Female >> 12th grade, no diploma
#GKT025:      Female >> High school graduate (includes equivalency)
#GKT026:      Female >> Some college, less than 1 year
#GKT027:      Female >> Some college, 1 or more years, no degree
#GKT028:      Female >> Associate degree
#GKT029:      Female >> Bachelor's degree
#GKT030:      Female >> Master's degree
#GKT031:      Female >> Professional school degree
#GKT032:      Female >> Doctorate degree
csv2000_s$eduSum = csv2000_s$GKT001 + csv2000_s$GKT002 + csv2000_s$GKT003 + csv2000_s$GKT004 + csv2000_s$GKT005 +
  csv2000_s$GKT006 + csv2000_s$GKT007 + csv2000_s$GKT008 + csv2000_s$GKT009 + csv2000_s$GKT010 + csv2000_s$GKT011 + 
  csv2000_s$GKT012 + csv2000_s$GKT013 + csv2000_s$GKT014 + csv2000_s$GKT015 + csv2000_s$GKT016 + csv2000_s$GKT017 + 
  csv2000_s$GKT018 + csv2000_s$GKT019 + csv2000_s$GKT020 + csv2000_s$GKT021 + csv2000_s$GKT022 + csv2000_s$GKT023 +
  csv2000_s$GKT024 + csv2000_s$GKT025 + csv2000_s$GKT026 + csv2000_s$GKT027 + csv2000_s$GKT028
  
csv2000_s$gteBach = csv2000_s$GKT013 + csv2000_s$GKT014 + csv2000_s$GKT015 + csv2000_s$GKT016 + csv2000_s$GKT029 + 
  csv2000_s$GKT030 + csv2000_s$GKT031 + csv2000_s$GKT032
  
#GAJ001:      Built 1999 to March 2000
#GAJ002:      Built 1995 to 1998
#GAJ003:      Built 1990 to 1994
#GAJ004:      Built 1980 to 1989
#GAJ005:      Built 1970 to 1979
#GAJ006:      Built 1960 to 1969
#GAJ007:      Built 1950 to 1959
#GAJ008:      Built 1940 to 1949
#GAJ009:      Built 1939 or earlier
csv2000_s$hoUnitT = csv2000_s$GAJ001 + csv2000_s$GAJ002 + csv2000_s$GAJ003 + csv2000_s$GAJ004 + csv2000_s$GAJ005 + 
  csv2000_s$GAJ006 + csv2000_s$GAJ007 + csv2000_s$GAJ008 + csv2000_s$GAJ009
csv2000_s$houGte30 = csv2000_s$GAJ005 + csv2000_s$GAJ006 + csv2000_s$GAJ007 + csv2000_s$GAJ008 + csv2000_s$GAJ009
  
#GAK001:      Median year structure built
#FKL001:      Number of Occupied housing units
#FKL002:      Number of Vacant
#FKN001:      Owner occupied
#FKN002:      Renter occupied

#Subset to columns of interest
csv2000_sn = csv2000_s[, c("GISJOIN","STATE.x","COUNTY.x","YEAR.x","FL5001","FMR001","FMR002","FMR003","FMR004","FMR005",
                           "FMR006","FMR007","orgnHisp","GMY001","GBO001","GB7001","eduSum","gteBach","hoUnitT","houGte30",
                           "GAK001","FKL001","FKL002","FKN001","FKN002")]

#Rename
colnames(csv2000_sn) =  c("GISJOIN","STATE","COUNTY","YEAR","totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2",
                          "orgnHisp","medInc","medRent","homeVal","eduSum","gteBach","hoUnitT","houGte30","medYrHou","occHou",
                          "vacHou","ownHou","rentHou")

# STEP 3C -----------------------------------------------
#Subset to variables of interest 2010

#JMBE001:     Total Population
#JMBE002:     White alone
#JMBE003:     Black or African American alone
#JMBE004:     American Indian and Alaska Native alone
#JMBE005:     Asian alone
#JMBE006:     Native Hawaiian and Other Pacific Islander alone
#JMBE007:     Some other race alone
#JMBE008:     Two or more races
#JMJE012:     Hispanic or Latino

#JN9E001:     Education Pop Total (ppl over 25)
#JN9E015:     Male: Bachelor's degree
#JN9E016:     Male: Master's degree
#JN9E017:     Male: Professional school degree
#JN9E018:     Male: Doctorate degree
#JN9E032:     Female: Bachelor's degree
#JN9E033:     Female: Master's degree
#JN9E034:     Female: Professional school degree
#JN9E035:     Female: Doctorate degree

#Add educational attainment columns (Bachelor's degree or more)
csv2010_s$gteBach = csv2010_s$JN9E015 + csv2010_s$JN9E016 + csv2010_s$JN9E017 + csv2010_s$JN9E018 + csv2010_s$JN9E032 +
  csv2010_s$JN9E033 + csv2010_s$JN9E034 + csv2010_s$JN9E035

#JOIE001:     Median household income in the past 12 months (in 2010 inflation-adjusted dollars)
#JS5E001:     Median gross rent
#JTIE001:     Median home value (dollars)

#JRJE001:     Total housing units
#JRJE002:     Number of occupied housing units
#JRJE003:     Number of Vacant housing units

#JRKE001:     Total number of occupied housing units
#JRKE002:     Owner occupied
#JRKE003:     Renter occupied

#JSEE001:     Median year structure built

#JSDE001:     Total
#JSDE002:     Built 2005 or later
#JSDE003:     Built 2000 to 2004
#JSDE004:     Built 1990 to 1999
#JSDE005:     Built 1980 to 1989
#JSDE006:     Built 1970 to 1979
#JSDE007:     Built 1960 to 1969
#JSDE008:     Built 1950 to 1959
#JSDE009:     Built 1940 to 1949
#JSDE010:     Built 1939 or earlier

csv2010_s$houGte30 = csv2010_s$JSDE005 + csv2010_s$JSDE006 + csv2010_s$JSDE007 + csv2010_s$JSDE008 + csv2010_s$JSDE009 + csv2010_s$JSDE010

#Subset to columns of interest
csv2010_sn = csv2010_s[, c("GISJOIN","STATE.x","COUNTY.x","YEAR.x","JMBE001","JMBE002","JMBE003","JMBE004","JMBE005","JMBE006","JMBE007","JMBE008",
                         "JMJE012","JN9E001","gteBach","JOIE001","JS5E001","JTIE001","JRJE001","JRJE002","JRJE003","JRKE002","JRKE003",
                         "JSEE001","houGte30"
                         )]

#Rename                                 
colnames(csv2010_sn) =  c("GISJOIN","STATE","COUNTY","YEAR","totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2",
                          "orgnHisp","eduSum","gteBach","medInc","medRent","homeVal","hoUnitT","occHou","vacHou","ownHou","rentHou",
                          "medYrHou","houGte30")


# STEP 3D -----------------------------------------------
#Subset to variables of interest 2015

#ADKXE001:    Total
#ADKXE002:    White alone
#ADKXE003:    Black or African American alone
#ADKXE004:    American Indian and Alaska Native alone
#ADKXE005:    Asian alone
#ADKXE006:    Native Hawaiian and Other Pacific Islander alone
#ADKXE007:    Some other race alone
#ADKXE008:    Two or more races
#ADK5E012:    Hispanic or Latino

#ADMZE001:    Education Pop Total (ppl over 25)
#ADMZE022:    Bachelor's degree
#ADMZE023:    Master's degree
#ADMZE024:    Professional school degree
#ADMZE025:    Doctorate degree

#Add educational attainment columns (Bachelor's degree or more)
csv2015_s$gteBach = csv2015_s$ADMZE022 + csv2015_s$ADMZE023 + csv2015_s$ADMZE024 + csv2015_s$ADMZE025

#ADNKE001:    Median household income in the past 12 months (in 2015 inflation-adjusted dollars)
#ADRKE001:    Median gross rent
#ADRWE001:    Median value (dollars)

#ADPZE001:    Total number of housing units
#ADPZE002:    Occupied housing units
#ADPZE003:    Vacant housing units

#ADP0E001:    Occupied housing units
#ADP0E002:    Owner occupied
#ADP0E003:    Renter occupied

#ADQTE001:    Median year structure built

#ADQSE001:    Total
#ADQSE002:    Built 2014 or later
#ADQSE003:    Built 2010 to 2013
#ADQSE004:    Built 2000 to 2009
#ADQSE005:    Built 1990 to 1999
#ADQSE006:    Built 1980 to 1989
#ADQSE007:    Built 1970 to 1979
#ADQSE008:    Built 1960 to 1969
#ADQSE009:    Built 1950 to 1959
#ADQSE010:    Built 1940 to 1949
#ADQSE011:    Built 1939 or earlier
csv2015_s$houGte35 = csv2015_s$ADQSE006 + csv2015_s$ADQSE007 + csv2015_s$ADQSE008 + csv2015_s$ADQSE009 + csv2015_s$ADQSE010 + csv2015_s$ADQSE011


#Subset to columns of interest
csv2015_sn = csv2015_s[, c("GISJOIN","STATE.x","COUNTY.x","YEAR.x","ADKXE001","ADKXE002","ADKXE003","ADKXE004","ADKXE005","ADKXE006","ADKXE007","ADKXE008",
                           "ADK5E012","ADMZE001","gteBach","ADNKE001","ADRKE001","ADRWE001","ADPZE001","ADPZE002","ADPZE003","ADP0E002","ADP0E003",
                           "ADQTE001","houGte35")]

#Rename                                 
colnames(csv2015_sn) =  c("GISJOIN",   "STATE",  "COUNTY",  "YEAR",   "totPop",  "raceW",  "raceB",    "raceN",  "raceA",    "raceP",    "raceO",   "race2",
                          "orgnHisp",   "eduSum","gteBach",  "medInc",  "medRent","homeVal", "hoUnitT",  "occHou",  "vacHou","ownHou","rentHou",
                          "medYrHou","houGte35")
  
# STEP 3E -----------------------------------------------
#Subset to variables of interest 2020

#AMPWE001:    Total
#AMPWE002:    White alone
#AMPWE003:    Black or African American alone
#AMPWE004:    American Indian and Alaska Native alone
#AMPWE005:    Asian alone
#AMPWE006:    Native Hawaiian and Other Pacific Islander alone
#AMPWE007:    Some other race alone
#AMPWE008:    Two or more races
#AMP3E012:    Hispanic or Latino

#AMRZE001:    Total (pop over 25)
#AMRZE022:    Bachelor's degree
#AMRZE023:    Master's degree
#AMRZE024:    Professional school degree
#AMRZE025:    Doctorate degree

#Add educational attainment columns (Bachelor's degree or more)
csv2020_s$gteBach = csv2020_s$AMRZE022 + csv2020_s$AMRZE023 + csv2020_s$AMRZE024 + csv2020_s$AMRZE025
 
#AMR8E001:    Median household income in the past 12 months (in 2020 inflation-adjusted dollars)
#AMVZE001:    Median gross rent
#AMWBE001:    Median value (dollars)

#AMUEE001:    Total number of housing units
#AMUEE002:    Occupied housing units
#AMUEE003:    Vacant housing units

#AMUFE001:    Occupied housing units
#AMUFE002:    Owner occupied
#AMUFE003:    Renter occupied

#AMU8E001:    Median year structure built

#AMU7E001:    Total
#AMU7E002:    Built 2014 or later
#AMU7E003:    Built 2010 to 2013
#AMU7E004:    Built 2000 to 2009
#AMU7E005:    Built 1990 to 1999
#AMU7E006:    Built 1980 to 1989
#AMU7E007:    Built 1970 to 1979
#AMU7E008:    Built 1960 to 1969
#AMU7E009:    Built 1950 to 1959
#AMU7E010:    Built 1940 to 1949
#AMU7E011:    Built 1939 or earlier

csv2020_s$houGte30 = csv2020_s$AMU7E005 + csv2020_s$AMU7E006 + csv2020_s$AMU7E007 + csv2020_s$AMU7E008 + csv2020_s$AMU7E009 + csv2020_s$AMU7E010 + csv2020_s$AMU7E011

#Subset to columns of interest
csv2020_sn = csv2020_s[, c("GISJOIN","STATE.x","COUNTY.x","YEAR.x","AMPWE001","AMPWE002","AMPWE003","AMPWE004","AMPWE005","AMPWE006","AMPWE007","AMPWE008",
                           "AMP3E012","AMRZE001","gteBach","AMR8E001","AMVZE001","AMWBE001","AMUEE001","AMUEE002","AMUEE003","AMUFE002","AMUFE003",
                           "AMU8E001","houGte30")]

#Rename                                 
colnames(csv2020_sn) =  c("GISJOIN",   "STATE",  "COUNTY",  "YEAR",  "totPop",   "raceW",   "raceB",   "raceN",    "raceA", "raceP",    "raceO",   "race2",
                         "orgnHisp"   ,"eduSum","gteBach",   "medInc" ,"medRent","homeVal",  "hoUnitT",  "occHou",  "vacHou",  "ownHou","rentHou",
                         "medYrHou","houGte30")

# STEP 4 -----------------------------------------------    
#Merge with SHP and export
export1940 = inner_join(shp1940,csv1940_sn, by='GISJOIN')
export2000 = inner_join(shp2000,csv2000_sn, by='GISJOIN')
export2010 = inner_join(shp2010,csv2010_sn, by='GISJOIN')
export2015 = inner_join(shp2015,csv2015_sn, by='GISJOIN')
export2020 = inner_join(shp2020,csv2020_sn, by='GISJOIN')

#Select columns for export
export1940 = export1940[, c("GISJOIN","STATE.y","COUNTY.y","YEAR","totPop","homeVal","raceW","raceNW","raceB","raceFBW","empM","empF",
                            "eduSum","eduHS","hoUnitT","homeNRep","homeRepT","homeWRadio","homeRadioT","pplHoUnit","occHou","ownHou",
                            "rentHou","geometry")]
export2000 = export2000[, c("GISJOIN","STATE.y","COUNTY.y","YEAR","totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2",
                            "orgnHisp","medInc","medRent","homeVal","eduSum","gteBach","hoUnitT","houGte30","medYrHou","occHou",
                            "vacHou","ownHou","rentHou","geometry")]
export2010 = export2010[, c("GISJOIN","STATE","COUNTY","YEAR","totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2",
                            "orgnHisp","eduSum","gteBach","medInc","medRent","homeVal","hoUnitT","occHou","vacHou","ownHou","rentHou",
                            "medYrHou","houGte30","geometry")]
export2015 = export2015[, c("GISJOIN","STATE","COUNTY","YEAR","totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2",
                            "orgnHisp","eduSum","gteBach","medInc","medRent","homeVal","hoUnitT","occHou","vacHou","ownHou","rentHou",
                            "medYrHou","houGte35","geometry")]
export2020 = export2020[, c("GISJOIN","STATE","COUNTY","YEAR","totPop","raceW","raceB","raceN","raceA","raceP","raceO","race2",
                            "orgnHisp","eduSum","gteBach","medInc","medRent","homeVal","hoUnitT","occHou","vacHou","ownHou","rentHou",
                            "medYrHou","houGte30","geometry")]

#Export
st_write(export1940, "./Data/Census/Cleaned/census1940_Chicago_20241009.shp")
st_write(export2000, "./Data/Census/Cleaned/census2000_Chicago_20240930.shp")
st_write(export2010, "./Data/Census/Cleaned/census2010_Chicago_20240930.shp")
st_write(export2015, "./Data/Census/Cleaned/census2015_Chicago_20240930.shp")
st_write(export2020, "./Data/Census/Cleaned/census2020_Chicago_20240930.shp")
                                       