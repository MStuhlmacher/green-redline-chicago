#Michelle Stuhlmacher
#2024/12/13

# PART 1 -----------------------------------------------
#GOAL: Match the dates acquired from TPL data to the Chicago Park District data

#STEPS:
#1. Read in data and libraries
#2. Clean up data
#3. Merge

# PART 1, STEP 1 -----------------------------------------------
# Import data and libraries

#Libraries
#library(dplyr)
library(sf)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop

#CPD shapefile (from: https://data.cityofchicago.org/Parks-Recreation/CPD_Parks/ejsh-fztr/about_data)
CPD = st_read('./Data/CPD_Boundaries/CPD_Boundaries.shp')

#TPL csv
TPL = read.csv('./Data/CleanChicagoParkTPL/ParkServe_year_acq_chicago_MScleaned.csv')

# PART 1, STEP 2 -----------------------------------------------
# Clean up data

#Columns to keep CPD:
keepCPD = c("OBJECTID_1","PARK_NO","PARK","LOCATION","ZIP","PARK_CLASS","GISOBJID","geometry")
CPD_c = CPD[, keepCPD]

#Columns to keep TPL:
keepTPL = c("OBJECTID_1","ParkID","SourceID","Park_Name_Cap","Park_Designation","Park_Address_1","Park_Zip","Final_Year_Aquired")
TPL_c = TPL[, keepTPL]

# PART 1, STEP 3 -----------------------------------------------
# Merge

#Try on name and zip
DF1 = merge(x=CPD_c,y=TPL_c, by.x=c("PARK","ZIP"), by.y=c("Park_Name_Cap","Park_Zip"), all.x = T)

#Try on name only
DF2 = merge(x=CPD_c,y=TPL_c, by.x=c("PARK"), by.y=c("Park_Name_Cap"), all.x = T)

#Try on name and address
DF3 = merge(x=CPD_c,y=TPL_c, by.x=c("PARK","LOCATION"), by.y=c("Park_Name_Cap","Park_Address_1"), all.x = T)

#Try on address only
DF4 = merge(x=CPD_c,y=TPL_c, by.x=c("LOCATION"), by.y=c("Park_Address_1"), all.x = T)

#Count how many parks don't have a date with each of the 3 methods
sum(is.na(DF1$Final_Year_Aquired) | DF1$Final_Year_Aquired == "") #132
sum(is.na(DF2$Final_Year_Aquired) | DF2$Final_Year_Aquired == "") #103
sum(is.na(DF3$Final_Year_Aquired) | DF3$Final_Year_Aquired == "") #109
sum(is.na(DF4$Final_Year_Aquired) | DF4$Final_Year_Aquired == "") #107

#Going to use DF3 because it has the same number of rows as CPD (idk where the extra rows come from in DF1 and DF2)

# PART 1, STEP 4 -----------------------------------------------
# Clean up and export

#rename columns
colnames(DF3) =  c("PARK","LOCATION","OBJECTID_CPD","PARK_NO","ZIP","PARK_CLASS","GISOBJID","OBJECTID_TPL","ParkID_TPL","SourceID_TPL",
                   "Park_Designation_TPL","Park_Zip_TPL","Year_Aquired","geometry")

#export shp
st_write(DF3, "./Data/CPD_Boundaries/CPD_Boundaries_DateMerged_20241213.shp")

#remove geometry 
DF3_export = st_drop_geometry(DF3)

#export csv
write.csv(DF3_export,"./Data/CPD_Boundaries/CPD_Boundaries_DateMerged_20241213.csv")

# PART 2 -----------------------------------------------
#GOAL: Create 3 park shapefiles:
#1) all the parks that existed in 2010,
#2) all the parks that were added between 2011-2015
#3) all the parks that were added 2016-2020

#Use this to compute:
#2010-2015 change in park area (park area in 2015 - park area in 2010)
#2015-2020 change in park area (park area in 2020 - park area in 2015)

#STEPS:
#1. Read in data and libraries
#2. Merge the csv with the shapefile
#3. Create the 3 shapefiles for 2010, 2015, 2020
#4. Clean up and export

# PART 2, STEP 1 -----------------------------------------------
# Import data and libraries

#Libraries
library(dplyr)
library(sf)
library(ggplot2)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop

#CPD shapefile (from: https://data.cityofchicago.org/Parks-Recreation/CPD_Parks/ejsh-fztr/about_data)
CPD = st_read('./Data/CPD_Boundaries/CPD_Boundaries.shp')

#CSV with TPL dates (from merge above) and manually searched dates
DTS = read.csv("./Data/CPD_Boundaries/CPD_BoundariesWDatesFinal_20250103.csv")

# PART 2, STEP 2 -----------------------------------------------
# Join the csv with the shapefile, check to make sure no parks/dates were lost

#Subset CPD shapefile prior to joining
keepCPD = c("PARK_NO","PARK","LOCATION","PARK_CLASS","GISOBJID","geometry") #Columns to keep CPD:
CPD = CPD[, keepCPD]

#Merge on "PARK_NO"
park = left_join(CPD, DTS, by = c("PARK_NO","PARK","LOCATION","PARK_CLASS","GISOBJID"))

#Plot to make sure it looks as expected
ggplot(park) +
  geom_sf()

# PART 2, STEP 3 -----------------------------------------------
# Create the 3 shapefiles for 2010, 2015, 2020

#Get a sense for the distribution
table(park$Year_Acquired)

#separate out the "pre-2010" rows
pre2010Park = park %>%
  filter(grepl("pre-2010", Year_Acquired))
pre2010Park$Year_Acquired_Num = NA #adding an NA column so that columns match up for later rbind

#Convert the year column to numbers
numPark = park %>%
  filter(!grepl("pre-2010", Year_Acquired))

numPark$Year_Acquired_Num = as.integer(numPark$Year_Acquired)

#separate out based on the years
numPark2010 = numPark %>%
  filter(Year_Acquired_Num >= 1835 & Year_Acquired_Num <= 2010)
park2015 = numPark %>%
  filter(Year_Acquired_Num >= 2011 & Year_Acquired_Num <= 2015)
park2020 = numPark %>%
  filter(Year_Acquired_Num >= 2016 & Year_Acquired_Num <= 2020)

#Add the "pre-2010" rows to 2010 DF
park2010 = rbind(pre2010Park,numPark2010)

# PART 2, STEP 4 -----------------------------------------------
# Clean up and export

st_write(park2010, "./Data/CPD_Boundaries/CPD_Boundaries_2010.shp")
st_write(park2015, "./Data/CPD_Boundaries/CPD_Boundaries_2015.shp")
st_write(park2020, "./Data/CPD_Boundaries/CPD_Boundaries_2020.shp")

