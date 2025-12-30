#Michelle Stuhlmacher

#GOAL: Extract the 1) change in park coverage and 2) mean and median tree canopy coverage level for each census tract in each time period

#STEPS:
#1. Load in data and libraries
#The 3 park shapefiles are:
# -all the parks that existed in 2010
# -all the parks that were added between 2011-2015
# -all the parks that were added between 2016-2020

#2. Run the tree calculations and export

#3. Calculate the change in park area per census tract for each period:
#2010-2015 change in park area (park area in 2015 - park area in 2010)
#2015-2020 change in park area (park area in 2020 - park area in 2015)

#4. Buffer 0.5 mile (10 min walk) around each park and apply park area to census tracts within buffer

#5. Clean up and export

# STEP 1 -----------------------------------------------
# Import data and libraries

#Libraries
library(sf)
library(dplyr)
library(raster)

#Set working directory
#setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop
setwd("C:/Users/mfstu/OneDrive - DePaul University/Research/Redlining") #big girl build

#parks split out by year from parkDates.R
#park2010 = st_read('./Data/CPD_Boundaries/CPD_Boundaries_2010.shp')
park2015 = st_read('./Data/CPD_Boundaries/CPD_Boundaries_2015.shp')
park2020 = st_read('./Data/CPD_Boundaries/CPD_Boundaries_2020.shp')

#census tracts from combinedCensusSHP.ipynb
tractCHI = st_read('./Data/Census/Cleaned/megatractCHI_20241217.shp')

#City boundaries
boundsCHI_WGS84 = st_read('./Data/CityBounds/Chicago/CityBoundary.shp')

#Tree canopy rasters
chiTCC2011 = raster('./Data/TreeCanopy/Clipped2011/tccCHIclip2011.tif')
chiTCC2015 = raster('./Data/TreeCanopy/Clipped2015/tccCHIclip2015.tif')
chiTCC2020 = raster('./Data/TreeCanopy/Clipped2020/tccCHIclip2020.tif')

#Projections are not the same:
#Reproject so parks match census tracts
park2015RP = st_transform(park2015,crs=st_crs(tractCHI))
park2020RP = st_transform(park2020,crs=st_crs(tractCHI))

#Reproject city boundaries to match census data
boundsCHI = st_transform(boundsCHI_WGS84,crs=st_crs(tractCHI))

#Reproject tree canopy rasters to match census data
tccCHI2011 = projectRaster(chiTCC2011, crs=projection(tractCHI, asText = T))
tccCHI2015 = projectRaster(chiTCC2015, crs=projection(tractCHI, asText = T))
tccCHI2020 = projectRaster(chiTCC2020, crs=projection(tractCHI, asText = T))

# #Make sure everything lines up
# plot(st_geometry(tractCHI))
# plot(st_geometry(park2015RP), col = 'blue', add = T)
# plot(st_geometry(park2020RP), col = 'red', add = T)
# 
# plot(tccCHI2011)
# plot(st_geometry(tractCHI), add = T)

# STEP 2 -----------------------------------------------
# Run the tree calculations and export

#Clip census tracts to city boundary
tractCHIs = tractCHI[boundsCHI,] 

#Filter census tract boundary by year
tractCHI2010 = tractCHIs[tractCHIs$'YEAR' == '2006-2010', ]
tractCHI2015 = tractCHIs[tractCHIs$'YEAR' == '2011-2015', ]
tractCHI2020 = tractCHIs[tractCHIs$'YEAR' == '2016-2020', ]

#2011
tractCHI2010mean = raster::extract(tccCHI2011,tractCHI2010,fun=mean,na.rm=TRUE,df=TRUE)
tractCHI2010mean$plot_id <- tractCHI2010$cluster_id # grab the names of the plots from the original tract file
names(tractCHI2010mean) <- c('ID','tccMean2011','cluster_id') #fix the column names

tractCHI2010med = raster::extract(tccCHI2011,tractCHI2010,fun=median,na.rm=TRUE,df=TRUE)
tractCHI2010med$plot_id <- tractCHI2010$cluster_id # grab the names of the plots from the original tract file
names(tractCHI2010med) <- c('ID','tccMed2011','cluster_id') #fix the column names

#2015
tractCHI2015mean = raster::extract(tccCHI2015,tractCHI2015,fun=mean,na.rm=TRUE,df=TRUE)
tractCHI2015mean$plot_id <- tractCHI2015$cluster_id # grab the names of the plots from the original tract file
names(tractCHI2015mean) <- c('ID','tccMean2015','cluster_id') #fix the column names

tractCHI2015med = raster::extract(tccCHI2015,tractCHI2015,fun=median,na.rm=TRUE,df=TRUE)
tractCHI2015med$plot_id <- tractCHI2015$cluster_id # grab the names of the plots from the original tract file
names(tractCHI2015med) <- c('ID','tccMed2015','cluster_id') #fix the column names

#2020
tractCHI2020mean = raster::extract(tccCHI2020,tractCHI2020,fun=mean,na.rm=TRUE,df=TRUE)
tractCHI2020mean$plot_id <- tractCHI2020$cluster_id # grab the names of the plots from the original tract file
names(tractCHI2020mean) <- c('ID','tccMean2020','cluster_id') #fix the column names

tractCHI2020med = raster::extract(tccCHI2020,tractCHI2020,fun=median,na.rm=TRUE,df=TRUE)
tractCHI2020med$plot_id <- tractCHI2020$cluster_id # grab the names of the plots from the original tract file
names(tractCHI2020med) <- c('ID','tccMed2020','cluster_id') #fix the column names

#Add the values back to the original yearly df
tractCHI2010 = merge(tractCHI2010, tractCHI2010mean, by = 'cluster_id')
tractCHI2010 = merge(tractCHI2010, tractCHI2010med, by = 'cluster_id')
tractCHI2010 = merge(tractCHI2010, tractCHI2015mean, by = 'cluster_id')
tractCHI2010 = merge(tractCHI2010, tractCHI2015med, by = 'cluster_id')
tractCHI2010 = merge(tractCHI2010, tractCHI2020mean, by = 'cluster_id')
tractCHI2010 = merge(tractCHI2010, tractCHI2020med, by = 'cluster_id')

#Test if output looks like expected
plot(tractCHI2010["tccMean2011"])

#Remove all columns except cluster id and the mean/median values for each year
keep = c("cluster_id","tccMean2011","tccMed2011","tccMean2015","tccMed2015","tccMean2020","tccMed2020")
tractCHI2010 = tractCHI2010[keep]

#Export shapefile
st_write(tractCHI2010,"./Data/EOSummaryTables/TCC/ChicagoTCCallYears_20250326.shp")

#Remove geometry and export csv
chiSHP_csv = st_drop_geometry(tractCHI2010) #remove geometry
write.csv(chiSHP_csv, "./Data/EOSummaryTables/TCC/ChicagoTCCallYears_20250326.csv")

# STEP 3 -----------------------------------------------
#Calculate the change in park area per census tract for each period:
#Park area in 2010 = park2010
#Park area in 2015 = park2010 + park2015
#park area in 2020 = park2010 + park2015 + park2020

tractCHI = tractCHI[tractCHI$'YEAR' == '1940', ]  #Subset to only 1940, remove census tracts that have missing data in 1940

##---2010-2015 change in park area (park area in 2015 - park area in 2010)---##
#Following this stack exchange: https://gis.stackexchange.com/questions/397919/sum-aggregation-of-polygons-values-within-other-polygons-using-dyplr
SHAPE2015INT = st_intersection(park2015RP,tractCHI)

#Calculate area
SHAPE2015INT$areaM2_2015 = st_area(SHAPE2015INT)

#Sum the area of new parks in each census tract (park2015)
area2015DF = SHAPE2015INT %>% 
  st_drop_geometry() %>%
  group_by(cluster_id) %>% 
  summarise(areaM2_2015 = sum(areaM2_2015))

#Join these values back with original census DF
tractCHI = left_join(tractCHI, area2015DF, by = "cluster_id")

#Change in the remaining census tracts will be zero
tractCHI$areaM2_2015[is.na(tractCHI$areaM2_2015)] = 0
tractCHI$area2015m2 = as.numeric(tractCHI$areaM2_2015) 

##---2015-2020 change in park area (park area in 2020 - park area in 2015)---##
SHAPE2020INT = st_intersection(park2020RP,tractCHI)

#Calculate area
SHAPE2020INT$areaM2_2020 = st_area(SHAPE2020INT)

#Sum the area of new parks in each census tract (park2020)
area2020DF = SHAPE2020INT %>% 
  st_drop_geometry() %>%
  group_by(cluster_id) %>% 
  summarise(areaM2_2020 = sum(areaM2_2020))

#Join these values back with original census DF
tractCHI = left_join(tractCHI, area2020DF, by = "cluster_id")

#Change in the remaining census tracts will be zero
tractCHI$areaM2_2020[is.na(tractCHI$areaM2_2020)] = 0
tractCHI$area2020m2 = as.numeric(tractCHI$areaM2_2020) #convert to numeric from unit

# STEP 4 -----------------------------------------------
#Buffer 0.5 mile (10 min walk) around each park and apply park area to census tracts within buffer

#Calculate the area of each park
park2015RP$areaM2_2015 = st_area(park2015RP)
park2020RP$areaM2_2020 = st_area(park2020RP)

#Buffer the park shapefiles, 0.5 miles is 804.672 meters
park2015Buff = st_buffer(park2015RP,804.672)
park2020Buff = st_buffer(park2020RP,804.672)

#Find the census tracts that intersect with the buffers
BUFF2015INT = st_intersection(park2015Buff,tractCHI)
BUFF2020INT = st_intersection(park2020Buff,tractCHI)
#This results in a DF where there are duplicate parks and tracts. 

#Need to collapse so that the park area column is summed based on the cluster id column
area2015DFBuff = BUFF2015INT %>% 
  st_drop_geometry() %>%
  group_by(cluster_id) %>% 
  summarise(areaM2_2015Buff = sum(areaM2_2015))

area2020DFBuff = BUFF2020INT %>% 
  st_drop_geometry() %>%
  group_by(cluster_id) %>% 
  summarise(areaM2_2020Buff = sum(areaM2_2020))

#Join these values back with original census DF
tractCHIBuffArea = left_join(tractCHI, area2015DFBuff, by = "cluster_id")
tractCHIBuffArea = left_join(tractCHIBuffArea, area2020DFBuff, by = "cluster_id")

#Change in the remaining census tracts will be zero
tractCHIBuffArea$areaM2_2015Buff[is.na(tractCHIBuffArea$areaM2_2015Buff)] = 0
tractCHIBuffArea$area2015Buffm2 = as.numeric(tractCHIBuffArea$areaM2_2015Buff) #convert to numeric from unit

tractCHIBuffArea$areaM2_2020Buff[is.na(tractCHIBuffArea$areaM2_2020Buff)] = 0
tractCHIBuffArea$area2020Buffm2 = as.numeric(tractCHIBuffArea$areaM2_2020Buff) #convert to numeric from unit

# STEP 5 -----------------------------------------------
# Clean up and export

#Filter to just the park area columns
keep = c("cluster_id","area2015m2","area2020m2","geometry")
exportSHP = tractCHI[keep]

keepBuff = c("cluster_id","area2015Buffm2","area2020Buffm2","geometry")
exportSHPBuff = tractCHIBuffArea[keepBuff]

#Export shapefile
st_write(exportSHP, "./Data/CPD_Boundaries/ParkAreaByTract_20250106.shp")
st_write(exportSHPBuff, "./Data/CPD_Boundaries/ParkBuffAreaByTract_20250114.shp")

#Export csv
exportCSV = st_drop_geometry(exportSHPBuff) #remove geometry
write.csv(exportCSV, "./Data/CPD_Boundaries/ParkBuffAreaByTract_20250114.csv")

