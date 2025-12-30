#Michelle Stuhlmacher
#2020/06/20

#GOAL: Find the proportion of each tract belonging to each redlining grade.
#Methodology from ChiVes

#STEPS:
#1. Load in data and libraries
#2. Clip to city boundaries
#3. Run the calculations

#Final rules for grading:
#1. At least 25% of the census tract needs to have a grade, if it does not it is ungraded and is dropped from the analysis (Xu 2022)
#Check how many census tracts have 25% coverage when you add all the grades together but don't have 25% coverage of any individual grade
#Need to decide how those get treated
#2. If a census tract has one grade that covers 50% or more of the census tract, this is the grade assigned.
#(because mixed grades might end up being dropped from our analysis because they are hard to interpret, a mixed grade that is not with 
#the next closest grade (i.e., AB) is very rare. In Chicago there is only 1 BD tract)
#3. For census tracts that have more than 25% of multiple grades, both letter grades are assigned (mixed grade).
#These might end up being dropped depending on how many there are and if they distract from the overall interpretation

# STEP 1 -----------------------------------------------
# Import data and libraries

library(tidyverse)
library(sf)
library(ggplot2)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop

#Redlining data
holcCHI_WGS84 = st_read('./Data/Redlining/ILChicago1940/cartodb-query.shp')

#Census tract data
tractCHI = st_read('./Data/Census/Cleaned/megatractCHI_20241217.shp')

#City boundaries
boundsCHI_WGS84 = st_read('./Data/CityBounds/Chicago/CityBoundary.shp')

#Reproject redlining data to match census data
holcCHI = st_transform(holcCHI_WGS84,crs=st_crs(tractCHI))

#Reproject city boundaries to match census data
boundsCHI = st_transform(boundsCHI_WGS84,crs=st_crs(tractCHI))

# #Plot to check
# plot(st_geometry(tractCHI))
# plot(st_geometry(holcCHI),add = T, border = 'blue')
# plot(st_geometry(boundsCHI),add = T, border = 'red')

options(scipen = 999)

# STEP 2 -----------------------------------------------
# Clip to city boundaries

#Filter census tract boundary to just one year
#1940 census data doesn't cover all of Portland, since this is our limiting factor, using 1940 as the filter
tractCHIs = tractCHI[tractCHI$'YEAR' == '1940', ] 

# #Plot to check
# plot(st_geometry(tractCHIs))
# plot(st_geometry(holcCHI),add = T, border = 'blue')
# plot(st_geometry(boundsCHI),add = T, border = 'red')


# STEP 3a -----------------------------------------------
# Run the calculations (Chicago)
tractCHIs$proportionA <- NA
tractCHIs$proportionB <- NA
tractCHIs$proportionC <- NA
tractCHIs$proportionD <- NA

# loop through all tracts
for(i in 1:nrow(tractCHIs)){
  # assign each tract in turn to tract
  tract = tractCHIs[i,]
  # calculate total area of the current tract
  area_tract = st_area(tract)
  # find all of the holc boudaries which are in the current tract
  holc_in_tract = st_intersection(tract, holcCHI)
  # calculate the proportions - how much of the area within each tract is assigned to each category by HOLC
  tractCHIs[i,]$proportionA <- sum(st_area(holc_in_tract %>% filter(holc_grade == "A"))) / area_tract
  tractCHIs[i,]$proportionB <- sum(st_area(holc_in_tract %>% filter(holc_grade == "B"))) / area_tract
  tractCHIs[i,]$proportionC <- sum(st_area(holc_in_tract %>% filter(holc_grade == "C"))) / area_tract
  tractCHIs[i,]$proportionD <- sum(st_area(holc_in_tract %>% filter(holc_grade == "D"))) / area_tract
  # print i because this takes a little while, this way you can keep an eye on if its running
  print(i)
}

# calculate the proportion of each tract which is C or D by adding the two already existing variables for C and D together
tractCHIs$proportionCorD <- tractCHIs$proportionC + tractCHIs$proportionD

# make a list of all proportions
all_grades_vectors = list(a = tractCHIs$proportionA, 
                           b = tractCHIs$proportionB, 
                           c = tractCHIs$proportionC, 
                           d = tractCHIs$proportionD, 
                           cd = tractCHIs$proportionCorD)

# assign the primary grade to the category which covers the greatest proportion of tract area
tractCHIs$primary_grade_4levels <- pmap(all_grades_vectors, function(a,b,c,d,cd) which(c(a,b,c,d) == max(c(a,b,c,d)))) %>%
  map(~ifelse(length(.) > 1, NA, .)) %>%
  unlist() %>%
  as.factor()
# rename the categories in primary_grade_4levels from 1,2,3,4 to A,B,C,D
levels(tractCHIs$primary_grade_4levels) <- c("A","B","C","D")

#  assign the primary grade to the category which covers the greatest proportion of tract area, but consider C and D together
tractCHIs$primary_grade_3levels <- pmap(all_grades_vectors, function(a,b,c,d,cd) which(c(a,b,cd) == max(c(a,b,cd)))) %>%
  map(~ifelse(length(.) > 1, NA, .)) %>%
  unlist() %>%
  as.factor()
# rename the categories in primary_grade_4levels from 1,2,3 to A,B,CD
levels(tractCHIs$primary_grade_3levels) <- c("A","B","CD")

#Create additional variables
# #prmr_80 : Letter grade for 80% or more overlapping census tract (A/B/C/D/NA)
# #Chicago
# tractCHIs$prmr4_80 = ifelse(tractCHIs$proportionA >= 0.80, "A", 
#                          ifelse(tractCHIs$proportionB >= 0.80, "B",
#                                 ifelse(tractCHIs$proportionC >= 0.80, "C",
#                                        ifelse(tractCHIs$proportionD >= 0.80, "D",
#                                               NA))))
# tractCHIs$prmr3_80 = ifelse(tractCHIs$proportionA >= 0.80, "A", 
#                          ifelse(tractCHIs$proportionB >= 0.80, "B",
#                                 ifelse(tractCHIs$proportionCorD >= 0.80, "CD",
#                                        NA)))

#prmr_50 : Letter grade for 50% or more overlapping census tract (A/B/C/D/NA)
tractCHIs$prmr4_50 = ifelse(tractCHIs$proportionA >= 0.50, "A", 
                         ifelse(tractCHIs$proportionB >= 0.50, "B",
                                ifelse(tractCHIs$proportionC >= 0.50, "C",
                                       ifelse(tractCHIs$proportionD >= 0.50, "D",
                                              NA))))
tractCHIs$prmr3_50 = ifelse(tractCHIs$proportionA >= 0.50, "A", 
                         ifelse(tractCHIs$proportionB >= 0.50, "B",
                                ifelse(tractCHIs$proportionCorD >= 0.50, "CD",
                                       NA)))

#prmr_25 : Letter grade for 25% or more overlapping census tract (A/B/C/D/NA)
tractCHIs$prmr4_25 <- apply(tractCHIs, 1, function(row) {
  letters <- c("A", "B", "C", "D")
  proportions <- row[c("proportionA", "proportionB", "proportionC", "proportionD")]
  letter <- letters[proportions >= 0.25]
  if (length(letter) > 0) {
    return(paste(letter, collapse = ""))  # Handles ties
  } else {
    return(NA)  # Return NA if no proportions >= 25%
  }
})

ggplot(tractCHIs, aes(x = prmr4_25)) +
  geom_bar() +
  labs(title = "Distribution of Grades", x = "Grades", y = "Count") +
  theme_minimal()

tractCHIs$prmr3_25 <- apply(tractCHIs, 1, function(row) {
  letters3 <- c("A", "B", "CD")
  proportions3 <- row[c("proportionA", "proportionB", "proportionCorD")]
  letter3 <- letters3[proportions3 >= 0.25]
  if (length(letter3) > 0) {
    return(paste(letter3, collapse = ""))  # Handles ties
  } else {
    return(NA)  # Return NA if no proportions >= 25%
  }
})

# ggplot(tractCHIs, aes(x = prmr3_25)) +
#   geom_bar() +
#   labs(title = "Distribution of 3 Grades", x = "Grades", y = "Count") +
#   theme_minimal()

#mixGrade : Mixed grade (1 for Y, 0 for N)
tractCHIs$mixGrade = ifelse(tractCHIs$proportionA > 0 & tractCHIs$proportionB == 0 & tractCHIs$proportionC == 0 & tractCHIs$proportionD == 0, "0", 
                         ifelse(tractCHIs$proportionA == 0 & tractCHIs$proportionB > 0 & tractCHIs$proportionC == 0 & tractCHIs$proportionD == 0, "0",
                                ifelse(tractCHIs$proportionA == 0 & tractCHIs$proportionB == 0 & tractCHIs$proportionC > 0 & tractCHIs$proportionD == 0, "0",
                                       ifelse(tractCHIs$proportionA == 0 & tractCHIs$proportionB == 0 & tractCHIs$proportionC == 0 & tractCHIs$proportionD > 0, "0",
                                              "1"))))

#Final Grades
#1. At least 25% of the census tract needs to have a grade, if it does not it is ungraded and is dropped from the analysis (Wu 2022)
#2. If a census tract has one grade that covers 50% or more of the census tract, this is the grade assigned.
#3. For census tracts that have more than 25% of multiple grades, both letter grades are assigned (mixed grade).

#First pass to assign the majority grade
tractCHIs <- tractCHIs %>%
  mutate(finalGrade = case_when(
    (proportionA + proportionB + proportionC + proportionD) < 0.25 ~ NA, #NA for census tracts where the prop redlined isn't greater than 25%
    proportionA >= 0.5 ~ "A",
    proportionB >= 0.5 ~ "B",
    proportionC >= 0.5 ~ "C",
    proportionD >= 0.5 ~ "D",
    TRUE ~ "M" #give "M" to any tracts without majority
  ))

# Second pass to replace "M" with grades between 25-50%
replace_M_values <- function(row) {
  if (!is.na(row["finalGrade"]) && row["finalGrade"] == "M") {
    letters <- c("A", "B", "C", "D")
    proportions <- as.numeric(row[c("proportionA", "proportionB", "proportionC", "proportionD")])
    
    letter <- letters[proportions >= 0.25] # Get letters for proportions >= 0.25
    
    if (length(letter) > 0) {
      return(paste(letter, collapse = ""))  # Return letters if found
    } else {
      return("MM")  # Return NA if no proportions >= 25%
    }
  } else {
    #return(row["finalGrade"])  # Keep existing value if not "M"
    return(as.character(row["finalGrade"]))  # Keep existing value if not "M"
  }
}

# Apply function to each row
tractCHIs$finalGrade2 <- apply(tractCHIs, 1, replace_M_values)

tractCHIs %>%
  group_by(finalGrade2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

###----Export----
#Export as shapefile
st_write(tractCHIs,"./Data/Redlining/PropByTract/SHP/megatractRedPropCHI_20241217.shp")

#Export as csv
tractCHI_csv = st_drop_geometry(tractCHIs) #remove geometry
write.csv(tractCHI_csv, "./Data/Redlining/PropByTract/megatractRedPropCHI_20241217.csv")