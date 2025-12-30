#Michelle Stuhlmacher
#2024.10.02

#GOAL: Run analysis

#STEPS:
#1. Import data and libraries
#2. Categorize tracts by grade and homeownership trajectories
#3. Test regression assumptions
#4. Run Kruskal-Wallis and Dunn tests

# STEP 1 -----------------------------------------------
#Import data and libraries
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(moments)
library(stats)
library(FSA)
library(gridExtra)
library(grid)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/Redlining") #work laptop
#setwd("C:/Users/mfstu/OneDrive - DePaul University/Research/Redlining") #big girl build

#Combined DF (from combVar.R)
chiGreen = read.csv("./Data/EOSummaryTables/Combined/chiDF_20250326.csv")
#Census megatract shapefile
chiCensus = st_read('./Data/Census/Cleaned/megatractCHI_20241217.shp')

options(scipen = 999)

# STEP 2 -----------------------------------------------
#Break the census tracts into categories based on change in homeownership rates between 1940 and 2020. 

#calculate change in homeownership between 1940 and 2020
chiGreen$ownDelta = chiGreen$pctOwnHou2020 - chiGreen$pctOwnHou1940

#Remove rows with NA values for proportion of owners, they are missing most other census data as well
chi2 = chiGreen %>% drop_na(ownDelta)

#Some visualizations to see what we're working with. Delete later if needed
ggplot(chi2, aes(x=finalGrade2, y=ownDelta)) + 
  geom_boxplot() +
  labs(title = "Chicago")

chi2 %>%
  group_by(finalGrade2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Categories (Chicago):
#1. "Dec" - Home ownership rates declined
#2. "Inc" - Home ownership rates increased

#Create home ownership trajectory column for Chicago
chiOwn <- chi2 %>%
  group_by(finalGrade2) %>%
  mutate(
    group_median = median(ownDelta, na.rm = T),  # Calculate the median for each group
    ownTraj = case_when(
      ownDelta < 0 ~ paste0(finalGrade2, ": Dec"),
      ownDelta >= 0 ~ paste0(finalGrade2, ": Inc")
    )
  ) %>%
  ungroup()  # Ungroup to return to original dataframe structure

#Select only the non-mixed grades for visualization
chiOwnViz = chiOwn[chiOwn$finalGrade2 %in% c("A","B","C","D"), ]
chiOwnViz %>%
  group_by(ownTraj) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Table for showing demographic differences between increasing/decreasing homeownership in C and D grades
chiOwnViz %>%
  group_by(ownTraj) %>%
  summarise(
    pctB1940 = mean(pctB1940, na.rm = TRUE),
    pctW1940 = mean(pctW1940, na.rm = TRUE),
    pctOwn1940 = mean(pctOwnHou1940, na.rm = TRUE),
    homeVal1940 = mean(housValInf1940, na.rm = TRUE),
    pctOcc1940 = mean(pctOccHou1940, na.rm = TRUE),
    
    pctB2020 = mean(pctB2020, na.rm = TRUE),
    pctW2020 = mean(pctW2020, na.rm = TRUE),
    pctOwn2020 = mean(pctOwnHou2020, na.rm = TRUE),
    homeVal2020 = mean(homeVal2020, na.rm = TRUE),
    pctVac2020 = mean(pctVacHou2020, na.rm = TRUE)
  )

#Comparison to Chicago overall
chi2 %>%
  summarise(
    pctB1940 = mean(pctB1940, na.rm = TRUE),
    pctW1940 = mean(pctW1940, na.rm = TRUE),
    pctOwn1940 = mean(pctOwnHou1940, na.rm = TRUE),
    homeVal1940 = mean(housValInf1940, na.rm = TRUE),
    pctOcc1940 = mean(pctOccHou1940, na.rm = TRUE),
    
    pctB2020 = mean(pctB2020, na.rm = TRUE),
    pctW2020 = mean(pctW2020, na.rm = TRUE),
    pctOwn2020 = mean(pctOwnHou2020, na.rm = TRUE),
    homeVal2020 = mean(homeVal2020, na.rm = TRUE),
    pctVac2020 = mean(pctVacHou2020, na.rm = TRUE),
    count = n()
  )

#Join chiOwnViz with the shapefile for visualization
chiCensus = chiCensus[chiCensus$'YEAR' == '1940', ]  #Subset to only 1940

chiCensus = chiCensus[, c("cluster_id","geometry")] #Remove everything except the unique IDs

chiDF = full_join(chiCensus,chiOwnViz, by='cluster_id') #Join

#Figure showing the location of C and D home ownership trajectories 
ggplot(data = chiDF, aes(geometry = geometry)) +
  geom_sf(aes(fill = ownTraj)) +
  scale_fill_manual(breaks = c("A: Inc",  "A: Dec", "B: Inc", "B: Dec","C: Inc", "C: Dec", "D: Inc","D: Dec", NA),
                    values = c("#93BD6B","#517133","#99CBCC","#3E797A","#F49352", "#C0540C", "#EA9785","#AE381E","#FFFFFF"),
                    na.value = "#E0E0E0",
                    labels = c("A: Increasing", "A: Decreasing", "B: Increasing", "B: Decreasing","C: Increasing", 
                               "C: Decreasing", "D: Increasing","D: Decreasing", "Mixed or No Grade")) +
  theme_minimal() +
  theme(legend.position = "bottom",legend.title = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())+
  guides(fill = guide_legend(nrow = 3))+
  ggtitle("Homeownership Rate Change by HOLC Grade")

# STEP 3 -----------------------------------------------
#Determine if an ANOVA or Kruskal-Wallis is more appropriate for the data
  
#----Calculate skewness-----
#If skewness value lies above +1 or below -1, data is highly skewed. If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric
  
##TREE CANOPY##
skewness(chiOwnViz$deltaMeanTCC2010_2015, na.rm = T) #-0.4951988
#skewness(chiOwnViz$deltaMedTCC2010_2015, na.rm = T) 
skewness(chiOwnViz$deltaMeanTCC2015_2020, na.rm = T) #-0.6314676
#skewness(chiOwnViz$deltaMedTCC2015_2020, na.rm = T)

##NDVI##
skewness(chiOwnViz$deltaMeanNDVI2010_2015, na.rm = T) #0.6525928
#skewness(chiOwnViz$deltaMedNDVI2010_2015, na.rm = T) 
skewness(chiOwnViz$deltaMeanNDVI2015_2020, na.rm = T) #-0.2183113
#skewness(chiOwnViz$deltaMedNDVI2015_2020, na.rm = T) 

##LST##
skewness(chiOwnViz$deltaMeanLST2010_2015, na.rm = T) #-0.1591435
#skewness(chiOwnViz$deltaMedLST2010_2015, na.rm = T)
skewness(chiOwnViz$deltaMeanLST2015_2020, na.rm = T) # 0.1677422
#skewness(chiOwnViz$deltaMedLST2015_2020, na.rm = T)

##PARK##
skewness(chiOwnViz$area2015Buffm2, na.rm = T) #8.952637
skewness(chiOwnViz$area2020Buffm2, na.rm = T) #6.365794
#Very skewed

#INTERPRETATION: There are ~4 that are less than -0.5/0.5, the rest are more skewed
  
#----Plot using a qqplot----
##TREE CANOPY##
qqnorm(chiOwnViz$deltaMeanTCC2010_2015);qqline(chi2$deltaMeanTCC2010_2015, col=2)
#qqnorm(chiOwnViz$deltaMedTCC2010_2015);qqline(chi2$deltaMedTCC2010_2015, col=2)

qqnorm(chiOwnViz$deltaMeanTCC2015_2020);qqline(chi2$deltaMeanTCC2015_2020, col=2)
#qqnorm(chiOwnViz$deltaMedTCC2015_2020);qqline(chi2$deltaMedTCC2015_2020, col=2)

##NDVI##
qqnorm(chiOwnViz$deltaMeanNDVI2010_2015);qqline(chi2$deltaMeanNDVI2010_2015, col=2)
#qqnorm(chiOwnViz$deltaMedNDVI2010_2015);qqline(chi2$deltaMedNDVI2010_2015, col=2)

qqnorm(chiOwnViz$deltaMeanNDVI2015_2020);qqline(chi2$deltaMeanNDVI2015_2020, col=2)
#qqnorm(chiOwnViz$deltaMedNDVI2015_2020);qqline(chi2$deltaMedNDVI2015_2020, col=2)

##LST##
qqnorm(chiOwnViz$deltaMeanLST2010_2015);qqline(chi2$deltaMeanLST2010_2015, col=2)
#qqnorm(chiOwnViz$deltaMedLST2010_2015);qqline(chi2$deltaMedLST2010_2015, col=2)

qqnorm(chiOwnViz$deltaMeanLST2015_2020);qqline(chi2$deltaMeanLST2015_2020, col=2)
#qqnorm(chiOwnViz$deltaMedLST2015_2020);qqline(chi2$deltaMedLST2015_2020, col=2)
  
##PARK##
qqnorm(chiOwnViz$area2015Buffm2);qqline(chi2$area2015Buffm2, col=2)
qqnorm(chiOwnViz$area2020Buffm2);qqline(chi2$area2020Buffm2, col=2)
#Very skewed

#INTERPRETATION: There are some that could probably be corrected to be more normal, but not all. For consistency, going to use Kruskal-Wallis

# STEP 4 -----------------------------------------------
#Use Kruskal-Wallis and the Dunn Test to see if the green space trajectories were statistically significant in the different clusters.

#####----Chicago---#####
#Set up facet title for each year
chiOwnViz$title1015 = "2010-2015" 
chiOwnViz$title1520 = "2015-2020" 

#Only include C and D grades for the homeownership trajectories
codes = c("C: Inc", "C: Dec", "D: Inc","D: Dec")

chiOwnVizCD = 
  chiOwnViz %>%
  filter(ownTraj %in% codes)

####--TREE CANOPY--####
##Grades (2010-2015)##
#plot
tree1015 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=deltaMeanTCC2010_2015, color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-4.204,4.204) +
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y = "mean tree canopy cover % change") +
    theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

KWdeltaMeanTCC2010_2015_finalGrade_chi = kruskal.test(deltaMeanTCC2010_2015~finalGrade2, data = chiOwnViz)
DTdeltaMeanTCC2010_2015_finalGrade_chi = dunnTest(deltaMeanTCC2010_2015~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaMeanTCC2010_2015_finalGradeDF_chi = data.frame("Z" = DTdeltaMeanTCC2010_2015_finalGrade_chi$res$Z, "C" = DTdeltaMeanTCC2010_2015_finalGrade_chi$res$Comparison, "p" = DTdeltaMeanTCC2010_2015_finalGrade_chi$res$P.adj) #convert to df
DTdeltaMeanTCC2010_2015_finalGradeDF_chi = DTdeltaMeanTCC2010_2015_finalGradeDF_chi[order(DTdeltaMeanTCC2010_2015_finalGradeDF_chi$C),]
DTdeltaMeanTCC2010_2015_finalGradeDF_chi$sig = cut(DTdeltaMeanTCC2010_2015_finalGradeDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

##Grades (2015-2020)##
#plot
tree1520 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=deltaMeanTCC2015_2020, color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-4.204,4.204) +
  facet_grid(. ~title1520) +
  #scale_color_manual(values = c("#93BD6B","#99CBCC","#F49352","#EA9785"))+
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y = "")+
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

KWdeltaMeanTCC2015_2020_finalGrade_chi = kruskal.test(deltaMeanTCC2015_2020~finalGrade2, data = chiOwnViz)
DTdeltaMeanTCC2015_2020_finalGrade_chi = dunnTest(deltaMeanTCC2015_2020~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaMeanTCC2015_2020_finalGradeDF_chi = data.frame("Z" = DTdeltaMeanTCC2015_2020_finalGrade_chi$res$Z, "C" = DTdeltaMeanTCC2015_2020_finalGrade_chi$res$Comparison, "p" = DTdeltaMeanTCC2015_2020_finalGrade_chi$res$P.adj) #convert to df
DTdeltaMeanTCC2015_2020_finalGradeDF_chi = DTdeltaMeanTCC2015_2020_finalGradeDF_chi[order(DTdeltaMeanTCC2015_2020_finalGradeDF_chi$C),]
DTdeltaMeanTCC2015_2020_finalGradeDF_chi$sig = cut(DTdeltaMeanTCC2015_2020_finalGradeDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

#Plot both time periods together
treeGradeTitle = textGrob("B. Mean tree canopy % change by HOLC grade", gp = gpar(fontsize = 16), hjust = 0.75)
#treeGradeTitle = textGrob("Mean tree canopy % change by HOLC grade", gp = gpar(fontsize = 16))
grid.arrange(tree1015,tree1520,ncol = 2,top=treeGradeTitle)

##Ownership changes within grades (2010-2015)##
#plot
treeOwn1015 = ggplot(data = chiOwnVizCD,aes(x=ownTraj, y=deltaMeanTCC2010_2015, color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-3.5,3.5) +
  facet_grid(. ~title1015) +
  #scale_color_manual(values = c("#F49352", "#C0540C", "#EA9785","#AE381E","#FFFFFF"))+
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = "mean tree canopy cover % change") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
#boxplot(deltaMeanTCC2010_2015~ownTraj, data = chiOwnViz)

#Run Kruskal-Wallis and Dunn test
KWdeltaMeanTCC2010_2015_ownTraj_chi = kruskal.test(deltaMeanTCC2010_2015~ownTraj, data = chiOwnViz)
DTdeltaMeanTCC2010_2015_ownTraj_chi = dunnTest(deltaMeanTCC2010_2015~ownTraj,data=chiOwnViz,method="bh")
DTdeltaMeanTCC2010_2015_ownTrajDF_chi = data.frame("Z" = DTdeltaMeanTCC2010_2015_ownTraj_chi$res$Z, "C" = DTdeltaMeanTCC2010_2015_ownTraj_chi$res$Comparison, "p" = DTdeltaMeanTCC2010_2015_ownTraj_chi$res$P.adj) #convert to df
DTdeltaMeanTCC2010_2015_ownTrajDF_chi = DTdeltaMeanTCC2010_2015_ownTrajDF_chi[order(DTdeltaMeanTCC2010_2015_ownTrajDF_chi$C),]
DTdeltaMeanTCC2010_2015_ownTrajDF_chi$sig = cut(DTdeltaMeanTCC2010_2015_ownTrajDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

##Ownership changes within grades (2015-2020)##
#plot
treeOwn1520 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=deltaMeanTCC2015_2020, color = ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-3.5,3.5) +
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = "") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#Run Kruskal-Wallis and Dunn Test
KWdeltaMeanTCC2015_2020_ownTraj_chi = kruskal.test(deltaMeanTCC2015_2020~ownTraj, data = chiOwnViz)
DTdeltaMeanTCC2015_2020_ownTraj_chi = dunnTest(deltaMeanTCC2015_2020~ownTraj,data=chiOwnViz,method="bh")
DTdeltaMeanTCC2015_2020_ownTrajDF_chi = data.frame("Z" = DTdeltaMeanTCC2015_2020_ownTraj_chi$res$Z, "C" = DTdeltaMeanTCC2015_2020_ownTraj_chi$res$Comparison, "p" = DTdeltaMeanTCC2015_2020_ownTraj_chi$res$P.adj) #convert to df
DTdeltaMeanTCC2015_2020_ownTrajDF_chi = DTdeltaMeanTCC2015_2020_ownTrajDF_chi[order(DTdeltaMeanTCC2015_2020_ownTrajDF_chi$C),]
DTdeltaMeanTCC2015_2020_ownTrajDF_chi$sig = cut(DTdeltaMeanTCC2015_2020_ownTrajDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))
#less tree canopy loss in tracts with increased homeownership (significant only for D)

#Plot both time periods together
treeOwnTitle = textGrob("B. Tree canopy change by HOLC grade & homeownership trajectory", gp = gpar(fontsize = 16), hjust = 0.57)
#treeGradeTitle = textGrob("Mean tree canopy % change by HOLC grade", gp = gpar(fontsize = 16))
grid.arrange(treeOwn1015,treeOwn1520,ncol = 2,top=treeOwnTitle)

####--NDVI--####
##Grades (2010-2015)##
#plot
NDVI1015 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=deltaMeanNDVI2010_2015, color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-0.1122,0.1122)+
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y = "mean NDVI change")+
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#Kruskal-Wallis and Dunn Test
KWdeltaMeanNDVI2010_2015_finalGrade_chi = kruskal.test(deltaMeanNDVI2010_2015~finalGrade2, data = chiOwnViz)
DTdeltaMeanNDVI2010_2015_finalGrade_chi = dunnTest(deltaMeanNDVI2010_2015~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaMeanNDVI2010_2015_finalGradeDF_chi = data.frame("Z" = DTdeltaMeanNDVI2010_2015_finalGrade_chi$res$Z, "C" = DTdeltaMeanNDVI2010_2015_finalGrade_chi$res$Comparison, "p" = DTdeltaMeanNDVI2010_2015_finalGrade_chi$res$P.adj) #convert to df
DTdeltaMeanNDVI2010_2015_finalGradeDF_chi = DTdeltaMeanNDVI2010_2015_finalGradeDF_chi[order(DTdeltaMeanNDVI2010_2015_finalGradeDF_chi$C),]
DTdeltaMeanNDVI2010_2015_finalGradeDF_chi$sig = cut(DTdeltaMeanNDVI2010_2015_finalGradeDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))
#lower grades have more of an increase in NDVI

##Grades (2015-2020)##
#plot
NDVI1520 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=deltaMeanNDVI2015_2020, color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-0.1122,0.1122)+
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y = "")+
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

KWdeltaMeanNDVI2015_2020_finalGrade_chi = kruskal.test(deltaMeanNDVI2015_2020~finalGrade2, data = chiOwnViz)
DTdeltaMeanNDVI2015_2020_finalGrade_chi = dunnTest(deltaMeanNDVI2015_2020~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaMeanNDVI2015_2020_finalGradeDF_chi = data.frame("Z" = DTdeltaMeanNDVI2015_2020_finalGrade_chi$res$Z, "C" = DTdeltaMeanNDVI2015_2020_finalGrade_chi$res$Comparison, "p" = DTdeltaMeanNDVI2015_2020_finalGrade_chi$res$P.adj) #convert to df
DTdeltaMeanNDVI2015_2020_finalGradeDF_chi = DTdeltaMeanNDVI2015_2020_finalGradeDF_chi[order(DTdeltaMeanNDVI2015_2020_finalGradeDF_chi$C),]
DTdeltaMeanNDVI2015_2020_finalGradeDF_chi$sig = cut(DTdeltaMeanNDVI2015_2020_finalGradeDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

#Plot both time periods together
ndviGradeTitle = textGrob("A. Change in average greenness by HOLC grade", gp = gpar(fontsize = 16), hjust = 0.71)
grid.arrange(NDVI1015,NDVI1520,ncol = 2,top=ndviGradeTitle)

##Ownership changes within grades (2010-2015)##
#plot
NDVIOwn1015 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=deltaMeanNDVI2010_2015, color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-0.1122,0.1122)+
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = "mean NDVI change") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

KWdeltaMeanNDVI2010_2015_ownTraj_chi = kruskal.test(deltaMeanNDVI2010_2015~ownTraj, data = chiOwnViz)
DTdeltaMeanNDVI2010_2015_ownTraj_chi = dunnTest(deltaMeanNDVI2010_2015~ownTraj,data=chiOwnViz,method="bh")
DTdeltaMeanNDVI2010_2015_ownTrajDF_chi = data.frame("Z" = DTdeltaMeanNDVI2010_2015_ownTraj_chi$res$Z, "C" = DTdeltaMeanNDVI2010_2015_ownTraj_chi$res$Comparison, "p" = DTdeltaMeanNDVI2010_2015_ownTraj_chi$res$P.adj) #convert to df
DTdeltaMeanNDVI2010_2015_ownTrajDF_chi = DTdeltaMeanNDVI2010_2015_ownTrajDF_chi[order(DTdeltaMeanNDVI2010_2015_ownTrajDF_chi$C),]
DTdeltaMeanNDVI2010_2015_ownTrajDF_chi$sig = cut(DTdeltaMeanNDVI2010_2015_ownTrajDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))
#not significant

##Ownership changes within grades (2015-2020)##
#plot
NDVIOwn1520 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=deltaMeanNDVI2015_2020, color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-0.1122,0.1122)+
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = "") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
#boxplot(deltaMeanNDVI2015_2020~ownTraj, data = chiOwnViz) 

KWdeltaMeanNDVI2015_2020_ownTraj_chi = kruskal.test(deltaMeanNDVI2015_2020~ownTraj, data = chiOwnViz)
DTdeltaMeanNDVI2015_2020_ownTraj_chi = dunnTest(deltaMeanNDVI2015_2020~ownTraj,data=chiOwnViz,method="bh")
DTdeltaMeanNDVI2015_2020_ownTrajDF_chi = data.frame("Z" = DTdeltaMeanNDVI2015_2020_ownTraj_chi$res$Z, "C" = DTdeltaMeanNDVI2015_2020_ownTraj_chi$res$Comparison, "p" = DTdeltaMeanNDVI2015_2020_ownTraj_chi$res$P.adj) #convert to df
DTdeltaMeanNDVI2015_2020_ownTrajDF_chi = DTdeltaMeanNDVI2015_2020_ownTrajDF_chi[order(DTdeltaMeanNDVI2015_2020_ownTrajDF_chi$C),]
DTdeltaMeanNDVI2015_2020_ownTrajDF_chi$sig = cut(DTdeltaMeanNDVI2015_2020_ownTrajDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))
#less NDVI loss in tracts with increased homeownership (significant only for C)

#Plot both time periods together
ndviOwnTitle = textGrob("A. Change in average greenness by HOLC grade & homeownership trajectory", gp = gpar(fontsize = 16), hjust = 0.47)
#treeGradeTitle = textGrob("Mean tree canopy % change by HOLC grade", gp = gpar(fontsize = 16))
grid.arrange(NDVIOwn1015,NDVIOwn1520,ncol = 2,top=ndviOwnTitle)

####--LST--####
##Grades (2010-2015)##
#plot
LST1015 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=deltaMeanLST2010_2015, color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  ylim(-5.371,5.371) +
  theme_bw() +
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y =  expression("mean LST change (°C)")) +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#run Kruskal-Wallis and Dunn test
KWdeltaMeanLST2010_2015_finalGrade_chi = kruskal.test(deltaMeanLST2010_2015~finalGrade2, data = chiOwnViz)
DTdeltaMeanLST2010_2015_finalGrade_chi = dunnTest(deltaMeanLST2010_2015~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaMeanLST2010_2015_finalGradeDF_chi = data.frame("Z" = DTdeltaMeanLST2010_2015_finalGrade_chi$res$Z, "C" = DTdeltaMeanLST2010_2015_finalGrade_chi$res$Comparison, "p" = DTdeltaMeanLST2010_2015_finalGrade_chi$res$P.adj) #convert to df
DTdeltaMeanLST2010_2015_finalGradeDF_chi = DTdeltaMeanLST2010_2015_finalGradeDF_chi[order(DTdeltaMeanLST2010_2015_finalGradeDF_chi$C),]
DTdeltaMeanLST2010_2015_finalGradeDF_chi$sig = cut(DTdeltaMeanLST2010_2015_finalGradeDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

##Grades (2015-2020)##
#plot
LST1520 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=deltaMeanLST2015_2020, color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  ylim(-5.371,5.371) +
  theme_bw() +
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y =  "") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#run Kruskal-Wallis and Dunn test
KWdeltaMeanLST2015_2020_finalGrade_chi = kruskal.test(deltaMeanLST2015_2020~finalGrade2, data = chiOwnViz)
DTdeltaMeanLST2015_2020_finalGrade_chi = dunnTest(deltaMeanLST2015_2020~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaMeanLST2015_2020_finalGradeDF_chi = data.frame("Z" = DTdeltaMeanLST2015_2020_finalGrade_chi$res$Z, "C" = DTdeltaMeanLST2015_2020_finalGrade_chi$res$Comparison, "p" = DTdeltaMeanLST2015_2020_finalGrade_chi$res$P.adj) #convert to df
DTdeltaMeanLST2015_2020_finalGradeDF_chi = DTdeltaMeanLST2015_2020_finalGradeDF_chi[order(DTdeltaMeanLST2015_2020_finalGradeDF_chi$C),]
DTdeltaMeanLST2015_2020_finalGradeDF_chi$sig = cut(DTdeltaMeanLST2015_2020_finalGradeDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

#Plot both time periods together
lstGradeTitle = textGrob("D. Mean land surface temperature (LST) change by HOLC grade", gp = gpar(fontsize = 16), hjust = 0.57)
grid.arrange(LST1015,LST1520,ncol = 2,top=lstGradeTitle)

##Ownership changes within grades (2010-2015)##
LSTOwn1015 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=deltaMeanLST2010_2015, color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-5.371,5.371) +
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = expression("mean LST change (°C)")) +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

KWdeltaMeanLST2010_2015_ownTraj_chi = kruskal.test(deltaMeanLST2010_2015~ownTraj, data = chiOwnViz)
DTdeltaMeanLST2010_2015_ownTraj_chi = dunnTest(deltaMeanLST2010_2015~ownTraj,data=chiOwnViz,method="bh")
DTdeltaMeanLST2010_2015_ownTrajDF_chi = data.frame("Z" = DTdeltaMeanLST2010_2015_ownTraj_chi$res$Z, "C" = DTdeltaMeanLST2010_2015_ownTraj_chi$res$Comparison, "p" = DTdeltaMeanLST2010_2015_ownTraj_chi$res$P.adj) #convert to df
DTdeltaMeanLST2010_2015_ownTrajDF_chi = DTdeltaMeanLST2010_2015_ownTrajDF_chi[order(DTdeltaMeanLST2010_2015_ownTrajDF_chi$C),]
DTdeltaMeanLST2010_2015_ownTrajDF_chi$sig = cut(DTdeltaMeanLST2010_2015_ownTrajDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))

##Ownership changes within grades (2015-2020)##
LSTOwn1520 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=deltaMeanLST2015_2020, color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(-5.371,5.371) +
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = "") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

KWdeltaMeanLST2015_2020_ownTraj_chi = kruskal.test(deltaMeanLST2015_2020~ownTraj, data = chiOwnViz)
DTdeltaMeanLST2015_2020_ownTraj_chi = dunnTest(deltaMeanLST2015_2020~ownTraj,data=chiOwnViz,method="bh")
DTdeltaMeanLST2015_2020_ownTrajDF_chi = data.frame("Z" = DTdeltaMeanLST2015_2020_ownTraj_chi$res$Z, "C" = DTdeltaMeanLST2015_2020_ownTraj_chi$res$Comparison, "p" = DTdeltaMeanLST2015_2020_ownTraj_chi$res$P.adj) #convert to df
DTdeltaMeanLST2015_2020_ownTrajDF_chi = DTdeltaMeanLST2015_2020_ownTrajDF_chi[order(DTdeltaMeanLST2015_2020_ownTrajDF_chi$C),]
DTdeltaMeanLST2015_2020_ownTrajDF_chi$sig = cut(DTdeltaMeanLST2015_2020_ownTrajDF_chi$p, c(-Inf,0.05,Inf), c("significant","not significant"))
#less heat in tracts with increased homeownership (significant for C and D)

#Plot both time periods together
lstOwnTitle = textGrob("D. Land surface temperature change by HOLC & homeownership trajectory", gp = gpar(fontsize = 16), hjust = 0.51)
grid.arrange(LSTOwn1015,LSTOwn1520,ncol = 2,top=lstOwnTitle)

####--PARK--####
##Grades (2010-2015)##
#plot
PARK1015 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=log(area2015Buffm2), color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  ylim(0,13.2572) +
  theme_bw() +
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y =  expression("log of change in park area (m²)")) +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#run Kruskal-Wallis and Dunn test
KWdeltaAreaPARK2010_2015_finalGrade = kruskal.test(area2015Buffm2~finalGrade2, data = chiOwnViz)
DTdeltaAreaPARK2010_2015_finalGrade = dunnTest(area2015Buffm2~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaAreaPARK2010_2015_finalGradeDF = data.frame("Z" = DTdeltaAreaPARK2010_2015_finalGrade$res$Z, "C" = DTdeltaAreaPARK2010_2015_finalGrade$res$Comparison, "p" = DTdeltaAreaPARK2010_2015_finalGrade$res$P.adj) #convert to df
DTdeltaAreaPARK2010_2015_finalGradeDF = DTdeltaAreaPARK2010_2015_finalGradeDF[order(DTdeltaAreaPARK2010_2015_finalGradeDF$C),]
DTdeltaAreaPARK2010_2015_finalGradeDF$sig = cut(DTdeltaAreaPARK2010_2015_finalGradeDF$p, c(-Inf,0.05,Inf), c("significant","not significant"))

##Grades (2015-2020)##
#plot
PARK1520 = ggplot(data = chiOwnViz, aes(x=finalGrade2, y=log(area2020Buffm2), color=finalGrade2))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  ylim(0,13.2572) +
  theme_bw() +
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#517133","#3E797A","#C0540C","#AE381E"))+
  labs(x = "HOLC grade", y =  "") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#run Kruskal-Wallis and Dunn test
KWdeltaAreaPARK2015_2020_finalGrade = kruskal.test(area2020Buffm2~finalGrade2, data = chiOwnViz)
DTdeltaAreaPARK2015_2020_finalGrade = dunnTest(area2020Buffm2~finalGrade2,data=chiOwnViz,method="bh")
DTdeltaAreaPARK2015_2020_finalGradeDF = data.frame("Z" = DTdeltaAreaPARK2015_2020_finalGrade$res$Z, "C" = DTdeltaAreaPARK2015_2020_finalGrade$res$Comparison, "p" = DTdeltaAreaPARK2015_2020_finalGrade$res$P.adj) #convert to df
DTdeltaAreaPARK2015_2020_finalGradeDF = DTdeltaAreaPARK2015_2020_finalGradeDF[order(DTdeltaAreaPARK2015_2020_finalGradeDF$C),]
DTdeltaAreaPARK2015_2020_finalGradeDF$sig = cut(DTdeltaAreaPARK2015_2020_finalGradeDF$p, c(-Inf,0.05,Inf), c("significant","not significant"))

#Plot both time periods together
parkGradeTitle = textGrob("C. Change in park area by HOLC grade", gp = gpar(fontsize = 16), hjust = 0.93)
grid.arrange(PARK1015,PARK1520,ncol = 2,top=parkGradeTitle)

##Ownership changes within grades (2010-2015)##
PARKOwn1015 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=log(area2015Buffm2), color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(0,13.2572) +
  facet_grid(. ~title1015) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = expression("log of change in park area (m²)")) +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#run Kruskal-Wallis and Dunn test
KWdeltaAreaPARK2010_2015_ownTraj = kruskal.test(area2015Buffm2~ownTraj, data = chiOwnViz)
DTdeltaAreaPARK2010_2015_ownTraj = dunnTest(area2015Buffm2~ownTraj,data=chiOwnViz,method="bh")
DTdeltaAreaPARK2010_2015_ownTrajDF = data.frame("Z" = DTdeltaAreaPARK2010_2015_ownTraj$res$Z, "C" = DTdeltaAreaPARK2010_2015_ownTraj$res$Comparison, "p" = DTdeltaAreaPARK2010_2015_ownTraj$res$P.adj) #convert to df
DTdeltaAreaPARK2010_2015_ownTrajDF = DTdeltaAreaPARK2010_2015_ownTrajDF[order(DTdeltaAreaPARK2010_2015_ownTrajDF$C),]
DTdeltaAreaPARK2010_2015_ownTrajDF$sig = cut(DTdeltaAreaPARK2010_2015_ownTrajDF$p, c(-Inf,0.05,Inf), c("significant","not significant"))

##Ownership changes within grades (2015-2020)##
PARKOwn1520 = ggplot(data = chiOwnVizCD, aes(x=ownTraj, y=log(area2020Buffm2), color=ownTraj))+
  geom_hline(yintercept = 0,color = "grey")+
  geom_boxplot(size = 1) +
  theme_bw() +
  ylim(0,13.2572) +
  facet_grid(. ~title1520) +
  scale_color_manual(values = c("#fdbf6f", "#ff7f00", "#fb9a99","#e31a1c","#FFFFFF"))+
  labs(x = "HOLC grade / homeownership rate change", y = "") +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold facet titles and adjust size
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

#run Kruskal-Wallis and Dunn test
KWdeltaAreaPARK2015_2020_ownTraj = kruskal.test(area2020Buffm2~ownTraj, data = chiOwnViz)
DTdeltaAreaPARK2015_2020_ownTraj = dunnTest(area2020Buffm2~ownTraj,data=chiOwnViz,method="bh")
DTdeltaAreaPARK2015_2020_ownTrajDF = data.frame("Z" = DTdeltaAreaPARK2015_2020_ownTraj$res$Z, "C" = DTdeltaAreaPARK2015_2020_ownTraj$res$Comparison, "p" = DTdeltaAreaPARK2015_2020_ownTraj$res$P.adj) #convert to df
DTdeltaAreaPARK2015_2020_ownTrajDF = DTdeltaAreaPARK2015_2020_ownTrajDF[order(DTdeltaAreaPARK2015_2020_ownTrajDF$C),]
DTdeltaAreaPARK2015_2020_ownTrajDF$sig = cut(DTdeltaAreaPARK2015_2020_ownTrajDF$p, c(-Inf,0.05,Inf), c("significant","not significant"))

#Plot both time periods together (not significant)
parkOwnTitle = textGrob("C. Park area change by HOLC grade & homeownership trajectory", gp = gpar(fontsize = 16), hjust = 0.58)
grid.arrange(PARKOwn1015,PARKOwn1520,ncol = 2,top=parkOwnTitle)