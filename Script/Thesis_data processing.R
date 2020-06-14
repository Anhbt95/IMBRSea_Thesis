#       IMBRSea Thesis 
#       Tuan Anh Bui
#       20.02.2020

#       Belgian beam trawl Discard spatial analysis

#       This is the script for data processing
#       The raw data will be checked for important variables, NA, duplication
#       Processing of important variables will be conducted and added to the final data set
#       These variables include: 
#       Discard ratio (DR), Discard ratio by length (DR_lenght)
#       Price (price_eurobykg), Length median (L_median), Length weighted mean (L_wt_mean)

########################################

# Load support files and packages -----------------------------------------
# source("F:/backup_to_H/5_Competenties/Statistiek/R/0_SPATIAL/2019_Zuur_INLA/course_week/Monday/HighstatLibV12.R") # Jochen
source("HighstatLibV12.R")

# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

library(data.table)
#library(COSTeda)
library(lattice); #library(nnet); library(tcltk); library(tcltk2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(INLA)
library(sp)
library(raster)
library(tidyverse)
library(lubridate)
#library(RColorBrewer)

#library(dismo)
#library(splancs)
#library(reshape)
#library(gstat)


########################################

# Load data ------------------------------------------------


# Indicate local directory
dir_data = "F:/backup_to_H/R_gitlab/Thesis_Discard-spatial-analysis/Data" # Jochen
dir_data_raw <- paste(dir_data,"Data_raw", sep="/") # Jochen
#dir_data = "C:/Users/tbui/Thesis_R/Data" # Change directory here if the local dir for data is different

# Dir in Tuan-Anh laptop
dir_data <-  "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data"
dir_data_raw <- "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data/Data_raw"

# obs data -----------------------------------------------------------

# obs is data without length

obs <- readRDS(file=paste0(dir_data_raw,"/","obs_bio_2006to2019_TBB_DEF_70-99.rds"))
str(obs) #244270 obs - 82 vars

# obs_len data -------------------------------------------------------------

# obs_len is data with length and number of individuals at each length value

obs_len <- readRDS(file=paste0(dir_data_raw,"/","obs_length_bio_2006to2019_TBB_DEF_70-99.rds"))
str(obs_len) #1349086 obs - 86 vars


# sale data ----------------------------------------------------------

# sale data are data with fish price 

# List of data - pattern: begin with (^) salesraw
# f <- list.files(path = paste0("F:/backup_to_H/00_data/BELGISCH_VISSERIJ/fish_price/Rdata","/"), pattern="^salesraw",full.names=T) # Jochen
f <- list.files(path = paste0(dir_data_raw,"/"), pattern="^salesraw",full.names=T)
length(f)


# Data set description --------------------------------------------------------

# observer (obs) data - observoer data with landing and discard weight
# obs_len data  - observer data with landing and discard weight + length 
# sale data - data with sale value 

# Target (final) data
# 1. observation at species level (Trip > HaulID > Species)
# 2. variables of: Discard ratio, Discard ratio by length, Length_median, Price 

# Target data will be created from the combination of either obs or obs_len and sale data 
# obs_len and obs should be compared to select the one as input for final data set

# Name of target data: obs 


########################################

# Exploring obs data (raw) -------------------------------------------
o
# This step is to know the variables of obs data
# Other data should have similar structure, though the var name could be different

str(obs) #244270 obs - 82 variables
names(obs) # Variables

unique(obs$TripID)
unique(obs$TripCode)

unique(obs$Year) # 2006-2019

unique(obs$Vessel) # 36 Vessels
unique(obs$VesselCode)


class(obs$Power) # Vessel power - character - need to convert to numeric
obs$Power <- as.numeric(obs$Power)
sort(unique(obs$Power)) #184 - 1570
sort(unique(filter(obs,Power <= 221)$Power)) # <= 221 FleetSegment - 3 vessels 
sort(unique(filter(obs,Power > 221)$Power)) # >221 FleetSegment - 27


unique(obs$LengthCategory) # 4 cat
table(obs$FleetSegment) 

unique(obs$SortingType) #"Enkele sorteerband" - single  "Dubbele sorteerband" - double
table(obs$SortingType) #Mainly double sort (230k/240k obs)

unique(obs$TripStatus)
table(obs$TripStatus)


sort(unique(obs$CountHauls)) 
sort(unique(obs$CountHaulsIsSampled))
sort(unique(obs$Number_haul))

unique(obs$HaulRaisingFactor)
unique(obs$HaulTime)



table(obs$IsSampled) # 242499 obs True


unique(obs$TIcesDivisions)
unique(obs$IcesDivision)
unique(obs$IcesStatisticalRectangle)


unique(obs$Metier)
unique(obs$Metier_tripgear)
table(obs$Metier_tripgear) #1k >= 120

unique(obs$MeshType)
table(obs$MeshType) #Ruit 240k, T90 1k

sort(unique(obs$MeshSize)) # 70-120 (consider the 120 meshsize?)
table(obs$MeshSize) #mainly 80 (180k), 85 (40k), 70 (12k)

unique(obs2$meshSize)


unique(obs$SelectiveDevice)
table(obs$SelectiveDevice) #215k Exit window or panel
obs %>% 
  group_by(SelectiveDevice) %>%
  summarize(n = n()) #29k obs NA 

SelectiveDevice <- obs %>% 
  group_by(SelectiveDevice, Vessel, Year) %>%
  summarize(n = n()) %>%
  arrange(SelectiveDevice, Year) #2006, 2007, 2011-2013

unique(obs$SelectiveDeviceMeshType) #T90, Ruit
unique(obs$SelectiveDeviceMeshSize)
table(obs$SelectiveDeviceMeshSize) #180-450
obs %>%
  group_by(SelectiveDeviceMeshSize, FleetSegment) %>%
  summarize(n = n()) #no separation of FleetSegment and SelectiveDeviceMeshSize

unique(obs$TBBBeamLength)
obs %>%
  group_by(TBBBeamLength, FleetSegment) %>%
  summarize(n = n())
unique(obs$TBBGearSubType)
unique(obs$TBBStimulation)


unique(obs$VesselSide.x)
unique(obs$VesselSide.y)


unique(obs$FaoCode)
sort(unique(obs$NameScientific))
unique(obs$NameEnglish)
obs %>% 
  group_by(FaoCode, NameScientific, NameEnglish, NameDutch) %>%
  summarize(n = n())



unique(obs$FateCategory) 
unique(obs$FateCategoryCode) #R = D - Need to spread
obs %>% 
  group_by(FateCategory, FateCategoryCode) %>%
  summarize(n = n()) 

unique(obs$FishPresentation)
unique(obs$FishPresentationCode)


unique(obs$LandingCategory)
unique(obs$LandingCategoryCode)
obs %>% 
  group_by(FateCategoryCode, LandingCategoryCode) %>%
  summarize(n = n()) #Only 4 D as BMS


unique(obs$CatchFraction)
unique(obs$CatchFractionCode)
unique(obs$CatchPart)
unique(obs$CatchPartCode)
unique(obs$GearPart)
unique(obs$GearPartCode) 
#All NA - non important for analysis

unique(obs$FishSizeCategory) #Eu scale
unique(obs$FishSizeCategoryCode) #NA - 1-9



########################################

# Data processing ---------------------------------------------------------

########################################

# Observer data processing  -----------------------------------------------

########################################

# Subset data -------------------------------------------------------------

str(obs)

# Subset obs data with fewer variables
# TripID, TripCode, Year – Trip info
# VesselCode, Vessel, Power, FleetSegment, – Vessel info
# CountHauls, CountHaulIsSampled, Number_haul, HaulTime, Duration – Haul info
# HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision – Spatial info
# IsSampled – Sample status
# Metier – Metier info (MeshSize and MeshType can cause duplication)
# SelectiveDevice, SelectiveDeviceMeshSize – Selective device info
# FaoCode, NameScientific, NameDutch, NameEngish – Species info
# FateCategoryCode, LandingCategoryCode - catchCat
# WeightTotalBothSides – Size and weight 

obs <- obs %>%
  select(TripID, Year, TripCode, VesselCode, Vessel, Power, FleetSegment, SortingType, TripStatus, CountHauls, CountHaulsIsSampled, Number_haul, HaulTime, Duration, HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision, IsSampled, Metier, SelectiveDevice, SelectiveDeviceMeshSize, FaoCode, NameScientific, NameDutch, NameEnglish, FateCategoryCode, LandingCategoryCode, WeightTotalBothSides)

# Save data before processed
obs_raw <- obs

# Check and change the class of variables (character to numeric/date time)
str(obs)

#change Weight from character to numeric
obs$WeightTotalBothSides <- as.numeric(obs$WeightTotalBothSides)

# HaulLatitude and HaulLongitude are character
# should be converted to numeric
obs$HaulLatitude <- as.numeric(obs$HaulLatitude)
obs$HaulLongitude <- as.numeric(obs$HaulLongitude)


# Target data -------------------------------------------------------------

# Target data has observation at species level (HaulID > Species)
# Number of observation of target data could be checked as follow 
obs %>% group_by(HaulID, NameScientific) %>% summarize(n = n()) #155,757

# Target data should have 155,757 observations (or less - due to NA and duplications)

# Things need to be done for data processing
# 1. Eliminate NA and duplicate values
# 2. Extract Discard and Landing weight
# 3. Add quarter variable
# 4. Calculate CPUE and DPUE (kg/hour)
# 5. Calculate "discard ratio" (DR)
# 6. Calculate sample point (haul)

# 1. Eliminate NA and duplicate values  ------------------------------------------------

# Eliminate IsSampled False value - those with NA values
obs <- obs %>% filter(IsSampled == T)

# Select variables that might contain NA and check
str(ungroup(obs)) # Check structure 

# Check FateCategoryCode
any(is.na((obs$FateCategoryCode))) # TRUE
View(obs %>% filter(is.na(FateCategoryCode) == T)) # Have a look at NA value
obs <- obs %>% filter(is.na(FateCategoryCode) == F) # Remove NA value

# Check WeigthTotalBothSides
any(is.na((obs$WeightTotalBothSides))) # TRUE
View(obs %>% filter(is.na(WeightTotalBothSides) == T)) # Have a look at NA value
obs <- obs %>% filter(is.na(WeightTotalBothSides) == F) # Remove NA value


# Check LandingCategoryCode
# 4 D as BMS in LandingCategoryCode  
obs %>% 
  group_by(FateCategoryCode, LandingCategoryCode) %>%
  summarize(n = n()) #No more BMS

# Remove duplicates
obs <- obs %>% distinct()

# Keep an cleaned version to trace back in case of mistakes during the processing
obs_cln <- obs

# 2. Extract Discard and Landing weight  -------------------------------------------------

# Extract Discard (D) and Landing (L) weight

# The D and L are in the variable FateCategoryCode 
# D and L weight could be extracted using tidyr::spread from WeightTotalBothSides

unique(obs$FateCategoryCode) #catch category: D L R

# Check R 
R <- obs %>% 
  filter(FateCategoryCode == "R") %>%
  group_by(FateCategoryCode, Year, NameScientific, NameEnglish) %>%
  summarize(WeightTotalBothSides = sum(WeightTotalBothSides))
R

# R is from 2016 until 2019 for common sole only (Solea solea)

# Transform R to D (as R is equivalent to D)
obs[which(obs$FateCategoryCode == "R"),"FateCategoryCode"] <- "D"
table(obs$FateCategoryCode)


# Extract D and L weight (WeightTotalBothSides) from FateCategoryCode
obs <- tidyr::spread(obs, FateCategoryCode, WeightTotalBothSides, fill=0)

# 29568 Keys are shared for many observations 

# Example: row 3 and 11
View(obs[c(3,11),])

# those observations should be merged by WeightTotalBothSides
# before spread can be done 


# group_by is the same as previous except LandingCategoryCode and WeightTotalBothSides
# as we dont need LandingCategoryCode anymore as we solved the issue before
# also keeping might cause duplication of observation
# WeightTotalBothSides is used for weight
obs <- obs %>% 
  group_by(TripID, Year, TripCode, VesselCode, Vessel, Power, FleetSegment, SortingType, TripStatus, CountHauls, CountHaulsIsSampled, Number_haul, HaulTime, Duration, HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision, IsSampled, Metier, SelectiveDevice, SelectiveDeviceMeshSize, FaoCode, NameScientific, NameDutch, NameEnglish, FateCategoryCode) %>%
  summarize(WeightTotalBothSides = sum(WeightTotalBothSides))

# Repeat the spreading
obs <- tidyr::spread(obs, FateCategoryCode, WeightTotalBothSides, fill=0)
nrow(obs) # 154,253 observations

# Check observation with target data
obs %>% group_by(HaulID, NameScientific) %>% summarize(n = n()) #154,253

# obs is qualified

# 3. Add Quarter variable -------------------------------------------------

obs$Quarter = NA
obs[which(substr(obs$HaulTime,6,7) %in% c("01","02","03")),"Quarter"] = 1
obs[which(substr(obs$HaulTime,6,7) %in% c("04","05","06")),"Quarter"] = 2
obs[which(substr(obs$HaulTime,6,7) %in% c("07","08","09")),"Quarter"] = 3
obs[which(substr(obs$HaulTime,6,7) %in% c("10","11","12")),"Quarter"] = 4

# Transform quarter to factor
obs$Quarter <- factor(obs$Quarter, ordered = T)
class(obs$Quarter)

# 4. Calculate DPUE (kg/hour) ---------------------------------------

# DPUE can be calculated by dividing 
# LAN+DIS (CPUE) and DIS (DPUE) for fishing duration 

sort(unique(obs$Duration)) #Duration is in minute and need to be transformed to hour

# Transform effort unit foDur to hour
obs <- obs %>% mutate(Dur_hour = Duration/60)

# Calculate DPUE
obs <- obs %>% mutate(DPUE = D/Dur_hour)
summary(obs$DPUE)


# 5. Calculate "discard ratio" (DR) ------------------------------------------

# Data Discard ratio at species level
# Each sample point is identify at 1 haul location (lat lon) and time (Haultime)
# Each Haul will have several Species observations

# Structure of data: Vessel > Trip > Haul (CountHaulsIsSampled) > Species  

# Calculate "discard ratio"
obs <- obs %>% mutate(DR = D/(D+L))
hist(obs$DR)

# Check if any value is NA
any(is.na(obs$DR) == T) # TRUE

# Create a subset of NA value to check
na <- obs %>% filter(is.na(DR) == T)
nrow(na) #1
obs_na <- obs
nrow(obs_na) #154253 (old)

# DR is NA when Discard (D) and Landing (L) of obs are 0
# Eleminate those values 
obs <- obs %>%
  filter(D != 0 | L != 0) 
nrow(obs) == nrow(obs_na) - nrow(na) #TRUE - All NA values eliminated

# Remove obs_na and na
rm(obs_na,na)


# 6. Calculate sample point (haul) ----------------------------------------

# Each sample point (haul) is identified at certain location (s) and time (t)
# Time (Temporal) level: Year, Quarter (HaulTime) 
# Location (Spatial) level: IcesDivision (lat, lon)

# Vessel level: Vessel, TripCode, CountHaulsIsSampled 

obs_sample <- obs %>% 
  group_by(Year, #14
           Quarter, #56
           IcesDivision, #287
           HaulTime, #9986
           HaulLatitude, #10000
           HaulLongitude, #10000
           HaulID, #100001
           TripCode,
           Vessel, 
           VesselCode,
           CountHaulsIsSampled,
  ) %>%
  summarize(obs = n(),
            D_mean = mean(D),
            L_mean = mean(L),
            D_sum = sum(D),
            L_sum = sum(L),
            DPUE = mean(DPUE),
            DR = mean(DR)) #Number of observation each sample/haul

# There are total 10003 sample/haul points

# 10003 sample/haul points
# Multiple observations (species) from each sample
# Time (Temporal) level: Year, Quarter (HaulTime) 
# Location (Spatial) level: IcesDivision (lat, lon)


# Different vessel cannot haul/sample same location + time
# -> no. of sample/ haul with or without Vessel (Code) is the same 10003


# 7. Save data ------------------------------------------------------------

# Final obs data
obs #152,788 observations 
obs <- ungroup(obs) #Ungroup obs for further processing

# dir_data="F:/backup_to_H/R_gitlab/spatial_discards_TuanAnh/followup_jochen" # Jochen
save(obs,file=paste0(dir_data,"/","obs_processed_20062019_TBB_DEF_70-99.RData")) #Rdata
saveRDS(obs,file=paste0(dir_data,"/","obs_processed_20062019_TBB_DEF_70-99.rds")) #rds 


########################################

# obs_len data processing -------------------------------------------------

# The protocol is similar to that of obs

########################################

# Subset data -------------------------------------------------------------

str(obs_len)


# Subset obs data with fewer variables
# TripID, TripCode, Year – Trip info
# VesselCode, Vessel, Power, FleetSegment, – Vessel info
# CountHauls, CountHaulIsSampled, Number_haul, HaulTime, Duration – Haul info
# HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision – Spatial info
# IsSampled – Sample status
# Metier – Metier info (MeshSize and MeshType can cause duplication)
# SelectiveDevice, SelectiveDeviceMeshSize – Selective device info
# FaoCode, NameScientific, NameDutch, NameEngish – Species info
# FateCategoryCode, LandingCategoryCode - catchCat
# WeightTotalBothSides – weight 
# Length, Number_atlength

obs_len <- obs_len %>%
  select(TripID, Year, TripCode, VesselCode, Vessel, Power, FleetSegment, SortingType, TripStatus, CountHauls, CountHaulsIsSampled, Number_haul, HaulTime, Duration, HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision, IsSampled, Metier, SelectiveDevice, SelectiveDeviceMeshSize, FaoCode, NameScientific, NameDutch, NameEnglish, FateCategoryCode, LandingCategoryCode, WeightTotalBothSides, Length, Number_atlength)

# Save data before processing
obs_len_raw <- obs_len

# Check and change the class of variables (character to numeric/date time)
str(obs_len)
#obs_len$Year <- factor(obs_len$Year, ordered = T) #Change Year to factor (ordered)
obs_len$WeightTotalBothSides <- as.numeric(obs_len$WeightTotalBothSides)
#change Weight from character to numeric

# HaulLatitude and HaulLongitude are character
# should be converted to numeric
obs_len$HaulLatitude <- as.numeric(obs_len$HaulLatitude)
obs_len$HaulLongitude <- as.numeric(obs_len$HaulLongitude)


# Target data -------------------------------------------------------------

obs_len %>% group_by(HaulID, NameScientific) %>% summarize(n = n()) #73,610
# Target data should have 73,610 observations (or less - due to NA or duplicates)

# Things need to be done for data processing
# 1. Eliminate NA values


# obs_len has both weigth and length variables
# 3 main works and corresponding data
# DR (Discard ratio) - obs_wt
# DR_length (Discard ratio by length) - obs_len
# L_median (Length median) - obs_len_median

# # DR (Discard ratio) - obs_wt
# 2. Extract Discard and Landing weight
# 3. Calculate "discard ratio" (DR)

# DR_length (Discard ratio by length) - obs_len
# 4. Calculate Discard ratio by length DR_length

# L_median (Length median) - obs_len2
# 5. Calculate Length median (L_median) and Length weighted mean (L_wt_mean)

# 6. Merge obs_wt, obs_len, obs_len2

# 7. Calculate DPUE (kg/hour)
# 8. Add quarter variable
# 9. Calculate sample point (haul)


# 1. Eliminate NA and duplicate values  ------------------------------------------------
# Eliminate IsSampled False value
obs_len <- obs_len %>% filter(IsSampled == T)


# Check FateCategoryCode
any(is.na((obs_len$FateCategoryCode))) # FALSE - no NA values

# Check WeigthTotalBothSides
any(is.na((obs_len$WeightTotalBothSides))) # TRUE
View(obs_len %>% filter(is.na(WeightTotalBothSides) == T)) # Have a look at NA value
obs_len <- obs_len %>% filter(is.na(WeightTotalBothSides) == F) # Remove NA value

# Check Length
any(is.na((obs_len$Length))) # TRUE
View(obs_len %>% filter(is.na(Length) == T)) # Have a look at NA value
# NA values are of Raja undulata in 2019
# Check records of Raja undulata - only in 2019 this species does not have length values
# Since Length is an important predictor, NA values will be eliminated
obs_len <- obs_len %>% filter(is.na(Length) == F) # Remove NA value

# Check LandingCategoryCode
obs_len %>% 
  group_by(FateCategoryCode, LandingCategoryCode) %>%
  summarize(n = n()) #No BMS - no problem

# Remove LandingCategoryCode
obs_len <- obs_len %>% select(-LandingCategoryCode)

# Remove duplicates
obs_len <- obs_len %>% distinct()

# Keep an cleaned version to trace back in case of mistakes during the processing
obs_len_cln <- obs_len

########################################

# 3 subset for 3 works ----------------------------------------------------

# 3 work
# DR - obs_wt
# DR_length - obs_len
# Length median and Length wt mean - obs_len2

obs_wt <- obs_len #create obs_wt
obs_len2 <- obs_len #obs_len2 for Length median and Length wt mean

########################################

# Data with DR (from weight) - obs_wt -------------------------------------

# 2. Extract Discard and Landing weight  -------------------------------------------------

# Extract Discard (D) and Landing (L) weight

# The D and L are in the variable FateCategoryCode 
# D and L weight could be extracted using tidyr::spread from WeightTotalBothSides

unique(obs_wt$FateCategoryCode) #catch category: D L R

# Check R 
# WeightTotalBothSides is at Species level, not Length level (Species > Lenght)
# First have to group the data to Species level then summarize to avoid duplication of WeightTotalBothSides
R_len <- obs_len %>% 
  filter(FateCategoryCode == "R") %>%
  group_by(FateCategoryCode, Year, HaulID, NameScientific, NameEnglish, WeightTotalBothSides) %>%
  summarize(n = n())

R_len <- R_len %>% 
       group_by(FateCategoryCode, Year, NameScientific, NameEnglish) %>%
       summarize(WeightTotalBothSides = sum(WeightTotalBothSides))
# R is from 2016 until 2019 for common sole only (Solea solea) 
# However R_len and R seems differnt

# Compare R and R_len - at HaulID level from cleaned data

R_len_raw <- obs_len_cln %>% 
  filter(FateCategoryCode == "R") %>%
  group_by(FateCategoryCode, Year, HaulID, NameScientific, NameEnglish, WeightTotalBothSides) %>%
  summarize(n = n())

R_raw <- obs_cln %>% 
  filter(FateCategoryCode == "R") %>%
  group_by(FateCategoryCode, Year, HaulID, NameScientific, NameEnglish, WeightTotalBothSides) %>%
  summarize(n = n())

# Check the difference
R_dif <- anti_join(R_raw, R_len_raw, by = "HaulID")

# Check 1 HaulID in obs_len
View(obs_len_cln %>% filter(HaulID == "7C59BD84-CCA0-4B1F-811B-BC418DD2CFB0"))
# Record of Common sole is lack in obs_len 


# Transform R to D (as R is equivalent to D)
obs_wt[which(obs_wt$FateCategoryCode == "R"),"FateCategoryCode"] <- "D"
table(obs_wt$FateCategoryCode)

# Extract D and L weight (WeightTotalBothSides) from FateCategoryCode
obs_wt <- tidyr::spread(obs_len, FateCategoryCode, WeightTotalBothSides, fill=0)

# 14,258 Keys are shared for many observations 
# those observations should be merged by WeightTotalBothSides
# Before spread can be done 

# group_by is the same as previous except LandingCategoryCode and WeightTotalBothSides
# as we dont need LandingCategoryCode anymore as we solved the issue before
# also keeping might cause duplication of observation
# WeightTotalBothSides is used for weight

# WeightTotalBothSides is at Species level, not Length level (Species > Lenght)
# First have to group the data to Species level then summarize to avoid duplication
obs_wt <- obs_wt %>% 
  group_by(TripID, Year, TripCode, VesselCode, Vessel, Power, FleetSegment, SortingType, TripStatus, CountHauls, CountHaulsIsSampled, Number_haul, HaulTime, Duration, HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision, IsSampled, Metier, SelectiveDevice, SelectiveDeviceMeshSize, FaoCode, NameScientific, NameDutch, NameEnglish, FateCategoryCode, WeightTotalBothSides) %>%
  summarize(n = n())

obs_wt <- obs_wt %>% 
  group_by(TripID, Year, TripCode, VesselCode, Vessel, Power, FleetSegment, SortingType, TripStatus, CountHauls, CountHaulsIsSampled, Number_haul, HaulTime, Duration, HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision, IsSampled, Metier, SelectiveDevice, SelectiveDeviceMeshSize, FaoCode, NameScientific, NameDutch, NameEnglish, FateCategoryCode) %>%
  summarize(WeightTotalBothSides = sum(WeightTotalBothSides))

# Repeat the spreading
obs_wt <- tidyr::spread(obs_wt, FateCategoryCode, WeightTotalBothSides, fill=0)
nrow(obs_wt) # 73546 observations

# Check observations vs target data
obs_wt %>% group_by(HaulID, NameScientific) %>% summarize(n = n()) #73546 observations

# obs_wt is qualified


# obs_wt vs obs -----------------------------------------------------------

nrow(obs) #154252
nrow(obs_wt) #73546

# obs_wt has less observations than obs

unique(obs$NameScientific) #70 species
unique(obs_wt$NameScientific) #37 species
# It could be due to less species

# Check the D and L values of an example Plaice
View(obs_wt %>% filter(NameScientific == "Pleuronectes platessa") %>%
  group_by(Year, NameEnglish) %>%
  summarize(D = sum(D),
            L = sum(L)))

View(obs %>% filter(NameScientific == "Pleuronectes platessa") %>%
       group_by(Year, NameEnglish) %>%
       summarize(D = sum(D),
                 L = sum(L)))


# We see a slight difference between D and L
# Number of species should not be the only factor that makes the difference of observations

spp_obs_wt <- unique(obs_wt$NameScientific) # Indicate the spp of obs_wt
obs_spp_wt <- obs %>% filter(NameScientific %in% spp_obs_wt) # Select subset of obs regarding to obs_wt

# Check D and L
sum(obs$D) # 1.7 mil
sum(obs_spp_wt$D) # 1.6 mil
sum(obs_wt$D) # 0.8 mil (less than nearly half) 

sum(obs$L) # 2.3 mil
sum(obs_spp_wt$L) # 1.9 mil
sum(obs_wt$L) # 1.4 mil (less than nearly half)

# besides the difference in number of species, theres huge difference of Discard and Landing
# this might due to HaulID and records

# Difference of HaulID
a <- anti_join(obs_spp_wt, obs_wt, by = "HaulID")
a %>% group_by(HaulID) %>% summarize(n = n())
unique(a$Year)
# 56 HaulID that exists in obs but not in obs_wt (obs_len)
View(anti_join(obs_wt, obs_spp_wt, by = "HaulID")) 
# No HaulID that exists in obs_wt (obs_len) but not in obs

sum(a$D) # 7000 kg, does not equal to the difference of 0.8 mil

b <- anti_join(obs_spp_wt, obs_wt, by = key) # 54400 observations difference 
b %>% group_by(HaulID) %>% summarize(n = n()) # in 9796 HaulID
unique(b$Year) # Differences are through out the data 2006-2019 

sum(b$D) + sum(obs_wt$D)
sum(obs_spp_wt$D)

sum(b$L) + sum(obs_wt$L)
sum(obs_spp_wt$L)

# sum D and L of 2 groups are not exactly the same but are very close
# There is huge difference in term of Weight (D,L) between obs and obs_len (even though the species are shared)

# Example in 2019
View(obs_len %>% filter(HaulID == "FE09E002-463D-4E5B-8774-D9B521F83213"))
View(obs_spp_wt %>% filter(HaulID == "FE09E002-463D-4E5B-8774-D9B521F83213"))


# Since Length variable is needed for modelling 
# the obs_wt (from obs_len) will be used as the data for D, L and DR 

# 3. Calculate "discard ratio" (DR) ------------------------------------------

# Data Discard ratio at species level
# Each sample point is identify at 1 haul location (lat lon) and time (Haultime)
# Each Haul will have several Species observations

# Structure of data: Vessel > Trip > Haul (CountHaulsIsSampled) > Species  

# Calculate "discard ratio"
obs_wt <- obs_wt %>% mutate(DR = D/(D+L))
hist(obs_wt$DR)


# Ungroup for further processing
obs_wt <- ungroup(obs_wt)


########################################

# Data with DR_length (from length) - obs_len --------------------------------------------------------
# Preprocessing -----------------------------------------------------------

# The D and L are in the variable FateCategoryCode 
# D and L weight could be extracted using tidyr::spread from WeightTotalBothSides

unique(obs_len$FateCategoryCode) #catch category: D L R

# Transform R to D
obs_len[which(obs_len$FateCategoryCode == "R"),"FateCategoryCode"] <- "D"
table(obs_len$FateCategoryCode)


# Data of Minimum landing size
# Legal MLS are under the - Aanvoer en besomming - De Belgische Zeevisserij 
# Report on landings and revenues - The Belgian sea fisheries (Department of Agriculture and Fisheries)
# Reference: https://lv.vlaanderen.be/nl/visserij/cijfers-marktoverzichten/publicaties-zeevisserij

# Species whose MLS are not available by Belgian laws will be subjected to MLS in EC 1998
# COUNCIL REGULATION (EC) No 850/98 (1998) 
# Legal MLS is applied for Regions 1 to 5, except Skagerrak/Kattegat (COUNCIL REGULATION (EC) No 850/98 for detail)


# Species without legal MLS are subject to de-facto MLS (dicated by market demand) 
# Reference: Heath and Cook, 2015 - doi:10.1371/journal.pone.0117078


# First extract the species available in obs_len to have the persistent NameEnglish in MLS_list
spp_list <- tibble(
  NameScientific = unique(obs_len$NameScientific),
  NameEnglish = unique(obs_len$NameEnglish)
) %>% arrange(NameScientific)

# MLS_list (MLS is in mm)
MLS_list <- tribble(
  ~ NameScientific, ~ NameEnglish, ~ MLS, ~ MLS_cat, #Category is defined as legal or de-facto
  "Dicentrarchus labrax", "European seabass", 360, "legal",
  "Gadus morhua", "Atlantic cod", 350, "legal",
  "Melanogrammus aeglefinus", "Haddock", 300, "legal",
  "Merluccius merluccius", "European hake", 270, "legal",
  "Lepidorhombus whiffiagonis", "Megrim", 200, "legal",
  "Pleuronectes platessa", "European plaice", 270, "legal",
  "Pollachius pollachius", "Pollack", 300, "legal",
  "Solea solea", "Common sole", 250, "legal",
  "Merlangius merlangus", "Whiting", 270, "legal",
  "Molva molva", "Ling", 630, "legal",
  "Pecten maximus",	"King scallop", 100, "legal", #Whole area except ICES VIIa north of 52o30' and VIId
  "Limanda limanda", "Common dab", 230, "legal",
  "Microstomus kitt",	"Lemon sole", 250, "legal",
  "Scophthalmus rhombus",	"Brill", 300, "de-facto",
  "Platichthys flesus",	"European flounder", 270, "de-facto",
  "Chelidonichthys cuculus",	"Red gurnard", 300, "de-facto",
  "Chelidonichthys lucerna", 	"Tub gurnard", 300, "de-facto",
  "Eutrigla gurnardus",	"Grey gurnard", 300, "de-facto",
  "Triglidae",	"Gurnards, searobins nei", 300, "de-facto",
  "Scophthalmus maximus",	"Turbot", 300, "de-facto",
  "Scyliorhinus canicula",	"Lesser-spotted dogfish", 500, "de-facto",
  "Amblyraja radiata",	"Starry ray", 400, "de-facto",
  "Leucoraja circularis",	"Sandy ray", 400, "de-facto",
  "Leucoraja fullonica",	"Shagreen ray", 400, "de-facto",
  "Leucoraja naevus",	"Cuckoo ray", 400, "de-facto",
  "Raja brachyura",	"Blonde ray", 400, "de-facto",
  "Raja clavata",	"Thornback ray", 400, "de-facto",
  "Raja microocellata",	"Small-eyed ray", 400, "de-facto",
  "Raja montagui", "Spotted ray", 400, "de-facto",
  "Raja undulata",	"Undulate ray", 400, "de-facto", 
  "Dipturus batis",	"Blue skate", 400, "de-facto",
  # Other marketable species - anecdotal MLS
  "Conger conger", "European conger", 300, "de-facto", 
  "Lophius budegassa",  "Blackbellied angler", 300, "de-facto",  
  "Lophius piscatorius", "Monkfish (Anglerfish)", 300, "de-facto",
  "Trisopterus luscus", "Pouting", 300, "de-facto",
  "Mullus surmuletus",  "Surmullet", 160, "de-facto" #Surmullet can be considered as Mullets? 
  )


# Add MLS to obs_len
key <- c("NameScientific", "NameEnglish")
obs_len <- left_join(obs_len, MLS_list, by = key)

# Create Length_code variable
# DL - coded for Length < MLS (discard)
# LL - coded for Length > MLS (landing)
# NA - coded for species without MLS

obs_len$Length_code <- NA

# Indicated values of Length_code based on Length and MLS
obs_len$Length_code <- if_else(is.na(obs_len$MLS) == T, # Check if MLS is NA
                               "NA", # Yes - indicate Lengh_code as NA
                               if_else(obs_len$Length < obs_len$MLS, # Then compared with MLS
                                       "DL", # Smaller - DL Discard length
                                       "LL") # Equal or larger - LL Landing lenght
)


######################
# Save obs_len_freq for length frequency figures
obs_len_freq <- obs_len
save(obs_len_freq,file=paste0(dir_data,"/","obs_len_freq_20062019_TBB_DEF_70-99.RData")) #Rdata
saveRDS(obs_len_freq,file=paste0(dir_data,"/","obs_len_freq_20062019_TBB_DEF_70-99.rds")) #rds 

# Example of Length frequency figures
Sole <- obs_len_freq %>% filter(NameScientific == "Solea solea", Year == 2019) %>% 
  group_by(NameScientific, FateCategoryCode, Length_code, Length) %>% summarize(Number_atlength = sum(Number_atlength, na.rm =T))

ggplot(data = Sole, mapping = aes(x = Length, y = Number_atlength, color = FateCategoryCode)) + 
  geom_line()
######################


# Spread Length_code by Number_atlength 
# We will have 3 variables (DL, LL, NA)
obs_len <- obs_len %>% spread(Length_code, Number_atlength, fill = 0) #1345044


# 4. Discard ratio by length DR_length -------------------------------------------------


# Structure of data: trip > haul > species > length 
# Each length has a record of Number_atlength - which encoded to DL, LL, NA
# We need to group the data to species level
# then calculate the discard ratio by length DR_length
# DR_length = DL/(DL + LL) (DL and LL should be summed from multiple lengths)

obs_len <- obs_len %>% 
  group_by(TripID, Year, TripCode, VesselCode, Vessel, Power, FleetSegment, SortingType, TripStatus, CountHauls, CountHaulsIsSampled, Number_haul, HaulTime, Duration, HaulLatitude, HaulLongitude, HaulID, IcesStatisticalRectangle, IcesDivision, IsSampled, Metier, SelectiveDevice, SelectiveDeviceMeshSize, FaoCode, NameScientific, NameDutch, NameEnglish, #as previous except variables spreaded
           MLS) %>%
  summarize(LL = sum(LL),
            DL = sum(DL),
            DR_length = DL/(DL + LL))

# Check observations vs target data
nrow(obs_len) #73546 - obs_len is qualified 
obs_len <- ungroup(obs_len) #ungroup for further processing


# 5. Calculate Length median (L_median) and Length weighted mean (L_wt_mean) ----------------------------------------------------

# Length median and Length weighted mean may give "intuitive" insight into the length of observation 
# which is the real length value, rather than the ratio DR_length

# L_median
obs_len_median <- obs_len2 %>% uncount(Number_atlength)
obs_len_median <- obs_len2 %>% group_by(HaulID, NameScientific, NameEnglish) %>%
  summarize(L_median = median(Length))

# Check observations vs target data
nrow(obs_len_median) #73546 - obs_len is qualified 

# L_wt_mean (Mean)
obs_len_wt_mean <-  obs_len2 %>% group_by(HaulID, NameScientific, NameEnglish) %>% 
  summarize(L_wt_mean = weighted.mean(Length, Number_atlength))
# Check observations vs target data
nrow(obs_len_wt_mean) #73546 - obs_len is qualified 

# Merge obs_len_median and obs_len_wt_mean
# Identify keys for merging
key <- c("HaulID", "NameScientific", "NameEnglish")
obs_len2 <- left_join(obs_len_median, obs_len_wt_mean, by = key)


# 6. Merge obs_wt, obs_len, obs_len2 ------------------------------------------------

# obs_wt and obs_len_median will be merged to obs_len 

# Key by "HaulID", "NameScientific", "NameEnglish"
key <- c("HaulID", "NameScientific", "NameEnglish")

# Important variables
# obs_len DL,LL, DR_length
# Variables to be added
# obs_wt - D,L,DR
# obs_len_2 - L_medidan, L_wt_mean


# Subset obs_wt for HaulID, NameScientific, NameEnglish and the desire variables
obs_wt_sub <- obs_wt %>% select(HaulID, NameScientific, NameEnglish, D, L, DR)
obs_len2

# Merge with obs_wt (obs_wt_sub)
obs_len <- left_join(obs_len, obs_wt_sub, by = key)
# Merge with obs_len_median
obs_len <- left_join(obs_len, obs_len2, by = key)

# Check for NA in DR ------------------------------------------------------------

# There will be NA in DR_length (many records without MLS) so we dont need to check
# Check if any DR value is NA
any(is.na(obs_len$DR) == T) # FALSE - there is no NA values in DR


# 7. Calculate DPUE (kg/hour) ---------------------------------------

# DPUE can be calculated by dividing 
# LAN+DIS (CPUE) and DIS (DPUE) for fishing duration 

sort(unique(obs_len$Duration)) #Duration is in minute and need to be transformed to hour

# Transform effort unit foDur to hour
obs_len <- obs_len %>% mutate(Dur_hour = Duration/60)

# Calculate DPUE
obs_len <- obs_len %>% mutate(DPUE = D/Dur_hour)
summary(obs_len$DPUE)


# 8. Add Quarter and Month variable -------------------------------------------------

obs_len$Quarter = NA
obs_len[which(substr(obs_len$HaulTime,6,7) %in% c("01","02","03")),"Quarter"] = 1
obs_len[which(substr(obs_len$HaulTime,6,7) %in% c("04","05","06")),"Quarter"] = 2
obs_len[which(substr(obs_len$HaulTime,6,7) %in% c("07","08","09")),"Quarter"] = 3
obs_len[which(substr(obs_len$HaulTime,6,7) %in% c("10","11","12")),"Quarter"] = 4

# Transform quarter to factor
obs_len$Quarter <- factor(obs_len$Quarter, ordered = T)
class(obs_len$Quarter)

# Add Month variable
obs_len$Month <- substr(obs_len$HaulTime,6,7)
class(obs_len$Month)

# Each observation of obs is multiple sampled from sample/haul point

# 9. Calculate sample point (haul) ----------------------------------------

# Each sample point (haul) is identified at certain location (s) and time (t)
# Time (Temporal) level: Year, Quarter (HaulTime) 
# Location (Spatial) level: IcesDivision (lat, lon)

# Vessel level: Vessel, TripCode, CountHaulsIsSampled 

obs_len_sample <- obs_len %>% 
  group_by(Year, #14
           Quarter, #56
           IcesDivision, #287
           HaulTime, #9932
           HaulLatitude, #9946
           HaulLongitude, #9946
           HaulID, #9947
           TripCode,
           Vessel, 
           VesselCode,
           CountHaulsIsSampled,
  ) %>%
  summarize(obs = n(),
            D_mean = mean(D),
            L_mean = mean(L),
            D_sum = sum(D),
            L_sum = sum(L),
            DPUE = mean(DPUE),
            DR = mean(DR),
            DR_length = mean(DR_length)) #Number of observation each sample/haul

# There are total 9947 sample points 
# This is different from 10003 points of obs




# Save data ---------------------------------------------------------------

obs_len
obs <- ungroup(obs)
obs_len_sample
obs_len_sample <- ungroup(obs_len_sample)

# Save data
save(obs_len,file=paste0(dir_data,"/","obs_len_processed_20062019_TBB_DEF_70-99.RData")) #Rdata
saveRDS(obs_len,file=paste0(dir_data,"/","obs_len_processed_20062019_TBB_DEF_70-99.rds")) #rds 

save(obs_len_sample,file=paste0(dir_data,"/","obs_len_sample_processed_20062019_TBB_DEF_70-99.RData")) #Rdata
saveRDS(obs_len_sample,file=paste0(dir_data,"/","obs_len_sample_processed_20062019_TBB_DEF_70-99.rds")) #rds 


# Consider remove obs_len, obs_len_median, obs_len_median_sub, obs_len_sample, obs_wt, obs_wt_sub
# to reduce memory burden
# rm(obs_len, obs_len_median, obs_len_median_sub, obs_len_sample, obs_wt, obs_wt_sub)





########################################

# Sale data processing ----------------------------------------------------

# Check 1 data
sale <- readRDS(f[1])

########################################

# for loop to calculate mean price by Year, Quarter, and Month --------------------

# Procedure
# Load sale

# Change SaleYear to Year
# Change saleMonth to Month
# Change ScientiicName to NameScientific

# Add Quarter

# group_by Year, Quarter, Month, NameScientific - summarize mean(price_eurobykg)

# obs_sale <- na (new data frame)
# cbind obs_sale


# Create empty data obs_sale 
obs_sale <- tibble(Year = integer(),
                   Quarter = factor(),
                   Month = integer(),
                   NameScientific = character(),
                   NameEnglish = character(),
                   price_eurobykg = double(),
                   #Avoid the character vectors being converted to factors 
                   stringsAsFactors = F
)

for (i in 1:length(f)) {
  
  print(paste("Processing file",i,"from",length(f),sep=" "))
  
  sale <- readRDS(f[i])
  
  # Change SaleYear to Year
  # Change SaleMonth to Month
  # Change ScientificName to NameScientific
  sale$Year <- sale$SaleYear
  sale$Month <- sale$SaleMonth
  sale$NameScientific <- sale$ScientificName
  sale$NameEnglish <- sale$EnglishName
  
  # Add Quarter
  sale$Quarter = NA
  sale[which(substr(sale$SaleDate,6,7) %in% c("01","02","03")),"Quarter"] = 1
  sale[which(substr(sale$SaleDate,6,7) %in% c("04","05","06")),"Quarter"] = 2
  sale[which(substr(sale$SaleDate,6,7) %in% c("07","08","09")),"Quarter"] = 3
  sale[which(substr(sale$SaleDate,6,7) %in% c("10","11","12")),"Quarter"] = 4
  sale$Quarter <- factor(sale$Quarter)
  
  
  # group_by Year, Quarter, NameScientific - summarize mean(price_eurobykg)
  sale_temp <- sale %>% group_by(Year, Quarter, Month, NameScientific, NameEnglish) %>%
    summarize(price_eurobykg = mean(price_eurobykg, na.rm = T))
  sale_temp <- ungroup(sale_temp)
  
  # stack sale_temp to obs_sale
  obs_sale <- rbind(obs_sale, sale_temp, stringsAsFactors = F)
  
  rm(sale, sale_temp)
}

# Check NA
any(is.na(obs_sale$price_eurobykg)) #FALSE

# Add Date variable to explore temporal pattern of price data 
obs_sale$Date <- ymd(paste(obs_sale$Year, obs_sale$Month, "01", sep = "-")) #Day is identified as 01

# Save sale data ---------------------------------------------------------------
obs_sale
save(obs_sale,file=paste0(dir_data,"/","obs_sale_processed_20062018_TBB_DEF_70-99.RData")) #Rdata
saveRDS(obs_sale,file=paste0(dir_data,"/","obs_sale_processed_20062018_TBB_DEF_70-99.rds")) #rds 

########################################

# Merge obs_len and obs_sale --------------------------------------------------

# Remove Date from obs_sale
obs_sale_sub <- obs_sale %>% select(- Date)

# Add Month variable to obs

# Transform obs_len$Month to integer
obs_len$Month <- as.integer(obs_len$Month) # Transform quarter to factor
class(obs_len$Month)


########################################

# Check data before merging -----------------------------------------------

# Check overall obs_sale and obs_len
View(obs_sale %>% group_by(Year, NameScientific, NameEnglish) %>%
       summarize(n = n()) %>% spread(Year, n))

View(obs_len %>% group_by(Year, NameScientific, NameEnglish) %>%
       summarize(n = n()) %>% spread(Year, n))

# NameScientific - some names are not at species level (either genus or family)

# Check species names of 2 data sets
spp_len <- obs_len %>% select(FaoCode, NameScientific, NameEnglish)
spp_len <- unique(spp_len)
spp_sale <- obs_sale %>% select(NameScientific, NameEnglish)
spp_sale <- unique(spp_sale)

spp_merge <- left_join(spp_len, spp_sale, by = "NameScientific") %>% arrange(NameScientific)

# Names in obs_sale that mismatch to those in obs_len 
# Lepidorhombus spp	- Megrims nei
# Lophiidae	- Anglerfishes nei
# Mugilidae	- Mullets nei

# Name in obs_len
# Lepidorhombus whiffiagonis	- Megrim
# Lophius budegassa	- Blackbellied angler	NA
#	Lophius piscatorius	- Monkfish (Anglerfish)	


# Triglidae -	Gurnards, searobins nei - does not match to obs_sale 
# but there are only 5 records of this in 2012 - it is neglectable

# Ray - several regularly caught species do not have price in 2006,2007 - especially Sandy ray until 2009
# Those species will use sale values from Raja spp 

# 2006 - 2009
# Leucoraja circularis -	Sandy ray  

# 2006 - 2007
# Leucoraja naevus -	Cuckoo ray
# Raja brachyura -	Blonde ray
# Raja clavata -	Thornback ray
# Raja montagui -	Spotted ray


# Merging and finalizing ----------------------------------------------

# Merge obs_len and obs_sale 
key <- c("Year", "Quarter", "Month", "NameScientific")
obs_len <- left_join(obs_len, obs_sale_sub, by = key)

obs_len_raw <- obs_len

# Check obs_len after merging
View(test) 
# Since both obs_len and obs_sale have NameEnglish variable
# The merged obs_len has NameEnglish.x (of original obs_len) and NameEnglish.y (of obs_sale)
# NameEnglish.y should be removed and 
# NameEnglish.x should be changed back to NameEnglish

# Create NameEnglish 
obs_len$NameEnglish <- obs_len$NameEnglish.x
# Remove NameEnglish.x and NameEnglish.y
obs_len <- obs_len %>% select(- NameEnglish.x, - NameEnglish.y)


# Add price of mismatch species -------------------------------------------

# Those species includes
# Lepidorhombus whiffiagonis - Megrim
# Lophius budegassa	- Blackbellied angler
#	Lophius piscatorius	- Monkfish (Anglerfish)	

# and

# 2006 - 2009
# Leucoraja circularis -	Sandy ray  

# 2006 - 2007
# Leucoraja naevus -	Cuckoo ray
# Raja brachyura -	Blonde ray
# Raja clavata -	Thornback ray
# Raja montagui -	Spotted ray

# Lepidorhombus whiffiagonis - Megrim -------------------------------------

# Create subset of Megrim from obs_len without price (as price will be added)
MEG_len <- obs_len %>% 
  filter(NameScientific == "Lepidorhombus whiffiagonis") %>% 
  select(- price_eurobykg)

# Extract the price of Megrim from obs_sale (by Year, Quarter, and Month)
MEG_sale <- obs_sale %>% 
  filter(NameScientific == "Lepidorhombus spp") %>% 
  select(Year, Quarter, Month, price_eurobykg)

key <- c("Year", "Quarter", "Month")
MEG <- left_join(MEG_len, MEG_sale, by = key)

# Subset obs_len without Megrim and then add bind the subset with MEG
obs_len_sub <- obs_len %>% filter(NameScientific != "Lepidorhombus whiffiagonis")
obs_len <- rbind(obs_len_sub, MEG)


# Lophius budegassa	- Blackbellied angler -------------------------------------

# Create subset of Blackbellied angler from obs_len without price (as price will be added)
ANK_len <- obs_len %>% 
  filter(NameScientific == "Lophius budegassa") %>% 
  select(- price_eurobykg)

# Extract the price of Megrim from obs_sale (by Year, Quarter, and Month)
ANK_sale <- obs_sale %>% 
  filter(NameScientific == "Lophiidae") %>% 
  select(Year, Quarter, Month, price_eurobykg)

key <- c("Year", "Quarter", "Month")
ANK <- left_join(ANK_len, ANK_sale, by = key)

# Subset obs_len without Blackbellied angler and then add bind the subset with MEG
obs_len_sub <- obs_len %>% filter(NameScientific != "Lophius budegassa")
obs_len <- rbind(obs_len_sub, ANK)

# Lophius piscatorius	- Monkfish (Anglerfish)	 -------------------------------------

# Create subset of Monkfish (Anglerfish) from obs_len without price (as price will be added)
MON_len <- obs_len %>% 
  filter(NameScientific == "Lophius piscatorius") %>% 
  select(- price_eurobykg)

# Extract the price of Megrim from obs_sale (by Year, Quarter, and Month)
MON_sale <- obs_sale %>% 
  filter(NameScientific == "Lophiidae") %>% #both Monkfish and blackbellied angler used price data from Lophiidae
  select(Year, Quarter, Month, price_eurobykg)

key <- c("Year", "Quarter", "Month")
MON <- left_join(MON_len, MON_sale, by = key)

# Subset obs_len without Blackbellied angler and then add bind the subset with MEG
obs_len_sub <- obs_len %>% filter(NameScientific != "Lophius piscatorius")
obs_len <- rbind(obs_len_sub, MON)

# rays - need to consider -------------------------------------------------
# 2006 - 2009
# Leucoraja circularis -	Sandy ray  

# 2006 - 2007
# Leucoraja naevus -	Cuckoo ray
# Raja brachyura -	Blonde ray
# Raja clavata -	Thornback ray
# Raja montagui -	Spotted ray



########################################

# Save final data ---------------------------------------------------------------

obs_len

# Save data
save(obs_len,file=paste0(dir_data,"/","obs_final_20062019_TBB_DEF_70-99.RData")) #Rdata
saveRDS(obs_len,file=paste0(dir_data,"/","obs_final_20062019_TBB_DEF_70-99.rds")) #rds 




########################################
