#       IMBRSea Thesis 
#       Tuan Anh Bui
#       18.04.2020

#       Belgian beam trawl Discard spatial analysis

#       This is the script for data processing - environmental variables
#       Env vars: bathy, slope, chl, sst, substrate (mud, gravel, sand)

########################################

# Load support files and packages -----------------------------------------

source("HighstatLibV12.R")

# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


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
library(RColorBrewer)
library(rgdal)

#install.packages("ggpubr")
library(ggpubr)

#install.packages("ggridges")
library(ggridges)

#library(dismo)
#library(splancs)
#library(reshape)
#library(gstat)
#library(rgeos)
#library(sdmpredictors) # to get Bathymetry
#library(fields)

# library(devtools)
#install_github('timcdlucas/INLAutils', force = T)
#devtools::install_github("timcdlucas/INLAutils")
# library(INLAutils)

#install.packages("inlabru")
#library(inlabru)

#install_github("gfalbery/ggregplot")
#library(ggregplot)



########################################

# Load data ---------------------------------------------------------------

# Indicate local directory
# dir_data = "C:/Users/tbui/Thesis_R/Data" # Change directory here if the local dir for data is different

# Dir in Tuan-Anh laptop
dir_data = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data"

# obs_final_quota_20062019_TBB_DEF_70-99 with quota and other management variables
obs_final <- readRDS(file=paste0(dir_data,"/","obs_final_quota_20062019_TBB_DEF_70-99.rds"))
obs <- obs_final

########################################


# Data check --------------------------------------------------------------

# Since the rules are not available to all fishing activity 
# (certain IcesDivision or period, or in 2019 as rules are available to 2018 only)
# The availability of data should be check (non NA data)

obs_NA <- obs %>% filter(is.na(haul_limit) == T)
obs_NA_non <- obs %>% filter(is.na(haul_limit) == F)
unique(obs_NA_non$NameEnglish) 
# data is available for 9 out of 12 taxa of interest 

obs_NA_sub <- obs_NA %>% filter(NameEnglish %in% unique(obs_NA_non$NameEnglish)) %>% 
  select(Year, NameScientific, NameEnglish, HaulTime, IcesDivision, haul_limit)

a1 <-   obs_NA_non %>% group_by(NameScientific, NameEnglish) %>% summarize(n_ava = n())
a2 <-   obs_NA_sub %>% group_by(NameScientific, NameEnglish) %>% summarize(n_NA = n())
a3 <-   obs_NA_sub %>% filter(Year == 2019) %>% group_by(NameScientific, NameEnglish) %>% summarize(n_2019 = n())

obs_check <-  left_join(left_join(a1,a2),a3); rm(a1,a2,a3)
# 6/9 taxa has non NA samples more than NA ones.
# The other 3 species are: Common dab, Lemon sole, Raja spp


# Boundary Area of Interest -----------------------------------------------

# Set area of interest (aoi) 
world <- ne_countries(scale = "medium", returnclass = "sf")
aoi_lim <- c("Belgium", "Denmark", 
             "France", "Germany", 
             "Luxembourg", "Netherlands", 
             "United Kingdom", "Spain",
             "Switzerland", "Austria", "Italy","Ireland")#  "Isle of Man"
aoi <- world %>% filter(admin %in% aoi_lim)

ggplot(data = aoi) + geom_sf() + coord_sf(xlim = xlim, ylim = ylim)

st_write(aoi, paste0(dir_data,"/Data_shp","aoi.shp"))
aoi2 <- as(aoi, "Spatial") # Convert aoi from sf to sp to be processed in INLA
plot(aoi2)
# Sampling area
ggplot() +
  geom_sf(data = aoi) +
  geom_point(data = obs %>% 
               group_by(HaulLatitude, HaulLongitude) %>%
               summarize(n = n()),
             mapping = aes(x = HaulLongitude, y = HaulLatitude)) +
  coord_sf(xlim = c(-9,10), ylim = c(43.5, 56.5)) +
  theme_bw()

# Spatial range of data
range(obs$HaulLatitude) # 44.18333 - 55.98333
range(obs$HaulLongitude) # -8.250000  6.883333

# Define the boundary area around data 
# The boundary is selected so that the mesh boundary will cover the 
# sea in are of interest only 
# if x or y are set larger, other sea such as Mediterranean can be added
# thus making the mesh building complicated 

# Extent of area of interest 

xym2<- as.matrix(data.frame(x = c(min(obs$HaulLongitude)-0.5, 
                                  max(obs$HaulLongitude)+0.5, 
                                  max(obs$HaulLongitude)+0.5, 
                                  min(obs$HaulLongitude)-0.5), 
                            y = c(min(obs$HaulLatitude)-0.3, 
                                  min(obs$HaulLatitude)-0.3, 
                                  max(obs$HaulLatitude)+0.3, 
                                  max(obs$HaulLatitude)+0.3))) 

# Look at the box outside the dots
p <-  Polygon(xym2) 
ps <-  Polygons(list(p),1)
sps <-  SpatialPolygons(list(ps))
proj4string(sps) <- proj4string(aoi2) #add projection to sps
plot(sps)

# Since the substrate data does not cover the whole area of interest (sps)
# The spatial polygon for study area must be adjusted
# We first extract shapefile of boundaries (sps) and the land (aoi_rec)
# Then overlay with substrate data in QGIS to mask the boundary polygon

# Save sps to shapefile 
#sps1 <- st_as_sf(sps)
#st_write(sps1, paste0(dir_data, "/", "sps1.shp"))
#sps1

# Add new sps created in QGIS
sps <- readOGR(paste0(dir_data, "/Data_shp/", "sps_final.shp"))
plot(sps)

# Crop with the land (aoi)
aoi_rec <- crop(aoi2, sps) 
proj4string(sps) <- proj4string(aoi_rec) #Add projection to sps
plot(sps) # Plot the box
plot(aoi_rec,add=T) # Goc_rec is the land
# plot(aoi_rec)

#Select the polygon which contains the data => being the sea
coast <- rgeos::gDifference(sps, aoi_rec) #coast = the sea 
plot(coast, col="red") # in red  aoi_rec=the land

# coast will be used to identify the mesh in analysis
# save coast
coast_sf <- st_as_sf(coast)
st_write(coast_sf, paste0(dir_data, "/", "coast.shp"))
readOGR(paste0(dir_data, "/", "coast.shp")) #test reopen

########################################

# Environmental variables -------------------------------------------------

# Original files is located in local directory of Tuan-Anh's laptop
# The working version masked from the original files and smaller in size
# will be uploaded to Git_hub

# local directory for original files
dir_data_env <- "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_R/Data"
dir_data_env2 <- "C:/Users/User/Desktop/wget" #for monthly CHL and SST


#Get the file names
bathy <- raster(paste0(dir_data_env,"/","bathy_30s.tif"))
slope <- raster(paste0(dir_data_env,"/","biogeo06_30s.tif"))
gravel <- raster(paste0(dir_data_env,"/","GravelPercent.asc"))
mud <- raster(paste0(dir_data_env,"/","MudPercent.asc"))
sand <- raster(paste0(dir_data_env,"/","SandPercent.asc"))


# chl and sst pred are culmulative mean values (2002 - 2019) and will be used for prediction
chl_pred <- raster(paste0(dir_data_env,"/","A20021852019334.L3m_CU_CHL_chlor_a_4km.nc"))
sst_pred <- raster(paste0(dir_data_env,"/","AQUA_MODIS.20020704_20191130.L3m.CU.SST.sst.4km.nc"))


# Unify projection of data (WGS84 EPSG:4326)
proj4string(gravel) <- proj4string(mud) <- proj4string(sand) <- proj4string(bathy)
proj4string(chl_pred) <- proj4string(sst_pred) <- proj4string(bathy)


# Processing --------------------------------------------------------------

# Mask raster data to the area of interest
# First the data should be "croped" to adjust the extent of data
# as well as reduce computational burden
# Then the data will be "masked" to the area of interest

# Since the raster data are different in resolution
# The data should be
# 1. Crop to an extent that is larger than area of interst
# 2. Resample to unify resolution
# 3. Crop and mask again to area of interest 

# 1. Crop to an extent that is larger than area of interst (should use lapply)
# extent 
ext <- c(xmin = min(obs$HaulLongitude)-1, xmax = max(obs$HaulLongitude)+1,
         ymin = min(obs$HaulLatitude)-1, ymax = max(obs$HaulLatitude)+1)

bathy <- crop(bathy, ext)
slope <- crop(slope, ext)
chl_pred <- crop(chl_pred, ext)
sst_pred <- crop(sst_pred, ext)
gravel <- crop(gravel, ext)
mud <- crop(mud, ext)
sand <- crop(sand, ext)

# 2. Resample to unify resolution using nearest neighbor method
# chl, gravel, mud, sand are resampled to the resolution of bathy 
# (bathy has the highest resolution among the raster data)

chl_pred <- raster::resample(chl_pred, bathy, method = 'ngb') 
sst_pred <- raster::resample(sst_pred, bathy, method = 'ngb') 
gravel <- raster::resample(gravel, bathy, method = 'ngb') 
mud <- raster::resample(mud, bathy, method = 'ngb') 
sand <- raster::resample(sand, bathy, method = 'ngb') 

# 3. Crop and mask again to area of interest (sps)

# bathy
bathy <- crop(bathy, sps)
bathy <- mask(bathy, sps)
bathy <- abs(bathy) #make negative (depth) positive

# slope
slope <- crop(slope, sps)
slope <- mask(slope, sps)

# gravel, mud, sand
gravel <- crop(gravel, sps)
gravel <- mask(gravel, sps)
gravel <- gravel/100 #Convert 0-100% to ratio

mud <- crop(mud, sps)
mud <- mask(mud, sps)
mud <- mud/100 #Convert 0-100% to ratio

sand <- crop(sand, sps)
sand <- mask(sand, sps)
sand <- sand/100 #Convert 0-100% to ratio

# chl_pred sst_pred
chl_pred <- crop(chl_pred, sps) 
chl_pred <- mask(chl_pred, sps)

sst_pred <- crop(sst_pred, sps)
sst_pred <- mask(sst_pred, sps)

# Save env data -----------------------------------------------------------
dir_data
dir_data_env <- paste0(dir_data,"/Data_env")

raster::writeRaster(bathy, filename = paste0(dir_data_env,"/","bathy_sub.tif"))
raster::writeRaster(slope, filename = paste0(dir_data_env,"/","slope_sub.tif"))
raster::writeRaster(gravel, filename = paste0(dir_data_env,"/","gravel_sub.tif"))
raster::writeRaster(sand, filename = paste0(dir_data_env,"/","sand_sub.tif"))
raster::writeRaster(mud, filename = paste0(dir_data_env,"/","mud_sub.tif"))
raster::writeRaster(chl_pred, filename = paste0(dir_data_env,"/","chl_pred.tif"))
raster::writeRaster(sst_pred, filename = paste0(dir_data_env,"/","sst_pred.tif"))


# Raster visualization
p1 <- rasterVis::levelplot(bathy, margin = F, main = "bathy")
p2 <- rasterVis::levelplot(slope, margin = F, main = "slope")
p3 <- rasterVis::levelplot(chl_pred, margin = F, main = "chl_a")
p7 <- rasterVis::levelplot(sst_pred, margin = F, main = "sst_a")
p4 <- rasterVis::levelplot(gravel, margin = F, main = "gravel")
p5 <- rasterVis::levelplot(mud, margin = F, main = "mud")
p6 <- rasterVis::levelplot(sand, margin = F, main = "sand")
ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, p7,
                  ncol = 4, nrow = 2)
# Plot Gravel, Mud, Sand
ggpubr::ggarrange(p4, p5, p6,
                  ncol = 3, nrow = 1)

# Stack environmental predictors
predictors <- stack(bathy, slope, gravel, mud, sand) 


# Create the data set with the environmental predictors ---------------------------------

# raster::extract is used to extract values of raster layers at sample points
obs <- as_tibble(cbind(obs, raster::extract(predictors, as.matrix(cbind(obs$HaulLongitude,obs$HaulLatitude)))))

# Rename Environmental predictors
# layer - Bathy
# biogeo06_30s - Slope
# Chlorophyll.Concentration..OCI.Algorithm - Chl_a
names(obs)[names(obs) == "layer"] <- "Bathy"
names(obs)[names(obs) == "biogeo06_30s"] <- "Slope"
names(obs)[names(obs) == "Chlorophyll.Concentration..OCI.Algorithm"] <- "Chl_a"

# Add Substrate factor variables to data
# If percentage of any substrate type is >= 1/3, 
# the substrate will be categorized as that type
obs$Substrate <- NA
obs$Substrate <- if_else(obs$GravelPercent >= 1/3, "Gravel",
                         if_else(obs$SandPercent >= 1/3, "Sand", "Mud")
)
summary(obs$Substrate)

summary(obs) # IF you have NAs => check where they are! # It can be because the data are close to the coast
# It can also be because the extent is not overlapping the observer dataset


# SST and CHL extraction -------------------------------------------------------------

# Monthly SST and monthly and annual CHL are derived from 
# MODIS 4-km synthetic products from  
# the NASA Ocean Biology Distributed Active Archive Center (OB.DAAD) 
# https://oceancolor.gsfc.nasa.gov/ 

# SST ---------------------------------------------------------------------

library(ncdf4)
##Load netCDF files 
#Create a list of file 
# Change the directory - paste0("_change_this_part","/") - and name pattern - pattern="*_change_this_part" 
(f_sst <- list.files(path = paste0(dir_data_env2,"/"), 
                     pattern="*.L3m.MO.SST.sst.4km.nc",full.names=T))
#Note: Directory separation symbol of R "/" is not the same as of Windows "\

#Explore the netCDF file
nc_open(f_sst[1])

#Indicate variable, read from the netCDF file
var <- "sst" #"chlor_a"

##Convert netCDF files to StackRaster format
sst_stack <- stack(f_sst, varname = var) #Indicate variable as chlor_a
sst_stack

proj4string(sst_stack) <- proj4string(bathy)

#Crop RasterStack by arae of interest
sst_AOI <- crop(sst_stack, ext)

#Extract sst to obs
obs$sst <- NA

for (i in 1:length(f_sst)) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f_sst),sep=" "))
  
  data <- nc_open(f_sst[i])
  
  # Extract information from netCDF file
  # Extract date
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year <- as.integer(format(datemean, "%Y"))
  month <- as.integer(format(datemean, "%m"))
  
  obs_sub <- obs %>% filter(Year == year, Month == month)
  obs[which(obs$Year == year & obs$Month == month),"sst"] <-  raster::extract(sst_AOI[[i]],
                                                                              as.matrix(cbind(obs_sub$HaulLongitude,obs_sub$HaulLatitude)))
  
}
summary(obs$sst)

# CHL ---------------------------------------------------------------------
##Load netCDF files 
#Create a list of file 
# Change the directory - paste0("_change_this_part","/") - and name pattern - pattern="*_change_this_part" 
(f_chl <- list.files(path = paste0(dir_data_env2,"/"), 
                     pattern="*.L3m_MO_CHL_chlor_a_4km.nc",full.names=T))
#Note: Directory separation symbol of R "/" is not the same as of Windows "\

#Explore the netCDF file
nc_open(f_chl[1])

#Indicate variable, read from the netCDF file
var <- "chlor_a"

##Convert netCDF files to StackRaster format
chl_stack <- stack(f_chl, varname = var) #Indicate variable as chlor_a
chl_stack

proj4string(chl_stack) <- proj4string(bathy)

#Crop RasterStack by arae of interest
chl_AOI <- crop(chl_stack, ext)

#Extract chl to obs
obs$chl <- NA

for (i in 1:length(f_chl)) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f_chl),sep=" "))
  
  data <- nc_open(f_chl[i])
  
  # Extract information from netCDF file
  # Extract date
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year <- as.integer(format(datemean, "%Y"))
  month <- as.integer(format(datemean, "%m"))
  
  obs_sub <- obs %>% filter(Year == year, Month == month)
  obs[which(obs$Year == year & obs$Month == month),"chl"] <-  raster::extract(chl_AOI[[i]],
                                                                              as.matrix(cbind(obs_sub$HaulLongitude,obs_sub$HaulLatitude)))
  
}
summary(obs$chl) #  11280 observations are NA
obs$chl_month <- obs$chl
obs <- obs %>% select(-chl)
# Since a lot of chl are NA, annual chl is proposed to be used


# CHL annual --------------------------------------------------------------
##Load netCDF files 
#Create a list of file 
# Change the directory - paste0("_change_this_part","/") - and name pattern - pattern="*_change_this_part" 
(f_chl <- list.files(path = paste0(dir_data_env2,"/year"), 
                     pattern="*.L3m_YR_CHL_chlor_a_4km.nc",full.names=T))
#Note: Directory separation symbol of R "/" is not the same as of Windows "\

#Explore the netCDF file
nc_open(f_chl[1])

#Indicate variable, read from the netCDF file
var <- "chlor_a"

##Convert netCDF files to StackRaster format
chl_stack <- stack(f_chl, varname = var) #Indicate variable as chlor_a
chl_stack

proj4string(chl_stack) <- proj4string(bathy)

#Crop RasterStack by arae of interest
chl_AOI <- crop(chl_stack, ext)

#Extract chl to obs
obs$chl_year <- NA

for (i in 1:length(f_chl)) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f_chl),sep=" "))
  
  data <- nc_open(f_chl[i])
  
  # Extract information from netCDF file
  # Extract date
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year <- as.integer(format(datemean, "%Y"))
  
  
  obs_sub <- obs %>% filter(Year == year)
  obs[which(obs$Year == year),"chl_year"] <-  raster::extract(chl_AOI[[i]],
                                                              as.matrix(cbind(obs_sub$HaulLongitude,obs_sub$HaulLatitude)))
  
}
summary(obs$chl_year)

########################################

# Check NA ---------------------------------------------------------------

# Check 
obs_NA <- obs %>% filter(is.na(Bathy) == T | is.na(GravelPercent) == T | is.na(sst) == T | is.na(chl_year) == T) %>% 
  group_by(HaulLatitude, HaulLongitude) %>% summarize(n = n())
range(obs_NA$HaulLatitude)
range(obs_NA$HaulLongitude)

ggplot() +
  geom_sf(data = aoi) +
  geom_point(data = obs_NA, aes(x = HaulLongitude, y = HaulLatitude, color = "red")) +
  coord_sf(xlim = c(-3,3), ylim = c(44, 53.2))
# Most NA values are sample points on land (might be errors during input of lat/lon)
# 1 point in Biscay bay, should be out of range of substrate data

# Remove NA values in Bathy (Slope), Substrate (GravelPercent, Sand, Mud), sst, chl_year
obs <- obs %>% filter(is.na(Bathy) == F & is.na(GravelPercent) == F &
                        is.na(sst) == F & is.na(chl_year) == F)


# Calculate Catch, CPUE ---------------------------------------------------

obs <- obs %>% mutate(C = D + L,
                      CPUE = C/Dur_hour,
                      logCPUE = log10(CPUE)) 
summary(obs$CPUE) #no 0 CPUE, do not need to add 1 to log10
# Save data ---------------------------------------------------------------

# Final obs data
str(obs) #50,594 observations 

# dir_data="F:/backup_to_H/R_gitlab/spatial_discards_TuanAnh/followup_jochen" # Jochen
saveRDS(obs,file=paste0(dir_data,"/","obs_final_LA_full_pred_20062019_TBB_DEF_70-99.rds")) #rds 


# Adjustment of landing limitation in the final data  ----------------------

# This adjustment should be conducted in the 'Thesis_data processing_landing limitation' script
# 1. No haul_limit - quota_uti = 0
# 2. For close_fishing, if haul_limit > -100kg then 0, otherwise 1
# Ray has -5000 kg haul_limit, different from the others, but reasonable to keep at 1 quota_uti.

str(obs)

# 1. Transform Quota_uti from NA to 0.
# Assumption: no landing limitation at these sample points, thus the quota utilization is 0
obs[which(is.na(obs$Quota_uti) == T),"Quota_uti"] <- 0

# 2. For close_fishing, if haul_limit > -100kg then Quota_uti = 0, otherwise 1
# The original data has Quota_uti = 0 when haul_limit = 0, otherwise 1
obs[which(obs$limit_type == "closing_fishing" & obs$haul_limit > - 100), "Quota_uti"] <- 0

View(
  obs %>% filter(limit_type == "closing_fishing") #Double check
)


# Save new file for analysis
saveRDS(obs,file=paste0(dir_data,"/","obs_final_LA2_full_pred_20062019_TBB_DEF_70-99.rds"))
