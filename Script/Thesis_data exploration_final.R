#       IMBRSea Thesis 
#       Tuan Anh Bui
#       02.03.2020

#       Belgian beam trawl Discard spatial analysis

#       This is the script for data exploration
#       The exploration will be conducted in 2 parts
#       Part 1: full weigth data - obs_processed_20062019_TBB_DEF_70-99
#       Part 2: obs_final_20062019_TBB_DEF_70-99 (with weigth, length, sale variables)

#       The script for exploration of 2 part will be similar
#       To minimize changes and errors in the script, the working data will be name "obs"

#       This script is the final exploration
#       summarized from the draft exploration

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
#library(raster)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

#library(dismo)
#library(splancs)
#library(reshape)
#library(gstat)

########################################

# Load data ---------------------------------------------------------------

# Indicate local directory
# dir_data = "C:/Users/tbui/Thesis_R/Data" # Change directory here if the local dir for data is different

# Dir in Tuan-Anh laptop
dir_data <-  "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data"

# obs_processed_20062019_TBB_DEF_70-99 - full weight data
obs_wt <- readRDS(file=paste0(dir_data,"/","obs_processed_20062019_TBB_DEF_70-99.rds"))

# obs_final_20062019_TBB_DEF_70-99 (with weigth, length, sale variables)
obs_final <- readRDS(file=paste0(dir_data,"/","obs_final_20062019_TBB_DEF_70-99.rds"))

# obs_len_freq_20062019_TBB_DEF_70-99.rds (with weigth, length, sale variables)
obs_len_freq <- readRDS(file=paste0(dir_data,"/","obs_len_freq_20062019_TBB_DEF_70-99.rds"))

########################################

# obs_wt exploration --------------------------------------------------------

########################################

# Data overview ---------------------------------------------------------------------

# Working data: obs
obs <- obs_wt %>% filter(HaulLatitude >= 48.5) #Study area is from 48.5 Latitude

# Important variables needed to explore 

# D, L, DR - Discard, Landing, Discard ratio
# Temporal vars: Year, Quarter
# Spatial vars: IcesDivision, 

# General view
View(obs_wt %>% group_by(Year, NameScientific, NameEnglish) %>%
       summarize(n = n()) %>% spread(Year, n))

# ICES Division
unique(obs$IcesDivision) # 4A, 4B, 7A,D,E,F,G,H

# Year
sort(unique(obs$Year)) # 2006 - 2019

# Vessel
unique(obs$Vessel) # 34

# Trip
unique(obs$TripCode) # 353 

# Number of sample points (HaulID)
obs %>% group_by(HaulID) %>% summarize(n = n()) # 9166

# Number of observation: 
nrow(obs) #144858

########################################

# Mapping ---------------------------------------------------------------------

########################################

# Setting -----------------------------------------------------------------

# Set Figure directory
dir_figure <- "C:/Users/tbui/Thesis_R/Figure"

# Dir in Tuan-Anh laptop
# dir_figure = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_R/Figure"


# Select range of map

range(obs$HaulLongitude) # xlim -8.2 6.9
range(obs$HaulLatitude) # ylim 44.2 56

# The area of interest is in the North Sea
# constrain in xlim and ylim
xlim=c(-9,10); ylim=c(48.5,56) # xlim=c(-8,-4); ylim=c(49,52)

# Points that do not in area of interest  
obs_out <- obs %>% filter(HaulLatitude < 48.5)
obs_out %>% group_by(HaulID) %>% summarize(n = n()) #837 HaulID


# Set theme
theme_set(theme_bw()) 

# Set area of interest (aoi) 
world <- ne_countries(scale = "medium", returnclass = "sf")
aoi_lim <- c("Belgium", "Denmark", 
             "France", "Germany", 
             "Luxembourg",
             "Netherlands", "United Kingdom")
aoi <- world %>% filter(admin %in% aoi_lim)

# Set obs_sample
obs_sample <- obs %>% filter(HaulLatitude >= 48.5) %>%
  group_by(Year, 
           Quarter, 
           IcesDivision, 
           Vessel,
           VesselCode,
           HaulID, 
           HaulLongitude, 
           HaulLatitude) %>%
  summarize(D = sum(D),
            L = sum(L),
            DPUE = sum(DPUE),
            DR = mean(DR))


# All points - experiment design ------------------------------------------

# Map all points
# D as size
ggplot() + 
  geom_sf(data = aoi) + # Area of interest
  geom_point(data = obs_sample, 
             aes(x = HaulLongitude, y = HaulLatitude, size = D),
             alpha = 1/10) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(label = "Belgian observer data - obs (2006 - 2019)")

# Ices Div - Year ---------------------------------------------------------

# Ices Div - Year 
ggplot() + 
  geom_sf(data = aoi) + # Area of interest
  geom_point(data = obs_sample, 
             aes(x = HaulLongitude, 
                 y = HaulLatitude, 
                 color = IcesDivision),
             alpha = 1/2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(label = "Belgian observer data - obs (2006 - 2019)") +
  facet_wrap(~ Year)

# Check number of sample by Ices Div - Year
obs %>%
  group_by(IcesDivision, Year, HaulID) %>% 
  summarize(n = n()) %>%
  group_by(IcesDivision, Year) %>%
  summarize(n_HaulID = n()) %>%
  spread(Year, n_HaulID)

# TripCode
obs %>%
  group_by(IcesDivision, Year, TripCode) %>% 
  summarize(n = n()) %>%
  group_by(IcesDivision, Year) %>%
  summarize(n_TripCode = n()) %>%
  spread(Year, n_TripCode)

View(TripCode <- obs %>%
       group_by(Year, TripCode) %>% 
       summarize(n = n()) %>%
       group_by(Year) %>%
       summarize(n_TripCode = n())
)

# Vessel
View(
  obs %>%
    group_by(Year, Vessel) %>% 
    summarize(n = n()) %>%
    group_by(Year) %>%
    summarize(n_Vessel = n())
)

# Ices Div - Quarter ------------------------------------------------------

ggplot() + 
  geom_sf(data = aoi) + # Area of interest
  geom_point(data = obs_sample, 
             aes(x = HaulLongitude, 
                 y = HaulLatitude, 
                 color = IcesDivision),
             alpha = 1/2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(label = "Belgian observer data - obs (2006 - 2019)") +
  facet_wrap(~ Quarter)

# Quarter
obs %>%
  group_by(Quarter, HaulID) %>% 
  summarize(n = n()) %>%
  group_by(Quarter) %>%
  summarize(n_HaulID = n())

obs %>%
  group_by(IcesDivision, Quarter, HaulID) %>% 
  summarize(n = n()) %>%
  group_by(IcesDivision, Quarter) %>%
  summarize(n_HaulID = n()) %>%
  spread(IcesDivision, n_HaulID)

########################################

# Discard and Landing amount ----------------------------------------

########################################

# By species  -----------------------------------------------------------------

DL_spp <- obs %>% group_by(NameScientific, NameEnglish) %>%
  summarize(D = sum(D),
            L = sum(L))

# Discard by species
ggplot(data = DL_spp,
       aes(x = reorder(NameEnglish, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# Species with sum(D) > 25,000 kg
# Name English
ggplot(data = DL_spp %>% filter(D > 25000),
       aes(x = reorder(NameEnglish, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# Name Scientific
ggplot(data = DL_spp %>% filter(D > 25000),
       aes(x = reorder(NameScientific, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# Species with most discard 
View(DL_spp %>% filter(D > 25000) %>% arrange(desc(D)) )

# Discard and landing 
DL_spp %>% filter(D > 25000) %>% arrange(desc(D)) %>% 
  gather(Fate, Weight, D:L)

ggplot(data = DL_spp %>% filter(D > 25000) %>% arrange(desc(D)) %>% 
         gather(Fate, Weight, D:L),
       aes(x = reorder(NameEnglish,Weight),
           y = Weight, fill = Fate)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Species with most discard 
spp_list <- DL_spp %>% 
  filter(D > 25000) %>% 
  arrange(desc(D))


########################################

# DR ---------------------------------------------------------------------

########################################

# By species  -----------------------------------------------------------------

DR_spp <- obs %>% group_by(NameScientific, NameEnglish) %>%
  summarize(DR = mean(DR))

# All species
ggplot(data = DR_spp,
       aes(x = reorder(NameEnglish, DR),
           y = DR, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

View(DR_spp %>% 
       group_by(NameEnglish) %>% summarize(DR = mean(DR)) %>% arrange(desc(DR)))

# Most discarded species
ggplot(data = DR_spp %>% 
         filter(NameEnglish %in% spp_list$NameEnglish),
       aes(x = reorder(NameEnglish, DR),
           y = DR, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()



# Box plot
# All species
ggplot(data = obs,
       aes(x = reorder(NameEnglish, DR),
           y = DR, 
       )) +
  geom_boxplot() +
  coord_flip()

# Most discarded species
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list$NameEnglish),
       aes(x = reorder(NameEnglish, DR),
           y = DR, 
       )) +
  geom_boxplot() +
  coord_flip()

# View DR of all species
View(DR_spp %>% filter(NameEnglish %in% spp_list$NameEnglish) %>% arrange(desc(DR)))

# View Discard and Landing of all species
View(obs %>% group_by(NameEnglish) %>% summarize(L = sum(L), D = sum(D)) %>%
       arrange(D))

# Species with high DR (almost always discarded)
spp_discard <- filter(DR_spp, DR >= 0.79)$NameEnglish 

# Check D and L of those high DR species
View(obs %>% filter(NameEnglish %in% spp_discard) %>% 
       group_by(NameEnglish) %>% summarize(L = sum(L), D = sum(D)) %>%
       arrange(D))

########################################


# Species groups ----------------------------------------------------------


# Proposed study species (based on exploration of Discard amount and Discard ratio) 

# Always discarded : Picked dogfish, Smooth-hound, Undulated ray, Cuckoo ray, Spotted ray

# High variation: Lesser-spotted dogfish, Pouting, Haddock, Thornback ray, Common dab, 
# Whiting, European hake, Gurnards, searobins nei; European plaice, Atlantic cod, 
# Raja rays nei, Lemon sole, Monfish (Anglerfish)

# Never discarded: Common sole, Pollack, Turbot, Brill

spp_selected <- c("Common sole", 
                  "European plaice", 
                  "Whiting", "Haddock",
                  "Atlantic cod", "Spotted ray",  
                  "Thornback ray",
                  "Monkfish (Anglerfish)", 
                  "Megrim", "Turbot", "Gurnards, searobins nei", 
                  "European hake"
)

# Always discarded group
spp_selected1 <- c("Picked dogfish",
                   "Smooth-hound",
                   "Undulate ray",
                   "Cuckoo ray",
                   "Spotted ray")

# Create species list and dataframe
obs <- ungroup(obs) #Ungroup obs
spp_list1 <- obs %>% 
  select(NameScientific, NameEnglish) %>% 
  filter(NameEnglish %in% spp_selected1)

spp_list1 <- unique(spp_list1)
spp1 <- obs %>% filter(NameEnglish %in% spp_list1$NameEnglish)

# High variation group
spp_selected2 <- c("Lesser-spotted dogfish",
                   "Pouting",
                   "Haddock",
                   "Thornback ray",
                   "Common dab",
                   "Whiting",
                   "European hake",
                   "Gurnards, searobins nei",
                   "European plaice",
                   "Atlantic cod",
                   "Raja rays nei",
                   "Lemon sole",
                   "Monfish (Anglerfish)")

# Create species list and dataframe
spp_list2 <- obs %>% 
  select(NameScientific, NameEnglish) %>% 
  filter(NameEnglish %in% spp_selected2)

spp_list2 <- unique(spp_list2)
spp2 <- obs %>% filter(NameEnglish %in% spp_list2$NameEnglish)

# Never discarded
spp_selected3 <- c("Common sole",
                   "Pollack",
                   "Turbot",
                   "Brill")

# Create species list and dataframe
spp_list3 <- obs %>% 
  select(NameScientific, NameEnglish) %>% 
  filter(NameEnglish %in% spp_selected3)

spp_list3 <- unique(spp_list3)
spp3 <- obs %>% filter(NameEnglish %in% spp_list3$NameEnglish)


########################################

# Variation by Year -------------------------------------------------------

########################################

# Discard all species -----------------------------------------------------

# Discard
DL_year <- obs %>% group_by(Year) %>%
  summarize(D_mean = mean(D),
            L_mean = mean(L),
            D = sum(D),
            L = sum(L),
            DPUE_mean = mean(DPUE)
  )

# Discard by year
ggplot(data = DL_year,
       aes(x = Year,
           y = D, 
       )) +
  geom_bar(stat = "identity")

ggplot(data = DL_year %>% gather(Fate, Weight, D:L),
       aes(x = Year,
           y = Weight,
           fill = Fate)) +
  geom_bar(stat = "identity")

# HaulID

# D
ggplot(data = obs %>% group_by(Year, HaulID) %>%
         summarize(D = sum(D),
                   DPUE = sum(DPUE)),
       aes(x = as.factor(Year),
           y = D)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,500))

View(obs %>% group_by(Year, HaulID) %>%
  summarize(D = sum(D),
            DPUE = sum(DPUE)))

# DPUE
ggplot(data = obs %>% group_by(Year, HaulID) %>%
         summarize(D = sum(D),
                   DPUE = sum(DPUE)),
       aes(x = as.factor(Year),
           y = DPUE)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,10))


# DR all species ----------------------------------------------------------

DR_year <- obs %>% group_by(Year) %>%
  summarize(DR = mean(DR))

# DR by year
ggplot(data = DR_year,
       aes(x = Year,
           y = DR, 
       )) +
  geom_bar(stat = "identity")

# Boxplot
ggplot(data = obs,
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot()


# spp_list1 ---------------------------------------------------------------
# Discard
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Cuckoo ray, Spotted ray
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish) %>%
         filter(NameEnglish != "Cuckoo ray" & NameEnglish != "Spotted ray"),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish) +
  xlim(2005,2020)

# DR
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Discard and landing
ggplot(data = DL_spp %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish) %>%
         arrange(desc(D)) %>% 
         gather(Fate, Weight, D:L),
       aes(x = reorder(NameEnglish,Weight),
           y = Weight, fill = Fate)) +
  geom_bar(stat = "identity") +
  coord_flip()

#DPUE
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = as.factor(Year),
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,10))

# spp_list2 ---------------------------------------------------------------

# Discard
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Common dab, Lesser-spotted dogfish
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Common dab", "Lesser-spotted dogfish")),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish) %>%
         filter(NameEnglish != "Common dab" & NameEnglish != "Lesser-spotted dogfish"),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish) +
  xlim(2005,2020)

# European hake, Lemon sole
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("European hake", "Lemon sole")),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# DR
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Haddock, Lemon sole, Pouting, and Whiting

ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% c("Haddock", "Lemon sole", "Pouting", "Whiting")),
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# spp_list3 ---------------------------------------------------------------

# Discard
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Common sole
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Common sole")),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish) %>%
         filter(NameEnglish != "Common sole"),
       aes(x = Year,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish) +
  xlim(2005,2020)


# DR
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) + 
  coord_cartesian(ylim = c(0,0.2))

# Discard and landing
ggplot(data = DL_spp %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish) %>%
         arrange(desc(D)) %>% 
         gather(Fate, Weight, D:L),
       aes(x = reorder(NameEnglish,Weight),
           y = Weight, fill = Fate)) +
  geom_bar(stat = "identity") +
  coord_flip()

#DPUE
ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = as.factor(Year),
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,7.5))

ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% c("Brill","Turbot")),
       aes(x = as.factor(Year),
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,1))

# Check Brill
Brill <- obs %>% filter(NameEnglish == "Brill")
hist(Brill$DPUE, breaks = 100)
Brill1 <- Brill %>% filter(DPUE != 0)
hist(Brill1$DPUE, breaks = 100)
summary(Brill1$DPUE)

# Distribution without 0 discard
ggplot(data = Brill1 %>% 
         group_by(Year, NameEnglish),
       aes(x = as.factor(Year),
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,2))

ggplot(data = Brill1 %>% 
         group_by(Year, NameEnglish),
       aes(x = as.factor(Year),
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,0.5))

########################################

# Variation by Quarter -------------------------------------------------------

########################################

# Discard all species -----------------------------------------------------

DL_q <- obs %>% group_by(Quarter) %>%
  summarize(D_mean = mean(D),
            L_mean = mean(L),
            D = sum(D),
            L = sum(L),
            DPUE_mean = mean(DPUE)
  )

# Discard by quarter
ggplot(data = DL_q,
       aes(x = Quarter,
           y = D, 
       )) +
  geom_bar(stat = "identity")

ggplot(data = obs %>% group_by(Quarter, HaulID) %>% summarize(n = n()) %>%
         group_by(Quarter) %>% summarize(n_HaulID = n()),
       aes(x = Quarter, y = n_HaulID)) +
  geom_bar(stat = "identity")


# DR all species ----------------------------------------------------------

# Boxplot
ggplot(data = obs,
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot()


# spp_list1 ---------------------------------------------------------------
# Discard
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Cuckoo ray, Spotted ray
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish) %>%
         filter(NameEnglish != "Cuckoo ray" & NameEnglish != "Spotted ray"),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# DR
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# DPUE

ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = Quarter,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,20))

ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = Quarter,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) + 
  coord_cartesian(ylim = c(0,25))


# Discard and landing
ggplot(data = DL_spp %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish) %>%
         arrange(desc(D)) %>% 
         gather(Fate, Weight, D:L),
       aes(x = reorder(NameEnglish,Weight),
           y = Weight, fill = Fate)) +
  geom_bar(stat = "identity") +
  coord_flip()

# spp_list2 ---------------------------------------------------------------

# Discard
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Common dab, Lesser-spotted dogfish
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Common dab", "Lesser-spotted dogfish")),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish) %>%
         filter(NameEnglish != "Common dab" & NameEnglish != "Lesser-spotted dogfish"),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Atlantic cod, European hake, Lemon sole
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Atlantic cod","European hake", "Lemon sole")),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# DR
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Haddock, Lemon sole, Pouting, and Whiting

ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% c("Haddock", "Lemon sole", "Pouting", "Whiting")),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# spp_list3 ---------------------------------------------------------------

# Discard
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Common sole
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Common sole")),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish) %>%
         filter(NameEnglish != "Common sole"),
       aes(x = Quarter,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish) 

# DR
ggplot(data = obs %>% 
         group_by(Quarter) %>%
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,0.2))

ggplot(data = obs %>% 
         group_by(Quarter) %>%
         filter(NameEnglish == "Common sole"),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# DPUE

ggplot(data = obs %>% 
         group_by(Quarter, NameEnglish) %>%
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = Quarter,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,5))

# Check Brill
Brill <- obs %>% filter(NameEnglish == "Brill")
hist(Brill$DPUE, breaks = 100)
Brill1 <- Brill %>% filter(DPUE != 0)
hist(Brill1$DPUE, breaks = 100)
summary(Brill1$DPUE)

# Distribution without 0 discard
ggplot(data = Brill1 %>% 
         group_by(Quarter, NameEnglish),
       aes(x = Quarter,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,1))

ggplot(data = Brill1 %>% 
         group_by(Year, NameEnglish),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,1))







########################################

# Variation by Division -------------------------------------------------------

########################################

# Discard all species -----------------------------------------------------

DL_ices <- obs %>% group_by(IcesDivision) %>%
  summarize(D = sum(D),
            L = sum(L))

ggplot(data = DL_ices,
       aes(x = reorder(IcesDivision, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# D
ggplot(data = obs %>% group_by(IcesDivision, HaulID) %>%
         summarize(D = sum(D),
                   DPUE = sum(DPUE)),
       aes(x = IcesDivision,
           y = D)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,500))


# DR all species ----------------------------------------------------------

# Boxplot
ggplot(data = obs,
       aes(x = IcesDivision,
           y = DR)) +
  geom_boxplot()


# spp_list1 ---------------------------------------------------------------
# Discard
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Cuckoo ray, Spotted ray
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish) %>%
         filter(NameEnglish != "Cuckoo ray" & NameEnglish != "Spotted ray"),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# DR
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = IcesDivision,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = IcesDivision,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# DPUE

ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = IcesDivision,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,17))

ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% c("Cuckoo ray","Spotted ray")),
       aes(x = IcesDivision,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) + 
  coord_cartesian(ylim = c(0,25))

# spp_list2 ---------------------------------------------------------------

# Discard
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# Lesser-spotted dogfish, Pouting
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Pouting", "Lesser-spotted dogfish")),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish) %>%
         filter(NameEnglish != "Pouting" & NameEnglish != "Lesser-spotted dogfish"),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Atlantic cod, European hake, Lemon sole
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Atlantic cod","European hake", "Lemon sole")),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)


# DR
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = IcesDivision,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

#DPUE
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = IcesDivision,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,50))

# Haddock, Lemon sole, Pouting, and Whiting

ggplot(data = obs %>% 
         group_by(Year, NameEnglish) %>%
         filter(NameEnglish %in% c("Haddock", "Lemon sole", "Pouting", "Whiting")),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# spp_list3 ---------------------------------------------------------------

# Discard
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Common sole
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% c("Common sole")),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         summarize(D = sum(D)) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish) %>%
         filter(NameEnglish != "Common sole"),
       aes(x = IcesDivision,
           y = D)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NameEnglish) 

# DR
ggplot(data = obs %>% 
         group_by(IcesDivision) %>%
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = IcesDivision,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,0.7))

ggplot(data = obs %>% 
         group_by(IcesDivision) %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish) %>%
         filter(NameEnglish != "Common sole") %>%
         filter(DR != 0),
       aes(x = IcesDivision,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# DPUE

ggplot(data = obs %>% 
         group_by(IcesDivision, NameEnglish) %>%
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = IcesDivision,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,10))

# Check Brill
Brill <- obs %>% filter(NameEnglish == "Brill")
hist(Brill$DPUE, breaks = 100)
Brill1 <- Brill %>% filter(DPUE != 0)
hist(Brill1$DPUE, breaks = 100)
summary(Brill1$DPUE)

# Distribution without 0 discard
ggplot(data = Brill1 %>% 
         group_by(Quarter, NameEnglish),
       aes(x = Quarter,
           y = DPUE)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,1))

ggplot(data = Brill1 %>% 
         group_by(Year, NameEnglish),
       aes(x = Quarter,
           y = DR)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,1))







########################################

# obs_final exploration --------------------------------------------------------

########################################

# Data overview -----------------------------------------------------------

# Working data: obs
obs <- obs_final %>% filter(HaulLatitude >= 48.5)

# Important variables needed to explore 

# D, L, DR - Discard, Landing, Discard ratio
# DR_length - Discard ratio by length
# L_median, L_wt_mean - Length median, Length weighted mean
# DPUE - Discard per unit effort
# price_eurobykg - price
# Temporal vars: Year, Quarter
# Spatial vars: IcesDivision, 

# General view
View(obs %>% group_by(Year, NameScientific, NameEnglish) %>%
       summarize(n = n()) %>% spread(Year, n))

# ICES Division
unique(obs$IcesDivision) # 4A, 4B, 7A,D,E,F,G,H

# Year
sort(unique(obs$Year)) # 2006 - 2019

# Vessel
unique(obs$Vessel) # 34

# Trip
unique(obs$TripCode) # 353 

# Number of sample points (HaulID)
obs %>% group_by(HaulID) %>% summarize(n = n()) # 9111

# Number of observation: 
nrow(obs) #69327


# The obs_final was constructed from obs_len (length) data
# There fore only certain species (commercial species) have records for all years
# Most ray species have record until 2012 only

# Discard species  -----------------------------------------------------------------

DL_spp <- obs %>% group_by(NameScientific, NameEnglish) %>%
  summarize(D = sum(D),
            L = sum(L))

# Discard by species
ggplot(data = DL_spp,
       aes(x = reorder(NameEnglish, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# Species with sum(D) > 5,000 kg
# Name English
ggplot(data = DL_spp %>% filter(D > 5000),
       aes(x = reorder(NameEnglish, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# Name Scientific
ggplot(data = DL_spp %>% filter(D > 5000),
       aes(x = reorder(NameScientific, D),
           y = D, 
       )) +
  geom_bar(stat = "identity") +
  coord_flip()

# Species with most discard 
View(DL_spp %>% filter(D > 5000) %>% arrange(desc(D)) )

# Discard and landing 
DL_spp %>% filter(D > 5000) %>% arrange(desc(D)) %>% 
  gather(Fate, Weight, D:L)

ggplot(data = DL_spp %>% filter(D > 5000) %>% arrange(desc(D)) %>% 
         gather(Fate, Weight, D:L),
       aes(x = reorder(NameEnglish,Weight),
           y = Weight, fill = Fate)) +
  geom_bar(stat = "identity") +
  coord_flip()


# Check ray and skate -----------------------------------------------------

ray_skate <- obs %>%
  filter(str_detect(NameEnglish, "ray|skate")) %>%
  group_by(Year, NameScientific, NameEnglish) %>%
  summarize(D = sum(D),
            DR = mean(DR)) %>%
  arrange(NameScientific, Year)

# Check gurnard -----------------------------------------------------------

gurnard <- obs %>% 
  filter(str_detect(NameEnglish, "urnard")) %>% #detect all string with urnard  
  group_by(Year, NameScientific, NameEnglish) %>%
  summarize(D = sum(D),
            DR = mean(DR)) %>%
  arrange(NameScientific, Year)

# Triglidae is recorded in 2012 only
# Other gurnard species are recorded in 2010 only

########################################

# Mapping ---------------------------------------------------------------------

########################################

# Setting -----------------------------------------------------------------

# Set Figure directory
dir_figure <- "C:/Users/tbui/Thesis_R/Figure"

# Dir in Tuan-Anh laptop
# dir_data = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_R/Figure"


# Select range of map

range(obs$HaulLongitude) # xlim -8.2 6.9
range(obs$HaulLatitude) # ylim 44.2 56

# The area of interest is in the North Sea
# constrain in xlim and ylim
xlim=c(-9,10); ylim=c(48.5,56) # xlim=c(-8,-4); ylim=c(49,52)

# Points that do not in area of interest  
obs_out <- obs %>% filter(HaulLatitude < 48.5)
obs_out %>% group_by(HaulID) %>% summarize(n = n()) #836 HaulID


# Set theme
theme_set(theme_bw()) 

# Set area of interest (aoi) 
world <- ne_countries(scale = "medium", returnclass = "sf")
aoi_lim <- c("Belgium", "Denmark", 
             "France", "Germany", 
             "Luxembourg",
             "Netherlands", "United Kingdom")
aoi <- world %>% filter(admin %in% aoi_lim)

# Set obs_sample
obs_sample <- obs %>% filter(HaulLatitude >= 48.5) %>%
  group_by(Year, 
           Quarter, 
           IcesDivision, 
           Vessel,
           VesselCode,
           HaulID, 
           HaulLongitude, 
           HaulLatitude) %>%
  summarize(D = sum(D),
            L = sum(L),
            DPUE = sum(DPUE),
            DR = mean(DR))



# All points ------------------------------------------

# Map all points
ggplot() + 
  geom_sf(data = aoi) + # Area of interest
  geom_point(data = obs_sample, 
             aes(x = HaulLongitude, y = HaulLatitude, color = IcesDivision),
             alpha = 1/10) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle(label = "Belgian observer data - obs (2006 - 2019)")

# Each species (by Year and Quarter) ---------------------------------------------

# for loop for mapping (each map is for each species)

# spp_list has the NameSci (col 1) and NameEnglish (col 2) 
# of selected species

nrow(spp_list)

for (i in 1:nrow(spp_list)) { 
  
  # Indicating file processing
  print(paste0("Processing file ",i,"/",nrow(spp_list)," - ",spp_list[i,2]))
  
  # Subset the data of species being mapped
  spp <- obs_spp %>% filter(NameEnglish %in% spp_list[i,2])
  
  # Mapping
  ggplot() + 
    geom_sf(data = aoi) + # Area of interest
    geom_point(data = spp, 
               aes(x = HaulLongitude, y = HaulLatitude, col = Quarter),
               alpha=1/5) +
    facet_wrap(.~ Year) + # Wraping by year
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) + 
    
    xlab("Longitude") + ylab("Latitude") + 
    ggtitle(label = "Belgian observer data", 
            subtitle = paste0(spp_list[i,1]," (",spp_list[i,2],") | ",
                              paste(range(spp$Year),collapse=" - "))) 
  
  # Save map
  ggsave(path = paste0(dir_figure,"/"),
         filename = paste0("Belgian observer data - ",spp_list[i,2],".png"),
         scale = 2,
         plot = last_plot(), dpi = 600,
         width = 15, height = 12, units = c("cm"))
}
########################################

# Price -------------------------------------------------------------------

########################################

# All species -------------------------------------------------------------

spp_list_all <- rbind(spp_list1, spp_list2, spp_list3)

# subset based on species of interest
obs_sale_spp <-  obs %>% 
  filter(NameEnglish %in% spp_list_all$NameEnglish) #The same if use NameScientific (a bit trickier)

unique(obs_sale_spp$NameEnglish) #17 species out of 20 selected species have data for price

# Price by species
ggplot(data = obs_sale_spp %>% 
         group_by(NameEnglish) %>% 
         summarize(price_eurobykg = mean(price_eurobykg, na.rm = T)),
       aes(x = reorder(NameEnglish,price_eurobykg), 
           y = price_eurobykg)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(data = obs_sale_spp %>% filter(is.na(price_eurobykg) == F),
       aes(x = reorder(NameEnglish, price_eurobykg, FUN = "median"), 
           y = price_eurobykg)) +
  geom_boxplot() +
  coord_flip()


# Price plot
ggplot() + 
  geom_line(data = obs_sale_spp %>% 
              group_by(Year, Quarter, NameEnglish) %>%
              summarize(price_eurobykg = mean(price_eurobykg)), 
            aes(x = Year, y = price_eurobykg, color = Quarter),
            alpha=2/3) +
  facet_wrap(.~ NameEnglish) # This plot is not informative 

# Plot for price at different level: Year, Quarter, Month should be made 

# Brill, Sole and Turbot having higher price than other species
# Split data to high (Brill, Turbot, Sole) and low (others)
obs_sale_high <- obs_sale_spp %>%
  filter(NameEnglish %in% c("Brill", "Common sole","Turbot"))
obs_sale_low <- anti_join(obs_sale_spp, obs_sale_high, by = "NameEnglish")


# High price --------------------------------------------------------------

# Year
ggplot() + 
  geom_line(data = obs_sale_high %>% 
              group_by(Year, NameEnglish) %>%
              summarize(price_eurobykg = mean(price_eurobykg)), 
            aes(x = Year, y = price_eurobykg),
            alpha=2/3) +
  facet_wrap(.~ NameEnglish) # We see difference of price by year

# Quarter
ggplot() + 
  geom_line(data = obs_sale_high %>% 
              group_by(Year, Quarter, NameEnglish) %>%
              summarize(price_eurobykg = mean(price_eurobykg)), 
            aes(x = Year, y = price_eurobykg, col = Quarter),
            alpha=2/3) +
  facet_wrap(.~ NameEnglish) # We see difference of price by quarter but not so clear

# Month
ggplot(data = obs_sale_high %>% 
         group_by(Quarter, Month, NameEnglish) %>%
         summarize(price_eurobykg = mean(price_eurobykg, na.rm = T)), 
       aes(x = Month, y = price_eurobykg)) + 
  geom_line(alpha=2/3) +
  geom_point(aes(color = Quarter)) +
  facet_wrap(.~ NameEnglish) +
  scale_x_continuous(breaks=seq(1,12))

# Low price ---------------------------------------------------------------

# Year
ggplot() + 
  geom_line(data = obs_sale_low %>% 
              group_by(Year, NameEnglish) %>%
              summarize(price_eurobykg = mean(price_eurobykg)), 
            aes(x = Year, y = price_eurobykg),
            alpha=2/3) +
  facet_wrap(.~ NameEnglish) # We see difference of price by year

# Lemons sole
ggplot() + 
  geom_line(data = obs_sale_low %>% 
              filter(NameEnglish == "Lemon sole") %>%
              group_by(Year, NameEnglish) %>%
              summarize(price_eurobykg = mean(price_eurobykg)), 
            aes(x = Year, y = price_eurobykg),
            alpha=2/3) +
  facet_wrap(.~ NameEnglish) +
  coord_cartesian(xlim = c(2006,2018))

# Eliminate Gurnards, Lesser-spotted dogfish, Pollack, Undulate ray
ggplot() + 
geom_line(data = obs_sale_low %>% 
            filter(NameEnglish != "Lemon sole") %>%
            filter(NameEnglish != "Gurnards, searobins nei" & NameEnglish != "Lesser-spotted dogfish") %>%
            filter(NameEnglish != "Pollack" & NameEnglish != "Undulate ray") %>%
            group_by(Year, NameEnglish) %>%
            summarize(price_eurobykg = mean(price_eurobykg)), 
          aes(x = Year, y = price_eurobykg),
          alpha=2/3) +
  facet_wrap(.~ NameEnglish)



# Quarter
ggplot() + 
  geom_line(data = obs_sale_low %>% 
              group_by(Year, Quarter, NameEnglish) %>%
              summarize(price_eurobykg = mean(price_eurobykg)), 
            aes(x = Year, y = price_eurobykg, col = Quarter),
            alpha=2/3) +
  facet_wrap(.~ NameEnglish) # We see difference of price by quarter but not so clear


# Month
ggplot(data = obs_sale_low %>% 
         group_by(Quarter, Month, NameEnglish) %>%
         summarize(price_eurobykg = mean(price_eurobykg)), 
       aes(x = Month, y = price_eurobykg)) + 
  geom_line(alpha=2/3) +
  geom_point(aes(color = Quarter)) +
  facet_wrap(.~ NameEnglish) +
  scale_x_continuous(breaks=seq(1,12))


# Lemons sole
ggplot(data = obs_sale_low %>% 
         filter(NameEnglish == "Lemon sole") %>%
         group_by(Quarter, Month, NameEnglish) %>%
         summarize(price_eurobykg = mean(price_eurobykg, na.rm = T)), 
       aes(x = Month, y = price_eurobykg)) + 
  geom_line(alpha=2/3) +
  geom_point(aes(color = Quarter)) +
  facet_wrap(.~ NameEnglish) +
  scale_x_continuous(breaks=seq(1,12))

# Eliminate Gurnards, Lesser-spotted dogfish, Pollack, Undulate ray
ggplot(data = obs_sale_low %>% 
         filter(NameEnglish != "Lemon sole") %>%
         filter(NameEnglish != "Gurnards, searobins nei" & NameEnglish != "Lesser-spotted dogfish") %>%
         filter(NameEnglish != "Pollack" & NameEnglish != "Undulate ray") %>%
         group_by(Quarter, Month, NameEnglish) %>%
         summarize(price_eurobykg = mean(price_eurobykg, na.rm = T)), 
       aes(x = Month, y = price_eurobykg)) + 
  geom_line(alpha=2/3) +
  geom_point(aes(color = Quarter)) +
  facet_wrap(.~ NameEnglish) +
  scale_x_continuous(breaks=seq(1,12))

# Each species  -----------------------------------------------------------

# Each species 
# Closer look at each species (Monthly basis)
# We use obs_sale_spp

# Set Figure directory
dir_figure <- "C:/Users/tbui/Thesis_R/Figure"
# Dir in Tuan-Anh laptop
# dir_data = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_R/Figure"


# A loop to create figures of all species

# List of all species in obs_sale_spp
spp_sale_list <- unique(obs_sale_spp %>% select(NameScientific, NameEnglish))

for (i in 1:nrow(spp_sale_list)) {
  # Indicating file processing
  print(paste0("Processing file ",i,"/",nrow(spp_sale_list)," - ",spp_sale_list[i,2]))
  
  # Subset the data of species being mapped
  spp <- obs_sale_spp %>% filter(NameScientific %in% spp_sale_list[i,1])
  
  ggplot(data = spp, 
         aes(x = Month, y = price_eurobykg)) + 
    geom_line() +
    geom_point(aes(color = Quarter)) +
    facet_wrap(.~ Year) +
    scale_x_continuous(breaks=seq(1,12)) +
    
    ggtitle(label = "Sale data", 
            subtitle = paste0(spp_sale_list[i,1]," (",spp_sale_list[i,2],") | ",
                              paste(range(spp$Year),collapse=" - "))) 
  
  # Save map
  ggsave(path = paste0(dir_figure,"/"),
         filename = paste0("Sale data - ",spp_sale_list[i,2],".png"),
         scale = 2,
         plot = last_plot(), dpi = 600,
         width = 15, height = 12, units = c("cm"))
}












########################################

# L_wt_mean ---------------------------------------------------------------

# Price by species
ggplot(data = obs_sale_spp %>% 
         group_by(NameEnglish) %>% 
         summarize(L_wt_mean = mean(L_wt_mean, na.rm = T)),
       aes(x = reorder(NameEnglish,L_wt_mean), 
           y = L_wt_mean)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(data = obs_sale_spp,
       aes(x = reorder(NameEnglish, L_wt_mean, FUN = "median"), 
           y = L_wt_mean)) +
  geom_boxplot() +
  coord_flip()

########################################

# By Year -----------------------------------------------------------------

# spp_list1
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

# spp_list2
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

# Gurnards, Lesser-spotted dogfish and Pouting do not have sufficient data

# Atlantic cod, European hake
ggplot(data = obs %>% 
         filter(NameEnglish %in% c("Atlantic cod","European hake")), 
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>%
         filter(NameEnglish %in% spp_list2$NameEnglish) %>%
         filter(NameEnglish != "Atlantic cod" & NameEnglish != "European hake"), 
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)



# spp_list3

ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

ggplot(data = obs %>% 
         filter(NameEnglish == "Common sole"),
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 


# By Quarter -----------------------------------------------------------------

# spp_list1
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = Quarter,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

# spp_list2
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = Quarter,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

# Gurnards, Lesser-spotted dogfish and Pouting do not have sufficient data

# Atlantic cod, European hake
ggplot(data = obs %>% 
         filter(NameEnglish %in% c("Atlantic cod","European hake", "Lesser-spotted dogfish")), 
       aes(x = Quarter,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>%
         filter(NameEnglish %in% spp_list2$NameEnglish) %>%
         filter(NameEnglish != "Atlantic cod" & NameEnglish != "European hake") %>%
         filter(NameEnglish != "Lesser-spotted dogfish"), 
       aes(x = Quarter,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) +
  coord_cartesian(ylim = c(0,500))


# spp_list3

ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = Quarter,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

ggplot(data = obs %>% 
         filter(NameEnglish == "Common sole"),
       aes(x = as.factor(Year),
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

# By Division -----------------------------------------------------------------

# spp_list1
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list1$NameEnglish),
       aes(x = IcesDivision,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

# spp_list2
ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list2$NameEnglish),
       aes(x = IcesDivision,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Gurnards, Lesser-spotted dogfish and Pouting do not have sufficient data

# Atlantic cod, European hake, Lesser-spotted dogfish
ggplot(data = obs %>% 
         filter(NameEnglish %in% c("Atlantic cod","European hake", "Lesser-spotted dogfish")), 
       aes(x = IcesDivision,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)

# Other species
ggplot(data = obs %>%
         filter(NameEnglish %in% spp_list2$NameEnglish) %>%
         filter(NameEnglish != "Atlantic cod" & NameEnglish != "European hake") %>%
         filter(NameEnglish != "Lesser-spotted dogfish"), 
       aes(x = IcesDivision,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish)


# spp_list3

ggplot(data = obs %>% 
         filter(NameEnglish %in% spp_list3$NameEnglish),
       aes(x = IcesDivision,
           y = L_wt_mean)) +
  geom_boxplot() +
  facet_wrap(~ NameEnglish) 

########################################

# Length Frequency Distribution  ------------------------------------------

########################################

# Data overview
# Working data: obs
obs_len_freq <- obs_len_freq %>% 
  filter(HaulLatitude >= 48.5) %>%
  filter(NameEnglish %in% spp_list_all$NameEnglish)
  
# For loop to create figures of LDF

for (i in 1:nrow(spp_list_all)) {
  # Indicating file processing
  print(paste0("Processing file ",i,"/",nrow(spp_list_all)," - ",spp_list_all[i,2]))
  
  # Subset for number of observation
  spp_row <- obs_len_freq %>% 
    filter(NameScientific %in% spp_list_all[i,1]) %>%
    select(NameScientific, Year)
  nrow(spp_row)
  
  # Subset the data of species being mapped
  spp <- obs_len_freq %>% 
    filter(NameScientific %in% spp_list_all[i,1]) %>% 
    group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
    summarize(Number_atlength = sum(Number_atlength))
  
  ggplot(data = spp,
         aes(x = Length, 
             y = Number_atlength, 
             color = FateCategoryCode)) +
    geom_line(alpha = 2/3) +
    geom_vline(xintercept = spp$MLS, color = "red", size = 1) +
  
    ggtitle(label = "LFD", 
            subtitle = paste0(spp_list_all[i,1]," (",spp_list_all[i,2],") | ",
                              paste(range(spp_row$Year),collapse=" - "), 
                              " | ", "n = ", nrow(spp_row))) 
  
  # Save map
  ggsave(path = paste0(dir_figure,"/"),
         filename = paste0("LFD - ",spp_list_all[i,2],".png"),
         scale = 2,
         plot = last_plot(), dpi = 600,
         width = 14, height = 7, units = c("cm"))
}


########################################

# Exploration protocol ----------------------------------------------------

# Exploration following Zuur 2010 protocol  
# Protocol
# 1. Outlier Y and X (Y is response variable and X is predictor) - boxplot, cleveland dotplot
# 2. Homogeneity Y - conditional boxplot
# 3. Zero trouble (ratio has this problem?)
# 4. Collinearity X - VIR or scatter plots 
# 5. Relationship Y and X - (multipanel) scatterplots
# 6. Interaction - coplots
# 7. Independence Y - variogram

# Subset of most discarded species
obs_spp <-  obs %>% 
  filter(NameEnglish %in% spp_list$NameEnglish)


# 1. Outlier -----------------------------------------------------------------

library(lattice)
Z <- obs %>% select(DPUE, L_median, L_wt_mean, price_eurobykg)

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

# Could adopt the code from HighStat
# Dotplot to identify outlier
MyVar <- c("DPUE", "L_median", "L_wt_mean", "price_eurobykg")
Mydotplot(obs[,MyVar])


# 2. Homogeneity Y --------------------------------------------------------

# DR by year

ggplot(data = obs, mapping = aes(x = as.factor(Year), y = DR)) + 
  geom_boxplot()

# DPUE by year

ggplot(data = obs, mapping = aes(x = as.factor(Year), y = DPUE)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0,25))

hist(obs$DPUE, breaks = 500)

# Consider homogeneity Y

# 3. Zero trouble (ratio has this problem?) -----------------------------

hist(obs$DPUE, breaks = 500)

# 4. Collinearity X -----------------------------------------------------

MyVar <- c("L_median", "L_wt_mean", "price_eurobykg", "DR_length")
corvif(obs[,MyVar]) # L_median and L_wt_mean are collinear

MyVar <- c("L_wt_mean", "price_eurobykg", "DR_length")
corvif(obs[,MyVar]) # All values are under 3 - no collinearity

# 5. Relationship Y and X -------------------------------------------------
MyVar <-  c("L_median", "L_wt_mean", "price_eurobykg", "DR_length")

X11() # Graphic device

MyMultipanel.ggp2(Z = obs, 
                  varx = MyVar, 
                  vary = "DR", 
                  ylab = "DR",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)

# NA values are removed automatically

# With pearson correlation
MyVar <- c("DR", "DR_length", "L_median","L_wt_mean", "price_eurobykg")
Z <- obs %>% select(DR, DR_length, L_median, L_wt_mean, price_eurobykg)
pairs(Z,
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=MyVar)

# 6. Interaction -------------------------------------------------------------

# Does not expect interaction of price, L or DL_length

# 7. Independence ---------------------------------------------------------

# After model fitting








