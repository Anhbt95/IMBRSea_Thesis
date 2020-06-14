#       IMBRSea Thesis 
#       Tuan Anh Bui
#       25.04.2020

#       Belgian beam trawl Discard spatial analysis

#       This is the script for data processing_landing limitation and Landing limitation
#       Landing limitation was extracted from logbook data (tacsatEfalo)

#       Landing limitation of vessel (vessel_limit)
#       will be calculated based on rule and combination of conditions
#       cond_species cond_ices cond_kw start_date end_date

#       Landing limitation of trips (tirp_limit) and hauls (haul_limit) 
#       in observer data (trip_limit) will be calculated accordingly


########################################

# Load support files and packages -----------------------------------------

library(readxl)
library(tidyverse)
library(lubridate)

########################################

# Load data ---------------------------------------------------------------

# Indicate local directory
# dir_data = "C:/Users/tbui/Thesis_R/Data" # Change directory here if the local dir for data is different
dir_data = "F:/backup_to_H/R_gitlab/Thesis_Discard-spatial-analysis/Data" # Jochen
dir_data_raw <- paste(dir_data,"Data_raw", sep="/") # Jochen

# Dir in Tuan-Anh laptop
dir_data <-  "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data"
dir_data_raw <- "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data/Data_raw"

# obs_final
obs_final <- readRDS(file=paste0(dir_data,"/","obs_final_20062019_TBB_DEF_70-99.rds"))

# obs_wt weight raw data to extract fishing day 
obs_wt <- readRDS(file=paste0(dir_data_raw,"/","obs_bio_2006to2019_TBB_DEF_70-99.rds"))

# landing limit excel file
landing_limit <- read_excel(paste0(dir_data,"/","landing_limitations_subset2_processed.xlsx"))

# logbook data 
tacsat <- readRDS(file=paste0(dir_data_raw,"/","tacsatEflalo_DZinfo_2006-2019_TuanAnh.RDS"))

########################################

# Raja spp process --------------------------------------------------------

# obs_weight processed to extract weight data of Raja spp
obs_weight <- readRDS(file=paste0(dir_data,"/","obs_processed_20062019_TBB_DEF_70-99.rds"))

# Extract ray skate species
# This will be Rajidae family 
# NameScientific will be Raja spp to be linked to landing limitation data
ray_skate <- obs_weight %>% filter(str_detect(NameEnglish, "ray|skate"))

# Change Name
ray_skate$NameScientific <- "Raja spp"
ray_skate$NameEnglish <- "Raja rays nei"
ray_skate$NameDutch <- "Rog spp"
ray_skate$FaoCode <- "SKA"

ray_skate <- ungroup(ray_skate) #ungroup data

# Subset ray_skate as obs_final
colnames(obs_final)
ray_skate <- ray_skate[,which(colnames(ray_skate) %in% colnames(obs_final))] 

# Summarize D and L
values <- ray_skate %>% 
  group_by(HaulID, NameScientific) %>% 
  summarize(D = sum(D, na.rm = T),
            L = sum(L, na.rm = T)) 

# Eliminate D and L in the original data in order to squeeze data by HaulID
ray_skate <- ray_skate %>% select(-D, -L)
ray_skate$DPUE <- NA
ray_skate$DR <- NA
ray_skate <- distinct(ray_skate)

# Joint data into ray_skate and calculate DPUE, DR
ray_skate <- left_join(ray_skate, values)
ray_skate <- ray_skate %>% mutate(DPUE = D/Dur_hour,
                                  DR = D/(D+L))

ray_skate$Quarter = NA
ray_skate[which(substr(ray_skate$HaulTime,6,7) %in% c("01","02","03")),"Quarter"] = 1
ray_skate[which(substr(ray_skate$HaulTime,6,7) %in% c("04","05","06")),"Quarter"] = 2
ray_skate[which(substr(ray_skate$HaulTime,6,7) %in% c("07","08","09")),"Quarter"] = 3
ray_skate[which(substr(ray_skate$HaulTime,6,7) %in% c("10","11","12")),"Quarter"] = 4

# Transform quarter to factor
ray_skate$Quarter <- factor(ray_skate$Quarter, ordered = T)
class(ray_skate$Quarter)

# Add Month variable
ray_skate$Month <- substr(ray_skate$HaulTime,6,7)
ray_skate$Month <- as.integer(ray_skate$Month)
class(ray_skate$Month)

# Load obs_sale
obs_sale <- readRDS(file=paste0(dir_data,"/","obs_sale_processed_20062018_TBB_DEF_70-99.rds"))
obs_sale <- obs_sale %>% select(-Date)

ray_skate <- left_join(ray_skate, obs_sale)

# Add variables to ray_skate in order to bind with obs_final
ray_skate <- ray_skate %>% mutate(MLS = NA,
                                  LL = NA,
                                  DL = NA,
                                  DR_length = NA,
                                  L_median = NA,
                                  L_wt_mean = NA
)
# Eliminate ray/skate values in obs_final to add summarize data from ray_skate
obs_final2 <- obs_final[- which(str_detect(obs_final$NameEnglish, "ray|skate")),]
obs_final2 <- as_tibble(rbind(obs_final2,ray_skate))

obs_final <- obs_final2

########################################

# tacsat processing -------------------------------------------------------

head(tacsat)
colnames(tacsat)
tacsat <- tacsat[,-(11:12)] #Remove duplicate LE_KG_RJM LE_KG_RJN

tacsat <- tacsat %>% mutate(LE_KG_SKA = LE_KG_RJB + LE_KG_RJC + LE_KG_RJE + LE_KG_RJF + LE_KG_RJH +
                              LE_KG_RJI + LE_KG_RJK + LE_KG_RJM + LE_KG_RJN + LE_KG_RJR + LE_KG_RJU +
                              LE_KG_SKA)
colnames(tacsat)
tacsat <- tacsat[,-(19:29)]

#tacsat2 <- tacsat
head(tacsat)

tacsat <- tacsat %>% pivot_longer(cols = LE_KG_COD:LE_KG_PLE, names_to = "NameScientific",
                                  values_to = "L")
tacsat_raw <- tacsat
sort(unique(tacsat$div))

colnames(tacsat)
str(tacsat)

# TripID is from logbook data
# D1_TripID is from observer data
# Processing should be done with TripID, then joint to observer data by D1_TripID

# Extract date by trip ----------------------------------------------------
unique(tacsat$TripID)

# Check vessel
str(tacsat)
str(tacsat_landing)
tacsat$TotalKw <- as.numeric(tacsat$TotalKw)
tacsat$div <- as.character(tacsat$div)
tacsat$TripID <- as.character(tacsat$TripID)
tacsat$SI_DATE <- lubridate::date(tacsat$SI_DATIM)
# Remove NA
tacsat <-  tacsat %>% filter(is.na(div) == F)
unique(tacsat$TripID) # More than 16,000 trip
# but we only need trips in year that vessel (in obs data) operates 
vessel_check <- obs_final %>% group_by(VesselCode, Year) %>% summarize(n = n()) 

#df to extract 
tacsat_sub <- tacsat[0,]

vessel <- unique(vessel_check$VesselCode) #vessel 

# Run by vessel > year in vessel check
for ( i in 1:length(vessel)) { #length(vessel)
  print(paste("Processing file",i,"/",length(vessel),"-",vessel[i],"data",sep=" "))
  
  vessel_check_sub <- vessel_check %>% filter(VesselCode == vessel[i])
  df_temp <- tacsat %>% filter(VesselCode == vessel[i], 
                               SI_YEAR %in% unique(vessel_check_sub$Year))
  tacsat_sub <- as_tibble(rbind(tacsat_sub, df_temp, stringsAsFactors = F))
  
  rm(df_temp)
  
}

saveRDS(tacsat_sub, file=paste0(dir_data,"/","tacsatEflalo_sub_DZinfo_2006-2019_TuanAnh.rds"))
unique(tacsat_sub$TripID)
# We reduced a lot of trips that we dont need information


# for loop to calculate fishing day at each ICES Division
# each trip will be extracted and calculate for FishingDay at each ICES Division 

obs_trip <- tibble(SI_YEAR = numeric(),
                   VesselCode = character(),
                   FleetSegment = character(),
                   TotalKw = numeric(),
                   div = character(),
                   TripID = character(),
                   NameScientific = character(),
                   FishingDay = numeric(),
                   DepartureDateShort = date(), 
                   ReturnDateShort = date()
)

trip <- unique(tacsat_sub$TripID) #trip from obs_final

for (i in 1:length(trip)) {
  
  print(paste("Processing file",i,"/",length(trip),"-",trip[i],"data",sep=" "))
  
  obs_trip_sub <- tacsat_sub %>% filter(TripID == trip[i])
  
  ices_div <- unique(obs_trip_sub$div)
  
  #DepartureDateShort <- min(obs_trip_sub$HaulDate)
  #ReturnDateShort <- max(obs_trip_sub$HaulDate)
  
  for (k in 1:length(ices_div)) { #length(ices_div)
    # ICES Division
    
    print(paste("Processing file",k,"/",length(ices_div),"-",ices_div[k],"data",sep=" "))
    
    obs_ices <- obs_trip_sub %>% filter(div == ices_div[k])
    FishingDay <- as.numeric(max(obs_ices$SI_DATE) - min(obs_ices$SI_DATE) + 1) 
    DepartureDateShort <- min(obs_ices$SI_DATE)
    ReturnDateShort <- max(obs_ices$SI_DATE)
    
    df_temp <- obs_ices %>% 
      group_by(SI_YEAR, VesselCode, FleetSegment, TotalKw, div, TripID, NameScientific) %>%
      summarize(n = n()) %>% select(-n)
    df_temp <- ungroup(df_temp)
    df_temp$FishingDay <- FishingDay
    df_temp$DepartureDateShort <- DepartureDateShort
    df_temp$ReturnDateShort <- ReturnDateShort
    
    obs_trip <- as_tibble(rbind(obs_trip, df_temp, stringsAsFactors = F))
    rm(df_temp, FishingDay, DepartureDateShort, ReturnDateShort)
  }
}

# Vessel, trip, 

# Calculate at trip basis 
# Then the landing limitation will be summarized by group of rule period
# Weigth will be the same, but fishing day will need some more process

tacsat_landing <- tacsat_sub %>% group_by(SI_YEAR, VesselCode, FleetSegment,
                                          TotalKw, div, TripID, NameScientific) %>%
  summarize(L = sum(L, na.rm = T))
tacsat_landing <- ungroup(tacsat_landing)

# Transform variables of tacsat_landing
tacsat_landing$TotalKw <- as.numeric(tacsat_landing$TotalKw)
tacsat_landing$div <- as.character(tacsat_landing$div)
tacsat_landing$TripID <- as.character(tacsat_landing$TripID)

obs_trip_raw <- obs_trip

# Join L and D1_TripID
obs_trip <- left_join(obs_trip, tacsat_landing)
Trip_info$TripID <- as.character(Trip_info$TripID)
obs_trip <- left_join(obs_trip, Trip_info)

# Convert div
sort(unique(obs_trip$div))
obs_trip[which(obs_trip$div == "IIIa"),"div"] = "3A"
obs_trip[which(obs_trip$div == "IVa"),"div"] = "4A"
obs_trip[which(obs_trip$div == "IVb"),"div"] = "4B"
obs_trip[which(obs_trip$div == "IVc"),"div"] = "4C"
obs_trip[which(obs_trip$div == "VIa"),"div"] = "6A"
obs_trip[which(obs_trip$div == "VIIa"),"div"] = "7A"
obs_trip[which(obs_trip$div == "VIIb"),"div"] = "7B"
obs_trip[which(obs_trip$div == "VIIc2"),"div"] = "7C"
obs_trip[which(obs_trip$div == "VIId"),"div"] = "7D"
obs_trip[which(obs_trip$div == "VIIe"),"div"] = "7E"
obs_trip[which(obs_trip$div == "VIIf"),"div"] = "7F"
obs_trip[which(obs_trip$div == "VIIg"),"div"] = "7G"
obs_trip[which(obs_trip$div == "VIIh"),"div"] = "7H"
obs_trip[which(obs_trip$div == "VIIIa"),"div"] = "8A"
obs_trip[which(obs_trip$div == "VIIIb"),"div"] = "8B"
obs_trip[which(obs_trip$div == "VIIId2"),"div"] = "8D"
obs_trip[which(obs_trip$div == "VIIj2"),"div"] = "7J"
sort(unique(obs_trip$div)) #Fine

# Change name species # Could extract from excel file (might be less manual)
unique(obs_trip$NameScientific)
obs_trip[which(obs_trip$NameScientific == "LE_KG_ANF"),"NameScientific"] = "Lophius spp"
obs_trip[which(obs_trip$NameScientific == "LE_KG_BIB"),"NameScientific"] = "Trisopterus luscus"
obs_trip[which(obs_trip$NameScientific == "LE_KG_COD"),"NameScientific"] = "Gadus morhua"
obs_trip[which(obs_trip$NameScientific == "LE_KG_DAB"),"NameScientific"] = "Limanda limanda"
obs_trip[which(obs_trip$NameScientific == "LE_KG_HAD"),"NameScientific"] = "Melanogrammus aeglefinus"
obs_trip[which(obs_trip$NameScientific == "LE_KG_HKE"),"NameScientific"] = "Merluccius merluccius"
obs_trip[which(obs_trip$NameScientific == "LE_KG_LEM"),"NameScientific"] = "Microstomus kitt"
obs_trip[which(obs_trip$NameScientific == "LE_KG_PLE"),"NameScientific"] = "Pleuronectes platessa"
obs_trip[which(obs_trip$NameScientific == "LE_KG_SKA"),"NameScientific"] = "Raja spp"
obs_trip[which(obs_trip$NameScientific == "LE_KG_SOL"),"NameScientific"] = "Solea solea"
obs_trip[which(obs_trip$NameScientific == "LE_KG_WHG"),"NameScientific"] = "Merlangius merlangus"
sort(unique(obs_trip$NameScientific))

sort(unique(landing_limit$cond_species))

########################################

# Conditional check -------------------------------------------------------


# Activity in 8a_8b in year of limitation ---------------------------------

View(
  obs_trip %>% group_by(Year, VesselCode, IcesDivision) %>% summarize(n = n()) %>%
    spread(key = IcesDivision, value = n)
)
# Majority of vessel are not active in 8a_8b - conditional rule will be 0 (no)


# Activity in 4a_4b_4c and 7d within a trip -------------------------------

# Only occurs in 2015-2018 for Solea solea and Pleuronectes platessa

View(
  obs_trip %>% filter(Year >= 2015, 
                      NameScientific %in% c("Solea solea", "Pleuronectes platessa"),
                      IcesDivision %in% c("4A","4B","4C","7D")) %>% 
    group_by(Year, TripID, IcesDivision) %>% summarize(n = n()) %>%
    spread(key = IcesDivision, value = n)
)

cond_act1 <- obs_trip %>% filter(Year >= 2015, 
                                 NameScientific %in% c("Solea solea", "Pleuronectes platessa"),
                                 IcesDivision %in% c("4A","4B","4C","7D")) %>% 
  group_by(Year, TripID, IcesDivision) %>% summarize(n = n()) %>%
  spread(key = IcesDivision, value = n)

nrow(cond_act1) #87
nrow(cond_act1 %>% filter(is.na(`4B`) == F & is.na(`7D`) == F)) #0
nrow(cond_act1 %>% filter(is.na(`4C`) == F & is.na(`7D`) == F)) #14
nrow(cond_act1 %>% filter(is.na(`4C`) == F & is.na(`4B`) == F & is.na(`7D`) == F)) #0

# Activity in 4a_4b_4c and 7d within a trip is minor - conditional the rule will be 0 (no)


# Activity in 4a_4b_4c and 7d_7e within a trip ----------------------------
# Only occurs in 2018 for Pleuronectes platessa

View(
  obs_trip %>% filter(Year == 2018, 
                      NameScientific %in% c("Pleuronectes platessa"),
                      IcesDivision %in% c("4A","4B","4C","7D","7E")) %>% 
    group_by(Year, TripID, IcesDivision) %>% summarize(n = n()) %>%
    spread(key = IcesDivision, value = n)
)

# Activity in 4a_4b_4c and 7d_7e within a trip is minor - conditional the rule will be 0 (no)


# Activity in 7e  and 7f_7g within a trip ---------------------------------
# Only occurs in 2018 for Solea solea

View(
  obs_trip %>% filter(Year == 2018, 
                      NameScientific %in% c("Solea solea"),
                      IcesDivision %in% c("7E","7F","7G")) %>% 
    group_by(Year, TripID, IcesDivision) %>% summarize(n = n()) %>%
    spread(key = IcesDivision, value = n)
)

# 4/8 trips do not have activity - conditional rule will be 0 (no)


# Trip in June ------------------------------------------------------------
# Only occurs in 2016 for Pleuronectes platessa, 4a_4b_4c

View(
  obs_trip %>% filter(Year == 2016, 
                      NameScientific %in% c("Pleuronectes platessa"),
                      month(DepartureDateShort) == 6,
                      IcesDivision %in% c("4A","4B","4C")) %>% 
    group_by(Year, TripID, IcesDivision) %>% summarize(n = n()) %>%
    spread(key = IcesDivision, value = n)
)

# No data - conditional rule will be 0 (no)


# min80fishingdays_in_7a_in_2008_2009 -------------------------------------

View(
  obs_trip %>% filter(Year %in% c(2008,2009), 
                      NameScientific %in% c("Solea solea"),
                      IcesDivision %in% c("7A")) %>% 
    group_by(Year, VesselCode, IcesDivision) %>% summarize(FishingDay = sum(FishingDay))
)

# Dont know what it means by min80fishingdays 
# Rationale: 0 - less than 80 days, 1 - more than 80 days 
# but 1 has more fishing allowance than 0 (strange, should be the other way around)
# Based on data
# 1 - less fishing - more allowance and vice versa for 0 - more fishing
# related to request sent (will be considered later)


# min30fishingdays_in_7a --------------------------------------------------

View(
  obs_trip %>% filter(Year %in% c(2011,2012), 
                      NameScientific %in% c("Solea solea"),
                      IcesDivision %in% c("7A")) %>% 
    group_by(VesselCode, IcesDivision) %>% summarize(FishingDay = sum(FishingDay))
)


# Similar to min80 rule, the intuition is not clear
# Based on the legislation, it is assumed that 
# 0 - more than 30 days, less fishing allowance
# 1 - less than 30 days, more fishing allowance

# 2/3 Vessel has less than 30 days, the conditional rule will be 1 (yes)
# the associated landings_equalto_or_more_than_75tonlanding_between01012011_and_31122012 
# based on the check for less than 75 ton, will be 0 (no)

obs_wt$WeightTotalBothSides <- as.numeric(obs_wt$WeightTotalBothSides)
View(
  obs_wt %>% filter(Year %in% c(2011,2012), 
                    NameScientific %in% c("Solea solea"),
                    IcesDivision %in% c("7A"),
                    VesselCode %in% c("O.15","O.231","O.89") 
  ) %>%
    group_by(VesselCode, IcesDivision) %>% summarize(WeightTotalBothSides = sum(WeightTotalBothSides, na.rm = T))
)



# vessel_specific_limitations ---------------------------------------------

# Z548, Z333
View(
  obs_trip %>% filter(Year %in% c(2013), 
                      NameScientific %in% c("Solea solea"),
                      IcesDivision %in% c("7A"),
                      VesselCode %in% c("Z.548","Z.333") ) %>% 
    group_by(VesselCode, TripID, DepartureDateShort) %>% summarize(n = n())
)

# Z.548 in the data only has records in April and May - choose the rule for all vessesl


# O231_Z483_Z548_Z576
View(
  obs_trip %>% filter(Year %in% c(2014, 2015), 
                      NameScientific %in% c("Solea solea"),
                      IcesDivision %in% c("7A"),
                      VesselCode %in% c("O.231","Z.483","Z.548","Z.576") ) %>% 
    group_by(Year, VesselCode, TripID) %>% summarize(FishingDay = sum(FishingDay))
)

# Assuming only_valid_for_first_trip_in_icesdivision is 0 (no)
# the vessel specific rule will be the same as for all vessel


# Possible to send request ------------------------------------------------

# 1 - Yes - maximize profit
# Besides, request sent is linked to activity_min80fishingdays_in_7a and activity_min15fishingdays_in_7a
# Request sent 1 (yes) is as acitivy_min 1 (b) for both conditions. 

########################################

# Extract landing limitation from data ------------------------------------

# Should be split to Raja (all raja) and no raja

# Adjust landing_limit species name (i.e. joint name is splited)
landing_limit[which(landing_limit$cond_species == "Limanda limanda Platichthys flesus" ),"cond_species"] = "Limanda limanda"
landing_limit[which(landing_limit$cond_species == "Microstomus kitt Glyptocephalus cynoglossus" ),"cond_species"] = "Microstomus kitt"

# Change limit_adaptation and multiplier to numeric
landing_limit$limit_addition <- as.numeric(landing_limit$limit_addition)
landing_limit$limit_multiplier <- as.numeric(landing_limit$limit_multiplier)

#Adjust FleetSegment of obs_trip to be the same as cond_kw of landing_limit
obs_trip$cond_kw = NA
obs_trip[which(obs_trip$FleetSegment == "<= 221 kW" ),"cond_kw"] = "<=221"
obs_trip[which(obs_trip$FleetSegment == "> 221 kW" ),"cond_kw"] = ">221"


# For loop to extract Landing limitation ----------------------------------

# Species > Year > IcesDivision > FleetSegment > Rule period 

#landing_limit <- landing_limit %>% filter(cond_species != "Raja montagui", cond_species != "Leucoraja naevus")
#species <- unique(landing_limit$cond_species)
#species

df <- tibble(Year = integer(),
             TripID = character(),
             FleetSegment = character(),
             div = character(),
             Power = numeric(),
             NameScientific = character(),
             DepartureDateShort = lubridate::date(),
             ReturnDateShort = lubridate::date(),
             FishingDay = numeric(),
             cond_kw = character(),
             Landing_limit = numeric(),
             limit_type = character())

# Set of species based on landing_limit data
species <- unique(landing_limit$cond_species)
species
# Could use seq_along() instead of 1:length(), particularly when index is not sequential or the same as length

for (i in 1:length(species)) { #length(species)
  
  print(paste("Processing file",i,"/",length(species),"-",species[i],"data",sep=" "))
  
  # Species
  limit_spp <- landing_limit %>% filter(cond_species == species[i]) 
  obs_spp <- obs_trip %>% filter(NameScientific == species[i])
  
  year <- unique(limit_spp$cond_year)
  
  for (j in 1:length(year)) { #length(year)
    
    print(paste("Processing file",j,"/",length(year),"-",year[j],"data",sep=" "))
    
    # Year
    limit_year <- limit_spp %>% filter(cond_year == year[j])
    obs_year <- obs_spp %>% filter(SI_YEAR == year[j])
    
    if(nrow(limit_year) == 0 | nrow(obs_year) == 0) { #ICES_Div does not have in rule - but problem for 1
      next
    }
    
    ices_div <- unique(obs_year$div)
    
    for (k in 1:length(ices_div)) { #length(ices_div)
      # ICES Division
      
      print(paste("Processing file",k,"/",length(ices_div),"-",ices_div[k],"data",sep=" "))
      
      limit_ices <- limit_year %>% filter(str_detect(cond_ices_div, regex(ices_div[k], ignore_case = T)))
      obs_ices <- obs_year %>% filter(div == ices_div[k])
      
      if(nrow(limit_ices) == 0 | nrow(obs_ices) == 0) { #ICES_Div does not have in rule - but problem for 1
        next
      }
      
      fleet_segment <- unique(obs_ices$cond_kw)
      
      for (l in 1:length(fleet_segment)) { #length(fleet_segment)
        
        #print(paste("Processing file",l,"/",length(fleet_segment),"-",fleet_segment[l],"data",sep=" "))
        
        limit_kw <- limit_ices %>% filter(cond_kw %in% c(fleet_segment[l],"all"))
        obs_kw <- obs_ices %>% filter(cond_kw == fleet_segment[l])
        
        if(nrow(limit_kw) == 0 | nrow(obs_kw) == 0) { #fleet_segment does not have in rule
          next
        }
        
        start_date <- limit_kw$cond_date_start_legislation_incldate
        end_date <- limit_kw$cond_date_end_legislation_DeFacto_excldate
        
        for (m in 1:length(start_date)) { #length(start_date)
          
          #print(paste("Processing file",m,"/",length(start_date),"-",start_date[m],"data",sep=" "))
          
          limit_sub <- limit_kw %>% filter(cond_date_start_legislation_incldate == start_date[m] &
                                             cond_date_end_legislation_DeFacto_excldate == end_date[m])
          obs_sub <- obs_kw %>% filter(DepartureDateShort >= start_date[m] & DepartureDateShort < end_date[m])
          
          if(nrow(limit_sub) == 0 | nrow(obs_sub) == 0) { #If the fishing date is not in the rule period, then no data
            next
          }
          
          df_temp <- obs_sub
          
          df_temp$Landing_limit <- NA
          ifelse(limit_sub$limit_type == "weight_by_kw", df_temp$Landing_limit <-  limit_sub$limit_addition + limit_sub$limit_multiplier*df_temp$TotalKw,
                 ifelse(limit_sub$limit_type == "fishingday_limit", df_temp$Landing_limit <-  limit_sub$limit_multiplier,
                        ifelse(limit_sub$limit_type == "total_vessel_limit", df_temp$Landing_limit <-  limit_sub$limit_addition,
                               ifelse(limit_sub$limit_type == "closing_fishing", df_temp$Landing_limit <- 0, 
                                      df_temp$Landing_limit <- NA
                               )
                        )
                 )
          )
          df_temp$limit_type <- NA
          ifelse(limit_sub$limit_type == "weight_by_kw", df_temp$limit_type <-  "weight_by_kw",
                 ifelse(limit_sub$limit_type == "fishingday_limit", df_temp$limit_type <-  "fishingday_limit",
                        ifelse(limit_sub$limit_type == "total_vessel_limit", df_temp$limit_type <-  "total_vessel_limit",
                               ifelse(limit_sub$limit_type == "closing_fishing", df_temp$limit_type <-  "closing_fishing", 
                                      df_temp$limit_type <- NA
                               )
                        )
                 )
          )
          df <- as_tibble(rbind(df, df_temp, stringsAsFactors = F)) 
          rm(df_temp)
        }
      }
    }
  }
}
distinct(df) #59912


# Processing df -----------------------------------------------------------

# Landing limit in df was calculated by trip basis
# Landing limit values represents the landing limit of a vessel in a period
# exepct for fishingday_limit, which is the multiplier factor
# Real landing limit for vessel in period will need to be calculate as below

# Group vessel by start date and end date, landing limit = fishing day*multiplier(Landing_limit)

# First we separate df by limit type (fishing day and the other)
df <- df %>% select(- L, - D1_TripID)
unique(df$limit_type)
df_fishingday <- df %>% filter(limit_type == "fishingday_limit")
df_other <- df %>% filter(limit_type != "fishingday_limit" | is.na(limit_type) == T)

#df_other can be used directly


# Vessel
# Species > Year > IcesDivision > FleetSegment > Rule period 
unique(df_other$Landing_limit)



df_vessel <- df %>% filter(VesselCode == "B.462", NameScientific == "Raja spp", SI_YEAR == 2008, div == "4B", FleetSegment == "> 221 kW")
df_temp <- df_vessel  %>% 
  group_by(SI_YEAR, VesselCode, FleetSegment, div, NameScientific, limit_type) %>%
  summarize(n = n()) %>% select(-n)

FishingDay <- as.numeric(max(df_vessel$ReturnDateShort) - min(df_vessel$DepartureDateShort) + 1) 
DepartureDateShort <- min(df_vessel$DepartureDateShort)
ReturnDateShort <- max(df_vessel$ReturnDateShort)

df_temp <- ungroup(df_temp)
df_temp$FishingDay <- FishingDay
df_temp$DepartureDateShort <- DepartureDateShort
df_temp$ReturnDateShort <- ReturnDateShort
df_temp$Landing_limit <- unique(df_vessel$Landing_limit)*FishingDay


df_trip <- tibble(SI_YEAR = numeric(),
                  VesselCode = character(),
                  FleetSegment = character(),
                  TotalKw = numeric(),
                  div = character(),
                  TripID = character(),
                  NameScientific = character(),
                  FishingDay = numeric(),
                  DepartureDateShort = date(), 
                  ReturnDateShort = date()
)

# Vessel
# Species > Year > IcesDivision > FleetSegment > Rule period 
unique(df_other$Landing_limit)

df_vessel <- df %>% filter(VesselCode == "B.462", NameScientific == "Raja spp", SI_YEAR == 2008, div == "4B", FleetSegment == "> 221 kW")


df_fishingday_sum <- df_fishingday[0,]

vessel <- unique(df_fishingday$VesselCode) #trip from obs_final

for (a in 1:length(vessel)) { #length(vessel)
  
  print(paste("Processing file",a,"/",length(vessel),"-",vessel[a],"data",sep=" "))
  
  df_vessel <- df_fishingday %>% filter(VesselCode == vessel[a])
  
  species <- unique(df_vessel$NameScientific)
  
  for (i in 1:length(species)) { #length(species)
    
    print(paste("Processing file",i,"/",length(species),"-",species[i],"data",sep=" "))
    
    # Species
    df_spp <- df_vessel %>% filter(NameScientific == species[i])
    
    year <- unique(df_spp$SI_YEAR)
    
    for (j in 1:length(year)) { #length(year)
      
      print(paste("Processing file",j,"/",length(year),"-",year[j],"data",sep=" "))
      
      # Year
      df_year <- df_spp %>% filter(SI_YEAR == year[j])
      
      ices_div <- unique(df_year$div)
      
      for (k in 1:length(ices_div)) { #length(ices_div)
        # ICES Division
        
        print(paste("Processing file",k,"/",length(ices_div),"-",ices_div[k],"data",sep=" "))
        
        df_ices <- df_year %>% filter(div == ices_div[k])
        
        fleet_segment <- unique(df_ices$cond_kw)
        
        for (l in 1:length(fleet_segment)) { #length(fleet_segment)
          
          #print(paste("Processing file",l,"/",length(fleet_segment),"-",fleet_segment[l],"data",sep=" "))
          
          df_kw <- df_ices %>% filter(cond_kw == fleet_segment[l])
          
          multiplier <- unique(df_kw$Landing_limit) 
          
          for (m in 1:length(multiplier)) { #length(multiplier)
            
            df_limit <- df_kw %>% filter(Landing_limit == multiplier[m])
            
            FishingDay <- sum(df_limit$FishingDay)
            
            
            df_temp <- df_limit  
            #%>% 
            #  group_by(SI_YEAR, VesselCode, FleetSegment, TotalKw, div, NameScientific, limit_type, cond_kw) %>%
            #  summarize(FishingDay = sum(FishingDay, na.rm = T)) 
            
            #df_temp$DepartureDateShort <- min(df_limit$DepartureDateShort)
            #df_temp$ReturnDateShort <- max(df_limit$ReturnDateShort)
            
            #df_temp <- ungroup(df_temp)
            
            df_temp$Landing_limit <- multiplier[m]*FishingDay
            
            df_fishingday_sum <- as_tibble(rbind(df_fishingday_sum, df_temp))
            
          }
          
        }
      }
    }
  }
} 

# Several cases will be duplicated in day but that is unavoidable
View(
  df_fishingday %>% filter(VesselCode == "B.462", NameScientific == "Limanda limanda", div == "4B", SI_YEAR == "2008")
)

# Extract Landing_limit by trip - this will be hereby called vessel_limit (sum of landing limit for a vessel)

df_other$vessel_limit <- df_other$Landing_limit
df_other_sub <- df_other %>% select(SI_YEAR:NameScientific, vessel_limit, limit_type)

df_fishingday_sum$vessel_limit <- df_fishingday_sum$Landing_limit
df_fishingday_sub <- df_fishingday_sum %>% select(SI_YEAR:NameScientific, vessel_limit, limit_type)

df_all <- as_tibble(rbind(df_other_sub, df_fishingday_sub))

# Join obs_trip with landing limitation rules (df_all) -------------------------
obs_trip2 <- obs_trip

obs_trip <- left_join(obs_trip,df_all)
# obs_trip has vessel_limit for each vessel
# next step is to calculate landing_limit for each trip
# Culmulative landing of trip will be calculated
# Then vessel_limit (sum of limit) will substract culmulative landing by trip to get
# trip_limit (landing limit by trip)

# trip_limit = vessel_limit - culmulative L

# Year, 

df <- obs_trip[0,]

vessel <- unique(obs_trip$VesselCode) #trip from obs_final

for (a in 1:length(vessel)) { #length(vessel)
  
  print(paste("Processing file",a,"/",length(vessel),"-",vessel[a],"data",sep=" "))
  
  df_vessel <- obs_trip %>% filter(VesselCode == vessel[a])
  
  species <- unique(df_vessel$NameScientific)
  
  for (i in 1:length(species)) { #length(species)
    
    print(paste("Processing file",i,"/",length(species),"-",species[i],"data",sep=" "))
    
    # Species
    df_spp <- df_vessel %>% filter(NameScientific == species[i])
    
    year <- unique(df_spp$SI_YEAR)
    
    for (j in 1:length(year)) { #length(year)
      
      #print(paste("Processing file",j,"/",length(year),"-",year[j],"data",sep=" "))
      
      # Year
      df_year <- df_spp %>% filter(SI_YEAR == year[j])
      
      ices_div <- unique(df_year$div)
      
      for (k in 1:length(ices_div)) { #length(ices_div)
        # ICES Division
        
        #print(paste("Processing file",k,"/",length(ices_div),"-",ices_div[k],"data",sep=" "))
        
        df_ices <- df_year %>% filter(div == ices_div[k])
        
        fleet_segment <- unique(df_ices$cond_kw)
        
        for (l in 1:length(fleet_segment)) { #length(fleet_segment)
          
          #print(paste("Processing file",l,"/",length(fleet_segment),"-",fleet_segment[l],"data",sep=" "))
          
          df_kw <- df_ices %>% filter(cond_kw == fleet_segment[l])
          
          limit <- unique(df_kw$vessel_limit) 
          
          for (m in 1:length(limit)) { #length(multiplier)
            
            # arrange
            
            df_limit <- df_kw %>% filter(vessel_limit == limit[m]) %>% arrange(DepartureDateShort, ReturnDateShort)
            
            
            df_temp <- df_limit %>% mutate(L_cumsum_trip = cumsum(L),
                                           trip_limit = vessel_limit - L_cumsum_trip)
            df <- as_tibble(rbind(df, df_temp))
            
          }
          
        }
      }
    }
  }
} 


# Save obs_trip with full information (both trip logbook and trip obs)
saveRDS(obs_trip, paste0(dir_data,"/","obs_logbook_Landing_limit.rds"))

obs_trip_full <- obs_trip

obs_trip <- obs_trip_full %>% filter(is.na(D1_TripID) == F)

obs_trip$TripID <- obs_trip$D1_TripID
colnames(obs_trip)[1] <- "Year"
colnames(obs_trip)[5] <- "IcesDivision"
obs_trip <- obs_trip %>% select(- (L:cond_kw))


# Join obs_final and obs_trip
# Note obs_final - angler fish are recorded as "Lophius budegassa", "Lophius piscatorius"
# while in obs_trip it is Lophius spp
# However, there is no rule for Lophius spp
Lophius_spp <- obs_trip %>% filter(NameScientific == "Lophius spp")
summary(Lophius_spp$trip_limit) #NA

# Join obs_final (with ray) and obs_trip
obs_final <- left_join(obs_final, obs_trip)

View(
  obs_final %>% filter(NameScientific == "Raja spp")
)

# in 2014, 2015, Solea solea at 7A by certain vessels
# whose have vessel_specific_limitations conditional rules

View(
  obs_final %>% filter(VesselCode %in% c("O.231","Z.483","Z.548","Z.576"), 
                       Year %in% c(2014,2015), IcesDivision == "7A", 
                       NameScientific == "Solea solea") %>%
    select(VesselCode, TripID, NameScientific, IcesDivision, L, vessel_limit, L_cumsum_trip, trip_limit)
)

# The pragmatic choice was given to the rule of cond_only_valid_for_first_trip_in_icesdivision - 0 (no)
# but it turns out that those recorded trips were first trip, so the rule for 1 (yes) should be given

# Set 2014, 2015, Solea solea 7A, Vessel "O.231","Z.483","Z.548","Z.576" to 5000 (kg)

obs_final$vessel_limit <- if_else(obs_final$VesselCode %in% c("O.231","Z.483","Z.548","Z.576") & 
                                    obs_final$Year %in% c(2014,2015) & obs_final$IcesDivision == "7A" & obs_final$NameScientific == "Solea solea", 
                                  5000, 
                                  obs_final$vessel_limit)

obs_final$trip_limit <- if_else(obs_final$VesselCode %in% c("O.231","Z.483","Z.548","Z.576") & 
                                  obs_final$Year %in% c(2014,2015) & obs_final$IcesDivision == "7A" & obs_final$NameScientific == "Solea solea", 
                                obs_final$vessel_limit - obs_final$L_cumsum_trip, 
                                obs_final$trip_limit)

# Calculate haul_limit ---------------------------------------------

# haul_limit is is the biomass allowed to be landed at each haul (landing limit at each haul)
# haul_limit = trip_limit - cumsum_haul

# Processing
obs_final$HaulTime <- as_datetime(obs_final$HaulTime) #Convert HaulTime to datetime

# for loop to calculate landing allowance
# the culmulative sum will be calculated based on the premise that landing limit is set by species by Ices_div
# The procedure for the loop:
# extract trip, extract each species in the trip, 
# extract each Ices_div in each species, calculate culmulative sum and landing allowance

obs_final_sub <- obs_final %>% group_by(TripID, NameScientific, IcesDivision) %>%
  select(TripID, NameScientific, IcesDivision, HaulTime, L, trip_limit) %>%
  arrange(NameScientific, IcesDivision, HaulTime) 
obs_final_sub <- ungroup(obs_final_sub) #subset of data to reduce computing time

# Can use obs_final directly but the work takes more time as there are more var: df <- obs_final[0,]

df <- tibble(TripID = character(),
             NameScientific = character(),
             IcesDivision = character(),
             HaulTime = NA,
             L = numeric(),
             haul_limit = numeric(),
             L_cumsum = numeric(),
             Landing_allowance = numeric()
)

trip <- unique(obs_final_sub$TripID)

for (i in 1:length(trip)) { #length(trip)
  
  print(paste("Processing file",i,"/",length(trip),"-",trip[i],"data",sep=" "))
  
  obs_trip_sub <- obs_final_sub %>% filter(TripID == trip[i])
  
  species <- unique(obs_trip_sub$NameScientific)
  
  for (j in 1:length(species)) { #length(species)
    
    print(paste("Processing file",j,"/",length(species),"-",species[j],"data",sep=" "))
    
    obs_spp <- obs_trip_sub %>% filter(NameScientific == species[j])
    
    ices_div <- unique(obs_spp$IcesDivision)
    
    for (k in 1:length(ices_div)) { #length(ices_div)
      # ICES Division
      
      obs_ices <- obs_spp %>% filter(IcesDivision == ices_div[k])
      
      df_temp <- obs_ices %>% mutate(L_cumsum = cumsum(L),
                                     haul_limit = trip_limit - L_cumsum)
      df <- as_tibble(rbind(df, df_temp))
      
      #rm(df_temp)
    }
  }
}

#trip > species > ices division

obs_final <- left_join(obs_final, df)


# Quota utilization -------------------------------------------------------

# Quota utilization (of each haul) = what has been catch/quota 
# Quota_uti = (vessel_limit - haul_limit)/vessel_limit

# Quota utilization is info at each haul that how much thay have caught of a species
# and might affect the decision of discarding (high grading)

# Quota utilization from 0 to 1 (0 - 100%) or higher than 1 (> 100%) meaning over quota

# Interpretation of quota utilization
# high quota utilization, mean low landing limit, and might lead to higher discard ratios
# (as fishers want to increase benefit by discarding)

# If vessel_limit = 0 (closing fishing), haul_limit < 0 (still fishing)
# then Quota_uti = 1 (might be considered for some species as haul_limit varies up to - 5000)

# If vessel_limit = 0 (closing fishing), haul_limit = 0 (no fishing)
# then Quota_uti = 0

obs_final <- obs_final %>% 
  mutate(
    Quota_uti = if_else(vessel_limit == 0 & haul_limit < 0, 1,
                        if_else(vessel_limit == 0 & haul_limit == 0, 0,
                               (vessel_limit - haul_limit)/vessel_limit))
  )


# Visualization distribution of quota utilization
# Check Quota for selected species
spp <- obs_final %>% filter(is.na(haul_limit) == F)

library(ggridges)
ggplot(spp, aes(x = Quota_uti, y = NameEnglish, fill = NameEnglish)) +
  geom_density_ridges() + #stat="binline", bins=100
  theme_ridges() + 
  theme(legend.position = "none")

# Quota_uti vs DR in selected species (species with quota)
ggplot(data = spp, aes(x = Quota_uti, y = DR)) + 
  geom_point() + geom_smooth() + facet_wrap(~ NameEnglish)

ggplot(data = spp %>% filter(NameEnglish %in% c("European hake", 
                                                "Haddock", "Raja rays nei", "Whiting")), 
       aes(x = Quota_uti, y = DR)) + 
  geom_point() + geom_smooth() + facet_wrap(~ NameEnglish)

ggplot(data = spp %>% filter(NameEnglish == "Common dab"), aes(x = Quota_uti, y = DR)) +
  geom_point() + geom_smooth() + facet_wrap(~ NameEnglish)
# Note - several species (for example dab), NA vessel_limit (~ NA haul_limit)
# can be considered as 0 quota utilization (as no quota for these species)

# Problem - closing_fishing -> quota = infinite, need to set a value 
# suppose 1? But could not express the difference between 
# haul_limit = -1kg and -5000kg (though the effects would be different)

closing_fishing <- obs_final %>% filter(limit_type == "closing_fishing")

ggplot(data = closing_fishing, aes(x = haul_limit, y = DR)) + 
  geom_point() + facet_wrap(~ NameEnglish)

# Check European plaice and Atlantic cod
View(
  obs_final %>% filter(limit_type == "closing_fishing", haul_limit < 0,
                       NameEnglish %in% c("European plaice", "Atlantic cod")) %>%
  select(NameEnglish, haul_limit) %>% arrange(NameEnglish)
)

# Save data
saveRDS(obs_final, paste0(dir_data, "/", "obs_final_quota_20062019_TBB_DEF_70-99.rds"))

