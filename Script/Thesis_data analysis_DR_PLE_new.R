#       IMBRSea Thesis 
#       Tuan Anh Bui
#       26.04.2020

#       Belgian beam trawl Discard spatial analysis

#       This is the script for data analysis - spatial model 
#       The script is adopted from Zuur 2017 and M. Pennino 

#       This script is a compact version for single species analysis
#       fully explanation of each function is given in the script Thesis_data analysis_spatial model

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
library(patchwork)
library(lubridate)
library(RColorBrewer)
library(rgdal)
library(viridis)

#install.packages("ggpubr")
library(ggpubr)

#install.packages("ggridges")
library(ggridges)

# library(devtools)
#install_github('timcdlucas/INLAutils', force = T)
#devtools::install_github("timcdlucas/INLAutils")
library(INLAutils)

#install.packages("extrafont") #to add font
library(extrafont)
#font_import()
loadfonts(device = "win")

#library(dismo)
#library(splancs)
#library(reshape)
#library(gstat)
#library(rgeos)
#library(sdmpredictors) # to get Bathymetry
#library(fields)



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
dir_data_shp = paste0(dir_data,"/Data_shp")
dir_figure = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Figure"

# Dir in ILVO laptop
#dir_data = "H:/Desktop/Thesis_Discard-spatial-analysis/Data"
#dir_data_shp = paste0(dir_data,"/Data_shp")
#dir_figure = "H:/Desktop/Thesis_Discard-spatial-analysis/Figure"

# obs_final_LA2_full_pred_20062019_TBB_DEF_70-99 (with all variables)
obs_final <- readRDS(file=paste0(dir_data,"/","obs_final_LA2_full_pred_20062019_TBB_DEF_70-99.rds"))

obs <- obs_final %>% filter(VesselCode != "A.962") #Eliminate samples from Belgica 

#summary of obs
obs %>% group_by(VesselCode) %>% summarize(n = n())
sort(unique(obs$NameEnglish))

# env for prediction
dir_data_env <- paste0(dir_data,"/Data_env")
bathy <- raster(paste0(dir_data_env,"/","bathy_sub.tif"))
slope <- raster(paste0(dir_data_env,"/","slope_sub.tif"))
gravel <- raster(paste0(dir_data_env,"/","gravel_sub.tif"))
sand <- raster(paste0(dir_data_env,"/","sand_sub.tif"))
mud <- raster(paste0(dir_data_env,"/","mud_sub.tif"))
chl_pred <- raster(paste0(dir_data_env,"/","chl_pred.tif"))
sst_pred <- raster(paste0(dir_data_env,"/","sst_pred.tif"))


########################################

# Spatial model -----------------------------------------------------------

########################################

# Setting -----------------------------------------------------------------

# Select species
# Consider subset at 1 year to facilitate the computing speed for trial
spp <- obs %>% filter(NameEnglish == "European plaice") #Year == 2018

# Transform DR
N <- nrow(spp)
spp$DR_tran <- ( spp$DR * (N - 1) + 0.5 ) / N  #: => change zeros to zeros+minimal value

# Check range of predictors (consider standardize or not)
# DR_length, GravelPercent, MudPercent and SandPercent are from 0 to 1
# Convert L_wt_mean to cm
spp$L_wt_mean <- spp$L_wt_mean/10

summary(spp %>% select(L_wt_mean, price_eurobykg, logCPUE, Quota_uti, Bathy, Slope, chl_year, sst))
# L_wt_mean and Bathy has much different scale but keep as original

########################################

# Data exploration --------------------------------------------------------

# Exploration protocol 

# Exploration following Zuur 2010 protocol  
# Protocol
# 1. Outlier Y and X (Y is response variable and X is predictor) - boxplot, cleveland dotplot
# 2. Homogeneity Y - conditional boxplot
# 3. Zero trouble (ratio has this problem?)
# 4. Collinearity X - VIR or scatter plots 
# 5. Relationship Y and X - (multipanel) scatterplots
# 6. Interaction - coplots
# 7. Independence Y - variogram

# 1. Outlier -----------------------------------------------------------------

library(lattice)
Z <- spp %>% dplyr::select(DR_tran, L_wt_mean, price_eurobykg, Quota_uti, logCPUE,
                           Bathy, Slope, GravelPercent, MudPercent, SandPercent, sst, chl_year)

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = NULL)

# Extreme values in Quota_uti

View(
  spp %>% filter(Quota_uti > 5) #10 values
)
spp2 <- spp %>% filter(Quota_uti < 5) 
ggplot(data = spp, aes(x = Quota_uti, y = DR)) + geom_point() + geom_smooth()
ggplot(data = spp2, aes(x = Quota_uti, y = DR)) + geom_point() + geom_smooth()


# The extreme values could influence the relationship to DR 
# and will be eliminated

spp <- spp %>% filter(Quota_uti < 5) 

# 2. Homogeneity Y --------------------------------------------------------
# Model validation

# 3. Zero trouble (ratio has this problem?) -----------------------------
# No trouble

# 4. Collinearity X -----------------------------------------------------

MyVar <- c("L_wt_mean", "price_eurobykg", "Quota_uti", "logCPUE",
           "Bathy", "Slope", "GravelPercent", "MudPercent", "SandPercent", "chl_year", "sst")
corvif(spp[,MyVar]) 

summary(spp)

# All 3 vars GravelPercent, MudPercent and SandPercent at the same time cause NA

MyVar <- c("L_wt_mean", "price_eurobykg", "Quota_uti", "logCPUE",
           "Bathy", "Slope", "GravelPercent", "SandPercent", "chl_year", "sst", "Substrate")
corvif(spp[,MyVar]) 
# High collinearity between GravelPercen and SandPercent
# DR_length, L_wt_mean seems, Substrate have values above 3 but we will consider

MyVar <- c("L_wt_mean", "price_eurobykg", "Quota_uti", "logCPUE",
           "Bathy", "Slope", "GravelPercent", "MudPercent", "chl_year", "sst", "Substrate")
corvif(spp[,MyVar])
# High colllinearity between GravelPercent and Substrate

MyVar <- c("L_wt_mean", "price_eurobykg", "Quota_uti", "logCPUE",
           "Bathy", "Slope", "chl_year", "sst", "GravelPercent", "MudPercent")
corvif(spp[,MyVar])

# DR_length, L_wt_mean have values above 3 
# but we will keep them as predictors and compare using which might be better

# 5. Relationship Y and X -------------------------------------------------

MyVar <- c("L_wt_mean", "price_eurobykg", "Quota_uti", "logCPUE",
           "Bathy", "Slope", "chl_year", "sst", "GravelPercent", "MudPercent")

X11() # Graphic device

MyMultipanel.ggp2(Z = spp, 
                  varx = MyVar, 
                  vary = "DR_tran", 
                  ylab = "DR",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE) #automatic smooth

MyMultipanel.ggp2(Z = spp, 
                  varx = MyVar, 
                  vary = "DR_tran", 
                  ylab = "DR",
                  addSmoother = FALSE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE) + geom_smooth(se = TRUE, method = "lm") #lm

# NA values are removed automatically
# Potential predictors need smooth function: Bathy, L_wt_mean

# With pearson correlation

MyVar <- c("L_wt_mean", "price_eurobykg", "Quota_uti", "logCPUE",
           "Bathy", "Slope", "chl_year", "sst", "GravelPercent", "MudPercent")
Z <- spp %>% select(L_wt_mean, price_eurobykg, Quota_uti, logCPUE,
                    Bathy, Slope, MudPercent, GravelPercent, sst, chl_year)
pairs(Z,
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=MyVar)


# Non linear relationship between Bathy and DR
ggplot(data = spp, aes(x = Bathy, y = DR_tran)) + 
  geom_point() +
  geom_smooth()

ggplot(data = spp, aes(x = L_wt_mean, y = DR_tran)) + 
  geom_point() +
  geom_smooth()

# 6. Interaction -------------------------------------------------------------

# Does not expect interaction of price, L or DL_length

# 7. Independence ---------------------------------------------------------

# After model fitting


########################################

# Spatial model -----------------------------------------------------------

# Model:
#   DR_tran_ij ~ beta(Pi_ij, phi) (Pij is mu)

#   E[DR_tran_ij] = Pi_ij

#                           Pi_ij * (1 - Pi_ij)
#   var[DR_tran_ij] = ..............................
#                                phi + 1

#   logit(Pi_ij) = Intercept + DR_length_ij + L_wt_mean_ij + price_eurobykg_ij +
#                   Vessel_i (Random effects - Vessel) +
#                   u_ij (Spatial point at HaulID level)

#   u_ij ~ GMRF(0, covariance matrix SIGMA)

# DR_tran_ij is the j_th observed discard rate at Vessel_i and HaulID_ij
nrow(spp) # DR_tran_ij - 432
unique(spp$Vessel) # Vessel_i - 9

#   Priors (Default)  

#   Fixed parameters (Beta)
#   Bo ~ N(0,inf) (precision tau = 0)
#   Bi ~ N(0,31.6^2) (i = 1-3) (precision tau = 0.001)
#   Vessel_i ~ N(0, sigma_Vessel^2) or (0, tau_Vessel)
#   HaulID_ij ~ N(0, sigma_HaulID^2) or (0, tau_HaulID)

#   Hyperparameters
#   phi ~ LogGamma(1, 0.1) (precision tau, not variance) 
#   https://inla.r-inla-download.org/r-inla.org/doc/likelihood/beta.pdf
#   log(tau_Vessel) ~ LogGamma(1, 0.00005)
#   log(tau_HaulID) ~ LogGamma(1, 0.00005)


# Procedure (Zuur 2017)
# 1. Make a mesh
# 2. Define the weighting factors (projector matrix)
# 3. Define the SPDE
# 4. Define the spatial field
# 5. Make a stack
# 6. Specificy Model formulation
# 7. Run spatial model
# 8. Inspect the res  ults


# 1. Make a mesh ----------------------------------------------------------

# Check the range of distance between points (sites)
Loc <- cbind(spp$HaulLongitude, spp$HaulLatitude)
D <- dist(Loc) # dist are in degree
hist(D, breaks = 100) 


# Estimation --------------------------------------------------------------

# Clip the land from the mesh above...
# coast from previous processing - study area
coast <- readOGR(paste0(dir_data_shp,"/","coast_final.shp"))

bound.seg <- inla.sp2segment(coast)
mesh <- inla.mesh.2d(boundary=bound.seg, 
                     max.edge=c(0.6,10), #0.7,10#, # first argument = inner mesh / second argument = outer mesh = border
                     cutoff=0.1, # => the coastline is very bumpy, which gives difficulties with very small triangles (change the cutoff is one option, the other option is to make the coastline more straight)
                     offset=c(-0.1, -0.3))

mesh$n #2238
autoplot(mesh)


proj.est <- inla.mesh.projector(mesh, loc = Loc) #Mesh - observation point 
proj.pred <- inla.mesh.projector(mesh, loc = mesh$loc) #Mesh - point of mesh (vertices)

# Prediction --------------------------------------------------------------
# Mesh for prediction
mesh <- inla.mesh.2d(boundary=bound.seg, 
                     max.edge=c(0.3,3), #0.9,3 #, # first argument = inner mesh / second argument = outer mesh = border
                     cutoff=0.1, # => the coastline is very bumpy, which gives difficulties with very small triangles (change the cutoff is one option, the other option is to make the coastline more straight)
                     offset=c(-0.1, -0.3))
mesh$n #3199

proj.est <- inla.mesh.projector(mesh, loc = Loc) #Mesh - observation point 
proj.pred <- inla.mesh.projector(mesh, loc = mesh$loc) #Mesh - point of mesh (vertices)


# 2. Define the weighting factors (projector matrix) ----------------------

A.est <- inla.spde.make.A(mesh, loc = Loc) #observation*mesh vertices
A.pred <-  inla.spde.make.A(mesh, loc = proj.pred$loc) #mesh vertices 

# 3. Define the SPDE ---------------------------------------------------------

spde <- inla.spde2.matern(mesh, alpha = 2)
spde$n.spde

# 4. Define the spatial field ---------------------------------------------

w.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spde$n.spde,
  n.group = 1,
  n.repl  = 1)

# 5. Make a stack ---------------------------------------------------------

# Bathy and DR has non-linear relationship
# Paradinas et al. 2016 divide Bathy by increment (18m) to model non-linear relationship
# between DR and Bathy depth using second order random walk (RW2)
# We will use the same method for our analysis

# We will define the bin group of Bathy (both in est and pred data)
# The method for bin group is quantile, using equidistant quantiles in the probability space
# (https://becarioprecario.bitbucket.io/inla-gitbook/ch-smoothing.html chap 9.4)
# since the majority of fishing depth is under 200m while bathymetry extends to 1200m
summary(obs$Bathy) #overall fishing depth
hist(obs$Bathy)
hist(spp$Bathy)
bathy #overall data depth

# Extract bathymetry
Bat <-  raster::extract(bathy, proj.pred$loc[,-3])

# Create dataframe for estimation and prediction depth
# Estimation is spp$Bathy, Prediction is Bat (extract from raster)
Bathy_est <- tibble(Bathy = spp$Bathy,
                    idx = "est")
Bathy_pred <- tibble(Bathy = Bat,
                     idx = "pred")
Bathy_df <- rbind(Bathy_est, Bathy_pred)

# Divide bathymetry in group for rw2 model, method "cut"
Bathy_df$Bathy_grp <- inla.group(Bathy_df$Bathy, n = 400, method = "cut") #3m interval
sort(unique(Bathy_df$Bathy_grp))

# Add Bathy_grp to spp for estimation
spp$Bathy_grp <- Bathy_df[which(Bathy_df$idx == "est"),"Bathy_grp"]$Bathy_grp
sort(unique(spp$Bathy_grp))
# Extract Bathy_grp for prediction
Bathy_grp_pred <- Bathy_df[which(Bathy_df$idx == "pred"),"Bathy_grp"]$Bathy_grp
sort(unique(Bathy_grp_pred))


spp$L_wt_mean_grp <- inla.group(spp$L_wt_mean, n = 100, method = "cut")
sort(unique(spp$L_wt_mean_grp)) #0.5 cm?

# Estimation --------------------------------------------------------------

# Create a data frame with an intercept and covariates.

# Since INLA does not support categorical variables for SPDE (Spatial) model
# We need to use the trick to turn each value of categorical variable to new dummy variable (i.e. 1,0) 
# One value of each categorical variable will be chosen add reference level
# (i.e. containing only 0 and will be eliminated from the dataframe)

# We have 2 categorical variables: Quarter and IcesDivision
# Quarter: Quarter 1 will be the reference level
# IcesDivision: 4B will be the reference level

# Add Intercept to the data
spp$Intercept <- 1

# Matrix model with Quarter and IcesDivision
X0 <- model.matrix(~ -1 + Intercept + L_wt_mean + Quota_uti + logCPUE + 
                     Quarter + IcesDivision + chl_year + sst + #Potentially important 
                     price_eurobykg + Slope + MudPercent + GravelPercent + Bathy + #Potentially non-important
                     Year + Bathy_grp + L_wt_mean_grp, data = spp) 
#X0 <- model.matrix(~ -1 + Intercept + DR_length + L_wt_mean_std + Bathy + Slope + Quarter + IcesDivision, data = spp)
#Price has NA in 2019 -> add manually later
colnames(X0)

# Data frame X with covariates
X.est <- as.data.frame(X0[,-which(colnames(X0) %in% c("Quarter1"))]) #Eliminate Quarter1 (reference level) 
# IcesDivision4B, SubstrateGravel theortically should be in matrix X0 and then be eliminated 
# but it is eliminated already after matrix formation so we do not eliminate it anymore

# Add Vessel and TripID to X data frame
X.est$Vessel <- spp$Vessel
X.est$Vessel_TripID <- factor(paste0(spp$Vessel, spp$TripID)) #Vessel_Trip id for nested random factor (Trip nested in Vessel)
X.est$TripID <- spp$TripID

str(X.est)

# Tell INLA at which mesh points the covariates are sampled.
stk.est <- inla.stack(tag  = "est", 
                      data = list(y = spp$DR_tran),  
                      A    = list(A.est, 1),   # Matrix of covariance                
                      effects = list(w = w.index,           #Spatial field  
                                     X = as.data.frame(X.est)))  #Covariates               
########################################

# I1 Full model, linear L -------------------------------------
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,L_wt_mean:Bathy)), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f1 <- "I + LU + logC + Q + D + CHL + SST + P + S + M + G + B + L + V + T + W + Y"

I1  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I1,file=paste0(dir_data,"/Data_model/","DR_PLE1_new.rds"))

ggregplot::Efxplot(I1)
round(I1$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3)
I1$waic$waic

# I2 Full model, non-linear L -------------------------------------
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy)), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f2 <- "I + LU + logC + Q + D + CHL + SST + P + S + M + G + B + rw(L) + V + T + W + Y"

I2  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I2,file=paste0(dir_data,"/Data_model/","DR_PLE2_new.rds"))

ggregplot::Efxplot(list(I2))

round(I2$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 

I2$waic$waic
I1$waic$waic

# I3 -------------------------------------
# I2 - Price, Slope, Mud, Gravel
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy, -(price_eurobykg:GravelPercent)
                                                )), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f3 <- "I + LU + logC + Q + D + CHL + SST + B + rw(L) + V + T + W + Y"

I3  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I3,file=paste0(dir_data,"/Data_model/","DR_PLE3_new.rds"))

ggregplot::Efxplot(list(I2))

round(I3$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 

I3$waic$waic

# I4 -------------------------------------
# I2 - Price, Slope, Mud, Gravel, Bathy
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy, -(price_eurobykg:Bathy)
                         )), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f4 <- "I + LU + logC + Q + D + CHL + SST + rw(L) + V + T + W + Y"

I4  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I4,file=paste0(dir_data,"/Data_model/","DR_PLE4_new.rds"))

ggregplot::Efxplot(list(I2))

round(I4$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 

I4$waic$waic

# I5 -------------------------------------
# I2 - Price, Slope, Mud, Gravel, SST
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy, -(sst:GravelPercent), 
                         )), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f5 <- "I + LU + logC + Q + D + CHL + B + rw(L) + V + T + W + Y"

I5  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I5,file=paste0(dir_data,"/Data_model/","DR_PLE5_new.rds"))

ggregplot::Efxplot(list(I2))

round(I5$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 

# I6 -------------------------------------
# I2 - Price, Slope, Mud, Gravel, SST, Bathy
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy, -(sst:Bathy), 
                         )), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f6 <- "I + LU + logC + Q + D + CHL + rw(L) + V + T + W + Y"

I6  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I6,file=paste0(dir_data,"/Data_model/","DR_PLE6_new.rds"))

ggregplot::Efxplot(list(I2))

round(I6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 

# I7 -------------------------------------
# I2 - Price, Slope, Mud, Gravel, SST, 
# I5 without Year
str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy, -(sst:GravelPercent), 
                         )), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde)")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f7 <- "I + LU + logC + Q + D + CHL + B + rw(L) + V + T + W"

I7  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk.est),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk.est), link=1, compute=TRUE),
            verbose = T)

saveRDS(I7,file=paste0(dir_data,"/Data_model/","DR_PLE7_new.rds"))

ggregplot::Efxplot(list(I2))

round(I7$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 

# I8 Prediction -------------------------------------
# The same as I5

# Prediction stack 
# Stack - CHL is important so prediction is based on spatial effect and CHL

#Bat = raster::extract(bathy, proj.pred$loc[,-3])
#Slope = raster::extract(slope, proj.pred$loc[,-3])
#Mud = raster::extract(mud, proj.pred$loc[,-3])
#Sand = raster::extract(sand, proj.pred$loc[,-3])
#Gravel = raster::extract(gravel, proj.pred$loc[,-3])
#sst <- raster::extract(sst_pred, proj.pred$loc[,-3])
chl <-  raster::extract(chl_pred, proj.pred$loc[,-3])

N <- spde$n.spde
X.pred <- data.frame (Intercept      = rep(1, N),
                      chl_year       = chl) #only chl is important

stk.pred <-   inla.stack(data=list(y = NA),
                         A=list(A.pred, 1),
                         effects=list(w = w.index, #Spatial field  
                                      data.frame(X.pred)), # We should use a variable that is continuous and everywhere
                         tag="pred") 


stk <- inla.stack(stk.est, stk.pred)


str(X.est)
f   <- as.formula(paste0("y ~ -1 + Intercept + ", 
                         paste0(colnames(select(X.est,Quota_uti:Bathy, -(sst:GravelPercent), 
                         )), collapse = " + "),
                         " + f(Vessel, model = 'iid') + f(Vessel_TripID, model = 'iid') + 
                           f(L_wt_mean_grp, model = 'rw2') +
                           f(w, model=spde) + f(Year, model = 'iid')")) #f(L_wt_mean_grp, model = 'rw2') + 
f
f8 <- "I + LU + logC + Q + D + CHL + B + rw(L) + V + T + W + Y"

I8  <- inla(f,
            family = "beta", 
            data = inla.stack.data(stk),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
            #control.fixed=list(expand.factor.strategy='inla',prec=list(intercept=1/10)),
            control.predictor = list(A = inla.stack.A(stk), link=1, compute=TRUE),
            verbose = T)

saveRDS(I8,file=paste0(dir_data,"/Data_model/","DR_PLE8_new.rds"))

ggregplot::Efxplot(list(I8))

round(I8$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 
round(I5$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")],3) 
I8$waic$waic

I0 <- readRDS(file=paste0(dir_data,"/Data_model/","DR_PLE8_new.rds"))

########################################


# Model selection ---------------------------------------------------------------

#Later part
waic <- c(I1$waic$waic, I2$waic$waic, 
          I3$waic$waic, I4$waic$waic, 
          I5$waic$waic, I6$waic$waic,
          I7$waic$waic, I8$waic$waic)

lcpo <- c(-sum(log(I1$cpo$cpo)), -sum(log(I2$cpo$cpo)),
          -sum(log(I3$cpo$cpo)), -sum(log(I4$cpo$cpo)), 
          -sum(log(I5$cpo$cpo)), -sum(log(I6$cpo$cpo)),
          -sum(log(I7$cpo$cpo)), -sum(log(I7$cpo$cpo), na.rm = T)
)

Z     <- cbind(waic, lcpo)
rownames(Z) <- c(f1, f2, f3, f4, f5, f6, f7, f8)
Z

#I8 is the best model, set I0 as I8 for later processes
I0 <- I8




########################################

# Model validation --------------------------------------------------------

# Calculate Pi, phi, VarY
N    <- nrow(spp)
Pi   <- I0$summary.fitted.values[1:N,"mean"] # Extract fitted values - Pi (mu)
phi  <- I0$summary.hyper[1, "mean"] # Extract beta hyperparameter phi
phi
VarY <- Pi * (1 - Pi) / (1 + phi) # Variance

# Get Pearson residuals
E1   <- (spp$DR_tran - Pi) / sqrt(VarY)

# Dispersion statistic
# < 1 - Under dispersion, > 1 Over dispersion
p <- length(I0$names.fixed) #number of beta?
Dispersion <- sum(E1^2)/(N-p)
Dispersion 

# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), cex.lab = 1.5, mar = c(5,5,2,2))
plot(x = Pi, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

res <- tibble(fitted = Pi,
              res = E1)

p_res1 <- ggplot(data = res, aes(x = fitted, y = res)) + 
  geom_point(alpha = 0.5) +
  labs(x = "Fitted values",
       y = "Pearson residuals") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

p_res1

# inla_residuals
p_res <- ggplot_inla_residuals(I0, spp$DR_tran, binwidth = 0.1)

p_res2 <- p_res[[2]] +
  labs(x = "Fitted values",
       y = "Observed values") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"))

p_res2

(p_res1 | p_res2) +
  plot_annotation(
    tag_levels = 'A'
  ) & 
  theme(plot.tag = element_text(size = rel(2), 
                                face = "bold",
                                family = "Calibri"))

# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Validation",".png"), scale=2,
       plot = last_plot(),
       width = 12, height = 6, units = "cm",
       dpi = 300
)
########################################

# Result ------------------------------------------------------------------

# Fixed parameters - Posterior mean values and 95% CI
Beta <- I0$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Beta, digits = 2)

# Visualize fix parameters
ggregplot::Efxplot(list(I0))

# Hyper parameters
HP <- I0$summary.hyperpar[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(HP, digits = 4)

# Marginal posterior distributions
# Reference: Lucas 2020 - arXiv:2004.02324v1 

library(cowplot)
p <- autoplot(I0) # All results
cowplot::plot_grid(plotlist = p)

p[[1]]
p[[2]]
p[[3]]
p[[4]]

# Intraclass correlation random effect ------------------------------------

N    <- nrow(spp)
Pi   <- I0$summary.fitted.values[1:N,"mean"] # Extract fitted values - Pi (mu)
phi  <- I0$summary.hyper[1, "mean"] # Extract beta hyperparameter phi
phi
VarY <- Pi * (1 - Pi) / (1 + phi) # Variance

quantile(VarY)
mean(VarY)

# Calculate sigma Vessel and Vessel_Haul
tau_Vessel <- I0$marginals.hyperpar$`Precision for Vessel`
tau_Vessel_TripID <- I0$marginals.hyperpar$`Precision for Vessel_TripID`
tau_Year <- I0$marginals.hyperpar$`Precision for Year`
tau_L <- I0$marginals.hyperpar$`Precision for L_wt_mean_grp`

MySqrt <- function(x) {1/sqrt(x)}

# Use inla.emarginal to extract mean 
sigma_Vessel <- inla.emarginal(MySqrt, tau_Vessel)
sigma_Vessel_TripID <- inla.emarginal(MySqrt, tau_Vessel_TripID)
sigma_Year <- inla.emarginal(MySqrt, tau_Year)
sigma_L <- inla.emarginal(MySqrt, tau_L)

# Intraclass effect
# Vessel
sigma_Vessel^2/(sigma_Vessel^2 + sigma_Vessel_TripID^2 + mean(VarY) + sigma_Year^2 + sigma_L^2) 
# Vessel_trip
(sigma_Vessel^2 + sigma_Vessel_TripID^2)/(sigma_Vessel^2 + sigma_Vessel_TripID^2 + mean(VarY) + sigma_Year^2 + sigma_L^2)
# Year
sigma_Year^2/(sigma_Vessel^2 + sigma_Vessel_TripID^2 + mean(VarY) + sigma_Year^2 + sigma_L^2)

#No Year effect, no vessel effect, there is trip effect


# Mesh --------------------------------------------------------------------

#Mesh
spp_yes <- spp %>% filter(DPUE > 0)
spp_no <- spp %>% filter(DPUE == 0)

p_mesh <- autoplot(mesh)
p_mesh +   
  theme_bw() + 
  theme(plot.title = element_text(size = rel(2),
                                  family = "Calibri",
                                  face = "bold", 
                                  hjust = 0.5,
                                  color = "black"),
        axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",) +
  coord_sf() + 
  labs(title = "SPDE Mesh",
       x = NULL,
       y = NULL)

########################################

# Interpolate the spatial and predicted response var -----------------------------------

### --- plot in a grid m X m --- ##

# HERE WE ARE GOING TO MAKE A PLOT....

### --- Customize the grid to predict --- ###
bbox(coast) #Retrieve spatial bounding box from sp data
# Set xlim ylim
xlim = c(bbox(coast)[1,]); ylim = c(bbox(coast)[2,])

#Calculate difference of min and max x and y
(dxy <- apply(bbox(coast),1, diff)) #1 is margin for row in bbox(coast) matrix

# Indicate lattice (map) dimension 
(r <- dxy[1]/dxy[2]) #Calculate ratio between x and y
m <- 300 #Indicate y dimension
proj.grid.mat <-  inla.mesh.projector(mesh, 
                                      xlim=bbox(coast)[1,], #Set xlim based on bbox
                                      ylim=bbox(coast)[2,] , #Set ylim based on bbox
                                      dims=c(r, 1)*m) # Dimension will be 150r*150 in case m = 150

### --- clean (set NA to the values outside boundary) --- ###
# Use sp::over to overlay points of lattice projection (both coast and land) on coast (study area)
# Points that overlay (on coast) will be 1, and points on land or outside boundary will be NA

ov <- over(SpatialPoints(proj.grid.mat$lattice$loc, coast@proj4string),
           coast)


### --- check grid points inside the map --- ###
i.map <- is.na(ov) #True (NA) is point outside boundary, False is point inside boundary

### Plot the points where we will predict ###
par(mar=c(0,0,0,0))
plot(coast) #Overall boundary
points(proj.grid.mat$lattice$loc[!i.map,], col="red", cex=0.2) #Points in coast
points(proj.grid.mat$lattice$loc[i.map,], col="blue", cex=0.2)

# Setting -----------------------------------------------------------------

# Get the posterior mean, sd, Q0.025, Q0.975 of the spatial field at the mesh points:
mean.w <- inla.mesh.project(proj.grid.mat, I0$summary.random$w$mean)
sd.w <- inla.mesh.project(proj.grid.mat, I0$summary.random$w$sd)
#Q0.025.w <- inla.mesh.project(proj.grid.mat, I0$summary.random$w$`0.025quant`)
#Q0.975.w <- inla.mesh.project(proj.grid.mat, I0$summary.random$w$`0.975quant`)

# Set values on land to NA
sd.w[i.map] <- mean.w[i.map] <-  NA
#Q0.025.w[i.map] <- Q0.975.w[i.map] <- NA

# Get the posterior mean, sd, Q0.25, Q0.95 of the Predicted response variables
mean.res <- inla.mesh.project(proj.grid.mat, I0$summary.fitted.values[inla.stack.index(stk,"pred")$data, "mean"])
sd.res <- inla.mesh.project(proj.grid.mat, I0$summary.fitted.values[inla.stack.index(stk,"pred")$data, "sd"])
#Q0.025.res <- inla.mesh.project(proj.grid.mat, I0$summary.fitted.values[inla.stack.index(stk,"pred")$data, "0.025quant"])
#Q0.0975.res <- inla.mesh.project(proj.grid.mat, I0$summary.fitted.values[inla.stack.index(stk,"pred")$data, "0.975quant"])

sd.res[i.map] <- mean.res[i.map] <-  NA
#Q0.025.res[i.map] <- Q0.0975.res[i.map] <- NA


# Create Grid dataframe
Grid <- expand.grid(lon = proj.grid.mat$x, 
                    lat = proj.grid.mat$y) # Convert proj.grid.mat to dataframe
# Add spatial values to 
Grid$mean.w <- as.vector(mean.w)     
Grid$sd.w <- as.vector(sd.w)               
#Grid$Q0.025.w  <- as.vector(Q0.025.w)
#Grid$Q0.975.w <- as.vector(Q0.975.w)

# Add predicted values
Grid$mean.res <- as.vector(mean.res)     
Grid$sd.res <- as.vector(sd.res)               
#Grid$Q0.025.res  <- as.vector(Q0.025.res)
#Grid$Q0.0975.res <- as.vector(Q0.0975.res)

Grid <- na.exclude(Grid) #Eliminate NA values

summary(Grid)

# Plot setting

# Set colorRampPalette
#col.l <- colorRampPalette(c('red', 'green'))(50) 
#col.y <- colorRampPalette(c('red', 'yellow'))(50) 
#RLcolor <- colorRampPalette(c("purple","darkblue","royalblue", "skyblue","green","yellow","red"),bias=1,space="Lab",interpolate="linear")(20)

# Set area of interest (aoi) 
world <- ne_countries(scale = "medium", returnclass = "sf")
aoi_lim <- c("Belgium", "Denmark", 
             "France", "Germany", 
             "Luxembourg", "Netherlands", 
             "United Kingdom", "Spain",
             "Switzerland", "Austria", "Italy","Ireland") #"Isle of Man"
aoi <- world %>% filter(sovereignt %in% aoi_lim) 

########################################

# Plot spatial field ----------------------------------------------------------------

# Posterior mean --------------------------------------------------------------------


p1 <- ggplot() +
  geom_raster(data = Grid, aes(x = lon, y = lat, fill = mean.w)) +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  scale_fill_viridis(name = NULL,
                     #breaks = legend_break,
                     option = "viridis",
                     direction = 1,
                     guide = guide_colorbar(
                       barheight = unit(18, units = "cm"),
                       barwidth = unit(0.5, units = "cm"))
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "Mean",
       x = NULL,
       y = NULL) +
  theme(
    plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
    plot.title = element_text(size = rel(3),
                              family = "Calibri",
                              face = "bold", 
                              hjust = 0.5,
                              color = "black"),
    axis.text = element_text(size = rel(1.5),
                             family = "Calibri"), 
    legend.text = element_text(size = rel(1.5),
                               family = "Calibri"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p1
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Spatial_mean",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# Posterior sd ------------------------------------------------------------

p2 <- ggplot() +
  geom_raster(data = Grid, aes(x = lon, y = lat, fill = sd.w)) +
  geom_sf(data = aoi, fill = "grey", color = "white") + 
  scale_fill_viridis(name = NULL,
                     #breaks = legend_break,
                     option = "magma",
                     direction = -1,
                     guide = guide_colorbar(
                       barheight = unit(18, units = "cm"),
                       barwidth = unit(0.5, units = "cm"))
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    title = "Standard deviation", 
    x = NULL, 
    y = NULL
  ) + 
  theme(
    plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
    plot.title = element_text(size = rel(3),
                              family = "Calibri",
                              face = "bold", 
                              hjust = 0.5,
                              color = "black"),
    axis.text = element_text(size = rel(1.5),
                             family = "Calibri"), 
    legend.text = element_text(size = rel(1.5),
                               family = "Calibri"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p2
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Spatial_sd",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)
# Group and save plot -----------------------------------------------------

(p1 | p2) +
  plot_annotation(
    tag_levels = 'A'
  ) & 
  theme(plot.tag = element_text(size = rel(2), 
                                face = "bold",
                                family = "Calibri"))

# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Spatial",".png"), scale=2,
       plot = last_plot(),
       width = 22, height = 12, units = "cm",
       dpi = 300
)

########################################

# Plot prediction ---------------------------------------------------------

# Posterior mean --------------------------------------------------------------------

summary(Grid$mean.res)
hist(Grid$mean.res, breaks = 100)
legend_break <- c(0.1, 0.2, 0.3, 0.5, 0.8)  

p3 <- ggplot() +
  geom_raster(data = Grid, aes(x = lon, y = lat, fill = mean.res)) +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  scale_fill_viridis(name = NULL,
                     breaks = legend_break,
                     option = "viridis",
                     trans="log",
                     direction = 1,
                     guide = guide_colorbar(
                       barheight = unit(18, units = "cm"),
                       barwidth = unit(0.5, units = "cm"))
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "Predictive mean DR",
       x = NULL,
       y = NULL) +
  theme(
    plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
    plot.title = element_text(size = rel(3),
                              family = "Calibri",
                              face = "bold", 
                              hjust = 0.5,
                              color = "black"),
    axis.text = element_text(size = rel(1.5),
                             family = "Calibri"), 
    legend.text = element_text(size = rel(1.5),
                               family = "Calibri"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p3
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Pred_mean",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# Posterior sd ------------------------------------------------------------

#summary(Grid$sd.res)
#legend_break <- round(seq(0.02, 0.2, by = 0.02), 2)
p4 <- ggplot() +
  geom_raster(data = Grid, aes(x = lon, y = lat, fill = sd.res)) +
  geom_sf(data = aoi, fill = "grey", color = "white") + 
  scale_fill_viridis(name = NULL,
                     #breaks = legend_break,
                     option = "magma",
                     direction = -1,
                     guide = guide_colorbar(
                       barheight = unit(18, units = "cm"),
                       barwidth = unit(0.5, units = "cm"))
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    title = "Standard deviation DR", 
    x = NULL, 
    y = NULL
  ) + 
  theme(
    plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
    plot.title = element_text(size = rel(3),
                              family = "Calibri",
                              face = "bold", 
                              hjust = 0.5,
                              color = "black"),
    axis.text = element_text(size = rel(1.5),
                             family = "Calibri"), 
    legend.text = element_text(size = rel(1.5),
                               family = "Calibri"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p4
# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Pred_sd",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# DPUE --------------------------------------------------------------------
spp_no <- anti_join(obs,spp, by = "HaulID")
spp_no <- spp_no %>% group_by(HaulID, HaulLongitude, HaulLatitude) %>% summarize(n = n())
summary(spp_yes$DPUE)
hist(spp_yes$DPUE, breaks = 100)
mybreaks <- c(1,10,100,500)

p5 <- ggplot() +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  geom_point(data = spp_yes %>% arrange(desc(DPUE)), 
             aes(x = HaulLongitude, 
                 y = HaulLatitude, 
                 size = DPUE, 
                 color = DPUE, 
                 alpha = DPUE), 
             #color = "purple", 
             #alpha = 0.5
  ) +
  geom_point(data = spp_no, 
             aes(x = HaulLongitude, y = HaulLatitude), 
             color = "red", 
             alpha = 1/5,
             shape = 3,
             size = rel(1))
p5 <- p5 + 
  scale_size_continuous(name="DPUE (kg/h)", range=c(3,8), breaks=mybreaks) +
  scale_color_viridis(name="DPUE (kg/h)", trans="log", breaks=mybreaks) +
  scale_alpha_continuous(name="DPUE (kg/h)", trans="log", range=c(0.1, 0.9), breaks=mybreaks) +
  guides(colour = guide_legend()) + 
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "DPUE",
       x = NULL,
       y = NULL)
p5 <- p5 + 
  theme_bw() +
  theme(plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
        plot.title = element_text(size = rel(3),
                                  family = "Calibri",
                                  face = "bold", 
                                  hjust = 0.5,
                                  color = "black"),
        axis.text = element_text(size = rel(1.5),
                                 family = "Calibri"), 
        legend.title = element_text(size = rel(1.5),
                                    face = "bold",
                                    family = "Calibri"),
        legend.text = element_text(size = rel(1.5),
                                   family = "Calibri"),
        legend.position = c(0.15, 0.15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

p5 <- p5 + 
  annotate("text", x = 3, y = 56, label = "North Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3.2, y = 49, label = "English Channel", size = 5, family = "Calibri") +
  annotate("text", x = -7.6, y = 50.9, label = "Celtic Sea", size = 5, family = "Calibri") +
  annotate("text", x = -4.5, y = 54.5, label = "Irish Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3, y = 44.2, label = "Biscay Bay", size = 5, family = "Calibri")

p5
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_DPUE",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# DR --------------------------------------------------------------------
spp_no <- anti_join(obs,spp, by = "HaulID")
spp_no <- spp_no %>% group_by(HaulID, HaulLongitude, HaulLatitude) %>% summarize(n = n())
summary(spp$DR)
hist(spp$DR, breaks = 100)
mybreaks <- c(0.05,0.2,0.5,1)

p6 <- ggplot() +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  geom_point(data = spp %>% arrange(desc(DR)), 
             aes(x = HaulLongitude, 
                 y = HaulLatitude, 
                 size = DR, 
                 color = DR), 
             #color = "purple", 
             alpha = 0.7) +
  geom_point(data = spp_no, 
             aes(x = HaulLongitude, y = HaulLatitude), 
             color = "red", 
             alpha = 1/5,
             shape = 3,
             size = rel(1)) 
p6 <- p6 + 
  scale_size_continuous(name="DR", range=c(1,8), breaks=mybreaks) +
  scale_color_viridis(name="DR", trans="log", breaks=mybreaks) + #trans="log", 
  guides(colour = guide_legend()) + 
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "DR",
       x = NULL,
       y = NULL)
p6 <- p6 + 
  theme_bw() +
  theme(plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
        plot.title = element_text(size = rel(3),
                                  family = "Calibri",
                                  face = "bold", 
                                  hjust = 0.5,
                                  color = "black"),
        axis.text = element_text(size = rel(1.5),
                                 family = "Calibri"), 
        legend.title = element_text(size = rel(1.5),
                                    face = "bold",
                                    family = "Calibri"),
        legend.text = element_text(size = rel(1.5),
                                   family = "Calibri"),
        legend.position = c(0.15, 0.15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

p6 <- p6 + 
  annotate("text", x = 3, y = 56, label = "North Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3.2, y = 49, label = "English Channel", size = 5, family = "Calibri") +
  annotate("text", x = -7.6, y = 50.9, label = "Celtic Sea", size = 5, family = "Calibri") +
  annotate("text", x = -4.5, y = 54.5, label = "Irish Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3, y = 44.2, label = "Biscay Bay", size = 5, family = "Calibri")

p6
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_DR",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# CPUE --------------------------------------------------------------------

spp_no <- anti_join(obs,spp, by = "HaulID")
spp_no <- spp_no %>% group_by(HaulID, HaulLongitude, HaulLatitude) %>% summarize(n = n())
summary(spp$CPUE)
hist(spp$CPUE, breaks = 100)
mybreaks <- c(1,10,50,100)

p7 <- ggplot() +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  geom_point(data = spp %>% arrange(desc(DR)), 
             aes(x = HaulLongitude, 
                 y = HaulLatitude, 
                 size = CPUE, 
                 color = CPUE), 
             #color = "purple", 
             alpha = 0.7) +
  geom_point(data = spp_no, 
             aes(x = HaulLongitude, y = HaulLatitude), 
             color = "red", 
             alpha = 1/5,
             shape = 3,
             size = rel(1)) 
p7 <- p7 + 
  scale_size_continuous(name="CPUE", range=c(1,8), breaks=mybreaks) +
  scale_color_viridis(name="CPUE", trans="log", breaks=mybreaks) + #trans="log", 
  guides(colour = guide_legend()) + 
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "CPUE",
       x = NULL,
       y = NULL)
p7 <- p7 + 
  theme_bw() +
  theme(plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
        plot.title = element_text(size = rel(3),
                                  family = "Calibri",
                                  face = "bold", 
                                  hjust = 0.5,
                                  color = "black"),
        axis.text = element_text(size = rel(1.5),
                                 family = "Calibri"), 
        legend.title = element_text(size = rel(1.5),
                                    face = "bold",
                                    family = "Calibri"),
        legend.text = element_text(size = rel(1.5),
                                   family = "Calibri"),
        legend.position = c(0.15, 0.15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

p7 <- p7 + 
  annotate("text", x = 3, y = 56, label = "North Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3.2, y = 49, label = "English Channel", size = 5, family = "Calibri") +
  annotate("text", x = -7.6, y = 50.9, label = "Celtic Sea", size = 5, family = "Calibri") +
  annotate("text", x = -4.5, y = 54.5, label = "Irish Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3, y = 44.2, label = "Biscay Bay", size = 5, family = "Calibri")

p7
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_CPUE",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# LPUE --------------------------------------------------------------------

spp_no <- anti_join(obs,spp, by = "HaulID")
spp_no <- spp_no %>% group_by(HaulID, HaulLongitude, HaulLatitude) %>% summarize(n = n())

spp$LPUE <- spp$L/spp$Dur_hour

summary(spp$LPUE)
hist(spp$LPUE, breaks = 100)
mybreaks <- c(1,10,50,100)

p8 <- ggplot() +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  geom_point(data = spp %>% arrange(desc(LPUE)), 
             aes(x = HaulLongitude, 
                 y = HaulLatitude, 
                 size = LPUE, 
                 color = LPUE), 
             #color = "purple", 
             alpha = 0.7) +
  geom_point(data = spp_no, 
             aes(x = HaulLongitude, y = HaulLatitude), 
             color = "red", 
             alpha = 1/5,
             shape = 3,
             size = rel(1)) 
p8 <- p8 + 
  scale_size_continuous(name="LPUE", range=c(1,8), breaks=mybreaks) +
  scale_color_viridis(name="LPUE", trans="log1p", breaks=mybreaks) + #trans="log", 
  guides(colour = guide_legend()) + 
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "LPUE",
       x = NULL,
       y = NULL)
p8 <- p8 + 
  theme_bw() +
  theme(plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
        plot.title = element_text(size = rel(3),
                                  family = "Calibri",
                                  face = "bold", 
                                  hjust = 0.5,
                                  color = "black"),
        axis.text = element_text(size = rel(1.5),
                                 family = "Calibri"), 
        legend.title = element_text(size = rel(1.5),
                                    face = "bold",
                                    family = "Calibri"),
        legend.text = element_text(size = rel(1.5),
                                   family = "Calibri"),
        legend.position = c(0.15, 0.15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

p8 <- p8 + 
  annotate("text", x = 3, y = 56, label = "North Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3.2, y = 49, label = "English Channel", size = 5, family = "Calibri") +
  annotate("text", x = -7.6, y = 50.9, label = "Celtic Sea", size = 5, family = "Calibri") +
  annotate("text", x = -4.5, y = 54.5, label = "Irish Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3, y = 44.2, label = "Biscay Bay", size = 5, family = "Calibri")

p8
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_LPUE",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

# Group and save plot -----------------------------------------------------

(p3 | p5) / (p4 | p6) +
  plot_annotation(
    tag_levels = 'A'
  ) & 
  theme(plot.tag = element_text(size = rel(2), 
                                face = "bold",
                                family = "Calibri"))

# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Pred_all",".png"), scale=2,
       plot = last_plot(),
       width = 22, height = 24, units = "cm",
       dpi = 300
)

(p3 / p5) | (p4 / p6) +
  plot_annotation(
    tag_levels = 'A'
  ) & 
  theme(plot.tag = element_text(size = rel(2), 
                                face = "bold",
                                family = "Calibri"))

# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_COD_Pred_all2",".png"), scale=2,
       plot = last_plot(),
       width = 22, height = 24, units = "cm",
       dpi = 300
)


########################################

# Model interpretation ---------------------------------------------------

# Effect of predictors  ----------------------------------------------------

# Effect of predictors will be visualized 
# by calculating the mean, Q0.025 and Q0.975 of fitted DR regarding the effect of the chosen predictors
# other predictors will be set at mean value
ggregplot::Efxplot(I0)

# Predictors visualized: Quota_uti, logCPUE, CHL, L_wt_mean

# DR = Intercept + Quota_uti + logCPUE + CHL + SST + L

b_I <-  I0$summary.fixed["Intercept",]
b_Q <- I0$summary.fixed["Quota_uti",]
b_logC <- I0$summary.fixed["logCPUE",]
b_CHL <- I0$summary.fixed["chl_year",]
b_B <- I0$summary.fixed["Bathy",]

mean_Q <- mean(spp$Quota_uti)
mean_logC <- mean(spp$logCPUE)
mean_CHL <- mean(spp$chl_year)
mean_B <- mean(spp$Bathy)

L_b <- I0$summary.random$L_wt_mean_grp
mean(spp$L_wt_mean)
mean(spp$L_wt_mean_grp) #28.2
L_b$ID #ID 30 28.3
b_L <- L_b[30,] 

# Quota_uti ---------------------------------------------------------------

# Quota_uti
# DR = Intercept + Quota_uti + logCPUE + CHL + Bathy + L

#linear predictor
g_mean <- b_I$mean  + b_Q$mean*spp$Quota_uti + b_logC$mean*mean_logC + b_CHL$mean*mean_CHL + b_B$mean*mean_B + b_L$mean
g_0.025 <- b_I$`0.025quant` + b_Q$`0.025quant`*spp$Quota_uti + b_logC$`0.025quant`*mean_logC + b_CHL$`0.025quant`*mean_CHL + b_B$`0.025quant`*mean_B + b_L$`0.025quant`
g_0.975 <- b_I$`0.975quant` + b_Q$`0.975quant`*spp$Quota_uti + b_logC$`0.975quant`*mean_logC + b_CHL$`0.975quant`*mean_CHL + b_B$`0.975quant`*mean_B + b_L$`0.975quant` 

#miu 
mu_mean <- exp(g_mean)/(1+exp(g_mean))
mu_0.025 <- exp(g_0.025)/(1+exp(g_0.025))
mu_0.975 <- exp(g_0.975)/(1+exp(g_0.975))

df_Q <- data.frame(
  fitted =  I0$summary.fitted.values[inla.stack.index(stk,"est")$data, "mean"],
  mu_mean = mu_mean,
  mu_0.025 = mu_0.025,
  mu_0.975 = mu_0.975,
  Quota_uti = spp$Quota_uti
)

p_Q <- ggplot() + geom_point(data = df_Q, 
                             aes(x = Quota_uti, y = fitted), 
                             alpha = 0.2, color = "grey") +
  geom_line(data = df_Q, 
            aes(x = Quota_uti, y = mu_mean), 
            colour = "black",
            lwd = 1) +
  geom_ribbon(data = df_Q, 
              aes(x = Quota_uti, ymax = mu_0.975, ymin = mu_0.025), 
              alpha = 0.5) + 
  labs(x = "Limit utilization",
       y = "Fitted ratio") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

p_Q
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Effect_LU",".png"), scale=2,
       plot = last_plot(),
       width = 7, height = 6, units = "cm",
       dpi = 300
)

# logCPUE ---------------------------------------------------------------

# loCPUE
# DR = Intercept + Quota_uti + logCPUE + CHL + Bathy + L

#linear predictor
g_mean <- b_I$mean  + b_Q$mean*mean_Q + b_logC$mean*spp$logCPUE + b_CHL$mean*mean_CHL + b_B$mean*mean_B + b_L$mean
g_0.025 <- b_I$`0.025quant` + b_Q$`0.025quant`*mean_Q + b_logC$`0.025quant`*spp$logCPUE + b_CHL$`0.025quant`*mean_CHL + b_B$`0.025quant`*mean_B + b_L$`0.025quant`
g_0.975 <- b_I$`0.975quant` + b_Q$`0.975quant`*mean_Q + b_logC$`0.975quant`*spp$logCPUE + b_CHL$`0.975quant`*mean_CHL + b_B$`0.975quant`*mean_B + b_L$`0.975quant` 

#miu 
mu_mean <- exp(g_mean)/(1+exp(g_mean))
mu_0.025 <- exp(g_0.025)/(1+exp(g_0.025))
mu_0.975 <- exp(g_0.975)/(1+exp(g_0.975))

df_logC <- data.frame(
  fitted =  I0$summary.fitted.values[inla.stack.index(stk,"est")$data, "mean"],
  mu_mean = mu_mean,
  mu_0.025 = mu_0.025,
  mu_0.975 = mu_0.975,
  logCPUE = spp$logCPUE
)

p_C <- ggplot() + geom_point(data = df_logC, 
                             aes(x = logCPUE, y = fitted), 
                             alpha = 0.2, color = "grey") +
  geom_line(data = df_logC, 
            aes(x = logCPUE, y = mu_mean), 
            colour = "black",
            lwd = 1) +
  geom_ribbon(data = df_logC, 
              aes(x = logCPUE, ymax = mu_0.975, ymin = mu_0.025), 
              alpha = 0.5) + 
  labs(x = "Log10 CPUE (log-kg/h)",
       y = "Fitted ratio") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

p_C
  ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Effect_logC",".png"), scale=2,
       plot = last_plot(),
       width = 7, height = 6, units = "cm",
       dpi = 300
)

# CHL ---------------------------------------------------------------

# CHL
# DR = Intercept + Quota_uti + logCPUE + CHL + Bathy + L

#linear predictor
g_mean <- b_I$mean  + b_Q$mean*mean_Q + b_logC$mean*mean_logC + b_CHL$mean*spp$chl_year + b_B$mean*mean_B + b_L$mean
g_0.025 <- b_I$`0.025quant` + b_Q$`0.025quant`*mean_Q + b_logC$`0.025quant`*mean_logC + b_CHL$`0.025quant`*spp$chl_year + b_B$`0.025quant`*mean_B + b_L$`0.025quant`
g_0.975 <- b_I$`0.975quant` + b_Q$`0.975quant`*mean_Q + b_logC$`0.975quant`*mean_logC + b_CHL$`0.975quant`*spp$chl_year + b_B$`0.975quant`*mean_B + b_L$`0.975quant` 

#miu 
mu_mean <- exp(g_mean)/(1+exp(g_mean))
mu_0.025 <- exp(g_0.025)/(1+exp(g_0.025))
mu_0.975 <- exp(g_0.975)/(1+exp(g_0.975))

df_CHL <- data.frame(
  fitted =  I0$summary.fitted.values[inla.stack.index(stk,"est")$data, "mean"],
  mu_mean = mu_mean,
  mu_0.025 = mu_0.025,
  mu_0.975 = mu_0.975,
  CHL = spp$chl_year
)

p_CHL <- ggplot() + geom_point(data = df_CHL, 
                               aes(x = CHL, y = fitted), 
                               alpha = 0.2, color = "grey") +
  geom_line(data = df_CHL, 
            aes(x = CHL, y = mu_mean), 
            colour = "black",
            lwd = 1) +
  geom_ribbon(data = df_CHL, 
              aes(x = CHL, ymax = mu_0.975, ymin = mu_0.025), 
              alpha = 0.5) + 
  labs(x = "Chlorophyll-a concentration (mg/m3)",
       y = "Fitted ratio") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

p_CHL
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Effect_CHL",".png"), scale=2,
       plot = last_plot(),
       width = 7, height = 6, units = "cm",
       dpi = 300
)

# L_wt_mean ---------------------------------------------------------------

L_b

df_L <- data.frame(
  L_wt_mean_grp = spp$L_wt_mean_grp,
  g_mean = 0,
  g_0.025 = 0,
  g_0.975 = 0
)


for(i in 1:nrow(L_b)) {
  df_L[which(df_L$L_wt_mean_grp == L_b$ID[i]),"g_mean"] <-  b_I$mean  + 
    b_Q$mean*mean_Q + b_logC$mean*mean_logC + b_CHL$mean*mean_CHL + 
    b_B$mean*mean_B + L_b[i,"mean"] 
  df_L[which(df_L$L_wt_mean_grp == L_b$ID[i]),"g_0.025"] <- b_I$`0.025quant`  + 
    b_Q$`0.025quant`*mean_Q + b_logC$`0.025quant`*mean_logC + b_CHL$`0.025quant`*mean_CHL + 
    b_B$`0.025quant`*mean_B + L_b[i,"0.025quant"] 
  df_L[which(df_L$L_wt_mean_grp == L_b$ID[i]),"g_0.975"] <- b_I$`0.975quant`  + 
    b_Q$`0.975quant`*mean_Q + b_logC$`0.975quant`*mean_logC + b_CHL$`0.975quant`*mean_CHL + 
    b_B$`0.975quant`*mean_B + L_b[i,"0.975quant"] 
} 


df_L <- df_L %>% mutate(
  mu_mean = exp(g_mean)/(1+exp(g_mean)),
  mu_0.025 = exp(g_0.025)/(1+exp(g_0.025)),
  mu_0.975 = exp(g_0.975)/(1+exp(g_0.975)),
  fitted   = I0$summary.fitted.values[inla.stack.index(stk,"est")$data, "mean"]
)

MCRS <- unique(spp$MLS/10)
p_L <- ggplot() + geom_point(data = df_L, 
                             aes(x = L_wt_mean_grp, y = fitted), 
                             alpha = 0.2, color = "grey") +
  geom_line(data = df_L, 
            aes(x = L_wt_mean_grp, y = mu_mean), 
            colour = "black",
            lwd = 1) +
  geom_ribbon(data = df_L, 
              aes(x = L_wt_mean_grp, ymax = mu_0.975, ymin = mu_0.025), 
              alpha = 0.5) +
  labs(x = "Mean length (cm)",
       y = "Fitted ratio") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  annotate("segment",
           color = "red",
           x = MCRS, 
           xend = MCRS,
           y = 0, 
           yend = 1,
           linetype="dashed") +
  annotate("text", x = (MCRS - 7), 
           y = 0.02, 
           label = paste0("MCRS"," (",MCRS," cm)"), 
           size = 4, 
           family = "Calibri")

p_L
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Effect_L",".png"), scale=2,
       plot = last_plot(),
       width = 7, height = 6, units = "cm",
       dpi = 300
)

# Group predictors effects ------------------------------------------------

(p_Q | p_C)/(p_L | p_CHL) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = rel(1), 
                                face = "bold",
                                family = "Calibri"))

# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Effect_all",".png"), scale=2,
       plot = last_plot(),
       width = 14, height = 12, units = "cm",
       dpi = 300
)

# Non-linear effect -------------------------------------------------------

# Effect L_wt_mean
L_wt_mean_grp <- as.data.frame(I0$summary.random$L_wt_mean_grp)

p1 <- ggplot(L_wt_mean_grp) +   
  geom_line(aes(ID, `0.5quant`)) +   
  geom_line(aes(ID, `0.025quant`), linetype="dashed") +   
  geom_line(aes(ID, `0.975quant`), linetype="dashed") +
  labs(x = "Weighted mean length (cm)",
       y = "Effect of weighted mean length") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

MCRS <- unique(spp$MLS/10)
L_min <- min(spp$L_wt_mean)
L_max <- max(spp$L_wt_mean)
mean_min <- min(L_wt_mean_grp$`0.025quant`)
mean_max <- max(L_wt_mean_grp$`0.975quant`)

p1 <- p1 + 
  annotate("segment",
           #color = "grey",
           x = L_min, 
           xend = L_max,
           y = 0, yend = 0,
           linetype="dotted"
  ) + 
  annotate("segment",
           #color = "grey",
           x = MCRS, 
           xend = MCRS,
           y = mean_min, 
           yend = mean_max,
           linetype="dotted"
  ) +
  annotate("text", x = (MCRS + 3), 
           y = mean_max, 
           label = "MCRS", 
           size = 4, 
           family = "Calibri")

p1

# non-linear Bathymetry - if applicable
Bathy_random <- as.data.frame(I0$summary.random$Bathy)
Bathy_min <- min(spp$Bathy)
Bathy_max <- max(spp$Bathy)

p2 <- ggplot(Bathy_random) +   
  geom_line(aes(ID, `0.5quant`)) +   
  geom_line(aes(ID, `0.025quant`), linetype="dashed") +   
  geom_line(aes(ID, `0.975quant`), linetype="dashed") +
  labs(x = "Bathymetry (m)",
       y = "Effect of Bathymetry") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  annotate("segment",
           #color = "grey",
           x = Bathy_min, 
           xend = Bathy_max,
           y = 0, yend = 0,
           linetype="dotted")

(p1 | p2) +
  plot_annotation(
    tag_levels = 'A'
  ) & 
  theme(plot.tag = element_text(size = rel(2), 
                                face = "bold",
                                family = "Calibri"))

# save plots as .png
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("DR_PLE_Non-linear effect",".png"), scale=2,
       plot = last_plot(),
       width = 15, height = 8, units = "cm",
       dpi = 300
)

# Fitted DR and non-linear variables
spp$fitted <- I0$summary.fitted.values[inla.stack.index(stk,"est")$data, "mean"]

#spp$L_wt_mean_grp <- round(spp$L_wt_mean_grp,1)
#levels(as.factor(spp$L_wt_mean_grp))[c(T, rep(F, 9))] #select values at every 5 nodes


p_L <- ggplot(data = spp, 
              aes(x = as.factor(round(L_wt_mean_grp,1)), y = fitted)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = levels(as.factor(round(spp$L_wt_mean_grp,1)))[c(T, rep(F, 5))]) +
  labs(x = "Weighted mean length (cm)",
       y = "Fitted ratio")

p_L <- p_L +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.minor = element_blank()) +
  annotate("segment",
           color = "red",
           x = as.factor(MCRS), 
           xend = as.factor(MCRS),
           y = 0, 
           yend = 1) +
  annotate("text", x = (MCRS - 4), 
           y = 0.05, 
           label = "MCRS",
           size = 4,
           family = "Calibri",
           fontface = 2) #for annotate, bold is fontface 2
p_L


# Effect of bathymetry if applicable
#levels(as.factor(spp$Bathy_grp))[c(T, rep(F, 1))] #select values at every 1 nodes

ggplot(data = spp, aes(x = as.factor(Bathy_grp), y = fitted)) + geom_boxplot()

p_B <- ggplot(data = spp, 
              aes(x = as.factor(Bathy_grp), y = fitted)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = levels(as.factor(spp$Bathy_grp))[c(T, rep(F, 1))]) +
  labs(x = "Bathymetry (m)",
       y = "Fitted ratio") +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.minor = element_blank()) 
p_B

p_L/p_B







