#       IMBRSea Thesis 
#       Tuan Anh Bui
#       01.06.2020

#       Belgian beam trawl Discard spatial analysis

#       This is the script for the summary in the final manuscript

########################################

# Load support files and packages -----------------------------------------

library(tidyverse)
library(ggridges)
library(patchwork)
library(ggrepel)

#install.packages("extrafont") #to add font
library(extrafont)
library(viridis)
#font_import()
loadfonts(device = "win")

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgdal)


#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

#devtools::install_github('oswaldosantos/ggsn')
library(ggsn)

##################################################

# Load data ---------------------------------------------------------------

# Dir in Tuan-Anh laptop
dir_data = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Data"
dir_figure = "D:/IMBRSea/1.Study/_IMBRSea_Thesis/Data_analysis/Thesis_Discard-spatial-analysis/Figure"


# obs_final_LA_full_pred_20062019_TBB_DEF_70-99 (with weigth, length, sale variables)
obs_final <- readRDS(file=paste0(dir_data,"/","obs_final_LA2_full_pred_20062019_TBB_DEF_70-99.rds"))

obs <- obs_final %>% filter(VesselCode != "A.962") #Eliminate samples from Belgica 

unique(obs_final$NameEnglish)

obs2 <- obs
obs2[which(obs2$NameEnglish == "Raja rays nei"),"NameEnglish"] <- "Skates"
obs2 <- obs2 %>% mutate(Quota_cut = if_else(Quota_uti >= 0 & Quota_uti <= 1, "Within limitations", "Over limitations"))

# obs_wt (original data)
obs_wt <- readRDS(file=paste0(dir_data,"/","obs_processed_20062019_TBB_DEF_70-99.rds"))
obs_wt <- ungroup(obs_wt)

# obs_len_freq 
obs_len_freq <- readRDS(file=paste0(dir_data,"/","obs_len_freq_20062019_TBB_DEF_70-99.rds"))


# IcesDivision
dir_data_shp <- paste0(dir_data,"/Data_shp")
IcesDiv <- st_read(paste0(dir_data_shp,"/","IcesDiv_new.shp"))
coast <- readOGR(paste0(dir_data_shp,"/","coast_final.shp"))

##################################################


# 2.1.1 Discard sampling program ------------------------------------------

bbox(coast) #Retrieve spatial bounding box from sp data
# Set xlim ylim
xlim = c(bbox(coast)[1,]); ylim = c(bbox(coast)[2,])


world <- ne_countries(scale = "medium", returnclass = "sf")
aoi_lim <- c("Belgium", "Denmark", 
             "France", "Germany", 
             "Luxembourg", "Netherlands", 
             "United Kingdom", "Spain",
             "Switzerland", "Austria", "Italy","Ireland") #"Isle of Man"
aoi <- world %>% filter(sovereignt %in% aoi_lim) 


# Sampled haul
obs_sample <- obs %>% 
  group_by(HaulID, HaulLongitude, HaulLatitude) %>%
  summarize(n = n())

unique(IcesDiv$Area_27)
IcesDiv_sub1 <- IcesDiv %>% filter(Area_27 != "7.d" & Area_27 != "7.e" & Area_27 != "7.f")
IcesDiv_sub2 <- IcesDiv %>% filter(Area_27 == "7.d" | Area_27 == "7.e" | Area_27 == "7.f")

ggplot() +
  geom_point(data = obs_sample, 
             aes(x = HaulLongitude, y = HaulLatitude),
                 alpha = 1/5,
                 size = rel(1.5)) +
  geom_sf(data = aoi, fill = "grey", color = "white") +
  geom_sf(data = IcesDiv, fill = "transparent", color = "dark grey") +
  geom_sf_label(data = IcesDiv_sub1, aes(label = Area_27), fill = "transparent") +
  geom_sf_label_repel(data = IcesDiv_sub2, aes(label = Area_27), fill = "transparent",
    nudge_x = 0.8, nudge_y = -0.1, seed = 1) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "Belgian observer data (2006 - 2018)",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(
    plot.margin = margin(t = 0.3, r = 0.3, b = 0.3, l = 0.3, unit = "cm"),
    plot.title = element_text(size = rel(2.5),
                              family = "Calibri",
                              face = "bold", 
                              hjust = 0.5,
                              color = "black"),
    axis.text = element_text(size = rel(1.2),
                             family = "Calibri"), 
    legend.text = element_text(size = rel(1.2),
                               family = "Calibri"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 3, y = 56, label = "North Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3.2, y = 49, label = "English Channel", size = 5, family = "Calibri") +
  annotate("text", x = -7.7, y = 50.7, label = "Celtic Sea", size = 5, family = "Calibri") +
  annotate("text", x = -4.5, y = 54.5, label = "Irish Sea", size = 5, family = "Calibri") +
  annotate("text", x = -3, y = 44.2, label = "Biscay Bay", size = 5, family = "Calibri") +
  #north(x.min = -8, x.max = 7, y.min = 44, y.max = 56.3, 
  #      location = "topleft", 
  #      symbol = 12, 
  #      scale = 0.07) +
  north(x.min = -8, x.max = 4, y.min = 44.9, y.max = 50, 
        location = "bottomright", 
        symbol = 12, 
        scale = 0.1) +
  scalebar(x.min = -8, x.max = 6, y.min = 44.5, y.max = 56, 
           dist = 200, dist_unit = "km",
           transform = TRUE, model = "WGS84", 
           location = "bottomright",
           height = 0.01,
           st.size = 4)
  
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("Sampling",".png"), scale=2,
       plot = last_plot(),
       width = 11, height = 12, units = "cm",
       dpi = 300
)

##################################################

# 2.1.1 Discard sampling program ------------------------------------------

# Summary sampling effort (haul)
# Haul by IcesDivision
View(
  obs %>% group_by(IcesDivision, HaulID) %>% 
  summarize(n = n()) %>%
  group_by(IcesDivision) %>%
  summarize(n = n())
)

# Haul by Year
View(
  obs %>% group_by(IcesDivision, Year, HaulID) %>% 
    summarize(n = n()) %>% 
    group_by(IcesDivision, Year) %>%
    summarize(n = n()) %>%
    spread(key = Year, value = n)
)



##################################################

# 3.1	Discard ratios in Belgian beam trawl fisheries ----------------------

# Summary obs weigth  -----------------------------------------------------

obs_wt_new <- obs_wt %>% select(Year, HaulID, NameScientific, NameEnglish)
obs_wt_new <- obs_wt_new %>% mutate(D = obs_wt$D,
                                    L = obs_wt$L,
                                    DR = obs_wt$DR)
obs_new <- obs_wt_new %>% filter(HaulID %in% obs$HaulID)

# Summary all species - all sampling program
sum(obs_new$D) #Total discard
sum(obs_new$L) + sum(obs_new$D) # Total Catch
sum(obs_new$D)/(sum(obs_new$L) + sum(obs_new$D)) # Overall discard ratio

# Summary only studied taxa
sum(obs$D) #Total discard
sum(obs$L) + sum(obs$D) # Total Catch
sum(obs$D)/(sum(obs$L) + sum(obs$D)) # Overall discard ratio

# Most Discarded and caught species (sort by Discard biomass)
View(
  obs_new %>% group_by(NameScientific, NameEnglish) %>% summarize(D = sum(D, na.rm = T),
                                                                  L = sum(L, na.rm = T),
                                                                  C = D + L,
                                                                  DR = D/C) %>%
    arrange(desc(C))
)

# Skates - summary of all Rajidae family
ray_skate <- obs_new %>%
  filter(str_detect(NameEnglish, "ray|skate")) %>%
  group_by(NameScientific, NameEnglish) %>%
  summarize(D = sum(D),
            L = sum(L),
            C = D + L) %>%
  arrange(NameScientific)

sum(ray_skate$D) #sum D skates
sum(ray_skate$C) #sum C skates
sum(ray_skate$D)/sum(ray_skate$C) #DR

# Monkfish - summary of all monkfish (Lophius spp)
monkfish <- obs_new %>%
  filter(str_detect(NameEnglish, "angler|Angler")) %>%
  group_by(NameScientific, NameEnglish) %>%
  summarize(D = sum(D),
            L = sum(L),
            C = D + L) %>%
  arrange(NameScientific)

sum(monkfish$D) #sum D skates
sum(monkfish$C) #sum C skates
sum(monkfish$D)/sum(monkfish$C) #DR


# Distribution DR ---------------------------------------------------------
  
# sample size
sample_size = obs2 %>% group_by(NameEnglish) %>% summarize(num=n())

#Plot ridge
p1 <- obs2 %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(NameEnglish, "\n", "n=", num)) %>%
  ggplot(aes(x = DR, y=reorder(myaxis, DR), fill=NameEnglish)) +
  geom_density_ridges(alpha=0.8) +
  scale_fill_viridis(discrete = TRUE) +
  xlim(0,1) +
  labs(x = "Discard ratio",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p1


# Kruskal-Wallis test -------------------------------------
kruskal.test(DR ~ NameEnglish, data = obs2)
install.packages("dunn.test")
dunn.test::dunn.test(obs2$DR, obs2$NameEnglish) #dunn test
unique(sort(obs2$NameEnglish))

# Check anova and hsd tukey, the results are the same (no difference between hake and haddock)
# Thus we use the annotation generated from hsd.test to facilitate visualization of the results

# Anova and multiple comparison Tukey
aov <- aov(DR ~ NameEnglish, data = obs2)
summary(aov)

#Multiple comparison by means of Tukey 
hsd <-  agricolae::HSD.test(aov, "NameEnglish", group=T)
hsd
hsd_compare <-  agricolae::HSD.test(aov, "NameEnglish", group=F)
hsd_compare
hsd$groups

#Get group annotation
hsd_group <- as_tibble(hsd$groups) %>%
  mutate(NameEnglish = row.names(hsd$groups)) %>%
  arrange(NameEnglish)

p2 <- ggplot() + 
  geom_boxplot(data = obs2,mapping = aes(x = reorder(NameEnglish, DR), y = DR)) +
  geom_text(data = hsd_group, 
            aes(x = NameEnglish, y = 1.1, label = groups), 
            vjust = 0, family = "Calibri") +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = NULL,
       y = "Discard ratio") +
  coord_flip()

(p1|p2)


ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("Ridgeline plot2",".png"), scale=2,
       plot = last_plot(),
       width = 10, height = 8, units = "cm",
       dpi = 300
)


obs2 %>% group_by(NameEnglish) %>% 
  summarize(mean = mean(DR)) %>% 
  arrange(mean)

#Variation per taxon

nrow(obs2 %>% filter(NameEnglish == "Common sole", DR < 0.25))/nrow(obs2 %>% filter(NameEnglish == "Common sole")) #0.93
nrow(obs2 %>% filter(NameEnglish == "Lemon sole", DR < 0.25))/nrow(obs2 %>% filter(NameEnglish == "Lemon sole")) #0.61
nrow(obs2 %>% filter(NameEnglish == "Atlantic cod", DR < 0.25))/nrow(obs2 %>% filter(NameEnglish == "Atlantic cod")) #0.57
nrow(obs2 %>% filter(NameEnglish == "European plaice", DR < 0.25))/nrow(obs2 %>% filter(NameEnglish == "European plaice")) #0.39

nrow(obs2 %>% filter(NameEnglish == "Whiting", DR > 0.75))/nrow(obs2 %>% filter(NameEnglish == "Whiting")) #0.52
nrow(obs2 %>% filter(NameEnglish == "Haddock", DR > 0.75))/nrow(obs2 %>% filter(NameEnglish == "Haddock")) #0.51
nrow(obs2 %>% filter(NameEnglish == "European hake", DR > 0.75))/nrow(obs2 %>% filter(NameEnglish == "European hake")) #0.62
nrow(obs2 %>% filter(NameEnglish == "Common dab", DR > 0.75))/nrow(obs2 %>% filter(NameEnglish == "Common dab")) #0.70


##################################################

# 3.2	Variation in the three main regulatory and economic variables -------

# Limit utilization -------------------------------------------------------
# Limit utilization > 1
ggplot(data = obs, aes(x = haul_limit, y = DR)) + geom_point() + facet_wrap(~ NameEnglish)

library(ggridges)
ggplot(data = obs, aes(x = Quota_uti, y = NameEnglish, fill = NameEnglish)) + 
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# Quota_uti
obs_Q <- obs2 %>% group_by(NameEnglish, Quota_cut) %>% summarize(n = n()) %>%
  spread(key = Quota_cut, value = n) 
obs_Q[which(is.na(obs_Q$`Over limitations`) == T),]$`Over limitations` <- 0
obs_Q <- obs_Q %>% mutate(value.within = `Within limitations`/(`Over limitations`+`Within limitations`),
                          value.over = `Over limitations`/(`Over limitations`+`Within limitations`)
                          )
#obs_Q <- obs_Q %>% gather(key = "category", value = "value", value.within, value.over) %>% arrange(desc(value))

# Barplot over limitations

p1 <- ggplot(data = obs_Q, aes(x = reorder(NameEnglish, value.over), 
                         y = value.over, 
                         fill = reorder(NameEnglish,value.over)
                         )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Percentage of hauls over landing limitations") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  coord_flip()

# Density plot limit utilization [0,1]

p2 <- ggplot(data = obs2, 
       aes(x = Quota_uti, y = reorder(NameEnglish, Quota_uti), fill = reorder(NameEnglish, Quota_uti))
) + 
  geom_density_ridges(alpha=0.7) +
  scale_fill_viridis(discrete = TRUE) +
  #facet_wrap(~ reorder(NameEnglish, Quota_uti)) +
  xlim(0,1) +
  labs(x = "Limit utilization",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = rel(1),
                                  family = "Calibri"),
        legend.position = "none")

(p1|p2)
#(p1/p2)

ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("Limit utilization",".png"), scale=2,
       plot = last_plot(),
       width = 10, height = 4, units = "cm",
       dpi = 300
)

#Proportion of limit utilization
nrow(obs2 %>% filter(NameEnglish == "Common sole", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Common sole")) #0.26
nrow(obs2 %>% filter(NameEnglish == "Atlantic cod", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Atlantic cod")) #0.46
nrow(obs2 %>% filter(NameEnglish == "European plaice", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "European plaice")) #0.52

nrow(obs2 %>% filter(NameEnglish == "Haddock", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Haddock")) #0.59
nrow(obs2 %>% filter(NameEnglish == "Skates", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Skates")) #0.71
nrow(obs2 %>% filter(NameEnglish == "Whiting", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Whiting")) #0.85
nrow(obs2 %>% filter(NameEnglish == "European hake", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "European hake")) #0.88
nrow(obs2 %>% filter(NameEnglish == "Lemon sole", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Lemon sole")) #0.97
nrow(obs2 %>% filter(NameEnglish == "Common dab", Quota_uti < 0.1))/nrow(obs2 %>% filter(NameEnglish == "Common dab")) #1



# Price -------------------------------------------------------------------

# Mean fish price
obs_sale <- obs2 %>%
  group_by(NameEnglish, Month) %>% summarize(price_eurobykg = mean(price_eurobykg, na.rm = T)) 

ggplot() + 
  geom_line(data = obs_sale, 
            aes(x = Month, y = price_eurobykg, 
                color = reorder(NameEnglish, price_eurobykg),
                group = NameEnglish),
            size = 1, alpha = 0.8) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(breaks=seq(1,13,2)) +
  scale_x_continuous(breaks=seq(1,12)) +
  coord_cartesian(xlim = c(1,16)) +
  labs(x = "Month",
       y = "Fish price (EUR/kg)") +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_label_repel(data = obs_sale %>% filter(Month == last(Month)), aes(label = NameEnglish, 
                                                                         x = Month, 
                                                                         y = price_eurobykg),
                   nudge_x = 2,
                   force = 4,
                   na.rm = TRUE,
                   size = 2.5,
                   segment.size  = 0.3,
                   segment.color = "darkgrey")

# Summary fish price
obs_sale %>% filter(NameEnglish == "Atlantic cod") %>% summarize(max = max(price_eurobykg))

obs_sale %>% group_by(NameEnglish) %>% summarize(min = min(price_eurobykg),
                                                 max = max(price_eurobykg),
                                                 dif = max - min,
                                                 dif_perc = (dif/max)*100) %>%
  arrange(dif_perc)

obs_sale %>% group_by(NameEnglish) %>% summarize(min = min(price_eurobykg),
                                                 max = max(price_eurobykg),
                                                 dif = max - min,
                                                 dif_perc = (dif/max)*100) %>%
  arrange(dif)

#Save plot
ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("Fish price",".png"), scale=2,
       plot = last_plot(),
       width = 5, height = 8, units = "cm",
       dpi = 300
)

# Correlation fish price and DR
cor.test(obs$price_eurobykg, obs$DR, method=c("pearson"))

# LFD ------------------------------------------------------------------

# Subset the data of species being mapped
obs_len_freq$Length <- round(obs_len_freq$Length/10,0)
round(obs_len_freq$Length/10,0)  
summary(obs_len_freq$Length)

#obs_len_freq[which(obs_len_freq$NameEnglish == "Common dab"),"MLS"] <- 230
#obs_len_freq[which(obs_len_freq$NameEnglish == "Common sole"),"MLS"] <- 250

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% unique(obs2$NameEnglish), NameEnglish != "Skates") %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength))

unique(spp$NameEnglish)


# LFD plots ---------------------------------------------------------------

spp_list_all <- unique(obs2[which(obs2$NameEnglish != "Skates"),]$NameEnglish)

# Plot from p1 to p8
spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[1]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p1 <- ggplot(data = spp,
            aes(x = Length, 
                y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p1
p1 <- p1 +   annotate("text", x = (MCRS + 20), 
               y = 40000, 
               label = paste0("MCRS"," (",MCRS," cm)"), 
               size = 3, 
               family = "Calibri") 
p1

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[2]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p2 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p2
p2 <- p2 +   annotate("text", x = (MCRS + 20), 
                      y = 20000, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p2

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[3]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p3 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p3
p3 <- p3 +   annotate("text", x = (MCRS + 20), 
                      y = 150000, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p3

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[4]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p4 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p4
p4 <- p4 +   annotate("text", x = (MCRS + 20), 
                      y = 150000, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p4

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[5]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p5 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p5
p5 <- p5 +   annotate("text", x = (MCRS + 20), 
                      y = 35000, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p5

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[6]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p6 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p6
p6 <- p6 +   annotate("text", x = (MCRS + 30), 
                      y = 2500, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p6

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[7]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p7 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p7
p7 <- p7 +   annotate("text", x = (MCRS + 30), 
                      y = 11000, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p7

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all[8]) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

MCRS <- unique(spp$MLS/10)
mean_min <- min(spp$Length)
mean_max <- max(spp$Length)
p8 <- ggplot(data = spp,
             aes(x = Length, 
                 y = Number_atlength)) +
  geom_line(size = 0.5, aes(linetype = FateCategoryCode)) + 
  facet_wrap(~ NameEnglish) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_vline(xintercept = spp$MLS/10, size = 0.5, linetype="dotted") +    
  labs(x = "Length (cm)",
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1),
                                 family = "Calibri"),
        axis.title = element_text(size = rel(1),
                                  family = "Calibri",
                                  face = "bold"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
p8
p8 <- p8 +   annotate("text", x = (MCRS + 30), 
                      y = 17000, 
                      label = paste0("MCRS"," (",MCRS," cm)"), 
                      size = 3, 
                      family = "Calibri") 
p8

library(patchwork)
(p1|p2|p3|p4)/(p5|p6|p7|p8) +
  plot_annotation()

ggsave(path = paste0(dir_figure,"/"),
       filename = paste0("Length frequency",".png"), scale=2,
       plot = last_plot(),
       width = 12, height = 7, units = "cm",
       dpi = 300
)



# Discards above MCRS -----------------------------------------------------
summary(obs_len_freq$Length)

spp <- obs_len_freq %>% 
  filter(NameEnglish %in% spp_list_all) %>% 
  group_by(FateCategoryCode, NameScientific, NameEnglish, Length, MLS) %>%
  summarize(Number_atlength = sum(Number_atlength)) 

spp_over <- spp %>% filter(FateCategoryCode == "D", Length > MLS) %>% 
  group_by(NameEnglish) %>%
  summarize(n_over = sum(Number_atlength))

spp_D <- spp %>% filter(FateCategoryCode == "D") %>% 
  group_by(NameEnglish) %>%
  summarize(n_D = sum(Number_atlength))

spp2 <- left_join(spp_over, spp_D)
spp2 <- spp2 %>% mutate(over_perc = n_over/n_D*100) %>% arrange(over_perc)
spp2

############################################








