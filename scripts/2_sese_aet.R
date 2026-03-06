library(terra)
library(dplyr)
library(sf)
library(ggplot2)
library(rgeoda)
library(readxl)
library(tidyverse)

##reading in the possible sampling areas, which is subset to the SESE range
##and excludes OG, twice burned and private industrials
sese.fires = vect("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\poss_sampling_sese_from_R_over30ha_2Feb2026.shp")
head(sese.fires)


##reading in the AET raster
aet1 = rast("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Data\\Climate\\aet1991_2020_ave.asc")
#defining the existing project for R - that the AET projection as CA Teale Albers, NAD83
crs(aet1) <- "EPSG:3310"

#now projecting AET to the same projection as the fire layer (doesn't matter which is projected to which, they just need to be the same)
aet = aet1  %>%
  project(sese.fires)
#confrming they are the same projection
same.crs(aet, sese.fires)

#################
################
##get aet representative of range
sese = vect("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\final_calveg_rd_woodward.shp") %>%
  project(sese.fires)
#confrming they are the same projection
same.crs(sese, sese.fires)
same.crs(sese, aet)
sese

#mask out sese range only
aet_sese <- mask(aet, sese)
plot(aet_sese)

#get the interquartile range for the whole range
aet_sese_tbl <- as.data.frame(aet_sese) %>%
  filter(aet1991_2020_ave > 0)
sese.range.quant = quantile(aet_sese_tbl$aet1991_2020_ave, probs = seq(0, 1, 0.2))
#creates a list, so the below just defines the quantiles to plug in later
sese.quant.25 = as.numeric(sese.range.quant[2])
sese.quant.75 = as.numeric(sese.range.quant[4])


##now get the aet for the sampling areas only
#to later convert to polygons, you need to first crop to the bounding box
#and then clip (mask) to the potential sampling polygons
aet_fires_sese_crp <- crop(aet, sese.fires)
aet_fires_sese <- mask(aet_fires_sese_crp, sese.fires)
plot(aet_fires_sese)

#convert the selected aet areas to polygons
#setting dissolve as false so it retains one record for each pixel
aet.polys.sese <- as.polygons(aet_fires_sese, dissolve=FALSE)

##########
##now switching to sf because its easier for me to work with polygons/vectors

#convert the aet polys to an sf file and summarize aet
aet_polys_sese_sf = st_as_sf(aet.polys.sese) %>%
  #get rid of aet < 0
  filter(aet1991_2020_ave >= 0) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry()
head(aet_polys_sese_sf)
summary(aet_polys_sese_sf)

# writeRaster(aet_clp_sese, "C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\aet_sese_fires_clp.tif", overwrite = TRUE)

#plot histogram to check out aet with range quantiles
h = ggplot(aet_polys_sese_sf, aes(aet1991_2020_ave)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_vline(xintercept = sese.quant.25, col = "red") +
  geom_vline(xintercept = sese.quant.75, col = "red")
h


##########
##now switching to sf because I know it better for polygons/vectors

#convert the fires polys to an sf file
sese.fires_st = st_as_sf(sese.fires) %>%
  mutate(Fire_ID = gsub("  ", " ", Fire_ID, fixed = TRUE)) %>%
  mutate(Fire_ID = gsub("(", "", Fire_ID, fixed = TRUE)) %>%
  mutate(Fire_ID = gsub(")", "", Fire_ID, fixed = TRUE))
sese.fires_st

#convert the aet polys to an sf file
aet_polys_st = st_as_sf(aet.polys.sese) %>%
 #intersect the aet polys to the fire polys, so that the fire name and year are now attached to the aet polys
  st_intersection(sese.fires_st) %>%
 #get rid of < 0
  filter(aet1991_2020_ave >= 0) %>%
 #create new column with unique fire/year identifier and calc ha
  mutate(fire_yr = paste(YEAR_,"-", FIRE_NAME,sep = ''), 
         area_ha = as.numeric(st_area(.)*0.0001))
head(aet_polys_st)

#drop the geometry so you are left with just a dataframe
aet_plot_df = st_drop_geometry(aet_polys_st)  %>%
  mutate(FIRE_NAME = gsub("  ", " ", FIRE_NAME, fixed = TRUE)) %>%
  mutate(FIRE_NAME = gsub("(", "", FIRE_NAME, fixed = TRUE)) %>%
  mutate(FIRE_NAME = gsub(")", "", FIRE_NAME, fixed = TRUE))

ggplot(aet_plot_df, aes(fire_yr, aet1991_2020_ave,weight = area_ha)) +
  geom_boxplot(varwidth = T) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  ylim(200,900) +
  geom_hline(yintercept = sese.quant.25, color = "red") +
  geom_hline(yintercept = sese.quant.75, color = "red")

options(scipen=999)

aet.new.sum = aet_plot_df %>%
  group_by(YEAR_, FIRE_NAME) %>%
  summarise(mn.aet = round(mean(aet1991_2020_ave),1), mdn.aet = round(median(aet1991_2020_ave),1),
            sd.aet = round(sd(aet1991_2020_ave),1), min.aet = round(min(aet1991_2020_ave),1), 
            max.aet = round(max(aet1991_2020_ave),1), area_ha = round(sum(area_ha),1)) %>%
  arrange(mn.aet)
aet.new.sum 
unique(aet.new.sum$FIRE_NAME) 
hist(aet.new.sum$mn.aet, breaks = 22)
abline(v = sese.quant.25, col = "red")
abline(v = sese.quant.75, col = "red")

aet.new.sum.ref = aet.new.sum %>%
  # filter(area_ha>= 81) %>%
  mutate(aet_class = case_when(mn.aet < sese.quant.25 ~ "very dry",
                               mn.aet >= sese.quant.25 & mn.aet < sese.quant.75 ~ "~mean aet",
                               mn.aet > sese.quant.75 ~ "very wet")) %>%
  select(YEAR_, FIRE_NAME, aet_class)
aet.new.sum.ref
# View(aet.new.sum.ref)
table(aet.new.sum.ref$YEAR_, aet.new.sum.ref$aet_class)
# View(aet.new.sum.ref)
write.csv(aet.new.sum.ref, here("outputs/output_tables/fires_with_aet_class_6March2026.csv"))

          