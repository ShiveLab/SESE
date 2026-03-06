library(sf)
library(dplyr)
library(janitor)
library(terra)
library(tidyverse)
library(here)

options(scipen = 999)

#####################################
##This code selects SESE sample areas, starting from scratch due to severity debacle!!
#####################################


#import all CA fire perimeters, subset to >=2003
perims1 = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Data\\Fire\\CalFire\\fire24_1.gdb", layer = "firep24_1") %>%
  filter(YEAR_ >= 2003) %>%
  st_cast("MULTIPOLYGON") 
# %>%
#   st_cast("POLYGON")
perims.crs = st_crs(perims1)

#bring in Chetco Bar (Oregon fire) perimeter
chetco = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\archive\\burnseverity_mtbs\\2017_ChetcoBarFire\\or4229712395420170712_20151013_20171018_burn_bndy.shp") %>%
  st_transform(perims.crs) %>%
  mutate(StartDay = as.numeric(152), StartMonth = as.numeric(258),
         Fire_ID = paste(Year,"-",Fire_Name, sep = ""),
         area_ha = Acres/2.471) %>%
  rename(FIRE_NAME = Fire_Name, Shape = geometry, YEAR_ = Year) %>%
  select(YEAR_, FIRE_NAME, Fire_ID, area_ha, StartDay, StartMonth) %>%
  group_by(YEAR_, FIRE_NAME, Fire_ID, StartDay, StartMonth) %>%
  summarise(area_ha = sum(area_ha))
chetco

##combine chetco with CA fires
perims = perims1 %>%
  bind_rows(chetco)
perims

#import SESE range and project to fire perimeters
sese = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\final_calveg_rd_woodward.shp") %>%
  st_transform(perims.crs) %>%
  st_cast("POLYGON")
sese

# # get all fires that intersect SESE range to have a list
# sese.fires = st_filter(perims, sese, .predicate = st_intersects) %>%
# nrow(sese.fires)

#now clip the fire perims to SESE range itself
sese.fires.clp = perims %>%
  st_intersection(sese)
sese.fires.clp

#confirm clipping worked right?
czu = sese.fires.clp %>%
  filter(FIRE_NAME == "CZU LIGHTNING COMPLEX")
plot(czu)

#erase all overlapping pieces
intersections.fires = st_intersection(sese.fires.clp) %>%
  st_collection_extract("POLYGON") %>%
  st_cast("POLYGON") %>%
  select(YEAR_, FIRE_NAME, n.overlaps, origins) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001))
intersections.fires

#now check the intersection still makes sense
czu = intersections.fires %>%
  filter(FIRE_NAME == "CZU LIGHTNING COMPLEX")
plot(czu)

#get rid of any reburn areas
no.twice.perims = intersections.fires[intersections.fires$n.overlaps == 1, ]
no.twice.perims$area_ha = as.numeric(st_area(no.twice.perims)*0.0001)
no.twice.perims

no.twice.perims.ex = no.twice.perims %>%
  summarise(.by = c(YEAR_, FIRE_NAME), 
          geometry = st_union(st_combine(Shape)))
no.twice.perims.ex

#make sure that worked
czu = no.twice.perims.ex %>%
  filter(FIRE_NAME == "CZU LIGHTNING COMPLEX")
plot(czu)

sob = no.twice.perims %>%
  filter(FIRE_NAME == "SOBERANES")
plot(sob)

#now erase old growth
sese.og = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Data\\Forest\\Coast redwood range\\old_growth_and_older_forest_on_public_lands.shp") %>%
  st_transform(perims.crs) %>%
  st_cast("POLYGON")

# Erase sampling area that is covered by old growth
no.twice.no.og <- st_difference(no.twice.perims.ex, st_union(sese.og)) %>%
  st_make_valid() 
# %>%
#   st_cast("MULTIPOLYGON")
no.twice.no.og$area_ha = as.numeric(st_area(no.twice.no.og)*0.0001)
nrow(no.twice.no.og)
# View(no.twice.no.og)

no.twice.no.og.u = no.twice.no.og %>%
  summarise(.by = c(YEAR_, FIRE_NAME), 
            geometry = st_union(st_combine(geometry))) %>%
   st_cast("MULTIPOLYGON")
no.twice.no.og.u

czu = no.twice.no.og.u %>%
  filter(FIRE_NAME == "CZU LIGHTNING COMPLEX")
plot(czu)

no.twice.no.og10 = no.twice.no.og %>%
  filter(area_ha > 30) %>%
  mutate(Fire_ID = paste(YEAR_,"-",FIRE_NAME, sep = "")) %>%
  st_make_valid()
nrow(no.twice.no.og10)

sample.area.all = st_union(no.twice.no.og10)

# View(no.twice.no.og10)

czu = no.twice.no.og10 %>%
  filter(FIRE_NAME == "CZU LIGHTNING COMPLEX")
plot(czu)

st_write(no.twice.no.og10, here("outputs\\output_spatial_data\\poss_sampling_sese_from_R_over30ha_2Feb2026.shp",
         delete_dsn = TRUE)
# c = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\fires_ex_10.shp")
# head(c)
# class(c$NAME) - char
# class(c$Fire_ID) - char
# class(c$Year) - integer
# class(c$StartDay) - numeric
# class(c$StartMonth) - numeric

#selecting final full fire perimeters based on the selected fires list for running GEE
list.of.fires.needed.sese = st_drop_geometry(no.twice.no.og10)

sese.selected.fire.perims = perims %>%
  select(YEAR_, FIRE_NAME) %>%
  inner_join(list.of.fires.needed.sese) %>%
  rename(Year = YEAR_) %>%
  mutate(StartDay = as.numeric(152), StartMonth = as.numeric(258), area_ha = as.numeric(st_area(.)*0.0001)) %>%
 #this area filter is actually just to get rid of an extraneous 2017 Bear and 2008 Indian - both were in perims and so was kept, but are out of SESE range  
  filter(area_ha > 11)
sese.selected.fire.perims
nrow(sese.selected.fire.perims)

# #bring in Chetco Bar (Oregon fire) perimeter
# chetco = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\burnseverity_mtbs\\2017_ChetcoBarFire\\or4229712395420170712_20151013_20171018_burn_bndy.shp") %>%
#   st_transform(perims.crs) %>%
#   mutate(StartDay = as.numeric(152), StartMonth = as.numeric(258),
#          Fire_ID = paste(Year,"-",Fire_Name, sep = ""),
#          area_ha = Acres/2.471) %>%
#   rename(FIRE_NAME = Fire_Name, Shape = geometry) %>%
#   select(Year, FIRE_NAME, Fire_ID, area_ha, StartDay, StartMonth) %>%
#   group_by(Year, FIRE_NAME, Fire_ID, StartDay, StartMonth) %>%
#   summarise(area_ha = sum(area_ha))
# chetco
# 
# ##combine chetco with CA fires
# sese.selected.fire.perims.all = sese.selected.fire.perims %>%
#   rbind(chetco)
# sese.selected.fire.perims.all
st_write(sese.selected.fire.perims, "C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\sese_poss_fire_perims_for_gee_2Feb2026.shp")

##################################################################################
#create severity layer
##################################################################################

###############################################################
#get severity polygons from rdnbr tifs from GEE
rast.dir = ("C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/shapes/fire_severity_rdnbr_tifs/")
rast.list <- list.files(rast.dir, pattern='tif$', full.names = T)
rast.list.names <- list.files(rast.dir, pattern='tif$', full.names = F)

#check all same crs
for(i in 1:length(rast.list)) {
  rast.i = rast(rast.list[i])
  j = i+1
  rast.j = rast(rast.list[12])
  print(same.crs(rast.i,rast.j))
}

#################################################################
##creating cbi layer with high severity extra split
#classify and turn each raster into polys
reclass_matrix <- tibble(
  from = c(-32769, 69, 315, 640),
  to = c(69, 315, 640, 9992),
  becomes = c(1, 2, 3, 4)
)

##first make classified rasters
for(i in 1:length(rast.list)){
  rast.1 = rast(rast.list[i])
  # names(rast.1) = "rdnbr"
  imf <- rast.1 %>% 
    classify(reclass_matrix,
             right = T) # include "from" value in category
  writeRaster(imf, paste("C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/shapes/fire_severity_rdnbr_tifs/classified_r/",gsub('.{10}$', '', as.character(rast.list.names[i])),"_CBI4_r.tif",sep = ''), overwrite = T)
  
}

#get list of now classified rasters
cls.rast.dir = ("C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/shapes/fire_severity_rdnbr_tifs/classified_r/")
cls.rast.list <- list.files(cls.rast.dir, pattern='tif$', full.names = T)
cls.rast.list.names <- list.files(cls.rast.dir, pattern='tif$', full.names = F)


##convert to polys
for(i in 1:length(cls.rast.list)){
  rast.1 = rast(cls.rast.list[i])
  names(rast.1) = "rdnbr"
  imf <- rast.1 %>%
    as.polygons() %>% 
    st_as_sf() %>%
    mutate(burnsev = rdnbr,
           burnsev_text = case_when(burnsev == 1 ~ "und_chg",
                                    burnsev == 2 ~ "low",
                                    burnsev == 3 ~ "moderate",
                                    burnsev == 4 ~ "high",
                                    .default = "oopsy"),
           year = paste(substr(as.character(rast.list.names[i]),1,4)),
           fireyr = paste(gsub('.{10}$', '', as.character(rast.list.names[i])))
    )
  write_sf(imf, paste("C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/shapes/fire_severity_rdnbr_tifs/classified_r/polys_r/",gsub('.{10}$', '', as.character(rast.list.names[i])),"_CBI5_polys_r.shp",sep = ''))
  
}

#merge polys
shapes.dir <- ("C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/shapes/fire_severity_rdnbr_tifs/classified_r/polys_r/")
shapes.list <- list.files(shapes.dir, pattern="\\.shp$", full.names=TRUE)
shapes.all = lapply(shapes.list,st_read)

cbi_all_ks <- do.call(rbind, shapes.all)
cbi = cbi_all_ks %>%
  st_transform(crs(perims))
st_write(cbi, "C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/shapes/fire_severity_rdnbr_tifs/classified_r/polys_r/fire_severity_sese_full_fire_areas_3Feb2026.shp")

#clip to sese fire areas
cbi.sese = cbi %>%
  st_intersection(sample.area.all) %>%
  st_make_valid() %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001)) %>%
  st_cast("MULTIPOLYGON")
cbi.sese

czu = cbi.sese %>%
  filter(fireyr == "2008-COWSHED_LIGHTNING")
plot(czu["brnsv_t"])

cbi.sese.tbl = cbi.sese %>%
  st_drop_geometry() %>%
  mutate(YEAR_ = substr(fireyr, 1, 4), fire = str_extract(fireyr, "(?<=-).*"),
         FIRE_NAME = gsub("_", " ", fire, fixed = TRUE),
         fireyr = gsub("_", " ", fireyr, fixed = TRUE)) %>%
  select(YEAR_, FIRE_NAME, fireyr, brnsv_t, area_ha) %>%
  # group_by(YEAR_, brnsv_t) %>%
  # summarise(area_ha = sum(area_ha)) %>%
  pivot_wider(names_from = brnsv_t, values_from = area_ha, values_fill = 0) %>%
  relocate(und_chg, .after = fireyr) %>%
  relocate(high, .after = moderate) %>%
  mutate(total_ha = high+low+moderate+und_chg, 
         across(c(und_chg, low, moderate, high, total_ha), ~round(., 1)))
cbi.sese.tbl
write.csv(cbi.sese.tbl, "C:/Users/kshive/Documents/UCB/GIS/Projects/SESE_fuels/tables/sese_severity_within_poss_sample_areas_3Feb2026.csv")
#

##################################################################################
##################################################################################
##################################################################################





