##this selects plot locations based on severity & refined poss sampling area
library(sf)
library(dplyr)
library(tidyverse)
library(here)

options(scipen = 999)

##########################################
##########################################
##note - this code works in R but cannot write out shapefile of
##severity/aspect/cpad intersected due to linestrings created
##did the intersection in ArcPro
##########################################
##########################################



##all poss fire areas (no OG, no reburn, no industrial)
samp.area = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\poss_sampling_sese_from_R_over30ha_2Feb2026.shp") %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001))
sum(samp.area$area_ha)
head(samp.area)

###keep fires actually under consideration
samp.fires = read.csv(here("outputs/output_tables/sese_sample_areas_byFireYrSevAET_quant_25perc_2March2026.csv")) %>%
  filter(aet_class == "~mean aet") %>%
  select(fireyr) %>%
  mutate(fireyr = gsub(" ", "_", fireyr, fixed = TRUE))
head(samp.fires)
unique(samp.fires$fireyr)

#severity
sese.sev = st_read(here("outputs\\output_spatial_data\\fire_severity_sese_full_fire_areas_3Feb2026.shp")) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001)) %>%
  inner_join(samp.fires)
# sum(sese.sev$area_ha)
sese.sev
unique(sese.sev$fireyr)
st_write(sese.sev, here("outputs\\output_spatial_data\\sese.sev_ONLY_geomCheck_5March2026.shp"))

sa.diss = st_union(samp.area)
st_write(sa.diss, here("outputs\\output_spatial_data\\sa.diss_ONLY_geomCheck_5March2026.shp"))

poss.sev.samp = sese.sev %>%
  filter(burnsev %in% c(2,4)) %>%
  select(-rdnbr, -burnsev) %>%
  st_intersection(sa.diss) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),1)) %>%
  # group_by(fireyr) %>%
  # filter(all(area_ha > 10)) %>%
  # ungroup() %>%
#remove Glass as too far south
  filter(fireyr != "2020-GLASS")
poss.sev.samp

st_write(poss.sev.samp, here("outputs\\output_spatial_data\\poss.sev.samp_ONLY_geomCheck_5March2026.shp"))


#intersect wtih aspect - classified into N/E and S/W
##R says its too large to convert to polygons even after clipping, so did it in Arc and bringing inas polys

# library(terra)
# asp = rast("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\SESE_fuels\\shapes\\aspect_reclassified_1_NE_5_SW.tif") %>%
#   project(samp.area)
# same.crs(asp, samp.area)
# clipped_extent_asp <- mask(asp, samp.area)
# plot(clipped_extent_asp)
# # convert to polys and sf
# asp_poly <- as.polygons(clipped_extent_asp, dissolve=FALSE) %>%
#   filter(Aspect_reclassified > 0)
# asp_poly_sf = st_as_sf(asp_poly)

asp = st_read(here("outputs\\output_spatial_data\\SESE_aspect_polys_clp_diss.shp")) %>%
  st_transform(st_crs(samp.area))
st_crs(samp.area) == st_crs(asp)
asp

sev.asp = poss.sev.samp %>%
  st_intersection(asp) %>%
  rename(burnsev = brnsv_t) %>%
  st_make_valid() %>%
  filter(st_is_valid(geometry)) %>%
  st_polygon(st_union(linestring_sf)) %>%
  st_cast("POLYGON")
sev.asp
st_write(sev.asp, here("outputs\\output_spatial_data\\sese_potential_fires_sev_aspect_ONLY_geomCheck_5March2026.shp"))

##import CPAD to get ownership
cpad = st_read("C:\\Users\\kshive\\Documents\\UCB\\GIS\\Data\\CPAD\\CPAD_2023\\CPAD_2023a_SuperUnits.shp") %>%
  select(ACCESS_TYP, PARK_NAME, MNG_AGNCY)
#check matching crs
st_crs(sese.sev) == st_crs(cpad)

#intesect with cpad
sev.asp.cpad = sev.asp %>%
  st_intersection(cpad) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),1))  %>%
  filter(area_ha > 2) %>%
  st_make_valid() %>%
  filter(st_is_valid(geometry)) %>%
  st_polygon(st_union(linestring_sf)) %>%
  st_cast("POLYGON")

# %>%
#   group_by(fireyr, asp_class) %>%
#   filter(all(area_ha > 5)) %>%
#   ungroup()
sev.asp.cpad
# View(sev.asp.cpad)
# st_write(sev.asp.cpad, here("outputs\\output_spatial_data\\sese_potential_fires_sev_aspect_5March2026.shp"))

