library(sf)
library(dplyr)
library(here)


sev.asp.owner.selected = ##add shapefile here, 
  #after you filter for low and high sev and meant aet

##make a grid across the potential sample areas
xgrid200.a <- sev.asp.owner.selected %>%
  st_make_grid(cellsize = c(200,200), what = "centers") %>% # grid of points
  st_as_sf()

##bring in slope tif
#exclude >50%


#assign location/strata, using the aspect/severity/owner layer
xgrid200 = xgrid200.a %>%
  st_intersection(sev.asp.owner.selected)

#randomly pick 8 each per severity/aspect class and location/year?