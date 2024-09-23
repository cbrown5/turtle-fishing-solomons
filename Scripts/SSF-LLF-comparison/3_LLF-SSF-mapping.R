#Map footprints of the two fisheries 

#CJ Brown 2022-07-29
#
# Make sure we use same resolution for both maps
#Need to remake logsets density in other project then bring across here

library(sf)
library(tidyverse)
library(tmap)

logsets <- read.csv("Shared/Data/LLF-data/logbook-data.csv")
sols <- st_read(dsn = "Shared/Data/SI_admin", 
                "SLB_adm0")
load("data-raw/turtle-harvest-prob.rda")
#load data from other project: 
load()

solsutm <- st_transform(sols, st_crs(rdistprob)) %>%
  st_simplify(dTolerance = 1000)

slogsets <- logsets %>% dplyr::select(Set_ID, lat, lon, Date) %>%
  distinct() %>%
  st_as_sf(., coords = c("lon", "lat"))
st_crs(slogsets) <- st_crs(sols)

slogsetsutm <- st_transform(slogsets, st_crs(rdistprob))

#
# Spatial footprint
#
llf_hull <- st_convex_hull(st_union(slogsetsutm))
st_area(llf_hull)

#
# Mapping 
#

#turtle harvest prob
tm1 <- tm_shape(solsutm) +
  tm_fill() +
  tm_shape(rdistprob) +
  tm_raster() #+
  # tm_shape(slogsets) +
  # tm_dots()

#logsets
tm2 <- tm_shape(rlogsets, bbox = sols_bbox) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(eez) + 
  tm_borders() + 
  tm_shape(sols2) +
  tm_fill(col = "grey20") + 
  tm_layout() + 
  tm_graticules(alpha = 0.1)



tmap_save(tm1, filename = "Shared/Outputs/catch-comparison/SSF-map.png")




