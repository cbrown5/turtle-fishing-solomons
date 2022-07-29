#Map footprints of the two fisheries 

#CJ Brown 2022-07-29
#Sort out logbook data, 
# why is it in a grid? Look

library(sf)
library(tidyverse)
library(tmap)

logsets <- read.csv("Shared/Data/LLF-data/logbook-data.csv")
sols <- st_read(dsn = "Shared/Data/SI_admin", 
                "SLB_adm0")
load("data-raw/turtle-harvest-prob.rda")

solsutm <- st_transform(sols, st_crs(rdistprob)) %>%
  st_simplify(dTolerance = 1000)

slogsets <- logsets %>% dplyr::select(Set_ID, lat, lon, Date) %>%
  distinct() %>%
  st_as_sf(., coords = c("lon", "lat"))
st_crs(slogsets) <- st_crs(sols)

slogsetsutm <- st_transform(slogsets, st_crs(rdistprob))
slogsetsutm$x <- 1
#
# Mapping 
#

tm1 <- tm_shape(solsutm) +
  tm_fill() +
  tm_shape(rdistprob) +
  tm_raster() #+
  # tm_shape(slogsets) +
  # tm_dots()

tmap_save(tm1, filename = "Shared/Outputs/catch-comparison/SSF-map.png")




