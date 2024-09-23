#Map footprints of the two fisheries 

#CJ Brown 2022-07-29
#Sort out logbook data, 
# why is it in a grid? Look

library(sf)
library(tidyverse)
library(tmap)
library(terra)

logsets <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/logbook-sets.csv")
sols <- st_read(dsn = "../turtle-fishing-solomons/Shared/Data/SI_admin", 
                "SLB_adm0")
load("../turtle-fishing-solomons/data-raw/turtle-harvest-prob.rda")
load("Data/logsets-density-map.rda")


save(eez2, sols3,sols_bbox, file = "Data/logsets-density-map.rda")
rlogsets2 <- rast("Data/logsets_raster.GPKG")

#
# Coastline map 
#

gcoasts <- st_read("C:/Users/cjbrown0/Dropbox/sesync-analysis/data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries_v2.gpkg")

newcrs <- st_crs("+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
paccosts <- gcoasts %>% st_transform(newcrs) %>%
  st_crop(xmin = -1000000, ymin = -3000000, xmax = 4000000,ymax = 00000)
plot(paccosts[1])

#
# Sols data 
#
solsutm <- st_transform(sols, st_crs(rdistprob)) %>%
  st_simplify(dTolerance = 1000)

slogsets <- logsets %>% dplyr::select(Set_ID, Lat, Lon, Date_obs) %>%
  distinct() %>%
  st_as_sf(., coords = c("Lon", "Lat"))
st_crs(slogsets) <- st_crs(sols)

slogsetsutm <- st_transform(slogsets, st_crs(rdistprob))


#set minimum value of rdistprob to 0
rdistprob[rdistprob <= 0] <- NA

#
# Spatial footprint
#
llf_hull <- st_convex_hull(st_union(slogsetsutm))
#this is an overestimate as the EEZ isn't a convex hull
#convert to km2
st_area(llf_hull)*1e-6
st_area(eez2)
as.numeric(st_area(llf_hull))/as.numeric(st_area(eez2))

st_area(eez2)
#convert rdistprob to polygon
rdistprob2 <- rdistprob
rdistprob2[rdistprob <= 0.2] <- NA
pdistprob <- rast(rdistprob2) %>%
  as.polygons() %>%
  st_as_sf() %>%
  filter(layer ==1)

plot(pdistprob)
st_area(pdistprob)
#convert m2 to km2
st_area(pdistprob)*1e-6

#divide two areas
st_area(eez2)/st_area(pdistprob)

#
# Mapping 
#

#SSF
tm1 <- tm_shape(solsutm, bbox = sols_bbox) +
  tm_fill(col = "grey20") +
  tm_shape(rdistprob) +
  tm_raster(title = "SSF probability") +
  tm_shape(eez2) + 
  tm_borders() + 
  tm_scale_bar(text.size = 1) +
  tm_layout(legend.position = c("right", "top")) + 
  tm_credits(c("A"), position = c("left", "top"),
  size = 1.5)
  # tm_shape(slogsets) +
  # tm_dots()
tm1

#LLF

tm2 <- tm_shape(rlogsets2, bbox = sols_bbox) + 
  tm_raster(legend.show = TRUE, title = "LLF set density") + 
  tm_shape(eez2) + 
  tm_borders() + 
  tm_shape(sols3) +
  tm_fill(col = "grey20") + 
  tm_layout() +
  tm_scale_bar(text.size = 1) +
  tm_layout(legend.position = c("right", "top"))+
  tm_credits(c("B"), position = c("left", "top"),
  size = 1.5)

tm2

tm_inset <- tm_shape(paccosts) + 
  tm_fill() + 
  tm_shape(eez2) + 
  tm_borders(col = "lightblue", lwd = 3) + 
  tm_shape(sols3) +
  tm_fill(col = "black") + 
  tm_layout() +
  tm_credits(c("C"), position = c("left", "top"),
             size = 1.5)+
  tm_layout(inner.margins = c(0.04,0.04,0.04,0.04), 
            outer.margins=c(0,0,0,0))
tm_inset


#use tmap_arrange to map tm1 and tm2
tm3 <- tmap_arrange(tm1, tm2, nrow = 1)
  
tm3
#save tm3 as a png, set width and height
tmap_save(tm3,
          filename = "Outputs/SSF-LLF-maps.png",
          width = 8, height = 4, units = "in", dpi = 300)

tmap_save(tm_inset,
          filename = "Outputs/SI_inset.png",
          width = 4, height = 4, units = "in", dpi = 300)


#Make map with inset
w <- 1
xy <- st_bbox(paccosts)
asp2 <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)
h <- w*asp2

library(grid)
vp <- viewport(x=.5, y=.5, width = w, height=h, 
               just=c("left", "bottom"))

#save tm3 as a png, set width and height
tmap_save(tm3,
          filename = "Outputs/SSF-LLF-maps_inset.png",
          insets_tm = tm_inset, insets_vp = vp,
          width = 8, height = 4, units = "in", dpi = 300)


#Try grob code - doesn't work
library(gridExtra)
library(tmaptools)
library(cowplot)

main_gg <- tmap_grob(tm3)
inset_gg <- tmap_grob(tm_inset)

dev.new(width = 4.944444, height = 3.055556, noRStudioGD = T)
final_figure <- 
  ggdraw() +
  draw_plot(main_gg, x = 0.02, y = 0.05, 
            width = 1, height = 0.82) +
  draw_plot(inset_gg, x = -0.1, y = 0.3, 
            scale = 0.25)
final_figure
