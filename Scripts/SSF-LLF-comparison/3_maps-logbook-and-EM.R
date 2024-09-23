# Map and basic stats for logbook and EM data
# 
# 2023-07-31

#
# Deviance residuals 
# residuals(m1, type = "deviance")

library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(terra)
library(patchwork)
library(mgcv)

emdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/EM-data.csv")
logdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/logbook-data.csv")
logsets <- read.csv("Data/LLF-data/logbook-sets.csv")
emlogdat <- read.csv("Data/LLF-data/em_logbook_matched.csv")

eez <- st_read("Data/eez_sols.gpkg")
sols2 <- st_read("Data/sol_islands_simplified.gpkg")

#load for CRS
load("../turtle-fishing-solomons/data-raw/turtle-harvest-prob.rda")


# ------------ 
# Functions 
# ------------ 

#Identify cells that fall outside a region to remove from raster 
rm_boundary <- function(r, xbound){
  #Identify region outside of convex hull
  ivals <- extract(r, xbound, xy = TRUE)
  allcells <- 1:ncell(r)
  #cells to remove
  icells_rm <- allcells[!allcells %in% cellFromXY(r, ivals[,c("x", "y")])]
  return(icells_rm)
}

#convert kernal density to raster
density_to_raster <- function(dat, n = 100, crs = "epsg:4326"){
  #Use negative of latitude so density matrix
  # is in correct orientation for conversion
  # to raster (otherwise it is mirrow image)
  de <- MASS::kde2d(dat$Lon, -dat$Lat,
                    n = n) 
  rlogsets <- rast(t(de[[3]]),
                   extent = c(min(de$x), max(de$x),
                              min(-de$y), max(-de$y)),
                   crs = crs)
  
  icells <- rm_boundary(rlogsets, eez)
  #Set values outside to NA
  rlogsets[icells] <- NA
  return(rlogsets)
}

# ------------ 
# Define region 
# ------------

sols_bbox <- eez %>%
  st_transform(st_crs(rdistprob)) %>%
  st_bbox()
eez2 <- st_transform(eez, st_crs(rdistprob))
sols3 <- st_transform(sols2, st_crs(rdistprob))
# ------------ 
# Maps of data distribution 
# ------------ 

#
# logbook
#
slogsets <- st_as_sf(logsets, 
                     coords = c("Lon", "Lat"),
                     crs = 4326) %>%
  st_transform(st_crs(rdistprob))

rlogsets <- density_to_raster(logsets, 100)
rlogsets2 <- project(rlogsets, crs(rdistprob))

tm1 <- tm_shape(rlogsets2, bbox = sols_bbox) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(eez2) + 
  tm_borders() + 
  tm_shape(sols3) +
  tm_fill(col = "grey20") + 
  tm_layout() +
  tm_scale_bar()
tm1

#save for use in turtle upscaling maps 
writeRaster(rlogsets2, filename = "Data/logsets_raster.GPKG")
save(eez2, sols3,sols_bbox, file = "Data/logsets-density-map.rda")

# tmap_save(tm1, filename = "Outputs/logbook-set-density.png")
#
# EM data 
#
emsets <- emdat %>%
  select(Set_ID, Lon, Lat) %>%
  distinct()

semsets <- st_as_sf(emsets, 
                     coords = c("Lon", "Lat"),
                     crs = 4326)
remsets <- density_to_raster(emsets, 100)

tm2 <- tm_shape(remsets, bbox = sols_bbox) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(eez) + 
  tm_borders() + 
  tm_shape(sols2) +
  tm_fill(col = "grey20") + 
  tm_shape(semsets) + 
  tm_dots(size = 0.05, alpha = 0.5) + 
  tm_graticules(alpha = 0.1)
tmap_save(tm2, filename = "Outputs/EM-set-density.png")

# ------------ 
# Fishing by flag
# ------------ 

flags <- unique(logsets$Flag)[-5] #remove single Korea set
tm_flags <- NULL
for (iflag in flags){
  rlogsets_temp <- density_to_raster(filter(logsets,Flag == iflag), 100)  
  tm_temp <- tm_shape(rlogsets_temp, bbox = sols_bbox) + 
    tm_raster(legend.show = FALSE) +
    tm_shape(eez) + 
    tm_borders() + 
    tm_shape(sols2) +
    tm_fill(col = "grey20") + 
    tm_graticules(alpha = 0.1) +
    tm_layout(title = iflag,
              title.size = 2,
              title.position = c("right", "top"))
  tm_flags <- c(tm_flags, list(tm_temp))
}

tm_flags <- tmap_arrange(tm_flags[[1]], tm_flags[[2]], 
             tm_flags[[3]], tm_flags[[4]], 
             tm_flags[[5]])

tmap_save(tm_flags, filename = "Outputs/flags-set-density.png")

# ------------ 
# Species catch rates in EM
# ------------ 

#Set-up raster of lon and lat to predict to with Terra
rLon <- rast(remsets)
rLat <- rast(remsets)
xrast <- xFromCell(rLon, cells(rLon))
rLon[] <- xrast
yrast <- yFromCell(rLat, cells(rLat))
rLat[] <- yrast
rpred <- c(rLon, rLat)
names(rpred) <- c("Lon", "Lat")

#Set-up boundary for cropping predictions
em_region <- st_convex_hull(st_union(semsets)) %>%
  #Buffer by 10km
  st_buffer(10000) %>%
  #convert to polygon that can be used by extract
  st_as_sf() 
#Cells to remove 
icells_rm1 <- rm_boundary(rLat, em_region)
icells_rm2 <- rm_boundary(rLat, eez)
icells_rm <- c(icells_rm1, icells_rm2)

sharks <- c("BSH", "FAL", "BTH", "LMA", "OCS", 
            "PSK", "RSK", "SKH", "SHX", "SMA", "THR", "SPY")
emdat <- emdat %>%
  mutate(sp_new = if_else(Sp_code %in% sharks,
                          "Shark",
                          Sp_code))
spp_names <- c("ALB", "BET", "YFT","Shark")

tm_species <- NULL
for (ispp in spp_names){
  sp <- filter(emdat, sp_new == ispp) %>%
    select(Set_ID, n, Fate)
  
  sp1 <- sp %>% 
    group_by(Set_ID) %>%
    summarize(n = sum(n)) %>%
    full_join(emsets)
  
  sp2 <- filter(sp1, n>0) %>%
    st_as_sf(coords = c("Lon", "Lat"),
             crs = 4326)
  
  m1 <- gam(n ~ s(Lon, Lat, bs = "sos", k=15),
            data  = sp1,
            family = "poisson")
  
  rsp_pred <- predict(rpred, m1,
                      type = "response")
  #set cells outside region to NA
  rsp_pred[icells_rm] <- NA
  
  tm_temp <- tm_shape(rsp_pred, bbox = sols_bbox) + 
    tm_raster(title = ispp) +
    tm_shape(eez) + 
    tm_borders()+
    tm_shape(sols2) + 
    tm_fill(col = "grey20") + 
    tm_shape(sp2) + 
    tm_dots(size = 0.05, alpha = 0.3) + 
    tm_graticules(alpha = 0.1) +
    tm_layout(#title = ispp,
              # title.size = 0.5,
              # title.position = c("right", "top"),
              # legend.position = c("bottom", "right"),
              legend.outside = TRUE)
  tm_species <- c(tm_species, list(tm_temp))
  
}

tm_spp_em <- tmap_arrange(tm_species[[1]], tm_species[[2]], 
             tm_species[[3]], tm_species[[4]])
tmap_save(tm_spp_em, filename = "Outputs/EM-species-catch.png",
          width = 10, height = 8)

# ------------ 
# Turtles
# ------------ 

em_ttx <- filter(emdat, Sp_code == "TTX" & n>0) %>%
  st_as_sf(coords = c("Lon", "Lat"),
           crs = st_crs(semsets))

tmttx <- tm_shape(eez) + 
  tm_borders()+
  tm_shape(sols2) + 
  tm_fill(col = "grey20") + 
  tm_shape(em_ttx) + 
  tm_dots(size = 0.1, alpha = 1, col = "red") + 
  tm_graticules(alpha = 0.1)

tmap_save(tmttx, 
          filename = "Outputs/turtles-map.png",
          width = 4, height = 4)


# ------------ 
# Species catch rates in logbooks
# ------------ 

#Set-up raster of lon and lat to predict to with Terra
rLon <- rast(rlogsets)
rLat <- rast(rlogsets)
xrast <- xFromCell(rLon, cells(rLon))
rLon[] <- xrast
yrast <- yFromCell(rLat, cells(rLat))
rLat[] <- yrast
rpred <- c(rLon, rLat)
names(rpred) <- c("Lon", "Lat")

#Set-up boundary for cropping predictions
em_region <- st_convex_hull(st_union(slogsets)) %>%
  #Buffer by 10km
  st_buffer(10000) %>%
  #convert to polygon that can be used by extract
  st_as_sf() 
#Cells to remove 
icells_rm1 <- rm_boundary(rLat, em_region)
icells_rm2 <- rm_boundary(rLat, eez)
icells_rm <- c(icells_rm1, icells_rm2)

sharks <- c("BSH", "FAL", "BTH", "LMA", "OCS", 
            "PSK", "RSK", "SKH", "SHX", "SMA", "THR", "SPY")
logdat <- logdat %>%
  mutate(sp_new = if_else(sp_code %in% sharks,
                          "Shark",
                          sp_code))
spp_names <- c("ALB", "BET", "YFT","Shark")

tm_species <- NULL
for (ispp in spp_names){
  sp <- filter(logdat, sp_new == ispp) %>%
    select(Set_ID, n, fate_code)
  
  sp1 <- sp %>% 
    group_by(Set_ID) %>%
    summarize(n = sum(n)) %>%
    full_join(logsets)
  
  m1 <- gam(n ~ s(Lon, Lat, bs = "sos", k=15),
            data  = sp1,
            family = "poisson")
  
  rsp_pred <- predict(rpred, m1,
                      type = "response")
  #set cells outside region to NA
  rsp_pred[icells_rm] <- NA
  
  tm_temp <- tm_shape(rsp_pred, bbox = sols_bbox) + 
    tm_raster(title = ispp) +
    tm_shape(eez) + 
    tm_borders()+
    tm_shape(sols2) + 
    tm_fill(col = "grey20") + 
    tm_graticules(alpha = 0.1) +
    tm_layout(#title = ispp,
      # title.size = 0.5,
      # title.position = c("right", "top"),
      # legend.position = c("bottom", "right"),
      legend.outside = TRUE)
  tm_species <- c(tm_species, list(tm_temp))
  
}

tm_spp_log <- tmap_arrange(tm_species[[1]], tm_species[[2]], 
                          tm_species[[3]], tm_species[[4]])
tm_spp_log

tmap_save(tm_spp_log, filename = "Outputs/Logbook-species-catch.png",
          width = 10, height = 8)
