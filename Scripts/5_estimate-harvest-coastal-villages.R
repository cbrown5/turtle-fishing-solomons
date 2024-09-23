# Estimate SI wide harvest by multiplying up
# from the number of villages
#
# CJ Brown 
# 2022-04-20

rm(list = ls())
library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(fasterize)
library(tmap)

abnormal_comms <- c("Kia", "Wagina", "Buala") 
#communitess with abnormal harvesting , Remove from basic
#calculations (they are special cases with market forces 
#driving extra harvest)

load("data-raw/turtle-data-files.rda")

reefs <- st_read(dsn = "Shared/Data/Turtle Reef Areas 2019 Jan 24/Turtle Reef Areas 2019 Jan 24", 
        "Turtle_Reef_Areas_2019_0124")
sols <- st_read(dsn = "Shared/Data/SI_admin", 
                "SLB_adm0")
sols2 <- st_transform(sols, st_crs(reefs))

solvil <- st_read(dsn = "Shared/Data/sols-villages", "Solomon_Settlements")
solvil2 <- st_transform(solvil, st_crs(reefs))

 reefs_si <- st_read(dsn = "Shared/Data/Solomons - MCRM Reefs", 
                     "Coral_Reef_MCRM_2018_SI") %>% st_transform(st_crs(reefs))

 reefs_si_all <- reefs_si %>%
   filter(!(RB_ATTRIB %in% c("land", "non-reef")))
  reefs_si2 <- reefs_si_all %>%
   summarize()
  
#Missing temotu reefs 
reefs_temotu <- st_read(dsn = "Shared/Data/IMARS - EastSolomons/",
                          "EastSolomons") %>%
    st_transform(st_crs(reefs)) %>%
  filter(!(RB_ATTRIB %in% c("land", "non-reef"))) %>%
  summarize()
  
 
#load

#reef area convex hulls
load("data-raw/reef_conv.rda")
#use alpha hulls
# load("data-raw/reef_aconv.rda")

#coastal villages
load("data-raw/sv_coastal.rda")

#Harvest sum
load("data-raw/harvest_sum.rda")

#
# Distance layers
#

#Create raster
rbase <- raster(sv_coastal, res = c(3000, 3000))
rbase <- extend(rbase, c(50, 50))
rland <- fasterize(sols2, rbase)
#  r_reef_area <- fasterize(reefs_si2, rbase, fun = "count")

#Distance to village 
rvilldist <- distanceFromPoints(rbase, st_coordinates(sv_coastal))
rvilldist[rland[]==1] <- NA
#Point centre of reefs
reef_pt <- st_centroid(reefs)

# Distance of reefs to nearest village 
reef_pt$dist <- extract(rvilldist, reef_pt)

#Join reefs to harvest and Calculate 'distance footprint'
harv_dist <- left_join(harvest, reef_pt, by = c("reef_id" = "REEF_ID")) %>%
  group_by(Community) %>%
  summarize(dist = max(dist, na.rm = TRUE))

# Join distances to standardized harvest
harvest_sum <- harvest_sum %>% inner_join(harv_dist)

#Reef area per hull, using all data, not just reef polygons
reef_area_comms <- st_intersection(reef_conv, reefs_si2)
reef_area_comms$reef_area_allreefs <- st_area(reef_area_comms)

reef_area_comms_temotu <- st_intersection(reef_conv, reefs_temotu)
reef_area_comms_temotu$reef_area_allreefs <- st_area(reef_area_comms_temotu)

#Join temotu and rest of MCMR data 
reef_area_comms_all <- rbind(reef_area_comms, reef_area_comms_temotu)

# Get average harvest per unit area reefs for 'normal' villages 
#(remove Kia and Wagina from averages)
reef_area_comms_all$total_area <- as.numeric(reef_area_comms_all$reef_area_allreefs)/
  (10000*10)
units(reef_area_comms_all$total_area) <- units::as_units("10ha")
harv_areas <- harvest_sum %>% inner_join(reef_area_comms_all) %>%
  mutate(harv_per_10ha = nharv_est/total_area)
harv_areas$harv_per_10ha

# Harvest estimate just given footprint size 
turtle_stats <- harv_areas %>% filter(!(Community %in% abnormal_comms)) %>%
  summarize(mdist = mean(dist), mharv = mean(nharv_est), 
            mharv_10ha = mean(harv_per_10ha), 
            max_harv = max(harv_per_10ha), 
            min_harv = min(harv_per_10ha))

#
# Empirical prob estimates
#
focal_comms <- harv_areas %>% filter(!(Community %in% abnormal_comms)) 
ncomms <- nrow(focal_comms)

#'Weighted distances' calculate weightings based on empirical distribution'
#of distances
P <- ecdf(focal_comms$dist)
plot(seq(0, 15000, 100), 1-P(seq(0, 15000, 100)), type = "l")


rdistprob <- raster(rvilldist)
rdistprob[] <- 1-P(rvilldist[])
# plot(rdistprob)

# Get reef centroid
reef_cent <- st_centroid(reefs_si_all)
# get reef distance and area
reef_cent$reef_dist <- extract(rvilldist, reef_cent)
reef_cent$reef_dist[is.na(reef_cent$reef_dist)] <- 0
reef_cent$area <- as.numeric(st_area(reefs_si_all))
#Convert distance to probability
reef_cent$reef_pr <- 1-P(reef_cent$reef_dist)
reefs_si_all$harv_prob <- reef_cent$reef_pr
# plot(reefs_si_all["harv_prob"])

#
#Bootstrap over reefs
#

#Bootstrap over empirical estimates of harvest per unit area 
area_conversion <- 1/(10 * 10000) #convert per m2 to per 10ha

nreps <- 5000
nreefs <- nrow(reef_cent)
set.seed(42)
xout <- matrix(nrow = nreps, ncol = 1)
vals <- as.matrix(focal_comms$harv_per_10ha)

#Data to sample from CDF
xy <- sortedXyData(sort(vals), 1-(1:ncomms)/ncomms)
for (reps in 1:nreps){ #bootstrap over random samples of distances
  irand_reef <- reef_cent$reef_pr > runif(nreefs) #random reefs to include
  areatemp <- sum(reef_cent$area[irand_reef]) * area_conversion
  
  #Random sample of distance 
  harv_temp <- stats::NLSstClosestX(xy, runif(1))
  #random sample of empirical harvest, from the CDF
  xout[reps] <- areatemp * harv_temp #multiply two to get harvest estimate. 
}

hist(xout, 100, plot = TRUE)
dat_xout <- data.frame(xout = xout)

g1 <- ggplot(dat_xout) + 
  aes(x = xout) + 
  geom_density(fill = "lightblue") + 
  geom_vline(aes(xintercept=median(xout)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=quantile(xout, 0.025)),
             color="blue", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=quantile(xout, 0.975)),
             color="blue", linetype="dashed", size=0.5) +
  xlim(0, 22000)+
  xlab("Harvest estimate") + 
  ylab("Density") +
  theme_classic()

# ggsave(g1, file = "Shared/Outputs/annual-turtle-typical.png",
       # width = 6, height = 4)

quantile(xout, c(0.025, 0.5, 0.975))

#Map of harvest probabilities

redpal <- c("white", RColorBrewer::brewer.pal(4, "Reds"))

tm1 <- tm_shape(rland) + 
  tm_raster(palette = "grey30",
            legend.show = FALSE) +
  tm_shape(rdistprob) + 
  tm_raster( palette = redpal,
             title= 'Prob. \n harvest') +
  tm_layout(legend.position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"),
               size = 0.8) +
  tm_compass(size = 1.2) +
  tm_layout(title = "A")

tm2 <- tm_shape(rland, 
                bbox = st_bbox(c(xmin = 140942, xmax = 762153, 
                ymin = 8842195, ymax = 9296513),
                crs = st_crs(rland))
                  ) + 
  tm_raster(palette = "grey30",
            legend.show = FALSE) +
  tm_shape(rdistprob) + 
  tm_raster( palette = redpal,
             title= 'Prob. \n harvest') +
  tm_layout(legend.position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"),
               size = 1.2) +
  tm_compass(size = 0.8)  +
  tm_layout(title = "B")

tmboth <- tmap_arrange(tm1, tm2)

# tmap_save(tmboth, 
          # filename = "Shared/Outputs/harvest-prob-maps.png",
          # width = 5, height = 7)


# save(rdistprob, file = "data-raw/turtle-harvest-prob.rda")

#
# Add back in  villages with abnormally high harvest
#

n_special <- 4 #estimated number of special villages SI wide
xout_total <- xout + 
  n_special*(filter(harv_areas, Community == "Kia")$nharv_est) + 
  (filter(harv_areas, Community == "Wagina")$nharv_est) +
  (filter(harv_areas, Community == "Buala")$nharv_est)

round(quantile(xout_total, c(0.025, 0.5, 0.975)))
sum(xout_total>5800)/length(xout_total)

write.csv(xout_total, "Shared/Outputs/turtle-SSF-bootstraps.csv",
          row.names = FALSE)

#Proportion in hotspots
(n_special*(filter(harv_areas, Community == "Kia")$nharv_est) + 
  (filter(harv_areas, Community == "Wagina")$nharv_est) +
  (filter(harv_areas, Community == "Buala")$nharv_est))/
  median(xout_total)

