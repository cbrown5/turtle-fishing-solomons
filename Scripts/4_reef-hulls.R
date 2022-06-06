# Maps of harvest 
# CJ Brown 
# 2022-04-20


rm(list = ls())
library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)

# Identify 'coastal' villages - those within 800 metres of the coast 
coastal_threshold <- 800

load("data-raw/turtle-data-files.rda")

reefs <- st_read(dsn = "Shared/Data/Turtle Reef Areas 2019 Jan 24/Turtle Reef Areas 2019 Jan 24", 
        "Turtle_Reef_Areas_2019_0124")
sols <- st_read(dsn = "Shared/Data/SI_admin", 
                "SLB_adm0")
sols2 <- st_transform(sols, st_crs(reefs))

solvil <- st_read(dsn = "Shared/Data/sols-villages", "Solomon_Settlements")
solvil2 <- st_transform(solvil, st_crs(reefs))
load("data-raw/harvest_sum.rda")

#Fix typo in reef provinces
reefs$Province[reefs$Province == "Gaudalcanal"] <- "Guadalcanal"

reefs_si <- st_read(dsn = "Shared/Data/Solomons - MCRM Reefs", 
                    "Coral_Reef_MCRM_2018_SI") %>% st_transform(st_crs(reefs))

reefs_si_all <- reefs_si %>%
  filter(!(RB_ATTRIB %in% c("land", "non-reef")))
reefs_si2 <- reefs_si_all %>%
  summarize()


#
# Spatial analysis
#

hreef <- harvest %>% 
  filter(!is.na(reef_id)) %>%
  group_by(reef_id, Community, Province) %>% 
  summarize(nturtles = n()) %>% 
  left_join(harvest_sum) %>% 
  rename(REEF_ID = reef_id)

reeft <- left_join(reefs, hreef, by = c("REEF_ID", "Province")) %>%
  mutate(nturt_est = nturtles * (1/prop_days) * (365/duration) * hunter_mult)

# plot(reeft["nturtles"])

#Maps for each region 
mypal <-  RColorBrewer::brewer.pal(8, "Reds")
g <- NULL
for (prov in unique(reeft$Province)){
  # prov <- "Isabel"
  reeftempt <- filter(reeft, Province == prov)
  ext <- st_bbox(reeftempt)
  sols3 <- st_crop(sols2, ext)
   gt <- 
    ggplot() + 
            geom_sf(data = sols3, fill = "wheat", color = NA) +
      geom_sf(data = reeftempt, aes(color = nturt_est, fill = nturt_est), alpha = 0.8) +
     scale_fill_gradientn(name = "Number \n of turtles", 
                          colors = mypal, na.value = NA) + 
     scale_color_gradientn(name = "Number \n of turtles", 
                          colors = mypal, na.value = "black") + 
     
       # coord_sf(xlim = c(ext[1], ext[3]),
       #          ylim = c(ext[2], ext[4]),
       #   datum = st_crs(reeftempt)) +
      labs(title = prov)
   gt
  
   g <- c(g, list(gt))
}

ggsave("Shared/Outputs/maps.pdf",
       device = "pdf",
       marrangeGrob(g, nrow = 1, ncol = 1))

#
# Convex hulls for community fishing footprint
#

reef_turtles <- filter(reeft, nharv>0)
comms <- unique(reef_turtles$Community)

reef_conv <- NULL

for (icomm in comms){
  xtemp1 <- reef_turtles %>%
    filter(Community == icomm)
  xtemp2 <- xtemp1 %>% 
    st_union() %>%
    st_convex_hull()
  comm_att <- data.frame(Community = xtemp1$Community[1],
                         reef_area = sum(st_area(xtemp1)))
  reef_conv <- c(reef_conv, list(st_sf(comm_att, geometry = xtemp2)))
              
}

reef_conv <- do.call("rbind", reef_conv)

save(reef_conv, file = "data-raw/reef_conv.rda")



gm <- ggplot() + 
  geom_sf(data = sols2, fill = "wheat", color = NA) +
  geom_sf(data = reeft, aes(color = nturt_est, fill = nturt_est), alpha = 0.8) + 
  geom_sf(data = reef_conv)

ggsave("Shared/Outputs/map-footprint.png", gm)


#
# Villages and upscaling 
#

#main method, but using saved file now 
# sols2_lines <- sols2 %>% st_cast("MULTILINESTRING")
# sols2_coast <- st_buffer(sols2_lines, dist = coastal_threshold)
# sv_coastal <- st_intersection(solvil2, sols2_coast)
# 
# save(sv_coastal, sols2_lines,sols2_coast, file = "data-raw/sv_coastal.rda")

load("data-raw/sv_coastal.rda")

#
#plotting to check each province
#
doplot <- FALSE 
if (doplot){
ggplot() +
  # geom_sf(data = sols2_coast) +
  geom_sf(data = solvil2, fill = "black", size = 0.5) +
  geom_sf(data = sv_coastal, color = "red", fill = "red", size = 0.5)

unique(harvest$Province)
prov <- "Temotu"
reeftempt <- filter(reeft, Province == prov)
ext <- st_bbox(reeftempt)
sv3 <- st_crop(solvil2, ext)
x <- st_crop(sols2_coast, ext)
xpoly <- st_crop(sols2, ext)
y <- st_crop(sv_coastal, ext)

#Check how it looks
# library(tmap)
# tmap_mode("view")
#   tm_shape(reefs_si2) + 
#   tm_polygons() +
#     tm_shape(reeftempt) + 
#     tm_fill(col = "red") 
  
# 
ggplot() +
  geom_sf(data = xpoly, fill = 'wheat') +
  geom_sf(data = x, col = "darkblue", fill = "lightblue", alpha = 0.25) +
   geom_sf(data= sv3, col = "red", size = 3) +
  geom_sf(data = y, col = "darkblue", size = 3)


}
