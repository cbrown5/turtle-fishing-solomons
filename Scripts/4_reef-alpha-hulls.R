# Maps of harvest 
# CJ Brown 
# 2022-04-20


rm(list = ls())
library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)
library(alphahull)
library(igraph)
# functions from Cecina Babich Morrow for converting ashape 
# to lines https://babichmorrowc.github.io/post/2019-03-18-alpha-hull/
ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape$edges[,1] <- as.character(ashape$edges[,1])
  ashape_graph <- graph_from_edgelist(ashape$edges[,1:2], directed = FALSE)
  if (!is.connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - E(ashape_graph)[1]
  # Find chain end points
  ends = names(which(degree(cut_graph) == 1))
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(V(ashape_graph)[path]$name)
  # join the ends
  pathX = c(pathX, pathX[1])
  return(pathX)
}

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

distmult <- 5 #multiple of mean edge length to use
# for setting alpha parameter
reef_conv <- NULL

for (icomm in comms){
  # icomm <- comms[1]
  xtemp1 <- reef_turtles %>%
    filter(Community == icomm) 
  xtemp2 <- xtemp1 %>%
    # st_centroid() %>%
    st_coordinates()
  #voronio 
  xdup <- duplicated(xtemp2[,1])
  ydup <- duplicated(xtemp2[,2])
  i <- !(xdup & ydup)
  reef_vor <- data.frame(delvor(xtemp2[i,1:2])$mesh)
  
  #mean distance of edges
  vor_dists <- sqrt((reef_vor$mx1 - reef_vor$mx2)^2 + (reef_vor$my1 - reef_vor$my2)^2)
  alpha <- mean(vor_dists) * distmult
  
  #Do ashape and convert to sf polygon
  reef_ahull <- ashape(xtemp2[i,1:2], alpha = alpha)
  ihull <- try( ashape2poly(reef_ahull))
  
  #use convex hull for when linear shapes when
  # area gets split into multiple polygons
  #doing so adds more water area  to the hulls,
  # but doesn't add reef area
  if (class(ihull) == "try-error"){
    polcrs <- xtemp1 %>% 
      st_union() %>%
      st_convex_hull() 
  } else {
  x <- data.frame(xtemp2[i,][ihull,])
  polbasic <- st_polygon(
    list(cbind(x$X, 
        x$Y)))
  polcrs <- st_sfc(polbasic, crs=st_crs(xtemp1))
  }
  #Make a list of all ahulls
  comm_att <- data.frame(Community = xtemp1$Community[1],
                         reef_area = sum(st_area(xtemp1)))
  reef_conv <- c(reef_conv, list(st_sf(comm_att, geometry = polcrs)))
              
}

reef_conv <- do.call("rbind", reef_conv)

save(reef_conv, file = "data-raw/reef_aconv.rda")



gm <- ggplot() + 
  geom_sf(data = sols2, fill = "wheat", color = NA) +
  geom_sf(data = reeft, aes(color = nturt_est, fill = nturt_est), alpha = 0.8) + 
  geom_sf(data = reef_conv)

ggsave("Shared/Outputs/map-footprint_acnov.png", gm)


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
