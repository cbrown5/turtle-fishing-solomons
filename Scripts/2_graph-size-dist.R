#Graphs of size distribution by species and community
# CJ Brown 2022-04-20

rm(list = ls())
library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)


theme_set(theme_bw())

load("data-raw/turtle-data-files.rda")


harvest$Community[grep("Auki", harvest$Community)] <- "Radefasu"
#
# Size distribution overall
#
datmature <- data.frame(species_id = c("Green", "Hawksbill"), sizemat = c(90, 75), 
                        x = c(0.5, 1.5), xend = c(1.5, 2.5)) # add lines
gsizes <- 
  filter(harvest,species_id %in% c("Green", "Hawksbill")) %>% 
  ggplot(aes(x = species_id, y = `CCL (cm)`, fill = species_id)) + 
  geom_violin() + 
  scale_fill_manual(values = c("DarkGreen", "orange")) +
  geom_segment(data = datmature, aes(x = x, xend = xend, y = sizemat, yend = sizemat),
               size = 1.5, linetype = 1, color = "grey40") + 
  xlab("Species") + 
  ylab("Carapace length (cm)") + 
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave("Shared/Outputs/size-dist-by-spp.eps", gsizes,
       width = 3, height = 3, device = "eps")


#
# Size distribution by region
#


gsizes_region <- 
  filter(harvest,species_id %in% c("Green", "Hawksbill")) %>% 
  ggplot(aes(x = species_id, y = `CCL (cm)`, fill = species_id)) + 
  geom_violin() + 
  geom_segment(data = datmature, aes(x = x, xend = xend, y = sizemat, yend = sizemat),
               size = 1.5, linetype = 1, color = "grey40") + 
  facet_wrap(~Community) + 
  scale_fill_manual(values = c("DarkGreen", "orange")) +
  xlab("Species") + 
  ylab("Carapace length (cm)") + 
  theme(legend.title = element_blank())

ggsave("Shared/Outputs/size-dist-by-region.eps", gsizes_region,
       width = 6, height = 6, device = "eps")

#
# Patchwork version
#

gboth <- 
gsizes + gsizes_region + 
  plot_annotation(tag_levels = "A")

ggsave("Shared/Outputs/size-dist-plots.eps", gboth,
       width = 12, height = 4, device = "eps")


