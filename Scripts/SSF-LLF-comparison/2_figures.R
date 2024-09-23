# Upscaling turtle catch 
# CJ Brown
# 2022-07-29

# Load packages
library(ggplot2)
library(scales)
library(dplyr)
 
load("Data/LLF-national-ttx-catch.rda")

dplot <- dout$catch2018_CIs %>%
  rbind(data.frame(Type = "Small-scale fishery",
                   X2.5. = 5862,
                   X50. = 11184,
                   X97.5. = 23717 ,
                   mean = NA))
dplot$Type <- c("Long-line fishery: \n catch rate method",
                "Long-line fishery: \n flag method",
                "Long-line fishery: \n catch ratio method",
                "Small-scale fishery: \n (Hamilton et al. 2023)")
dplot$Fishery <- c("LLF", "LLF", "LLF", "SSF")

dplot$X50.[4]/dplot$X50.[1]
dplot$X50.[4]/dplot$X50.[2]
dplot$X50.[4]/dplot$X50.[3]

g1 <- ggplot(dplot) + 
  aes(x = Type, y = X50., color = Fishery) +
  geom_point(size = 3) +
  geom_linerange(
    aes(ymin = X2.5., ymax = X97.5.),
    linewidth = 1) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   size = 9)) + 
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000), 
                     limits = c(0, 25000)) +
  xlab("") +
  ylab("Estimated turtle \n catch for 2018") 
g1

ggsave(g1, 
       filename = "Outputs/national-turtle-catch-estimates.png",
       width = 3, height = 3)

#
# Figure S1
#
emdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/EM-data.csv")
hist(emdat$hooks_per_float)
g2 <- ggplot(emdat) + 
  aes(x = hooks_per_float) +
  geom_histogram() +
  xlab("Hooks between floats") +
  theme_classic() 
ggsave(g2, 
       filename = "Outputs/hks-btw-floats.png",
       width = 5, height = 3)
