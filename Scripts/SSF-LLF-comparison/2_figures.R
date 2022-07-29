# Upscaling turtle catch 
# CJ Brown
# 2022-07-29

# Load packages
library(ggplot2)
library(scales)
 
load("data-raw/LLF-national-ttx-catch.rda")

dplot <- dout$catch2018_CIs %>%
  rbind(data.frame(Type = "Small-scale fishery",
                   X2.5. = 5862,
                   X50. = 11184,
                   X97.5. = 23717 ,
                   mean = NA))
dplot$Type <- c("Long-line fishery: \n catch rate method",
                "Long-line fishery: \n catch ratio method",
                "Small-scale fishery: \n (Hamilton et al. subm)")
dplot$Fishery <- c("LLF", "LLF", "SSF")

g1 <- ggplot(dplot) + 
  aes(x = Type, y = X50., color = Fishery) +
  geom_point(size = 2) +
  geom_linerange(
    aes(ymin = X2.5., ymax = X97.5.),
    size = 1) +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30,
                                   hjust = 1)) +
  ylim(0,25000)+
  xlab("") +
  ylab("Estimated turtle \n catch for 2018") +
  scale_y_continuous(labels = comma)
g1

ggsave(g1, 
       filename = "Shared/Outputs/catch-comparison/national-turtle-catch-estimates.png",
       width = 3, height = 3)


