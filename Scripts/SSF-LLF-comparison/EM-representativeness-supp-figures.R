#Plots of representativeness of EM compared to logbook 

# Load packages
library(tidyverse)
library(ggplot2)
library(patchwork)

emdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/EM-data.csv")
logdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/logbook-data.csv")
logsets <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/logbook-sets.csv")
emlogdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/em_logbook_matched.csv")


logsets2018 <- logsets %>%
  filter(grepl("2018", Date)) %>%
  #make depart_date and return_date date-time objects
  mutate(Depart_date = as.Date(depart_date),
         Return_date = as.Date(return_date)) %>%
  mutate(trip_duration = Return_date - Depart_date,
         has_em = if_else(Trip_ID %in% emlogdat$Trip_ID,
                          "Yes",
                          "No"))

log_target_spp2018 <- logdat %>%
  #group_by Set_ID and identify primary species caught
  group_by(Trip_ID, Set_ID, sp_code) %>%
  summarize(n = sum(n)) %>%
  #identify main species caught per set
  top_n(1, n) %>%
  ungroup() %>%
  group_by(Trip_ID, sp_code) %>%
  #identify main species caught per trip
  summarize(n = sum(n)) %>%
  top_n(1, n)

table(logsets2018$has_em)

theme_set(theme_minimal())

#Plot density of trip durations
g1_duration_density <- logsets2018 %>% 
  #make unique trip database
  distinct(Trip_ID, trip_duration, has_em) %>%
ggplot(aes(x = trip_duration, fill = has_em)) +
  geom_density(color = NA, alpha = 0.5) +
  labs(x = "Trip duration (days)",
       y = "Density",
       fill = "Has EM?") 

ggsave(g1_duration_density, 
       filename = "Outputs/trip-duration-density.png",
       width = 5, height = 3)


#Plot density of hooks
g2_hooks_density <- logsets2018 %>% 
  #make unique trip database
  distinct(Trip_ID, hooks_n , has_em) %>%
  ggplot(aes(x = hooks_n, fill = has_em)) +
  geom_density(color = NA, alpha = 0.5) +
  labs(x = "Hooks per set",
       y = "Density",
       fill = "Has EM?") 
g2_hooks_density

ggsave(g2_hooks_density, 
       filename = "Outputs/hooks-density.png",
       width = 5, height = 3)


#density of months fished
g3_month_density <- logsets2018 %>% 
  mutate(month = month(Depart_date)) %>%
  #make unique trip database
  distinct(Trip_ID, month , has_em) %>%
  ggplot(aes(x = month, fill = has_em)) +
  geom_density(color = NA, alpha = 0.5) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  labs(x = "Month fished",
       y = "Density",
       fill = "Has EM?")
g3_month_density

# barplot of main target species
g4_target_spp <- log_target_spp2018 %>%
  inner_join(logsets2018, by = "Trip_ID") %>%
  ggplot(aes(x = has_em, fill = sp_code)) +
  #add a barplot that shows sets as a proportion of total
  geom_bar(position = "fill") +
  labs(x = "Has EM?",
       y = "Proportion of trips",
       fill = "Species") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Put plots together 

gplots <- g1_duration_density + g2_hooks_density + 
  g3_month_density +g4_target_spp+
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels  = "A")

ggsave(gplots, 
       filename = "Outputs/representativeness-plots.png",
       width = 8, height = 6)
