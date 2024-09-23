
#this script is out of date

# Load packages
library(tidyverse)
library(ggplot2)

emdat <- read.csv("Shared/Data/LLF-data/EM-data.csv")
logdat <- read.csv("Shared/Data/LLF-data/logbook-data.csv")
logsets <- read.csv("Shared/Data/LLF-data/logbook-sets.csv")
emlogdat <- read.csv("Shared/Data/LLF-data/em_logbook_matched.csv")

ssf_bootstraps <- read.csv("Shared/Outputs/turtle-SSF-bootstraps.csv")

turtles <- c("TTX")
nsamp <- 10000 #samples for bootstrapping

# ------------ 
# Basic stats
# ------------ 

#
#Total number of turtles and EM sets
#
x <- emdat %>% filter((Sp_code == turtles) & (n > 0))
sum(x$n)
unique(x$Vessel)
unique(x$Flag)
length(unique(emdat$Set_ID))

#
# logbook stats
#

logsets2018 <- logsets %>%
  filter(grepl("2018", Date))
nsets2018 <- length(unique(logsets2018$Set_ID))
length(unique(logsets2018$Trip_ID))
length(unique(logsets2018$vessel_name))

#trip duration
x = logsets2018 %>%
  mutate(trip_len=as.Date(return_date) - as.Date(depart_date)) %>%
  group_by(Trip_ID) %>%
  summarize(trip_len = mean(trip_len)) 
quantile(x$trip_len)
# ------------ 
# Per set catch rate upscaling
# ------------ 

ttx_catch <- emdat %>% filter((Sp_code == turtles)) %>%
  group_by(Set_ID) %>%
  summarize(n = sum(n)) 
n_em_sets <- nrow(ttx_catch)
#catch per set in EM
catch_per_set_EM <- sum(ttx_catch$n)/nrow(ttx_catch)
