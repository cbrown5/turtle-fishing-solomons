# Upscaling turtle catch 
# CJ Brown
# 2022-07-21

#TODO: clean vessel names for em and logbook dat
# so I can get flag ID
#Check number sets
# in report we found: In 2018 there were 15807 sets 
#and 51834 thousand hooks.
#below we have 15996 sets... 
#also check issue with duplicate EM sets (set_id_old has fewer
# sets than set ID, make sure I get correct number sets)
#Then update YFT method... 
#CHeck why logbook data is in a grid...

# Load packages
library(tidyverse)
library(ggplot2)

emdat <- read.csv("Shared/Data/LLF-data/EM-data.csv")
logdat <- read.csv("Shared/Data/LLF-data/logbook-data.csv")
logsets <- read.csv("Shared/Data/LLF-data/logbook-sets.csv")
emlogdat <- read.csv("Shared/Data/LLF-data/em_logbook_matched.csv")
 
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
length(unique(emdat$Set_ID))

#
# logbook stats
#

logsets2018 <- logsets %>%
  filter(grepl("2018", Date))
nsets2018 <- length(unique(logsets2018$Set_ID))
length(unique(logsets2018$Trip_ID))
length(unique(logsets2018$vessel_name))

# ------------ 
# Per set catch rate upscaling
# ------------ 

ttx_catch <- emdat %>% filter((Sp_code == turtles)) %>%
  group_by(Set_ID) %>%
  summarize(n = sum(n)) 
n_em_sets <- nrow(ttx_catch)
#catch per set in EM
catch_per_set_EM <- sum(ttx_catch$n)/nrow(ttx_catch)

#
#Resampled standard errors
#


catch_samps <- lapply(1:nsamp, function(x) 
  mean(sample(ttx_catch$n, n_em_sets, replace = TRUE))) %>%
  unlist()
catch_mean <- mean(catch_samps)
catch_CIs <- quantile(catch_samps, c(0.025,0.5, 0.975))

catch_mean_2018 <- nsets2018*catch_mean
catch_quant_2018 <- nsets2018*catch_CIs

# ------------ 
# Per set per flag upscaling 
# ------------ 

# TODO, need to sort out flag IDs

# ------------ 
# Per YFT upscaling
# ------------ 

#
# Join DFs
#

ttx_catch2 <- emdat %>% filter((Sp_code == turtles)) %>%
  group_by(Set_ID_old) %>%
  summarize(n = sum(n)) 
yft_log <- emlogdat %>% filter(Sp_code2 == "YFT") %>%
  #Just change to BET/ALB/YFT to see species correlations
  group_by(Set_ID_old) %>%
  summarize(n_EM = sum(n_EM),
            n_Logbook = sum(n_Logbook)) %>%
  full_join(ttx_catch2) 
nrow(yft_log)
length(unique(yft_log$Set_ID_old))

#
# Establish correlation
#


#YFT in logbook and EM
cor(yft_log$n_EM, yft_log$n_Logbook,
    use = "pairwise.complete.obs",
    method = "spearman")
#YFT EM to turtles
cor(yft_log$n_EM, yft_log$n,
    use = "pairwise.complete.obs",
    method = "spearman")
#YFT logbook to turtles
cor(yft_log$n, yft_log$n_Logbook,
    use = "pairwise.complete.obs",
    method = "spearman")


#
# Catch rate per YFT
#

yft_ttx_ratio <- yft_log$n/yft_log$n_Logbook
yft_ttx_ratio[is.na(yft_ttx_ratio)] <- 0
yft_ttx_ratio[is.infinite(yft_ttx_ratio)] <- 0

#
# Resampling 
#
ratio_samps <- lapply(1:nsamp, function(x) 
  mean(sample(yft_ttx_ratio, length(yft_ttx_ratio), replace = TRUE))) %>%
  unlist()
ratio_mean <- mean(ratio_samps)
ratio_CIs <- quantile(ratio_samps, c(0.025,0.5, 0.975))

#
# upscaling
#

nyft2018 <- logsets2018 %>%
  left_join(logdat) %>%
  filter(sp_code == "YFT") %>%
  summarize(sum(n)) %>%
  as.numeric()

ratio_mean_2018 <- nyft2018*ratio_mean
ratio_quant_2018 <- nyft2018*ratio_CIs

#
# Save all data
#

#catch per set/per tuna
estimate_CIs <- rbind(catch_CIs,
                      ratio_CIs) %>%
  cbind(mean = c(catch_mean, ratio_mean)) %>%
  data.frame() %>%
  rownames_to_column("Type")

#scaled up estimates for 2018
catch2018_CIs <- rbind(catch_quant_2018,
                      ratio_quant_2018) %>%
  cbind(mean = c(catch_mean_2018, ratio_mean_2018)) %>%
  data.frame() %>%
  rownames_to_column("Type")


dout <- list(estimate_CIs = estimate_CIs,
             catch2018_CIs = catch2018_CIs)

save(dout, file= "data-raw/LLF-national-ttx-catch.rda")

#Data needs
#Logbook to EM matched
# All EM
# All logbook 
#
# Spatial files for area of EEZ and mapping 
#(do in another script)
# Map sets with coastal fishery footprint... 
# Add hawksbill migration routes? 
# ask Amy to make these maps? 

# 
# Upscaling 
#

#1.  Turtle catch per logbook YFT (check YFT is best predictor)
# Verify YFT as best predictor - spearman rank correlation
# Matched logbook and EM. Calculate turtles per logbook tuna with SE
# Multipy logbook tuna catch by turtle rate and SEs.
#
# 2. Turtle catch per set
# Logbook sets and EM turtle rate with SE
#
# 3. Calculate turtle catch rate by flag
# As above, but do a group_by on flag
#
# 4. Catch per unit area
# Not really possible. But we can calculate turtles per ha 
# of convex hull around fishing ops. Just for comparison
#

# Save results (mean + SE) for figures




