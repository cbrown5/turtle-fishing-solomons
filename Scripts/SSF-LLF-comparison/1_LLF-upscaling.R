# Upscaling turtle catch 
# CJ Brown
# 2022-07-21

# Load packages
library(tidyverse)
library(ggplot2)

emdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/EM-data.csv")
logdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/logbook-data.csv")
logsets <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/logbook-sets.csv")
emlogdat <- read.csv("../turtle-fishing-solomons/Shared/Data/LLF-data/em_logbook_matched.csv")

ssf_bootstraps <- read.csv("Outputs/turtle-SSF-bootstraps.csv")

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

ttx_catch_flag <- emdat %>% filter((Sp_code == turtles)) %>%
  group_by(Set_ID, Flag) %>%
  summarize(n = sum(n)) 
ttx_van <- filter(ttx_catch_flag, Flag == "Vanuatu")
ttx_CT <- filter(ttx_catch_flag, Flag == "Chinese Taipei")

per_flag_catch_rate <- ttx_catch_flag %>%
  ungroup() %>%
  group_by(Flag) %>%
  summarize(sum(n)/n())

#
#Resampled standard errors
#

catch_samps_van <- lapply(1:nsamp, function(x) 
  mean(sample(ttx_van$n, nrow(ttx_van), replace = TRUE))) %>%
  unlist()
catch_samps_CT <- lapply(1:nsamp, function(x) 
  mean(sample(ttx_CT$n, nrow(ttx_CT), replace = TRUE))) %>%
  unlist()
catch_samps_other <- lapply(1:nsamp, function(x) 
  mean(sample(ttx_catch$n, nrow(ttx_catch), replace = TRUE))) %>%
  unlist()

#Flag specific catch rates
catch_mean_van <- mean(catch_samps_van)
catch_CIs_van <- quantile(catch_samps_van, c(0.025,0.5, 0.975))
catch_mean_CT <- mean(catch_samps_CT)
catch_CIs_CT <- quantile(catch_samps_CT, c(0.025,0.5, 0.975))
catch_mean_chn <- 0
catch_CIs_chn <- c(0,0,0)

catch_mean_other <- mean(catch_samps_other)
catch_CIs_other <- quantile(catch_samps_other, c(0.025,0.5, 0.975))

#upscaling
nsets_van <- data.frame(table(logsets2018$Flag))
names(nsets_van) <- c("Flag", "nsets")
nsets_van$catch_rate_mean <- c(catch_mean_chn,
                               catch_mean_CT,
                               catch_mean_other,
                               catch_mean_van)
dat_flag_catch_rates <- cbind(nsets_van,
                              rbind(catch_CIs_chn,
                                    catch_CIs_CT,
                                    catch_CIs_other,
                                    catch_mean_van))

catch_mean_flag_2018 <- sum(dat_flag_catch_rates$catch_rate_mean * 
  dat_flag_catch_rates$nsets)

catch_quant_flag_2018 <- colSums(cbind(dat_flag_catch_rates$`2.5%` * 
  dat_flag_catch_rates$nsets,
  dat_flag_catch_rates$`50%` * 
    dat_flag_catch_rates$nsets,
  dat_flag_catch_rates$`97.5%` * 
    dat_flag_catch_rates$nsets
))


#

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
  full_join(ttx_catch2) %>%
  rename(n_ttx = n)
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
cor(yft_log$n_EM, yft_log$n_ttx,
    use = "pairwise.complete.obs",
    method = "spearman")
#YFT logbook to turtles
cor(yft_log$n_ttx, yft_log$n_Logbook,
    use = "pairwise.complete.obs",
    method = "spearman")


#
# Catch rate per YFT
#

yft_ttx_ratio <- yft_log$n_ttx/yft_log$n_Logbook
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

nyft2018 <- logdat %>%
  filter(Set_ID %in% logsets2018$Set_ID) %>%
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
                       catch_quant_flag_2018,
                      ratio_quant_2018) %>%
  cbind(mean = c(catch_mean_2018, catch_mean_flag_2018, ratio_mean_2018)) %>%
  data.frame() %>%
  rownames_to_column("Type")


dout <- list(estimate_CIs = estimate_CIs,
             catch2018_CIs = catch2018_CIs,
             dat_flag_catch_rates = dat_flag_catch_rates)

save(dout, file= "Data/LLF-national-ttx-catch.rda")

#
# Shark species caught
#
sharks <- c("BSH", "FAL", "BTH", "LMA", "OCS", 
            "PSK", "RSK", "SKH", "SHX", "SMA", "THR", "SPY")
#read in fao codes
fao_codes <- read.csv("Data/FAO-species-codes.csv")

x <- emdat %>%
  filter(Sp_code %in% sharks) %>%
  group_by(Sp_code) %>%
  summarize(n = sum(n)) %>%
  arrange(desc(n)) %>%
  left_join(fao_codes, by = c("Sp_code" = "X3A_CODE")) %>%
  print()
x$Scientific_name


#
# Main species by flag
#

#join logdat and logsets
logdat2 <- logdat %>%
  left_join(logsets, by = c("Set_ID" = "Set_ID")) %>%
  filter(sp_code %in% c("YFT", "BET", "ALB")) %>%
  group_by(Flag, sp_code) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  group_by(Flag) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(sp_code) %>%
  arrange(Flag) %>%
  print()

#group logsets by flags and count unique set ids
logsets2 <- logsets %>%
  group_by(Flag) %>%
  summarize(n = n_distinct(Set_ID)) %>%
  print()

length(unique(emdat$Vessel))



#
# National estimate for both fisheries 
#

#Total
quantile(nsets2018*catch_samps[1:5000] + ssf_bootstraps[,1], probs = c(0.025,0.5,0.975))

#Proportion
quantile(ssf_bootstraps[,1]/(nsets2018*catch_samps[1:5000] + ssf_bootstraps[,1]), probs = c(0.025,0.5,0.975))

