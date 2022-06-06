#Estimate village level total annual harvest
# CJ Brown 2022-04-20


rm(list = ls())
library(gridExtra)
library(sf)
library(dplyr)
library(ggplot2)


load("data-raw/turtle-data-files.rda")

#
# Calculate harvest rate per village 
#

#Summary of days surveyed 
surv_sum <- surv_dates %>% group_by(Community, Province) %>% 
  summarize(tot_days = n(), 
            active_days = sum(is.na(available))) %>%
  ungroup() %>%
  mutate(prop_days = active_days/tot_days)  


#Summarize harvest data 
harvest_sum <- harvest %>% 
  # filter((date >= min(surv_dates$date)) & (date <= max(surv_dates$date))) %>% #(rm data before calendars?)
  group_by(Community, Province) %>% 
  summarize(nharv = n(), 
            lastday = max(c(max(date), as.Date("18-06-01", "%y-%m-%d"))), 
            #Based on Rick's advice assuming last day of survey is the max
            # of last capture or when Simon went around to collect data sheets
            #(about 1st June 2018)
            firstday = min(date), 
            duration = as.numeric(lastday - firstday)) %>%
  left_join(surv_sum) %>%
  left_join(hunters) %>% 
  #estimate harvest, accounting for days not working
  # and change to per year
  mutate(hunter_mult = total_fishers/total_fishers_participate, 
         nharv_est = nharv * (1/prop_days) * (365/duration) * hunter_mult) %>%
  dplyr::select(-tot_days, -active_days, -total_fishers, 
         -total_fishers_not_participating) %>%
  ungroup()

#Save for us in other scripts
save(harvest_sum, file = "data-raw/harvest_sum.rda")
write.csv(harvest_sum, file = "Shared/Outputs/Table_SX_harvest-estimates.csv")

sum(harvest_sum$nharv)
sum(harvest_sum$nharv_est)

gbar <- ggplot(harvest_sum, aes(x = Community, y = nharv_est)) + 
  geom_bar(stat = "identity") + 
  ylab("Harvest estimate (annual)") + 
  theme_bw()

ggsave("Shared/Outputs/Harvest_estimates.png", 
       gbar)

#
# Species level harvest 
#

harvest_spp <- harvest %>% 
  # filter((date >= min(surv_dates$date)) & (date <= max(surv_dates$date))) %>% 
  group_by(Community, Province, species_id) %>% 
  summarize(nharv = n()) %>%
  ungroup() %>% 
  left_join(select(harvest_sum, -nharv)) %>%
  #estimate harvest, accounting for days not working
  # and change to per year
  mutate(nharv_est = nharv * (1/prop_days) * (365/duration) * hunter_mult) 


gbar_spp <- ggplot(harvest_spp, aes(x = species_id, y = nharv_est, fill = species_id)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~Community) + 
  ylab("Harvest estimate (annual)") + 
  theme_bw() + 
  scale_fill_manual(values = c("DarkGreen", "orange", "LightBlue", "grey"))
gbar_spp

ggsave("Shared/Outputs/Harvest_estimates_species.png", 
       gbar_spp)

