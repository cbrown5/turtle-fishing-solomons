#Wrangle data and dates
# CJ Brown 2022-04-20

rm(list = ls())

library(tidyverse)


#Survey dates
surv_dates_in <- read_csv("data-raw/survey_dates.csv")

surv_dates <- surv_dates_in %>% gather(month, available, -Community, -Province, -Day) %>%
  mutate(date_temp = paste(Day, month, sep = "-")) %>%
  mutate(date = as.Date(date_temp, "%d-%b-%y"))

#remove dates that don't exist 
surv_dates <- surv_dates[!is.na(surv_dates$date),]

hunters <- read_csv("data-raw/turtle-fishers.csv")

#"turtle-harvest-sighted.csv"
harvest_in <- read_csv("data-raw/MIKES datasheet final_20.4.2022.csv",
                       na = c("NA", "", "Not recorded"))

unique(harvest_in$species_id)

#fix species IDs
harvest <- within(harvest_in, {
  date_temp <- date
  date <- as.Date(date, "%d.%m.%y")
  reef_id[reef_id > 9999] <- NA
  species_id[grep("Hawksbil", species_id)] <- "Hawksbill"
  species_id[grep("Olive Redley", species_id)] <- "Olive Ridley"
  # Community[grep("Auki", Community)] <- "Radefasu"
  Province[grep("Wagina", Community)] <- "Choiseul"
})


unique(harvest$species_id)

#change 2006 dates to 2016
ifix <- which(harvest$date < "2010-12-01")
harvest$date[ifix] <- as.Date("16.12.16", "%d.%m.%y")

  
#"turtle-harvest-not-sighted.csv"
missed_in <- read_csv("data-raw/turtle-harvest-not-sighted.csv")

missed <- within(missed_in,{
  date_temp <- date
  date <- as.Date(date_temp, "%d.%m.%y")
})


save(harvest, hunters, missed, surv_dates, file = "data-raw/turtle-data-files.rda")
