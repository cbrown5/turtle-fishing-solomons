# Free divers capture thousands of turtles a year in Solomon Islands

Code and data in support of the manuscript

CJ Brown, Rick Hamilton

Citation:

Richard Hamilton, Simon Vuto, Christopher Brown, Peter Waldie, John Pita, Rosalie Masu, Nate Peterson, Christine Hof and Col Limpus. (2022) "Free divers capture thousands of turtles a year in Solomon Islands". Unpublished manuscript


## Overview of scripts

Scripts are in the scripts folder and include

1_data-reshaping.R  

Basic data wrangling and data input for the harvest survey datasheets

2_graph-size-dist.R  

Graphs of size distribution of catches

3_village-level-harvest.R  

Estimate annual harvest for each community, by scaling up days surveyed and coverage of fishermen

4_reef-hulls.R  

Convex hulls of village fishing footprints

5_estimate-harvest-coastal-villages.R  

Estimate scaled up annual harvest for all of Solomon Islands

6_analysis-historical-catch.R

Analysis trends in survey data of catch rates from fisherman.

## Overview of open data  

Primary data is available in the folder 'data-raw/'. This includes all data files that we have permission to make freely available.

MIKES datasheet final_20.4.2022.csv  

Turtles observed to be hunted by community monitors.

survey_dates.csv  

Dates community monitors were working and looking for hunted turtles.

turtle-fishers.csv  

Estimated number of fishers that participated and did not particate in surveys in each community.  

turtle-harvest-not-sighted.csv  

Estimates from the community monitors of turtles they heard about but weren't include in the survey (not used in the manuscript).

## Overview of other data  

Some data has been kept private for privacy reasons or because we do not have the license to share it. This is in the file path 'Shared/Data/' which is not synced to github. This includes:

IMARS - EastSolomons and Solomons - MCRM Reefs: Shapefiles of Solomon Islands reefs form the Millenium coral reef monitoring.

sols-villages: shapefiles of village locations/census points

Turtle Reef Areas 2019 Jan 24: shapefiles of reefs identified by local communities as fishing grounds and their local names. (Used to spatially reference turtle harvest to spatial coordinates).

SI_admin: shapefile of Solomon Islands

turtle-hunter-questions.csv survey data of questions asked to turtle hunters about their contemporary and historical catch.
