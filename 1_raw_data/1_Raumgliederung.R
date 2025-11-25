## ---------------------------
##
## Master thesis
## Author: Katja Hager
## Date Created: June 2025
#
## Relevant min wages based on districts, assigned to LSE's MS_regions
#
#LSE-data's most-finegrained geographical unit is MS-Region (msreg_an)
#min wages in CBA are assigned by district, a different geographical unit
#lowest geographical unit for BFS: municipality. districts & MS_regions cover several municipalities.
#however, MS-regions can cover several districts, and a district can cover several MS_regions.
#mapping problem -> take lower bar within each MS_region, since our end goal is to merge to LSE data by MS_region
#
#BFS Raumgliederung 
#https://www.agvchapp.bfs.admin.ch/de/typologies/
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(readxl)

search()

## ---------------------------

## load regional definition of min wage
MiLo_Raumgliederung <- read_excel("raw_data/MiLo_Raumgliederung.xlsx", range = "A1:C34")
View(MiLo_Raumgliederung)

## load file from BFS (Raumgliederung per 01.01.2013 to avoid mutated districts / regions)
# https://www.agvchapp.bfs.admin.ch/de/typologies/results?SnapshotDate=01.01.2013&SelectedTypologies%5B0%5D=HR_MSREG2000

geographical_levels <- read_excel("raw_data/Raumgliederungen.xlsx", 
                               col_names = c("municipality_nr", "municipality", "cantonal_nr", "canton", 
                                             "district_nr", "district", "msreg_an"), 
                               col_types = c("numeric", "text", "text", "text", "numeric", 
                                             "text", "numeric"))

geographical_levels <- geographical_levels  %>%
  filter(!is.na(msreg_an))

#we have 106 MS regions
print(length(unique(geographical_levels$msreg_an)))
#but more observations (2408 on municipality level)
print(length(geographical_levels$msreg_an))
#number of districts: 148
print(length(unique(geographical_levels$district_nr)))

######################
#wage regions are assigned on district level
#we need our data on municipality level -> assign minimum wages (district-wise) 
#to the corresponding municipality number

geographical_levels <- geographical_levels %>%
  mutate(wage_region = NA)

##assign for cantons which have a min wage region for the WHOLE canton
NA_region <-  MiLo_Raumgliederung %>%
  filter(MiLo_Raumgliederung$Bezirk == "NA")

for (i in 1:nrow(NA_region)) {
  for (x in 1:nrow(geographical_levels)) {
    if (geographical_levels$canton[x] == NA_region$Kanton[i])
      geographical_levels$wage_region[x] <- NA_region$Region[i]
  }
}

##assign for districts which are especially mentioned (included)
included_region <- MiLo_Raumgliederung %>%
  filter(Bezirk != "NA") %>%
  filter(!grepl("Ohne", Bezirk))

for (i in 1:nrow(included_region)) {
  # Split the Bezirk string into individual districts
  districts_list <- unlist(strsplit(included_region$Bezirk[i], ", "))
  print(districts_list)
  
  for (x in 1:nrow(geographical_levels)) {
    if (geographical_levels$district[x] %in% districts_list) {
      geographical_levels$wage_region[x] <- included_region$Region[i]
    }
  }
}

##assign for districts which are EXcluded (canton WITHOUT these districts)
excluded_region <- MiLo_Raumgliederung %>%
  filter(Bezirk != "NA") %>%
  filter(grepl("Ohne", Bezirk)) 

for (i in 1:nrow(excluded_region)) {
  for (x in 1:nrow(geographical_levels)) {
    if (geographical_levels$canton[x] == excluded_region$Kanton[i])
      if (is.na(geographical_levels$wage_region[x]))
        geographical_levels$wage_region[x] <- excluded_region$Region[i]
  }
}

#all municipalities belong to a minimum wage region
table(geographical_levels$wage_region)
print(sum(!is.na(geographical_levels$wage_region)))

######################

#our LSE data is on MS region level
#we need to go from municipality level to MS region level
#for our purpose, with the LSE data being on MS level, we need one wage region per MS region
#What to do with MS regions that have several wage regions? -> take lower bar (lowest min wage region)

#MS regions are analytical regions. They are across cantonal and district borders
cantons_I_ask <- geographical_levels %>%
  group_by(msreg_an) %>%
  filter(n_distinct(cantonal_nr) > 1)

# MS_regions without problems
no_problem <- geographical_levels %>%
  group_by(msreg_an) %>%
  filter(n_distinct(wage_region) == 1) %>%
  slice(1) %>% #just keep one observation (any, so we just take the first)
  select(-c(district, district_nr, cantonal_nr, canton, municipality_nr, municipality))

# MS_regions with problems (which have more than one minimum wage region within)
problem_regions <- geographical_levels %>%
  group_by(msreg_an) %>%
  summarise(problems = n_distinct(wage_region)) %>%
  filter(problems > 1) 

# Assign lower bar to those MS regions
geographical_levels_problematic <- geographical_levels %>%
  filter(msreg_an %in% problem_regions$msreg_an) %>%
  group_by(msreg_an) %>%
  arrange(msreg_an, desc(wage_region)) %>% #alphabetically descending: min wage is lowest in C, highest in A
  slice(1) %>% #keep lower bar, since alphabetically descending (min wage in A > min wage in B)
  select(-c(district, district_nr, cantonal_nr, canton, municipality_nr, municipality))

#append the dataframes
min_wage_regions <- rbind(geographical_levels_problematic, no_problem)
print(nrow(min_wage_regions)) #106 MS regions. perfect!

min_wage_regions$msreg_an <- as.factor(as.character(min_wage_regions$msreg_an))

saveRDS(min_wage_regions, file = "output/data/min_wage_regions.Rds")
