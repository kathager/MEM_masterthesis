## ---------------------------
##
## Master thesis
## Merge datasets
## LSE with min wage regions (by MS region) and MEM sector (by NOGA code)
## Author: Katja Hager
##
## Date Created: June 2025
##
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation

library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)

## ---------------------------

#load file from 2_LSE
LSE_08_18 <- readRDS(file = "output/data/1_LSE_08_18.rds")

### MEM-sector -> time invariant
#load MEM-sector data
#table(is.na(LSE_08_18$noga08), LSE_08_18$erhebja) #until 2010
#table(is.na(LSE_08_18$noga08_ent), LSE_08_18$erhebja) #from 2012

#keep mem sector and all workers who are in MEM in 2012

#1) identify MEM sector
noga_data_MEM <- readRDS(file = "output/data/MEM_noga.rds")
LSE_08_18$mem <- ifelse(LSE_08_18$noga08 %in% noga_data_MEM | 
                          LSE_08_18$noga08_ent %in% noga_data_MEM, 1, 0)

#1) identify watch sector
noga_data_watch <- readRDS(file = "output/data/Watch_noga.rds")
LSE_08_18$watch <- ifelse(LSE_08_18$noga08 %in% noga_data_watch | 
                            LSE_08_18$noga08_ent %in% noga_data_watch, 1, 0)


#same noga variable for all years
table(!is.na(LSE_08_18$noga08), LSE_08_18$erhebja) #2008, 2010
table(!is.na(LSE_08_18$noga08_ent), LSE_08_18$erhebja) #from 2012
LSE_08_18$noga <- NA
LSE_08_18$noga <- as.factor(ifelse(!is.na(LSE_08_18$noga08), 
                                   as.character(LSE_08_18$noga08), 
                                   as.character(LSE_08_18$noga08_ent)))

#2) identify workers who are in any of the sectors in 2012 and can be traced by the ahv_nb number
sectoral_workers_2012 <- LSE_08_18 %>%
  filter(erhebja == 2012 & avs_nb!="") %>%
  group_by(avs_nb) %>%
  summarise(
    mem_2012 = ifelse(any(mem==1), 1, 0 ), #if people have >1 job, not all in MEM
    watch_2012 =ifelse(any(watch==1), 1, 0 )) 

sum(sectoral_workers_2012$mem_2012==1) #92'460
sum(sectoral_workers_2012$watch_2012==1) #21'114

#18 workers work in both sectors
table(sectoral_workers_2012$watch_2012, sectoral_workers_2012$mem_2012)

LSE_08_18 <- left_join(LSE_08_18, sectoral_workers_2012, by = ("avs_nb"))

sum(LSE_08_18$mem_2012==1, na.rm = TRUE) #226'493 -> those observed in 2012 and also later! same persons, cool. hello panel.
sum(LSE_08_18$watch_2012==1, na.rm = TRUE) #57845
rm(sectoral_workers_2012)

LSE_08_18 <- LSE_08_18 %>%
  filter(mem==1 | watch == 1  | mem_2012==1 |watch_2012==1 ) 

#check distribution
table(LSE_08_18$mem, LSE_08_18$erhebja) #number of obs declines drastically in 2012

### MS regions -> time invariant
class(LSE_08_18$msreg_an)
length(unique(LSE_08_18$msreg_an)) #106 ms regions + missing
sum(LSE_08_18$msreg_an=="") #2092
LSE_08_18$msreg_an[LSE_08_18$msreg_an==""] <- NA
table(LSE_08_18$erhebja[is.na(LSE_08_18$msreg_an)]) #only from 2012 data

LSE_08_18$msreg_an <- as.factor(LSE_08_18$msreg_an)

levels(LSE_08_18$msreg_an)
length(levels(LSE_08_18$msreg_an))

#load data set with MS regions and corresponding min wage region
min_wage_regions <- readRDS(file = "output/data/min_wage_regions.Rds")

#merge data (all rows from LSE (many) to single rows from min_wage_regions (one)) 
LSE_08_18 <- left_join(LSE_08_18, min_wage_regions, by = "msreg_an")

#minimum wages, not differentiated by qualification level
##lower bar: take the lower bar for "unqualifiziert"
LSE_08_18 <- LSE_08_18 %>%
  mutate(
    min_wage = case_when(wage_region == "A"~ 3850,
                         wage_region == "B"~ 3600,
                         wage_region == "C" ~ 3300,
                         TRUE ~ 3300 #obs with missing MS region: lowest min wage
  ))


##unique firm identifier
LSE_08_18 %>% count(erhebja, burnr) %>% count(erhebja) #2008, 2010 -> Betriebsstätte
LSE_08_18 %>% count(erhebja, entid) %>% count(erhebja) #2012 - 2018 -> Firma
LSE_08_18$burnr[LSE_08_18$erhebja>2010 & is.na(LSE_08_18$burnr)] <- 
  LSE_08_18$entid[LSE_08_18$erhebja>2010 & is.na(LSE_08_18$burnr)]
LSE_08_18 %>% count(erhebja, burnr) %>% count(erhebja) #all years

###Inflation 
inflation_oct <- readRDS(file = "output/data/inflation_oct.Rds")

#merge data (all rows from LSE (many) to single rows from inflation_oct (one)) 
LSE_08_18$erhebja <- as.numeric(LSE_08_18$erhebja)
LSE_08_18 <- left_join(LSE_08_18, inflation_oct, by = "erhebja")


#save working file merged
saveRDS(LSE_08_18, file = "output/data/2a_merged_all_sectors.rds")

###################################

##adapt all types of variables (numeric, characters, ...)
#sapply(all_data, class)

integer_vars <- c( "bezstd", "mbls", "mnliu", "blimok", "gewicht", "iwaz", "gewibgrs", "buwaz1", 
                   "buwaz2", "ibgr", "iwaz", "sonderza", "sozabg", "th", "thi", 
                   "verduz", "xiiimloh", "zulagen", "dienstja", "alter", "anzbe", 
                   "anzlohn", "erhebja", "nrep", "stra_n", "untgroe")

categorical_vars <- c("ausbild","anforni", "berufst", "lohnver", "anforni", "geschle", 
                      "gr", "kurzastd", "lohnform", "msreg_an", "natkat", "nog_2", 
                      "nog_2_08", "noga08", "noga08_2", "noga08_ent", "noga08_ent_2",
                      "privoef", "taetigk", "k_untgroe", "isco_2", "fs03", "fs04",
                      "zivsta", "bae_an", "va_ps07")

string_vars <- c("arbtko", "burnr", "wart", "entid")

all_data <- all_data %>%
  mutate(across(all_of(integer_vars), function(x) as.numeric(x))) %>%
  mutate(across(all_of(categorical_vars), function(x) as.factor(as.character(x))))

