## ---------------------------
## Master thesis
## Author: Katja Hager
##
## Identify MEM sector via NOGA codes
##
## Date Created: June 2025 
## ---------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation
library(lifecycle)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
## ---------------------------
#MEM
# read in NOGA data, filter out relevant MEM-CBA
noga_data_MEM <- read.csv("raw_data/NOGA_KOF_6Steller.csv", header = TRUE, sep = ",")

noga_data_MEM <- noga_data_MEM %>% 
  filter(contract_nr == 47 & year>=2008 & year <=2018) %>% #keep if contract_nr==47 
  group_by(year) %>%
  arrange(year, desc(noga6))

#are the NOGA sectors constant over the years? get the noga codes from 2008 as vector
reference_codes <- noga_data_MEM %>% 
  filter(year == 2008) %>%        
  pull(noga6) %>%                 # Extract the noga6 column as a vector
  unique()                        # Remove duplicates

#check for each year, if we have the same 80 noga codes as in 2008
same_codes_check <- noga_data_MEM %>%
  group_by(year) %>%
  summarise(has_same_codes = setequal(unique(noga6), reference_codes))
table(same_codes_check$has_same_codes) #we have constant and the same NOGA codes over the 11 years

noga_data_MEM <- noga_data_MEM %>% 
  select(c(noga6, label_en)) %>%
  group_by(noga6) %>% slice(1) %>%
  mutate(MEM_CBA = 1) 
noga_data_MEM$year <- NULL

noga_vect_MEM <- noga_data_MEM %>% pull(noga6) %>% unique() #80 noga codes

#save NOGA file to merge later
saveRDS(noga_vect_MEM, file = "output/data/MEM_noga.rds")

## --------- WATCH --------------------
# read in NOGA data, filter out relevant watch-CBA
noga_data_watch <- read.csv("raw_data/NOGA_KOF_6Steller.csv", header = TRUE, sep = ",") %>% 
  filter(contract_nr == 74 & year>=2008 & year <=2018) %>%
  group_by(year) %>% arrange(year, desc(noga6))

#are the NOGA sectors constant over the years?
#get the noga codes from 2008 as vector
reference_codes_watch <- noga_data_watch %>%  filter(year == 2008) %>%        
  pull(noga6) %>% unique()                       
#check for each year, if we have the same 80 noga codes as in 2008
same_codes_check <- noga_data_watch %>%
  group_by(year) %>%
  summarise(has_same_codes = setequal(unique(noga6), reference_codes_watch))

table(same_codes_check$has_same_codes) #we have constant and the same NOGA codes over the 11 years

noga_data_watch <- noga_data_watch %>% 
  select(c(noga6, label_en)) %>%
  group_by(noga6) %>% slice(1) %>%
  mutate(watch_CBA = 1) 
noga_data_watch$year <- NULL

noga_vect_watch <- noga_data_watch %>% pull(noga6) %>% unique() #5 subsectors
saveRDS(noga_vect_watch, file = "output/data/Watch_noga.rds")

## --------------------
# read in NOGA data, filter out relevant metals-CBA
#"0131","Landes-Gesamtarbeitsvertrag für das Metallgewerbe" -> nur diese (auch gesamte Schweiz)
#5851,"0362","CCT pour les métiers techniques de la métallurgie du bÃ¢timent dans le canton de GenÃ¨ve",2018,"2599","259900","Herstellung von sonstigen Metallwaren a. n. g.","Manufacture of other fabricated metal products n.e.c."
#in contract_nr 0362: all four cantonal CBAs are included

noga_data_metal <- read.csv("raw_data/NOGA_KOF_6Steller.csv", header = TRUE, sep = ",") %>% 
  filter(contract_nr %in% c(131, 362) & year>=2008 & year <=2018) %>% #Geneva, Ticino also included
  group_by(year) %>% arrange(year, desc(noga6))

#are the NOGA sectors constant over the years? get the noga codes from 2008 as vector
reference_codes <- noga_data_metal %>%  filter(year == 2008) %>%        
  pull(noga6) %>%                
  unique()  
#check for each year, if we have the same noga codes as in 2008
same_codes_check <- noga_data_metal %>%
  group_by(year) %>% summarise(has_same_codes = setequal(unique(noga6), reference_codes))
table(same_codes_check$has_same_codes) #same NOGA codes over the 11 years

noga_data_metal <- noga_data_metal %>% 
  select(c(noga6, label_en, contract_name)) %>%
  group_by(noga6) %>% slice(1) %>%
  mutate(metal_CBA = 1) 
noga_data_metal$year <- NULL

noga_data_metal_list <- noga_data_metal %>% pull(noga6) %>% unique() #25 noga codes
saveRDS(noga_data_metal_list, file = "output/data/Metal_noga.rds") 

## --------------------
#overlap of NOGA and sectoral CBAs?

noga_MEM_metal_sectors <- full_join(noga_data_MEM, noga_data_metal, by = c("noga6", "label_en"))
noga_MEM_metal_sectors <- noga_MEM_metal_sectors %>%
  mutate(overlap = case_when(MEM_CBA==1 & metal_CBA==1~1,
                             TRUE ~0),
         MEM_CBA = case_when(MEM_CBA==1~1, 
                             is.na(MEM_CBA) ~ 0),
         metal_CBA = case_when(metal_CBA==1~ 1, 
                              is.na(metal_CBA) ~ 0))

noga_MEM_metal_sectors_unique <- noga_MEM_metal_sectors %>% distinct(noga6) %>% pull(noga6)
print(length(noga_MEM_metal_sectors_unique)) #88

sum(noga_MEM_metal_sectors$metal_CBA==0 & noga_MEM_metal_sectors$MEM_CBA==1)
sum(noga_MEM_metal_sectors$metal_CBA==1 & noga_MEM_metal_sectors$MEM_CBA==0)
sum(noga_MEM_metal_sectors$metal_CBA==1 & noga_MEM_metal_sectors$MEM_CBA==1)

noga_MEM_metal_sectors <- noga_MEM_metal_sectors %>% 
  filter(overlap==1) %>%
  select(c( noga6, label_en)) 

stargazer(noga_MEM_metal_sectors, type = "latex",
          title = "Overlap of sub-sectors in MEM-CBA and Metal-CBA",
          label = "tab:1_noga_codes_MEM_metal",
          summary = FALSE, rownames = FALSE, digits = 2, no.space = TRUE,
          font.size = "small", column.sep.width = "3pt",
          out = "output/table/1_noga_codes_MEM_metal.tex")

noga_data_watch <- noga_data_watch %>%
  select(c( noga6, label_en,watch_CBA)) 
stargazer(noga_data_watch, type = "latex",
          title = "Watch-CBA NACE codes",
          label = "tab:1_noga_codes_watch",
          summary = FALSE, rownames = FALSE, digits = 2, no.space = TRUE,
          font.size = "small", column.sep.width = "3pt",
          out = "output/table/1_noga_codes_watch.tex")

